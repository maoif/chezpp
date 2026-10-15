(library (chezpp concurrency fiber)
  (export run-fibers spawn-fiber
          fiber-quit fiber-yield

          event-never event-always
          event-wrap event-select event-sync
          net-operation-event
          net-operation-event-pool-metrics
          wrap-operation choice-operation perform-operation

          make-channel
          channel-get-event channel-put-event get-operation put-operation
          channel-get channel-put!
          get-message put-message

          make-fiber-mutex fiber-mutex-acquire fiber-mutex-release

          make-fiber-cvar fiber-cvar-wait fiber-cvar-signal fiber-cvar-broadcast

          with-timer-off with-fiber-mutex
          milisleep-event sleep-event fiber-milisleep fiber-sleep)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp dlist)
          (chezpp concurrency)
          (chezpp system)
          (chezpp io)
          (chezpp internal)
          (chezpp net operation)
          (chezpp net operation private))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   fiber core
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define *default-ticks* 3000)

  ;; Chez continuations migrate between OS threads, but thread parameters do not.
  ;; Set both parameters before invoking every fresh or resumed task.
  (define-threaded current-scheduler #f)
  (define-threaded current-fiber-task #f)

  (define enqueue!
    (lambda (queue value)
      (dlist-add! queue value)))

  (define dequeue!
    (lambda (queue)
      (if (dlist-empty? queue)
          #f
          (let ([value (dlist-ref queue 0)])
            (dlist-delete! queue 0)
            value))))

  (define-record-type fiber-run-context
    (fields (mutable schedulers)
            stop-flag
            (mutable init-values)
            (mutable terminal-error)
            park-mutex park-condition
            (mutable runnable-count)))

  (define-record-type scheduler
    (fields i context rq rq-lock (mutable running?) (mutable thread) (mutable k)))

  ;; A resumed continuation has the same token and init status as its original task.
  (define-record-type fiber-task
    (fields token init? thunk))

  (define context-stopped?
    (lambda (context)
      (unabox (fiber-run-context-stop-flag context))))

  (define scheduler-return
    (lambda (type value)
      ;; Never return to the scheduler captured on the continuation's source thread.
      ((scheduler-k current-scheduler) type value)))

  (define request-context-stop!
    (lambda (context error)
      (let ([mutex (fiber-run-context-park-mutex context)])
        (mutex-acquire mutex)
        (unless (context-stopped? context)
          (when error
            (fiber-run-context-terminal-error-set! context error))
          (abox-set! (fiber-run-context-stop-flag context) #t))
        (condition-broadcast (fiber-run-context-park-condition context))
        (mutex-release mutex))))

  #|macro:with-timer-off
  Evaluate expressions `expression ...` with instruction-count preemption disabled. Restore
  the previous timer on normal return or an exception, and return all expression values.
  |#
  (define-syntax with-timer-off
    (syntax-rules ()
      [(_ expression ...)
       (let ([previous-timer (set-timer 0)])
         (call-with-values
          (lambda ()
            ;; Exception cleanup avoids dynamic-wind exit/entry on an ordinary fiber yield.
            (guard (condition
                    [else (set-timer previous-timer) (raise condition)])
              (begin expression ...)))
          (lambda results
            (set-timer previous-timer)
            (apply values results))))]))

  #|proc:fiber-quit
  The `fiber-quit` procedure terminates the current fiber without returning. When the init
  fiber quits, its entire run context stops and `run-fibers` returns an unspecified value.
  A child fiber quitting lets its init fiber and siblings continue.
  |#
  (define-who fiber-quit
    (lambda ()
      (pcheck ()
        (unless current-scheduler
          (errorf who "not running scheduler"))
        (set-timer 0)
        (when (fiber-task-init? current-fiber-task)
          (request-context-stop! (scheduler-context current-scheduler) #f))
        (scheduler-return 'quit #f))))

  #|proc:fiber-yield
  The `fiber-yield` procedure suspends the current fiber and requeues it to run later.
  When the fiber resumes, it returns an unspecified value.
  |#
  (define-who fiber-yield
    (lambda ()
      (pcheck ()
        (unless current-scheduler
          (errorf who "not running scheduler"))
        (set-timer 0)
        ;; The yielded continuation is resumed at most once.
        (call/1cc (lambda (resume) (scheduler-return 'yield resume))))))

  (define put-task!
    (case-lambda
      [(task) (put-task! current-scheduler task)]
      [(sched task)
       (assert (= 0 (set-timer 0)))
       (let* ([context (scheduler-context sched)]
              [mutex (fiber-run-context-park-mutex context)])
         ;; Publication and the park predicate use one mutex, always before queue locks.
         (mutex-acquire mutex)
         (unless (context-stopped? context)
           (let ([lock (scheduler-rq-lock sched)])
             (spinlock-acquire lock)
             (enqueue! (scheduler-rq sched) task)
             (spinlock-release lock))
           (fiber-run-context-runnable-count-set!
            context (fx1+ (fiber-run-context-runnable-count context)))
           (condition-signal (fiber-run-context-park-condition context)))
         (mutex-release mutex))]))

  ;; The caller holds the context park mutex throughout queue inspection and removal.
  (define get-task-locked!
    (lambda (sched)
      (let ([lock (scheduler-rq-lock sched)])
        (spinlock-acquire lock)
        (let ([task (dequeue! (scheduler-rq sched))])
          (spinlock-release lock)
          task))))

  (define get-or-steal-task-locked!
    (lambda (sched)
      (or (get-task-locked! sched)
          (let* ([schedulers (fiber-run-context-schedulers (scheduler-context sched))]
                 [count (vector-length schedulers)])
            (let loop ([i 0])
              (and (fx< i count)
                   (or (get-task-locked! (vector-ref schedulers i))
                       (loop (fx1+ i)))))))))

  (define await-task!
    (lambda (sched)
      (let* ([context (scheduler-context sched)]
             [mutex (fiber-run-context-park-mutex context)]
             [condition (fiber-run-context-park-condition context)])
        (mutex-acquire mutex)
        (let loop ()
          (cond
           [(context-stopped? context) (mutex-release mutex) #f]
           [(fx> (fiber-run-context-runnable-count context) 0)
            (let ([task (get-or-steal-task-locked! sched)])
              (assert task)
              (fiber-run-context-runnable-count-set!
               context (fx1- (fiber-run-context-runnable-count context)))
              (mutex-release mutex)
              task)]
           [else
            ;; condition-wait releases this mutex atomically and reacquires it once.
            ;; Every wake rechecks both shutdown and runnable work; no spinlock is held.
            (condition-wait condition mutex)
            (loop)])))))

  (define continue-task
    (lambda (task thunk)
      (make-fiber-task (fiber-task-token task) (fiber-task-init? task) thunk)))

  (define run-scheduler
    (lambda (sched)
      (let ([original-handler (timer-interrupt-handler)]
            [context (scheduler-context sched)])
        (set-timer 0)
        (let loop ()
          (let ([task (await-task! sched)])
            (when task
              (unless (context-stopped? context)
                (set! current-fiber-task task)
                (let-values ([(type value)
                              ;; Each scheduler escape is invoked exactly once.
                              (call/1cc
                               (lambda (return)
                                 (scheduler-k-set! sched return)
                                 (timer-interrupt-handler
                                  (lambda ()
                                    (set-timer 0)
                                    ;; The interrupted continuation is resumed once.
                                    (call/1cc
                                     (lambda (resume)
                                       (scheduler-return 'time resume)))))
                                 (with-exception-handler
                                     (lambda (condition)
                                       (set-timer 0)
                                       (scheduler-return 'error condition))
                                   (lambda ()
                                     (set-timer *default-ticks*)
                                     ((fiber-task-thunk task))
                                     (set-timer 0)
                                     (scheduler-return 'complete #f)))))])
                  (set-timer 0)
                  (timer-interrupt-handler original-handler)
                  (set! current-fiber-task #f)
                  (unless (context-stopped? context)
                    (case type
                      [(yield time)
                       (put-task! sched (continue-task task (lambda () (value (void)))))]
                      [(callback)
                       ;; Registration executes on the scheduler, with preemption disabled.
                       ;; Keep task identity visible for event registration and ownership.
                       (set! current-fiber-task task)
                       (guard (condition
                               [else (request-context-stop! context condition)])
                         (value))
                       (set! current-fiber-task #f)]
                      [(error) (request-context-stop! context value)]
                      [else (void)]))))
              (loop))))
        (set-timer 0)
        (timer-interrupt-handler original-handler)
        (scheduler-k-set! sched #f))))

  (define wrap-thunk
    (lambda (context init? thunk)
      (make-fiber-task
       (cons 'fiber-task '()) init?
       (lambda ()
         (call-with-values
          thunk
          (lambda results
            (set-timer 0)
            (when init?
              (fiber-run-context-init-values-set! context results)
              (request-context-stop! context #f))
            ;; This boundary also handles completion after continuation migration.
            (scheduler-return 'complete #f)))))))

  (define random-sched
    (lambda (context)
      (let* ([schedulers (fiber-run-context-schedulers context)]
             [count (vector-length schedulers)])
        (vector-ref schedulers (fxmod (random 99999) count)))))

  #|proc:spawn-fiber
  The `spawn-fiber` procedure schedules `thunk`, a zero-argument procedure, as a child fiber
  in the current run context. Its result is discarded. Returns an unspecified value.
  |#
  (define-who spawn-fiber
    (lambda (thunk)
      (pcheck ([procedure? thunk])
        (unless current-scheduler
          (errorf who "not running scheduler"))
        (with-timer-off
          (let ([context (scheduler-context current-scheduler)])
            (put-task! (random-sched context) (wrap-thunk context #f thunk)))))))

  #|proc:run-fibers
  The `run-fibers` procedure runs `thunk`, a zero-argument procedure, as the init fiber in a new
  context. Init completion or quit stops the context and drops pending child work. All workers
  join before its init values are returned or the first fiber failure is raised to the caller.
  A nested call enqueues `thunk` as a child in the current context and returns an unspecified
  value immediately, without waiting for the nested thunk to finish.
  |#
  (define-who run-fibers
    (lambda (thunk)
      (pcheck ([procedure? thunk])
        (if current-scheduler
            (spawn-fiber thunk)
            (let* ([context (make-fiber-run-context
                             #f (abox #f) (list (void)) #f
                             (make-mutex 'fiber-run-context)
                             (make-condition 'fiber-run-condition) 0)]
                   [schedulers
                    (list->vector
                     (map (lambda (i)
                            (make-scheduler i context (make-dlist)
                                            (make-spinlock 'fiber-scheduler-queue)
                                            #f #f #f))
                          (iota (cpu-count))))])
              (fiber-run-context-schedulers-set! context schedulers)
              (with-timer-off
                (put-task! (vector-ref schedulers 0) (wrap-thunk context #t thunk)))
              (let ([threads
                     (vector-map
                      (lambda (sched)
                        (let ([thread
                               (fork-thread
                                (lambda ()
                                  (let ([previous-scheduler current-scheduler]
                                        [previous-task current-fiber-task])
                                    (set! current-scheduler sched)
                                    (set! current-fiber-task #f)
                                    (scheduler-running?-set! sched #t)
                                    (run-scheduler sched)
                                    (scheduler-running?-set! sched #f)
                                    (set! current-fiber-task previous-task)
                                    (set! current-scheduler previous-scheduler))))])
                          (scheduler-thread-set! sched thread)
                          thread))
                      schedulers)])
                (vector-for-each thread-join threads)
                (let ([condition (fiber-run-context-terminal-error context)])
                  (if condition
                      (raise condition)
                      (apply values (fiber-run-context-init-values context))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   first-class events
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define-record-type event (fields))

  ;; the base event type
  (define-record-type base-event
    (parent event)
    (sealed #t)
    (fields wrap-fn
            ;; unit -> any
            try-fn
            ;; abox -> resume -> unit
            block-fn))

  ;; event type returned by `select-event`
  (define-record-type select-event
    (parent event)
    (sealed #t)
    ;; vector of base events
    (fields (mutable events)))


  #|doc
  Make an event that will never be successfully synchronized.
  |#
  (define event-never
    (let ([e (make-base-event
              #f
              (lambda () (lambda () #f))
              ;; just drop the fiber
              (lambda (my-flag resume) (void)))])
      (lambda () e)))


  #|doc
  Make an event that will always return the value `x` when synchronized.
  |#
  (define event-always
    (lambda (x)
      (make-base-event
       #f
       (lambda () (lambda () x))
       (lambda (my-flag resume) (assert-unreachable)))))


  #|doc
  Wrap the first-class event `evt` with function `f`.
  When the event `evt` is successfully synchronized,
  the return value will be passed to `f`, and the return value of `f`
  will be the final return value of the synchronization action.
  |#
  (define-who event-wrap
    (lambda (evt f)
      (pcheck ([event? evt] [procedure? f])
              (if (select-event? evt)
                  (let ([bevts (select-event-events evt)])
                    (select-event-events-set! evt
                                              (vector-map (lambda (e) (event-wrap e f)) bevts))
                    evt)
                  (let ([wrap-fn (base-event-wrap-fn evt)])
                    (make-base-event
                     ;; compose
                     (if wrap-fn
                         (lambda args
                           (call-with-values (lambda () (apply wrap-fn args)) f))
                         f)
                     (base-event-try-fn evt)
                     (base-event-block-fn evt)))))))

  (define all-events?
    (lambda (evts)
      (andmap (lambda (e) (or (event? e) (select-event? e))) evts)))


  #|doc
  Construct a first-class select event that when synchronized,
  will nondeterministically choose one component event to synchronize.
  This enables selective communication among, e.g., a group of channels.
  |#
  (define-who event-select
    (lambda evts
      (pcheck ([all-events? evts])
              (if (null? evts)
                  ;; TODO check
                  event-never
                  ;; make sure all events in a select event are base events
                  (let* ([flatten
                          (lambda (evts)
                            (let loop ([e* evts] [res '()])
                              (if (null? e*)
                                  res
                                  (let ([e (car e*)])
                                    (if (base-event? e)
                                        (loop (cdr e*) (cons e res))
                                        (loop (cdr e*)
                                              (append (vector->list (select-event-events e))
                                                      res)))))))]
                         [bevts (flatten evts)])
                    (make-select-event (list->vector bevts)))))))


  #|proc:event-sync
  The `event-sync` procedure synchronizes the current fiber on event `evt`. It returns the
  event's synchronization values after applying its wrap functions in their construction order.
  If no synchronization is possible yet, the fiber waits without blocking its scheduler worker.
  |#
  (define-who event-sync
    (lambda (evt)
      (pcheck ([event? evt])
              (define wrap-resume
                (lambda (resume wrap-fn)
                  (if wrap-fn
                      (lambda (thunk)
                        (resume (lambda ()
                                  (call-with-values thunk wrap-fn))))
                      resume)))
              (define block
                (lambda (resume)
                  ;; the atomic flag used to mark event state per sync instance
                  (let ([flag (abox 'W)])
                    (if (select-event? evt)
                        (let* ([bevts (select-event-events evt)]
                               [len (vector-length bevts)])
                          ;; call block-fn on every base event
                          (vector-for-each
                           (lambda (evt)
                             (let ([wrap-fn  (base-event-wrap-fn  evt)]
                                   [block-fn (base-event-block-fn evt)])
                               (block-fn flag (wrap-resume resume wrap-fn))))
                           bevts))
                        (let ([wrap-fn  (base-event-wrap-fn  evt)]
                              [block-fn (base-event-block-fn evt)])
                          (block-fn flag (wrap-resume resume wrap-fn)))))))
              ;; capture the cont.
              (define suspend
                (lambda ()
                  ;; Return to scheduler and do blocking there, since timer is turned off there.
                  ;; This is a function call that usually calls the `thunk` below.
                  (set-timer 0)
                  (let ([suspended-scheduler current-scheduler]
                        [suspended-task current-fiber-task])
                  ;; Event registrations may resume a continuation from another OS thread.
                  ((call/cc
                    (lambda (k)
                      ;; This is called in things like channel code when doing rendez-vous
                      ;; and handling over messege.
                      ;; `thunk`, when called, will return the message.
                      (define resume
                        (lambda (thunk)
                          (assert (= 0 (set-timer 0)))
                          (put-task! suspended-scheduler
                                     (continue-task suspended-task (lambda () (k thunk))))))
                      (assert (scheduler-running? current-scheduler))
                      ;; RET
                      ((scheduler-k current-scheduler) 'callback (lambda () (block resume)))))))))
              (if (select-event? evt)
                  ;; Start from a random base event, and try all of them.
                  ;; If no one works, block on all of them.
                  (let* ([bevts (select-event-events evt)]
                         [len   (vector-length bevts)]
                         [start (fxmod (random 99999) len)])
                    (let loop ([i 0] [start start])
                      (if (fx= i len)
                          (suspend)
                          (let* ([evt      (vector-ref bevts start)]
                                 [wrap-fn  (base-event-wrap-fn  evt)]
                                 [try-fn   (base-event-try-fn   evt)]
                                 [th       (with-timer-off (try-fn))])
                            (if th
                                (if wrap-fn
                                    (call-with-values th wrap-fn)
                                    (th))
                                (loop (fx1+ i) (fxmod (fx1+ start) len)))))))
                  (let ([wrap-fn  (base-event-wrap-fn  evt)]
                        [try-fn   (base-event-try-fn   evt)]
                        [block-fn (base-event-block-fn evt)])
                    (let ([th (with-timer-off (try-fn))])
                      (if th
                          (if wrap-fn
                              (call-with-values th wrap-fn)
                              (th))
                          (suspend))))))))


  (define wrap-operation event-wrap)
  (define choice-operation event-select)
  (define perform-operation event-sync)

  ;;;;===----------------------------------------------------------------------===
  ;;;; Network operation events
  ;;;;===----------------------------------------------------------------------===

  (define net-driver-lock (make-spinlock 'fiber-net-driver))
  (define net-driver-job* '())
  (define net-driver-workers 0)
  (define net-driver-job-free (map (lambda (_) (vector #f '() #f)) (iota 64)))
  (define net-driver-waiter-free (map (lambda (_) (make-vector 5 #f)) (iota 256)))
  (define net-driver-waiters-in-use 0)
  (define net-driver-waiter-high-water 0)

  (define release-net-waiter-locked!
    (lambda (waiter)
      (vector-fill! waiter #f)
      (set! net-driver-waiter-free (cons waiter net-driver-waiter-free))
      (set! net-driver-waiters-in-use (fx1- net-driver-waiters-in-use))))

  (define net-waiter-live?
    (lambda (waiter)
      (and (not (context-stopped? (vector-ref waiter 3)))
           (scheduler-running? (vector-ref waiter 4))
           (not (eq? 'S (unabox (vector-ref waiter 0)))))))

  (define detach-net-job-locked!
    (lambda (job)
      (let ([waiter* (vector-ref job 1)])
        (set! net-driver-job* (remq job net-driver-job*))
        (vector-set! job 0 #f)
        (vector-set! job 1 '())
        (vector-set! job 2 #f)
        (set! net-driver-job-free (cons job net-driver-job-free))
        waiter*)))

  (define snapshot-net-result
    (lambda (operation)
      (case (net-operation-state operation)
        [(completed)
         (let ([result (net-operation-result operation)]) (lambda () result))]
        [(failed cancelled)
         (let ([failure (net-operation-condition operation)]) (lambda () (raise failure)))]
        [else #f])))

  (define deliver-net-waiter!
    (lambda (waiter result)
      (with-timer-off
       (let ([flag (vector-ref waiter 0)]
             [resume (vector-ref waiter 1)]
             [result-box (vector-ref waiter 2)]
             [context (vector-ref waiter 3)]
             [scheduler (vector-ref waiter 4)])
        (with-spinlock net-driver-lock (release-net-waiter-locked! waiter))
        (guard (ignored [else (void)])
          (when (and (not (context-stopped? context)) (scheduler-running? scheduler))
            (abox-set! result-box result)
            (let spin ()
              (case (abox-cas! flag 'W 'S)
                [(W) (resume result)]
                [(C) (spin)]
                [(S) (void)]
                [else (assert-unreachable)]))))))))

  (define drive-net-job!
    (lambda (job)
      (let ([operation
             (with-timer-off
               (with-spinlock net-driver-lock
                 (let ([live* '()])
                   (for-each
                    (lambda (waiter)
                      (if (net-waiter-live? waiter)
                          (set! live* (cons waiter live*))
                          (release-net-waiter-locked! waiter)))
                    (vector-ref job 1))
                   (vector-set! job 1 (reverse live*))
                   (if (null? live*)
                       (begin (detach-net-job-locked! job) #f)
                       (vector-ref job 0)))))])
        (when operation
          ;; Only the OS driver thread enters the operation's ordinary mutexes and callbacks.
          (guard (ignored [else (void)])
            (when (eq? 'pending (net-operation-state operation))
              (net-operation-step! operation)))
          (let ([result (snapshot-net-result operation)])
            (if result
              (let ([waiter*
                     (with-timer-off
                       (with-spinlock net-driver-lock (detach-net-job-locked! job)))])
                (for-each (lambda (waiter) (deliver-net-waiter! waiter result)) waiter*))
              (with-timer-off
                (with-spinlock net-driver-lock (vector-set! job 2 #f)))))))))

  (define drive-net-operations
    (lambda ()
      (set! current-scheduler #f)
      (let loop ()
        (let ([job
               (with-timer-off
                 (with-spinlock net-driver-lock
                   (if (null? net-driver-job*)
                       (begin (set! net-driver-workers (fx1- net-driver-workers)) 'stop)
                       (let ([job (find (lambda (job) (not (vector-ref job 2)))
                                        net-driver-job*)])
                         (when job
                           (vector-set! job 2 #t)
                           ;; Rotate claims so a pending operation cannot starve later jobs.
                           (set! net-driver-job* (append (remq job net-driver-job*) (list job))))
                         job))))])
          (unless (eq? job 'stop)
            (when job (drive-net-job! job))
            (milisleep 1)
            (loop))))))

  #|proc:net-operation-event-pool-metrics
  The `net-operation-event-pool-metrics` procedure returns a fresh vector of driver job count,
  active waiter count, and waiter high-water count. Four workers serve up to 64 jobs and 256 waiters.
  |#
  (define net-operation-event-pool-metrics
    (lambda ()
      (pcheck ()
        (with-timer-off
          (with-spinlock net-driver-lock
            (vector (length net-driver-job*) net-driver-waiters-in-use
                    net-driver-waiter-high-water))))))

  #|proc:net-operation-event
  The `net-operation-event` procedure creates a fiber event for network `operation`.
  Synchronization drives the operation on a bounded OS worker pool and returns its result.
  The original failure or cancellation condition is raised when the fiber resumes.
  Losing event choices and stopped schedulers release their bounded waiters without advancing work.
  |#
  (define net-operation-event
    (lambda (operation)
      (pcheck ([net-operation? operation])
        (let ([result (abox #f)])
          (define try (lambda () (unabox result)))
          (define block
            (lambda (flag resume)
              (let ([accepted?
                     (with-timer-off
                       (with-spinlock net-driver-lock
                         (let ([job (find (lambda (job) (eq? operation (vector-ref job 0)))
                                          net-driver-job*)])
                           (and (pair? net-driver-waiter-free)
                                (or job (pair? net-driver-job-free))
                                (begin
                                  (unless job
                                    (set! job (car net-driver-job-free))
                                    (set! net-driver-job-free (cdr net-driver-job-free))
                                    (vector-set! job 0 operation)
                                    (set! net-driver-job* (cons job net-driver-job*)))
                                  (let ([waiter (car net-driver-waiter-free)])
                                    (set! net-driver-waiter-free (cdr net-driver-waiter-free))
                                    (vector-set! waiter 0 flag)
                                    (vector-set! waiter 1 resume)
                                    (vector-set! waiter 2 result)
                                    (vector-set! waiter 3 (scheduler-context current-scheduler))
                                    (vector-set! waiter 4 current-scheduler)
                                    (vector-set! job 1 (cons waiter (vector-ref job 1)))
                                    (set! net-driver-waiters-in-use
                                          (fx1+ net-driver-waiters-in-use))
                                    (set! net-driver-waiter-high-water
                                          (fxmax net-driver-waiter-high-water
                                                 net-driver-waiters-in-use)))
                                  (let start-workers ()
                                    (when (fx< net-driver-workers 4)
                                      (set! net-driver-workers (fx1+ net-driver-workers))
                                      (fork-thread drive-net-operations)
                                      (start-workers)))
                                  #t)))))])
                (unless accepted?
                  (when (eq? 'W (abox-cas! flag 'W 'S))
                    (resume (lambda () (errorf 'net-operation-event
                                               "network event waiter pool exhausted"))))))))
          (make-base-event #f try block)))))


  (define-record-type channel
    (fields
     ;; item: #(flag resume-put msg)
     putQ
     ;; item: #(flag resume-get)
     getQ
     putQ-lock getQ-lock putQ-gc-counter getQ-gc-counter)
    (protocol (lambda (new)
                (lambda ()
                  (new (dlist) (dlist) (make-spinlock 'channel-getQ) (make-spinlock 'channel-putQ) (abox 0) (abox 0))))))

  (define $locked-enqueue!
    (lambda (Q lock v)
      (with-timer-off
       (spinlock-acquire lock)
       (enqueue! Q v)
       (spinlock-release lock))))

  (define $locked-dequeue!
    (lambda (Q lock)
      (with-timer-off
       (spinlock-acquire lock)
       (let ([v (dequeue! Q)])
         (spinlock-release lock)
         v))))

  (define $locked-undequeue!
    (lambda (Q lock v)
      (with-timer-off
       (spinlock-acquire lock)
       (dlist-add! Q 0 v)
       (spinlock-release lock))))

  (define $locked-dequeue-match!
    (lambda (Q lock pred)
      (with-timer-off
       (spinlock-acquire lock)
       (let ([v (if (fx= 0 (dlist-size Q))
                    #f
                    ;; TODO optimize
                    (let ([i (dlist-find-index Q pred)])
                      (if i
                          (let ([v (dlist-ref Q i)])
                            (dlist-delete! Q i)
                            v)
                          #f)))])
         (spinlock-release lock)
         v))))

  (define $locked-dequeue-filter!
    (lambda (Q lock pred)
      (with-timer-off
       (spinlock-acquire lock)
       (dlist-filter! Q pred)
       (spinlock-release lock))))


  (define $locked-drain-queue!
    (lambda (Q lock)
      (with-timer-off
       (spinlock-acquire lock)
       (let ([v* (dlist->list Q)])
         (dlist-clear! Q)
         (spinlock-release lock)
         v*))))



  #|doc
  Construct a first-class event that when synchronized,
  will return a message received from the channel `ch`.
  If there is no senders on the other side of the channel,
  the current fiber is blocked.
  |#
  (define-who channel-get-event
    (lambda (ch)
      (pcheck ([channel? ch])
              (let ([putQ (channel-putQ ch)]
                    [getQ (channel-getQ ch)]
                    [putQ-lock (channel-putQ-lock ch)]
                    [getQ-lock (channel-getQ-lock ch)]
                    [getQ-gc-counter (channel-getQ-gc-counter ch)])
                ;; called by event-sync
                (define try-fn
                  (lambda ()
                    ;; (printf "~a ~a~n" who 'try-fn)
                    (assert (= 0 (set-timer 0)))
                    (let try ()
                      (let ([item ($locked-dequeue! putQ putQ-lock)])
                        (if item
                            ;; TODO resume-put and resume-get below
                            ;; should be sth that will not disrupt the current fiber's stack
                            (let ([other-flag (vector-ref item 0)]
                                  [resume-put (vector-ref item 1)]
                                  [msg        (vector-ref item 2)])
                              (let spin ()
                                (case (abox-cas! other-flag 'W 'S)
                                  ;; sync success
                                  [W (resume-put void) (lambda () msg)]
                                  [C (spin)]
                                  [S (try)]
                                  [else (assert-unreachable)])))
                            #f)))))
                ;; called by scheduler
                (define block-fn
                  (lambda (my-flag resume)
                    ;; In this get event, it is used to fetch an item from the putQ.
                    ;; That the my-flag also exists in the putQ can happen when
                    ;; syncing on select event like (select (put-event ch) (get-event ch)).
                    (define (not-me? item)
                      (let ([other-flag (vector-ref item 0)])
                        (not (eq? my-flag other-flag))))
                    (assert (= 0 (set-timer 0)))
                    ;;(printf "~a ~a~n" who 'block-fn)
                    ;; publish
                    ($locked-enqueue! getQ getQ-lock (vector my-flag resume))
                    ;; GC
                    (when (fx= (abox-sub1&get! getQ-gc-counter) 0)
                      ($locked-dequeue-filter! getQ getQ-lock
                                               (lambda (x) (not (eq? 'S (unabox (vector-ref x 0))))))
                      (abox-set! getQ-gc-counter 42))
                    ;; try commit again
                    (let get-try ()
                      (let ([item ($locked-dequeue-match! putQ putQ-lock not-me?)])
                        (when item
                          (let ([other-flag (vector-ref item 0)]
                                [resume-put (vector-ref item 1)]
                                [msg        (vector-ref item 2)])
                            (if (eq? 'S (unabox other-flag))
                                ;; the item is already sync'ed by other fibers, try another one
                                (get-try)
                                ;; since we have published the flag, claim the flag to
                                ;; prevent ourselves from being sync'ed by others
                                (let spin ()
                                  (case (abox-cas! my-flag 'W 'C)
                                    ;; claim success, try commit
                                    [W (case (abox-cas! other-flag 'W 'S)
                                         [W
                                          ;; sync success
                                          (abox-set! my-flag 'S)
                                          (resume-put void)
                                          (resume (lambda () msg))
                                          ;; return to scheduler
                                          ]
                                         [C (abox-set! my-flag 'W)
                                            (spin)]
                                         [S (abox-set! my-flag 'W)
                                            (get-try)]
                                         [else (assert-unreachable)])]
                                    ;; we are sync'ed by others, so put the item back
                                    [S ($locked-undequeue! putQ putQ-lock item)]
                                    [else (assert-unreachable)])))))))))
                (make-base-event #f try-fn block-fn)))))


  #|doc
  Construct a first-class event that when synchronized,
  will put a message to the channel `ch`.
  If there is no receivers on the other side of the channel,
  the current fiber is blocked.
  |#
  (define-who channel-put-event
    (lambda (ch msg)
      ;; (printf "~a ~a~n" who msg)
      (pcheck ([channel? ch])
              (let ([putQ (channel-putQ ch)]
                    [getQ (channel-getQ ch)]
                    [putQ-lock (channel-putQ-lock ch)]
                    [getQ-lock (channel-getQ-lock ch)]
                    [putQ-gc-counter (channel-putQ-gc-counter ch)])
                ;; called by event-sync
                (define try-fn
                  (lambda ()
                    (assert (= 0 (set-timer 0)))
                    ;; (printf "~a ~a~n" who 'try-fn)
                    (let try ()
                      (let ([item ($locked-dequeue! getQ getQ-lock)])
                        (if item
                            (let ([other-flag (vector-ref item 0)]
                                  [resume-get (vector-ref item 1)])
                              (let spin ()
                                (case (abox-cas! other-flag 'W 'S)
                                  ;; sync success
                                  [W (resume-get (lambda () msg)) void]
                                  [C (spin)]
                                  [S (try)]
                                  [else (assert-unreachable)])))
                            #f)))))
                ;; called by scheduler
                (define block-fn
                  (lambda (my-flag resume)
                    (define (not-me? item)
                      (let ([other-flag (vector-ref item 0)])
                        (not (eq? other-flag my-flag))))
                    (assert (= 0 (set-timer 0)))
                    ;; (printf "~a ~a~n" who 'block-fn)
                    ;; publish
                    ($locked-enqueue! putQ putQ-lock (vector my-flag resume msg))
                    ;; GC
                    (when (fx= (abox-sub1&get! putQ-gc-counter) 0)
                      ($locked-dequeue-filter! putQ putQ-lock
                                               (lambda (x) (not (eq? 'S (unabox (vector-ref x 0))))))
                      (abox-set! putQ-gc-counter 42))
                    ;; try commit again
                    (let put-try ()
                      (let ([item ($locked-dequeue-match! getQ getQ-lock not-me?)])
                        (when item
                          (let ([other-flag (vector-ref item 0)]
                                [resume-get (vector-ref item 1)])
                            (if (eq? 'S (unabox other-flag))
                                (put-try)
                                ;; since we have published the flag,
                                ;; prevent ourselves from being sync'ed by others
                                (let spin ()
                                  (case (abox-cas! my-flag 'W 'C)
                                    [W (case (abox-cas! other-flag 'W 'S)
                                         [W (abox-set! my-flag 'S)
                                            (resume-get (lambda () msg))
                                            (resume void)
                                            ;; return to scheduler
                                            ]
                                         [C (abox-set! my-flag 'W)
                                            (spin)]
                                         [S (abox-set! my-flag 'W)
                                            (put-try)]
                                         [else (assert-unreachable)])]
                                    [S ($locked-undequeue! getQ getQ-lock item)]
                                    [else (assert-unreachable)])))))))))
                (make-base-event #f try-fn block-fn)))))


  (define get-operation channel-get-event)
  (define put-operation channel-put-event)


  (define $channel-get
    (lambda (who ch)
      (pcheck ([channel? ch])
              (event-sync (channel-get-event ch)))))

  (define $channel-put!
    (lambda (who ch m)
      (pcheck ([channel? ch])
              (event-sync (channel-put-event ch m)))))

  #|doc
  Get a message from channel `ch`, block if senders are available.
  |#
  (define-who channel-get
    (lambda (ch)
      ($channel-get who ch)))

  #|doc
  Put a message to channel `ch`, block is no receivers are available.
  |#
  (define-who channel-put!
    (lambda (ch m)
      ($channel-put! who ch m)))

  #|doc
  Alias of `channel-get`.
  |#
  (define-who get-message
    (lambda (ch)
      ($channel-get who ch)))

  #|doc
  Alias of `channel-put!`.
  |#
  (define-who put-message
    (lambda (ch m)
      ($channel-put! who ch m)))




  (define-record-type fiber-mutex
    (fields
     (immutable name)
     ;; abox
     (immutable flag)
     ;; dlist
     (immutable blockQ)
     ;; spinlock
     (immutable blockQ-lock))
    (protocol (lambda (new)
                (rec f
                  (case-lambda
                    [() (f 'fiber-mutex)]
                    [(name)
                     (new name (abox #f) (dlist) (make-spinlock 'fiber-mutex-Q-lock))])))))


  (define-record-type fiber-cvar
    (fields
     (immutable name)
     ;; dlist
     (immutable waitQ)
     ;; spinlock
     (immutable waitQ-lock))
    (protocol (lambda (new)
                (rec f
                  (case-lambda
                    [() (f 'fiber-cvar)]
                    [(name) (new name (dlist) (make-spinlock 'fiber-cvar-Q-lock))])))))


  #|doc
  Acquire the fiber-mutex.
  If the fiber-mutex is currently held by another fiber,
  the current fiber blocks until the fiber-mutex can be acquired.
  |#
  (define-who fiber-mutex-acquire
    (lambda (mtx)
      (event-sync (fiber-mutex-acquire-event mtx))))

  #|doc
  Release the fiber-mutex.
  An error is raised if the current fiber does not already hold the fiber-mutex.
  |#
  (define-who fiber-mutex-release
    (lambda (mtx)
      (event-sync (fiber-mutex-release-event mtx))))

  (define-who fiber-cvar-wait
    (lambda (cvar mtx)
      (event-sync (fiber-cvar-wait-event cvar mtx))))


  (define-who fiber-mutex-acquire-event
    (lambda (mtx)
      (pcheck ([fiber-mutex? mtx])
              (let ([lock        (fiber-mutex-flag   mtx)]
                    [blockQ      (fiber-mutex-blockQ mtx)]
                    [blockQ-lock (fiber-mutex-blockQ-lock mtx)])
                (define try-fn
                  (lambda ()
                    #;
                    (with-spinlock blockQ-lock
                    (printf "acq: ~a~n" (dlist-size blockQ)))
                    (if (abox-cas! lock #f #t)
                        ;; locked
                        #f
                        ;; fail
                        void)))
                (define block-fn
                  (lambda (my-flag resume)
                    ($locked-enqueue! blockQ blockQ-lock resume)))
                (make-base-event #f try-fn block-fn)))))


  (define-who fiber-mutex-release-event
    (lambda (mtx)
      (pcheck ([fiber-mutex? mtx])
              (let ([lock        (fiber-mutex-flag   mtx)]
                    [blockQ      (fiber-mutex-blockQ mtx)]
                    [blockQ-lock (fiber-mutex-blockQ-lock mtx)])
                (define try-fn
                  (lambda ()
                    (if (unabox lock)
                        ;; good, wake up one fiber
                        (let ([resume-task ($locked-dequeue! blockQ blockQ-lock)])
                          (if resume-task
                              ;; TODO return what?
                              (resume-task void)
                              (abox-set! lock #f))
                          void)
                        ;; not holding the lock
                        (errorf 'who "fiber does not own fiber-mutex ~a" (fiber-mutex-name mtx)))))
                (define block-fn (lambda (my-flag resume) (assert-unreachable)))
                (make-base-event #f try-fn block-fn)))))


  (define-syntax with-fiber-mutex
    (syntax-rules ()
      [(_ m e ...)
       (let ([mtx m])
         (fiber-mutex-acquire mtx)
         (let ([v (begin e ...)])
           (fiber-mutex-release mtx)
           v))]))


  ;; note: this is different from the cvar in PCML and Guile fiber,
  ;; where no mutex is needed, so there's no mutual exclusion.
  (define-who fiber-cvar-wait-event
    (lambda (cvar mtx)
      ;; (println who)
      (pcheck ([fiber-cvar? cvar] [fiber-mutex? mtx])
              (let ([waitQ       (fiber-cvar-waitQ cvar)]
                    [waitQ-lock  (fiber-cvar-waitQ-lock cvar)]
                    [lock        (fiber-mutex-flag   mtx)]
                    [blockQ      (fiber-mutex-blockQ mtx)]
                    [blockQ-lock (fiber-mutex-blockQ-lock mtx)])
                (define try-fn
                  (lambda ()
                    #;
                    (with-spinlock waitQ-lock
                    (printf "wait: ~a~n" (dlist-size waitQ)))
                    (if (unabox lock)
                        ;; "release" the lock
                        (let ([resume-task ($locked-dequeue! blockQ blockQ-lock)])
                          (if resume-task
                              (resume-task void)
                              (abox-set! lock #f))
                          ;; go to block-fn
                          #f)
                        ;; not holding the lock
                        (errorf 'who "fiber does not own fiber-mutex ~a" (fiber-mutex-name mtx)))))
                (define block-fn
                  (lambda (my-flag resume)
                    (define resume1 (lambda ()
                                      ;; reacquire the mtx when woken up,
                                      ;; which may fail
                                      (resume (lambda () (fiber-mutex-acquire mtx)))))
                    ($locked-enqueue! waitQ waitQ-lock resume1)))
                (make-base-event #f try-fn block-fn)))))

  (define-who fiber-cvar-signal
    (lambda (cvar)
      (pcheck ([fiber-cvar? cvar])
              (let ([waitQ       (fiber-cvar-waitQ cvar)]
                    [waitQ-lock  (fiber-cvar-waitQ-lock cvar)])
                (let ([resume ($locked-dequeue! waitQ waitQ-lock)])
                  (if resume
                      (with-timer-off (resume))
                      ))))))

  (define-who fiber-cvar-broadcast
    (lambda (cvar)
      ;; (println who)
      (pcheck ([fiber-cvar? cvar])
              (let ([waitQ       (fiber-cvar-waitQ cvar)]
                    [waitQ-lock  (fiber-cvar-waitQ-lock cvar)])
                (let ([resumes ($locked-drain-queue! waitQ waitQ-lock)])
                  (with-timer-off
                   (for-each (lambda (x) (x)) resumes)))))))



;;;; time ops


  (define-who time-event
    (lambda ()
      (todo)))

  #|doc
  Construct a first-class event that when synchronized,
  will make the current fiber sleep at least `t` miliseconds.
  |#
  (define-who milisleep-event
    (lambda (t)
      (pcheck ([natural? t])
              (define try-fn (lambda () #f))
              (define block-fn
                (lambda (my-flag resume)
                  (todo)))
              (make-base-event #f try-fn block-fn))))

  #|doc
  Construct a first-class event that when synchronized,
  will make the current fiber sleep at least `t` seconds.
  |#
  (define-who sleep-event
    (lambda (t)
      (pcheck ([natural? t])
              (define try-fn (lambda () #f))
              (define block-fn
                (lambda (my-flag resume)
                  (todo)))
              (make-base-event #f try-fn block-fn))))

  #|doc
  Make the current fiber sleep at least `t` miliseconds.
  |#
  (define-who fiber-milisleep
    (lambda (t)
      (event-sync (sleep-event t))))

  #|doc
  Make the current fiber sleep at least `t` seconds.
  |#
  (define-who fiber-sleep
    (lambda (t)
      (event-sync (sleep-event t))))




  (record-writer (type-descriptor base-event)
                 (lambda (r p wr)
                   (display "#<base-event>" p)))

  (record-writer (type-descriptor select-event)
                 (lambda (r p wr)
                   (display "#<select-event " p)
                   (wr (vector-length (select-event-events r)) p)
                   (display ">" p)))

  (record-writer (type-descriptor channel)
                 (lambda (r p wr)
                   (display "#<channel>" p)))

  (record-writer (type-descriptor fiber-mutex)
                 (lambda (r p wr)
                   (display "#<fiber-mutex " p)
                   (wr (fiber-mutex-name r) p)
                   (display ">" p)))

  (%net-operation-wait-hook
   (lambda (operation blocking-wait)
     (if current-scheduler
         (event-sync (net-operation-event operation))
         (blocking-wait operation))))

  )
