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
            (mutable terminal-error?)
            (mutable terminal-error)
            park-mutex park-condition
            (mutable runnable-count)
            (mutable transactions)
            (mutable timers)))

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
    (lambda (context error? error)
      ;; The hierarchy is event mutex -> park mutex -> queue spinlock. No park-held
      ;; path enters the event protocol, and no condition wait holds the event mutex.
      (with-event-protocol
        (let ([mutex (fiber-run-context-park-mutex context)])
          (mutex-acquire mutex)
          (unless (context-stopped? context)
            ;; Raised objects may be #f, so error presence is stored separately.
            (when error?
              (fiber-run-context-terminal-error?-set! context #t)
              (fiber-run-context-terminal-error-set! context error))
            (abox-set! (fiber-run-context-stop-flag context) #t))
          (condition-broadcast (fiber-run-context-park-condition context))
          (mutex-release mutex))
        ;; Cleanups only detach registrations; they never invoke user code or reenter
        ;; this mutex. Stop is already visible before any registration is detached.
        (for-each cancel-sync-transaction! (fiber-run-context-transactions context)))))

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
          (request-context-stop! (scheduler-context current-scheduler) #f #f))
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

  (define time-duration-expired?
    (lambda (duration)
      (let ([seconds (time-second duration)]
            [nanoseconds (time-nanosecond duration)])
        (or (< seconds 0)
            (and (= seconds 0) (<= nanoseconds 0))))))

  (define-record-type fiber-timer
    (fields deadline transaction resume))

  ;; The timer list is ordered by absolute monotonic deadline. The caller holds park.
  (define timer-before?
    (lambda (left right)
      (time-duration-expired?
       (time-difference (fiber-timer-deadline left) (fiber-timer-deadline right)))))

  (define insert-timer-locked!
    (lambda (context timer)
      (let loop ([before '()] [rest (fiber-run-context-timers context)])
        (if (or (null? rest) (timer-before? timer (car rest)))
            (fiber-run-context-timers-set!
             context (append (reverse before) (cons timer rest)))
            (loop (cons (car rest) before) (cdr rest))))))

  (define remove-timer-locked!
    (lambda (context timer)
      (fiber-run-context-timers-set!
       context (remq timer (fiber-run-context-timers context)))))

  (define expire-timers-locked!
    (lambda (context now)
      (let loop ([rest (fiber-run-context-timers context)] [expired '()])
        (if (and (pair? rest)
                 (time-duration-expired?
                  (time-difference (fiber-timer-deadline (car rest)) now)))
            (loop (cdr rest) (cons (car rest) expired))
            (begin
              (fiber-run-context-timers-set! context rest)
              (reverse expired))))))

  ;; Expiry commits only after park is released; event protocol is acquired by caller.
  (define commit-expired-timers!
    (lambda (expired)
      (unless (null? expired)
        (with-event-protocol
          (for-each (lambda (timer) ((fiber-timer-resume timer))) expired)))))

  (define await-task!
    (lambda (sched)
      (let* ([context (scheduler-context sched)]
             [mutex (fiber-run-context-park-mutex context)]
             [condition (fiber-run-context-park-condition context)])
        (mutex-acquire mutex)
        (let loop ()
          (cond
           [(context-stopped? context) (mutex-release mutex) #f]
           [else
            (let ([expired (expire-timers-locked! context (current-time 'time-monotonic))])
              (if (pair? expired)
                  (begin
                    (mutex-release mutex)
                    (commit-expired-timers! expired)
                    (mutex-acquire mutex)
                    (loop))
                  (if (fx> (fiber-run-context-runnable-count context) 0)
                       (let ([task (get-or-steal-task-locked! sched)])
                         (assert task)
                         (fiber-run-context-runnable-count-set!
                          context (fx1- (fiber-run-context-runnable-count context)))
                         (mutex-release mutex)
                         task)
                       (let ([timers (fiber-run-context-timers context)])
                         ;; condition-wait releases this mutex atomically and reacquires it once.
                         ;; Every wake rechecks shutdown, timers, and runnable work.
                         (if (null? timers)
                             (condition-wait condition mutex)
                             (let ([duration
                                    (time-difference
                                     (fiber-timer-deadline (car timers))
                                     (current-time 'time-monotonic))])
                               (unless (time-duration-expired? duration)
                                 (condition-wait condition mutex duration))))
                         (loop)))))])))))

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
                               [else (request-context-stop! context #t condition)])
                         (value))
                       (set! current-fiber-task #f)]
                      [(error) (request-context-stop! context #t value)]
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
              (request-context-stop! context #f #f))
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
                             #f (abox #f) (list (void)) #f #f
                             (make-mutex 'fiber-run-context)
                             (make-condition 'fiber-run-condition) 0 '() '())]
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
                (if (fiber-run-context-terminal-error? context)
                    (raise (fiber-run-context-terminal-error context))
                    (apply values (fiber-run-context-init-values context)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   first-class events
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define-record-type event (fields))

  ;; All event poll/register/commit/cleanup operations hold this mutex. One global
  ;; protocol also makes rendezvous between independent run contexts atomic, without
  ;; acquiring two park mutexes. It serializes short queue operations, never user code.
  (define event-protocol-mutex (make-mutex 'fiber-event-protocol))

  (define-syntax with-event-protocol
    (syntax-rules ()
      [(_ expression ...)
       (with-timer-off
         (mutex-acquire event-protocol-mutex)
         (call-with-values
          (lambda ()
            (guard (condition
                    [else (mutex-release event-protocol-mutex) (raise condition)])
              expression ...))
          (lambda results
            (mutex-release event-protocol-mutex)
            (apply values results))))]))

  (define-record-type sync-transaction
    (fields context resume (mutable state) (mutable cleanups)))

  (define sync-transaction-live?
    (lambda (transaction)
      (and (eq? 'waiting (sync-transaction-state transaction))
           (not (context-stopped? (sync-transaction-context transaction))))))

  ;; Each cleanup is installed before publishing its registration. Finishing first
  ;; clears the cleanup list, so cleanup and cancellation are idempotent. Callers hold
  ;; event-protocol-mutex; cleanups must only unlink storage and never reacquire it.
  (define add-sync-cleanup!
    (lambda (transaction cleanup)
      (sync-transaction-cleanups-set!
       transaction (cons cleanup (sync-transaction-cleanups transaction)))))

  (define finish-sync-transaction!
    (lambda (transaction state)
      (when (eq? 'waiting (sync-transaction-state transaction))
        (sync-transaction-state-set! transaction state)
        (let ([context (sync-transaction-context transaction)]
              [cleanups (sync-transaction-cleanups transaction)])
          (sync-transaction-cleanups-set! transaction '())
          (fiber-run-context-transactions-set!
           context (remq transaction (fiber-run-context-transactions context)))
          (for-each (lambda (cleanup) (cleanup)) cleanups)))))

  (define cancel-sync-transaction!
    (lambda (transaction) (finish-sync-transaction! transaction 'cancelled)))

  (define commit-sync-transaction!
    (lambda (transaction thunk)
      (and (sync-transaction-live? transaction)
           (begin
             (finish-sync-transaction! transaction 'committed)
             ;; Resume only publishes a task: wrappers and result thunks run later,
             ;; outside this mutex. put-task! owns park before its queue spinlock.
             ((sync-transaction-resume transaction) thunk)
             #t))))

  (define-record-type base-event
    (parent event)
    (sealed #t)
    (fields wrap-fn
            ;; unit -> result thunk or #f, under the event protocol mutex
            try-fn
            ;; transaction -> committing resume -> unit, under the same mutex
            block-fn))

  (define-record-type select-event
    (parent event)
    (sealed #t)
    ;; Immutable vector of base events; wrapping always allocates a new graph.
    (fields events))

  #|proc:event-never
  The `event-never` procedure returns a disabled event. Synchronizing it waits forever,
  unless the run context stops or another branch of a selection succeeds.
  |#
  (define event-never
    (let ([event (make-base-event #f (lambda () #f)
                                  (lambda (transaction resume) (void)))])
      (lambda () (pcheck () event))))

  #|proc:event-always
  The `event-always` procedure returns an event that immediately synchronizes with value `value`.
  |#
  (define event-always
    (lambda (value)
      (pcheck ()
        (make-base-event #f (lambda () (lambda () value))
                         (lambda (transaction resume) (assert-unreachable))))))

  #|proc:event-wrap
  The `event-wrap` procedure returns a new event wrapping `event`, leaving `event` unchanged.
  After synchronization, `procedure` receives all event result values as its arguments and its
  return values become the synchronization results. Repeated wrappers run in construction order.
  |#
  (define-who event-wrap
    (lambda (event procedure)
      (pcheck ([event? event] [procedure? procedure])
        (if (select-event? event)
            (make-select-event
             (vector-map (lambda (base) (event-wrap base procedure))
                         (select-event-events event)))
            (let ([wrap (base-event-wrap-fn event)])
              (make-base-event
               (if wrap
                   (lambda arguments
                     (call-with-values (lambda () (apply wrap arguments)) procedure))
                   procedure)
               (base-event-try-fn event) (base-event-block-fn event)))))))

  (define all-events? (lambda (events) (andmap event? events)))

  #|proc:event-select
  The `event-select` procedure returns an event choosing one of `events` nondeterministically.
  Each argument must be an event. Nested selections are flattened without changing their sources.
  With no arguments, it returns a disabled event equivalent to `(event-never)`.
  |#
  (define-who event-select
    (lambda events
      (pcheck ([all-events? events])
        (if (null? events)
            (event-never)
            (make-select-event
             (list->vector
              (apply append
                     (map (lambda (event)
                            (if (base-event? event)
                                (list event)
                                (vector->list (select-event-events event))))
                          events))))))))

  #|proc:event-sync
  The `event-sync` procedure synchronizes the current fiber on event `event`. It returns the
  event's synchronization values after applying wrappers in their construction order.
  A blocked synchronization parks the fiber without blocking its scheduler worker. Successful
  selection and context shutdown detach all of that synchronization's losing registrations.
  |#
  (define-who event-sync
    (lambda (event)
      (pcheck ([event? event])
        (unless current-scheduler (errorf who "not running scheduler"))
        (let ([scheduler current-scheduler] [task current-fiber-task])
          (define wrap-result
            (lambda (base thunk)
              (let ([wrap (base-event-wrap-fn base)])
                (if wrap (lambda () (call-with-values thunk wrap)) thunk))))
          (set-timer 0)
          ;; A synchronization continuation is committed and resumed at most once.
          ((call/1cc
            (lambda (continuation)
              (scheduler-return
               'callback
               (lambda ()
                 (with-event-protocol
                   (let* ([context (scheduler-context scheduler)]
                          [transaction
                           (make-sync-transaction
                            context
                            (lambda (thunk)
                              (put-task! scheduler
                                         (continue-task task
                                           (lambda () (continuation thunk)))))
                            'waiting '())]
                          [bases (if (base-event? event)
                                     (vector event) (select-event-events event))]
                          [length (vector-length bases)]
                          [start (fxmod (random 99999) length)])
                     (unless (context-stopped? context)
                       (fiber-run-context-transactions-set!
                        context (cons transaction (fiber-run-context-transactions context)))
                       ;; Poll all branches before registering any. The same mutex covers
                       ;; the second phase, closing the poll-to-registration wakeup gap.
                       (let poll ([index 0])
                         (if (fx= index length)
                             (vector-for-each
                              (lambda (base)
                                (when (sync-transaction-live? transaction)
                                  ((base-event-block-fn base)
                                   transaction
                                   (lambda (thunk)
                                     (commit-sync-transaction!
                                      transaction (wrap-result base thunk))))))
                              bases)
                             (let* ([base (vector-ref bases (fxmod (fx+ start index) length))]
                                    [thunk ((base-event-try-fn base))])
                               (if thunk
                                   (commit-sync-transaction! transaction (wrap-result base thunk))
                                   (poll (fx1+ index))))))))))))))))))

  #|proc:wrap-operation
  Alias of `event-wrap`. Accepts an event and a procedure receiving its synchronization values.
  Returns a new event whose synchronization returns the procedure's result values.
  |#
  (define wrap-operation event-wrap)

  #|proc:choice-operation
  Alias of `event-select`. Accepts zero or more events and returns an event choosing one of them.
  No arguments returns a disabled event.
  |#
  (define choice-operation event-select)

  #|proc:perform-operation
  Alias of `event-sync`. Accepts an event and returns its synchronization values after waiting
  for it to become ready and applying its wrappers.
  |#
  (define perform-operation event-sync)

  ;;;;===----------------------------------------------------------------------===
  ;;;; Network operation events
  ;;;;===----------------------------------------------------------------------===

  ;; The existing four-worker driver and bounded pools remain a compatibility path.
  ;; Pool ownership and waiter membership use event-protocol-mutex. Operation advance
  ;; and result inspection use ordinary network mutexes only after releasing it.
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
      ;; Context liveness and scheduler liveness are separate from transaction state.
      (and (sync-transaction-live? (vector-ref waiter 0))
           (not (context-stopped? (vector-ref waiter 3)))
           (scheduler-running? (vector-ref waiter 4)))))

  (define detach-net-job-locked!
    (lambda (job)
      ;; A claimed job cannot be recycled while a driver still holds its identity.
      (assert (not (vector-ref job 2)))
      (assert (null? (vector-ref job 1)))
      (set! net-driver-job* (remq job net-driver-job*))
      (vector-set! job 0 #f)
      (set! net-driver-job-free (cons job net-driver-job-free))))

  (define remove-net-waiter-locked!
    (lambda (job waiter)
      (when (memq waiter (vector-ref job 1))
        (vector-set! job 1 (remq waiter (vector-ref job 1)))
        (release-net-waiter-locked! waiter)
        (when (and (null? (vector-ref job 1)) (not (vector-ref job 2)))
          (detach-net-job-locked! job)))))

  (define snapshot-net-result
    (lambda (operation)
      (case (net-operation-state operation)
        [(completed)
         (let ([result (net-operation-result operation)]) (lambda () result))]
        [(failed cancelled)
         (let ([failure (net-operation-condition operation)]) (lambda () (raise failure)))]
        [else #f])))

  (define drive-net-job!
    (lambda (job operation)
      ;; The job is claimed, but no event/park/queue lock is held over user callbacks.
      (guard (ignored [else (void)])
        (when (eq? 'pending (net-operation-state operation))
          (net-operation-step! operation)))
      (let ([result (snapshot-net-result operation)])
        (with-event-protocol
          (when result
            ;; Capture delivery data before any commit recycles pooled vectors. One
            ;; transaction may have several branches registered on this same job.
            (let ([deliveries
                   (map (lambda (waiter)
                          (and (net-waiter-live? waiter)
                               (vector (vector-ref waiter 0) (vector-ref waiter 1)
                                       (vector-ref waiter 2))))
                        (vector-ref job 1))])
              (for-each
               (lambda (delivery)
                 (when (and delivery (sync-transaction-live? (vector-ref delivery 0)))
                   (abox-set! (vector-ref delivery 2) result)
                   ((vector-ref delivery 1) result)))
               deliveries)))
          (vector-set! job 2 #f)
          (when (null? (vector-ref job 1)) (detach-net-job-locked! job))))))

  (define drive-net-operations
    (lambda ()
      (set! current-scheduler #f)
      (set! current-fiber-task #f)
      (let loop ()
        (let ([claim
               (with-event-protocol
                 (if (null? net-driver-job*)
                     (begin (set! net-driver-workers (fx1- net-driver-workers)) 'stop)
                     (let ([job (find (lambda (job) (not (vector-ref job 2)))
                                      net-driver-job*)])
                       (and job
                            (begin
                              (vector-set! job 2 #t)
                              ;; Rotate claims so a pending job cannot starve later work.
                              (set! net-driver-job*
                                    (append (remq job net-driver-job*) (list job)))
                              (cons job (vector-ref job 0)))))))])
          (unless (eq? claim 'stop)
            (when claim (drive-net-job! (car claim) (cdr claim)))
            (milisleep 1)
            (loop))))))

  #|proc:net-operation-event-pool-metrics
  The `net-operation-event-pool-metrics` procedure returns a fresh vector of driver job count,
  active waiter count, and waiter high-water count. Four workers serve up to 64 jobs and
  256 waiters.
  |#
  (define net-operation-event-pool-metrics
    (lambda ()
      (pcheck ()
        (with-event-protocol
          (vector (length net-driver-job*) net-driver-waiters-in-use
                  net-driver-waiter-high-water)))))

  #|proc:net-operation-event
  The `net-operation-event` procedure creates a fiber event for network `operation`.
  Synchronization drives the operation on a bounded OS worker pool and returns its result.
  The original failure or cancellation condition is raised when the fiber resumes.
  Losing choices and context shutdown immediately detach their waiters. An already running
  driver callback may finish, but cleanup never cancels a shared operation owned by the caller.
  |#
  (define net-operation-event
    (lambda (operation)
      (pcheck ([net-operation? operation])
        (let ([result (abox #f)])
          (define try (lambda () (unabox result)))
          (define block
            (lambda (transaction resume)
              (let ([job (find (lambda (job) (eq? operation (vector-ref job 0)))
                               net-driver-job*)])
                (if (and (pair? net-driver-waiter-free)
                         (or job (pair? net-driver-job-free)))
                    (begin
                      (unless job
                        (set! job (car net-driver-job-free))
                        (set! net-driver-job-free (cdr net-driver-job-free))
                        (vector-set! job 0 operation)
                        (set! net-driver-job* (cons job net-driver-job*)))
                      (let ([waiter (car net-driver-waiter-free)])
                        (set! net-driver-waiter-free (cdr net-driver-waiter-free))
                        (vector-set! waiter 0 transaction)
                        (vector-set! waiter 1 resume)
                        (vector-set! waiter 2 result)
                        (vector-set! waiter 3 (scheduler-context current-scheduler))
                        (vector-set! waiter 4 current-scheduler)
                        (add-sync-cleanup!
                         transaction (lambda () (remove-net-waiter-locked! job waiter)))
                        (vector-set! job 1 (cons waiter (vector-ref job 1)))
                        (set! net-driver-waiters-in-use (fx1+ net-driver-waiters-in-use))
                        (set! net-driver-waiter-high-water
                              (fxmax net-driver-waiter-high-water net-driver-waiters-in-use)))
                      (let start-workers ()
                        (when (fx< net-driver-workers 4)
                          (set! net-driver-workers (fx1+ net-driver-workers))
                          (fork-thread drive-net-operations)
                          (start-workers))))
                    (resume (lambda () (errorf 'net-operation-event
                                                "network event waiter pool exhausted")))))))
          (make-base-event #f try block)))))


  (define-record-type (channel %make-channel channel?)
    (fields putQ getQ))

  #|proc:make-channel
  The `make-channel` procedure returns a new synchronous channel for rendezvous between fibers.
  |#
  (define make-channel
    (lambda () (pcheck () (%make-channel (make-dlist) (make-dlist)))))

  ;; Channel, mutex, and cvar queues share #(transaction resume payload) entries.
  ;; Their caller holds event-protocol-mutex while taking or registering a waiter.
  ;; Exclude the calling transaction when matching opposite channel branches.
  (define take-sync-waiter!
    (lambda (queue transaction)
      (let loop ()
        (let ([index (dlist-find-index queue
                       (lambda (item) (not (eq? transaction (vector-ref item 0)))))])
          (and index
               (let ([item (dlist-ref queue index)])
                 (dlist-delete! queue index)
                 (if (sync-transaction-live? (vector-ref item 0)) item (loop))))))))

  (define register-sync-waiter!
    (lambda (queue transaction resume message)
      (let ([item (vector transaction resume message)])
        (add-sync-cleanup!
         transaction
         (lambda ()
           (let ([index (dlist-find-index queue (lambda (entry) (eq? entry item)))])
             (when index (dlist-delete! queue index)))))
        (enqueue! queue item))))

  #|proc:channel-get-event
  The `channel-get-event` procedure returns an event receiving a message from channel `channel`.
  Synchronization returns the sender's message, waiting if no sender is available.
  |#
  (define-who channel-get-event
    (lambda (channel)
      (pcheck ([channel? channel])
        (define try
          (lambda ()
            (let ([item (take-sync-waiter! (channel-putQ channel) #f)])
              (and item
                   (begin ((vector-ref item 1) void)
                          (let ([message (vector-ref item 2)]) (lambda () message)))))))
        (define block
          (lambda (transaction resume)
            (let ([item (take-sync-waiter! (channel-putQ channel) transaction)])
              (if item
                  (begin
                    ((vector-ref item 1) void)
                    (resume (let ([message (vector-ref item 2)]) (lambda () message))))
                  (register-sync-waiter! (channel-getQ channel) transaction resume #f)))))
        (make-base-event #f try block))))

  #|proc:channel-put-event
  The `channel-put-event` procedure returns an event sending `message` on channel `channel`.
  Synchronization waits for a receiver and returns an unspecified value after handing it
  the message.
  |#
  (define-who channel-put-event
    (lambda (channel message)
      (pcheck ([channel? channel])
        (define try
          (lambda ()
            (let ([item (take-sync-waiter! (channel-getQ channel) #f)])
              (and item
                   (begin ((vector-ref item 1) (lambda () message)) void)))))
        (define block
          (lambda (transaction resume)
            (let ([item (take-sync-waiter! (channel-getQ channel) transaction)])
              (if item
                  (begin ((vector-ref item 1) (lambda () message)) (resume void))
                  (register-sync-waiter! (channel-putQ channel) transaction resume message)))))
        (make-base-event #f try block))))

  #|proc:get-operation
  Alias of `channel-get-event`. Accepts a channel and returns an event whose synchronization
  receives and returns a message from that channel.
  |#
  (define get-operation channel-get-event)

  #|proc:put-operation
  Alias of `channel-put-event`. Accepts a channel and any message, and returns an event sending
  that message. Its synchronization waits for a receiver and returns an unspecified value.
  |#
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



;;;;===----------------------------------------------------------------------===
;;;; Fiber mutexes and condition variables
;;;;===----------------------------------------------------------------------===

  ;; Ownership and both wait queues are guarded by event-protocol-mutex. Tokens are
  ;; stable across continuation migration; OS thread identity does not own a mutex.
  (define-record-type (fiber-mutex %make-fiber-mutex fiber-mutex?)
    (fields name (mutable owner) blockQ))

  (define-record-type (fiber-cvar %make-fiber-cvar fiber-cvar?)
    (fields name waitQ))

  #|proc:make-fiber-mutex
  The `make-fiber-mutex` procedure returns a new, initially unlocked fiber mutex.
  The optional `name` parameter is any object used as a diagnostic label; its default is
  the symbol `fiber-mutex`. Mutex ownership belongs to a fiber and survives yielding.
  |#
  (define make-fiber-mutex
    (case-lambda
      [() (make-fiber-mutex 'fiber-mutex)]
      [(name) (pcheck () (%make-fiber-mutex name #f (make-dlist)))]))

  #|proc:make-fiber-cvar
  The `make-fiber-cvar` procedure returns a new fiber condition variable with no waiters.
  The optional `name` parameter is any object used as a diagnostic label; its default is
  the symbol `fiber-cvar`. A condition variable retains no signal when no fiber is waiting.
  |#
  (define make-fiber-cvar
    (case-lambda
      [() (make-fiber-cvar 'fiber-cvar)]
      [(name) (pcheck () (%make-fiber-cvar name (make-dlist)))]))

  (define require-fiber-token
    (lambda (who)
      (unless (and current-scheduler current-fiber-task)
        (errorf who "not running a fiber"))
      (fiber-task-token current-fiber-task)))

  ;; The caller holds event-protocol-mutex for ownership validation and transfer.
  (define check-fiber-mutex-owner!
    (lambda (who mutex token)
      (unless (eq? token (fiber-mutex-owner mutex))
        (errorf who "fiber does not own fiber-mutex ~a" (fiber-mutex-name mutex)))))

  (define release-fiber-mutex-locked!
    (lambda (who mutex token)
      (check-fiber-mutex-owner! who mutex token)
      (let ([waiter (take-sync-waiter! (fiber-mutex-blockQ mutex) #f)])
        (if waiter
            (begin
              ;; Transfer before publication, so another acquisition cannot barge in.
              (fiber-mutex-owner-set! mutex (vector-ref waiter 2))
              ((vector-ref waiter 1) void))
            (fiber-mutex-owner-set! mutex #f)))
      (void)))

  #|proc:fiber-mutex-acquire
  The `fiber-mutex-acquire` procedure acquires fiber mutex `mutex` for the current fiber.
  If it is already held, the fiber waits without blocking its scheduler worker. Acquisition
  is not recursive. It returns an unspecified value after becoming the owner.
  |#
  (define-who fiber-mutex-acquire
    (lambda (mutex)
      (pcheck ([fiber-mutex? mutex])
        (let ([token (require-fiber-token who)])
          (event-sync
           (make-base-event
            #f
            (lambda ()
              (and (not (fiber-mutex-owner mutex))
                   (begin (fiber-mutex-owner-set! mutex token) void)))
            (lambda (transaction resume)
              (register-sync-waiter! (fiber-mutex-blockQ mutex)
                                     transaction resume token))))))))

  #|proc:fiber-mutex-release
  The `fiber-mutex-release` procedure releases fiber mutex `mutex`, transferring ownership
  to a waiting fiber if one exists. It raises an error unless the current fiber owns `mutex`.
  It returns an unspecified value.
  |#
  (define-who fiber-mutex-release
    (lambda (mutex)
      (pcheck ([fiber-mutex? mutex])
        (let ([token (require-fiber-token who)])
          (with-event-protocol (release-fiber-mutex-locked! who mutex token))))))

  #|macro:with-fiber-mutex
  The `with-fiber-mutex` macro evaluates `mutex` once, acquires that fiber mutex, and evaluates
  `expression ...` while owning it. Yielding preserves ownership. Normal return releases the
  mutex and returns all expression values; an exception releases it and raises the same object.
  |#
  (define-syntax with-fiber-mutex
    (syntax-rules ()
      [(_ mutex expression ...)
       (let ([held-mutex mutex])
         (fiber-mutex-acquire held-mutex)
         (call-with-values
          (lambda ()
            ;; A continuation yield must not run a dynamic-wind exit/entry cleanup.
            (guard (condition
                    [else (fiber-mutex-release held-mutex) (raise condition)])
              expression ...))
          (lambda results
            (fiber-mutex-release held-mutex)
            (apply values results))))]))

  #|proc:fiber-cvar-wait
  The `fiber-cvar-wait` procedure waits on condition variable `cvar` with fiber mutex `mutex`.
  The current fiber must own `mutex`. Waiting registers the fiber and releases the mutex
  atomically. A signal or broadcast wakes the fiber, which reacquires `mutex` before returning
  an unspecified value. Callers should recheck their protected predicate after every wakeup.
  |#
  (define-who fiber-cvar-wait
    (lambda (cvar mutex)
      (pcheck ([fiber-cvar? cvar] [fiber-mutex? mutex])
        (let ([token (require-fiber-token who)])
          ;; Report invalid ownership on the calling fiber, where its guard can catch it.
          (with-event-protocol (check-fiber-mutex-owner! who mutex token))
          (event-sync
           (make-base-event
            #f (lambda () #f)
            (lambda (transaction resume)
              ;; Publish before unlocking, under the same protocol used by signal/broadcast.
              (register-sync-waiter!
               (fiber-cvar-waitQ cvar) transaction
               (lambda (ignored)
                 ;; Reacquisition executes on the resumed fiber, outside the protocol mutex.
                 (resume (lambda () (fiber-mutex-acquire mutex))))
               #f)
              (release-fiber-mutex-locked! who mutex token))))))))

  #|proc:fiber-cvar-signal
  The `fiber-cvar-signal` procedure wakes one fiber waiting on condition variable `cvar`, if any.
  It returns an unspecified value; the awakened fiber reacquires its mutex before returning.
  |#
  (define-who fiber-cvar-signal
    (lambda (cvar)
      (pcheck ([fiber-cvar? cvar])
        (with-event-protocol
          (let ([waiter (take-sync-waiter! (fiber-cvar-waitQ cvar) #f)])
            (when waiter ((vector-ref waiter 1) void)))
          (void)))))

  #|proc:fiber-cvar-broadcast
  The `fiber-cvar-broadcast` procedure wakes every fiber waiting on condition variable `cvar`.
  It returns an unspecified value; each awakened fiber reacquires its mutex before returning.
  |#
  (define-who fiber-cvar-broadcast
    (lambda (cvar)
      (pcheck ([fiber-cvar? cvar])
        (with-event-protocol
          (let loop ()
            (let ([waiter (take-sync-waiter! (fiber-cvar-waitQ cvar) #f)])
              (when waiter
                ((vector-ref waiter 1) void)
                (loop))))
          (void)))))



;;;; time ops


  (define make-timer-event
    (lambda (duration)
      (make-base-event
       #f
       (lambda ()
         (if (time-duration-expired? duration)
             (lambda () (void))
             #f))
       (lambda (transaction resume)
         (let* ([context (sync-transaction-context transaction)]
                [deadline (add-duration (current-time 'time-monotonic) duration)]
                [timer #f])
           (set! timer (make-fiber-timer
                        deadline transaction
                        ;; Resume through the event protocol callback so wrappers and
                        ;; selections apply their result transformation before wakeup.
                        (lambda () (resume (lambda () (void))))))
           ;; Cleanup runs under event-protocol and may briefly acquire park.
           (add-sync-cleanup!
            transaction
            (lambda ()
              (let ([mutex (fiber-run-context-park-mutex context)])
                (mutex-acquire mutex)
                (remove-timer-locked! context timer)
                (mutex-release mutex))))
           (let ([mutex (fiber-run-context-park-mutex context)])
             (mutex-acquire mutex)
             (if (sync-transaction-live? transaction)
                 (begin
                   (insert-timer-locked! context timer)
                   ;; Wake a worker if this became the earliest deadline.
                   (condition-signal (fiber-run-context-park-condition context)))
                 (void))
             (mutex-release mutex)))))))

  #|proc:milisleep-event
  The `milisleep-event` procedure returns an event that completes after at least `t`
  milliseconds. `t` is a natural number and zero completes immediately.
  |#
  (define-who milisleep-event
    (lambda (t)
      (pcheck ([natural? t])
        (make-timer-event
         (make-time 'time-duration
                    (* (modulo t 1000) 1000000)
                    (quotient t 1000))))))

  #|proc:sleep-event
  The `sleep-event` procedure returns an event that completes after at least `t` seconds.
  `t` is a natural number and zero completes immediately.
  |#
  (define-who sleep-event
    (lambda (t)
      (pcheck ([natural? t])
        (make-timer-event (make-time 'time-duration 0 t)))))

  #|proc:fiber-milisleep
  The `fiber-milisleep` procedure suspends the current fiber for at least `t` milliseconds.
  `t` is a natural number; the return value is unspecified.
  |#
  (define-who fiber-milisleep
    (lambda (t)
      (pcheck ([natural? t]) (event-sync (milisleep-event t)))))

  #|proc:fiber-sleep
  The `fiber-sleep` procedure suspends the current fiber for at least `t` seconds.
  `t` is a natural number; the return value is unspecified.
  |#
  (define-who fiber-sleep
    (lambda (t)
      (pcheck ([natural? t]) (event-sync (sleep-event t)))))




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
