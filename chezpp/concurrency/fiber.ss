(library (chezpp concurrency fiber)
  (export run-fibers spawn-fiber
          fiber-quit fiber-yield

          event-never event-always
          event-wrap event-select event-sync
          wrap-operation choice-operation perform-operation

          make-channel
          channel-get-event channel-put-event get-operation put-operation
          channel-get channel-put!
          get-message put-message

          make-fiber-mutex fiber-mutex-acquire fiber-mutex-release

          make-fiber-cvar fiber-cvar-wait fiber-cvar-signal fiber-cvar-broadcast

          with-timer-off with-fiber-mutex)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp dlist)
          (chezpp concurrency)
          (chezpp os)
          (chezpp io)
          (chezpp internal))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   fiber core
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define *default-ticks* 3000)
  ;; vector of all schedulers
  (define *scheds* #f)

  ;; even non-fiber threads will inherit a copy of these
  (define-threaded current-scheduler #f)
  (define-threaded *timer* (timer-interrupt-handler))
  (define-threaded *preemption-count* 0)


  (define enqueue!
    (lambda (Q v)
      (dlist-add! Q v)))


  (define dequeue!
    (lambda (Q)
      ;; (printf "remaining ~a tasks~n" (dlist-length Q))
      (if (dlist-empty? Q)
          #f
          (let ([v (dlist-ref Q 0)])
            (dlist-delete! Q 0)
            v))))


  (define-record-type scheduler
    (fields i (mutable running?)
            rq
            ;; spinlock
            rq-lock
            (mutable thread)
            (mutable k)))


  #|doc
  Evaluate the expression e, ... with timed preemption turned off.
  Timed preemption is turn on again after the expression are evaluated.
  The value of `with-timer-off` is the value of the last expression in e, ...
  |#
  (define-syntax with-timer-off
    (syntax-rules ()
      [(_ e ...)
       ;; Must write nested let because the evaluation order of rhs in the
       ;; same let is NOT left to right.
       (let ([t (set-timer 0)])
         (let ([v (begin e ...)])
           (set-timer t)
           v))]))


  ;; TODO other things to worry about?
  #|doc
  Terminate the execution of the current fiber.
  |#
  (define-who fiber-quit
    (lambda ()
      (unless current-scheduler
        (errorf who "not running scheduler"))
      ;; TODO restore timer handler?
      ;; TODO restore *sched-k*?
      (set-timer 0)
      ;; RET
      ((scheduler-k current-scheduler) 'quit #f)))

  #|doc
  Yield control of the current fiber,
  which will be run some time later.
  |#
  (define-who fiber-yield
    (lambda ()
      (unless current-scheduler
        (errorf who "not running scheduler"))
      (call/cc
       (lambda (k)
         (set-timer 0)
         ;; RET
         ((scheduler-k current-scheduler) 'yield k)))))


  (define get-task!
    (case-lambda
      [()
       (get-task! current-scheduler)]
      [(sched)
       (assert (= 0 (set-timer 0)))
       (let ([lock (scheduler-rq-lock sched)])
         (spinlock-acquire lock)
         (let ([task (dequeue! (scheduler-rq sched))])
           (spinlock-release lock)
           task))]))


  (define put-task!
    (case-lambda
      [(t)
       (put-task! current-scheduler t)]
      [(sched task)
       (assert (= 0 (set-timer 0)))
       (assert (scheduler-running? sched))
       (let ([lock (scheduler-rq-lock sched)])
         (spinlock-acquire lock)
         (enqueue! (scheduler-rq sched) task)
         (spinlock-release lock))]))


  (define-who get-or-steal-task!
    (lambda (sched)
      (assert (= 0 (set-timer 0)))
      ;; (println who)
      (let* ([rq (scheduler-rq sched)]
             [task (get-task!)])
        (if task task
            (begin
              ;;(println "steal task")
              (get-task! (random-sched)))))))


  (define-who run-scheduler
    (lambda (sched quit?)
      ;; (printf "running scheduler ~a~n" (scheduler-i sched))
      (let loop ([i 0] [j 0])
        ;; quit? returns #t when the main init thunk is finished
        ;;(printf "[~a] ~a~n" (scheduler-i sched) 'sched-loop)
        ;; (when (and (> i 0) (= 0 (modulo i #e1e5)))
        ;;   (printf "[~a] loop ~a times~n" (scheduler-i sched) i))
        (if (quit? (scheduler-i sched))
            (begin
              #;
              (printf "[~a] sched quit (remaining tasks: ~a)~n"
              (scheduler-i sched) (dlist-length (scheduler-rq sched)))
              ;; (printf "[~a] sched quit~n" (scheduler-i sched))
              (void))
            (let lp ([j j])
              ;; (printf "[~a] ~a~n" (scheduler-i sched) 'sched-lp)
              ;; (when (and (> j 0) (= 0 (modulo j #e1e5)))
              ;;   (printf "[~a] lp ~a times~n" (scheduler-i sched) j))
              (let ([task (get-or-steal-task! sched)])
                (if task
                    (let-values ([(type val0)
                                  (call/cc
                                   (lambda (return)
                                     ;; save
                                     (set! *timer* (timer-interrupt-handler))
                                     (timer-interrupt-handler
                                      (lambda ()
                                        ;; When timer interrupt occurs, capture the cont. of the task
                                        ;; and return to scheduler.
                                        (call/cc
                                         (lambda (resume-k)
                                           (set-timer 0)
                                           ;; RET
                                           (return 'time resume-k)))))
                                     (scheduler-k-set! current-scheduler return)
                                     (set-timer *default-ticks*)
                                     (task)
                                     ;; Reach here only if `task` is a function.
                                     ;; restore, necessary?
                                     ;; (set! *sched-k* #f)
                                     (timer-interrupt-handler *timer*)
                                     ;; disable the timer here in case the scheduler code gets interrupted
                                     (set-timer 0)
                                     ;; Always return to the current thread's scheduler.
                                     ;; If this task is created in and stolen from other threads,
                                     ;; naively returning will cause the current thread to
                                     ;; run the scheduler in other threads.
                                     (return 'complete #f)))])
                      (assert (= 0 (set-timer 0)))
                      (case type
                        [complete
                         (lp (fx1+ j))]
                        [quit
                         ;;(printf "sched task quit~n")
                         (lp (fx1+ j))]
                        ;; enqueue the task to be run later
                        [yield
                         (put-task! val0)
                         (lp (fx1+ j))]
                        ;; enqueue the task to be run later
                        [time
                         (put-task! val0)
                         (lp (fx1+ j))]
                        [callback
                         (val0)
                         (lp (fx1+ j))]
                        [error
                         (printf "sched task error: ~a~n" val0)
                         (lp (fx1+ j))]
                        [else (errorf who "unknown fiber return type: ~a" type)]))
                    ;; Is sleeping a good way to avoid busy looping?
                    (begin (nanosleep #e1e4) (loop (fx1+ i) j)))))))))


  ;; TODO maybe move this into scheduler
  (define wrap-thunk
    (lambda (t)
      (lambda ()
        (with-exception-handler
            (lambda (con)
              ;; RET
              (set-timer 0)
              ((scheduler-k current-scheduler) 'error (format "msg: ~a, irritants: ~a"
                                                              (condition-message con) (condition-irritants con))))
          (lambda ()
            (t)
            (set-timer 0)
            ;; RET
            ;; Always return to the current thread's scheduler.
            ;; If this task is created in and stolen from other threads,
            ;; naive returning will cause the current thread to
            ;; run the scheduler in other threads, or, in the case where the orginal
            ;; thread has terminated, we'll return to stale foreign context.
            ((scheduler-k current-scheduler) 'complete 'task-complete))))))


  (define-who random-sched
    (lambda ()
      (unless current-scheduler
        (errorf who "not running scheduler"))
      (let* ([len (vector-length *scheds*)] [i (fxmod (random 99999) len)])
        ;; TOOD a better way to randomly select scheds?
        (let loop ([i i])
          (let ([sched (vector-ref *scheds* i)])
            (if (scheduler-running? sched)
                sched
                (loop (fxmod (fx1+ i) len))))))))


  #|doc
  Run the thunk `th` in a new fiber.
  |#
  (define-who spawn-fiber
    (lambda (th)
      (unless current-scheduler
        (errorf who "not running scheduler"))
      ;; disable timer here
      ;; Otherwise when timer int. occurs and we are holding the mutex,
      ;; and when the next fiber also tries to hold the mutex, we have deadlock.
      (with-timer-off
       ;; randomly choose sched in spawn-fiber
       (put-task! (random-sched) (wrap-thunk th)))))



  #|doc
  Initiate a new fiber context and run `th` as the init fiber in the context.
  As the init fiber, when it exits, the fiber context is destroyed
  and all other fibers will be terminated.
  |#
  (define-who run-fibers
    (lambda (th)
      (if current-scheduler
          ;; already running, just enqueue the task
          (todo)
          ;; spawn sched threads,
          ;; set current-scheduler,
          ;; add sched threads and sched pairs to global sched vector,
          ;; schedule `th` to run by one of the scheds,
          ;; when `th` completes, set flag to signal all other scheds to quit
          (let* ([quit-flag (abox #f)] [init-res (void)]
                 [init (lambda ()
                         (with-exception-handler
                             (lambda (con)
                               (abox-set! quit-flag #t)
                               ;; (println ">>> init abnormal quit <<<")
                               ;; RET
                               (set-timer 0)
                               ((scheduler-k current-scheduler) 'error
                                (format "msg: ~a, irritants: ~a"
                                        (condition-message con) (condition-irritants con))))
                           (lambda ()
                             (set! init-res (th))
                             (abox-set! quit-flag #t)
                             ;; (println ">>> init normal quit <<<")
                             (set-timer 0)
                             ;; RET
                             ;; Always return to the current thread's scheduler.
                             ;; If this task is created in and stolen from other threads,
                             ;; naive returning will cause the current thread to
                             ;; run the scheduler in other threads, or, in the case where the orginal
                             ;; thread has terminated, we'll return to stale foreign context.
                             ((scheduler-k current-scheduler) 'complete 'init-complete))))]
                 ;; make scheds wait for the main thunk to exit in run-fibers
                 [quit? (lambda (i)
                          (let ([f (unabox quit-flag)])
                            (if f (begin
                                    #;
                                    (printf "sched [~a] received quit (*preemption-count*: ~a)~n" ; ;
                                    i *preemption-count*)
                                    f)
                                f)))]
                 [scheds (map (lambda (i)
                                (let ([sched (make-scheduler i #f (make-dlist)
                                                             (make-spinlock (string->symbol (format "fiber-sched-Q-~a" i)))
                                                             #f #f)])
                                  sched))
                              (iota (cpu-count)
                                    ))])
            (enqueue! (scheduler-rq (car scheds)) init)
            (set! *scheds* (list->vector scheds))
            ;; spawn a bunch of scheduler threads
            (let ([threads (map (lambda (s)
                                  (let ([t (fork-thread (lambda ()
                                                          ;; TODO race here?
                                                          (set! current-scheduler s)
                                                          (scheduler-running?-set! s #t)
                                                          ;; (call/cc (lambda (k) (run-scheduler s quit? k)))
                                                          (run-scheduler s quit?)
                                                          (set! current-scheduler #f)
                                                          (scheduler-running?-set! s #f)))])
                                    (scheduler-thread-set! s t)
                                    t))
                                scheds)])
              (println "---------------------------- all scheds up")
              (for-each thread-join threads)
              (println "---------------------------- all scheds down")
              (set! *scheds* #f)
              init-res)))))



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


  #|doc
  Synchronize on the given first-class event value `evt`.

  When the synchronization succeeds, the synchronization value of the
  event is returned, if any, with all wrap functions applied to the original synchronization value
  in the order they are added to the event via `event-wrap`.

  If the synchronization is currently impossible, the current fiber blocks
  untill it becomes ready to do the synchronization.
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
                  ((call/cc
                    (lambda (k)
                      ;; This is called in things like channel code when doing rendez-vous
                      ;; and handling over messege.
                      ;; `thunk`, when called, will return the message.
                      (define resume
                        (lambda (thunk)
                          (assert (= 0 (set-timer 0)))
                          (put-task! (random-sched) (lambda () (k thunk)))))
                      (assert (scheduler-running? current-scheduler))
                      ;; RET
                      ((scheduler-k current-scheduler) 'callback (lambda () (block resume))))))))
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
       (let ([v (if (fx= 0 (dlist-length Q))
                    #f
                    ;; TODO optimize
                    (let ([i (dlist-contains/p? Q pred)])
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
                    (printf "acq: ~a~n" (dlist-length blockQ)))
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
                    (printf "wait: ~a~n" (dlist-length waitQ)))
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


  )
