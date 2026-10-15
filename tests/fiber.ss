(import (chezpp)
        (chezpp concurrency fiber)
        (chezpp net operation))

(mat fiber-event-never-blocks
     (let ([attempted (make-channel)] [done (abox #f)])
       (run-fibers
        (lambda ()
          (spawn-fiber
           (lambda ()
             (channel-put! attempted #t)
             (event-sync (event-never))
             (abox-set! done #t)))
          (channel-get attempted)
          ;; Give the child time to attempt synchronization before stopping the context.
          (let loop ([remaining 1000])
            (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))))
       ;; Negative case: event-never must not let the waiting child complete.
       (not (unabox done))))

(mat fiber-empty-select-is-never
     (let ([attempted (make-channel)] [done (abox #f)])
       (run-fibers
        (lambda ()
          (spawn-fiber
           (lambda ()
             (channel-put! attempted #t)
             (event-sync (event-select))
             (abox-set! done #t)))
          (channel-get attempted)
          (let loop ([remaining 1000])
            (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))))
       ;; Negative case: an empty selection must remain blocked.
       (not (unabox done))))

(mat fiber-wrap-does-not-mutate-source
     (let ([source (event-select (event-always 1))])
       ;; Negative case: constructing a wrapper must not modify its source event.
       (let ([wrapped (event-wrap source add1)])
         (equal? '(1 2)
                 (run-fibers
                  (lambda ()
                    (list (event-sync source) (event-sync wrapped))))))))

(mat fiber-nested-selection-and-repeated-wrapping
     (let* ([source (event-select (event-never) (event-always 3))]
            [inner (event-wrap source add1)]
            [outer (event-wrap (event-select (event-never) inner) (lambda (x) (* x 2)))]
            [again (event-wrap outer sub1)])
       ;; Negative case: wrapping a shared selection must not mutate any previous event.
       (equal? '(3 4 8 7 3 4 8)
               (run-fibers
                (lambda ()
                  (map event-sync (list source inner outer again source inner outer)))))))

(mat fiber-event-wrap-preserves-multiple-values
     (equal? '(4 5)
             (call-with-values
              (lambda ()
                (run-fibers
                 (lambda ()
                   (event-sync
                    (event-wrap
                     (event-wrap (event-always 3) (lambda (x) (values x (add1 x))))
                     (lambda (left right) (values (add1 left) (add1 right))))))))
              list)))

(mat fiber-quit-stops-context
     (begin (run-fibers (lambda () (fiber-quit))) #t))

(mat fiber-quit-stops-only-child
     (let ([events (make-channel)])
       (run-fibers
        (lambda ()
          (spawn-fiber (lambda () (channel-put! events 'child) (fiber-quit)))
          (spawn-fiber (lambda () (channel-put! events 'sibling)))
          (let ([first (channel-get events)] [second (channel-get events)])
            (and (memq 'child (list first second))
                 (memq 'sibling (list first second))
                 #t))))))

(mat fiber-shutdown-drops-runnable-children
     (let ([result (run-fibers
                    (lambda ()
                      (spawn-fiber
                       (lambda ()
                         (let loop () (fiber-yield) (loop))))
                      'complete))])
       (eq? result 'complete)))

(mat fiber-init-error-reaches-caller
     ;; Negative case: the original init condition must reach the caller.
     (let ([original (condition (make-error) (make-who-condition 'fiber-init)
                               (make-message-condition "boom")
                               (make-irritants-condition '(original)))])
       (let ([caught (guard (condition [else condition])
                       (run-fibers (lambda () (fiber-yield) (raise original)))
                       #f)])
         (eq? original caught))))

(mat fiber-child-error-reaches-caller
     ;; Negative case: a child failure must stop the init fiber and reach its caller.
     (let ([original (condition (make-error) (make-message-condition "child boom"))])
       (eq? original
            (guard (condition [else condition])
              (run-fibers
               (lambda ()
                 (spawn-fiber (lambda () (fiber-yield) (raise original)))
                 (let loop () (fiber-yield) (loop))))
              #f))))

(mat fiber-init-false-error-reaches-caller
     ;; Negative case: #f is a raised object, even though it is also the default error value.
     (guard (condition [else (eq? condition #f)])
       (run-fibers (lambda () (fiber-yield) (raise #f)))
       #f))

(mat fiber-child-false-error-reaches-caller
     ;; Negative case: a child's raised #f must stop the context and reach its caller.
     (guard (condition [else (eq? condition #f)])
       (run-fibers
        (lambda ()
          (spawn-fiber (lambda () (fiber-yield) (raise #f)))
          ;; Bound the init wait so a swallowed child error fails instead of hanging.
          (let loop ([remaining 1000])
            (unless (zero? remaining)
              (fiber-yield)
              (loop (sub1 remaining))))))
       #f))

(mat fiber-multiple-values-preserved
     (call-with-values (lambda () (run-fibers (lambda () (values 'left 'right))))
       (lambda (left right) (and (eq? left 'left) (eq? right 'right)))))

(mat fiber-nested-run-spawns-and-returns
     (let ([nested-ran #f] [nested-done (make-channel)] [outer-ready (make-channel)])
       (let ((result
              (run-fibers
               (lambda ()
                 (run-fibers
                  (lambda ()
                    ;; The nested thunk waits for work published after run-fibers returns.
                    (channel-get outer-ready)
                    (set! nested-ran #t)
                    (channel-put! nested-done #t)))
                 (channel-put! outer-ready #t)
                 (channel-get nested-done)
                 'outer))))
         (and (eq? result 'outer) nested-ran))))

(mat fiber-cvar-no-lost-wakeup
     (let ([mutex (make-fiber-mutex)] [cvar (make-fiber-cvar)]
           [ready (make-channel)] [finished (make-channel)]
           [iterations 10000] [count 0])
       (let ((result
              (run-fibers
               (lambda ()
                 (spawn-fiber
                  (lambda ()
                    (let loop ([i 0])
                      (unless (= i iterations)
                        (fiber-mutex-acquire mutex)
                        (channel-put! ready #t)
                        (fiber-cvar-wait cvar mutex)
                        (set! count (add1 count))
                        (fiber-mutex-release mutex)
                        (loop (add1 i))))
                    (channel-put! finished #t)))
                 (spawn-fiber
                  (lambda ()
                    (let loop ([i 0])
                      (unless (= i iterations)
                        (channel-get ready)
                        (fiber-mutex-acquire mutex)
                        (fiber-cvar-signal cvar)
                        (fiber-mutex-release mutex)
                        (loop (add1 i))))))
                 (spawn-fiber
                  (lambda ()
                    ;; Negative case: a lost wakeup must fail without depending on fiber timers.
                    (let loop ([remaining 1000000])
                      (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))
                    (channel-put! finished 'timeout)))
                 (channel-get finished)))))
         (and (eq? result #t) (= count iterations)))))

(mat fiber-mutex-rejects-foreign-release
     (let ([mutex (make-fiber-mutex)] [outcome (make-channel)])
       (run-fibers
        (lambda ()
          (fiber-mutex-acquire mutex)
          (spawn-fiber
           (lambda ()
             ;; Negative case: a foreign fiber must not release this mutex.
             (channel-put! outcome
               (guard (condition
                       [else (and (who-condition? condition)
                                  (eq? 'fiber-mutex-release (condition-who condition)))])
                 (fiber-mutex-release mutex)
                 #f))))
          ;; The owner keeps the mutex until the foreign release has actually been attempted.
          (let ([rejected (channel-get outcome)])
            (when rejected (fiber-mutex-release mutex))
            rejected)))))

(mat fiber-mutex-unwind-releases-to-peer
     (let ([mutex (make-fiber-mutex)] [done (make-channel)]
           [original (condition (make-error) (make-message-condition "mutex unwind"))])
       (run-fibers
        (lambda ()
          ;; Negative case: an exception after yielding must release exactly once.
          (let ([caught (guard (condition [else condition])
                          (with-fiber-mutex mutex (fiber-yield) (raise original))
                          #f)])
            (spawn-fiber
             (lambda ()
               (with-fiber-mutex mutex (void))
               (channel-put! done 'released)))
            (spawn-fiber
             (lambda ()
               (let loop ([remaining 100000])
                 (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))
               (channel-put! done 'timeout)))
            (and (eq? caught original) (eq? 'released (channel-get done))))))))

(mat fiber-mutex-yield-preserves-ownership
     (let ([mutex (make-fiber-mutex)] [attempted (make-channel)] [done (make-channel)]
           [entered (abox #f)])
       (run-fibers
        (lambda ()
          (let ([retained
                 (with-fiber-mutex mutex
                   (spawn-fiber
                    (lambda ()
                      (channel-put! attempted #t)
                      (with-fiber-mutex mutex (abox-set! entered #t))
                      (channel-put! done #t)))
                   (channel-get attempted)
                   ;; Negative case: yielding inside the scope must not unlock its mutex.
                   (let loop ([remaining 1000])
                     (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))
                   (not (unabox entered)))])
            (and retained (channel-get done) (unabox entered)))))))

(mat fiber-mutex-scope-preserves-multiple-values
     (let ([mutex (make-fiber-mutex)])
       (equal? '(left right)
               (run-fibers
                (lambda ()
                  (let ([result
                         (call-with-values
                          (lambda ()
                            (with-fiber-mutex mutex (fiber-yield) (values 'left 'right)))
                          list)])
                    ;; A normal scope exit must release before another acquisition.
                    (with-fiber-mutex mutex result)))))))

(mat fiber-cvar-wait-requires-mutex-owner
     (let ([mutex (make-fiber-mutex)] [cvar (make-fiber-cvar)])
       (run-fibers
        (lambda ()
          ;; Negative case: waiting without ownership must raise on the calling fiber.
          (guard (condition
                  [else (and (who-condition? condition)
                             (eq? 'fiber-cvar-wait (condition-who condition)))])
            (fiber-cvar-wait cvar mutex)
            #f)))))

(mat fiber-cvar-broadcast-wakes-predicate-waiters
     (let ([mutex (make-fiber-mutex)] [cvar (make-fiber-cvar)]
           [ready (make-channel)] [done (make-channel)] [open? #f])
       (run-fibers
        (lambda ()
          (do ([index 0 (add1 index)]) ((= index 4))
            (spawn-fiber
             (lambda ()
               (with-fiber-mutex mutex
                 (channel-put! ready #t)
                 (let wait ()
                   (unless open? (fiber-cvar-wait cvar mutex) (wait))))
               (channel-put! done #t))))
          (spawn-fiber
           (lambda ()
             ;; Negative case: a broadcast must not strand any of the registered waiters.
             (let loop ([remaining 100000])
               (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))
             (channel-put! done #f)))
          (do ([index 0 (add1 index)]) ((= index 4)) (channel-get ready))
          ;; Acquiring proves the final child has registered its wait and released the mutex.
          (with-fiber-mutex mutex
            (set! open? #t)
            (fiber-cvar-broadcast cvar))
          (let loop ([remaining 4])
            (or (zero? remaining)
                (and (channel-get done) (loop (sub1 remaining)))))))))

(mat fiber-mutex-shutdown-discards-waiter
     (let ([mutex (make-fiber-mutex)] [held (make-channel)] [attempted (make-channel)]
           [release (make-channel)] [released (make-channel)] [done (make-channel)])
       (equal? '#(owner reused)
               (run-threads 0
                (lambda ()
                  (run-fibers
                   (lambda ()
                     (fiber-mutex-acquire mutex)
                     (channel-put! held #t)
                     (channel-get release)
                     (fiber-mutex-release mutex)
                     (channel-put! released #t)
                     'owner)))
                (lambda ()
                  (run-fibers
                   (lambda ()
                     (channel-get held)
                     (spawn-fiber
                      (lambda ()
                        (channel-put! attempted #t)
                        (with-fiber-mutex mutex (void))))
                     (channel-get attempted)
                     (let loop ([remaining 1000])
                       (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))))
                  (run-fibers
                   (lambda ()
                     (channel-put! release #t)
                     (channel-get released)
                     (spawn-fiber
                      (lambda ()
                        (with-fiber-mutex mutex (void))
                        (channel-put! done 'reused)))
                     (spawn-fiber
                      (lambda ()
                        ;; Negative case: a canceled waiter must never receive ownership.
                        (let loop ([remaining 100000])
                          (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))
                        (channel-put! done 'timeout)))
                     (channel-get done))))))))

(mat fiber-cvar-shutdown-discards-waiter
     (let ([mutex (make-fiber-mutex)] [cvar (make-fiber-cvar)]
           [ready (make-channel)] [done (make-channel)])
       (run-fibers
        (lambda ()
          (spawn-fiber
           (lambda ()
             (with-fiber-mutex mutex
               (channel-put! ready #t)
               (fiber-cvar-wait cvar mutex))))
          (channel-get ready)
          ;; The child is registered once this acquisition succeeds.
          (with-fiber-mutex mutex (void))))
       (eq? 'reused
            (run-fibers
             (lambda ()
               (spawn-fiber
                (lambda ()
                  (with-fiber-mutex mutex
                    (channel-put! ready #t)
                    (fiber-cvar-wait cvar mutex))
                  (channel-put! done 'reused)))
               (spawn-fiber
                (lambda ()
                  ;; Negative case: a stale cvar waiter must not consume the fresh signal.
                  (let loop ([remaining 100000])
                    (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))
                  (channel-put! done 'timeout)))
               (channel-get ready)
               (with-fiber-mutex mutex (fiber-cvar-signal cvar))
               (channel-get done))))))

(mat fiber-select-cleans-losing-registrations
     (let ([unused (make-channel)] [active (make-channel)] [done (make-channel)]
           [iterations 1000])
       (eq? 'complete
            (run-fibers
             (lambda ()
               (spawn-fiber
                (lambda ()
                  (let loop ([i 0])
                    (when (< i iterations)
                      (spawn-fiber (lambda () (fiber-yield) (channel-put! active i)))
                      (unless (= i (event-sync
                                    (event-select (channel-get-event unused)
                                                  (channel-get-event active))))
                        (error 'fiber-select "wrong channel value"))
                      (loop (add1 i))))
                  (channel-put! done 'complete)))
               (spawn-fiber
                (lambda ()
                  ;; Negative case: leaked/blocked choices must fail through public behavior.
                  (let loop ([remaining 100000])
                    (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))
                  (channel-put! done 'timeout)))
               (channel-get done))))))

(mat fiber-select-cleans-losing-network-registrations
     (let ([active (make-channel)] [done (make-channel)]
           [operation (make-net-operation 'losing-choice
                        (lambda () (net-operation-pending '() #f)) void)])
       (eq? 'complete
            (run-fibers
             (lambda ()
               (spawn-fiber
                (lambda ()
                  (let loop ([remaining 400])
                    (unless (zero? remaining)
                      (spawn-fiber
                       (lambda ()
                         ;; Ensure the network branch registered before the channel wins.
                         (let wait ([attempts 10000])
                           (when (and (positive? attempts)
                                      (zero? (vector-ref
                                              (net-operation-event-pool-metrics) 1)))
                             (fiber-yield) (wait (sub1 attempts))))
                         (channel-put! active 'channel)))
                      (unless (eq? 'channel
                                   (event-sync
                                    (event-select (net-operation-event operation)
                                                  (channel-get-event active))))
                        (error 'fiber-select "wrong selection result"))
                      ;; Losing network waiters must be returned immediately to the pool.
                      (unless (zero? (vector-ref (net-operation-event-pool-metrics) 1))
                        (error 'fiber-select "network waiter retained"))
                      (loop (sub1 remaining))))
                  (channel-put! done 'complete)))
               (spawn-fiber
                (lambda ()
                  ;; Negative case: repeated losing registrations must not exhaust the pool.
                  (let loop ([remaining 200000])
                    (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))
                  (channel-put! done 'timeout)))
               (channel-get done))))))

(mat fiber-select-duplicate-network-branches
     (let* ([operation (make-net-operation 'duplicate-choice
                         (lambda () (net-operation-completed 'done)) void)]
            [event (net-operation-event operation)])
       ;; Negative case: committing one branch must safely detach its pooled sibling waiters.
       (and (eq? 'done
                 (run-fibers (lambda () (event-sync (event-select event event event)))))
            (zero? (vector-ref (net-operation-event-pool-metrics) 1)))))

(mat fiber-channel-shutdown-discards-waiter
     (let ([channel (make-channel)] [attempted (make-channel)])
       (run-fibers
        (lambda ()
          (spawn-fiber
           (lambda () (channel-put! attempted #t) (channel-put! channel 'abandoned)))
          (channel-get attempted)
          (let loop ([remaining 1000])
            (unless (zero? remaining) (fiber-yield) (loop (sub1 remaining))))))
       ;; Negative case: a stopped context's sender must not participate in a later rendezvous.
       (eq? 'fresh
            (run-fibers
             (lambda ()
               (spawn-fiber (lambda () (channel-put! channel 'fresh)))
               (channel-get channel))))))

(mat fiber-channel-selection-does-not-rendezvous-with-itself
     (let ([channel (make-channel)])
       (eq? 'peer
            (run-fibers
             (lambda ()
               (spawn-fiber (lambda () (fiber-yield) (channel-put! channel 'peer)))
               ;; Negative case: opposite branches of one transaction cannot match each other.
               (event-sync (event-select (channel-get-event channel)
                                         (channel-put-event channel 'self))))))))

(mat fiber-channel-rendezvous-across-contexts
     (let* ([channel (make-channel)]
            [results
             (run-threads 0
              (lambda () (run-fibers (lambda () (channel-get channel))))
              (lambda () (run-fibers (lambda () (channel-put! channel 'shared) 'sent))))])
       (and (eq? 'shared (vector-ref results 0)) (eq? 'sent (vector-ref results 1)))))

(mat fiber-sleep-event
     (let ([milliseconds #f] [seconds #f])
       (run-fibers
        (lambda ()
          (event-sync (milisleep-event 1))
          (set! milliseconds #t)
          (event-sync (sleep-event 1))
          (set! seconds #t)))
       (and milliseconds seconds)))

(mat fiber-zero-sleep-is-immediate
     (let ([done #f])
       (run-fibers
        (lambda ()
          (fiber-milisleep 0)
          (set! done #t)))
       done))

(mat fiber-sleep-honors-minimum-delay
     (let ([started #f] [finished #f])
       (run-fibers
        (lambda ()
          (let ([before (current-time 'time-monotonic)])
            (set! started before)
            (fiber-milisleep 20)
            (set! finished (current-time 'time-monotonic)))))
       ;; Negative case: a timer must not resume before its requested deadline.
       (and started finished
            (let ([elapsed (time-difference finished started)])
              (or (> (time-second elapsed) 0)
                  (>= (time-nanosecond elapsed) 15000000))))))

(mat fiber-many-sleepers-share-workers
     (let ([done (make-channel)] [count 32])
       (run-fibers
        (lambda ()
          (do ([i 0 (add1 i)]) ((= i count))
            (spawn-fiber
             (lambda ()
               (fiber-milisleep 1)
               (channel-put! done #t))))
          (let loop ([remaining count])
            (if (zero? remaining)
                #t
                (and (channel-get done) (loop (sub1 remaining)))))))))

(mat fiber-sleep-shutdown-cleans-timers
     (begin
       (run-fibers
        (lambda ()
          (spawn-fiber (lambda () (fiber-sleep 60)))
          'stopped))
       ;; Negative case: an abandoned timer must not affect a later context.
       (eq? 'reused (run-fibers (lambda () 'reused)))))

(mat fiber-with-timer-off-restores-on-error
     (let ([caught #f] [restored #f])
       (run-fibers
        (lambda ()
          ;; Negative case: timer-off must restore preemption after an exception.
          (set-timer 10000000)
          (set! caught
                (guard (condition [else #t])
                  (with-timer-off (error 'timer "boom"))
                  #f))
          (set! restored (positive? (set-timer 0)))))
       (and caught restored)))

(mat fiber-independent-contexts
     (let ([results (run-threads 0
                                 (lambda () (run-fibers (lambda () 'left)))
                                 (lambda () (run-fibers (lambda () 'right))))])
       (and (eq? (vector-ref results 0) 'left)
            (eq? (vector-ref results 1) 'right))))
