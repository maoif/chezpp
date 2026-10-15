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
           [iterations 8] [count 0] [timed-out #f])
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
                 ;; Bound the wait so a lost cvar wakeup fails this case promptly.
                 (event-sync
                  (event-select
                   (channel-get-event finished)
                   (event-wrap (milisleep-event 100)
                               (lambda (_) 'timeout))))))))
         (set! timed-out (eq? result 'timeout))
         (and (not timed-out) (= count iterations)))))

(mat fiber-mutex-owner-and-unwind
     (let ([mutex (make-fiber-mutex)] [foreign-error #f] [released #f])
       (run-fibers
        (lambda ()
          (fiber-mutex-acquire mutex)
          (spawn-fiber
           (lambda ()
             ;; Negative case: a foreign fiber must not release this mutex.
             (set! foreign-error
                   (guard (condition [else #t]) (fiber-mutex-release mutex) #f))))
          (fiber-yield)
          (fiber-mutex-release mutex)
          (guard (condition [else #t])
            (with-fiber-mutex mutex (error 'fiber-mutex "unwind"))
            #f)
          (fiber-mutex-acquire mutex)
          (set! released #t)
          (fiber-mutex-release mutex)))
       (and foreign-error released)))

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
