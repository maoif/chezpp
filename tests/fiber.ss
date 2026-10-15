(import (chezpp)
        (chezpp concurrency fiber))

(define fiber-test-wait
  (lambda (flag)
    (let loop ([remaining 1000])
      (cond
       [flag #t]
       [(zero? remaining) #f]
       [else (milisleep 1) (loop (sub1 remaining))]))))

(mat fiber-event-never-blocks
     (let ([finished (make-channel)] [peer (make-channel)] [done #f])
       (run-fibers
        (lambda ()
          (spawn-fiber
           (lambda ()
             (event-sync (event-never))
             (set! done #t)
             (channel-put! finished #t)))
          (spawn-fiber (lambda () (channel-put! peer #t)))
          (channel-get peer)))
       ;; Negative case: event-never must not let the waiting child complete.
       (and (not done) (not (fiber-test-wait done)))))

(mat fiber-empty-select-is-never
     (let ([finished (make-channel)] [done #f])
       (run-fibers
        (lambda ()
          (spawn-fiber
           (lambda ()
             (event-sync (event-select))
             (set! done #t)
             (channel-put! finished #t)) )
          (fiber-yield)))
       ;; Negative case: an empty selection must remain blocked.
       (and (not done) (not (fiber-test-wait done)))))

(mat fiber-wrap-does-not-mutate-source
     (let ([source (event-select (event-always 1))])
       (equal? '(1 2)
               (run-fibers
                (lambda ()
                  (list (event-sync source)
                        (event-sync (event-wrap source add1))))))))

(mat fiber-quit-stops-context
     (let ([result (run-fibers (lambda () (fiber-quit)))])
       (void result)
       #t))

(mat fiber-quit-stops-only-child
     (let ([events (make-channel)])
       (run-fibers
        (lambda ()
          (spawn-fiber (lambda () (channel-put! events 'child) (fiber-quit)))
          (spawn-fiber (lambda () (channel-put! events 'sibling)))
          (let ([first (channel-get events)] [second (channel-get events)])
            (and (memq 'child (list first second))
                 (memq 'sibling (list first second))))))))

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
     (let ([caught (guard (condition [else condition])
                    (run-fibers (lambda () (error 'fiber-init "boom")))
                    #f)])
       (and (condition? caught)
            (string=? "boom" (condition-message caught)))))

(mat fiber-multiple-values-preserved
     (call-with-values (lambda () (run-fibers (lambda () (values 'left 'right))))
       (lambda (left right) (and (eq? left 'left) (eq? right 'right)))))

(mat fiber-nested-run-spawns-and-returns
     (let ([nested-ran #f])
       (and (eq? 'outer
                 (run-fibers
                  (lambda ()
                    (run-fibers (lambda () (set! nested-ran #t)))
                    'outer)))
            nested-ran)))

(mat fiber-cvar-no-lost-wakeup
     (let ([mutex (make-fiber-mutex)] [cvar (make-fiber-cvar)]
           [ready (make-channel)] [finished (make-channel)]
           [iterations 8] [count 0])
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
                 (loop (add1 i)))
             (channel-put! finished #t)))
          (spawn-fiber
           (lambda ()
             (let loop ([i 0])
               (unless (= i iterations)
                 (channel-get ready)
                 (fiber-mutex-acquire mutex)
                 (fiber-cvar-signal cvar)
                 (fiber-mutex-release mutex)
                 (loop (add1 i)))))))
          (channel-get finished)))
       (= count iterations)))

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
     (let ([unused (make-channel)] [active (make-channel)] [done 0])
       (run-fibers
        (lambda ()
          (for-each
           (lambda (value)
             (spawn-fiber (lambda () (channel-put! active value)))
             (event-sync (event-select (channel-get-event unused)
                                       (channel-get-event active)))
             (set! done (add1 done)))
           (iota 16))))
       (= done 16)))

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
     (let ([caught #f] [continued #f])
       (run-fibers
        (lambda ()
          ;; Negative case: timer-off must restore preemption after an exception.
          (set! caught
                (guard (condition [else #t])
                  (with-timer-off (error 'timer "boom"))
                  #f))
          (fiber-yield)
          (set! continued #t)))
       (and caught continued)))

(mat fiber-independent-contexts
     (let ([results (run-threads 0
                                 (lambda () (run-fibers (lambda () 'left)))
                                 (lambda () (run-fibers (lambda () 'right))))])
       (and (eq? (vector-ref results 0) 'left)
            (eq? (vector-ref results 1) 'right))))
