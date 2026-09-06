(import (chezpp)
        (chezpp concurrency fiber)
        (chezpp net operation))

(mat net-operation-fiber-event-completes
     (eq? 'done
          (run-fibers
           (lambda ()
             (event-sync
              (net-operation-event
               (make-net-operation
                'fiber-immediate
                (lambda () (net-operation-completed 'done))
                void)))))))

(mat net-operation-fiber-event-drives-pending
     (let ([step-count 0])
       (and
        (eq? 'done
             (run-fibers
              (lambda ()
                (net-operation-wait
                 (make-net-operation
                  'fiber-pending
                  (lambda ()
                    (set! step-count (fx1+ step-count))
                    (if (fx= step-count 3)
                        (net-operation-completed 'done)
                        (net-operation-pending '() #f)))
                  void)))))
        (fx= step-count 3))))
