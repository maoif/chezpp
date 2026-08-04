(import (chezpp)
        (chezpp net operation))

(define capture-condition
  (lambda (thunk)
    (guard (failure [else failure])
      (thunk)
      #f)))


(mat net-operation-lifecycle

     (let ([operation
            (make-net-operation 'immediate
                                (lambda () (net-operation-completed 'done))
                                void)])
       (and (eq? 'pending (net-operation-state operation))
            (eq? operation (net-operation-step! operation))
            (eq? 'completed (net-operation-state operation))
            (eq? 'done (net-operation-result operation))))

     (let* ([steps 0]
            [operation
             (make-net-operation
              'test
              (lambda ()
                (set! steps (fx+ steps 1))
                (if (fx= steps 1)
                    (net-operation-pending '() 5000)
                    (net-operation-completed 'done)))
              (lambda () (void)))])
       (and (eq? 'pending (net-operation-state operation))
            (begin (net-operation-step! operation) #t)
            (eq? 'pending (net-operation-state operation))
            (begin (net-operation-step! operation) #t)
            (eq? 'completed (net-operation-state operation))
            (eq? 'done (net-operation-result operation))))

     (let* ([steps 0]
            [operation
             (make-net-operation
              'timeout
              (lambda ()
                (set! steps (fx+ steps 1))
                (if (fx= steps 1)
                    (net-operation-pending '() 0)
                    (net-operation-completed 'timeout-wakeup)))
              void)])
       (net-operation-step! operation)
       (and (fx= 0 (net-operation-remaining-timeout-ms operation))
            (eq? 'timeout-wakeup (net-operation-wait operation))))

     ;; A cancelled operation cannot be stepped or read as a successful result.
     (let ([operation
            (make-net-operation 'test
                                (lambda () (net-operation-pending '() #f))
                                (lambda () (void)))])
       (net-operation-cancel! operation)
       (let ([cancel-condition
              (guard (failure [else #f])
                (net-operation-condition operation))])
         (and (eq? 'cancelled (net-operation-state operation))
              (error? cancel-condition)
              (eq? cancel-condition
                   (capture-condition (lambda () (net-operation-wait operation))))
              (error? (capture-condition (lambda () (net-operation-step! operation))))
              (error? (capture-condition (lambda () (net-operation-result operation)))))))

     ;; Repeated cancellation invokes cancellation and cleanup procedures only once.
     (let ([cancel-count 0]
           [cleanup-count 0])
       (let ([operation
              (make-net-operation
               'cleanup
               (lambda () (net-operation-pending '() #f))
               (lambda () (set! cancel-count (fx+ cancel-count 1)))
               (lambda () (set! cleanup-count (fx+ cleanup-count 1))))])
         (and (eq? operation (net-operation-cancel! operation))
              (eq? operation (net-operation-cancel! operation))
              (fx= cancel-count 1)
              (fx= cleanup-count 1))))

     ;; Failed operations expose their condition but not a successful result.
     (let* ([failure (condition (make-error) (make-message-condition "failed"))]
            [cleanup-count 0]
            [operation
             (make-net-operation
              'failure
              (lambda () (net-operation-failed failure))
              void
              (lambda () (set! cleanup-count (fx+ cleanup-count 1))))])
       (net-operation-step! operation)
       (and (eq? 'failed (net-operation-state operation))
            (eq? failure (net-operation-condition operation))
            (error? (capture-condition (lambda () (net-operation-result operation))))
            (fx= cleanup-count 1)))

     ;; Conditions raised by advancement become failed operation results.
     (let ([operation
            (make-net-operation
             'raised
             (lambda () (error 'raised "failed"))
             void)])
       (net-operation-step! operation)
       (and (eq? 'failed (net-operation-state operation))
            (condition? (net-operation-condition operation))))

     ;; Pending and completed operations do not expose a failure condition.
     (let ([operation
            (make-net-operation 'pending
                                (lambda () (net-operation-completed 'done))
                                void)])
       (and (error? (capture-condition (lambda () (net-operation-result operation))))
            (error? (capture-condition (lambda () (net-operation-condition operation))))
            (begin (net-operation-step! operation) #t)
            (error? (capture-condition (lambda () (net-operation-condition operation))))))

     ;; An invalid advancement update is caught and stored as a failed condition.
     (let ([operation (make-net-operation 'invalid (lambda () 'invalid) void)])
       (net-operation-step! operation)
       (and (eq? 'failed (net-operation-state operation))
            (condition? (net-operation-condition operation))))
     )


(mat net-would-block-record

     (let ([would-block (make-net-would-block 'socket '(read write))])
       (and (net-would-block? would-block)
            (eq? 'socket (net-would-block-resource would-block))
            (equal? '(read write) (net-would-block-events would-block))))

     ;; Error case: would-block event lists must contain supported event symbols.
     (let ([failure
            (capture-condition
             (lambda () (make-net-would-block 'socket '(unknown))))])
       (error? failure))
     )
