(import (chezpp)
        (chezpp net lws ffi))

(define call-with-lws-context
  (lambda (procedure)
    (let ([context (lws-context-open 32 16)])
      (dynamic-wind
        void
        (lambda () (procedure context))
        (lambda () (lws-context-close context))))))

(mat net-lws-context-lifecycle
     (call-with-lws-context
      (lambda (context)
        (and (positive? context)
             (fixnum? (lws-context-wakeup-fd context))
             (vector? (lws-context-poll-snapshot context))
             (integer? (lws-context-timeout-ms context 1000))
             (begin
               (lws-context-wakeup context)
               (lws-context-service-fd context (lws-context-wakeup-fd context) 1)
               #t)))))

(mat net-lws-copied-event-order
     (call-with-lws-context
      (lambda (context)
        (lws-context-inject-event! context 'headers 10 11 3 200 #vu8(1 2))
        (lws-context-inject-event! context 'readable 10 11 3 0 #vu8(3 4 5))
        (lws-context-inject-event! context 'complete 10 11 3 0 #vu8())
        (let* ([headers (lws-context-next-event context)]
               [readable (lws-context-next-event context)]
               [complete (lws-context-next-event context)])
          (and (eq? (vector-ref headers 0) 'headers)
               (positive? (vector-ref headers 1))
               (= (vector-ref headers 2) 10)
               (= (vector-ref headers 3) 11)
               (= (vector-ref headers 4) 3)
               (= (vector-ref headers 5) 200)
               (equal? (vector-ref headers 6) #vu8(1 2))
               (eq? (vector-ref readable 0) 'readable)
               (equal? (vector-ref readable 6) #vu8(3 4 5))
               (eq? (vector-ref complete 0) 'complete)
               (not (lws-context-next-event context)))))))

(mat net-lws-fake-callback-tags
     (call-with-lws-context
      (lambda (context)
        (let ([tags '(connected headers writable closed failed reset goaway)])
          (let inject ([pending tags] [stream-id 21])
            (unless (null? pending)
              (lws-context-inject-event!
               context (car pending) 20 stream-id 7 0 #vu8(9))
              (inject (cdr pending) (+ stream-id 1))))
          (let loop ([expected tags] [stream-id 21])
            (if (null? expected)
                (not (lws-context-next-event context))
                (let ([event (lws-context-next-event context)])
                  (and (eq? (vector-ref event 0) (car expected))
                       (= (vector-ref event 2) 20)
                       (= (vector-ref event 3) stream-id)
                       (= (vector-ref event 4) 7)
                       (loop (cdr expected) (+ stream-id 1))))))))))

(mat net-lws-poll-change-seam
     (call-with-lws-context
      (lambda (context)
        (and (let loop ([descriptor 101])
               (if (> descriptor 200)
                   #t
                   (and (lws-context-inject-poll! context 'add descriptor 1)
                        (lws-context-inject-poll! context 'change descriptor 5)
                        (lws-context-inject-poll! context 'delete descriptor 0)
                        (begin
                          (lws-context-next-event context)
                          (lws-context-next-event context)
                          (lws-context-next-event context)
                          (loop (+ descriptor 1))))))
             (lws-context-inject-poll! context 'add 101 1)
             (lws-context-inject-poll! context 'change 101 5)
             (= (vector-length (lws-context-poll-snapshot context)) 1)
             (= (vector-ref (vector-ref (lws-context-poll-snapshot context) 0) 0) 101)
             (= (vector-ref (vector-ref (lws-context-poll-snapshot context) 0) 1) 5)
             (lws-context-inject-poll! context 'delete 101 0)
             (zero? (vector-length (lws-context-poll-snapshot context)))
             (equal? (map (lambda (ignored)
                            (vector-ref (lws-context-next-event context) 0))
                          '(1 2 3))
                     '(poll-add poll-change poll-delete))))))

(mat net-lws-stream-exhaustion-releases-connection
     ;; Error case: acquiring a connection before stream exhaustion must not leak the connection.
     (let ([context (lws-context-open 2 4)])
       (dynamic-wind
         void
         (lambda ()
           (and (lws-context-inject-event! context 'writable 1 1 1 0 #vu8())
                (begin (lws-context-next-event context) #t)
                (lws-context-inject-event! context 'writable 1 2 1 0 #vu8())
                (begin (lws-context-next-event context) #t)
                (not (lws-context-inject-event! context 'writable 2 3 1 0 #vu8()))
                (lws-context-inject-event! context 'complete 1 1 1 0 #vu8())
                (begin (lws-context-next-event context) #t)
                (lws-context-inject-event! context 'complete 1 2 1 0 #vu8())
                (begin (lws-context-next-event context) #t)
                (zero? (vector-ref (lws-context-pool-metrics context) 8))))
         (lambda () (lws-context-close context)))))

(mat net-lws-body-flow-control
     (call-with-lws-context
      (lambda (context)
        (and (lws-context-inject-event! context 'readable 30 31 2 0 #vu8(1 2 3))
             (not (lws-context-inject-event! context 'readable 30 31 2 0 #vu8(4)))
             (begin (lws-context-next-event context) #t)
             (lws-body-consumed context 30 31 2 3)
             (lws-context-inject-event! context 'readable 30 31 2 0 #vu8(4))
             (begin (lws-context-next-event context) #t)
             (lws-body-consumed context 30 31 2 1)))))

(mat net-lws-generation-filtering
     (call-with-lws-context
      (lambda (context)
        (and (lws-context-inject-event! context 'writable 40 41 9 0 #vu8())
             (not (lws-context-inject-event! context 'writable 40 41 8 0 #vu8()))
             (begin (lws-context-next-event context) #t)
             (lws-context-inject-event! context 'reset 40 41 9 1 #vu8())
             (begin (lws-context-next-event context) #t)
             (not (lws-context-inject-event! context 'writable 40 41 8 0 #vu8()))
             (lws-context-inject-event! context 'writable 40 41 10 0 #vu8())
             (begin (lws-context-next-event context) #t)))))

(mat net-lws-bounded-event-pool
     ;; Error case: a full callback queue must fail deterministically instead of growing.
     (let ([context (lws-context-open 2 4)])
       (dynamic-wind
         void
         (lambda ()
           (and (lws-context-inject-event! context 'writable 1 1 1 0 #vu8(1 2 3 4))
                (lws-context-inject-event! context 'complete 1 1 1 0 #vu8())
                (not (lws-context-inject-event! context 'writable 1 1 2 0 #vu8()))
                (let ([metrics (lws-context-pool-metrics context)])
                  (and (= (vector-ref metrics 0) 2)
                       (= (vector-ref metrics 1) 2)
                       (= (vector-ref metrics 2) 2)
                       (positive? (vector-ref metrics 4))))))
         (lambda () (lws-context-close context)))))

(mat net-lws-pool-reuse
     (call-with-lws-context
      (lambda (context)
        (let loop ([generation 1])
          (if (> generation 100)
              (let ([metrics (lws-context-pool-metrics context)])
                (and (= (vector-ref metrics 1) 0)
                     (= (vector-ref metrics 2) 1)
                     (zero? (vector-ref metrics 4))))
              (and (lws-context-inject-event!
                    context 'complete 50 51 generation 0 #vu8())
                   (vector? (lws-context-next-event context))
                   (loop (+ generation 1))))))))

(mat net-lws-private-operation-boundary
     (and (procedure? lws-client-start)
          (procedure? lws-client-body-submit)
          (procedure? lws-client-body-drain)
          (procedure? lws-server-request-dequeue)
          (procedure? lws-server-response-submit)
          (procedure? lws-stream-cancel)
          (procedure? lws-body-consumed)))
