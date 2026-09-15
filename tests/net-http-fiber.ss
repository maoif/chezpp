(import (chezpp)
        (chezpp concurrency fiber)
        (chezpp net operation))

(load "net-common.ss")
(load "net-lws-fixture.ss")

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

(mat net-operation-fiber-event-advances-on-driver
     (let ([scheduler-thread #f] [driver-thread #f])
       (and
        (eq? 'done
             (run-fibers
              (lambda ()
                (set! scheduler-thread (get-thread-id))
                (net-operation-wait
                 (make-net-operation
                  'fiber-driver
                  (lambda ()
                    (set! driver-thread (get-thread-id))
                    (net-operation-completed 'done))
                  void)))))
        (not (equal? scheduler-thread driver-thread)))))

(mat net-operation-fiber-cancelled-condition
     ;; Error case: a cancelled operation resumes a fiber with the original condition.
     (let ([operation (make-net-operation 'cancelled
                                         (lambda () (net-operation-pending '() #f)) void)])
       (net-operation-cancel! operation)
       (let ([failure (net-operation-condition operation)])
         (run-fibers
          (lambda ()
            (guard (condition [else (eq? failure condition)])
              (net-operation-wait operation)
              #f))))))

(mat net-operation-fiber-shared-waiters
     (let ([ready (abox #f)] [steps 0])
       (run-fibers
        (lambda ()
          (let ([results (make-channel)]
                [operation
                 (make-net-operation
                  'shared-fiber-result
                  (lambda ()
                    (set! steps (fx1+ steps))
                    (if (unabox ready)
                        (net-operation-completed 'shared)
                        (net-operation-pending '() #f)))
                  void)])
            (for-each
             (lambda (_)
               (spawn-fiber (lambda () (channel-put! results (net-operation-wait operation)))))
             (iota 4))
            (abox-set! ready #t)
            (and (for-all (lambda (_) (eq? 'shared (channel-get results))) (iota 4))
                 (positive? steps)))))))

(mat net-operation-fiber-choice-does-not-start-unused-work
     (let ([steps 0])
       (and (eq? 'chosen
                 (run-fibers
                  (lambda ()
                    (event-sync
                     (event-select
                      (net-operation-event
                       (make-net-operation
                        'unused-choice
                        (lambda ()
                          (set! steps (fx1+ steps))
                          (net-operation-pending '() #f))
                        void))
                      (event-always 'chosen))))))
            (fxzero? steps))))

(mat net-operation-fiber-waiter-reuse
     (run-fibers
      (lambda ()
        (and (for-all
              (lambda (value)
                (eqv? value
                      (net-operation-wait
                       (make-net-operation 'reused-waiter
                                           (lambda () (net-operation-completed value)) void))))
              (iota 100))
             (let ([metrics (net-operation-event-pool-metrics)])
               (and (fxzero? (vector-ref metrics 0))
                    (fxzero? (vector-ref metrics 1))
                    (fx<= (vector-ref metrics 2) 4)))))))

(mat net-operation-fiber-slow-callback-isolation
     ;; Error case: a slow callback must not prevent an unrelated operation from making progress.
     (let ([started (abox #f)] [released (abox #f)] [timed-out (abox #f)])
       (run-fibers
        (lambda ()
          (let ([done (make-channel)])
            (spawn-fiber
             (lambda ()
               (net-operation-wait
                (make-net-operation
                 'slow-callback
                 (lambda ()
                   (abox-set! started #t)
                   (let loop ([remaining 500])
                     (unless (unabox released)
                       (if (fxzero? remaining)
                           (abox-set! timed-out #t)
                           (begin (milisleep 1) (loop (fx1- remaining))))))
                   (net-operation-completed 'slow))
                 void))
               (channel-put! done #t)))
            (let wait () (unless (unabox started) (fiber-yield) (wait)))
            (net-operation-wait
             (make-net-operation
              'fast-callback
              (lambda () (abox-set! released #t) (net-operation-completed 'fast))
              void))
            (and (channel-get done) (not (unabox timed-out))))))))

(mat net-operation-fiber-shutdown-releases-waiters
     (let ([operation (make-net-operation 'abandoned
                                         (lambda () (net-operation-pending '() #f)) void)])
       (run-fibers
        (lambda ()
          (spawn-fiber (lambda () (net-operation-wait operation)))
          (let wait ()
            (when (fxzero? (vector-ref (net-operation-event-pool-metrics) 1))
              (fiber-yield)
              (wait)))))
       (let wait ([remaining 500])
         (let ([metrics (net-operation-event-pool-metrics)])
           (cond
            [(and (fxzero? (vector-ref metrics 0)) (fxzero? (vector-ref metrics 1)))
             (net-operation-cancel! operation)
             #t]
            [(fxzero? remaining) (net-operation-cancel! operation) #f]
            [else (milisleep 1) (wait (fx1- remaining))])))))

(mat net-operation-fiber-live-http1
     (let* ([port (+ 19000 (modulo (get-process-id) 20000))]
            [server (http-listen "127.0.0.1" port)]
            [client (http-open)]
            [server-thread #f])
       (dynamic-wind
         void
         (lambda ()
           (http-set-timeout! client 1000)
           (http-register-handler! server 'get "/fiber"
             (lambda (request) (make-http-response 200 "OK" '() "fiber-ok" '() 'h1)))
           (set! server-thread (fork-thread (lambda () (http-serve server))))
           (let ([response
                  (run-fibers
                   (lambda ()
                     (http-get client (format "http://127.0.0.1:~a/fiber" port))))])
             (thread-join server-thread)
             (and (eq? 'h1 (http-response-version response))
                  (equal? "fiber-ok" (utf8->string (http-response-body response))))))
         (lambda () (http-close client) (http-server-close server)))))

(mat net-operation-fiber-live-mixed-stress
     ;; Reuse the clients across scheduler lifetimes while both protocols complete concurrently.
     (call-with-h2-tls-fixture
      16
      (lambda (port command)
        (let* ([tls (make-tls-context 'client)]
               [http1 (http-open tls)] [http2 (http-open tls)] [high-water #f])
          (dynamic-wind
            void
            (lambda ()
              (tls-context-load-ca-file! tls "/tmp/chezpp-net-test-cert.pem")
              (tls-context-set-verify! tls #t)
              (http-client-version-set! http1 'http/1.1)
              (http-client-version-set! http2 'h2)
              (http-set-timeout! http1 2000)
              (http-set-timeout! http2 2000)
              (for-all
               (lambda (round)
                 (and
                  (run-fibers
                   (lambda ()
                     (let ([results (make-channel)])
                       (for-each
                        (lambda (index)
                          (spawn-fiber
                           (lambda ()
                             (channel-put! results
                               (guard (condition [else #f])
                                 (let ([response
                                        (http-get (if (even? index) http1 http2)
                                          (format "https://127.0.0.1:~a/" port))])
                                   (and (= 200 (http-response-status response))
                                        (eq? (if (even? index) 'h1 'h2)
                                             (http-response-version response))
                                        (equal? (make-bytevector 10 120)
                                                (http-response-body response)))))))))
                        (iota 8))
                       (for-all (lambda (_) (channel-get results)) (iota 8)))))
                  (let ([metrics (net-operation-event-pool-metrics)])
                    (unless high-water (set! high-water (vector-ref metrics 2)))
                    (and (fxzero? (vector-ref metrics 0))
                         (fxzero? (vector-ref metrics 1))
                         (fx<= (vector-ref metrics 2) 8)))))
               (iota 8)))
            (lambda () (http-close http1) (http-close http2) (close-tls-context tls)))))))

(mat net-operation-fiber-live-cancel-timeout-close
     ;; Error cases: cancel, deadline expiry, and client shutdown must release live H2 waiters.
     (call-with-h2-fixture
      8
      (lambda (port command)
        (for-all
         (lambda (mode)
           (let ([client (http-open)])
             (dynamic-wind
               void
               (lambda ()
                 (http-client-version-set! client 'h2)
                 (http-set-timeout! client (if (eq? mode 'timeout) 100 1500))
                 (let* ([operation (http-send/nonblocking client
                                     (make-http-request 'get
                                       (format "http://127.0.0.1:~a/hold" port)))]
                        [result
                         (run-fibers
                          (lambda ()
                            (let ([results (make-channel)])
                              (spawn-fiber
                               (lambda ()
                                 (channel-put! results
                                   (guard (condition [else #t])
                                     (net-operation-wait operation) #f))))
                              (let wait ([remaining 10000])
                                (when (and (positive? remaining)
                                           (fxzero? (vector-ref (net-operation-event-pool-metrics) 1)))
                                  (fiber-yield) (wait (fx1- remaining))))
                              (case mode
                                [(cancel) (net-operation-cancel! operation)]
                                [(close) (http-close client)])
                              (channel-get results))))])
                   (and result
                        (let ([metrics (net-operation-event-pool-metrics)])
                          (and (fxzero? (vector-ref metrics 0))
                               (fxzero? (vector-ref metrics 1)))))))
               (lambda () (http-close client)))))
         '(cancel timeout close)))))
