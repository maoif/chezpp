(import (chezpp)
        (chezpp net))

(load "net-common.ss")

(define with-env
  (lambda (name value proc)
    (let ([old (getenv name)])
      (dynamic-wind
        (lambda () (putenv name value))
        proc
        (lambda () (putenv name (or old "")))))))

(define with-env*
  (lambda (bindings proc)
    (if (null? bindings)
        (proc)
        (with-env (caar bindings)
                  (cdar bindings)
                  (lambda ()
                    (with-env* (cdr bindings) proc))))))

(mat net-grpc-records
     (let ([req (grpc-request "/chezpp.test.Meta/Inspect"
                              "payload"
                              '(("x-id" . "42")
                                ("x-bin" . #vu8(1 2 3))))])
       (let ([resp (grpc-response #vu8(9 8)
                                  '(("x-answer" . "ok")
                                    ("x-bin" . #vu8(4 5)))
                                  7
                                  "status-text")])
         (and
          (grpc-request? req)
          (equal? (utf8->string (grpc-request-payload req)) "payload")
          (equal? (grpc-metadata-ref req "x-id") "42")
          (equal? (grpc-metadata-ref req "x-bin") #vu8(1 2 3))
          (grpc-response? resp)
          (equal? (grpc-response-payload resp) #vu8(9 8))
          (= (grpc-status-code resp) 7)
          (equal? (grpc-status-message resp) "status-text")
          (equal? (grpc-metadata-ref resp "x-answer") "ok")
          (equal? (grpc-metadata-ref resp "x-bin") #vu8(4 5))))))

(mat net-grpc-unary
     (with-env* '(("http_proxy" . "")
                  ("https_proxy" . "")
                  ("all_proxy" . "")
                  ("HTTP_PROXY" . "")
                  ("HTTPS_PROXY" . "")
                  ("ALL_PROXY" . "")
                  ("no_proxy" . "127.0.0.1,localhost")
                  ("NO_PROXY" . "127.0.0.1,localhost"))
                (lambda ()
                  (let* ([port (reserve-loopback-port)]
                         [server (grpc-open-channel 'server "127.0.0.1" port)]
                         [client (grpc-open-channel "127.0.0.1" port)]
                         [seen '()])
                    (grpc-register-service!
                     server
                     "/chezpp.test.Echo/Unary"
                     (lambda (req)
                       (set! seen (cons (grpc-metadata-ref req "x-mode" #f) seen))
                       (grpc-response
                        (grpc-request-payload req)
                        '(("x-reply" . "ok"))
                        0
                        "")))
                    (let ([th (fork-thread
                               (lambda ()
                                 (grpc-serve server)
                                 (grpc-serve server)
                                 (grpc-serve server)))])
                      (dynamic-wind
                        void
                        (lambda ()
                          (and
                           (grpc-channel? server)
                           (grpc-channel? client)
                           (let ([resp (grpc-call client
                                                  "/chezpp.test.Echo/Unary"
                                                  "hello grpc"
                                                  '(("x-mode" . "blocking"))
                                                  2000)])
                             (and
                              (grpc-response? resp)
                              (= (grpc-status-code resp) 0)
                              (equal? (grpc-status-message resp) "")
                              (equal? (utf8->string (grpc-response-payload resp)) "hello grpc")
                              (equal? (grpc-metadata-ref resp "x-reply") "ok")))
                           (let loop ()
                             (let ([resp (grpc-call/nonblocking client
                                                                "/chezpp.test.Echo/Unary"
                                                                #vu8(1 2 3 4)
                                                                '(("x-mode" . "nonblocking"))
                                                                2000)])
                               (if resp
                                   (and
                                    (grpc-response? resp)
                                    (= (grpc-status-code resp) 0)
                                    (equal? (grpc-response-payload resp) #vu8(1 2 3 4))
                                    (equal? (grpc-metadata-ref resp "x-reply") "ok"))
                                   (begin
                                     (milisleep 10)
                                     (loop)))))
                           (let ([resp (grpc-call client
                                                  "/chezpp.test.Echo/Missing"
                                                  #f
                                                  '()
                                                  2000)])
                             (and
                              (grpc-response? resp)
                              (= (grpc-status-code resp) 12)
                              (equal? (grpc-status-message resp) "unimplemented")
                              (not (grpc-response-payload resp))))
                           (equal? (reverse seen) '("blocking" "nonblocking"))))
                        (lambda ()
                          (thread-join th)
                          (grpc-close-channel client)
                          (grpc-close-channel server))))))))
