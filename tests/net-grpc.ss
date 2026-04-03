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

(define with-grpc-env
  (lambda (proc)
    (with-env* '(("http_proxy" . "")
                 ("https_proxy" . "")
                 ("all_proxy" . "")
                 ("HTTP_PROXY" . "")
                 ("HTTPS_PROXY" . "")
                 ("ALL_PROXY" . "")
                 ("no_proxy" . "127.0.0.1,localhost")
                 ("NO_PROXY" . "127.0.0.1,localhost"))
               proc)))

(define call-with-grpc-peer
  (lambda (serve-count register proc)
    (let* ([port (reserve-loopback-port)]
           [server (grpc-open-channel 'server "127.0.0.1" port)]
           [client (grpc-open-channel "127.0.0.1" port)])
      (register server)
      (let ([th (fork-thread
                 (lambda ()
                   (let loop ([n serve-count])
                     (unless (= n 0)
                       (grpc-serve server)
                       (loop (- n 1))))))])
        (dynamic-wind
          void
          (lambda () (proc server client))
          (lambda ()
            (thread-join th)
            (grpc-close-channel client)
            (grpc-close-channel server)))))))

(define wait-grpc-call/nonblocking
  (lambda (client method payload metadata timeout-ms)
    (let loop ()
      (let ([resp (grpc-call/nonblocking client method payload metadata timeout-ms)])
        (if resp
            resp
            (begin
              (milisleep 10)
              (loop)))))))

(define grpc-server-stream-ok?
  (lambda (client)
    (let ([stream (grpc-call/server-stream
                   client
                   "/chezpp.test.Echo/ServerStream"
                   "watch"
                   '()
                   2000)])
      (let ([ok?
             (let ([m1 (grpc-stream-recv stream)]
                   [m2 (grpc-stream-recv stream)]
                   [done (grpc-stream-recv stream)])
               (and (grpc-stream? stream)
                    (equal? (utf8->string m1) "watch:0")
                    (equal? (utf8->string m2) "watch:1")
                    (eof-object? done)))])
        (grpc-stream-close stream)
        ok?))))

(define grpc-client-stream-ok?
  (lambda (client)
    (let ([stream (grpc-call/client-stream
                   client
                   "/chezpp.test.Echo/ClientStream"
                   '()
                   2000)])
      (let ([ok?
             (begin
               (grpc-stream-send stream "3")
               (grpc-stream-send stream "5")
               (grpc-stream-send stream "8")
               (grpc-stream-close-send stream)
               (let ([reply (grpc-stream-recv stream)]
                     [done (grpc-stream-recv stream)])
                 (and (equal? (utf8->string reply) "sum:16")
                      (eof-object? done))))])
        (grpc-stream-close stream)
        ok?))))

(define grpc-bidi-stream-ok?
  (lambda (client)
    (let ([stream (grpc-call/bidi-stream
                   client
                   "/chezpp.test.Echo/Bidi"
                   '()
                   2000)])
      (let ([ok?
             (begin
               (grpc-stream-send stream "one")
               (let ([r1 (grpc-stream-recv stream)])
                 (if (equal? (utf8->string r1) "echo:one")
                     (begin
                       (grpc-stream-send stream "two")
                       (let ([r2 (grpc-stream-recv stream)])
                         (and (equal? (utf8->string r2) "echo:two")
                              (begin
                                (grpc-stream-close-send stream)
                                (eof-object? (grpc-stream-recv stream))))))
                     #f)))])
        (grpc-stream-close stream)
        ok?))))

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
     (with-grpc-env
      (lambda ()
        (let ([seen '()])
          (call-with-grpc-peer
           3
           (lambda (server)
             (grpc-register-service!
              server
              "/chezpp.test.Echo/Unary"
              (lambda (req)
                (set! seen (cons (grpc-metadata-ref req "x-mode" #f) seen))
                (grpc-response
                 (grpc-request-payload req)
                 '(("x-reply" . "ok"))
                 0
                 ""))))
           (lambda (server client)
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
              (let ([resp (wait-grpc-call/nonblocking
                           client
                           "/chezpp.test.Echo/Unary"
                           #vu8(1 2 3 4)
                           '(("x-mode" . "nonblocking"))
                           2000)])
                (and
                 (grpc-response? resp)
                 (= (grpc-status-code resp) 0)
                 (equal? (grpc-response-payload resp) #vu8(1 2 3 4))
                 (equal? (grpc-metadata-ref resp "x-reply") "ok")))
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
              (equal? (reverse seen) '("blocking" "nonblocking")))))))))

(mat net-grpc-streams
     (with-grpc-env
      (lambda ()
        (call-with-grpc-peer
         3
         (lambda (server)
           (grpc-register-service!
            server
            "/chezpp.test.Echo/ServerStream"
            'server
            (lambda (stream)
              (let ([payload (grpc-stream-recv stream)])
                (when (bytevector? payload)
                  (let ([text (utf8->string payload)])
                    (grpc-stream-send stream (string-append text ":0"))
                    (grpc-stream-send stream (string-append text ":1"))))
                #f)))
           (grpc-register-service!
            server
            "/chezpp.test.Echo/ClientStream"
            'client
            (lambda (stream)
              (let loop ([parts '()])
                (let ([payload (grpc-stream-recv stream)])
                  (if (eof-object? payload)
                      (string-append "sum:"
                                     (number->string
                                      (apply +
                                             (map string->number
                                                  (reverse parts)))))
                      (loop (cons (utf8->string payload) parts)))))))
           (grpc-register-service!
            server
            "/chezpp.test.Echo/Bidi"
            'bidi
            (lambda (stream)
              (let loop ()
                (let ([payload (grpc-stream-recv stream)])
                  (unless (eof-object? payload)
                    (grpc-stream-send
                     stream
                     (string-append "echo:" (utf8->string payload)))
                    (loop))))
              #f)))
         (lambda (server client)
           (and
            (grpc-channel? server)
            (grpc-channel? client)
            (grpc-server-stream-ok? client)
            (grpc-client-stream-ok? client)
            (grpc-bidi-stream-ok? client)))))))
