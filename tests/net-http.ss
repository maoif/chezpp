(import (chezpp)
        (chezpp net))

(load "net-common.ss")

(define http-net-error-message?
  (lambda (message thunk)
    (guard (c [else
               (and (net-error? c)
                    (equal? (net-error-message c) message))])
      (thunk)
      #f)))

(define http-error-message-contains?
  (lambda (fragment thunk)
    (guard (c [else
               (and (condition? c)
                    (string-contains?
                     (call-with-string-output-port
                      (lambda (p) (display-condition c p)))
                     fragment))])
      (thunk)
      #f)))

(define start-http-dispatch-loop-server
  (lambda (serve-count setup . maybe-tls-ctx)
    (let* ([tls-ctx (if (null? maybe-tls-ctx) #f (car maybe-tls-ctx))]
           [port (reserve-loopback-port)]
           [server (http-listen "127.0.0.1" port tls-ctx)]
           [done? #f])
      (setup server)
      (values server
              port
              (fork-thread
               (lambda ()
                 (let loop ([n serve-count])
                   (unless (or done? (= n 0))
                     (guard (c [else #f])
                       (http-serve server))
                     (loop (- n 1))))))
              (lambda ()
                (set! done? #t)
                (http-server-close server))))))

(define start-http-cancel-cache-server
  (lambda ()
    (let* ([port (reserve-loopback-port)]
           [server (http-listen "127.0.0.1" port)]
           [done? #f]
           [accept-count 0])
      (define respond!
        (lambda (conn next-id)
          (let* ([req (http-read-request conn)]
                 [path (uri-path (http-request-uri req))]
                 [resp (cond
                        [(string=? path "/slow-cancel")
                         (milisleep 120)
                         (make-http-response 200
                                             "OK"
                                             '()
                                             (format "slow-conn-~a" next-id))]
                        [(string=? path "/after-cancel")
                         (set! done? #t)
                         (make-http-response 200
                                             "OK"
                                             '()
                                             (format "after-conn-~a" next-id))]
                        [else
                         (make-http-response 404 "Not Found" '() "missing")])])
            (http-write-response conn resp))))
      (values server
              port
              (fork-thread
               (lambda ()
                 (let loop ([conn-id 0])
                   (unless done?
                     (guard (c [else #f])
                       (let ([conn (http-accept server)])
                         (let ([next-id (+ conn-id 1)])
                           (set! accept-count next-id)
                           (dynamic-wind
                             void
                             (lambda ()
                               (respond! conn next-id))
                             (lambda ()
                               (guard (c [else #f])
                                 (http-connection-close conn))))
                           (unless done?
                             (loop next-id)))))))))
              (lambda ()
                (set! done? #t)
                (http-server-close server))
              (lambda () accept-count)))))

(define await-http-nonblocking
  (lambda (thunk)
    (let loop ([i 0])
      (let ([ans (thunk)])
        (if ans
            ans
            (begin
              (when (> i 100)
                (error 'await-http-nonblocking "HTTP nonblocking operation did not complete"))
              (milisleep 10)
              (loop (+ i 1))))))))

(mat net-http-runtime
     (let-values ([(server port th)
                   (start-http-connection-server
                    (lambda (conn)
                      (let ([req (http-read-request conn)])
                        (http-write-response
                         conn
                         (make-http-response
                          200
                          "OK"
                          `(("Content-Type" . "text/plain")
                            ("X-Client" . ,(or (http-header-ref (http-request-headers req)
                                                                "X-Client"
                                                                #f)
                                               ""))
                            ("X-Method" . ,(http-request-method req)))
                          "hello")))))])
       (let ([client (http-open)])
         (and (not (http-accept/nonblocking server))
              (begin
                (http-set-timeout! client 250)
                #t)
              (begin
                (http-set-header! client "X-Client" "ok")
                #t)
              (let ([resp (http-send
                           client
                           (make-http-request
                            'get
                            (format "http://127.0.0.1:~a/hello?q=1" port)
                            '(("Accept" . "text/plain"))
                            #f))])
                (begin
                  (http-close client)
                  (thread-join th)
                  (and (= (http-response-status resp) 200)
                       (equal? (http-header-ref (http-response-headers resp) "X-Client")
                               "ok")
                       (equal? (http-header-ref (http-response-headers resp) "X-Method")
                               "GET")
                       (equal? (utf8->string (http-response-body resp)) "hello")))))))
     (let-values ([(server port th)
                   (start-http-dispatch-server
                    (lambda (server)
                      (http-register-handler!
                       server
                       'head
                       "/meta"
                       (lambda (req)
                         (make-http-response
                          200
                          "OK"
                          '(("X-Head" . "yes"))
                          "body-ignored")))))])
       (let ([resp (http-head (format "http://127.0.0.1:~a/meta" port))])
         (thread-join th)
         (and (= (http-response-status resp) 200)
              (equal? (http-header-ref (http-response-headers resp) "X-Head")
                      "yes")
              (not (http-response-body resp))))))

(mat net-http-transfer
     (let* ([upload-path "/tmp/chezpp-net-upload.bin"]
            [payload (string->utf8 "payload")])
       (write-bytevector-file upload-path payload)
       (let-values ([(server port th)
                     (start-http-connection-server
                      (lambda (conn)
                        (let ([req (http-read-request conn)])
                          (http-write-response
                           conn
                           (make-http-response
                            200
                            "OK"
                            `(("X-Size" . ,(number->string
                                            (bytevector-length
                                             (http-request-body req)))))
                            (http-request-body req))))))])
         (let ([client (http-open)])
           (let ([resp (http-upload client
                                    (format "http://127.0.0.1:~a/upload" port)
                                    upload-path)])
             (begin
               (http-close client)
               (thread-join th)
               (and (= (http-response-status resp) 200)
                    (equal? (http-header-ref (http-response-headers resp) "X-Size")
                            "7")
                    (equal? (http-response-body resp) payload))))))
     (let* ([download-path "/tmp/chezpp-net-download.bin"]
            [payload #vu8(1 2 3 4 5)])
       (let-values ([(server port th)
                     (start-http-dispatch-server
                      (lambda (server)
                        (http-register-handler!
                         server
                         'get
                         "/download"
                         (lambda (req)
                           (make-http-response
                            200
                            "OK"
                            '(("Content-Type" . "application/octet-stream"))
                            payload)))))])
         (let ([client (http-open)])
           (let ([resp (http-download client
                                      (format "http://127.0.0.1:~a/download" port)
                                      download-path)])
             (begin
               (http-close client)
               (thread-join th)
               (and (= (http-response-status resp) 200)
                    (equal? (http-response-body resp) payload)
                    (equal? (read-u8vec download-path) payload)))))))))

(mat net-https
     (let ([server-ctx (make-test-http-server-context)]
           [client-ctx (make-test-http-client-context)])
       (let-values ([(server port th)
                     (start-http-dispatch-server
                      (lambda (server)
                        (http-register-handler!
                         server
                         'get
                         "/secure"
                         (lambda (req)
                           (make-http-response
                            200
                            "OK"
                            '(("Content-Type" . "text/plain"))
                            "secure"))))
                      server-ctx)])
         (let ([client (http-open client-ctx)])
           (let ([resp (http-get client
                                 (format "https://127.0.0.1:~a/secure" port))])
             (begin
               (http-close client)
               (close-tls-context client-ctx)
               (close-tls-context server-ctx)
               (thread-join th)
               (and (= (http-response-status resp) 200)
                    (equal? (utf8->string (http-response-body resp)) "secure"))))))))

(mat net-http-reuse
     (call-with-values
      (lambda ()
        (start-http-dispatch-loop-server
         1
         (lambda (server)
           (let ((count 0))
             (http-register-handler!
              server
              'get
              "/reuse"
              (lambda (req)
                (set! count (+ count 1))
                (make-http-response
                 200
                 "OK"
                 (if (= count 2)
                     '(("Connection" . "close")
                       ("X-Seq" . "2"))
                     '(("X-Seq" . "1")))
                 "reuse")))))))
      (lambda (server port th stop)
        (let ((client (http-open)))
          (dynamic-wind
            void
            (lambda ()
              (let ((resp1 (http-get client (format "http://127.0.0.1:~a/reuse" port))))
                (let ((resp2 (http-get client (format "http://127.0.0.1:~a/reuse" port))))
                  (and (= (http-response-status resp1) 200)
                       (= (http-response-status resp2) 200)
                       (equal? (http-header-ref (http-response-headers resp1) "Connection")
                               "keep-alive")
                       (equal? (http-header-ref (http-response-headers resp1) "X-Seq")
                               "1")
                       (equal? (http-header-ref (http-response-headers resp2) "Connection")
                               "close")
                       (equal? (http-header-ref (http-response-headers resp2) "X-Seq")
                               "2")
                       (equal? (utf8->string (http-response-body resp2)) "reuse")))))
            (lambda ()
              (http-close client)
              (stop)
              (thread-join th)))))))

(mat net-http-timeout
     (let-values ([(server port th stop)
                   (start-http-dispatch-loop-server
                    1
                    (lambda (server)
                      (http-register-handler!
                       server
                       'get
                       "/slow"
                       (lambda (req)
                         (milisleep 150)
                         (make-http-response 200 "OK" '() "slow")))))])
       (let ([client (http-open)])
         (dynamic-wind
           void
           (lambda ()
             (http-set-timeout! client 50)
             (http-net-error-message?
              "HTTP request timed out"
              (lambda ()
                (http-get client (format "http://127.0.0.1:~a/slow" port)))))
           (lambda ()
             (http-close client)
             (stop)
             (thread-join th)))))
     (let-values ([(server port th stop)
                   (start-http-dispatch-loop-server
                    2
                    (lambda (server)
                      (http-register-handler!
                       server
                       'get
                       "/a"
                       (lambda (req)
                         (milisleep 40)
                         (make-http-response
                          302
                          "Found"
                          '(("Location" . "/b"))
                          #f)))
                      (http-register-handler!
                       server
                       'get
                       "/b"
                       (lambda (req)
                         (milisleep 40)
                         (make-http-response 200 "OK" '() "done")))))])
       (let ([client (http-open)])
         (dynamic-wind
           void
           (lambda ()
             (http-set-timeout! client 60)
             (http-follow-redirects! client #t)
             (http-net-error-message?
              "HTTP request timed out"
              (lambda ()
                (http-get client (format "http://127.0.0.1:~a/a" port)))))
           (lambda ()
             (http-close client)
             (stop)
             (thread-join th)))))
     (let ([server-ctx (make-test-http-server-context)]
           [client-ctx (make-test-http-client-context)])
       (let-values ([(server port th stop)
                     (start-http-dispatch-loop-server
                      1
                      (lambda (server)
                        (http-register-handler!
                         server
                         'get
                         "/slow"
                         (lambda (req)
                           (milisleep 150)
                           (make-http-response 200 "OK" '() "secure"))))
                      server-ctx)])
         (let ([client (http-open client-ctx)])
           (dynamic-wind
             void
             (lambda ()
               (http-set-timeout! client 50)
               (http-net-error-message?
                "HTTP request timed out"
                (lambda ()
                  (http-get client (format "https://127.0.0.1:~a/slow" port)))))
             (lambda ()
               (http-close client)
               (stop)
               (thread-join th)
               (close-tls-context client-ctx)
               (close-tls-context server-ctx)))))))

(mat net-http-timeout-validation
     (let ([client (http-open)])
       (dynamic-wind
         void
         (lambda ()
           (http-error-message-contains?
            "timeout must be non-negative"
            (lambda ()
              (http-set-timeout! client -1))))
         (lambda ()
           (http-close client)))))

(mat net-http-listen-validation
     (and
      (http-error-message-contains?
       "port must be between 0 and 65535"
       (lambda ()
         (http-listen "127.0.0.1" -1)))
      (http-error-message-contains?
       "port must be between 0 and 65535"
       (lambda ()
         (http-listen "127.0.0.1" 70000)))
      (http-error-message-contains?
       "backlog must be non-negative"
       (lambda ()
         (http-listen "127.0.0.1" 0 #f -1)))))

(mat net-http-listen-failure-cleanup
     (let ([sock (open-socket 'inet 'stream)])
       (dynamic-wind
         (lambda ()
           (socket-set-option! sock 'reuse-address #t)
           (socket-bind! sock (make-socket-address 'inet "127.0.0.1" 0))
           (socket-listen! sock 4))
         (lambda ()
           (let ([port (socket-address-port (socket-local-address sock))]
                 [before (proc-fd-count)])
             (and
              (let loop ([i 0])
                (if (= i 8)
                    #t
                    (and (guard (c [else #t])
                           (http-listen "127.0.0.1" port)
                           #f)
                         (loop (+ i 1)))))
              (= before (proc-fd-count)))))
         (lambda ()
           (close-socket sock)))))

(mat net-http-request-failure-cleanup
     (let ([client (http-open)]
           [port (reserve-loopback-port)])
       (dynamic-wind
         void
         (lambda ()
           (let ([before (proc-fd-count)]
                 [uri (format "http://127.0.0.1:~a/unreachable" port)])
             (and
              (let loop ([i 0])
                (if (= i 8)
                    #t
                    (and (guard (c [else #t])
                           (http-get client uri)
                           #f)
                         (loop (+ i 1)))))
              (= before (proc-fd-count)))))
         (lambda ()
           (http-close client)))))

(mat net-http-accept-failure-cleanup
     (let ([server-ctx (make-test-http-server-context)])
       (let* ([port (reserve-loopback-port)]
              [server (http-listen "127.0.0.1" port server-ctx)])
         (dynamic-wind
           void
           (lambda ()
             (let ([before (proc-fd-count)])
               (and
                (let loop ([i 0])
                  (if (= i 8)
                      #t
                      (let ([sock (open-socket 'inet 'stream)])
                        (dynamic-wind
                          (lambda ()
                            (socket-connect! sock
                                             (make-socket-address 'inet "127.0.0.1" port))
                            (close-socket sock))
                          (lambda ()
                            (and (guard (c [else #t])
                                   (let ([conn (http-accept server)])
                                     (http-connection-close conn)
                                     #f))
                                 (loop (+ i 1))))
                          (lambda ()
                            (guard (c [else #f])
                              (close-socket sock)))))))
                (= before (proc-fd-count)))))
           (lambda ()
             (http-server-close server)
             (close-tls-context server-ctx))))))

(mat net-http-nonblocking
     (let-values ([(server port th stop)
                   (start-http-dispatch-loop-server
                    1
                    (lambda (server)
                      (http-register-handler!
                       server
                       'get
                       "/slow"
                       (lambda (req)
                         (milisleep 60)
                         (make-http-response 200 "OK" '() "nb-ok")))))])
       (let ([client (http-open)])
         (dynamic-wind
           void
           (lambda ()
             (let* ([request (make-http-request 'get
                                                (format "http://127.0.0.1:~a/slow" port)
                                                '()
                                                #f)]
                    [first (http-send/nonblocking client request)])
               (let ([resp (await-http-nonblocking
                            (lambda ()
                              (http-send/nonblocking client request)))])
                 (and (not first)
                      (= (http-response-status resp) 200)
                      (equal? (utf8->string (http-response-body resp)) "nb-ok")))))
           (lambda ()
             (http-close client)
             (stop)
             (thread-join th)))))
     (let-values ([(server port th stop)
                   (start-http-dispatch-loop-server
                    1
                    (lambda (server)
                      (http-register-handler!
                       server
                       'post
                       "/echo"
                       (lambda (req)
                         (milisleep 60)
                         (make-http-response 200
                                             "OK"
                                             '(("Content-Type" . "text/plain"))
                                             (http-request-body req))))))])
       (let ([client (http-open)])
         (dynamic-wind
           void
           (lambda ()
             (let ([first (http-request/nonblocking client
                                                    'post
                                                    (format "http://127.0.0.1:~a/echo" port)
                                                    '(("Content-Type" . "text/plain"))
                                                    "payload")])
               (let ([resp (await-http-nonblocking
                            (lambda ()
                              (http-request/nonblocking client
                                                        'post
                                                        (format "http://127.0.0.1:~a/echo" port)
                                                        '(("Content-Type" . "text/plain"))
                                                        "payload")))])
                 (and (not first)
                      (= (http-response-status resp) 200)
                      (equal? (utf8->string (http-response-body resp)) "payload")))))
           (lambda ()
             (http-close client)
             (stop)
             (thread-join th)))))
     (let* ([download-path "/tmp/chezpp-net-download-nonblocking.bin"]
            [payload #vu8(10 20 30 40)])
       (let-values ([(server port th stop)
                     (start-http-dispatch-loop-server
                      1
                      (lambda (server)
                        (http-register-handler!
                         server
                         'get
                         "/download-nb"
                         (lambda (req)
                           (milisleep 60)
                           (make-http-response
                            200
                            "OK"
                            '(("Content-Type" . "application/octet-stream"))
                            payload)))))])
         (let ([client (http-open)])
           (dynamic-wind
             void
             (lambda ()
               (let ([first (http-download/nonblocking
                             client
                             (format "http://127.0.0.1:~a/download-nb" port)
                             download-path)])
                 (let ([resp (await-http-nonblocking
                              (lambda ()
                                (http-download/nonblocking
                                 client
                                 (format "http://127.0.0.1:~a/download-nb" port)
                                 download-path)))])
                   (and (not first)
                        (= (http-response-status resp) 200)
                        (equal? (http-response-body resp) payload)
                        (equal? (read-u8vec download-path) payload)))))
             (lambda ()
               (http-close client)
               (stop)
               (thread-join th))))))
     (let* ([upload-path "/tmp/chezpp-net-upload-nonblocking.bin"]
            [payload (string->utf8 "nb-upload")])
       (write-bytevector-file upload-path payload)
       (let-values ([(server port th stop)
                     (start-http-dispatch-loop-server
                      1
                      (lambda (server)
                        (http-register-handler!
                         server
                         'put
                         "/upload-nb"
                         (lambda (req)
                           (milisleep 60)
                           (make-http-response 200 "OK" '() (http-request-body req))))))])
         (let ([client (http-open)])
           (dynamic-wind
             void
             (lambda ()
               (let ([first (http-upload/nonblocking
                             client
                             (format "http://127.0.0.1:~a/upload-nb" port)
                             upload-path)])
                 (let ([resp (await-http-nonblocking
                              (lambda ()
                                (http-upload/nonblocking
                                 client
                                 (format "http://127.0.0.1:~a/upload-nb" port)
                                 upload-path)))])
                   (and (not first)
                        (= (http-response-status resp) 200)
                        (equal? (http-response-body resp) payload)))))
             (lambda ()
               (http-close client)
               (stop)
               (thread-join th)))))))

(mat net-http-cancel
     (let-values ([(server port th stop)
                   (start-http-dispatch-loop-server
                    2
                    (lambda (server)
                      (http-register-handler!
                       server
                       'get
                       "/slow-cancel"
                       (lambda (req)
                         (milisleep 120)
                         (make-http-response 200 "OK" '() "slow")))
                      (http-register-handler!
                       server
                       'get
                       "/after-cancel"
                       (lambda (req)
                         (make-http-response 200 "OK" '() "after")))))])
       (let ([client (http-open)])
         (dynamic-wind
           void
           (lambda ()
             (let* ([slow-request (make-http-request 'get
                                                     (format "http://127.0.0.1:~a/slow-cancel" port)
                                                     '()
                                                     #f)]
                    [first (http-send/nonblocking client slow-request)])
               (and (not first)
                    (begin
                      (http-cancel-pending! client)
                      #t)
                    (let ([resp (http-get client
                                          (format "http://127.0.0.1:~a/after-cancel" port))])
                      (and (= (http-response-status resp) 200)
                           (equal? (utf8->string (http-response-body resp)) "after"))))))
           (lambda ()
             (http-close client)
             (stop)
             (thread-join th))))))

(mat net-http-cancel-does-not-overwrite-cache
     (let-values ([(server port th stop accepted-count)
                   (start-http-cancel-cache-server)])
       (let ([client (http-open)])
         (dynamic-wind
           void
           (lambda ()
             (let* ([slow-request (make-http-request 'get
                                                     (format "http://127.0.0.1:~a/slow-cancel" port)
                                                     '()
                                                     #f)]
                    [first (http-send/nonblocking client slow-request)])
               (and (not first)
                    (begin
                      (http-cancel-pending! client)
                      #t)
                    (begin
                      (milisleep 200)
                      #t)
                    (let ([after (http-get client
                                           (format "http://127.0.0.1:~a/after-cancel" port))])
                      (and
                       (= (http-response-status after) 200)
                       (equal? (utf8->string (http-response-body after)) "after-conn-2")
                       (= (accepted-count) 2))))))
           (lambda ()
             (http-close client)
             (stop)
             (thread-join th))))))
