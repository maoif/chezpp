(import (chezpp)
        (chezpp net))

(load "net-common.ss")

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
