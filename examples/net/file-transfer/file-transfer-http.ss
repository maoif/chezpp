#|proc:http-file-server
The `http-file-server` procedure listens on localhost and stores uploaded HTTP
frame bodies in `dir` until the client sends a `done` frame.
|#
(define http-file-server
  (lambda (dir)
    (pcheck ([string? dir])
      (ensure-upload-directory dir)
      (let ([server (http-listen file-transfer-host http-file-transfer-port
                                 http-file-transfer-server-tls-context)]
            [done? #f])
        (call-with-values
         (lambda ()
           (make-upload-frame-handler 'http-file-server dir))
         (lambda (handle-frame! close-handler!)
           (http-register-handler!
            server
            'post
            "/frame"
            (lambda (req)
              (let ([body (http-request-body req)])
                (unless (bytevector? body)
                  (errorf 'http-file-server "expected bytevector request body"))
                (call-with-values
                 (lambda ()
                   (parse-stream-transfer-frame body))
                 (lambda (tag name payload)
                   (when (handle-frame! tag name payload)
                     (set! done? #t))))
                (make-http-response 200 "OK" '() "ok"))))
           (dynamic-wind
             void
             (lambda ()
               (let loop ()
                 (unless done?
                   (guard (c [else
                              (unless (benign-http-eof? c)
                                (raise c))])
                 (http-serve server))
                   (loop)))
               dir)
             (lambda ()
               (close-handler!)
               (http-server-close server)))))))))

#|proc:http-file-client
The `http-file-client` procedure uploads each file in `path*` to the localhost
HTTP file server using streamed request frames and then sends the done frame.
|#
(define http-file-client
  (lambda (path*)
    (validate-file-list 'http-file-client path*)
    (let ([client (if http-file-transfer-client-tls-context
                      (http-open http-file-transfer-client-tls-context)
                      (http-open))]
          [base (format "~a://~a:~a"
                        (if http-file-transfer-client-tls-context "https" "http")
                        file-transfer-host http-file-transfer-port)])
      (dynamic-wind
        void
        (lambda ()
          (for-each
           (lambda (path)
             (send-path-via-transfer-frames
              (lambda (tag name payload)
                (http-send
                 client
                 (make-http-request
                  'post
                  (string-append base "/frame")
                  '()
                  (make-stream-transfer-frame tag name payload))))
              path))
           path*)
          (http-send
           client
           (make-http-request
            'post
            (string-append base "/frame")
            '()
            (make-stream-transfer-frame transfer-frame-tag-done "" #vu8())))
          path*)
        (lambda ()
          (http-close client))))))
