#|proc:http-file-server
The `http-file-server` procedure serves streamed FileChunk uploads from `dir` and registers a
streaming GET route for each completed file. It remains active until the optional download ends.
|#
(define http-file-server
  (lambda (dir)
    (pcheck ([string? dir])
      (ensure-upload-directory dir)
      (let ([server (http-listen file-transfer-host http-file-transfer-port
                                 http-file-transfer-server-tls-context)]
            [uploaded? #f]
            [downloaded? #f])
        (call-with-values
         (lambda () (make-upload-file-chunk-handler 'http-file-server dir))
         (lambda (handle-chunk! close-handler!)
           (http-register-handler!
            server
            'post
            "/frame"
            (lambda (req)
              (let ([body (http-request-body req)])
                (unless (bytevector? body)
                  (errorf 'http-file-server "expected bytevector FileChunk body"))
                (let* ([chunk (bytevector->file-chunk body)]
                       [result (handle-chunk! chunk)])
                  (when result
                    (set! uploaded? #t)
                    (let ([name (file-chunk-name chunk)])
                      (http-register-handler!
                       server 'get (string-append "/download/" name)
                       (lambda (download-request)
                         (set! downloaded? #t)
                         (make-http-response
                          200 "OK"
                          '(("content-type" . "application/octet-stream"))
                          (make-http-file-body-source
                           (validated-upload-path 'http-file-server dir name))))))))
                (make-http-response 200 "OK" '() "ok"))))
           (dynamic-wind
             void
             (lambda ()
               (let loop ()
                 (unless (and uploaded?
                              (or (not (getenv "CHEZPP_TRANSFER_DOWNLOAD")) downloaded?))
                   (guard (c [else
                              (unless (benign-http-eof? c) (raise c))])
                     (http-serve server))
                   (loop)))
               dir)
             (lambda ()
               (close-handler!)
               (http-server-close server)))))))))

#|proc:http-file-client
The `http-file-client` procedure streams each `path*` through POST FileChunk requests. If
`CHEZPP_TRANSFER_DOWNLOAD` is set, it then uses GET streaming to download the first file there.
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
          (let ([rss-before (current-peak-rss-kib)])
           (for-each
           (lambda (path)
             (send-path-via-file-chunks
              (lambda (chunk)
                (http-send client
                           (make-http-request
                            'post (string-append base "/frame") '()
                            (file-chunk-encode chunk))))
              path))
           path*)
          (let ([after-upload (current-peak-rss-kib)]
                [destination (getenv "CHEZPP_TRANSFER_DOWNLOAD")])
            (when (and destination (not (string=? destination "")))
              (http-download client
                             (string-append base "/download/" (path-basename (car path*)))
                             destination))
            (write-transfer-rss-growth-value!
             (max (- after-upload rss-before)
                  (- (current-peak-rss-kib) after-upload))))
           path*))
        (lambda ()
          (http-close client))))))
