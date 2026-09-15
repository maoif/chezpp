#|proc:websocket-file-server
The `websocket-file-server` procedure receives generated FileChunk upload messages and, when a
download is requested, sends bounded FileChunk messages for the completed file in `dir`.
|#
(define websocket-file-server
  (lambda (dir)
    (pcheck ([string? dir])
      (ensure-upload-directory dir)
      (let ([server (if websocket-file-transfer-options
                        (websocket-listen file-transfer-host websocket-file-transfer-port
                                          websocket-file-transfer-options)
                        (websocket-listen file-transfer-host websocket-file-transfer-port))]
            [conn #f])
        (call-with-values
         (lambda () (make-upload-file-chunk-handler 'websocket-file-server dir))
         (lambda (handle-chunk! close-handler!)
           (dynamic-wind
             void
             (lambda ()
               (milisleep 1500)
               (set! conn (await-websocket-client server))
               (let ([upload-complete? #f])
                 (let loop ()
                   (let ([message (websocket-recv conn)])
                     (unless (and (websocket-message? message)
                                  (eq? (websocket-message-type message) 'binary))
                       (errorf 'websocket-file-server "expected binary FileChunk message"))
                     (let ([chunk (bytevector->file-chunk
                                   (websocket-message-data message))])
                       (if upload-complete?
                           (begin
                             (unless (and (not (file-chunk-done? chunk))
                                          (= (file-chunk-offset chunk) 0))
                               (errorf 'websocket-file-server
                                       "invalid download request FileChunk"))
                             (send-path-via-file-chunks
                              (lambda (response)
                                (websocket-send-binary conn (file-chunk-encode response)))
                              (validated-upload-path
                               'websocket-file-server dir (file-chunk-name chunk)))
                             dir)
                           (begin
                             (if (handle-chunk! chunk)
                                 (if (getenv "CHEZPP_TRANSFER_DOWNLOAD")
                                     (begin (set! upload-complete? #t) (loop))
                                     dir)
                                 (loop)))))))))
             (lambda ()
               (close-handler!)
               (when conn (websocket-close conn))
               (websocket-server-close server)))))))))

#|proc:websocket-file-client
The `websocket-file-client` procedure sends FileChunk uploads for `path*`. If
`CHEZPP_TRANSFER_DOWNLOAD` is set, it requests and streams the first file to that destination.
|#
(define websocket-file-client
  (lambda (path*)
    (validate-file-list 'websocket-file-client path*)
    (let ([conn (websocket-connect
                 (format "~a://~a:~a/"
                         (if websocket-file-transfer-options "wss" "ws")
                         file-transfer-host websocket-file-transfer-port)
                 (or websocket-file-transfer-options "chezpp-websocket"))])
      (dynamic-wind
        void
        (lambda ()
          (let ([rss-before (current-peak-rss-kib)])
           (for-each
           (lambda (path)
             (send-path-via-file-chunks
              (lambda (chunk)
                (websocket-send-binary conn (file-chunk-encode chunk)))
              path))
           path*)
          (let ([after-upload (current-peak-rss-kib)]
                [destination (getenv "CHEZPP_TRANSFER_DOWNLOAD")])
            (when (and destination (not (string=? destination "")))
              (websocket-send-binary
               conn
               (file-chunk-encode
                (make-file-chunk (path-basename (car path*)) 0 #vu8() #vu8() #f)))
              (call-with-values
               (lambda () (make-file-chunk-writer 'websocket-file-client destination 0))
               (lambda (write-chunk! close-writer! next-offset)
                 (dynamic-wind
                   void
                   (lambda ()
                     (let loop ([complete? #f] [chunk-count 0])
                       (let ([message (websocket-recv conn)])
                         (unless (and (websocket-message? message)
                                      (eq? (websocket-message-type message) 'binary))
                           (errorf 'websocket-file-client
                                   "expected binary download FileChunk"))
                         (let ([next (write-chunk!
                                      (bytevector->file-chunk
                                       (websocket-message-data message)))])
                           (unless next
                             (loop (or next complete?) (+ chunk-count 1)))))))
                   (lambda () (close-writer!))))))
            (write-transfer-rss-growth-value!
             (max (- after-upload rss-before)
                  (- (current-peak-rss-kib) after-upload))))
           path*))
        (lambda ()
          (websocket-close conn))))))
