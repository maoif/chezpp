(define grpc-upload-method file-transfer-upload-method)
(define grpc-download-method file-transfer-download-method)

(define grpc-transfer-request-count
  (lambda ()
    (let ([text (getenv "CHEZPP_TRANSFER_REQUESTS")])
      (if (and text (string->number text))
          (string->number text)
          1))))

#|proc:grpc-file-server
The `grpc-file-server` procedure serves generated streaming upload and download RPCs from `dir`.
Each upload enforces contiguous offsets and SHA-256; each download emits bounded FileChunk messages.
The request count defaults to one and may be set through `CHEZPP_TRANSFER_REQUESTS`.
|#
(define grpc-file-server
  (lambda (dir)
    (pcheck ([string? dir])
      (ensure-upload-directory dir)
      (with-grpc-env
       (lambda ()
         (let ([server (if grpc-file-transfer-server-credentials
                           (grpc-open-channel 'server
                                              grpc-file-transfer-server-credentials
                                              file-transfer-host
                                              grpc-file-transfer-port)
                           (grpc-open-channel 'server
                                              file-transfer-host
                                              grpc-file-transfer-port))])
           (grpc-register-service!
            server
            grpc-upload-method
            'client
            (lambda (stream)
              (call-with-values
               (lambda ()
                 (make-upload-file-chunk-handler 'grpc-file-server dir))
               (lambda (handle-chunk! close-handler!)
                 (dynamic-wind
                   void
                   (lambda ()
                     (let loop ([result #f])
                       (let ([payload (grpc-stream-recv stream)])
                         (if (eof-object? payload)
                             (if result
                                 (transfer-result-encode result)
                                 (errorf 'grpc-file-server
                                         "upload ended before a final FileChunk"))
                             (let ([next (handle-chunk!
                                          (bytevector->file-chunk payload))])
                               (when (and result next)
                                 (errorf 'grpc-file-server
                                         "upload contains more than one file"))
                               (loop (or next result)))))))
                   close-handler!)))))
           (grpc-register-service!
            server
            grpc-download-method
            'server
            (lambda (stream)
              (let ([payload (grpc-stream-recv stream)])
                (when (eof-object? payload)
                  (errorf 'grpc-file-server "download request is missing"))
                (let* ([request (bytevector->file-chunk payload)]
                       [name (file-chunk-name request)]
                       [path (validated-upload-path 'grpc-file-server dir name)])
                  (unless (file-regular? path)
                    (errorf 'grpc-file-server "download file does not exist: ~a" name))
                  (send-path-via-file-chunks
                   (lambda (chunk)
                     (grpc-stream-send stream (file-chunk-encode chunk)))
                   path
                   (file-chunk-offset request))))
              #f))
           (dynamic-wind
             void
             (lambda ()
               (let loop ([remaining (grpc-transfer-request-count)])
                 (unless (= remaining 0)
                   (grpc-serve server)
                   (loop (- remaining 1))))
               dir)
             (lambda ()
               (grpc-close-channel server)))))))))

(define open-grpc-file-channel
  (lambda ()
    (if grpc-file-transfer-client-credentials
        (grpc-open-channel grpc-file-transfer-client-credentials
                           file-transfer-host
                           grpc-file-transfer-port)
        (grpc-open-channel file-transfer-host grpc-file-transfer-port))))

(define grpc-upload-path!
  (lambda (client path)
    (let ([stream (grpc-call/client-stream client grpc-upload-method '() 300000)])
      (dynamic-wind
        void
        (lambda ()
          (let ([size
                 (send-path-via-file-chunks
                  (lambda (chunk)
                    (grpc-stream-send stream (file-chunk-encode chunk)))
                  path)])
            (grpc-stream-close-send stream)
            (let ([result (bytevector->transfer-result (grpc-stream-recv stream))])
              (unless (and (= size (transfer-result-size result))
                           (equal? (sha256-file path)
                                   (transfer-result-sha256 result)))
                (errorf 'grpc-file-client "upload result mismatch for ~a" path)))))
        (lambda ()
          (grpc-stream-close stream))))))

(define grpc-download-path!
  (lambda (client name destination)
    (let ([stream
           (grpc-call/server-stream
            client
            grpc-download-method
            (file-chunk-encode (make-file-chunk name 0 #vu8() #vu8() #f))
            '()
            300000)])
      (call-with-values
       (lambda ()
         (make-file-chunk-writer 'grpc-file-client destination 0))
       (lambda (write-chunk! close-writer! next-offset)
         (dynamic-wind
           void
           (lambda ()
             (let loop ([complete? #f] [chunk-count 0])
               (let ([payload (grpc-stream-recv stream)])
                 (if (eof-object? payload)
                     (unless complete?
                       (errorf 'grpc-file-client
                               "download ended before a final FileChunk"))
                     (let ([next (write-chunk! (bytevector->file-chunk payload))])
                       (loop (or next complete?) (+ chunk-count 1)))))))
           (lambda ()
             (close-writer!)
             (grpc-stream-close stream))))))))

#|proc:grpc-file-client
The `grpc-file-client` procedure uploads every file in `path*` with generated FileChunk messages.
When `CHEZPP_TRANSFER_DOWNLOAD` names a destination, it downloads the first uploaded file there and
verifies contiguous offsets and SHA-256. The return value is `path*`.
|#
(define grpc-file-client
  (lambda (path*)
    (validate-file-list 'grpc-file-client path*)
    (with-grpc-env
     (lambda ()
       (let ([client (open-grpc-file-channel)])
         (dynamic-wind
           void
           (lambda ()
             (let ([rss-before (current-peak-rss-kib)])
               (for-each (lambda (path) (grpc-upload-path! client path)) path*)
               (let ([after-upload (current-peak-rss-kib)]
                     [destination (getenv "CHEZPP_TRANSFER_DOWNLOAD")])
                 (when (and destination (not (string=? destination "")))
                   (grpc-download-path! client (path-basename (car path*)) destination))
                 (write-transfer-rss-growth-value!
                  (max (- after-upload rss-before)
                       (- (current-peak-rss-kib) after-upload))))
               path*))
           (lambda ()
             (grpc-close-channel client))))))))
