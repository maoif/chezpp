(define grpc-upload-method "/chezpp.examples.FileTransfer/Upload")

#|proc:grpc-file-server
The `grpc-file-server` procedure listens on localhost, accepts one client-stream
gRPC upload session, stores the streamed files in `dir`, and returns after the
done frame is received.
|#
(define grpc-file-server
  (lambda (dir)
    (pcheck ([string? dir])
      (ensure-upload-directory dir)
      (with-grpc-env
        (lambda ()
          (let ([server (grpc-open-channel 'server
                                           file-transfer-host
                                           grpc-file-transfer-port)])
            (grpc-register-service!
             server
             grpc-upload-method
             'client
             (lambda (stream)
               (call-with-values
                (lambda ()
                  (make-upload-frame-handler 'grpc-file-server dir))
                (lambda (handle-frame! close-handler!)
                  (dynamic-wind
                    void
                    (lambda ()
                      (let loop ()
                        (let ([payload (grpc-stream-recv stream)])
                          (if (eof-object? payload)
                              (errorf 'grpc-file-server
                                      "unexpected EOF before done frame")
                              (call-with-values
                               (lambda ()
                                 (parse-stream-transfer-frame payload))
                               (lambda (tag name chunk)
                                 (if (handle-frame! tag name chunk)
                                     "done"
                                     (loop))))))))
                    (lambda ()
                      (close-handler!)))))))
            (dynamic-wind
              void
              (lambda ()
                (grpc-serve server)
                dir)
              (lambda ()
                (grpc-close-channel server)))))))))

#|proc:grpc-file-client
The `grpc-file-client` procedure uploads each file in `path*` to the localhost
gRPC file server and then calls the `Done` method.
|#
(define grpc-file-client
  (lambda (path*)
    (validate-file-list 'grpc-file-client path*)
    (with-grpc-env
     (lambda ()
       (let ([client (grpc-open-channel file-transfer-host grpc-file-transfer-port)])
         (dynamic-wind
           void
           (lambda ()
             (let ([stream (grpc-call/client-stream client grpc-upload-method '() 300000)])
               (dynamic-wind
                 void
                 (lambda ()
                   (for-each
                    (lambda (path)
                      (send-path-via-transfer-frames
                       (lambda (tag name payload)
                         (grpc-stream-send
                          stream
                          (make-stream-transfer-frame tag name payload)))
                       path))
                    path*)
                   (grpc-stream-send
                    stream
                    (make-stream-transfer-frame transfer-frame-tag-done "" #vu8()))
                   (grpc-stream-close-send stream)
                   (grpc-stream-recv stream)
                   path*)
                 (lambda ()
                   (grpc-stream-close stream)))))
           (lambda ()
             (grpc-close-channel client))))))))
