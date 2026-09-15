#|proc:tcp-socket-file-server
The `tcp-socket-file-server` procedure accepts one localhost TCP client and
stores streamed upload frames in `dir` until the client sends the end marker.
|#
(define tcp-socket-file-server
  (lambda (dir)
    (pcheck ([string? dir])
      (ensure-upload-directory dir)
      (let ([listener (open-socket 'inet 'stream)])
        (socket-set-option! listener 'reuse-address #t)
        (socket-bind! listener
                      (make-socket-address 'inet
                                           file-transfer-host
                                           tcp-socket-file-transfer-port))
        (socket-listen! listener 4)
        (guard (c [else
                   (guard (x [else #f])
                     (close-socket listener))
                   (raise c)])
          (let-values ([(client peer) (socket-accept listener)])
            (guard (c [else
                       (guard (x [else #f])
                         (close-socket client))
                       (guard (x [else #f])
                         (close-socket listener))
                       (raise c)])
              (let ([result
                     (call-with-port
                      (open-socket-input-port client)
                      (lambda (ip)
                        (call-with-values
                         (lambda ()
                           (make-upload-frame-handler 'tcp-socket-file-server dir))
                         (lambda (handle-frame! close-handler!)
                           (dynamic-wind
                             void
                             (lambda ()
                               (let loop ([done? #f])
                                 (if done?
                                     dir
                                     (let* ([header (read-exactly ip 9)]
                                            [name-len (u32-ref header 1)]
                                            [payload-len (u32-ref header 5)]
                                            [rest (read-exactly ip (+ name-len payload-len))]
                                            [frame (make-bytevector (+ 9 name-len payload-len) 0)])
                                       (bytevector-copy! header 0 frame 0 9)
                                       (bytevector-copy! rest 0 frame 9 (+ name-len payload-len))
                                       (call-with-values
                                        (lambda ()
                                          (parse-stream-transfer-frame frame))
                                        (lambda (tag name payload)
                                          (loop (handle-frame! tag name payload))))))))
                             (lambda ()
                               (close-handler!)))))))])
                (close-socket client)
                (close-socket listener)
                result))))))))

#|proc:tcp-socket-file-client
The `tcp-socket-file-client` procedure uploads each file in `path*` to the
localhost TCP file server using streamed frames and then sends the end marker.
|#
(define tcp-socket-file-client
  (lambda (path*)
    (validate-file-list 'tcp-socket-file-client path*)
    (let ([sock (open-socket 'inet 'stream)])
      (dynamic-wind
        (lambda ()
          (socket-connect! sock
                           (make-socket-address 'inet
                                                file-transfer-host
                                                tcp-socket-file-transfer-port)))
        (lambda ()
          (call-with-port
           (open-socket-output-port sock)
           (lambda (op)
             (for-each
              (lambda (path)
                (send-path-via-transfer-frames
                 (lambda (tag name payload)
                   (put-bytevector op
                                   (make-stream-transfer-frame tag name payload)))
                 path))
              path*)
             (put-bytevector op
                             (make-stream-transfer-frame
                              transfer-frame-tag-done
                              ""
                              #vu8()))
             (flush-output-port op)
             path*)))
        (lambda ()
          (close-socket sock))))))
