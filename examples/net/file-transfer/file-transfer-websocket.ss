#|proc:websocket-file-server
The `websocket-file-server` procedure accepts one localhost WebSocket client and
stores binary file messages in `dir` until the end marker arrives.
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
         (lambda ()
           (make-upload-frame-handler 'websocket-file-server dir))
         (lambda (handle-frame! close-handler!)
           (dynamic-wind
             void
             (lambda ()
               ;; The current websocket server path is more reliable when the
               ;; client has already started the handshake before `accept`.
               (milisleep 1500)
               (set! conn (await-websocket-client server))
               (let loop ([done? #f])
                 (if done?
                     dir
                     (let ([msg (websocket-recv conn)])
                       (cond
                        [(eof-object? msg)
                         (errorf 'websocket-file-server
                                 "unexpected EOF before done frame")]
                        [(and (websocket-message? msg)
                              (eq? (websocket-message-type msg) 'binary))
                         (call-with-values
                          (lambda ()
                            (parse-stream-transfer-frame
                             (websocket-message-data msg)))
                          (lambda (tag name payload)
                            (loop (handle-frame! tag name payload))))]
                        [else
                         (errorf 'websocket-file-server
                                 "unexpected websocket message ~s"
                                 msg)])))))
             (lambda ()
               (close-handler!)
               (when conn
                 (websocket-close conn))
               (websocket-server-close server)))))))))

#|proc:websocket-file-client
The `websocket-file-client` procedure uploads each file in `path*` to the
localhost WebSocket server and then sends the end marker.
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
          (for-each
           (lambda (path)
             (send-path-via-transfer-frames
              (lambda (tag name payload)
                (websocket-send-binary
                 conn
                 (make-stream-transfer-frame tag name payload)))
              path))
           path*)
          (websocket-send-binary
           conn
           (make-stream-transfer-frame transfer-frame-tag-done "" #vu8()))
          path*)
        (lambda ()
          (websocket-close conn))))))
