(import (chezpp)
        (chezpp net))

(load "net-common.ss")

(mat net-websocket
     (let* ([port (reserve-loopback-port)]
            [server (websocket-listen "127.0.0.1" port)]
            [uri1 (format "ws://127.0.0.1:~a/chat?room=one" port)]
            [uri2 (format "ws://127.0.0.1:~a/call-with" port)])
       (dynamic-wind
         void
         (lambda ()
           (and
            (websocket-server? server)
            (not (websocket-accept/nonblocking server))
            (let ([client (websocket-connect uri1)]
                  [accepted #f])
              (dynamic-wind
                void
                (lambda ()
                  (set! accepted (or (websocket-accept/nonblocking server)
                                     (websocket-accept server)))
                  (and
                   (websocket-connection? client)
                   (websocket-connection? accepted)
                   (not (websocket-recv/nonblocking client))
                   (not (websocket-recv/nonblocking accepted))
                   (= (websocket-send-text client "hello websocket") 15)
                   (let ([msg (websocket-next-message accepted)])
                     (and (websocket-message? msg)
                          (eq? (websocket-message-type msg) 'text)
                          (equal? (websocket-message-data msg) "hello websocket")))
                   (let ([ans (websocket-send/nonblocking accepted 'binary #vu8(1 2 3 4))])
                     (or (not ans) (= ans 4)))
                   (let ([msg (websocket-recv client)])
                     (and (websocket-message? msg)
                          (eq? (websocket-message-type msg) 'binary)
                          (equal? (websocket-message-data msg) #vu8(1 2 3 4))))))
                (lambda ()
                  (when accepted
                    (websocket-close accepted))
                  (websocket-close client))))
            (call-with-websocket
             uri2
             (lambda (client)
               (let ([accepted #f])
                 (dynamic-wind
                   void
                   (lambda ()
                     (set! accepted (websocket-accept server))
                     (and
                      (websocket-connection? client)
                      (websocket-connection? accepted)
                      (let ([ans (websocket-send/nonblocking client 'text "via-call")])
                        (or (not ans) (= ans 8)))
                      (let ([msg (websocket-recv accepted)])
                        (and (websocket-message? msg)
                             (eq? (websocket-message-type msg) 'text)
                             (equal? (websocket-message-data msg) "via-call")))
                      (= (websocket-send-binary accepted #vu8(9 8 7)) 3)
                      (let ([msg (websocket-recv client)])
                        (and (websocket-message? msg)
                             (eq? (websocket-message-type msg) 'binary)
                             (equal? (websocket-message-data msg) #vu8(9 8 7))))))
                   (lambda ()
                     (when accepted
                       (websocket-close accepted)))))))))
         (lambda ()
           (websocket-server-close server)))))
