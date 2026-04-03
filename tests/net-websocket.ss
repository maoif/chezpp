(import (chezpp)
        (chezpp net))

(load "net-common.ss")

(define websocket-net-error-message?
  (lambda (message thunk)
    (guard (c [else
               (and (net-error? c)
                    (equal? (net-error-message c) message))])
      (thunk)
      #f)))

(define websocket-error-message-contains?
  (lambda (fragment thunk)
    (guard (c [else
               (and (condition? c)
                    (string-contains?
                     (call-with-string-output-port
                      (lambda (p) (display-condition c p)))
                     fragment))])
      (thunk)
      #f)))

(define start-stalled-websocket-handshake-server
  (lambda (delay-ms)
    (let ([listener (open-socket 'inet 'stream)])
      (socket-set-option! listener 'reuse-address #t)
      (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
      (socket-listen! listener 4)
      (let ([port (socket-address-port (socket-local-address listener))])
        (values listener
                port
                (fork-thread
                 (lambda ()
                   (let-values ([(client peer) (socket-accept listener)])
                     (milisleep delay-ms)
                     (close-socket client)
                     (close-socket listener)))))))))

(define start-websocket-send-with-pending
  (lambda (conn payload)
    (let ([first (websocket-send/nonblocking conn 'binary payload)])
      (if (not first)
          'pending-first
          (let ([second (websocket-send/nonblocking conn 'binary payload)])
            (if (not second)
                'sent-once-then-pending
                (error 'start-websocket-send-with-pending
                       "WebSocket nonblocking send did not become pending on the second send")))))))

(define await-websocket-message
  (lambda (conn)
    (let loop ([i 0])
      (let ([ans (websocket-recv/nonblocking conn)])
        (if ans
            ans
            (begin
              (when (> i 1000)
                (error 'await-websocket-message
                       "WebSocket message did not arrive"))
              (milisleep 10)
              (loop (+ i 1))))))))

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

(mat net-websocket-timeout
     (let* ([port (reserve-loopback-port)]
            [server (websocket-listen "127.0.0.1" port)])
       (dynamic-wind
         void
         (lambda ()
           (websocket-net-error-message?
            "websocket accept timed out"
            (lambda ()
              (websocket-accept server 50))))
         (lambda ()
           (websocket-server-close server))))
     (let-values ([(listener port th)
                   (start-stalled-websocket-handshake-server 150)])
       (dynamic-wind
         void
         (lambda ()
           (websocket-net-error-message?
            "websocket connect timed out"
            (lambda ()
              (websocket-connect (format "ws://127.0.0.1:~a/stall" port) 50))))
         (lambda ()
           (thread-join th)
           (guard (c [else #f])
             (close-socket listener)))))
     (let* ([port (reserve-loopback-port)]
            [server (websocket-listen "127.0.0.1" port)]
            [uri (format "ws://127.0.0.1:~a/idle" port)])
       (dynamic-wind
         void
         (lambda ()
           (let ([client (websocket-connect uri)]
                 [accepted #f])
             (dynamic-wind
               void
               (lambda ()
                 (set! accepted (websocket-accept server))
                 (and (websocket-net-error-message?
                       "websocket receive timed out"
                       (lambda ()
                         (websocket-recv client 50)))
                      (websocket-net-error-message?
                       "websocket receive timed out"
                       (lambda ()
                         (websocket-next-message accepted 50)))))
               (lambda ()
                 (when accepted
                   (websocket-close accepted))
                 (websocket-close client)))))
         (lambda ()
           (websocket-server-close server)))))

(mat net-websocket-timeout-validation
     (let* ([port (reserve-loopback-port)]
            [server (websocket-listen "127.0.0.1" port)]
            [uri (format "ws://127.0.0.1:~a/validate" port)])
       (dynamic-wind
         void
         (lambda ()
           (and
            (websocket-error-message-contains?
             "timeout must be non-negative"
             (lambda ()
               (websocket-accept server -1)))
            (websocket-error-message-contains?
             "timeout must be non-negative"
             (lambda ()
               (websocket-connect uri -1)))
            (websocket-error-message-contains?
             "timeout must be non-negative"
             (lambda ()
               (call-with-websocket uri -1 websocket-connection?)))))
         (lambda ()
           (websocket-server-close server))))
     (let* ([port (reserve-loopback-port)]
            [server (websocket-listen "127.0.0.1" port)]
            [uri (format "ws://127.0.0.1:~a/io" port)])
       (dynamic-wind
         void
         (lambda ()
           (let ([client (websocket-connect uri)]
                 [accepted #f])
             (dynamic-wind
               void
               (lambda ()
                 (set! accepted (websocket-accept server))
                 (and
                  (websocket-error-message-contains?
                   "timeout must be non-negative"
                   (lambda ()
                     (websocket-send-text client "x" -1)))
                  (websocket-error-message-contains?
                   "timeout must be non-negative"
                   (lambda ()
                     (websocket-send-binary accepted #vu8(1) -1)))
                  (websocket-error-message-contains?
                   "timeout must be non-negative"
                   (lambda ()
                     (websocket-send-ping client #vu8() -1)))
                  (websocket-error-message-contains?
                   "timeout must be non-negative"
                   (lambda ()
                     (websocket-send-pong accepted #vu8() -1)))
                  (websocket-error-message-contains?
                   "timeout must be non-negative"
                   (lambda ()
                     (websocket-recv client -1)))
                  (websocket-error-message-contains?
                   "timeout must be non-negative"
                   (lambda ()
                     (websocket-next-message accepted -1)))))
               (lambda ()
                 (when accepted
                   (websocket-close accepted))
                 (websocket-close client)))))
         (lambda ()
           (websocket-server-close server)))))

(mat net-websocket-cancel
     (let* ([port (reserve-loopback-port)]
            [server (websocket-listen "127.0.0.1" port)]
            [uri (format "ws://127.0.0.1:~a/cancel" port)]
            [payload (make-bytevector (* 1024 1024) 65)])
       (dynamic-wind
         void
         (lambda ()
           (let ([client (websocket-connect uri)]
                 [accepted #f])
             (dynamic-wind
               void
               (lambda ()
                 (set! accepted (websocket-accept server))
                 (let ([state (start-websocket-send-with-pending client payload)])
                   (and (begin
                        (websocket-cancel-pending-send! client)
                        #t)
                        (= (websocket-send-text client "after-cancel") 12)
                        (let ([first (await-websocket-message accepted)])
                          (if (eq? state 'pending-first)
                              (and (websocket-message? first)
                                   (eq? (websocket-message-type first) 'text)
                                   (equal? (websocket-message-data first) "after-cancel"))
                              (and (websocket-message? first)
                                   (eq? (websocket-message-type first) 'binary)
                                   (= (bytevector-length (websocket-message-data first))
                                      (bytevector-length payload))
                                   (let ([next (await-websocket-message accepted)])
                                     (and (websocket-message? next)
                                          (eq? (websocket-message-type next) 'text)
                                          (equal? (websocket-message-data next) "after-cancel")))))))))
               (lambda ()
                 (when accepted
                   (websocket-close accepted))
                 (websocket-close client)))))
         (lambda ()
           (websocket-server-close server)))))
