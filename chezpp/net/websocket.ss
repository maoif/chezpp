(library (chezpp net websocket)
  (export websocket-server?
          websocket-listen
          websocket-server-close
          websocket-accept
          websocket-accept/nonblocking
          websocket-connection?
          websocket-connect
          websocket-close
          websocket-send-text
          websocket-send-binary
          websocket-send-ping
          websocket-send-pong
          websocket-cancel-pending-send!
          websocket-send/nonblocking
          websocket-recv
          websocket-recv/nonblocking
          websocket-next-message
          websocket-message?
          websocket-message-type
          websocket-message-data
          call-with-websocket)
  (import (chezpp chez)
          (chezpp os)
          (chezpp utils)
          (chezpp net uri)
          (chezpp net errors)
          (chezpp net ffi)
          (chezpp net private))

  (define websocket-default-timeout-ms 30000)
  (define websocket-no-timeout -1)

  (define-record-type (websocket-server %make-websocket-server websocket-server?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle websocket-server-handle websocket-server-handle-set!)
            (immutable host websocket-server-host)
            (immutable port websocket-server-port)
            (immutable protocol websocket-server-protocol)
            (mutable closed? websocket-server-closed? websocket-server-closed?-set!)))

  (define-record-type (websocket-connection %make-websocket-connection websocket-connection?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle websocket-connection-handle websocket-connection-handle-set!)
            (immutable host websocket-connection-host)
            (immutable port websocket-connection-port)
            (immutable path websocket-connection-path)
            (immutable secure? websocket-connection-secure?)
            (immutable protocol websocket-connection-protocol)
            (mutable closed? websocket-connection-closed? websocket-connection-closed?-set!)))

  (define-record-type (websocket-message-record %make-websocket-message websocket-message?)
    (sealed #t)
    (opaque #f)
    (fields (immutable type websocket-message-type)
            (immutable data websocket-message-data)))

  (define ensure-success
    (lambda (who x)
      (cond
       [(ffi-error? x)
        (raise-net-error who 'websocket (ffi-error-message x) x)]
       [else x])))

  (define ensure-server-open
    (lambda (who server)
      (when (websocket-server-closed? server)
        (raise-net-error who 'websocket "websocket server is closed" server))))

  (define ensure-connection-open
    (lambda (who conn)
      (when (websocket-connection-closed? conn)
        (raise-net-error who 'websocket "websocket connection is closed" conn))))

  (define normalize-uri
    (lambda (who value)
      (let ([u (cond
                [(uri? value) value]
                [(string? value)
                 (or (string->uri value)
                     (errorf who "invalid websocket URI ~s" value))]
                [else
                 (errorf who "expected websocket URI object or string, given ~s" value)])])
        (unless (member (uri-scheme u) '("ws" "wss"))
          (errorf who "expected ws or wss URI, given ~s" (uri-scheme u)))
        (unless (uri-host u)
          (errorf who "websocket URI is missing a host"))
        u)))

  (define uri-default-port
    (lambda (u)
      (or (uri-port u)
          (if (string=? (uri-scheme u) "wss") 443 80))))

  (define uri-path*
    (lambda (u)
      (let ([path (uri-path u)]
            [query (uri-query u)])
        (string-append
         (if (or (not path) (string=? path "")) "/" path)
         (if query (string-append "?" query) "")))))

  (define websocket-type->int
    (lambda (who type)
      (case type
        [(text) 1]
        [(binary) 2]
        [(ping) 3]
        [(pong) 4]
        [else
         (errorf who "invalid websocket message type ~s" type)])))

  (define message-from-ffi
    (lambda (v)
      (let ([type (vector-ref v 0)]
            [data (vector-ref v 1)])
        (%make-websocket-message
         type
         (if (eq? type 'text)
             (utf8->string data)
             data)))))

  (define recv-result
    (lambda (who x)
      (cond
       [(ffi-would-block? x) #f]
       [(ffi-error? x) (ensure-success who x)]
       [(eof-object? x) x]
       [(vector? x) (message-from-ffi x)]
       [else (ensure-success who x)])))

  (define accept-result
    (lambda (who server x)
      (cond
       [(ffi-error? x) (ensure-success who x)]
       [(ffi-would-block? x) #f]
       [(eof-object? x) x]
       [else
        (%make-websocket-connection x
                                    (websocket-server-host server)
                                    (websocket-server-port server)
                                    "/"
                                    #f
                                    (websocket-server-protocol server)
                                    #f)])))

  (define send-result
    (lambda (who x)
      (cond
       [(fixnum? x) x]
       [(ffi-would-block? x) #f]
       [else (ensure-success who x)])))

  (define normalize-binary-payload
    (lambda (who payload)
      (unless (bytevector? payload)
        (errorf who "expected bytevector payload, given ~s" payload))
      payload))

  (define check-timeout-ms
    (lambda (who timeout-ms)
      (unless (fixnum? timeout-ms)
        (errorf who "timeout must be a fixnum, given ~s" timeout-ms))
      (when (fx< timeout-ms 0)
        (errorf who "timeout must be non-negative, given ~s" timeout-ms))
      timeout-ms))

  (define current-time-ms
    (lambda ()
      (let ([t (current-time)])
        (+ (* (time-second t) 1000)
           (quotient (time-nanosecond t) 1000000)))))

  (define timeout->deadline-ms
    (lambda (timeout-ms)
      (+ (current-time-ms) timeout-ms)))

  (define remaining-timeout-ms
    (lambda (deadline-ms)
      (max 0 (- deadline-ms (current-time-ms)))))

  (define wait-nonblocking
    (lambda (who timeout-ms timeout-message thunk)
      (let ([deadline-ms (timeout->deadline-ms timeout-ms)])
        (let loop ()
          (let ([ans (thunk)])
            (if ans
                ans
                (let ([remaining-ms (remaining-timeout-ms deadline-ms)])
                  (if (fx<= remaining-ms 0)
                      (raise-net-error who 'websocket timeout-message)
                      (begin
                        (milisleep (min 10 remaining-ms))
                        (loop))))))))))

  (define do-send
    (lambda (who conn type payload nonblocking? timeout-ms)
      (ensure-connection-open who conn)
      (cond
       [(eq? type 'text)
        (unless (string? payload)
          (errorf who "expected string payload for websocket text message"))
        (let ([bv (string->utf8 payload)])
          (send-result who
                       (ffi-net-websocket-send (websocket-connection-handle conn)
                                               (websocket-type->int who type)
                                               bv
                                               0
                                               (bytevector-length bv)
                                               (if nonblocking? 1 0)
                                               (if nonblocking? websocket-no-timeout timeout-ms))))]
       [else
        (let ([bv (normalize-binary-payload who payload)])
          (send-result who
                       (ffi-net-websocket-send (websocket-connection-handle conn)
                                               (websocket-type->int who type)
                                               bv
                                               0
                                               (bytevector-length bv)
                                               (if nonblocking? 1 0)
                                               (if nonblocking? websocket-no-timeout timeout-ms))))])))

  #|proc:websocket-listen
The `websocket-listen` procedure creates a WebSocket server listener.
|#
  (define-who websocket-listen
    (case-lambda
      [(host port) (websocket-listen host port "chezpp-websocket")]
      [(host port protocol)
       (pcheck ([string? host] [fixnum? port] [string? protocol])
               (check-port who port)
               (let ([ans (ffi-net-websocket-listen host port protocol)])
                 (if (ffi-error? ans)
                     (ensure-success who ans)
                     (%make-websocket-server ans host port protocol #f))))]))

  #|proc:websocket-server-close
The `websocket-server-close` procedure closes a WebSocket server listener.
|#
  (define-who websocket-server-close
    (lambda (server)
      (pcheck ([websocket-server? server])
              (unless (websocket-server-closed? server)
                (ensure-success who
                                (ffi-net-websocket-server-close (websocket-server-handle server)))
                (websocket-server-handle-set! server 0)
                (websocket-server-closed?-set! server #t))
              server)))

  #|proc:websocket-accept
The `websocket-accept` procedure accepts an incoming WebSocket connection.
|#
  (define-who websocket-accept
    (case-lambda
      [(server)
       (websocket-accept server websocket-default-timeout-ms)]
      [(server timeout-ms)
       (pcheck ([websocket-server? server])
               (check-timeout-ms who timeout-ms)
               (ensure-server-open who server)
               (accept-result who
                              server
                              (ffi-net-websocket-accept (websocket-server-handle server)
                                                        0
                                                        timeout-ms)))]))

  #|proc:websocket-accept/nonblocking
The `websocket-accept/nonblocking` procedure accepts an incoming WebSocket connection if one is ready, and returns `#f` otherwise.
|#
  (define-who websocket-accept/nonblocking
    (lambda (server)
      (pcheck ([websocket-server? server])
              (ensure-server-open who server)
              (accept-result who
                             server
                             (ffi-net-websocket-accept (websocket-server-handle server)
                                                       1
                                                       websocket-no-timeout)))))

  #|proc:websocket-connect
The `websocket-connect` procedure connects to a WebSocket endpoint described by a `ws:` or `wss:` URI.
|#
  (define-who websocket-connect
    (case-lambda
      [(value)
       (websocket-connect value "chezpp-websocket" websocket-default-timeout-ms)]
      [(value protocol-or-timeout)
       (cond
        [(string? protocol-or-timeout)
         (websocket-connect value protocol-or-timeout websocket-default-timeout-ms)]
        [(fixnum? protocol-or-timeout)
         (websocket-connect value "chezpp-websocket" protocol-or-timeout)]
        [else
         (errorf who "expected websocket protocol string or timeout fixnum, given ~s"
                 protocol-or-timeout)])]
      [(value protocol timeout-ms)
       (pcheck ([string? protocol])
               (check-timeout-ms who timeout-ms)
               (let* ([u (normalize-uri who value)]
                      [host (uri-host u)]
                      [port (uri-default-port u)]
                      [path (uri-path* u)]
                      [secure? (string=? (uri-scheme u) "wss")]
                      [ans (ffi-net-websocket-connect host
                                                      port
                                                      path
                                                      protocol
                                                      (if secure? 1 0)
                                                      timeout-ms)])
                 (if (ffi-error? ans)
                     (ensure-success who ans)
                     (%make-websocket-connection ans host port path secure? protocol #f))))]))

  #|proc:websocket-close
The `websocket-close` procedure closes a WebSocket connection.
|#
  (define-who websocket-close
    (lambda (conn)
      (pcheck ([websocket-connection? conn])
              (unless (websocket-connection-closed? conn)
                (ensure-success who
                                (ffi-net-websocket-close (websocket-connection-handle conn)))
                (websocket-connection-handle-set! conn 0)
                (websocket-connection-closed?-set! conn #t))
              conn)))

  #|proc:websocket-send-text
The `websocket-send-text` procedure sends a text message on a WebSocket connection.
|#
  (define-who websocket-send-text
    (case-lambda
      [(conn payload)
       (websocket-send-text conn payload websocket-default-timeout-ms)]
      [(conn payload timeout-ms)
       (pcheck ([websocket-connection? conn] [string? payload])
               (check-timeout-ms who timeout-ms)
               (do-send who conn 'text payload #f timeout-ms))]))

  #|proc:websocket-send-binary
The `websocket-send-binary` procedure sends a binary message on a WebSocket connection.
|#
  (define-who websocket-send-binary
    (case-lambda
      [(conn payload)
       (websocket-send-binary conn payload websocket-default-timeout-ms)]
      [(conn payload timeout-ms)
       (pcheck ([websocket-connection? conn] [bytevector? payload])
               (check-timeout-ms who timeout-ms)
               (do-send who conn 'binary payload #f timeout-ms))]))

  #|proc:websocket-send-ping
The `websocket-send-ping` procedure sends a ping frame on a WebSocket connection.
|#
  (define-who websocket-send-ping
    (case-lambda
      [(conn) (websocket-send-ping conn (make-bytevector 0 0))]
      [(conn payload)
       (pcheck ([websocket-connection? conn] [bytevector? payload])
               (websocket-send-ping conn payload websocket-default-timeout-ms))]
      [(conn payload timeout-ms)
       (pcheck ([websocket-connection? conn] [bytevector? payload])
               (check-timeout-ms who timeout-ms)
               (do-send who conn 'ping payload #f timeout-ms))]))

  #|proc:websocket-send-pong
The `websocket-send-pong` procedure sends a pong frame on a WebSocket connection.
|#
  (define-who websocket-send-pong
    (case-lambda
      [(conn) (websocket-send-pong conn (make-bytevector 0 0))]
      [(conn payload)
       (pcheck ([websocket-connection? conn] [bytevector? payload])
               (websocket-send-pong conn payload websocket-default-timeout-ms))]
      [(conn payload timeout-ms)
       (pcheck ([websocket-connection? conn] [bytevector? payload])
               (check-timeout-ms who timeout-ms)
               (do-send who conn 'pong payload #f timeout-ms))]))

  #|proc:websocket-cancel-pending-send!
The `websocket-cancel-pending-send!` procedure cancels and discards the currently pending non-blocking send on a WebSocket connection, if any.
|#
  (define-who websocket-cancel-pending-send!
    (lambda (conn)
      (pcheck ([websocket-connection? conn])
              (ensure-connection-open who conn)
              (ensure-success who
                              (ffi-net-websocket-cancel-send
                               (websocket-connection-handle conn)))
              conn)))

  #|proc:websocket-send/nonblocking
The `websocket-send/nonblocking` procedure attempts to send a WebSocket frame without blocking and returns `#f` if the send is still pending.
|#
  (define-who websocket-send/nonblocking
    (lambda (conn type payload)
      (pcheck ([websocket-connection? conn])
              (do-send who conn type payload #t websocket-no-timeout))))

  #|proc:websocket-recv
The `websocket-recv` procedure receives the next complete WebSocket message.
|#
  (define-who websocket-recv
    (case-lambda
      [(conn)
       (websocket-recv conn websocket-default-timeout-ms)]
      [(conn timeout-ms)
       (pcheck ([websocket-connection? conn] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (ensure-connection-open who conn)
               (recv-result who
                            (ffi-net-websocket-recv (websocket-connection-handle conn)
                                                    0
                                                    timeout-ms)))]))

  #|proc:websocket-recv/nonblocking
The `websocket-recv/nonblocking` procedure receives the next complete WebSocket message if one is ready, and returns `#f` otherwise.
|#
  (define-who websocket-recv/nonblocking
    (lambda (conn)
      (pcheck ([websocket-connection? conn])
              (ensure-connection-open who conn)
              (recv-result who
                           (ffi-net-websocket-recv (websocket-connection-handle conn)
                                                   1
                                                   websocket-no-timeout)))))

  #|proc:websocket-next-message
The `websocket-next-message` procedure is an alias of `websocket-recv`.
|#
  (define-who websocket-next-message
    (case-lambda
      [(conn)
       (websocket-recv conn)]
      [(conn timeout-ms)
       (pcheck ([websocket-connection? conn] [fixnum? timeout-ms])
               (websocket-recv conn timeout-ms))]))

  #|proc:call-with-websocket
The `call-with-websocket` procedure opens a WebSocket connection, passes it to `proc`, and closes it afterward.
|#
  (define-who call-with-websocket
    (case-lambda
      [(value proc)
       (call-with-websocket value "chezpp-websocket" websocket-default-timeout-ms proc)]
      [(value protocol-or-timeout proc)
       (pcheck ([procedure? proc])
               (cond
                [(string? protocol-or-timeout)
                 (call-with-websocket value
                                      protocol-or-timeout
                                      websocket-default-timeout-ms
                                      proc)]
                [(fixnum? protocol-or-timeout)
                 (call-with-websocket value
                                      "chezpp-websocket"
                                      protocol-or-timeout
                                      proc)]
                [else
                 (errorf who "expected websocket protocol string or timeout fixnum, given ~s"
                         protocol-or-timeout)]))]
      [(value protocol timeout-ms proc)
       (pcheck ([string? protocol] [procedure? proc])
               (check-timeout-ms who timeout-ms)
               (let ([conn (websocket-connect value protocol timeout-ms)])
                 (dynamic-wind
                   void
                   (lambda () (proc conn))
                   (lambda () (websocket-close conn)))))]))
  )
