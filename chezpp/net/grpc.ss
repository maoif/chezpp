(library (chezpp net grpc)
  (export grpc-open-channel
          grpc-close-channel
          grpc-cancel-pending!
          grpc-channel?
          grpc-stream?
          grpc-stream-send
          grpc-stream-recv
          grpc-stream-close-send
          grpc-stream-close
          grpc-register-service!
          grpc-serve
          grpc-call
          grpc-call/nonblocking
          grpc-call/server-stream
          grpc-call/client-stream
          grpc-call/bidi-stream
          grpc-call/server-stream/nonblocking
          grpc-call/client-stream/nonblocking
          grpc-call/bidi-stream/nonblocking
          grpc-request
          grpc-request?
          grpc-request-method
          grpc-request-payload
          grpc-request-metadata
          grpc-response
          grpc-response?
          grpc-response-payload
          grpc-response-metadata
          grpc-status-code
          grpc-status-message
          grpc-metadata-ref)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp net errors)
          (chezpp net address)
          (chezpp net socket)
          (chezpp net poll)
          (chezpp net ffi)
          (chezpp net private))

  (define grpc-status-ok 0)
  (define grpc-status-internal 13)
  (define grpc-status-unimplemented 12)
  (define grpc-default-timeout-ms 30000)

  (define-record-type (grpc-request-record %make-grpc-request-record grpc-request-record?)
    (sealed #t)
    (opaque #f)
    (fields (immutable handle grpc-request-handle)
            (immutable method grpc-request-method)
            (immutable payload grpc-request-payload)
            (immutable metadata grpc-request-metadata)))

  (define-record-type (grpc-response-record %make-grpc-response-record grpc-response?)
    (sealed #t)
    (opaque #f)
    (fields (immutable payload grpc-response-payload)
            (immutable metadata grpc-response-metadata)
            (immutable status-code grpc-status-code)
            (immutable status-message grpc-status-message)))

  (define grpc-request? grpc-request-record?)

  (define-record-type (grpc-pending-op %make-grpc-pending-op grpc-pending-op?)
    (sealed #t)
    (opaque #f)
    (fields (immutable kind grpc-pending-op-kind)
            (immutable args grpc-pending-op-args)
            (mutable handle grpc-pending-op-handle grpc-pending-op-handle-set!)
            (immutable reader grpc-pending-op-reader)
            (immutable writer grpc-pending-op-writer)
            (immutable thread grpc-pending-op-thread)
            (mutable done? grpc-pending-op-done? grpc-pending-op-done?-set!)
            (mutable result grpc-pending-op-result grpc-pending-op-result-set!)
            (mutable cancelled? grpc-pending-op-cancelled? grpc-pending-op-cancelled?-set!)))

  (define-record-type (grpc-stream %make-grpc-stream grpc-stream?)
    (sealed #t)
    (opaque #f)
    (fields (immutable handle grpc-stream-handle)
            (immutable side grpc-stream-side)
            (immutable shape grpc-stream-shape)
            (mutable send-closed? grpc-stream-send-closed? grpc-stream-send-closed?-set!)
            (mutable recv-closed? grpc-stream-recv-closed? grpc-stream-recv-closed?-set!)
            (mutable closed? grpc-stream-closed? grpc-stream-closed?-set!)))

  (define-record-type (grpc-channel %make-grpc-channel grpc-channel?)
    (sealed #t)
    (opaque #f)
    (fields (immutable role grpc-channel-role)
            (immutable endpoint grpc-channel-endpoint)
            (mutable handle grpc-channel-handle grpc-channel-handle-set!)
            (immutable handlers grpc-channel-handlers)
            (mutable pending grpc-channel-pending grpc-channel-pending-set!)
            (mutable closed? grpc-channel-closed? grpc-channel-closed?-set!)))

  (define ensure-success
    (lambda (who x)
      (when (ffi-error? x)
        (raise-net-error who 'grpc (ffi-error-message x) x))
      x))

  (define ensure-channel-open
    (lambda (who channel)
      (when (grpc-channel-closed? channel)
        (raise-net-error who 'grpc "gRPC channel is closed" channel))))

  (define ensure-stream-open
    (lambda (who stream)
      (when (grpc-stream-closed? stream)
        (raise-net-error who 'grpc "gRPC stream is closed" stream))))

  (define ensure-role
    (lambda (who channel role)
      (unless (eq? (grpc-channel-role channel) role)
        (raise-net-error who 'grpc
                         (format "gRPC channel role mismatch, expected ~a" role)
                         channel))))

  (define make-handler-table
    (lambda ()
      (make-hashtable string-hash string=?)))

  (define make-unary-pending-op
    (lambda (args handle)
      (%make-grpc-pending-op 'unary args handle #f #f #f #f #f #f)))

  (define check-slice
    (lambda (who len start stop)
      (unless (and (fixnum? start) (fixnum? stop) (fx<= 0 start stop len))
        (errorf who "invalid slice [~a, ~a) for length ~a" start stop len))))

  (define check-timeout-ms
    (lambda (who timeout-ms)
      (unless (fixnum? timeout-ms)
        (errorf who "expected timeout fixnum, given ~s" timeout-ms))
      (when (fx< timeout-ms 0)
        (errorf who "timeout must be non-negative, given ~s" timeout-ms))
      timeout-ms))

  (define endpoint-string
    (case-lambda
      [(who endpoint)
       (unless (string? endpoint)
         (errorf who "expected endpoint string, given ~s" endpoint))
       endpoint]
      [(who host port)
       (unless (string? host)
         (errorf who "expected host string, given ~s" host))
       (unless (fixnum? port)
         (errorf who "expected port fixnum, given ~s" port))
       (check-port who port)
       (format "~a:~a" host port)]))

  (define normalize-payload
    (lambda (who payload)
      (cond
       [(bytevector? payload) payload]
       [(string? payload) (string->utf8 payload)]
       [(eq? payload #f) #f]
       [else
        (errorf who "expected bytevector, string, or #f payload, given ~s" payload)])))

  (define string-suffix?
    (lambda (suffix s)
      (let ([sn (string-length suffix)]
            [n (string-length s)])
        (and (fx<= sn n)
             (string=? suffix (substring s (fx- n sn) n))))))

  (define normalize-metadata
    (lambda (who metadata)
      (cond
       [(eq? metadata #f) '()]
       [(null? metadata) '()]
       [(list? metadata)
        (map (lambda (entry)
               (unless (pair? entry)
                 (errorf who "expected metadata pair, given ~s" entry))
               (let* ([raw-key (car entry)]
                      [raw-value (cdr entry)]
                      [key (cond
                            [(string? raw-key) raw-key]
                            [(symbol? raw-key) (symbol->string raw-key)]
                            [else (errorf who "expected string or symbol metadata key, given ~s"
                                          raw-key)])]
                      [value (cond
                              [(bytevector? raw-value) raw-value]
                              [(string? raw-value) (string->utf8 raw-value)]
                              [else (errorf who "expected string or bytevector metadata value, given ~s"
                                            raw-value)])])
                 (cons key value)))
             metadata)]
       [else
        (errorf who "expected metadata alist or #f, given ~s" metadata)])))

  (define metadata-value->scheme
    (lambda (key value)
      (cond
       [(not value) #f]
       [(string-suffix? "-bin" key) value]
       [else (utf8->string value)])))

  (define metadata-ref*
    (lambda (metadata key default)
      (let ([target (if (symbol? key) (symbol->string key) key)])
        (unless (string? target)
          (errorf 'grpc-metadata-ref "expected metadata key string or symbol, given ~s" key))
        (let loop ([rest metadata])
          (if (null? rest)
              default
              (let* ([entry (car rest)]
                     [entry-key (car entry)]
                     [entry-value (cdr entry)])
                (if (string-ci=? entry-key target)
                    (metadata-value->scheme entry-key entry-value)
                    (loop (cdr rest)))))))))

  (define maybe-response-from-ffi
    (lambda (who x)
      (cond
       [(eq? x #f) #f]
       [(and (vector? x) (= (vector-length x) 4))
        (%make-grpc-response-record
         (vector-ref x 0)
         (vector-ref x 1)
         (vector-ref x 2)
         (vector-ref x 3))]
       [else
        (errorf who "unexpected gRPC response payload ~s" x)])))

  (define request-from-ffi
    (lambda (who x)
      (unless (and (vector? x) (= (vector-length x) 4))
        (errorf who "unexpected gRPC request payload ~s" x))
      (%make-grpc-request-record
       (vector-ref x 0)
       (vector-ref x 1)
       (vector-ref x 2)
       (vector-ref x 3))))

  (define normalize-response
    (lambda (value)
      (cond
       [(grpc-response? value) value]
       [(or (bytevector? value) (string? value) (eq? value #f))
        (%make-grpc-response-record (normalize-payload 'grpc-response value) '() grpc-status-ok "")]
       [else
        (errorf 'grpc-serve "handler must return a gRPC response, string, bytevector, or #f: ~s" value)])))

  (define method-name
    (lambda (who method)
      (cond
       [(string? method) method]
       [(symbol? method) (symbol->string method)]
       [else (errorf who "expected gRPC method string or symbol, given ~s" method)])))

  (define normalize-stream-shape
    (lambda (who shape)
      (case shape
        [(unary server client bidi) shape]
        [else
         (errorf who "invalid gRPC stream shape ~s" shape)])))

  (define stream-shape->int
    (lambda (who shape)
      (case shape
        [(server) 1]
        [(client) 2]
        [(bidi) 3]
        [else
         (errorf who "invalid gRPC stream shape ~s" shape)])))

  (define stream-send-allowed?
    (lambda (stream)
      (or (eq? (grpc-stream-side stream) 'server)
          (memq (grpc-stream-shape stream) '(client bidi)))))

  (define stream-recv-allowed?
    (lambda (stream)
      (or (eq? (grpc-stream-side stream) 'server)
          (memq (grpc-stream-shape stream) '(server client bidi)))))

  (define normalize-stream-response
    (lambda (value)
      (cond
       [(grpc-response? value) value]
       [(or (bytevector? value) (string? value) (eq? value #f))
        (grpc-response value '() grpc-status-ok "")]
       [else
        (errorf 'grpc-serve
                "streaming handler must return a gRPC response, string, bytevector, or #f: ~s"
                value)])))

  (define make-client-stream
    (lambda (handle shape)
      (%make-grpc-stream handle 'client shape #f #f #f)))

  (define make-server-stream
    (lambda (handle shape)
      (%make-grpc-stream handle 'server shape #f #f #f)))

  (define close-stream-handle!
    (lambda (who stream)
      (unless (grpc-stream-closed? stream)
        (ensure-success who (ffi-net-grpc-stream-close (grpc-stream-handle stream)))
        (grpc-stream-closed?-set! stream #t))))

  (define finish-server-stream!
    (lambda (who stream response)
      (ensure-success who
                      (ffi-net-grpc-stream-finish
                       (grpc-stream-handle stream)
                       (grpc-response-payload response)
                       0
                       (if (grpc-response-payload response)
                           (bytevector-length (grpc-response-payload response))
                           0)
                       (grpc-status-code response)
                       (grpc-status-message response)
                       (grpc-response-metadata response)))
      (grpc-stream-send-closed?-set! stream #t)
      stream))

  (define open-stream
    (lambda (who channel method shape payload metadata timeout-ms)
      (ensure-channel-open who channel)
      (ensure-role who channel 'client)
      (let* ([method* (method-name who method)]
             [payload* (normalize-payload who payload)]
             [metadata* (normalize-metadata who metadata)]
             [handle (ensure-success who
                                     (ffi-net-grpc-stream-open
                                      (grpc-channel-handle channel)
                                      method*
                                      (stream-shape->int who shape)
                                      payload*
                                      0
                                      (if payload*
                                          (bytevector-length payload*)
                                          0)
                                      metadata*
                                      timeout-ms))])
        (make-client-stream handle shape))))

  (define stream-recv-result
    (lambda (who x)
      (cond
       [(or (bytevector? x) (eof-object? x)) x]
       [else (ensure-success who x)])))

  (define accept-stream-request
    (lambda (who channel)
      (let ([ans (ensure-success who
                                 (ffi-net-grpc-server-request-stream
                                  (grpc-channel-handle channel)))])
        (unless (and (vector? ans) (= (vector-length ans) 3))
          (errorf who "unexpected gRPC stream request payload ~s" ans))
        (values (vector-ref ans 0)
                (vector-ref ans 1)
                (vector-ref ans 2)))))

  (define close-pending-notifier!
    (lambda (pending)
      (let ([reader (grpc-pending-op-reader pending)]
            [writer (grpc-pending-op-writer pending)])
        (when reader
          (guard (c [else #f])
            (close-socket reader)))
        (when writer
          (guard (c [else #f])
            (close-socket writer))))))

  (define open-pending-notifier
    (lambda ()
      (let ([listener (open-socket 'inet 'stream)]
            [client #f]
            [server #f])
        (guard (c [else
                   (when server
                     (guard (x [else #f])
                       (close-socket server)))
                   (when client
                     (guard (x [else #f])
                       (close-socket client)))
                   (guard (x [else #f])
                     (close-socket listener))
                   (raise c)])
          (dynamic-wind
            void
            (lambda ()
              (socket-set-option! listener 'reuse-address #t)
              (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
              (socket-listen! listener 1)
              (let ([addr (socket-local-address listener)])
                (set! client (open-socket 'inet 'stream))
                (socket-connect! client addr)
                (let-values ([(accepted peer) (socket-accept listener)])
                  (set! server accepted)
                  (values server client))))
            (lambda ()
              (guard (c [else #f])
                (close-socket listener))))))))

  (define start-pending-stream!
    (lambda (channel kind args thunk)
      (let-values ([(reader writer) (open-pending-notifier)])
        (letrec ([pending
                  (%make-grpc-pending-op
                   kind
                   args
                   #f
                   reader
                   writer
                   (fork-thread
                    (lambda ()
                      (let ([result
                             (guard (c [else c])
                               (thunk))])
                        (when (and (grpc-stream? result)
                                   (grpc-pending-op-cancelled? pending))
                          (guard (c [else #f])
                            (grpc-stream-close result))
                          (set! result #f))
                        (grpc-pending-op-result-set! pending result))
                      (grpc-pending-op-done?-set! pending #t)
                      (guard (c [else #f])
                        (socket-send-all writer #vu8(1)))))
                   #f
                   #f
                   #f)])
          (grpc-channel-pending-set! channel pending)
          pending))))

  (define pending-ready?
    (lambda (pending)
      (or (grpc-pending-op-done? pending)
          (let ([reader (grpc-pending-op-reader pending)])
            (and reader
                 (let* ([target (make-poll-target reader '(read error hup invalid))]
                        [ready (car (poll/nonblocking (list target)))])
                   (pair? (poll-target-ready-events ready))))))))

  (define finish-stream-pending!
    (lambda (who channel pending)
      (grpc-channel-pending-set! channel #f)
      (thread-join (grpc-pending-op-thread pending))
      (close-pending-notifier! pending)
      (let ([result (grpc-pending-op-result pending)])
        (if (condition? result)
            (raise result)
            result))))

  (define cancel-pending!
    (lambda (who channel pending)
      (grpc-pending-op-cancelled?-set! pending #t)
      (grpc-channel-pending-set! channel #f)
      (clear-pending! who pending)
      (wait-pending-thread! pending)
      channel))

  (define wait-pending-thread!
    (lambda (pending)
      (let ([th (grpc-pending-op-thread pending)])
        (when th
          (thread-join th)))))

  (define clear-pending!
    (lambda (who pending)
      (case (grpc-pending-op-kind pending)
        [(unary)
         (let ([handle (grpc-pending-op-handle pending)])
           (when handle
             (ensure-success who (ffi-net-grpc-unary-close handle))
             (grpc-pending-op-handle-set! pending #f)))]
        [else
         (close-pending-notifier! pending)])))

  (define ensure-pending-matches
    (lambda (who pending kind args)
      (unless (and (eq? (grpc-pending-op-kind pending) kind)
                   (equal? (grpc-pending-op-args pending) args))
        (raise-net-error who 'grpc "another nonblocking gRPC operation is pending" pending))))

  #|proc:grpc-request
The `grpc-request` procedure constructs a gRPC request record.
|#
  (define-who grpc-request
    (case-lambda
      [(method payload)
       (grpc-request method payload '())]
      [(method payload metadata)
       (%make-grpc-request-record
        #f
        (method-name who method)
        (normalize-payload who payload)
        (normalize-metadata who metadata))]))

  #|proc:grpc-response
The `grpc-response` procedure constructs a gRPC response record.
|#
  (define-who grpc-response
    (case-lambda
      [(payload)
       (grpc-response payload '() grpc-status-ok "")]
      [(payload metadata)
       (grpc-response payload metadata grpc-status-ok "")]
      [(payload metadata status-code status-message)
       (pcheck ([fixnum? status-code] [string? status-message])
               (%make-grpc-response-record
                (normalize-payload who payload)
                (normalize-metadata who metadata)
                status-code
                status-message))]))

  #|proc:grpc-metadata-ref
The `grpc-metadata-ref` procedure looks up a metadata entry from a gRPC metadata alist or gRPC request/response record.
|#
  (define-who grpc-metadata-ref
    (case-lambda
      [(x key)
       (grpc-metadata-ref x key #f)]
      [(x key default)
       (let ([metadata (cond
                        [(grpc-request? x) (grpc-request-metadata x)]
                        [(grpc-response? x) (grpc-response-metadata x)]
                        [else x])])
         (unless (list? metadata)
           (errorf who "expected gRPC metadata list or request/response, given ~s" x))
         (metadata-ref* metadata key default))]))

  #|proc:grpc-open-channel
The `grpc-open-channel` procedure opens a client gRPC channel or a server listener.
Use `(grpc-open-channel endpoint)`, `(grpc-open-channel host port)`, or `(grpc-open-channel 'server host port)`.
|#
  (define-who grpc-open-channel
    (case-lambda
      [(endpoint)
       (let ([ep (endpoint-string who endpoint)])
         (%make-grpc-channel
          'client
          ep
          (ensure-success who (ffi-net-grpc-channel-open ep))
          (make-handler-table)
          #f
          #f))]
      [(host port)
       (let ([ep (endpoint-string who host port)])
         (%make-grpc-channel
          'client
          ep
          (ensure-success who (ffi-net-grpc-channel-open ep))
          (make-handler-table)
          #f
          #f))]
      [(role host port)
       (pcheck ([symbol? role] [string? host] [fixnum? port])
               (check-port who port)
               (case role
                 [(server)
                  (let ([ans (ensure-success who (ffi-net-grpc-server-open host port))])
                    (unless (and (vector? ans) (= (vector-length ans) 2))
                      (errorf who "unexpected gRPC server open result ~s" ans))
                    (%make-grpc-channel
                     'server
                     (format "~a:~a" host (vector-ref ans 1))
                     (vector-ref ans 0)
                     (make-handler-table)
                     #f
                     #f))]
                 [else
                  (errorf who "invalid gRPC role ~s" role)]))]))

  #|proc:grpc-close-channel
The `grpc-close-channel` procedure closes a gRPC client channel or server listener.
|#
  (define-who grpc-close-channel
    (lambda (channel)
      (pcheck ([grpc-channel? channel])
              (unless (grpc-channel-closed? channel)
                (let ([pending (grpc-channel-pending channel)])
                  (when pending
                    (cancel-pending! who channel pending)
                    (wait-pending-thread! pending)))
                (let ([handle (grpc-channel-handle channel)])
                  (when handle
                    (ensure-success who
                                    (if (eq? (grpc-channel-role channel) 'server)
                                        (ffi-net-grpc-server-close handle)
                                        (ffi-net-grpc-channel-close handle)))
                    (grpc-channel-handle-set! channel 0)))
                (grpc-channel-closed?-set! channel #t))
              channel)))

  #|proc:grpc-cancel-pending!
The `grpc-cancel-pending!` procedure cancels and discards the currently pending non-blocking gRPC operation on a channel, if any.
|#
  (define-who grpc-cancel-pending!
    (lambda (channel)
      (pcheck ([grpc-channel? channel])
              (let ([pending (grpc-channel-pending channel)])
                (when pending
                  (cancel-pending! who channel pending)))
              channel)))

  #|proc:grpc-register-service!
The `grpc-register-service!` procedure registers a gRPC handler on a server channel.
|#
  (define-who grpc-register-service!
    (case-lambda
      [(channel method proc)
       (grpc-register-service! channel method 'unary proc)]
      [(channel method shape proc)
       (pcheck ([grpc-channel? channel] [procedure? proc])
               (ensure-channel-open who channel)
               (ensure-role who channel 'server)
               (hashtable-set! (grpc-channel-handlers channel)
                               (method-name who method)
                               (vector (normalize-stream-shape who shape) proc))
               channel)]))

  (define call-unary
    (lambda (who channel method payload metadata timeout-ms)
      (ensure-channel-open who channel)
      (ensure-role who channel 'client)
      (let* ([method* (method-name who method)]
             [payload* (normalize-payload who payload)]
             [metadata* (normalize-metadata who metadata)]
             [ans (ensure-success who
                                  (ffi-net-grpc-unary-call (grpc-channel-handle channel)
                                                           method*
                                                           payload*
                                                           0
                                                           (if payload*
                                                               (bytevector-length payload*)
                                                               0)
                                                           metadata*
                                                           timeout-ms))])
        (maybe-response-from-ffi who ans))))

  #|proc:grpc-call
The `grpc-call` procedure performs a blocking unary gRPC call and returns a gRPC response record.
|#
  (define-who grpc-call
    (case-lambda
      [(channel method payload)
       (grpc-call channel method payload '() grpc-default-timeout-ms)]
      [(channel method payload metadata)
       (grpc-call channel method payload metadata grpc-default-timeout-ms)]
      [(channel method payload metadata timeout-ms)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (call-unary who channel method payload metadata timeout-ms))]))

  (define start-pending-unary!
    (lambda (who channel method payload metadata timeout-ms)
      (let* ([method* (method-name who method)]
             [payload* (normalize-payload who payload)]
             [metadata* (normalize-metadata who metadata)]
             [args (list method* payload* metadata* timeout-ms)]
             [ans (ensure-success who
                                  (ffi-net-grpc-unary-start (grpc-channel-handle channel)
                                                            method*
                                                            payload*
                                                            0
                                                            (if payload*
                                                                (bytevector-length payload*)
                                                                0)
                                                            metadata*
                                                            timeout-ms))])
        (grpc-channel-pending-set! channel (make-unary-pending-op args ans)))))

  #|proc:grpc-call/nonblocking
The `grpc-call/nonblocking` procedure progresses a unary gRPC call without blocking and returns `#f` while the response is pending.
|#
  (define-who grpc-call/nonblocking
    (case-lambda
      [(channel method payload)
       (grpc-call/nonblocking channel method payload '() grpc-default-timeout-ms)]
      [(channel method payload metadata)
       (grpc-call/nonblocking channel method payload metadata grpc-default-timeout-ms)]
      [(channel method payload metadata timeout-ms)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (ensure-role who channel 'client)
               (let* ([method* (method-name who method)]
                      [payload* (normalize-payload who payload)]
                      [metadata* (normalize-metadata who metadata)]
                      [args (list method* payload* metadata* timeout-ms)])
                 (unless (grpc-channel-pending channel)
                   (start-pending-unary! who channel method payload metadata timeout-ms))
                 (ensure-pending-matches who (grpc-channel-pending channel) 'unary args))
               (let* ([pending (grpc-channel-pending channel)]
                      [ans (ensure-success who (ffi-net-grpc-unary-poll (grpc-pending-op-handle pending)))]
                      [response (maybe-response-from-ffi who ans)])
                 (when response
                   (grpc-channel-pending-set! channel #f))
                 response))]))

  (define open-stream/nonblocking
    (lambda (who channel kind method shape payload metadata timeout-ms)
      (ensure-channel-open who channel)
      (ensure-role who channel 'client)
      (let* ([method* (method-name who method)]
             [payload* (normalize-payload who payload)]
             [metadata* (normalize-metadata who metadata)]
             [args (list kind method* payload* metadata* timeout-ms)]
             [pending (grpc-channel-pending channel)])
        (when pending
          (ensure-pending-matches who pending kind args))
        (let ([pending (or pending
                           (start-pending-stream!
                            channel
                            kind
                            args
                            (lambda ()
                              (open-stream who
                                           channel
                                           method*
                                           shape
                                           payload*
                                           metadata*
                                           timeout-ms))))])
          (if (pending-ready? pending)
              (finish-stream-pending! who channel pending)
              #f)))))

  (define respond-to-request
    (lambda (who request response)
      (let ([payload (grpc-response-payload response)])
        (ensure-success
         who
         (ffi-net-grpc-server-respond (grpc-request-handle request)
                                      payload
                                      0
                                      (if payload (bytevector-length payload) 0)
                                      (grpc-status-code response)
                                      (grpc-status-message response)
                                      (grpc-response-metadata response))))))

  #|proc:grpc-serve
The `grpc-serve` procedure accepts and processes one gRPC request on a server channel.
|#
  (define-who grpc-serve
    (lambda (channel)
      (pcheck ([grpc-channel? channel])
              (ensure-channel-open who channel)
              (ensure-role who channel 'server)
              (let-values ([(handle method metadata)
                            (accept-stream-request who channel)])
                (let* ([entry (hashtable-ref (grpc-channel-handlers channel) method #f)]
                       [shape (and entry (vector-ref entry 0))]
                       [proc (and entry (vector-ref entry 1))]
                       [stream (make-server-stream handle (or shape 'bidi))]
                       [request (%make-grpc-request-record handle method #f metadata)])
                  (dynamic-wind
                    void
                    (lambda ()
                      (cond
                       [(not entry)
                        (finish-server-stream!
                         who
                         stream
                         (grpc-response #f '() grpc-status-unimplemented "unimplemented"))
                        request]
                       [(eq? shape 'unary)
                        (let* ([payload (stream-recv-result who
                                                           (ffi-net-grpc-stream-recv
                                                            (grpc-stream-handle stream)))]
                               [request* (%make-grpc-request-record handle method payload metadata)]
                               [response
                                (guard (c [else
                                           (grpc-response #f
                                                          '()
                                                          grpc-status-internal
                                                          (if (condition? c)
                                                              (format "~a" c)
                                                              (format "~s" c)))])
                                  (normalize-response (proc request*)))])
                          (when (eof-object? payload)
                            (raise-net-error who 'grpc "unexpected EOF in unary gRPC request" request*))
                          (grpc-stream-recv-closed?-set! stream #t)
                          (finish-server-stream! who stream response)
                          request*)]
                       [else
                        (let ([result
                               (guard (c [else
                                          (finish-server-stream!
                                           who
                                           stream
                                           (grpc-response #f
                                                          '()
                                                          grpc-status-internal
                                                          (if (condition? c)
                                                              (format "~a" c)
                                                              (format "~s" c))))
                                          #f])
                                 (proc stream))])
                          (unless (or (not result) (grpc-stream-send-closed? stream))
                            (finish-server-stream!
                             who
                             stream
                             (normalize-stream-response result)))
                          (unless (grpc-stream-send-closed? stream)
                            (finish-server-stream! who stream (grpc-response #f '() grpc-status-ok "")))
                          request)]))
                    (lambda ()
                      (guard (c [else #f])
                        (grpc-stream-close stream)))))))))

  #|proc:grpc-stream-send
The `grpc-stream-send` procedure sends one message on a gRPC streaming call.
|#
  (define-who grpc-stream-send
    (lambda (stream payload)
      (pcheck ([grpc-stream? stream])
              (ensure-stream-open who stream)
              (unless (stream-send-allowed? stream)
                (raise-net-error who 'grpc "gRPC stream does not support sending" stream))
              (when (grpc-stream-send-closed? stream)
                (raise-net-error who 'grpc "gRPC stream send side is closed" stream))
              (let ([payload* (normalize-payload who payload)])
                (ensure-success who
                                (ffi-net-grpc-stream-send
                                 (grpc-stream-handle stream)
                                 payload*
                                 0
                                 (if payload*
                                     (bytevector-length payload*)
                                     0)))
                stream))))

  #|proc:grpc-stream-recv
The `grpc-stream-recv` procedure receives one message from a gRPC streaming call and returns an EOF object when the peer finishes sending.
|#
  (define-who grpc-stream-recv
    (lambda (stream)
      (pcheck ([grpc-stream? stream])
              (ensure-stream-open who stream)
              (unless (stream-recv-allowed? stream)
                (raise-net-error who 'grpc "gRPC stream does not support receiving" stream))
              (let ([ans (stream-recv-result who
                                            (ffi-net-grpc-stream-recv
                                             (grpc-stream-handle stream)))])
                (when (eof-object? ans)
                  (grpc-stream-recv-closed?-set! stream #t))
                ans))))

  #|proc:grpc-stream-close-send
The `grpc-stream-close-send` procedure closes the local send side of a gRPC streaming call.
|#
  (define-who grpc-stream-close-send
    (lambda (stream)
      (pcheck ([grpc-stream? stream])
              (ensure-stream-open who stream)
              (unless (stream-send-allowed? stream)
                (raise-net-error who 'grpc "gRPC stream does not support sending" stream))
              (unless (grpc-stream-send-closed? stream)
                (ensure-success who
                                (ffi-net-grpc-stream-close-send
                                 (grpc-stream-handle stream)))
                (grpc-stream-send-closed?-set! stream #t))
              stream)))

  #|proc:grpc-stream-close
The `grpc-stream-close` procedure closes a gRPC streaming call and releases its resources.
|#
  (define-who grpc-stream-close
    (lambda (stream)
      (pcheck ([grpc-stream? stream])
              (unless (grpc-stream-closed? stream)
                (guard (c [else #f])
                  (when (and (eq? (grpc-stream-side stream) 'server)
                             (not (grpc-stream-send-closed? stream)))
                    (grpc-stream-close-send stream)))
                (close-stream-handle! who stream)
                (grpc-stream-send-closed?-set! stream #t)
                (grpc-stream-recv-closed?-set! stream #t))
              stream)))

  #|proc:grpc-call/server-stream
The `grpc-call/server-stream` procedure opens a blocking server-streaming gRPC call and returns a gRPC stream object.
|#
  (define-who grpc-call/server-stream
    (case-lambda
      [(channel method payload)
       (grpc-call/server-stream channel method payload '() grpc-default-timeout-ms)]
      [(channel method payload metadata-or-timeout)
       (if (fixnum? metadata-or-timeout)
           (grpc-call/server-stream channel method payload '() metadata-or-timeout)
           (grpc-call/server-stream channel method payload metadata-or-timeout grpc-default-timeout-ms))]
      [(channel method payload metadata timeout-ms)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (open-stream who channel method 'server payload metadata timeout-ms))]))

  #|proc:grpc-call/client-stream
The `grpc-call/client-stream` procedure opens a blocking client-streaming gRPC call and returns a gRPC stream object.
|#
  (define-who grpc-call/client-stream
    (case-lambda
      [(channel method)
       (grpc-call/client-stream channel method '() grpc-default-timeout-ms)]
      [(channel method metadata-or-timeout)
       (if (fixnum? metadata-or-timeout)
           (grpc-call/client-stream channel method '() metadata-or-timeout)
           (grpc-call/client-stream channel method metadata-or-timeout grpc-default-timeout-ms))]
      [(channel method metadata timeout-ms)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (open-stream who channel method 'client #f metadata timeout-ms))]))

  #|proc:grpc-call/bidi-stream
The `grpc-call/bidi-stream` procedure opens a blocking bidirectional gRPC streaming call and returns a gRPC stream object.
|#
  (define-who grpc-call/bidi-stream
    (case-lambda
      [(channel method)
       (grpc-call/bidi-stream channel method '() grpc-default-timeout-ms)]
      [(channel method metadata-or-timeout)
       (if (fixnum? metadata-or-timeout)
           (grpc-call/bidi-stream channel method '() metadata-or-timeout)
           (grpc-call/bidi-stream channel method metadata-or-timeout grpc-default-timeout-ms))]
      [(channel method metadata timeout-ms)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (open-stream who channel method 'bidi #f metadata timeout-ms))]))

  #|proc:grpc-call/server-stream/nonblocking
The `grpc-call/server-stream/nonblocking` procedure progresses opening a server-streaming gRPC call without blocking and returns `#f` until the stream is ready.
|#
  (define-who grpc-call/server-stream/nonblocking
    (case-lambda
      [(channel method payload)
       (grpc-call/server-stream/nonblocking channel method payload '() grpc-default-timeout-ms)]
      [(channel method payload metadata-or-timeout)
       (if (fixnum? metadata-or-timeout)
           (grpc-call/server-stream/nonblocking channel method payload '() metadata-or-timeout)
           (grpc-call/server-stream/nonblocking channel method payload metadata-or-timeout grpc-default-timeout-ms))]
      [(channel method payload metadata timeout-ms)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (open-stream/nonblocking
                who
                channel
                'server-stream
                method
                'server
                payload
                metadata
                timeout-ms))]))

  #|proc:grpc-call/client-stream/nonblocking
The `grpc-call/client-stream/nonblocking` procedure progresses opening a client-streaming gRPC call without blocking and returns `#f` until the stream is ready.
|#
  (define-who grpc-call/client-stream/nonblocking
    (case-lambda
      [(channel method)
       (grpc-call/client-stream/nonblocking channel method '() grpc-default-timeout-ms)]
      [(channel method metadata-or-timeout)
       (if (fixnum? metadata-or-timeout)
           (grpc-call/client-stream/nonblocking channel method '() metadata-or-timeout)
           (grpc-call/client-stream/nonblocking channel method metadata-or-timeout grpc-default-timeout-ms))]
      [(channel method metadata timeout-ms)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (open-stream/nonblocking
                who
                channel
                'client-stream
                method
                'client
                #f
                metadata
                timeout-ms))]))

  #|proc:grpc-call/bidi-stream/nonblocking
The `grpc-call/bidi-stream/nonblocking` procedure progresses opening a bidirectional gRPC stream without blocking and returns `#f` until the stream is ready.
|#
  (define-who grpc-call/bidi-stream/nonblocking
    (case-lambda
      [(channel method)
       (grpc-call/bidi-stream/nonblocking channel method '() grpc-default-timeout-ms)]
      [(channel method metadata-or-timeout)
       (if (fixnum? metadata-or-timeout)
           (grpc-call/bidi-stream/nonblocking channel method '() metadata-or-timeout)
           (grpc-call/bidi-stream/nonblocking channel method metadata-or-timeout grpc-default-timeout-ms))]
      [(channel method metadata timeout-ms)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (open-stream/nonblocking
                who
                channel
                'bidi-stream
                method
                'bidi
                #f
                metadata
                timeout-ms))])))
