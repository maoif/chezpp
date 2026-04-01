(library (chezpp net grpc)
  (export grpc-open-channel
          grpc-close-channel
          grpc-channel?
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
          (chezpp net ffi)
          (chezpp net private))

  (define grpc-status-ok 0)
  (define grpc-status-internal 13)
  (define grpc-status-unimplemented 12)

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
    (fields (mutable handle grpc-pending-op-handle grpc-pending-op-handle-set!)))

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

  (define ensure-role
    (lambda (who channel role)
      (unless (eq? (grpc-channel-role channel) role)
        (raise-net-error who 'grpc
                         (format "gRPC channel role mismatch, expected ~a" role)
                         channel))))

  (define make-handler-table
    (lambda ()
      (make-hashtable string-hash string=?)))

  (define check-slice
    (lambda (who len start stop)
      (unless (and (fixnum? start) (fixnum? stop) (fx<= 0 start stop len))
        (errorf who "invalid slice [~a, ~a) for length ~a" start stop len))))

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
                    (ensure-success who (ffi-net-grpc-unary-close (grpc-pending-op-handle pending)))
                    (grpc-channel-pending-set! channel #f)))
                (let ([handle (grpc-channel-handle channel)])
                  (when handle
                    (ensure-success who
                                    (if (eq? (grpc-channel-role channel) 'server)
                                        (ffi-net-grpc-server-close handle)
                                        (ffi-net-grpc-channel-close handle)))
                    (grpc-channel-handle-set! channel 0)))
                (grpc-channel-closed?-set! channel #t))
              channel)))

  #|proc:grpc-register-service!
The `grpc-register-service!` procedure registers a unary gRPC handler on a server channel.
|#
  (define-who grpc-register-service!
    (lambda (channel method proc)
      (pcheck ([grpc-channel? channel] [procedure? proc])
              (ensure-channel-open who channel)
              (ensure-role who channel 'server)
              (hashtable-set! (grpc-channel-handlers channel) (method-name who method) proc)
              channel)))

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
       (grpc-call channel method payload '() 30000)]
      [(channel method payload metadata)
       (grpc-call channel method payload metadata 30000)]
      [(channel method payload metadata timeout-ms)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (call-unary who channel method payload metadata timeout-ms))]))

  (define start-pending-unary!
    (lambda (who channel method payload metadata timeout-ms)
      (let* ([method* (method-name who method)]
             [payload* (normalize-payload who payload)]
             [metadata* (normalize-metadata who metadata)]
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
        (grpc-channel-pending-set! channel (%make-grpc-pending-op ans)))))

  #|proc:grpc-call/nonblocking
The `grpc-call/nonblocking` procedure progresses a unary gRPC call without blocking and returns `#f` while the response is pending.
|#
  (define-who grpc-call/nonblocking
    (case-lambda
      [(channel method payload)
       (grpc-call/nonblocking channel method payload '() 30000)]
      [(channel method payload metadata)
       (grpc-call/nonblocking channel method payload metadata 30000)]
      [(channel method payload metadata timeout-ms)
       (pcheck ([grpc-channel? channel] [fixnum? timeout-ms])
               (ensure-channel-open who channel)
               (ensure-role who channel 'client)
               (unless (grpc-channel-pending channel)
                 (start-pending-unary! who channel method payload metadata timeout-ms))
               (let* ([pending (grpc-channel-pending channel)]
                      [ans (ensure-success who (ffi-net-grpc-unary-poll (grpc-pending-op-handle pending)))]
                      [response (maybe-response-from-ffi who ans)])
                 (when response
                   (grpc-channel-pending-set! channel #f))
                 response))]))

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
The `grpc-serve` procedure accepts and processes one unary gRPC request on a server channel.
|#
  (define-who grpc-serve
    (lambda (channel)
      (pcheck ([grpc-channel? channel])
              (ensure-channel-open who channel)
              (ensure-role who channel 'server)
              (let* ((request (request-from-ffi who
                                                (ensure-success who
                                                                (ffi-net-grpc-server-request
                                                                 (grpc-channel-handle channel)))))
                     (handler (hashtable-ref (grpc-channel-handlers channel)
                                             (grpc-request-method request)
                                             #f))
                     (response
                      (cond
                       ((not handler)
                        (grpc-response #f '() grpc-status-unimplemented "unimplemented"))
                       (else
                        (guard (c [else
                                   (grpc-response #f
                                                  '()
                                                  grpc-status-internal
                                                  (if (condition? c)
                                                      (format "~a" c)
                                                      (format "~s" c)))])
                          (normalize-response (handler request)))))))
                (respond-to-request who request response)
                request))))

  (define not-implemented-stream
    (lambda (who)
      (raise-net-error who 'grpc "streaming gRPC APIs are not implemented yet")))

  #|proc:grpc-call/server-stream
The `grpc-call/server-stream` procedure is reserved for future streaming gRPC support.
|#
  (define-who grpc-call/server-stream
    (lambda args
      (not-implemented-stream who)))

  #|proc:grpc-call/client-stream
The `grpc-call/client-stream` procedure is reserved for future streaming gRPC support.
|#
  (define-who grpc-call/client-stream
    (lambda args
      (not-implemented-stream who)))

  #|proc:grpc-call/bidi-stream
The `grpc-call/bidi-stream` procedure is reserved for future streaming gRPC support.
|#
  (define-who grpc-call/bidi-stream
    (lambda args
      (not-implemented-stream who)))

  #|proc:grpc-call/server-stream/nonblocking
The `grpc-call/server-stream/nonblocking` procedure is reserved for future streaming gRPC support.
|#
  (define-who grpc-call/server-stream/nonblocking
    (lambda args
      (not-implemented-stream who)))

  #|proc:grpc-call/client-stream/nonblocking
The `grpc-call/client-stream/nonblocking` procedure is reserved for future streaming gRPC support.
|#
  (define-who grpc-call/client-stream/nonblocking
    (lambda args
      (not-implemented-stream who)))

  #|proc:grpc-call/bidi-stream/nonblocking
The `grpc-call/bidi-stream/nonblocking` procedure is reserved for future streaming gRPC support.
|#
  (define-who grpc-call/bidi-stream/nonblocking
    (lambda args
      (not-implemented-stream who))))
