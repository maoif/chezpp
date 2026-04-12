(library (chezpp net rpc)
  (export define-rpc-message
          define-rpc-service
          rpc-open
          rpc-close
          rpc-cancel-pending!
          rpc-call
          rpc-call/server-stream
          rpc-call/server-stream/nonblocking
          rpc-call/client-stream
          rpc-call/client-stream/nonblocking
          rpc-call/bidi-stream
          rpc-call/bidi-stream/nonblocking
          rpc-notify
          rpc-register-handler!
          rpc-serve
          rpc-serve/nonblocking
          rpc-channel?
          rpc-stream?
          rpc-stream-send
          rpc-stream-recv
          rpc-stream-close-send
          rpc-stream-close
          rpc-request
          rpc-request?
          rpc-request-id
          rpc-request-method
          rpc-request-payload
          rpc-request-notify?
          rpc-response
          rpc-response?
          rpc-response-ok?
          rpc-response-payload
          rpc-response-error-message
          rpc-call/nonblocking
          rpc-notify/nonblocking)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp net errors)
          (chezpp net address)
          (chezpp net socket)
          (chezpp net poll)
          (chezpp net private)
          (chezpp net private rpc))

  (define-syntax define-rpc-message
    (lambda (stx)
      (define make-id
        (lambda (ctx . parts)
          (datum->syntax
           ctx
           (string->symbol
            (apply string-append
                   (map (lambda (part)
                          (cond
                           [(symbol? part) (symbol->string part)]
                           [(string? part) part]
                           [else (error 'define-rpc "invalid identifier part")]))
                        parts))))))
      (define quoted-datum-stx
        (lambda (ctx x)
          (datum->syntax ctx (list 'quote x))))
      (syntax-case stx ()
        [(_ name ([index field type]) ...)
         (let* ([name* (syntax->datum #'name)]
                [maker (make-id #'name "make-" name*)]
                [raw-maker (make-id #'name "%make-" name*)]
                [pred (make-id #'name name* "?")]
                [descriptor (make-id #'name name* "-rpc-message")]
                [field* (syntax->list #'(field ...))]
                [type* (map syntax->datum (syntax->list #'(type ...)))]
                [accessor* (map (lambda (field)
                                  (make-id #'name name* "-" (syntax->datum field)))
                                field*)]
                [optional?* (map (lambda (type)
                                 (and (pair? type)
                                        (eq? (car type) 'optional)))
                                 type*)])
           (with-syntax ([(accessor ...) accessor*]
                         [(type-datum ...) (map (lambda (type)
                                                  (quoted-datum-stx #'name type))
                                                type*)]
                         [(optional? ...) (map (lambda (flag) (if flag #'#t #'#f)) optional?*)]
                         [(message-name ...) (map (lambda (field)
                                                    (quoted-datum-stx #'name
                                                                      (syntax->datum field)))
                                                  field*)]
                         [maker maker]
                         [raw-maker raw-maker]
                         [pred pred]
                         [descriptor descriptor]
                         [message-symbol (quoted-datum-stx #'name name*)])
             #'(begin
                 (define-record-type (name raw-maker pred)
                   (sealed #t)
                   (opaque #f)
                   (fields (immutable field accessor) ...))

                 (define-who maker
                   (lambda (field ...)
                     (pcheck ([(rpc-type-predicate type-datum) field] ...)
                             (raw-maker field ...))))

                 (define descriptor
                   (register-rpc-message-descriptor!
                    (make-rpc-message-descriptor
                     message-symbol
                     message-symbol
                     (list (make-rpc-field-descriptor index message-name type-datum accessor optional?) ...)
                     pred
                     maker))))))]
        [_ (syntax-error stx "invalid define-rpc-message form")])))

  (define-syntax define-rpc-service
    (lambda (stx)
      (define make-id
        (lambda (ctx . parts)
          (datum->syntax
           ctx
           (string->symbol
            (apply string-append
                   (map (lambda (part)
                          (cond
                           [(symbol? part) (symbol->string part)]
                           [(string? part) part]
                           [else (error 'define-rpc "invalid identifier part")]))
                        parts))))))
      (define quoted-datum-stx
        (lambda (ctx x)
          (datum->syntax ctx (list 'quote x))))
      (define datum-stx
        (lambda (ctx x)
          (datum->syntax ctx x)))
      (define parse-method-clause
        (lambda (clause)
          (let ([datum (syntax->datum clause)])
            (unless (and (list? datum)
                         (pair? datum)
                         (eq? (car datum) 'rpc)
                         (pair? (cdr datum))
                         (symbol? (cadr datum)))
              (syntax-error clause "invalid RPC method declaration"))
            (let loop ([rest (cddr datum)] [req #f] [resp #f] [stream 'unary])
              (if (null? rest)
                  (begin
                    (unless req
                      (syntax-error clause "RPC method is missing request type"))
                    (unless resp
                      (syntax-error clause "RPC method is missing response type"))
                    (list (cadr datum) req resp stream))
                  (let ([entry (car rest)])
                    (unless (and (list? entry)
                                 (= (length entry) 2)
                                 (symbol? (car entry)))
                      (syntax-error clause "invalid RPC method clause"))
                    (case (car entry)
                      [(request) (loop (cdr rest) (cadr entry) resp stream)]
                      [(response) (loop (cdr rest) req (cadr entry) stream)]
                      [(stream) (loop (cdr rest) req resp (cadr entry))]
                      [else (syntax-error clause "invalid RPC method clause")])))))))
      (syntax-case stx ()
        [(_ service method-clause ...)
         (let* ([service* (syntax->datum #'service)]
                [service-id (make-id #'service service* "-rpc-service")]
                [method-info* (map parse-method-clause
                                   (syntax->list #'(method-clause ...)))]
                [method-id* (map (lambda (info)
                                   (make-id #'service
                                            service*
                                            "-"
                                            (car info)))
                                 method-info*)]
                [request-id* (map (lambda (info)
                                    (make-id #'service (cadr info) "-rpc-message"))
                                  method-info*)]
                [response-id* (map (lambda (info)
                                     (make-id #'service (caddr info) "-rpc-message"))
                                   method-info*)]
                [fullname* (map (lambda (info)
                                 (format "/~a/~a" service* (car info)))
                                method-info*)]
                [stream* (map cadddr method-info*)]
                [name* (map car method-info*)])
           (with-syntax ([(method-id ...) method-id*]
                         [(request-id ...) request-id*]
                         [(response-id ...) response-id*]
                         [(full-name ...) (map (lambda (x) (datum-stx #'service x))
                                               fullname*)]
                         [(stream ...) (map (lambda (x)
                                              (quoted-datum-stx #'service x))
                                            stream*)]
                         [(method-name ...) (map (lambda (x)
                                                   (quoted-datum-stx #'service x))
                                                 name*)]
                         [service-id service-id]
                         [service-symbol (quoted-datum-stx #'service service*)])
             #'(begin
                 (define method-id
                   (make-rpc-method-descriptor
                    service-symbol
                    method-name
                    full-name
                    request-id
                    response-id
                    stream))
                 ...
                 (define service-id
                   (make-rpc-service-descriptor
                    service-symbol
                    (list method-id ...))))))]
        [_ (syntax-error stx "invalid define-rpc-service form")])))

  (define-record-type (rpc-request-record %make-rpc-request-record rpc-request-record?)
    (sealed #t)
    (opaque #f)
    (fields (immutable id rpc-request-id)
            (immutable method rpc-request-method)
            (immutable payload rpc-request-payload)
            (immutable notify? rpc-request-notify?)))

  (define-record-type (rpc-response-record %make-rpc-response-record rpc-response-record?)
    (sealed #t)
    (opaque #f)
    (fields (immutable ok? rpc-response-ok?)
            (immutable payload rpc-response-payload)
            (immutable error-message rpc-response-error-message)))

  (define-record-type (rpc-stream-record %make-rpc-stream rpc-stream?)
    (sealed #t)
    (opaque #f)
    (fields (immutable socket rpc-stream-socket)
            (immutable id rpc-stream-id)
            (immutable send-type rpc-stream-send-type)
            (immutable recv-type rpc-stream-recv-type)
            (immutable deadline-ms rpc-stream-deadline-ms)
            (mutable send-closed? rpc-stream-send-closed? rpc-stream-send-closed?-set!)
            (mutable recv-closed? rpc-stream-recv-closed? rpc-stream-recv-closed?-set!)
            (mutable closed? rpc-stream-closed? rpc-stream-closed?-set!)))

  (define rpc-request? rpc-request-record?)
  (define rpc-response? rpc-response-record?)

  (define-record-type (rpc-pending-op %make-rpc-pending-op rpc-pending-op?)
    (sealed #t)
    (opaque #f)
    (fields (immutable kind rpc-pending-kind)
            (immutable args rpc-pending-args)
            (mutable socket rpc-pending-socket rpc-pending-socket-set!)
            (immutable notify? rpc-pending-notify?)
            (immutable response-type rpc-pending-response-type)
            (immutable deadline-ms rpc-pending-deadline-ms)
            (immutable request-bytes rpc-pending-request-bytes)
            (mutable request-offset rpc-pending-request-offset rpc-pending-request-offset-set!)
            (mutable stage rpc-pending-stage rpc-pending-stage-set!)
            (mutable header rpc-pending-header rpc-pending-header-set!)
            (mutable header-offset rpc-pending-header-offset rpc-pending-header-offset-set!)
            (mutable body rpc-pending-body rpc-pending-body-set!)
            (mutable body-offset rpc-pending-body-offset rpc-pending-body-offset-set!)
            (immutable reader rpc-pending-reader)
            (immutable writer rpc-pending-writer)
            (immutable thread rpc-pending-thread)
            (mutable done? rpc-pending-done? rpc-pending-done?-set!)
            (mutable result rpc-pending-result rpc-pending-result-set!)
            (mutable cancelled? rpc-pending-cancelled? rpc-pending-cancelled?-set!)))

  (define-record-type (rpc-channel %make-rpc-channel rpc-channel?)
    (sealed #t)
    (opaque #f)
    (fields (immutable role rpc-channel-role)
            (immutable host rpc-channel-host)
            (immutable port rpc-channel-port)
            (mutable socket rpc-channel-socket rpc-channel-socket-set!)
            (immutable handlers rpc-channel-handlers)
            (mutable pending rpc-channel-pending rpc-channel-pending-set!)
            (mutable closed? rpc-channel-closed? rpc-channel-closed?-set!)))

  (define ensure-channel-open
    (lambda (who channel)
      (when (rpc-channel-closed? channel)
        (raise-net-error who 'rpc "RPC channel is closed" channel))))

  (define ensure-stream-open
    (lambda (who stream)
      (when (rpc-stream-closed? stream)
        (raise-net-error who 'rpc "RPC stream is closed" stream))))

  (define ensure-pending-matches
    (lambda (who channel kind args)
      (let ([pending (rpc-channel-pending channel)])
        (when (and pending
                   (or (not (eq? (rpc-pending-kind pending) kind))
                       (not (equal? (rpc-pending-args pending) args))))
          (raise-net-error who 'rpc "another nonblocking RPC operation is pending" pending)))))

  (define ensure-role
    (lambda (who channel role)
      (unless (eq? (rpc-channel-role channel) role)
        (raise-net-error who 'rpc
                         (format "RPC channel role mismatch, expected ~a" role)
                         channel))))

  (define make-method-table
    (lambda ()
      (make-hashtable string-hash string=?)))

  (define rpc-default-timeout-ms 30000)

  (define current-time-ms
    (lambda ()
      (let ([t (current-time)])
        (+ (* (time-second t) 1000)
           (quotient (time-nanosecond t) 1000000)))))

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

  (define timeout->deadline-ms
    (lambda (timeout-ms)
      (and (fx>= timeout-ms 0)
           (+ (current-time-ms) timeout-ms))))

  (define u32-set!
    (lambda (bv index value)
      (bytevector-u8-set! bv index (fxlogand (fxsrl value 24) #xff))
      (bytevector-u8-set! bv (+ index 1) (fxlogand (fxsrl value 16) #xff))
      (bytevector-u8-set! bv (+ index 2) (fxlogand (fxsrl value 8) #xff))
      (bytevector-u8-set! bv (+ index 3) (fxlogand value #xff))))

  (define u32-ref
    (lambda (bv index)
      (fxlogor (fxsll (bytevector-u8-ref bv index) 24)
               (fxsll (bytevector-u8-ref bv (+ index 1)) 16)
               (fxsll (bytevector-u8-ref bv (+ index 2)) 8)
               (bytevector-u8-ref bv (+ index 3)))))

  (define datum->frame
    (lambda (datum)
      (let-values ([(op get) (open-bytevector-output-port)]) ;; TODO port closed?
        (fasl-write datum op)
        (let* ([body (get)]
               [n (bytevector-length body)]
               [frame (make-bytevector (+ 4 n) 0)])
          (u32-set! frame 0 n)
          (bytevector-copy! body 0 frame 4 n)
          frame))))

  (define frame->datum
    (lambda (bv)
      (call-with-port (open-bytevector-input-port bv)
        (lambda (ip)
          (fasl-read ip)))))

  (define read-frame
    (lambda (who sock)
      (let ([header (make-bytevector 4 0)])
        (let loop ([offset 0])
          (if (= offset 4)
              (let* ([body-len (u32-ref header 0)]
                     [body (make-bytevector body-len 0)])
                (let fill ([body-offset 0])
                  (if (= body-offset body-len)
                      (frame->datum body)
                      (let ([n (socket-recv! sock body body-offset body-len)])
                        (cond
                         [(eof-object? n)
                          (raise-net-error who 'rpc "unexpected EOF while reading RPC frame" sock)]
                         [else
                          (fill (+ body-offset n))])))))
              (let ([n (socket-recv! sock header offset 4)])
                (cond
                 [(eof-object? n)
                  (raise-net-error who 'rpc "unexpected EOF while reading RPC frame header" sock)]
                 [else
                  (loop (+ offset n))])))))))

  (define write-frame!
    (lambda (who sock datum)
      (let ([frame (datum->frame datum)])
        (socket-send-all sock frame 0 (bytevector-length frame)))))

  (define normalize-method
    (lambda (who method)
      (cond
       [(rpc-method-descriptor? method)
        (values (rpc-method-descriptor-full-name method)
                (rpc-method-descriptor-request method)
                (rpc-method-descriptor-response method)
                (rpc-method-descriptor-stream method))]
       [(string? method) (values method #f #f 'unary)]
       [(symbol? method) (values (symbol->string method) #f #f 'unary)]
       [else
        (errorf who "invalid RPC method ~s" method)])))

  (define handler-entry
    (lambda (channel method-name)
      (hashtable-ref (rpc-channel-handlers channel) method-name #f)))

  (define make-request-datum
    (lambda (id method-name payload notify? request-type)
      (vector 'rpc-request
              id
              method-name
              (rpc-encode-payload payload request-type)
              notify?)))

  (define decode-response-datum
    (lambda (who datum response-type)
      (unless (and (vector? datum)
                   (= (vector-length datum) 5)
                   (eq? (vector-ref datum 0) 'rpc-response))
        (raise-net-error who 'rpc "invalid RPC response frame" datum))
      (%make-rpc-response-record
       (vector-ref datum 2)
       (rpc-decode-payload (vector-ref datum 3) response-type)
       (vector-ref datum 4))))

  (define decode-request-datum
    (lambda (who datum method-type)
      (unless (and (vector? datum)
                   (= (vector-length datum) 5)
                   (eq? (vector-ref datum 0) 'rpc-request))
        (raise-net-error who 'rpc "invalid RPC request frame" datum))
      (%make-rpc-request-record
       (vector-ref datum 1)
       (vector-ref datum 2)
       (rpc-decode-payload (vector-ref datum 3) method-type)
       (vector-ref datum 4))))

  (define stream-timeout-message
    (lambda (stream)
      "RPC stream timed out"))

  (define stream-remaining-timeout-ms
    (lambda (stream)
      (let ([deadline (rpc-stream-deadline-ms stream)])
        (and deadline
             (max 0 (- deadline (current-time-ms)))))))

  (define wait-stream-ready!
    (lambda (who stream event*)
      (let* ([timeout-ms (let ([x (stream-remaining-timeout-ms stream)])
                           (if x x -1))]
             [target (car (poll (list (make-poll-target (rpc-stream-socket stream) event*))
                                timeout-ms))]
             [ready (poll-target-ready-events target)])
        (when (null? ready)
          (raise-net-error who 'rpc (stream-timeout-message stream) stream))
        ready)))

  (define make-stream-open-datum
    (lambda (id method-name stream payload request-type)
      (vector 'rpc-stream-open
              id
              method-name
              stream
              (rpc-encode-payload payload request-type))))

  (define decode-stream-open-datum
    (lambda (who datum request-type)
      (unless (and (vector? datum)
                   (= (vector-length datum) 5)
                   (eq? (vector-ref datum 0) 'rpc-stream-open))
        (raise-net-error who 'rpc "invalid RPC stream-open frame" datum))
      (values (vector-ref datum 1)
              (vector-ref datum 2)
              (vector-ref datum 3)
              (rpc-decode-payload (vector-ref datum 4) request-type))))

  (define make-stream-message-datum
    (lambda (stream payload)
      (vector 'rpc-stream-message
              (rpc-stream-id stream)
              (rpc-encode-payload payload (rpc-stream-send-type stream)))))

  (define stream-frame-id
    (lambda (who datum tag length)
      (unless (and (vector? datum)
                   (= (vector-length datum) length)
                   (eq? (vector-ref datum 0) tag))
        (raise-net-error who 'rpc
                         (format "invalid RPC ~a frame" tag)
                         datum))
      (vector-ref datum 1)))

  (define decode-stream-message-datum
    (lambda (who stream datum)
      (let ([id (stream-frame-id who datum 'rpc-stream-message 3)])
        (unless (= id (rpc-stream-id stream))
          (raise-net-error who 'rpc "RPC stream id mismatch" datum))
        (rpc-decode-payload (vector-ref datum 2)
                            (rpc-stream-recv-type stream)))))

  (define stream-close-frame?
    (lambda (stream datum)
      (and (vector? datum)
           (= (vector-length datum) 2)
           (eq? (vector-ref datum 0) 'rpc-stream-close)
           (= (vector-ref datum 1) (rpc-stream-id stream)))))

  (define decode-stream-error-datum
    (lambda (who stream datum)
      (let ([id (stream-frame-id who datum 'rpc-stream-error 3)])
        (unless (= id (rpc-stream-id stream))
          (raise-net-error who 'rpc "RPC stream id mismatch" datum))
        (vector-ref datum 2))))

  (define send-stream-error!
    (lambda (who stream message)
      (guard (c [else #f])
        (write-frame! who
                      (rpc-stream-socket stream)
                      (vector 'rpc-stream-error
                              (rpc-stream-id stream)
                              message)))))

  (define close-stream-socket!
    (lambda (stream)
      (unless (rpc-stream-closed? stream)
        (guard (c [else #f])
          (close-socket (rpc-stream-socket stream)))
        (rpc-stream-closed?-set! stream #t))))

  (define maybe-close-stream-socket!
    (lambda (stream)
      (when (and (rpc-stream-send-closed? stream)
                 (rpc-stream-recv-closed? stream))
        (close-stream-socket! stream))))

  (define make-server-stream
    (lambda (who sock id stream request-type response-type)
      (case stream
        [(server)
         (%make-rpc-stream sock id response-type #f #f #f #t #f)]
        [(client)
         (%make-rpc-stream sock id response-type request-type #f #f #f #f)]
        [(bidi)
         (%make-rpc-stream sock id response-type request-type #f #f #f #f)]
        [else
         (raise-net-error who 'rpc "invalid RPC stream shape" stream)])))

  (define normalize-stream-final-response
    (lambda (value)
      (if (rpc-response-record? value)
          value
          (%make-rpc-response-record #t value #f))))

  (define open-client-stream
    (lambda (who channel method stream payload timeout-ms)
      (let-values ([(method-name request-type response-type stream-shape) (normalize-method who method)])
        (unless (eq? stream-shape stream)
          (raise-net-error who 'rpc "RPC stream shape mismatch" method))
        (let-values ([(sock ans) (open-client-socket who channel #f)])
          (guard (c [else
                     (guard (x [else #f])
                       (close-socket sock))
                     (raise c)])
            (let ([stream-obj
                   (case stream
                     [(server)
                      (%make-rpc-stream sock
                                        1
                                        #f
                                        response-type
                                        (timeout->deadline-ms timeout-ms)
                                        #t
                                        #f
                                        #f)]
                     [(client)
                      (%make-rpc-stream sock
                                        1
                                        request-type
                                        response-type
                                        (timeout->deadline-ms timeout-ms)
                                        #f
                                        #f
                                        #f)]
                     [(bidi)
                      (%make-rpc-stream sock
                                        1
                                        request-type
                                        response-type
                                        (timeout->deadline-ms timeout-ms)
                                        #f
                                        #f
                                        #f)]
                     [else
                      (raise-net-error who 'rpc "invalid RPC stream shape" stream)])])
              (write-frame! who
                            sock
                            (make-stream-open-datum 1
                                                    method-name
                                                    stream
                                                    payload
                                                    (and (eq? stream 'server) request-type)))
              stream-obj))))))

  (define normalize-response
    (lambda (value)
      (if (rpc-response-record? value)
          value
          (%make-rpc-response-record #t value #f))))

  (define open-client-socket
    (lambda (who channel nonblocking?)
      (let ([sock (open-socket 'inet 'stream)])
        (guard (c [else
                   (guard (x [else #f])
                     (close-socket sock))
                   (raise c)])
          (when nonblocking?
            (socket-set-blocking! sock #f))
          (let ([ans (socket-connect! sock
                                      (make-socket-address 'inet
                                                           (rpc-channel-host channel)
                                                           (rpc-channel-port channel)))])
            (values sock ans))))))

  (define close-pending-socket!
    (lambda (pending)
      (let ([sock (rpc-pending-socket pending)])
        (when sock
          (guard (c [else #f])
            (close-socket sock))
          (rpc-pending-socket-set! pending #f)))))

  (define close-pending-notifier!
    (lambda (pending)
      (let ([reader (rpc-pending-reader pending)]
            [writer (rpc-pending-writer pending)])
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

  (define clear-pending!
    (lambda (channel pending)
      (close-pending-socket! pending)
      (close-pending-notifier! pending)
      (rpc-channel-pending-set! channel #f)))

  (define cancel-pending!
    (lambda (channel pending)
      (rpc-pending-cancelled?-set! pending #t)
      (let ([result (rpc-pending-result pending)])
        (when (rpc-stream? result)
          (guard (c [else #f])
            (rpc-stream-close result))
          (rpc-pending-result-set! pending #f)))
      (clear-pending! channel pending)
      (wait-pending-thread! pending)))

  (define wait-pending-thread!
    (lambda (pending)
      (let ([th (rpc-pending-thread pending)])
        (when th
          (thread-join th)))))

  (define pending-timeout-message
    (lambda (pending)
      (case (rpc-pending-kind pending)
        [(notify) "RPC notification timed out"]
        [(call) "RPC call timed out"]
        [else "RPC stream timed out"])))

  (define pending-expired?
    (lambda (pending)
      (let ([deadline (rpc-pending-deadline-ms pending)])
        (and deadline (<= deadline (current-time-ms))))))

  (define remaining-timeout-ms
    (lambda (pending)
      (let ([deadline (rpc-pending-deadline-ms pending)])
        (and deadline
             (max 0 (- deadline (current-time-ms)))))))

  (define pending-events
    (lambda (pending)
      (case (rpc-pending-stage pending)
        [(connect send) '(write error hup invalid)]
        [(recv-header recv-body) '(read error hup invalid)]
        [else '(error hup invalid)])))

  (define raise-pending-timeout!
    (lambda (who channel pending)
      (cancel-pending! channel pending)
      (raise-net-error who 'rpc (pending-timeout-message pending) pending)))

  (define call-with-pending-cleanup
    (lambda (channel thunk)
      (guard (c [else
                 (let ([pending (rpc-channel-pending channel)])
                   (when pending
                     (cancel-pending! channel pending)))
                 (raise c)])
        (thunk))))

  ;; TODO doc
  (define unary-pending-key
    (lambda (who method payload notify? timeout-ms)
      (let-values ([(method-name request-type response-type stream) (normalize-method who method)])
        (unless (eq? stream 'unary)
          (raise-net-error who 'rpc "streaming RPC methods are not implemented yet" method))
        (let ([frame (datum->frame
                      (make-request-datum 1 method-name payload notify? request-type))])
          (values method-name request-type response-type frame
                  (list method-name notify? frame timeout-ms))))))

  (define stream-pending-key
    (lambda (who method stream payload timeout-ms)
      (let-values ([(method-name request-type response-type stream-shape) (normalize-method who method)])
        (unless (eq? stream-shape stream)
          (raise-net-error who 'rpc "RPC stream shape mismatch" method))
        (let ([frame (datum->frame
                      (make-stream-open-datum 1
                                              method-name
                                              stream
                                              payload
                                              (and (eq? stream 'server) request-type)))])
          (values method-name request-type response-type frame
                  (list method-name stream frame timeout-ms))))))

  (define start-pending-op!
    (lambda (who channel method payload notify? timeout-ms)
      (let-values ([(method-name request-type response-type frame key)
                    (unary-pending-key who method payload notify? timeout-ms)])
        (let* ([kind (if notify? 'notify 'call)]
               [args key])
          (let-values ([(sock ans) (open-client-socket who channel #t)])
            (rpc-channel-pending-set!
             channel
             (%make-rpc-pending-op kind
                                   args
                                   sock
                                   notify?
                                   response-type
                                   (timeout->deadline-ms timeout-ms)
                                   frame
                                   0
                                   'send
                                   (make-bytevector 4 0)
                                    0
                                    #f
                                    0
                                    #f
                                    #f
                                    #f
                                    #f
                                    #f
                                    #f))
            (unless ans
              (rpc-pending-stage-set! (rpc-channel-pending channel) 'connect)))))))

  (define start-pending-stream!
    (lambda (who channel method stream payload timeout-ms)
      (let-values ([(method-name request-type response-type frame key)
                    (stream-pending-key who method stream payload timeout-ms)])
        (let-values ([(reader writer) (open-pending-notifier)])
          (letrec ([pending
                    (%make-rpc-pending-op
                     (case stream
                       [(server) 'server-stream]
                       [(client) 'client-stream]
                       [(bidi) 'bidi-stream])
                     key
                     #f
                     #f
                     #f
                     (timeout->deadline-ms timeout-ms)
                     #f
                     0
                     #f
                     #f
                     0
                     #f
                     0
                     reader
                     writer
                     (fork-thread
                      (lambda ()
                        (let ([result
                               (guard (c [else c])
                                 (open-client-stream who channel method stream payload timeout-ms))])
                          (when (and (rpc-stream? result)
                                     (rpc-pending-cancelled? pending))
                            (guard (c [else #f])
                              (rpc-stream-close result))
                            (set! result #f))
                          (rpc-pending-result-set! pending result))
                        (rpc-pending-done?-set! pending #t)
                        (guard (c [else #f])
                          (socket-send-all writer #vu8(1)))))
                     #f
                     #f
                     #f)])
            (rpc-channel-pending-set! channel pending)
            pending)))))

  (define pending-stream-ready?
    (lambda (pending)
      (or (rpc-pending-done? pending)
          (let ([reader (rpc-pending-reader pending)])
            (and reader
                 (let* ([target (make-poll-target reader '(read error hup invalid))]
                        [ready (car (poll/nonblocking (list target)))])
                   (memq 'read (poll-target-ready-events ready))))))))

  (define finish-pending-stream!
    (lambda (who channel pending)
      (when (pending-expired? pending)
        (raise-pending-timeout! who channel pending))
      (rpc-channel-pending-set! channel #f)
      (thread-join (rpc-pending-thread pending))
      (close-pending-notifier! pending)
      (let ([result (rpc-pending-result pending)])
        (if (condition? result)
            (raise result)
            result))))

  (define open-client-stream/nonblocking
    (lambda (who channel kind method stream payload timeout-ms)
      (ensure-channel-open who channel)
      (ensure-role who channel 'client)
      (let-values ([(method-name request-type response-type frame key)
                    (stream-pending-key who method stream payload timeout-ms)])
        (ensure-pending-matches who channel kind key)
        (let ([pending (or (rpc-channel-pending channel)
                           (start-pending-stream!
                            who
                            channel
                            method
                            stream
                            payload
                            timeout-ms))])
          (if (pending-stream-ready? pending)
              (finish-pending-stream! who channel pending)
              (begin
                (when (pending-expired? pending)
                  (raise-pending-timeout! who channel pending))
                #f))))))

  (define maybe-progress-connect!
    (lambda (pending)
      (let* ([sock (rpc-pending-socket pending)]
             [target (make-poll-target sock '(write error hup invalid))]
             [ready (car (poll/nonblocking (list target)))])
        (and (pair? (poll-target-ready-events ready))
             (begin
               (rpc-pending-stage-set! pending 'send)
               #t)))))

  (define maybe-send!
    (lambda (pending)
      (let* ([sock (rpc-pending-socket pending)]
             [frame (rpc-pending-request-bytes pending)]
             [offset (rpc-pending-request-offset pending)]
             [n (bytevector-length frame)])
        (if (= offset n)
            #t
            (let ([sent (socket-send/nonblocking sock frame offset n)])
              (cond
               [(not sent) #f]
               [else
                (rpc-pending-request-offset-set! pending (+ offset sent))
                (= (+ offset sent) n)]))))))

  (define maybe-recv-into!
    (lambda (sock bv offset stop)
      (let ([n (socket-recv!/nonblocking sock bv offset stop)])
        (cond
         [(not n) #f]
         [(eof-object? n)
          (raise-net-error 'rpc 'rpc "unexpected EOF during nonblocking RPC read" sock)]
         [else n]))))

  (define pending-result
    (lambda (who channel pending)
      (when (pending-expired? pending)
        (raise-pending-timeout! who channel pending))
      (case (rpc-pending-stage pending)
        [(connect)
         (if (maybe-progress-connect! pending)
             (pending-result who channel pending)
             #f)]
        [(send)
         (if (maybe-send! pending)
             (if (rpc-pending-notify? pending)
                 (begin
                   (close-pending-socket! pending)
                   (rpc-channel-pending-set! channel #f)
                   channel)
                 (begin
                   (rpc-pending-stage-set! pending 'recv-header)
                   (pending-result who channel pending)))
             #f)]
        [(recv-header)
         (let* ([sock (rpc-pending-socket pending)]
                [header (rpc-pending-header pending)]
                [offset (rpc-pending-header-offset pending)]
                [n (maybe-recv-into! sock header offset 4)])
           (cond
            [(not n) #f]
            [else
             (let ([next (+ offset n)])
               (rpc-pending-header-offset-set! pending next)
               (if (= next 4)
                   (let* ([body-len (u32-ref header 0)]
                          [body (make-bytevector body-len 0)])
                     (rpc-pending-body-set! pending body)
                     (rpc-pending-body-offset-set! pending 0)
                     (rpc-pending-stage-set! pending 'recv-body)
                     (pending-result who channel pending))
                   #f))]))]
        [(recv-body)
         (let* ([sock (rpc-pending-socket pending)]
                [body (rpc-pending-body pending)]
                [offset (rpc-pending-body-offset pending)]
                [limit (bytevector-length body)]
                [n (maybe-recv-into! sock body offset limit)])
           (cond
            [(not n) #f]
            [else
             (let ([next (+ offset n)])
               (rpc-pending-body-offset-set! pending next)
               (if (= next limit)
                   (let* ([response (decode-response-datum who
                                                          (frame->datum body)
                                                          (rpc-pending-response-type pending))]
                          [payload (rpc-response-payload response)])
                     (close-pending-socket! pending)
                     (rpc-channel-pending-set! channel #f)
                     (if (rpc-response-ok? response)
                         payload
                         (raise-net-error who 'rpc
                                          (or (rpc-response-error-message response)
                                              "RPC call failed")
                                          response)))
                   #f))]))]
        [else
         (raise-net-error who 'rpc "invalid RPC pending state" pending)])))

  (define wait-for-pending!
    (lambda (who channel pending)
      (when (pending-expired? pending)
        (raise-pending-timeout! who channel pending))
      (let* ([sock (rpc-pending-socket pending)]
             [timeout-ms (let ([x (remaining-timeout-ms pending)])
                           (if x x -1))]
             [target (car (poll (list (make-poll-target sock (pending-events pending)))
                                timeout-ms))])
        (when (null? (poll-target-ready-events target))
          (raise-pending-timeout! who channel pending)))))

  (define finish-pending!
    (lambda (who channel)
      (call-with-pending-cleanup
       channel
       (lambda ()
         (let loop ()
           (let ([pending (rpc-channel-pending channel)])
             (if (not pending)
                 channel
                 (let ([result (pending-result who channel pending)])
                   (if result
                       result
                       (begin
                         (wait-for-pending! who channel pending)
                         (loop)))))))))))

  #|proc:rpc-request
The `rpc-request` procedure constructs an RPC request record.
|#
  (define-who rpc-request
    (case-lambda
      [(method payload) (rpc-request method payload #f)]
      [(method payload notify?)
       (let-values ([(method-name req-type resp-type stream) (normalize-method who method)])
         (%make-rpc-request-record 1 method-name payload notify?))]))

  #|proc:rpc-response
The `rpc-response` procedure constructs an RPC response record.
|#
  (define-who rpc-response
    (case-lambda
      [(payload) (%make-rpc-response-record #t payload #f)]
      [(ok? payload error-message)
       (pcheck ([boolean? ok?])
               (%make-rpc-response-record ok? payload error-message))]))

  #|proc:rpc-open
The `rpc-open` procedure opens a client RPC channel or a server RPC listener.
Use `(rpc-open host port)` for a client channel and `(rpc-open 'server host port)` for a server listener.
|#
  (define-who rpc-open
    (case-lambda
      [(host port)
       (pcheck ([string? host] [fixnum? port])
               (check-port who port)
               (%make-rpc-channel 'client host port #f (make-method-table) #f #f))]
      [(role host port)
       (pcheck ([symbol? role] [string? host] [fixnum? port])
               (check-port who port)
               (case role
                 [(server)
                  (let ([sock (open-socket 'inet 'stream)])
                    (guard (c [else
                               (guard (x [else #f])
                                 (close-socket sock))
                               (raise c)])
                      (socket-set-option! sock 'reuse-address #t)
                      (socket-bind! sock (make-socket-address 'inet host port))
                      (socket-listen! sock 16)
                      (%make-rpc-channel 'server
                                         host
                                         (socket-address-port (socket-local-address sock))
                                         sock
                                         (make-method-table)
                                         #f
                                         #f)))]
                 [else
                  (errorf who "invalid RPC role ~s" role)]))]))

  #|proc:rpc-close
The `rpc-close` procedure closes an RPC channel or listener.
|#
  (define-who rpc-close
    (lambda (channel)
      (pcheck ([rpc-channel? channel])
              (unless (rpc-channel-closed? channel)
                (let ([pending (rpc-channel-pending channel)])
                  (when pending
                    (cancel-pending! channel pending)
                    (wait-pending-thread! pending)))
                (let ([sock (rpc-channel-socket channel)])
                  (when sock
                    (close-socket sock)
                    (rpc-channel-socket-set! channel #f)))
                (rpc-channel-closed?-set! channel #t))
              channel)))

  #|proc:rpc-cancel-pending!
The `rpc-cancel-pending!` procedure cancels and discards the currently pending non-blocking RPC operation on a channel, if any.
|#
  (define-who rpc-cancel-pending!
    (lambda (channel)
      (pcheck ([rpc-channel? channel])
              (let ([pending (rpc-channel-pending channel)])
                (when pending
                  (cancel-pending! channel pending)))
              channel)))

  #|proc:rpc-register-handler!
The `rpc-register-handler!` procedure registers a request handler on a server RPC channel.
|#
  (define-who rpc-register-handler!
    (lambda (channel method proc)
      (pcheck ([rpc-channel? channel] [procedure? proc])
              (ensure-channel-open who channel)
              (ensure-role who channel 'server)
              (let-values ([(method-name req-type resp-type stream) (normalize-method who method)])
                (hashtable-set! (rpc-channel-handlers channel)
                                method-name
                                (vector method proc req-type resp-type stream))
                channel))))

  (define dispatch-request
    (lambda (who channel datum)
      (let* ([method-name (vector-ref datum 2)]
             [entry (handler-entry channel method-name)]
             [request-type (and entry (vector-ref entry 2))]
             [request (decode-request-datum who datum request-type)])
        (cond
         [(not entry)
          (values request
                  (%make-rpc-response-record #f #f
                                             (format "no RPC handler registered for ~a"
                                                     method-name)))]
         [else
          (let ([stream (vector-ref entry 4)])
            (if (not (eq? stream 'unary))
                (values request
                        (%make-rpc-response-record
                         #f
                         #f
                         (format "RPC method ~a requires streaming transport"
                                 method-name)))
                (values request
                        (guard (c [else
                                   (%make-rpc-response-record
                                    #f
                                    #f
                                    (if (condition? c)
                                        (format "~a" c)
                                        (format "~s" c)))])
                          (normalize-response ((vector-ref entry 1) request))))))]))))

  (define dispatch-stream-open
    (lambda (who channel client datum)
      (let-values ([(id method-name stream payload)
                    (decode-stream-open-datum who datum #f)])
        (let ([entry (handler-entry channel method-name)])
          (if (not entry)
              (begin
                (write-frame! who
                              client
                              (vector 'rpc-stream-error
                                      id
                                      (format "no RPC handler registered for ~a"
                                              method-name)))
                (%make-rpc-request-record id method-name payload #f))
              (let* ([proc (vector-ref entry 1)]
                     [request-type (vector-ref entry 2)]
                     [response-type (vector-ref entry 3)]
                     [expected-stream (vector-ref entry 4)])
                (if (not (eq? expected-stream stream))
                    (begin
                      (write-frame! who
                                    client
                                    (vector 'rpc-stream-error
                                            id
                                            (format "RPC stream shape mismatch for ~a"
                                                    method-name)))
                      (%make-rpc-request-record id method-name payload #f))
                    (let-values ([(id* method-name* stream* payload*)
                                  (decode-stream-open-datum
                                   who
                                   datum
                                   (and (eq? stream 'server) request-type))])
                      (let ([request (%make-rpc-request-record id* method-name* payload* #f)]
                            [stream-obj (make-server-stream who
                                                            client
                                                            id*
                                                            stream*
                                                            request-type
                                                            response-type)])
                        (guard (c [else
                                   (send-stream-error!
                                    who
                                    stream-obj
                                    (if (condition? c)
                                        (format "~a" c)
                                        (format "~s" c)))
                                   request])
                          (case stream*
                            [(server)
                             (proc request stream-obj)
                             (unless (rpc-stream-send-closed? stream-obj)
                               (rpc-stream-close-send stream-obj))
                             request]
                            [(client)
                             (let ([result (proc stream-obj)])
                               (unless (rpc-stream-send-closed? stream-obj)
                                 (let ([response (normalize-stream-final-response result)])
                                   (if (rpc-response-ok? response)
                                       (begin
                                         (rpc-stream-send
                                          stream-obj
                                          (rpc-response-payload response))
                                         (rpc-stream-close-send stream-obj))
                                       (begin
                                         (send-stream-error!
                                          who
                                          stream-obj
                                          (or (rpc-response-error-message response)
                                              "RPC stream failed"))
                                         (rpc-stream-close stream-obj)))))
                               request)]
                            [(bidi)
                             (proc stream-obj)
                             (unless (rpc-stream-send-closed? stream-obj)
                               (rpc-stream-close-send stream-obj))
                             request]
                            [else
                             (raise-net-error who 'rpc
                                              "invalid RPC stream shape"
                                              stream*)])))))))))))

  (define serve-accepted
    (lambda (who channel client)
      (dynamic-wind
        void
        (lambda ()
          (let* ([datum (read-frame who client)])
            (cond
             [(and (vector? datum)
                   (= (vector-length datum) 5)
                   (eq? (vector-ref datum 0) 'rpc-request))
              (let-values ([(request response) (dispatch-request who channel datum)])
                (unless (rpc-request-notify? request)
                  (write-frame! who
                                client
                                ;; TODO perf
                                (vector 'rpc-response
                                        (rpc-request-id request)
                                        (rpc-response-ok? response)
                                        (rpc-encode-payload (rpc-response-payload response))
                                        (rpc-response-error-message response))))
                request)]
             [(and (vector? datum)
                   (= (vector-length datum) 5)
                   (eq? (vector-ref datum 0) 'rpc-stream-open))
              (dispatch-stream-open who channel client datum)]
             [else
              (raise-net-error who 'rpc "invalid RPC frame" datum)])))
        (lambda () (close-socket client)))))

  ;;===----------------------------------------------------------------------===
  ;; Stream APIs
  ;;===----------------------------------------------------------------------===

  #|proc:rpc-stream-send
The `rpc-stream-send` procedure sends one message on an RPC stream.
|#
  (define-who rpc-stream-send
    (lambda (stream payload)
      (pcheck ([rpc-stream? stream])
              (ensure-stream-open who stream)
              (when (rpc-stream-send-closed? stream)
                (raise-net-error who 'rpc "RPC stream send side is closed" stream))
              (unless (rpc-stream-send-type stream)
                (raise-net-error who 'rpc "RPC stream does not support sending" stream))
              (wait-stream-ready! who stream '(write error hup invalid))
              (write-frame! who
                            (rpc-stream-socket stream)
                            (make-stream-message-datum stream payload))
              stream)))

  #|proc:rpc-stream-recv
The `rpc-stream-recv` procedure receives one message from an RPC stream and returns an EOF object when the peer closes its send side.
|#
  (define-who rpc-stream-recv
    (lambda (stream)
      (pcheck ([rpc-stream? stream])
              (ensure-stream-open who stream)
              (unless (rpc-stream-recv-type stream)
                (raise-net-error who 'rpc "RPC stream does not support receiving" stream))
              (if (rpc-stream-recv-closed? stream)
                  (eof-object)
                  (begin
                    (wait-stream-ready! who stream '(read error hup invalid))
                    (let ([datum (read-frame who (rpc-stream-socket stream))])
                      (cond
                       [(and (vector? datum)
                             (= (vector-length datum) 3)
                             (eq? (vector-ref datum 0) 'rpc-stream-message))
                        (decode-stream-message-datum who stream datum)]
                       [(stream-close-frame? stream datum)
                        (rpc-stream-recv-closed?-set! stream #t)
                        (maybe-close-stream-socket! stream)
                        (eof-object)]
                       [(and (vector? datum)
                             (= (vector-length datum) 3)
                             (eq? (vector-ref datum 0) 'rpc-stream-error))
                        (rpc-stream-recv-closed?-set! stream #t)
                        (rpc-stream-send-closed?-set! stream #t)
                        (close-stream-socket! stream)
                        (raise-net-error who 'rpc
                                         (decode-stream-error-datum who stream datum)
                                         stream)]
                       [else
                        (raise-net-error who 'rpc "invalid RPC stream frame" datum)])))))))

  #|proc:rpc-stream-close-send
The `rpc-stream-close-send` procedure closes the local send side of an RPC stream.
|#
  (define-who rpc-stream-close-send
    (lambda (stream)
      (pcheck ([rpc-stream? stream])
              (ensure-stream-open who stream)
              (unless (rpc-stream-send-closed? stream)
                (write-frame! who
                              (rpc-stream-socket stream)
                              (vector 'rpc-stream-close (rpc-stream-id stream)))
                (rpc-stream-send-closed?-set! stream #t)
                (maybe-close-stream-socket! stream))
              stream)))

  #|proc:rpc-stream-close
The `rpc-stream-close` procedure closes an RPC stream and releases its socket.
|#
  (define-who rpc-stream-close
    (lambda (stream)
      (pcheck ([rpc-stream? stream])
              (unless (rpc-stream-closed? stream)
                (when (and (not (rpc-stream-send-closed? stream))
                           (rpc-stream-send-type stream))
                  (guard (c [else #f])
                    (rpc-stream-close-send stream)))
                (rpc-stream-send-closed?-set! stream #t)
                (rpc-stream-recv-closed?-set! stream #t)
                (close-stream-socket! stream))
              stream)))

  #|proc:rpc-serve
The `rpc-serve` procedure accepts and processes one RPC request on a server channel.
|#
  (define-who rpc-serve
    (lambda (channel)
      (pcheck ([rpc-channel? channel])
              (ensure-channel-open who channel)
              (ensure-role who channel 'server)
              (let-values ([(client address) (socket-accept (rpc-channel-socket channel))])
                (serve-accepted who channel client)))))

  #|proc:rpc-serve/nonblocking
The `rpc-serve/nonblocking` procedure processes one RPC request if a client connection is already ready, and returns `#f` otherwise.
|#
  (define-who rpc-serve/nonblocking
    (lambda (channel)
              (pcheck ([rpc-channel? channel])
                      (ensure-channel-open who channel)
                      (ensure-role who channel 'server)
                      (call-with-values
                          (lambda ()
                            (socket-accept/nonblocking (rpc-channel-socket channel)))
                        (case-lambda
                          [(client address)
                           (serve-accepted who channel client)]
                          [(x) #f])))))

  #|proc:rpc-call
The `rpc-call` procedure performs a blocking unary RPC call and returns the decoded response payload.
An optional timeout in milliseconds can be supplied as a fourth argument.
|#
  (define-who rpc-call
    (case-lambda
      [(channel method payload)
       (rpc-call channel method payload rpc-default-timeout-ms)]
      [(channel method payload timeout-ms)
       (pcheck ([rpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (ensure-role who channel 'client)
               (let-values ([(method-name request-type response-type frame key)
                             (unary-pending-key who method payload #f timeout-ms)])
                 (ensure-pending-matches who channel 'call key))
               (unless (rpc-channel-pending channel)
                 (start-pending-op! who channel method payload #f timeout-ms))
               (finish-pending! who channel))]))

  #|proc:rpc-call/server-stream
The `rpc-call/server-stream` procedure opens a server-streaming RPC and returns an RPC stream.
An optional timeout in milliseconds can be supplied as a fourth argument.
|#
  (define-who rpc-call/server-stream
    (case-lambda
      [(channel method payload)
       (rpc-call/server-stream channel method payload rpc-default-timeout-ms)]
      [(channel method payload timeout-ms)
       (pcheck ([rpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (ensure-role who channel 'client)
               (open-client-stream who channel method 'server payload timeout-ms))]))

  #|proc:rpc-call/server-stream/nonblocking
The `rpc-call/server-stream/nonblocking` procedure progresses opening a server-streaming RPC without blocking and returns `#f` until the stream is ready.
An optional timeout in milliseconds can be supplied as a fourth argument.
|#
  (define-who rpc-call/server-stream/nonblocking
    (case-lambda
      [(channel method payload)
       (rpc-call/server-stream/nonblocking channel method payload rpc-default-timeout-ms)]
      [(channel method payload timeout-ms)
       (pcheck ([rpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (open-client-stream/nonblocking
                who
                channel
                'server-stream
                method
                'server
                payload
                timeout-ms))]))

  #|proc:rpc-call/client-stream
The `rpc-call/client-stream` procedure opens a client-streaming RPC and returns an RPC stream.
An optional timeout in milliseconds can be supplied as a third argument.
|#
  (define-who rpc-call/client-stream
    (case-lambda
      [(channel method)
       (rpc-call/client-stream channel method rpc-default-timeout-ms)]
      [(channel method timeout-ms)
       (pcheck ([rpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (ensure-role who channel 'client)
               (open-client-stream who channel method 'client #f timeout-ms))]))

  #|proc:rpc-call/client-stream/nonblocking
The `rpc-call/client-stream/nonblocking` procedure progresses opening a client-streaming RPC without blocking and returns `#f` until the stream is ready.
An optional timeout in milliseconds can be supplied as a third argument.
|#
  (define-who rpc-call/client-stream/nonblocking
    (case-lambda
      [(channel method)
       (rpc-call/client-stream/nonblocking channel method rpc-default-timeout-ms)]
      [(channel method timeout-ms)
       (pcheck ([rpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (open-client-stream/nonblocking
                who
                channel
                'client-stream
                method
                'client
                #f
                timeout-ms))]))

  #|proc:rpc-call/bidi-stream
The `rpc-call/bidi-stream` procedure opens a bidirectional streaming RPC and returns an RPC stream.
An optional timeout in milliseconds can be supplied as a third argument.
|#
  (define-who rpc-call/bidi-stream
    (case-lambda
      [(channel method)
       (rpc-call/bidi-stream channel method rpc-default-timeout-ms)]
      [(channel method timeout-ms)
       (pcheck ([rpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (ensure-role who channel 'client)
               (open-client-stream who channel method 'bidi #f timeout-ms))]))

  #|proc:rpc-call/bidi-stream/nonblocking
The `rpc-call/bidi-stream/nonblocking` procedure progresses opening a bidirectional streaming RPC without blocking and returns `#f` until the stream is ready.
An optional timeout in milliseconds can be supplied as a third argument.
|#
  (define-who rpc-call/bidi-stream/nonblocking
    (case-lambda
      [(channel method)
       (rpc-call/bidi-stream/nonblocking channel method rpc-default-timeout-ms)]
      [(channel method timeout-ms)
       (pcheck ([rpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (open-client-stream/nonblocking
                who
                channel
                'bidi-stream
                method
                'bidi
                #f
                timeout-ms))]))

  #|proc:rpc-notify
The `rpc-notify` procedure sends a unary fire-and-forget RPC notification.
An optional timeout in milliseconds can be supplied as a fourth argument.
|#
  (define-who rpc-notify
    (case-lambda
      [(channel method payload)
       (rpc-notify channel method payload rpc-default-timeout-ms)]
      [(channel method payload timeout-ms)
       (pcheck ([rpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (ensure-role who channel 'client)
               (let-values ([(method-name request-type response-type frame key)
                             (unary-pending-key who method payload #t timeout-ms)])
                 (ensure-pending-matches who channel 'notify key))
               (unless (rpc-channel-pending channel)
                 (start-pending-op! who channel method payload #t timeout-ms))
               (finish-pending! who channel))]))

  #|proc:rpc-call/nonblocking
The `rpc-call/nonblocking` procedure progresses a unary RPC call without blocking and returns `#f` while the response is still pending.
An optional timeout in milliseconds can be supplied as a fourth argument.
|#
  (define-who rpc-call/nonblocking
    (case-lambda
      [(channel method payload)
       (rpc-call/nonblocking channel method payload rpc-default-timeout-ms)]
      [(channel method payload timeout-ms)
       (pcheck ([rpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (ensure-role who channel 'client)
               (let-values ([(method-name request-type response-type frame key)
                             (unary-pending-key who method payload #f timeout-ms)])
                 (ensure-pending-matches who channel 'call key))
               (unless (rpc-channel-pending channel)
                 (start-pending-op! who channel method payload #f timeout-ms))
               (call-with-pending-cleanup
                channel
                (lambda ()
                  (pending-result who channel (rpc-channel-pending channel)))))]))

  #|proc:rpc-notify/nonblocking
The `rpc-notify/nonblocking` procedure progresses a fire-and-forget RPC notification without blocking and returns `#f` while the send is still pending.
An optional timeout in milliseconds can be supplied as a fourth argument.
|#
  (define-who rpc-notify/nonblocking
    (case-lambda
      [(channel method payload)
       (rpc-notify/nonblocking channel method payload rpc-default-timeout-ms)]
      [(channel method payload timeout-ms)
       (pcheck ([rpc-channel? channel] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (ensure-role who channel 'client)
               (let-values ([(method-name request-type response-type frame key)
                             (unary-pending-key who method payload #t timeout-ms)])
                 (ensure-pending-matches who channel 'notify key))
               (unless (rpc-channel-pending channel)
                 (start-pending-op! who channel method payload #t timeout-ms))
               (call-with-pending-cleanup
                channel
                (lambda ()
                  (pending-result who channel (rpc-channel-pending channel)))))]))
  )
