(library (chezpp net lws server)
  (export lws-http-server?
          make-lws-http-server
          lws-http-server-close!
          lws-http-server-accept
          lws-http-server-accept/nonblocking
          lws-http-request?
          lws-http-request-method
          lws-http-request-path
          lws-http-request-has-body?
          lws-http-request-read-body
          lws-http-request-write-response!
          lws-http-request-close!)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp net lws reactor))

  #|record:lws-http-server
An internal LWS listening server with synchronized pending events and lifecycle state.
|#
  (define-record-type (lws-http-server %make-lws-http-server lws-http-server?)
    (sealed #t)
    (opaque #t)
    (fields (immutable reactor)
            (immutable mutex)
            (mutable pending-events)
            (mutable closed?)))

  #|record:lws-http-request
An accepted logical HTTP request identified by connection, stream, and generation.
|#
  (define-record-type (lws-http-request %make-lws-http-request lws-http-request?)
    (sealed #t)
    (opaque #t)
    (fields (immutable server)
            (immutable connection-id)
            (immutable stream-id)
            (immutable generation)
            (immutable method)
            (immutable path)
            (immutable has-body?)
            (mutable closed?)))

  (define split-request-payload
    (lambda (payload)
      (let ([length (bytevector-length payload)])
        (let find ([index 0])
          (if (or (fx= index length) (fxzero? (bytevector-u8-ref payload index)))
              (let* ([method-bytes (make-bytevector index)]
                     [path-start (fx1+ index)]
                     [path-length (max 0 (fx- length path-start 1))]
                     [path-bytes (make-bytevector path-length)])
                (bytevector-copy! payload 0 method-bytes 0 index)
                (when (fxpositive? path-length)
                  (bytevector-copy! payload path-start path-bytes 0 path-length))
                (values (utf8->string method-bytes) (utf8->string path-bytes)))
              (find (fx1+ index)))))))

  (define event->request
    (lambda (server event)
      (and event
           (eq? (vector-ref event 0) 'headers)
           (fx>= (vector-ref event 5) 0)
           (let-values ([(method path) (split-request-payload (vector-ref event 6))])
             (%make-lws-http-request server (vector-ref event 2) (vector-ref event 3)
                                     (vector-ref event 4) method path
                                     (or (fxpositive? (vector-ref event 5))
                                         (member (string-upcase method)
                                                 '("POST" "PUT" "PATCH")))
                                     #f)))))

  #|proc:make-lws-http-server
The `make-lws-http-server` procedure creates a listening server. `interface-name` and `port`
select its address, while `tls-context-handle` is zero or a native TLS context handle. It returns
an active server transport.
|#
  (define make-lws-http-server
    (lambda (interface-name port tls-context-handle)
      (pcheck ([string? interface-name] [fixnum? port] [natural? tls-context-handle])
        (let ([reactor (make-lws-server-reactor 256 65536 256 interface-name port
                                                tls-context-handle)])
          (lws-reactor-start! reactor)
          (%make-lws-http-server reactor (make-mutex 'lws-http-server) '() #f)))))

  #|proc:lws-http-server-close!
The `lws-http-server-close!` procedure closes `server` and all live logical requests. Closing is
idempotent, and the return value is `server`.
|#
  (define lws-http-server-close!
    (lambda (server)
      (pcheck ([lws-http-server? server])
        (unless (lws-http-server-closed? server)
          (lws-http-server-closed?-set! server #t)
          (lws-reactor-shutdown! (lws-http-server-reactor server)))
        server)))

  #|proc:lws-http-server-accept/nonblocking
The `lws-http-server-accept/nonblocking` procedure returns the next logical request from `server`,
or `#f` when none is ready.
|#
  (define lws-http-server-accept/nonblocking
    (lambda (server)
      (pcheck ([lws-http-server? server])
        (and (not (lws-http-server-closed? server))
             (let loop ()
               (let ([event (lws-reactor-server-request-dequeue!
                             (lws-http-server-reactor server))])
                 (and event
                      (or (event->request server event)
                          (begin
                            (with-mutex (lws-http-server-mutex server)
                              (lws-http-server-pending-events-set!
                               server (append (lws-http-server-pending-events server)
                                              (list event))))
                            (loop))))))))))

  #|proc:lws-http-server-accept
The `lws-http-server-accept` procedure waits for and returns the next logical request from `server`.
It raises an error when the server closes before a request arrives.
|#
  (define lws-http-server-accept
    (lambda (server)
      (pcheck ([lws-http-server? server])
        (let loop ()
          (or (lws-http-server-accept/nonblocking server)
              (if (lws-http-server-closed? server)
                  (errorf 'lws-http-server-accept "server is closed")
                  (begin ($sleep (make-time 'time-duration 1000000 0)) (loop))))))))

  #|proc:lws-http-request-write-response!
The `lws-http-request-write-response!` procedure queues `payload` with HTTP `status` for `request`.
`final?` marks the final response chunk. It returns whether the reactor accepted the command.
|#
  (define lws-http-request-write-response!
    (lambda (request status payload final?)
      (pcheck ([lws-http-request? request] [fixnum? status] [bytevector? payload]
               [boolean? final?])
        (and (not (lws-http-request-closed? request))
             (lws-reactor-server-submit-response!
              (lws-http-server-reactor (lws-http-request-server request))
              (lws-http-request-connection-id request)
              (lws-http-request-stream-id request)
              (lws-http-request-generation request) status payload final?)))))

  (define matching-event?
    (lambda (request event)
      (and (= (vector-ref event 2) (lws-http-request-connection-id request))
           (= (vector-ref event 3) (lws-http-request-stream-id request))
           (= (vector-ref event 4) (lws-http-request-generation request)))))

  (define next-request-event
    (lambda (request)
      (let* ([server (lws-http-request-server request)]
             [saved
              (with-mutex (lws-http-server-mutex server)
                (let loop ([before '()] [after (lws-http-server-pending-events server)])
                  (cond
                   [(null? after) #f]
                   [(matching-event? request (car after))
                    (lws-http-server-pending-events-set!
                     server (append (reverse before) (cdr after)))
                    (car after)]
                   [else (loop (cons (car after) before) (cdr after))])))])
        (or saved
            (let loop ()
              (let ([event (lws-reactor-server-request-dequeue!
                            (lws-http-server-reactor server))])
                (cond
                 [(not event) #f]
                 [(matching-event? request event) event]
                 [else
                  (with-mutex (lws-http-server-mutex server)
                    (lws-http-server-pending-events-set!
                     server (append (lws-http-server-pending-events server) (list event))))
                  (loop)])))))))

  #|proc:lws-http-request-read-body
The `lws-http-request-read-body` procedure waits for all bounded body chunks of logical `request`,
acknowledges each chunk to resume LWS receive flow, and returns their concatenated bytevector.
|#
  (define lws-http-request-read-body
    (lambda (request)
      (pcheck ([lws-http-request? request])
        (let loop ([chunk* '()] [length 0])
          (let ([event (next-request-event request)])
            (cond
             [(not event)
              ($sleep (make-time 'time-duration 1000000 0))
              (loop chunk* length)]
             [(eq? (vector-ref event 0) 'readable)
              (let ([chunk (vector-ref event 6)])
                (lws-reactor-consume-body!
                 (lws-http-server-reactor (lws-http-request-server request))
                 (lws-http-request-connection-id request)
                 (lws-http-request-stream-id request)
                 (lws-http-request-generation request) (bytevector-length chunk))
                (loop (cons chunk chunk*) (+ length (bytevector-length chunk))))]
             [(and (eq? (vector-ref event 0) 'headers)
                   (fx= (vector-ref event 5) -1))
              (let ([body (make-bytevector length)])
                (let copy ([rest (reverse chunk*)] [offset 0])
                  (if (null? rest) body
                      (let ([chunk (car rest)])
                        (bytevector-copy! chunk 0 body offset (bytevector-length chunk))
                        (copy (cdr rest) (+ offset (bytevector-length chunk)))))))]
             [else (loop chunk* length)]))))))

  #|proc:lws-http-request-close!
The `lws-http-request-close!` procedure cancels logical `request`. It leaves sibling HTTP/2
streams active and returns `request`.
|#
  (define lws-http-request-close!
    (lambda (request)
      (pcheck ([lws-http-request? request])
        (unless (lws-http-request-closed? request)
          (lws-http-request-closed?-set! request #t)
          (lws-reactor-close-stream!
           (lws-http-server-reactor (lws-http-request-server request))
           (lws-http-request-connection-id request)
           (lws-http-request-stream-id request)
           (lws-http-request-generation request) 0))
        request)))
  )
