(library (chezpp net http2)
  (export http2-session?
          http2-open
          http2-close
          http2-submit-request
          http2-submit-response
          http2-send
          http2-receive
          http2-next-event
          http2-consume!
          http2-reset-stream!
          http2-goaway!
          http2-want-read?
          http2-want-write?
          http2-peer-max-concurrent-streams)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp net ffi)
          (chezpp net errors))

  #|record:http2-session
The `http2-session` record owns a native HTTP/2 session for an immutable client or server role.
Its handle is valid until `http2-close` releases it; operations on a closed session raise an error.
|#
  (define-record-type (http2-session %make-http2-session http2-session?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle http2-session-handle http2-session-handle-set!)
            (immutable role http2-session-role)))

  (define ensure-open
    (lambda (who session)
      (unless (http2-session-handle session)
        (raise-net-error who 'http2 "HTTP/2 session is closed" session))))

  (define ensure-result
    (lambda (who result)
      (if (and (vector? result)
               (= (vector-length result) 2)
               (eq? (vector-ref result 0) 'error))
          (raise-net-error who 'http2 (vector-ref result 1) result)
          result)))

  (define headers->vector
    (lambda (who headers)
      (unless (list? headers)
        (errorf who "headers must be an alist, given ~s" headers))
      (list->vector
       (map (lambda (entry)
              (unless (and (pair? entry) (string? (car entry)) (string? (cdr entry)))
                (errorf who "HTTP/2 header must be a string pair, given ~s" entry))
              (vector (string-downcase (car entry)) (cdr entry)))
            headers))))

  (define request-headers->vector
    (lambda (who method scheme authority path headers)
      (headers->vector
       who
       (append `((":method" . ,method)
                 (":scheme" . ,scheme)
                 (":authority" . ,authority)
                 (":path" . ,path))
               headers))))

  (define response-headers->vector
    (lambda (who status headers)
      (headers->vector who (cons (cons ":status" (number->string status)) headers))))

  #|proc:http2-open
The `http2-open` procedure creates an HTTP/2 client or server session.
`role` is `client` or `server`; the return value is an open session.
|#
  (define-who http2-open
    (lambda (role)
      (pcheck ([symbol? role])
        (unless (memq role '(client server))
          (errorf who "role must be client or server, given ~s" role))
        (let ([handle (ensure-result who (ffi-net-http2-open (if (eq? role 'server) 1 0)))])
          (unless (and (fixnum? handle) (> handle 0))
            (raise-net-error who 'http2 "failed to open HTTP/2 session" handle))
          (%make-http2-session handle role)))))

  #|proc:http2-close
The `http2-close` procedure closes `session` and returns the same session.
|#
  (define-who http2-close
    (lambda (session)
      (pcheck ([http2-session? session])
        (let ([handle (http2-session-handle session)])
          (when handle
            (ensure-result who (ffi-net-http2-close handle))
            (http2-session-handle-set! session #f)))
        session)))

  #|proc:http2-submit-request
The `http2-submit-request` procedure queues a request and bytevector `body` on `session`.
In the full arity, `method`, `scheme`, `authority`, and `path` supply request pseudo-headers, and
`headers` supplies regular header pairs. The compatibility arity uses GET, https, localhost, and /.
It returns the newly assigned numeric stream identifier.
|#
  (define-who http2-submit-request
    (case-lambda
      [(session headers body)
       (http2-submit-request session "GET" "https" "localhost" "/" headers body)]
      [(session method scheme authority path headers body)
       (pcheck ([http2-session? session] [string? method scheme authority path]
                [list? headers] [bytevector? body])
         (ensure-open who session)
         (ensure-result who
                        (ffi-net-http2-submit-request
                         (http2-session-handle session)
                         (request-headers->vector
                          who method scheme authority path headers)
                         body)))]))

  #|proc:http2-submit-response
The `http2-submit-response` procedure queues a response for `stream-id` on `session`.
The full arity sends numeric `status`, regular `headers`, and bytevector `body`.
The compatibility arity sends status 200.
It produces no useful return value after the response is accepted.
|#
  (define-who http2-submit-response
    (case-lambda
      [(session stream-id headers body)
       (http2-submit-response session stream-id 200 headers body)]
      [(session stream-id status headers body)
       (pcheck ([http2-session? session] [fixnum? stream-id status]
                [list? headers] [bytevector? body])
         (unless (<= 100 status 999)
           (errorf who "status must be between 100 and 999, given ~s" status))
         (ensure-open who session)
         (ensure-result
          who
          (ffi-net-http2-submit-response
           (http2-session-handle session) stream-id
           (response-headers->vector who status headers) body)))]))

  #|proc:http2-send
The `http2-send` procedure returns pending serialized session bytes or `#f`.
|#
  (define-who http2-send
    (lambda (session)
      (pcheck ([http2-session? session])
        (ensure-open who session)
        (ensure-result who (ffi-net-http2-mem-send (http2-session-handle session))))))

  #|proc:http2-receive
The `http2-receive` procedure feeds serialized `bytes` from `start` through `stop`.
It returns the number of bytes consumed.
|#
  (define-who http2-receive
    (lambda (session bytes start stop)
      (pcheck ([http2-session? session] [bytevector? bytes]
               [natural? start stop])
        (ensure-open who session)
        (unless (<= start stop (bytevector-length bytes))
          (errorf who "invalid bytevector slice [~a, ~a)" start stop))
        (ensure-result who
                       (ffi-net-http2-mem-recv
                        (http2-session-handle session) bytes start stop)))))

  #|proc:http2-next-event
The `http2-next-event` procedure removes and returns the next adapter event, or `#f`.
|#
  (define-who http2-next-event
    (lambda (session)
      (pcheck ([http2-session? session])
        (ensure-open who session)
        (ffi-net-http2-next-event (http2-session-handle session)))))

  #|proc:http2-consume!
The `http2-consume!` procedure acknowledges `count` received bytes on `stream-id`.
It returns the native status code.
|#
  (define-who http2-consume!
    (lambda (session stream-id count)
      (pcheck ([http2-session? session] [fixnum? stream-id] [natural? count])
        (ensure-open who session)
        (ensure-result who (ffi-net-http2-consume (http2-session-handle session) stream-id count)))))

  #|proc:http2-reset-stream!
The `http2-reset-stream!` procedure sends an error reset for `stream-id`.
It returns the native status code.
|#
  (define-who http2-reset-stream!
    (lambda (session stream-id error-code)
      (pcheck ([http2-session? session] [fixnum? stream-id] [fixnum? error-code])
        (ensure-open who session)
        (ensure-result who
                       (ffi-net-http2-rst
                        (http2-session-handle session) stream-id error-code)))))

  #|proc:http2-goaway!
The `http2-goaway!` procedure sends GOAWAY with `last-stream-id` and `error-code`.
It returns the native status code.
|#
  (define-who http2-goaway!
    (lambda (session last-stream-id error-code)
      (pcheck ([http2-session? session] [fixnum? last-stream-id] [fixnum? error-code])
        (ensure-open who session)
        (ensure-result who
                       (ffi-net-http2-goaway
                        (http2-session-handle session) last-stream-id error-code)))))

  #|proc:http2-want-read?
The `http2-want-read?` procedure reports whether `session` needs input bytes.
|#
  (define-who http2-want-read?
    (lambda (session)
      (pcheck ([http2-session? session])
        (ensure-open who session)
        (ffi-net-http2-want-read (http2-session-handle session)))))

  #|proc:http2-want-write?
The `http2-want-write?` procedure reports whether `session` has output bytes.
|#
  (define-who http2-want-write?
    (lambda (session)
      (pcheck ([http2-session? session])
        (ensure-open who session)
        (ffi-net-http2-want-write (http2-session-handle session)))))

#|proc:http2-peer-max-concurrent-streams
The `http2-peer-max-concurrent-streams` procedure queries an open HTTP/2 `session`.
The return value is the peer's current concurrent stream limit as a natural number.
|#
  (define-who http2-peer-max-concurrent-streams
    (lambda (session)
      (pcheck ([http2-session? session])
        (ensure-open who session)
        (ensure-result
         who
         (ffi-net-http2-peer-max-concurrent-streams
          (http2-session-handle session))))))
)
