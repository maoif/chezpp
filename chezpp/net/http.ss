(library (chezpp net http)
  (export http-request?
          make-http-request
          http-request-method
          http-request-uri
          http-request-headers
          http-request-body
          http-response?
          make-http-response
          http-response-status
          http-response-reason
          http-response-headers
          http-response-body
          http-response-trailers
          http-response-version
          http-body-source? make-http-body-source http-body-source-length
          http-body-source-read
          http-body-sink? make-http-body-sink http-body-sink-write!
          http-body-sink-finish!
          make-http-port-body-source make-http-file-body-source
          make-http-port-body-sink make-http-file-body-sink
          http-cookie? make-http-cookie http-cookie-name http-cookie-value
          http-cookie-domain http-cookie-path http-cookie-secure?
          http-cookie-jar? make-http-cookie-jar
          http-proxy? make-http-proxy http-proxy-uri
          http-multipart-part? make-http-multipart-part
          http-multipart-part-name http-multipart-part-value
          http-multipart-part-filename http-multipart-part-content-type
          http-pool-policy? make-http-pool-policy http-pool-policy-max-idle
          http-pool-policy-max-active http-pool-policy-idle-timeout-ms
          http-client-cookie-jar-set! http-client-auth-set!
          http-client-proxy-set! http-client-pool-policy-set!
          http-client-version-set! make-http-multipart-body
          http-header-ref
          http-header-set
          http-header-add
          http-client?
          http-open
          http-close
          http-send
          http-get
          http-head
          http-post
          http-put
          http-delete
          http-request
          http-download
          http-upload
          http-follow-redirects!
          http-set-header!
          http-set-timeout!
          http-cancel-pending!
          http-send/nonblocking
          http-request/nonblocking
          http-download/nonblocking
          http-upload/nonblocking
          http-server?
          http-listen
          http-server-close
          http-accept
          http-accept/nonblocking
          http-serve
          http-serve-loop
          http-register-handler!
          http-handler-ref
          http-unregister-handler!
          http-connection?
          http-connection-close
          http-read-request
          http-read-request/nonblocking
          http-write-response
          http-write-response/nonblocking)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp string)
          (chezpp file)
          (chezpp net uri)
          (chezpp net errors)
          (chezpp net address)
          (chezpp net socket)
          (chezpp net poll)
          (chezpp net operation)
          (chezpp net ffi)
          (chezpp net private)
          (chezpp net tls)
          (chezpp optional-library))

  ;;===----------------------------------------------------------------------===
  ;; Data Types
  ;;===----------------------------------------------------------------------===

  (define check-backlog
    (lambda (who backlog)
      (when (fx< backlog 0)
        (errorf who "backlog must be non-negative, given ~s" backlog))
      backlog))

  (define-record-type (http-request-record %make-http-request http-request?)
    (sealed #t)
    (opaque #f)
    (fields (immutable method http-request-method)
            (immutable uri http-request-uri)
            (immutable headers http-request-headers)
            (immutable body http-request-body)))

  (define-record-type (http-response-record %make-http-response http-response?)
    (sealed #t)
    (opaque #f)
    (fields (immutable status http-response-status)
            (immutable reason http-response-reason)
            (immutable headers http-response-headers)
            (immutable body http-response-body)
            (immutable trailers http-response-trailers)
            (immutable version http-response-version)))

  (define-record-type (http-body-source %make-http-body-source http-body-source?)
    (sealed #t)
    (opaque #f)
    (fields (immutable producer http-body-source-producer)
            (immutable length http-body-source-length)))

  (define-record-type (http-body-sink %make-http-body-sink http-body-sink?)
    (sealed #t)
    (opaque #f)
    (fields (immutable consumer http-body-sink-consumer)
            (immutable finisher http-body-sink-finisher)
            (mutable finished? http-body-sink-finished? http-body-sink-finished?-set!)))

  (define-record-type (http-cookie %make-http-cookie http-cookie?)
    (sealed #t)
    (opaque #f)
    (fields (immutable name http-cookie-name)
            (immutable value http-cookie-value)
            (immutable domain http-cookie-domain)
            (immutable path http-cookie-path)
            (immutable secure? http-cookie-secure?)))

  (define-record-type (http-cookie-jar %make-http-cookie-jar http-cookie-jar?)
    (sealed #t)
    (opaque #f)
    (fields (mutable cookies http-cookie-jar-cookies http-cookie-jar-cookies-set!)
            (immutable mutex http-cookie-jar-mutex)))

  (define-record-type (http-proxy %make-http-proxy http-proxy?)
    (sealed #t)
    (opaque #f)
    (fields (immutable uri http-proxy-uri)))

  (define-record-type (http-multipart-part %make-http-multipart-part http-multipart-part?)
    (sealed #t)
    (opaque #f)
    (fields (immutable name http-multipart-part-name)
            (immutable value http-multipart-part-value)
            (immutable filename http-multipart-part-filename)
            (immutable content-type http-multipart-part-content-type)))

  (define-record-type (http-pool-policy %make-http-pool-policy http-pool-policy?)
    (sealed #t)
    (opaque #f)
    (fields (immutable max-idle http-pool-policy-max-idle)
            (immutable max-active http-pool-policy-max-active)
            (immutable idle-timeout-ms http-pool-policy-idle-timeout-ms)))

  (define-record-type (http-client %make-http-client http-client?)
    (sealed #t)
    (opaque #f)
    (fields (mutable default-headers http-client-default-headers http-client-default-headers-set!)
            (mutable follow-redirects? http-client-follow-redirects? http-client-follow-redirects?-set!)
            (mutable timeout-ms http-client-timeout-ms http-client-timeout-ms-set!)
            (immutable tls-context http-client-tls-context)
            (mutable cookie-jar http-client-cookie-jar http-client-cookie-jar-set-internal!)
            (mutable auth http-client-auth http-client-auth-set-internal!)
            (mutable proxy http-client-proxy http-client-proxy-set-internal!)
            (mutable pool-policy http-client-pool-policy http-client-pool-policy-set-internal!)
            (mutable version http-client-version http-client-version-set-internal!)
            (mutable cached-origin http-client-cached-origin http-client-cached-origin-set!)
            (mutable cached-connection http-client-cached-connection http-client-cached-connection-set!)
            (mutable pending http-client-pending http-client-pending-set!)
            (mutable closed? http-client-closed? http-client-closed?-set!)))

  (define-record-type (http-server %make-http-server http-server?)
    (sealed #t)
    (opaque #f)
    (fields (immutable socket http-server-socket)
            (immutable host http-server-host)
            (immutable port http-server-port)
            (immutable tls-context http-server-tls-context)
            (mutable handlers http-server-handlers http-server-handlers-set!)
            (mutable operations http-server-operations http-server-operations-set!)
            (mutable closed? http-server-closed? http-server-closed?-set!)
            (immutable close-mutex http-server-close-mutex)))

  (define-record-type (http-connection %make-http-connection http-connection?)
    (sealed #t)
    (opaque #f)
    (fields (immutable socket http-connection-socket)
            (immutable tls-session http-connection-tls-session)
            (immutable deadline-cell http-connection-deadline-cell)
            (immutable input-port http-connection-input-port)
            (immutable output-port http-connection-output-port)
            (immutable secure? http-connection-secure?)
            (mutable idle-since-ms http-connection-idle-since-ms
                     http-connection-idle-since-ms-set!)
            (mutable closed? http-connection-closed? http-connection-closed?-set!)))

  ;;===----------------------------------------------------------------------===
  ;; Helpers
  ;;===----------------------------------------------------------------------===

  (define normalize-http-method
    (lambda (who method)
      (cond
       [(string? method) (string-upcase method)]
       [(symbol? method) (string-upcase (symbol->string method))]
       [else (errorf who "expected HTTP method string or symbol, given ~s" method)])))

  (define normalize-http-uri
    (lambda (who value)
      (cond
       [(uri? value) value]
       [(string? value)
        (or (string->uri value)
            (errorf who "invalid URI string ~s" value))]
       [else
        (errorf who "expected URI object or string, given ~s" value)])))

  (define normalize-http-body
    (lambda (who body)
      (cond
       [(or (not body) (string? body) (bytevector? body) (http-body-source? body)) body]
       [else
        (errorf who "expected body bytes, string, source, or #f, given ~s" body)])))

  (define normalize-http-header-name
    (lambda (who name)
      (cond
       [(string? name) name]
       [(symbol? name) (symbol->string name)]
       [else
        (errorf who "expected header name string or symbol, given ~s" name)])))

  (define normalize-http-headers
    (lambda (who headers)
      (unless (list? headers)
        (errorf who "expected header association list, given ~s" headers))
      (map (lambda (entry)
             (unless (pair? entry)
               (errorf who "expected header pair, given ~s" entry))
             (let ([name (normalize-http-header-name who (car entry))]
                   [value (cdr entry)])
               (unless (string? value)
                 (errorf who "expected header value string, given ~s" value))
               (cons name value)))
           headers)))

  (define normalize-http-status
    (lambda (who status)
      (unless (and (integer? status) (exact? status) (<= 100 status 599))
        (errorf who "expected HTTP status in [100, 599], given ~s" status))
      status))

  (define ensure-client-open
    (lambda (who client)
      (when (http-client-closed? client)
        (raise-net-error who 'http "HTTP client is closed" client))))

  (define ensure-no-pending-mismatch
    (lambda (who client kind args)
      (let ([pending (http-client-pending client)])
        (when (and pending
                   (eq? 'pending (net-operation-state pending))
                   (not (eq? (net-operation-kind pending) kind)))
          (raise-net-error who 'http "another nonblocking HTTP operation is pending" pending)))))

  (define ensure-server-open
    (lambda (who server)
      (when (http-server-closed? server)
        (raise-net-error who 'http "HTTP server is closed" server))))

  (define ensure-connection-open
    (lambda (who conn)
      (when (http-connection-closed? conn)
        (raise-net-error who 'http "HTTP connection is closed" conn))))

  (define http-default-timeout-ms 30000)

  (define check-timeout-ms
    (lambda (who timeout-ms)
      (unless (fixnum? timeout-ms)
        (errorf who "expected timeout fixnum, given ~s" timeout-ms))
      (when (fx< timeout-ms 0)
        (errorf who "timeout must be non-negative, given ~s" timeout-ms))
      timeout-ms))

  (define current-time-ms
    (lambda ()
      (let ([t (current-time 'time-monotonic)])
        (+ (* (time-second t) 1000)
           (quotient (time-nanosecond t) 1000000)))))

  (define timeout->deadline-ms
    (lambda (timeout-ms)
      (and (fx>= timeout-ms 0)
           (+ (current-time-ms) timeout-ms))))

  (define remaining-timeout-ms
    (lambda (deadline-ms)
      (and deadline-ms
           (max 0 (- deadline-ms (current-time-ms))))))

  (define connection-close?
    (lambda (headers)
      (let ([value (http-header-ref headers "Connection" #f)])
        (and value
             (ormap (lambda (part)
                      (string-ci=? (string-trim part) "close"))
                    (string-split value #\,))))))

  (define request-origin-key
    (lambda (request)
      (let* ([u (http-request-uri request)]
             [scheme (or (uri-scheme u) "http")]
             [host (or (uri-host u) "localhost")]
             [port (default-port-for-uri 'request-origin-key u)])
        (list scheme host port))))

  (define http-connection-deadline-ms
    (lambda (conn)
      (vector-ref (http-connection-deadline-cell conn) 0)))

  (define http-connection-deadline-ms-set!
    (lambda (conn deadline-ms)
      (vector-set! (http-connection-deadline-cell conn) 0 deadline-ms)))

  (define cancel-pending!
    (lambda (client pending)
      (net-operation-cancel! pending)
      (http-client-pending-set! client #f)
      client))

  (define bytevector-slice
    (lambda (bv start stop)
      (let ([out (make-bytevector (fx- stop start) 0)])
        (bytevector-copy! bv start out 0 (fx- stop start))
        out)))

  (define append-http-bytevectors
    (lambda (left right)
      (let* ([left-length (bytevector-length left)]
             [right-length (bytevector-length right)]
             [out (make-bytevector (fx+ left-length right-length) 0)])
        (bytevector-copy! left 0 out 0 left-length)
        (bytevector-copy! right 0 out left-length right-length)
        out)))

  (define bytevector-find-crlf
    (lambda (bv start)
      (let ([len (bytevector-length bv)])
        (let loop ([i start])
          (cond
           [(fx>= (fx1+ i) len) #f]
           [(and (fx= (bytevector-u8-ref bv i) 13)
                 (fx= (bytevector-u8-ref bv (fx1+ i)) 10))
            i]
           [else (loop (fx1+ i))])))))

  (define bytevector-find-header-end
    (lambda (bv start)
      (let ([len (bytevector-length bv)])
        (let loop ([i start])
          (cond
           [(fx> (fx+ i 3) (fx1- len)) #f]
           [(and (fx= (bytevector-u8-ref bv i) 13)
                 (fx= (bytevector-u8-ref bv (fx1+ i)) 10)
                 (fx= (bytevector-u8-ref bv (fx+ i 2)) 13)
                 (fx= (bytevector-u8-ref bv (fx+ i 3)) 10))
            i]
           [else (loop (fx1+ i))])))))

  (define serialize-http-request-head
    (lambda (request headers)
      (let-values ([(port get) (open-bytevector-output-port)])
        (put-bytevector
         port
         (string->utf8
          (format "~a ~a HTTP/1.1\r\n"
                  (http-request-method request)
                  (http-uri-target (http-request-uri request)))))
        (write-header-lines port headers)
        (put-bytevector port (string->utf8 "\r\n"))
        (get))))

  (define parse-buffered-headers
    (lambda (who bv start stop)
      (let ([port (open-bytevector-input-port (bytevector-slice bv start stop))])
        (dynamic-wind
          void
          (lambda () (read-http-headers who port))
          (lambda () (close-port port))))))

  (define parse-buffered-chunked-body
    (lambda (who bv start)
      (let loop ([i start] [part* '()] [total 0])
        (let ([line-end (bytevector-find-crlf bv i)])
          (if (not line-end)
              (values #f #f #f #f)
              (let* ([line (utf8->string (bytevector-slice bv i line-end))]
                     [size (parse-chunk-size who line)]
                     [data-start (fx+ line-end 2)]
                     [data-stop (fx+ data-start size)])
                (cond
                 [(fx= size 0)
                  (let ([trailer-end (bytevector-find-header-end bv line-end)])
                    (if (not trailer-end)
                        (values #f #f #f #f)
                        (let ([out (make-bytevector total 0)])
                          (let fill ([rest (reverse part*)] [offset 0])
                            (unless (null? rest)
                              (let ([part (car rest)])
                                (bytevector-copy! part 0 out offset
                                                  (bytevector-length part))
                                (fill (cdr rest)
                                      (fx+ offset (bytevector-length part))))))
                          (values
                           #t out (fx+ trailer-end 4)
                           (if (= trailer-end line-end)
                               '()
                               (parse-buffered-headers
                                who bv (fx+ line-end 2) (fx+ trailer-end 2)))))))]
                 [(fx> (fx+ data-stop 2) (bytevector-length bv))
                  (values #f #f #f #f)]
                 [(or (not (fx= (bytevector-u8-ref bv data-stop) 13))
                      (not (fx= (bytevector-u8-ref bv (fx1+ data-stop)) 10)))
                  (raise-net-error who 'http "invalid HTTP chunk terminator")]
                 [else
                  (loop (fx+ data-stop 2)
                        (cons (bytevector-slice bv data-start data-stop) part*)
                        (fx+ total size))])))))))

  (define http-transfer/nonblocking
    (case-lambda
      [(who client kind request finish)
       (http-transfer/nonblocking who client kind request finish #f)]
      [(who client kind request finish response-sink)
      (ensure-client-open who client)
      (when (zero? (http-pool-policy-max-active (http-client-pool-policy client)))
        (raise-net-error who 'http "HTTP client active connection limit is zero" client))
      (ensure-no-pending-mismatch who client kind (request-key request))
      (let ([pending (http-client-pending client)])
        (if (and pending (eq? 'pending (net-operation-state pending)))
            pending
            (let ([current-request (apply-request-auth who client request #f)]
                  [auth-origin (request-origin-key request)]
                  [redirects-left 5]
                  [deadline-ms
                   (timeout->deadline-ms (http-client-timeout-ms client))]
                  [phase 'resolve]
                  [resolver-handle #f]
                  [resolver-fd #f]
                  [sock #f]
                  [tls-session #f]
                  [connection #f]
                  [connect-operation #f]
                  [tls-operation #f]
                  [request-headers '()]
                  [head (make-bytevector 0 0)]
                  [body (make-bytevector 0 0)]
                  [body-source #f]
                  [body-source-eof? #f]
                  [body-source-count 0]
                  [write-offset 0]
                  [input (make-bytevector 0 0)]
                  [status #f]
                  [reason ""]
                  [response-headers '()]
                  [response-trailers '()]
                  [body-start 0]
                  [response-received 0]
                  [chunk-remaining #f]
                  [chunk-crlf? #f]
                  [owned? #t]
                  [operation #f])
              (define release-transport!
                (lambda ()
                  (when resolver-handle
                    (ffi-net-resolver-close resolver-handle)
                    (set! resolver-handle #f)
                    (set! resolver-fd #f))
                  (when owned?
                    (cond
                     [connection (close-http-connection connection)]
                     [else
                      (when tls-session
                        (guard (failure [else #f])
                          (close-tls-session tls-session)))
                      (when sock
                        (guard (failure [else #f])
                          (close-socket sock)))])
                    (set! connection #f)
                    (set! tls-session #f)
                    (set! sock #f))))
              (define pending-update
                (lambda (resource event*)
                  (net-operation-pending
                   (if resource (list (make-poll-target resource event*)) '())
                   deadline-ms)))
              (define yield-update
                (lambda ()
                  (net-operation-pending '() (current-time-ms))))
              (define check-deadline!
                (lambda ()
                  (when (and deadline-ms (fx>= (current-time-ms) deadline-ms))
                    (raise-http-timeout who "HTTP request timed out" current-request))))
              (define transport-write
                (lambda (bv start)
                  (if tls-session
                      (tls-write/nonblocking tls-session bv start (bytevector-length bv))
                      (socket-send/nonblocking sock bv start (bytevector-length bv)))))
              (define transport-read
                (lambda ()
                  (if tls-session
                      (tls-read/nonblocking tls-session 65536)
                      (socket-recv/nonblocking sock 65536))))
              (define reset-request!
                (lambda (next-request)
                  (release-transport!)
                  (set! owned? #t)
                  (set! current-request next-request)
                  (set! phase 'resolve)
                  (set! resolver-handle #f)
                  (set! resolver-fd #f)
                  (set! connect-operation #f)
                  (set! tls-operation #f)
                  (set! request-headers '())
                  (set! head (make-bytevector 0 0))
                  (set! body (make-bytevector 0 0))
                  (set! body-source #f)
                  (set! body-source-eof? #f)
                  (set! body-source-count 0)
                  (set! write-offset 0)
                  (set! input (make-bytevector 0 0))
                  (set! status #f)
                  (set! reason "")
                  (set! response-headers '())
                  (set! response-trailers '())
                  (set! body-start 0)
                  (set! response-received 0)
                  (set! chunk-remaining #f)
                  (set! chunk-crlf? #f)))
              (define prepare-request-body!
                (lambda ()
                  (let ([request-body (http-request-body current-request)])
                    (if (http-body-source? request-body)
                        (begin
                          (set! body-source request-body)
                          (set! body-source-eof? #f)
                          (set! body-source-count 0)
                          (set! body (make-bytevector 0 0)))
                        (begin
                          (set! body-source #f)
                          (set! body (body->bytevector request-body)))))))
              (define frame-source-chunk
                (lambda (chunk)
                  (if (chunked-transfer? request-headers)
                      (let-values ([(port get) (open-bytevector-output-port)])
                        (put-bytevector
                         port
                         (string->utf8
                          (string-append
                           (number->string (bytevector-length chunk) 16) "\r\n")))
                        (put-bytevector port chunk)
                        (put-bytevector port (string->utf8 "\r\n"))
                        (get))
                      chunk)))
              (define refill-source-body!
                (lambda ()
                  (let ([chunk (http-body-source-read body-source 65536)])
                    (if (eof-object? chunk)
                        (begin
                          (let ([length (http-body-source-length body-source)])
                            (when (and length (not (= length body-source-count)))
                              (errorf who
                                      "HTTP body source produced ~a bytes; expected ~a"
                                      body-source-count length)))
                          (set! body-source-eof? #t)
                          (set! body
                                (if (chunked-transfer? request-headers)
                                    (string->utf8 "0\r\n\r\n")
                                    (make-bytevector 0 0))))
                        (begin
                          (set! body-source-count
                                (+ body-source-count (bytevector-length chunk)))
                          (let ([length (http-body-source-length body-source)])
                            (when (and length (> body-source-count length))
                              (errorf who "HTTP body source exceeded declared length ~a" length)))
                          (set! body (frame-source-chunk chunk))))
                    (set! write-offset 0))))
              (define complete-response
                (lambda (response)
                  (store-response-cookies!
                   client current-request (http-response-headers response))
                  (let ([next-request
                         (and (http-client-follow-redirects? client)
                              (fx> redirects-left 0)
                              (redirect-status? (http-response-status response))
                              (redirect-request current-request response))])
                    (if next-request
                        (begin
                          (set! redirects-left (fx1- redirects-left))
                          (reset-request!
                           (apply-request-auth who client next-request response))
                          (yield-update))
                        (begin
                          (when (reusable-response? request-headers response
                                                    (http-request-method current-request))
                            (unless connection
                              (set! connection
                                    (make-http-connection*
                                     who sock tls-session
                                     (and tls-session #t) deadline-ms)))
                            (if (cache-http-connection!
                                 client (request-origin-key current-request) connection)
                                (set! owned? #f)
                                (begin
                                  (set! connection #f)
                                  (set! tls-session #f)
                                  (set! sock #f))))
                          (net-operation-completed (finish response)))))))
              (define read-pending
                (lambda (allow-io?)
                  (if (not allow-io?)
                      (pending-update sock '(read error hup invalid))
                      (let ([answer (transport-read)])
                        (cond
                         [(net-would-block? answer)
                          (pending-update (net-would-block-resource answer)
                                          (net-would-block-events answer))]
                         [(eof-object? answer)
                          (advance #f)]
                         [else
                          (set! input (append-http-bytevectors input answer))
                          (advance #f)])))))
              (define consume-response-input!
                (lambda (maximum-bytes)
                  (let* ([available (- (bytevector-length input) body-start)]
                         [count (min available maximum-bytes)])
                    (when (positive? count)
                      (http-body-sink-write! response-sink input body-start
                                             (+ body-start count))
                      (set! response-received (+ response-received count))
                      (set! input
                            (bytevector-slice input (+ body-start count)
                                              (bytevector-length input)))
                      (set! body-start 0))
                    count)))
              (define consume-chunked-response!
                (lambda ()
                  (let loop ()
                    (cond
                     [chunk-crlf?
                      (if (< (- (bytevector-length input) body-start) 2)
                          #f
                          (begin
                            (unless (and (= 13 (bytevector-u8-ref input body-start))
                                         (= 10 (bytevector-u8-ref input (+ body-start 1))))
                              (raise-net-error who 'http "invalid HTTP chunk terminator"))
                            (set! input
                                  (bytevector-slice input (+ body-start 2)
                                                    (bytevector-length input)))
                            (set! body-start 0)
                            (set! chunk-crlf? #f)
                            (set! chunk-remaining #f)
                            (loop)))]
                     [(integer? chunk-remaining)
                      (let* ([available (- (bytevector-length input) body-start)]
                             [count (min available chunk-remaining)])
                        (if (zero? count)
                            #f
                            (begin
                              (http-body-sink-write!
                               response-sink input body-start (+ body-start count))
                              (set! response-received (+ response-received count))
                              (set! chunk-remaining (- chunk-remaining count))
                              (set! input
                                    (bytevector-slice input (+ body-start count)
                                                      (bytevector-length input)))
                              (set! body-start 0)
                              (when (zero? chunk-remaining)
                                (set! chunk-crlf? #t))
                              (loop))))]
                     [else
                      (let ([line-end (bytevector-find-crlf input body-start)])
                        (if (not line-end)
                            #f
                            (let ([size
                                   (parse-chunk-size
                                    who
                                    (utf8->string
                                     (bytevector-slice input body-start line-end)))])
                              (set! input
                                    (bytevector-slice input (+ line-end 2)
                                                      (bytevector-length input)))
                              (set! body-start 0)
                              (if (zero? size)
                                  (cond
                                   [(and (>= (bytevector-length input) 2)
                                         (= 13 (bytevector-u8-ref input 0))
                                         (= 10 (bytevector-u8-ref input 1)))
                                    (set! input
                                          (bytevector-slice input 2
                                                            (bytevector-length input)))
                                    (set! response-trailers '())
                                    #t]
                                   [else
                                    (let ([trailer-end
                                           (bytevector-find-header-end input 0)])
                                      (if trailer-end
                                          (begin
                                            (set! response-trailers
                                                  (parse-buffered-headers
                                                   who input 0 (+ trailer-end 2)))
                                            (set! input
                                                  (bytevector-slice
                                                   input (+ trailer-end 4)
                                                   (bytevector-length input)))
                                            #t)
                                          #f))])
                                  (begin
                                    (set! chunk-remaining size)
                                    (loop))))))]))))
              (define advance
                (lambda (allow-io?)
                  (check-deadline!)
                  (case phase
                    [(resolve)
                     (let ([cached (take-http-connection client current-request deadline-ms)])
                       (if cached
                           (begin
                             (set! connection cached)
                             (set! sock (http-connection-socket cached))
                             (set! tls-session (http-connection-tls-session cached))
                             (set! request-headers
                                   (merge-request-headers
                                    current-request client
                                    (equal? auth-origin
                                            (request-origin-key current-request))))
                             (set! head
                                   (serialize-http-request-head current-request request-headers))
                             (prepare-request-body!)
                             (set! phase 'write-head)
                             (yield-update))
                           (if (not resolver-handle)
                               (let* ([u (http-request-uri current-request)]
                                      [host (or (uri-host u) "localhost")]
                                      [port (default-port-for-uri who u)]
                                      [started
                                       (ffi-net-resolver-start
                                        host port 0 (net-sock-stream))])
                                 (when (ffi-error? started)
                                   (raise-net-error who 'http
                                                    "failed to start HTTP resolver" started))
                                 (set! resolver-handle (vector-ref started 0))
                                 (set! resolver-fd (vector-ref started 1))
                                 (pending-update resolver-fd '(read error hup invalid)))
                               (let ([resolved (ffi-net-resolver-poll resolver-handle)])
                                 (cond
                                  [(not resolved)
                                   (pending-update resolver-fd '(read error hup invalid))]
                                  [(ffi-error? resolved)
                                   (raise-net-error who 'http
                                                    "failed to resolve HTTP endpoint" resolved)]
                                  [else
                                   (let ([address (%socket-address-from-ffi resolved)])
                                     (ffi-net-resolver-close resolver-handle)
                                     (set! resolver-handle #f)
                                     (set! resolver-fd #f)
                                     (set! sock
                                           (open-socket
                                            (socket-address-family address) 'stream))
                                     (socket-set-blocking! sock #f)
                                     (set! connect-operation
                                           (socket-connect/nonblocking
                                            sock address
                                            (remaining-timeout-ms deadline-ms)))
                                     (set! phase 'connect)
                                     (yield-update))])))))]
                    [(connect)
                     (net-operation-step! connect-operation)
                     (case (net-operation-state connect-operation)
                       [(pending)
                        (net-operation-pending
                         (net-operation-poll-targets connect-operation)
                         deadline-ms)]
                       [(failed) (raise (net-operation-condition connect-operation))]
                       [(completed)
                        (if (string=? (uri-scheme (http-request-uri current-request)) "https")
                            (begin
                              (set! tls-operation
                                    (tls-connect/nonblocking
                                     (or (http-client-tls-context client)
                                         (make-tls-context 'client))
                                     sock
                                     (uri-host (http-request-uri current-request))
                                     (remaining-timeout-ms deadline-ms)))
                              (set! phase 'tls-handshake)
                              (yield-update))
                            (begin
                              (set! request-headers
                                    (merge-request-headers
                                     current-request client
                                     (equal? auth-origin
                                             (request-origin-key current-request))))
                              (set! head
                                    (serialize-http-request-head
                                     current-request request-headers))
                              (prepare-request-body!)
                              (set! phase 'write-head)
                              (yield-update)))])]
                    [(tls-handshake)
                     (net-operation-step! tls-operation)
                     (case (net-operation-state tls-operation)
                       [(pending)
                        (net-operation-pending
                         (net-operation-poll-targets tls-operation) deadline-ms)]
                       [(failed) (raise (net-operation-condition tls-operation))]
                       [(completed)
                        (set! tls-session (net-operation-result tls-operation))
                        (set! request-headers
                              (merge-request-headers
                               current-request client
                               (equal? auth-origin
                                       (request-origin-key current-request))))
                        (set! head
                              (serialize-http-request-head current-request request-headers))
                        (prepare-request-body!)
                        (set! phase 'write-head)
                        (yield-update)])]
                    [(write-head write-body)
                     (let ([bytes (if (eq? phase 'write-head) head body)])
                       (cond
                        [(fx= write-offset (bytevector-length bytes))
                         (set! write-offset 0)
                         (if (eq? phase 'write-head)
                             (begin
                               (set! phase 'write-body)
                               (pending-update sock '(write error hup invalid)))
                             (cond
                              [(and body-source (not body-source-eof?))
                               (refill-source-body!)
                               (advance #f)]
                              [else
                               (set! phase 'read-status)
                               (pending-update sock '(read error hup invalid))]))]
                        [(not allow-io?)
                         (pending-update sock '(write error hup invalid))]
                        [else
                         (let ([answer (transport-write bytes write-offset)])
                           (if (net-would-block? answer)
                               (pending-update (net-would-block-resource answer)
                                               (net-would-block-events answer))
                               (begin
                                 (set! write-offset (fx+ write-offset answer))
                                 (advance #f))))]))]
                    [(read-status)
                     (let ([line-end (bytevector-find-crlf input 0)])
                       (if line-end
                           (begin
                             (let-values ([(parsed-status parsed-reason)
                                           (parse-response-line
                                            who
                                            (utf8->string
                                             (bytevector-slice input 0 line-end)))])
                               (set! status parsed-status)
                               (set! reason parsed-reason))
                             (set! body-start (fx+ line-end 2))
                             (set! phase 'read-headers)
                             (advance allow-io?))
                           (read-pending allow-io?)))]
                    [(read-headers)
                     (let ([header-end (bytevector-find-header-end input body-start)])
                       (if header-end
                           (begin
                             (set! response-headers
                                   (parse-buffered-headers
                                    who input body-start (fx+ header-end 2)))
                             (set! body-start (fx+ header-end 4))
                             (set! phase 'read-body)
                             (advance allow-io?))
                           (read-pending allow-io?)))]
                    [(read-body)
                     (let ([method (http-request-method current-request)]
                           [content-length (response-body-length response-headers)])
                       (cond
                        [(or (string=? method "HEAD") (= status 204) (= status 304))
                         (complete-response
                          (make-http-response status reason response-headers #f))]
                        [(chunked-transfer? response-headers)
                         (if response-sink
                             (if (consume-chunked-response!)
                                 (begin
                                   (http-body-sink-finish! response-sink)
                                   (complete-response
                                    (%make-http-response
                                     status reason response-headers #f
                                     response-trailers 'http/1.1)))
                                 (read-pending allow-io?))
                             (let-values ([(done? parsed-body consumed trailers)
                                           (parse-buffered-chunked-body
                                            who input body-start)])
                               (if done?
                                   (complete-response
                                    (%make-http-response
                                     status reason response-headers parsed-body
                                     trailers 'http/1.1))
                                   (read-pending allow-io?))))]
                        [content-length
                         (if response-sink
                             (begin
                               (consume-response-input!
                                (- content-length response-received))
                               (if (= response-received content-length)
                                   (begin
                                     (http-body-sink-finish! response-sink)
                                     (complete-response
                                      (make-http-response
                                       status reason response-headers #f)))
                                   (read-pending allow-io?)))
                             (if (fx>= (fx- (bytevector-length input) body-start)
                                      content-length)
                                 (complete-response
                                  (make-http-response
                                   status reason response-headers
                                   (bytevector-slice
                                    input body-start (fx+ body-start content-length))))
                                 (read-pending allow-io?)))]
                        [else
                         (if allow-io?
                             (let ([answer (transport-read)])
                               (cond
                                [(net-would-block? answer)
                                 (pending-update
                                  (net-would-block-resource answer)
                                  (net-would-block-events answer))]
                                [(eof-object? answer)
                                 (if response-sink
                                     (begin
                                       (consume-response-input! (most-positive-fixnum))
                                       (http-body-sink-finish! response-sink)
                                       (complete-response
                                        (make-http-response
                                         status reason response-headers #f)))
                                     (complete-response
                                      (make-http-response
                                       status reason response-headers
                                       (bytevector-slice
                                        input body-start (bytevector-length input)))))]
                                [else
                                 (if response-sink
                                     (begin
                                       (consume-response-input! (most-positive-fixnum))
                                       (http-body-sink-write!
                                        response-sink answer 0 (bytevector-length answer))
                                       (set! response-received
                                             (+ response-received
                                                (bytevector-length answer))))
                                     (set! input (append-http-bytevectors input answer)))
                                 (pending-update sock '(read error hup invalid))]))
                             (pending-update sock '(read error hup invalid)))]))]
                    [else (assert-unreachable)])))
              (set! operation
                    (make-net-operation
                     kind
                     (lambda ()
                       (guard (failure [else (net-operation-failed failure)])
                         (advance #t)))
                     release-transport!
                     (lambda ()
                       (release-transport!)
                       (http-client-pending-set! client #f))))
              (http-client-pending-set! client operation)
              operation)))]))

  (define request-key
    (lambda (request)
      (list (http-request-method request)
            (uri->string (http-request-uri request))
            (http-request-headers request)
            (http-request-body request))))

  (define raise-http-timeout
    (lambda (who detail data)
      (raise-net-error who 'http detail data)))

  (define tls-timeout-condition?
    (lambda (c)
      (and (net-error? c)
           (eq? (net-error-kind c) 'tls)
           (string-contains? (net-error-message c) "timed out"))))

  (define call-with-http-timeout-translation
    (lambda (who thunk)
      (guard (c [else
                 (if (tls-timeout-condition? c)
                     (raise-http-timeout who "HTTP request timed out" c)
                     (raise c))])
        (thunk))))

  (define wait-socket-ready!
    (lambda (who sock event* deadline-ms detail)
      (let* ([timeout-ms (let ([x (remaining-timeout-ms deadline-ms)])
                           (if x x -1))]
             [target (car (poll (list (make-poll-target sock event*)) timeout-ms))]
             [ready (poll-target-ready-events target)])
        (when (null? ready)
          (raise-http-timeout who detail sock))
        ready)))

  (define make-deadline-socket-input-port
    (lambda (who sock deadline-ref)
      (make-custom-binary-input-port
       "chezpp-http-client-input"
       (lambda (bv start count)
         (let ([stop (fx+ start count)])
           (let loop ()
             (let ([n (socket-recv!/nonblocking sock bv start stop)])
               (cond
                [(fixnum? n) n]
                [(eof-object? n) 0]
                [else
                 (wait-socket-ready! who
                                     sock
                                     '(read error hup invalid)
                                     (deadline-ref)
                                     "HTTP request timed out")
                 (loop)])))))
       (lambda () #f)
       (lambda (x) #f)
       (lambda () #t))))

  (define make-deadline-socket-output-port
    (lambda (who sock deadline-ref)
      (make-custom-binary-output-port
       "chezpp-http-client-output"
       (lambda (bv start count)
         (let ([stop (fx+ start count)])
           (let loop ([i start])
             (if (fx= i stop)
                 count
                 (let ([n (socket-send/nonblocking sock bv i stop)])
                   (if n
                       (loop (fx+ i n))
                       (begin
                         (wait-socket-ready! who
                                             sock
                                             '(write error hup invalid)
                                             (deadline-ref)
                                             "HTTP request timed out")
                         (loop i))))))))
       (lambda () #f)
       (lambda (x) #f)
       (lambda () #t))))

  (define make-deadline-tls-input-port
    (lambda (who session deadline-ref)
      (make-custom-binary-input-port
       "chezpp-http-client-tls-input"
       (lambda (bv start count)
         (call-with-http-timeout-translation
          who
          (lambda ()
            (let ([n (tls-read! session
                                bv
                                start
                                (fx+ start count)
                                (let ([x (remaining-timeout-ms (deadline-ref))])
                                  (if x x -1)))])
              (if (eof-object? n) 0 n)))))
       (lambda () #f)
       (lambda (x) #f)
       (lambda () #t))))

  (define make-deadline-tls-output-port
    (lambda (who session deadline-ref)
      (make-custom-binary-output-port
       "chezpp-http-client-tls-output"
       (lambda (bv start count)
         (call-with-http-timeout-translation
          who
          (lambda ()
            (tls-write-all session
                           bv
                           start
                           (fx+ start count)
                           (let ([x (remaining-timeout-ms (deadline-ref))])
                             (if x x -1))))))
       (lambda () #f)
       (lambda (x) #f)
       (lambda () #t))))

  (define body->bytevector
    (lambda (body)
      (cond
       [(not body) (make-bytevector 0 0)]
       [(bytevector? body) body]
       [(string? body) (string->utf8 body)]
       [else (assert-unreachable)])))

  (define default-reason
    (lambda (status)
      (case status
        [(200) "OK"]
        [(201) "Created"]
        [(204) "No Content"]
        [(301) "Moved Permanently"]
        [(302) "Found"]
        [(303) "See Other"]
        [(307) "Temporary Redirect"]
        [(308) "Permanent Redirect"]
        [(400) "Bad Request"]
        [(401) "Unauthorized"]
        [(403) "Forbidden"]
        [(404) "Not Found"]
        [(500) "Internal Server Error"]
        [(502) "Bad Gateway"]
        [(503) "Service Unavailable"]
        [else ""])))

  (define default-port-for-uri
    (lambda (who u)
      (or (uri-port u)
          (cond
           [(string=? (uri-scheme u) "http") 80]
           [(string=? (uri-scheme u) "https") 443]
           [else
            (errorf who "unsupported HTTP scheme ~s" (uri-scheme u))]))))

  (define http-uri-target
    (lambda (u)
      (let ([path (uri-path u)]
            [query (uri-query u)])
        (string-append
         (if (or (not path) (string=? path "")) "/" path)
         (if query (string-append "?" query) "")))))

  (define http-host-header
    (lambda (u)
      (let* ([host (or (uri-host u) "localhost")]
             [port (uri-port u)]
             [default-port (if (string=? (uri-scheme u) "https") 443 80)])
        (if (and port (not (= port default-port)))
            (format "~a:~a" host port)
            host))))

  (define redirect-status?
    (lambda (status)
      (memq status '(301 302 303 307 308))))

  (define response-body-length
    (lambda (headers)
      (let ([value (http-header-ref headers "Content-Length" #f)])
        (and value
             (let ([n (string->number value)])
               (and n (exact? n) (integer? n) (>= n 0) n))))))

  (define header-token-member?
    (lambda (headers name token)
      (let ([value (http-header-ref headers name #f)])
        (and value
             (let ([target (string-downcase token)])
               (let loop ([rest (string-split value #\,)])
                 (and (not (null? rest))
                      (or (string=? (string-trim (string-downcase (car rest))) target)
                          (loop (cdr rest))))))))))

  (define chunked-transfer?
    (lambda (headers)
      (header-token-member? headers "Transfer-Encoding" "chunked")))

  (define parse-chunk-size
    (lambda (who line)
      (let* ([semi (string-search line (string-ref ";" 0))]
             [size-text (string-trim (if semi
                                         (substring line 0 semi)
                                         line))]
             [n (string->number size-text 16)])
        (unless (and n (exact? n) (integer? n) (>= n 0))
          (raise-net-error who 'http "invalid HTTP chunk size" line))
        n)))

  (define normalize-response-body
    (lambda (status method headers body)
      (if (or (string=? method "HEAD")
              (= status 204)
              (= status 304))
          #f
          body)))

  (define header-list-set-many
    (lambda (headers updates)
      (let loop ([rest updates] [out headers])
        (if (null? rest)
            out
            (loop (cdr rest)
                  (http-header-set out (caar rest) (cdar rest)))))))

  (define http-header-remove
    (lambda (headers name)
      (let loop ([rest headers] [out '()])
        (cond
         [(null? rest) (reverse out)]
         [(string-ci=? (caar rest) name)
          (loop (cdr rest) out)]
         [else
          (loop (cdr rest) (cons (car rest) out))]))))

  (define base64-alphabet
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

  (define http-base64-encode
    (lambda (bytes)
      (let* ([length (bytevector-length bytes)]
             [output-length (* 4 (quotient (+ length 2) 3))]
             [output (make-string output-length #\=)])
        (let loop ([input-index 0] [output-index 0])
          (when (< input-index length)
            (let* ([remaining (- length input-index)]
                   [a (bytevector-u8-ref bytes input-index)]
                   [b (if (> remaining 1) (bytevector-u8-ref bytes (+ input-index 1)) 0)]
                   [c (if (> remaining 2) (bytevector-u8-ref bytes (+ input-index 2)) 0)]
                   [bits (bitwise-ior (bitwise-arithmetic-shift a 16)
                                      (bitwise-arithmetic-shift b 8) c)])
              (string-set! output output-index
                           (string-ref base64-alphabet
                                       (bitwise-and (bitwise-arithmetic-shift bits -18) 63)))
              (string-set! output (+ output-index 1)
                           (string-ref base64-alphabet
                                       (bitwise-and (bitwise-arithmetic-shift bits -12) 63)))
              (when (> remaining 1)
                (string-set! output (+ output-index 2)
                             (string-ref base64-alphabet
                                         (bitwise-and (bitwise-arithmetic-shift bits -6) 63))))
              (when (> remaining 2)
                (string-set! output (+ output-index 3)
                             (string-ref base64-alphabet (bitwise-and bits 63))))
              (loop (+ input-index 3) (+ output-index 4)))))
        output)))

  (define cookie-domain-matches?
    (lambda (host domain)
      (or (string-ci=? host domain)
          (and (> (string-length host) (string-length domain))
               (let ([start (- (string-length host) (string-length domain))])
                 (and (string-ci=? domain (substring host start (string-length host)))
                      (char=? #\. (string-ref host (- start 1)))))))))

  (define http-string-prefix?
    (lambda (prefix value)
      (and (<= (string-length prefix) (string-length value))
           (string=? prefix (substring value 0 (string-length prefix))))))

  (define http-string-join
    (lambda (string* separator)
      (if (null? string*)
          ""
          (let loop ([rest (cdr string*)] [answer (car string*)])
            (if (null? rest)
                answer
                (loop (cdr rest) (string-append answer separator (car rest))))))))

  (define request-cookie-header
    (lambda (client request)
      (let ([jar (http-client-cookie-jar client)])
        (and jar
             (with-mutex (http-cookie-jar-mutex jar)
               (let* ([uri (http-request-uri request)]
                      [host (or (uri-host uri) "")]
                      [path (or (uri-path uri) "/")]
                      [secure? (string=? (uri-scheme uri) "https")]
                      [cookie*
                       (filter
                        (lambda (cookie)
                          (and (cookie-domain-matches? host (http-cookie-domain cookie))
                               (http-string-prefix? (http-cookie-path cookie) path)
                               (or (not (http-cookie-secure? cookie)) secure?)))
                        (http-cookie-jar-cookies jar))])
                 (and (pair? cookie*)
                      (http-string-join
                       (map (lambda (cookie)
                              (string-append (http-cookie-name cookie) "="
                                             (http-cookie-value cookie)))
                            cookie*)
                       "; "))))))))

  (define request-authorization-header
    (lambda (client request allowed?)
      (let ([auth (http-client-auth client)])
        (and allowed? auth
             (case (car auth)
               [(basic)
                (string-append
                 "Basic "
                 (http-base64-encode
                  (string->utf8
                   (string-append (car (cdr auth)) ":" (cdr (cdr auth))))))]
               [(bearer) (string-append "Bearer " (cdr auth))]
               [else #f])))))

  (define apply-request-auth
    (lambda (who client request response)
      (let ([auth (http-client-auth client)])
        (if (and auth (procedure? (car auth)))
            (let ([answer ((car auth) request response)])
              (unless (http-request? answer)
                (errorf who "HTTP auth procedure must return a request, given ~s" answer))
              answer)
            request))))

  (define default-cookie-path
    (lambda (request-path)
      (cond
       [(or (not request-path) (string=? request-path "")
            (not (char=? (string-ref request-path 0) #\/)))
        "/"]
       [else
        (let loop ([index (- (string-length request-path) 1)])
          (cond
           [(<= index 0) "/"]
           [(char=? (string-ref request-path index) #\/)
            (substring request-path 0 index)]
           [else (loop (- index 1))]))])))

  (define strip-leading-dot
    (lambda (domain)
      (if (and (positive? (string-length domain))
               (char=? (string-ref domain 0) #\.))
          (substring domain 1 (string-length domain))
          domain)))

  (define parse-set-cookie
    (lambda (request value)
      (let* ([part* (map string-trim (string-split value #\;))]
             [first (and (pair? part*) (car part*))]
             [equals (and first (string-search first #\=))])
        (and equals
             (positive? equals)
             (let* ([uri (http-request-uri request)]
                    [host (string-downcase (or (uri-host uri) ""))]
                    [name (substring first 0 equals)]
                    [cookie-value (substring first (+ equals 1) (string-length first))]
                    [domain host]
                    [path (default-cookie-path (uri-path uri))]
                    [secure? #f])
               (for-each
                (lambda (attribute)
                  (let ([index (string-search attribute #\=)])
                    (if index
                        (let ([key (string-downcase
                                    (string-trim (substring attribute 0 index)))]
                              [attribute-value
                               (string-trim
                                (substring attribute (+ index 1)
                                           (string-length attribute)))])
                          (cond
                           [(string=? key "domain")
                            (set! domain
                                  (string-downcase (strip-leading-dot attribute-value)))]
                           [(and (string=? key "path")
                                 (positive? (string-length attribute-value))
                                 (char=? (string-ref attribute-value 0) #\/))
                            (set! path attribute-value)]))
                        (when (string-ci=? attribute "secure")
                          (set! secure? #t)))))
                (cdr part*))
               (and (cookie-domain-matches? host domain)
                    (%make-http-cookie name cookie-value domain path secure?)))))))

  (define store-response-cookies!
    (lambda (client request headers)
      (let ([jar (http-client-cookie-jar client)])
        (when jar
          (let ([cookie*
                 (fold-right
                  (lambda (header answer)
                    (let ([cookie
                           (and (string-ci=? (car header) "Set-Cookie")
                                (parse-set-cookie request (cdr header)))])
                      (if cookie (cons cookie answer) answer)))
                  '() headers)])
            (when (pair? cookie*)
              (with-mutex (http-cookie-jar-mutex jar)
                (for-each
                 (lambda (cookie)
                   (http-cookie-jar-cookies-set!
                    jar
                    (cons
                     cookie
                     (filter
                      (lambda (old)
                        (not (and (string=? (http-cookie-name old)
                                           (http-cookie-name cookie))
                                  (string=? (http-cookie-domain old)
                                           (http-cookie-domain cookie))
                                  (string=? (http-cookie-path old)
                                           (http-cookie-path cookie)))))
                      (http-cookie-jar-cookies jar)))))
                 cookie*))))))))

  (define merge-request-headers
    (case-lambda
      [(request client) (merge-request-headers request client #t)]
      [(request client allow-authorization?)
      (let* ([body (http-request-body request)]
             [headers (header-list-set-many (http-client-default-headers client)
                                            (http-request-headers request))]
             [headers (if (http-header-ref headers "Host" #f)
                          headers
                          (http-header-set headers "Host"
                                           (http-host-header (http-request-uri request))))]
             [headers (if (http-header-ref headers "Connection" #f)
                          headers
                          (http-header-set headers "Connection" "keep-alive"))]
             [authorization
              (request-authorization-header client request allow-authorization?)]
             [headers (if authorization
                          (http-header-set headers "Authorization" authorization)
                          headers)]
             [cookie (request-cookie-header client request)]
             [headers (if cookie (http-header-set headers "Cookie" cookie) headers)])
        (cond
         [(http-body-source? body)
          (let ([length (http-body-source-length body)])
            (if length
                (http-header-set (http-header-remove headers "Transfer-Encoding")
                                 "Content-Length" (number->string length))
                (http-header-set (http-header-remove headers "Content-Length")
                                 "Transfer-Encoding" "chunked")))]
         [(http-header-ref headers "Content-Length" #f) headers]
         [else
          (http-header-set headers "Content-Length"
                           (number->string (bytevector-length (body->bytevector body))))]))]))

  (define http-u8-list->bytevector
    (lambda (u8*)
      (let ([out (make-bytevector (length u8*) 0)])
        (let loop ([rest u8*] [i 0])
          (unless (null? rest)
            (bytevector-u8-set! out i (car rest))
            (loop (cdr rest) (fx1+ i))))
        out)))

  (define read-http-line
    (lambda (ip)
      (let loop ([rev '()])
        (let ([b (get-u8 ip)])
          (cond
           [(eof-object? b)
            (if (null? rev)
                b
                (utf8->string (http-u8-list->bytevector (reverse rev))))]
           [(= b 10)
            (let ([rev (if (and (pair? rev) (= (car rev) 13))
                           (cdr rev)
                           rev)])
              (utf8->string (http-u8-list->bytevector (reverse rev))))]
           [else
            (loop (cons b rev))])))))

  (define read-http-headers
    (lambda (who ip)
      (let loop ([out '()])
        (let ([line (read-http-line ip)])
          (cond
           [(eof-object? line) (reverse out)]
           [(string=? line "") (reverse out)]
           [else
            (let ([i (string-search line #\:)])
              (unless i
                (errorf who "invalid HTTP header line ~s" line))
              (loop
               (cons (cons (substring line 0 i)
                           (string-trim-left
                            (substring line (fx1+ i) (string-length line))))
                     out)))])))))

  (define read-http-body/exact
    (lambda (who ip n)
      (let ([bv (make-bytevector n 0)])
        (let loop ([i 0])
          (if (fx= i n)
              bv
              (let ([b (get-u8 ip)])
                (when (eof-object? b)
                  (errorf who "unexpected EOF while reading HTTP body"))
                (bytevector-u8-set! bv i b)
                (loop (fx1+ i))))))))

  (define read-http-body/to-eof
    (lambda (ip)
      (let loop ([parts '()] [total 0])
        (let ([chunk (get-bytevector-n ip 4096)])
          (if (eof-object? chunk)
              (let ([out (make-bytevector total 0)])
                (let fill ([rest (reverse parts)] [i 0])
                  (if (null? rest)
                      out
                      (let* ([part (car rest)]
                             [n (bytevector-length part)])
                        (bytevector-copy! part 0 out i n)
                        (fill (cdr rest) (fx+ i n))))))
              (let ([n (bytevector-length chunk)])
                (loop (cons chunk parts) (fx+ total n))))))))

  (define read-http-body/chunked
    (lambda (who ip)
      (let loop ([parts '()] [total 0])
        (let ([line (read-http-line ip)])
          (when (eof-object? line)
            (raise-net-error who 'http "unexpected EOF while reading HTTP chunk size"))
          (let ([size (parse-chunk-size who line)])
            (if (= size 0)
                (begin
                  (let trailer-loop ()
                    (let ([trailer (read-http-line ip)])
                      (when (eof-object? trailer)
                        (raise-net-error who 'http "unexpected EOF while reading HTTP trailers"))
                      (unless (string=? trailer "")
                        (trailer-loop))))
                  (let ([out (make-bytevector total 0)])
                    (let fill ([rest (reverse parts)] [i 0])
                      (if (null? rest)
                          out
                          (let* ([part (car rest)]
                                 [n (bytevector-length part)])
                            (bytevector-copy! part 0 out i n)
                            (fill (cdr rest) (fx+ i n)))))))
                (let* ([chunk (read-http-body/exact who ip size)]
                       [crlf (read-http-line ip)])
                  (unless (string=? crlf "")
                    (raise-net-error who 'http "invalid HTTP chunk terminator" crlf))
                  (loop (cons chunk parts) (fx+ total size)))))))))

  (define parse-response-line
    (lambda (who line)
      (let ([parts (string-split line #\space)])
        (unless (>= (length parts) 2)
          (errorf who "invalid HTTP response line ~s" line))
        (let ([status (string->number (cadr parts))]
              [reason (if (>= (length parts) 3)
                          (substring line
                                     (+ (string-length (car parts))
                                        (string-length (cadr parts))
                                        2)
                                     (string-length line))
                          "")])
          (unless status
            (errorf who "invalid HTTP response line ~s" line))
          (values status reason)))))

  (define parse-request-line
    (lambda (who line)
      (let ([parts (string-split line #\space)])
        (unless (= (length parts) 3)
          (errorf who "invalid HTTP request line ~s" line))
        (values (car parts) (cadr parts) (caddr parts)))))

  (define request-target->uri
    (lambda (who conn target headers)
      (cond
       [(string-contains? target "://")
        (normalize-http-uri who target)]
       [else
        (let* ([host (or (http-header-ref headers "Host" #f) "localhost")]
               [scheme (if (http-connection-secure? conn) "https" "http")])
          (normalize-http-uri who (string-append scheme "://" host target)))])))

  (define write-header-lines
    (lambda (op headers)
      (for-each
       (lambda (entry)
         (put-bytevector op
                         (string->utf8
                          (format "~a: ~a\r\n" (car entry) (cdr entry)))))
       headers)))

  (define write-request-port
    (lambda (op request headers)
      (let ([body (body->bytevector (http-request-body request))])
        (put-bytevector op
                        (string->utf8
                         (format "~a ~a HTTP/1.1\r\n"
                                 (http-request-method request)
                                 (http-uri-target (http-request-uri request)))))
        (write-header-lines op headers)
        (put-bytevector op (string->utf8 "\r\n"))
        (unless (fx= 0 (bytevector-length body))
          (put-bytevector op body))
        (flush-output-port op))))

  (define ensure-response-headers
    (lambda (response)
      (let* ([body (body->bytevector (http-response-body response))]
             [headers (if (http-header-ref (http-response-headers response) "Connection" #f)
                          (http-response-headers response)
                          (http-header-set (http-response-headers response)
                                           "Connection"
                                           "close"))])
        (cond
         [(chunked-transfer? headers)
          (http-header-remove headers "Content-Length")]
         [(http-header-ref headers "Content-Length" #f)
          headers]
         [else
          (http-header-set headers "Content-Length"
                           (number->string (bytevector-length body)))]))))

  (define write-http-body/chunked
    (lambda (op body)
      (let ([len (bytevector-length body)])
        (unless (fx= len 0)
          (put-bytevector op
                          (string->utf8
                           (string-append (number->string len 16) "\r\n")))
          (put-bytevector op body)
          (put-bytevector op (string->utf8 "\r\n")))
        (put-bytevector op (string->utf8 "0\r\n\r\n")))))

  (define write-response-port
    (lambda (op response)
      (let ([headers (ensure-response-headers response)]
            [body (body->bytevector (http-response-body response))])
        (put-bytevector op
                        (string->utf8
                         (format "HTTP/1.1 ~a ~a\r\n"
                                 (http-response-status response)
                                 (http-response-reason response))))
        (write-header-lines op headers)
        (put-bytevector op (string->utf8 "\r\n"))
        (if (chunked-transfer? headers)
            (write-http-body/chunked op body)
            (unless (fx= 0 (bytevector-length body))
              (put-bytevector op body)))
        (flush-output-port op))))

  (define read-http-response*
    (lambda (who ip method)
      (let ([line (read-http-line ip)])
        (when (eof-object? line)
          (errorf who "unexpected EOF while reading HTTP response"))
        (let-values ([(status reason) (parse-response-line who line)])
          (let* ([headers (read-http-headers who ip)]
                 [content-length (response-body-length headers)]
                 [body (cond
                        [(chunked-transfer? headers)
                         (read-http-body/chunked who ip)]
                        [content-length
                         (read-http-body/exact who ip content-length)]
                        [else
                         (read-http-body/to-eof ip)])])
            (make-http-response status
                                reason
                                headers
                                (normalize-response-body status method headers body)))))))

  (define make-http-connection*
    (lambda (who sock tls-session secure? deadline-ms)
      (let ([deadline-cell (vector deadline-ms)])
        (%make-http-connection sock
                               tls-session
                               deadline-cell
                               (if tls-session
                                   (make-deadline-tls-input-port
                                    who
                                    tls-session
                                    (lambda () (vector-ref deadline-cell 0)))
                                   (make-deadline-socket-input-port
                                    who
                                    sock
                                    (lambda () (vector-ref deadline-cell 0))))
                               (if tls-session
                                   (make-deadline-tls-output-port
                                    who
                                    tls-session
                                    (lambda () (vector-ref deadline-cell 0)))
                                   (make-deadline-socket-output-port
                                    who
                                    sock
                                    (lambda () (vector-ref deadline-cell 0))))
                               secure?
                               (current-time-ms)
                               #f))))

  (define cache-http-connection!
    (lambda (client origin conn)
      (if (zero? (http-pool-policy-max-idle (http-client-pool-policy client)))
          (begin
            (close-http-connection conn)
            #f)
          (begin
            (let ([old (http-client-cached-connection client)])
              (when (and old (not (eq? old conn)))
                (close-http-connection old)))
            (http-connection-idle-since-ms-set! conn (current-time-ms))
            (http-client-cached-origin-set! client origin)
            (http-client-cached-connection-set! client conn)
            conn))))

  (define uncache-http-connection!
    (lambda (client conn)
      (when (eq? (http-client-cached-connection client) conn)
        (http-client-cached-origin-set! client #f)
        (http-client-cached-connection-set! client #f))))

  (define reusable-http-connection-stale?
    (lambda (conn)
      (let* ([ready (poll/nonblocking
                     (list (make-poll-target (http-connection-socket conn)
                                             '(read hup error invalid))))]
             [events (poll-target-ready-events (car ready))])
        (or (memq 'read events)
            (memq 'hup events)
            (memq 'error events)
            (memq 'invalid events)))))

  (define take-http-connection
    (lambda (client request deadline-ms)
      (let* ([origin (request-origin-key request)]
             [cached-origin (http-client-cached-origin client)]
             [cached-conn (http-client-cached-connection client)]
             [idle-timeout-ms
              (http-pool-policy-idle-timeout-ms (http-client-pool-policy client))]
             [expired?
              (and cached-conn
                   (>= (- (current-time-ms) (http-connection-idle-since-ms cached-conn))
                       idle-timeout-ms))])
        (cond
         [(and cached-conn
               (not expired?)
               (equal? cached-origin origin)
               (not (http-connection-closed? cached-conn))
               (not (reusable-http-connection-stale? cached-conn)))
          (http-client-cached-origin-set! client #f)
          (http-client-cached-connection-set! client #f)
          (http-connection-deadline-ms-set! cached-conn deadline-ms)
          cached-conn]
         [else
          (when (and cached-conn
                     (or expired?
                         (http-connection-closed? cached-conn)
                         (reusable-http-connection-stale? cached-conn)))
            (uncache-http-connection! client cached-conn)
            (close-http-connection cached-conn))
          #f]))))

  (define reusable-response?
    (lambda (request-headers response method)
      (let ([status (http-response-status response)]
            [body (http-response-body response)])
        (and (not (connection-close? request-headers))
             (not (connection-close? (http-response-headers response)))
             (or (string=? method "HEAD")
                 (= status 204)
                 (= status 304)
                 (http-header-ref (http-response-headers response) "Content-Length" #f)
                 (chunked-transfer? (http-response-headers response))
                 (and (bytevector? body)
                      (fx= 0 (bytevector-length body))))))))

  (define open-http-connection
    (lambda (who client request deadline-ms)
      (or (take-http-connection client request deadline-ms)
          (let* ([u (http-request-uri request)]
                 [host (or (uri-host u) "localhost")]
                 [port (default-port-for-uri who u)]
                 [address (or (resolve-address host port #f 'stream)
                              (raise-net-error who 'http "failed to resolve HTTP endpoint" u))]
                 [sock (open-socket (socket-address-family address) 'stream)]
                 [session #f])
            (guard (c [else
                       (when session
                         (guard (x [else #f])
                           (close-tls-session session)))
                       (guard (x [else #f])
                         (close-socket sock))
                       (raise c)])
              (socket-set-blocking! sock #f)
              (unless (socket-connect! sock address)
                (let ([ready (wait-socket-ready! who
                                                 sock
                                                 '(write error hup invalid)
                                                 deadline-ms
                                                 "HTTP request timed out")])
                  (when (and (memq 'error ready) (not (memq 'write ready)))
                    (raise-net-error who 'http "HTTP connect failed" address))
                  (when (memq 'invalid ready)
                    (raise-net-error who 'http "HTTP connect failed" address))))
              (if (string=? (uri-scheme u) "https")
                  (let ([ctx (or (http-client-tls-context client)
                                 (make-tls-context 'client))])
                    (set! session
                          (call-with-http-timeout-translation
                           who
                           (lambda ()
                             (tls-connect ctx
                                          sock
                                          host
                                          (let ([x (remaining-timeout-ms deadline-ms)])
                                            (if x x -1))))))
                    (make-http-connection* who sock session #t deadline-ms))
                  (make-http-connection* who sock #f #f deadline-ms)))))))

  (define close-http-connection
    (lambda (conn)
      (unless (http-connection-closed? conn)
        (guard (c [else #f])
          (close-port (http-connection-input-port conn)))
        (guard (c [else #f])
          (close-port (http-connection-output-port conn)))
        (when (http-connection-tls-session conn)
          (guard (c [else #f])
            (close-tls-session (http-connection-tls-session conn))))
        (guard (c [else #f])
          (close-socket (http-connection-socket conn)))
        (http-connection-closed?-set! conn #t))))

  (define server-prepare-response
    (lambda (request response)
      (let* ([headers (http-response-headers response)]
             [close? (or (connection-close? (http-request-headers request))
                         (connection-close? headers))]
             [headers (if (http-header-ref headers "Connection" #f)
                          headers
                          (http-header-set headers
                                           "Connection"
                                           (if close? "close" "keep-alive")))])
        (values close?
                (make-http-response (http-response-status response)
                                    (http-response-reason response)
                                    headers
                                    (http-response-body response))))))

  (define redirect-request
    (lambda (request response)
      (let ([location (http-header-ref (http-response-headers response) "Location" #f)])
        (and location
             (let* ([ref (normalize-http-uri 'redirect-request location)]
                    [next-uri (uri-resolve (http-request-uri request) ref)]
                    [status (http-response-status response)])
               (if (memq status '(301 302 303))
                   (make-http-request 'get next-uri (http-request-headers request) #f)
                   (make-http-request (http-request-method request)
                                      next-uri
                                      (http-request-headers request)
                                      (http-request-body request))))))))

  (define cache-connection-allowed?
    (lambda (client pending)
      (not (http-client-closed? client))))

  (define http-send*
    (case-lambda
      [(who client request redirects-left deadline-ms)
       (http-send* who client request redirects-left deadline-ms #f)]
      [(who client request redirects-left deadline-ms pending)
      (let ([conn (open-http-connection who client request deadline-ms)]
            [keep-open? #f]
            [origin (request-origin-key request)])
        (dynamic-wind
          void
          (lambda ()
            (http-connection-deadline-ms-set! conn deadline-ms)
            (uncache-http-connection! client conn)
            (let ([request-headers (merge-request-headers request client)])
              (write-request-port (http-connection-output-port conn)
                                  request
                                  request-headers)
              (let ([response (read-http-response* who
                                                   (http-connection-input-port conn)
                                                   (http-request-method request))])
                (set! keep-open?
                      (and (reusable-response? request-headers
                                               response
                                               (http-request-method request))
                           (cache-connection-allowed? client pending)
                           (cache-http-connection! client origin conn)))
                (if (and (http-client-follow-redirects? client)
                         (> redirects-left 0)
                         (redirect-status? (http-response-status response)))
                    (let ([next-request (redirect-request request response)])
                      (if next-request
                          (http-send* who client next-request (fx1- redirects-left) deadline-ms pending)
                          response))
                    response))))
          (lambda ()
            (unless keep-open?
              (close-http-connection conn)))))]))

  (define make-handler-key
    (case-lambda
      [(path) (if (string=? path "") "/" path)]
      [(method path)
       (cons (normalize-http-method 'make-handler-key method)
             (if (string=? path "") "/" path))]))

  (define lookup-handler
    (lambda (server request)
      (let* ([path (or (uri-path (http-request-uri request)) "/")]
             [path (if (string=? path "") "/" path)]
             [method-key (make-handler-key (http-request-method request) path)])
        (or (hashtable-ref (http-server-handlers server) method-key #f)
            (hashtable-ref (http-server-handlers server) path #f)))))

  (define default-handler
    (lambda (request)
      (make-http-response 404
                          "Not Found"
                          '(("Content-Type" . "text/plain"))
                          "not found")))

  (define make-server-connection
    (lambda (sock tls-context)
      (if tls-context
          (let ([session #f])
            (guard (c [else
                       (when session
                         (guard (x [else #f])
                           (close-tls-session session)))
                       (guard (x [else #f])
                         (close-socket sock))
                       (raise c)])
              (set! session (tls-accept tls-context sock))
              (%make-http-connection sock
                                     session
                                     (vector #f)
                                     (open-tls-input-port session)
                                     (open-tls-output-port session)
                                     #t
                                     (current-time-ms)
                                     #f)))
          (%make-http-connection sock
                                 #f
                                 (vector #f)
                                 (open-socket-input-port sock)
                                 (open-socket-output-port sock)
                                 #f
                                 (current-time-ms)
                                 #f))))

  (define serve-http-connection
    (lambda (who server conn)
      (dynamic-wind
        void
        (lambda ()
          (let loop ()
            (let ([request (guard (c [(and (net-error? c)
                                           (string=? (net-error-message c)
                                                     "unexpected EOF while reading HTTP request"))
                                      #f]
                                  [else (raise c)])
                             (http-read-request conn))])
              (when request
                (let* ([handler (or (lookup-handler server request)
                                    default-handler)]
                       [response (handler request)])
                  (unless (http-response? response)
                    (errorf who "HTTP handler must return an HTTP response, given ~s"
                            response))
                  (let-values ([(close? prepared)
                                (server-prepare-response request response)])
                    (http-write-response conn prepared)
                    (unless close?
                      (loop))))))))
        (lambda ()
          (http-connection-close conn)))))

  (define serialize-http-response
    (lambda (response)
      (let-values ([(port get) (open-bytevector-output-port)])
        (write-response-port port response)
        (get))))

  (define make-incremental-server-operation
    (lambda (who server sock)
      (socket-set-blocking! sock #f)
      (let ([tls-session #f]
            [tls-operation #f]
            [phase (if (http-server-tls-context server) 'tls-handshake 'read-request)]
            [deadline-ms (+ (current-time-ms) http-default-timeout-ms)]
            [input (make-bytevector 0 0)]
            [method #f]
            [target #f]
            [request-headers '()]
            [body-start 0]
            [request-stop 0]
            [response-bytes (make-bytevector 0 0)]
            [write-offset 0]
            [close-after-write? #f])
        (define release!
          (lambda ()
            (when (and tls-operation
                       (eq? 'pending (net-operation-state tls-operation)))
              (net-operation-cancel! tls-operation))
            (when tls-session
              (guard (failure [else #f])
                (close-tls-session tls-session))
              (set! tls-session #f))
            (guard (failure [else #f])
              (close-socket sock))))
        (define pending-update
          (lambda (event*)
            (net-operation-pending
             (list (make-poll-target sock event*)) deadline-ms)))
        (define yield-update
          (lambda ()
            (net-operation-pending '() (current-time-ms))))
        (define transport-read
          (lambda ()
            (if tls-session
                (tls-read/nonblocking tls-session 65536)
                (socket-recv/nonblocking sock 65536))))
        (define transport-write
          (lambda ()
            (if tls-session
                (tls-write/nonblocking
                 tls-session response-bytes write-offset
                 (bytevector-length response-bytes))
                (socket-send/nonblocking
                 sock response-bytes write-offset
                 (bytevector-length response-bytes)))))
        (define read-more
          (lambda (allow-io? eof-allowed?)
            (if (not allow-io?)
                (pending-update '(read error hup invalid))
                (let ([answer (transport-read)])
                  (cond
                   [(net-would-block? answer)
                    (net-operation-pending
                     (list
                      (make-poll-target
                       (net-would-block-resource answer)
                       (net-would-block-events answer)))
                     deadline-ms)]
                   [(eof-object? answer)
                    (if eof-allowed?
                        (net-operation-completed #t)
                        (raise-net-error
                         who 'http "unexpected EOF while reading HTTP request"))]
                   [else
                    (set! input (append-http-bytevectors input answer))
                    (advance #f)])))))
        (define request-uri
          (lambda ()
            (if (string-contains? target "://")
                (normalize-http-uri who target)
                (normalize-http-uri
                 who
                 (string-append
                  (if tls-session "https://" "http://")
                  (or (http-header-ref request-headers "Host" #f) "localhost")
                  target)))))
        (define prepare-response!
          (lambda (request)
            (let* ([handler (or (lookup-handler server request) default-handler)]
                   [response (handler request)])
              (unless (http-response? response)
                (errorf who "HTTP handler must return an HTTP response, given ~s"
                        response))
              (let-values ([(close? prepared)
                            (server-prepare-response request response)])
                (set! close-after-write? close?)
                (set! response-bytes (serialize-http-response prepared))
                (set! write-offset 0)
                (set! phase 'write-response)
                (pending-update '(write error hup invalid))))))
        (define finish-request!
          (lambda (body)
            (let ([request (make-http-request method (request-uri) request-headers body)])
              (set! input
                    (bytevector-slice input request-stop (bytevector-length input)))
              (prepare-response! request))))
        (define reset-for-next-request!
          (lambda ()
            (set! deadline-ms (+ (current-time-ms) http-default-timeout-ms))
            (set! method #f)
            (set! target #f)
            (set! request-headers '())
            (set! body-start 0)
            (set! request-stop 0)
            (set! response-bytes (make-bytevector 0 0))
            (set! write-offset 0)
            (set! close-after-write? #f)
            (set! phase 'read-request)))
        (define advance
          (lambda (allow-io?)
            (when (fx>= (current-time-ms) deadline-ms)
              (raise-http-timeout who "HTTP connection timed out" sock))
            (case phase
              [(tls-handshake)
               (unless tls-operation
                 (set! tls-operation
                       (tls-accept/nonblocking
                        (http-server-tls-context server)
                        sock
                        http-default-timeout-ms)))
               (net-operation-step! tls-operation)
               (case (net-operation-state tls-operation)
                 [(pending)
                  (net-operation-pending
                   (net-operation-poll-targets tls-operation) deadline-ms)]
                 [(failed) (raise (net-operation-condition tls-operation))]
                 [(completed)
                  (set! tls-session (net-operation-result tls-operation))
                  (set! phase 'read-request)
                  (yield-update)])]
              [(read-request)
               (let ([line-end (bytevector-find-crlf input 0)])
                 (if line-end
                     (begin
                       (let-values ([(parsed-method parsed-target version)
                                     (parse-request-line
                                      who
                                      (utf8->string
                                       (bytevector-slice input 0 line-end)))])
                         (set! method parsed-method)
                         (set! target parsed-target))
                       (set! body-start (fx+ line-end 2))
                       (set! phase 'read-headers)
                       (advance allow-io?))
                     (read-more allow-io? #t)))]
              [(read-headers)
               (let ([header-end (bytevector-find-header-end input body-start)])
                 (if header-end
                     (begin
                       (set! request-headers
                             (parse-buffered-headers
                              who input body-start (fx+ header-end 2)))
                       (set! body-start (fx+ header-end 4))
                       (set! phase 'read-body)
                       (advance allow-io?))
                     (read-more allow-io? #f)))]
              [(read-body)
               (let ([content-length (response-body-length request-headers)])
                 (cond
                  [(chunked-transfer? request-headers)
                   (let-values ([(done? body consumed trailers)
                                 (parse-buffered-chunked-body who input body-start)])
                     (if done?
                         (begin
                           (set! request-stop consumed)
                           (finish-request! body))
                         (read-more allow-io? #f)))]
                  [content-length
                   (if (fx>= (fx- (bytevector-length input) body-start) content-length)
                       (begin
                         (set! request-stop (fx+ body-start content-length))
                         (finish-request!
                          (if (fx= content-length 0)
                              #f
                              (bytevector-slice input body-start request-stop))))
                       (read-more allow-io? #f))]
                  [else
                   (set! request-stop body-start)
                   (finish-request! #f)]))]
              [(write-response)
               (cond
                [(fx= write-offset (bytevector-length response-bytes))
                 (if close-after-write?
                     (net-operation-completed #t)
                     (begin
                       (reset-for-next-request!)
                       (advance #f)))]
                [(not allow-io?) (pending-update '(write error hup invalid))]
                [else
                 (let ([answer (transport-write)])
                   (if (net-would-block? answer)
                       (net-operation-pending
                        (list
                         (make-poll-target
                          (net-would-block-resource answer)
                          (net-would-block-events answer)))
                        deadline-ms)
                       (begin
                         (set! write-offset (fx+ write-offset answer))
                         (advance #f))))])]
              [else (assert-unreachable)])))
        (make-net-operation
         'http-server-connection
         (lambda ()
           (guard (failure [else (net-operation-failed failure)])
             (advance #t)))
         release!
         release!))))

  ;;===----------------------------------------------------------------------===
  ;; Data Model API
  ;;===----------------------------------------------------------------------===

  #|proc:make-http-body-source
The `make-http-body-source` procedure creates a streaming body from `producer` and `length`.
`producer` has signature `(maximum-bytes) -> bytevector-or-eof` and must not exceed the maximum.
`length` is the exact byte count or `#f` when unknown. The return value is a body source.
|#
  (define make-http-body-source
    (lambda (producer length)
      (pcheck ([procedure? producer]
               [(lambda (value) (or (not value) (natural? value))) length])
        (%make-http-body-source producer length))))

  #|proc:http-body-source-read
The `http-body-source-read` procedure asks `source` for at most `maximum-bytes` bytes.
The return value is a nonempty bytevector or an EOF object.
|#
  (define http-body-source-read
    (lambda (source maximum-bytes)
      (pcheck ([http-body-source? source] [positive? maximum-bytes])
        (let ([answer ((http-body-source-producer source) maximum-bytes)])
          (unless (or (eof-object? answer)
                      (and (bytevector? answer)
                           (positive? (bytevector-length answer))
                           (<= (bytevector-length answer) maximum-bytes)))
            (errorf 'http-body-source-read
                    "producer must return EOF or 1 through ~a bytes, given ~s"
                    maximum-bytes answer))
          answer))))

  #|proc:make-http-body-sink
The `make-http-body-sink` procedure creates a streaming sink from `consumer` and optional `finish`.
`consumer` has signature `(bytevector start stop) -> unspecified`; `finish` has signature
`() -> unspecified`. The return value is a body sink.
|#
  (define make-http-body-sink
    (case-lambda
      [(consumer) (make-http-body-sink consumer void)]
      [(consumer finish)
       (pcheck ([procedure? consumer finish])
         (%make-http-body-sink consumer finish #f))]))

  #|proc:http-body-sink-write!
The `http-body-sink-write!` procedure passes bytevector `bytes` from `start` to `stop` to `sink`.
The return value is unspecified. Writing after the sink finishes raises an exception.
|#
  (define http-body-sink-write!
    (lambda (sink bytes start stop)
      (pcheck ([http-body-sink? sink] [bytevector? bytes]
               [natural? start stop])
        (when (http-body-sink-finished? sink)
          (errorf 'http-body-sink-write! "body sink is already finished"))
        (unless (<= start stop (bytevector-length bytes))
          (errorf 'http-body-sink-write! "invalid bytevector slice [~a, ~a)" start stop))
        ((http-body-sink-consumer sink) bytes start stop))))

  #|proc:http-body-sink-finish!
The `http-body-sink-finish!` procedure completes `sink` and runs its finisher at most once.
The return value is unspecified.
|#
  (define http-body-sink-finish!
    (lambda (sink)
      (pcheck ([http-body-sink? sink])
        (unless (http-body-sink-finished? sink)
          ((http-body-sink-finisher sink))
          (http-body-sink-finished?-set! sink #t)))))

  #|proc:make-http-port-body-source
The `make-http-port-body-source` procedure streams from caller-owned binary input `port`.
`length` is the exact byte count or `#f`. The returned source never closes `port`.
|#
  (define make-http-port-body-source
    (lambda (port length)
      (pcheck ([input-port? port]
               [(lambda (value) (or (not value) (natural? value))) length])
        (make-http-body-source
         (lambda (maximum-bytes) (get-bytevector-n port maximum-bytes))
         length))))

  #|proc:make-http-file-body-source
The `make-http-file-body-source` procedure streams pathname `path` through an owned input port.
The returned source reports the file length and closes its port at EOF.
|#
  (define make-http-file-body-source
    (lambda (path)
      (pcheck ([string? path])
        (let ([port (open-file-input-port path)] [closed? #f])
          (make-http-body-source
           (lambda (maximum-bytes)
             (let ([answer (get-bytevector-n port maximum-bytes)])
               (when (and (eof-object? answer) (not closed?))
                 (close-port port)
                 (set! closed? #t))
               answer))
           (file-size path))))))

  #|proc:make-http-port-body-sink
The `make-http-port-body-sink` procedure streams bytes to caller-owned binary output `port`.
The returned sink flushes but never closes `port` when finished.
|#
  (define make-http-port-body-sink
    (lambda (port)
      (pcheck ([output-port? port])
        (make-http-body-sink
         (lambda (bytes start stop) (put-bytevector port bytes start (- stop start)))
         (lambda () (flush-output-port port))))))

  #|proc:make-http-file-body-sink
The `make-http-file-body-sink` procedure streams to pathname `path` through an owned output port.
The returned sink closes its port when finished.
|#
  (define make-http-file-body-sink
    (lambda (path)
      (pcheck ([string? path])
        (let ([port (open-file-output-port path (file-options no-fail replace))])
          (make-http-body-sink
           (lambda (bytes start stop) (put-bytevector port bytes start (- stop start)))
           (lambda () (close-port port)))))))

  #|proc:make-http-request
The `make-http-request` procedure constructs an HTTP request record from a method, URI, headers, and optional body.
|#
  (define-who make-http-request
    (case-lambda
      [(method uri)
       (make-http-request method uri '() #f)]
      [(method uri headers)
       (make-http-request method uri headers #f)]
      [(method uri headers body)
       (%make-http-request (normalize-http-method who method)
                           (normalize-http-uri who uri)
                           (normalize-http-headers who headers)
                           (normalize-http-body who body))]))

  #|proc:make-http-response
The `make-http-response` procedure constructs an HTTP response record from a status, reason, headers, and optional body.
|#
  (define-who make-http-response
    (case-lambda
      [(status)
       (make-http-response status (default-reason status) '() #f)]
      [(status reason)
       (make-http-response status reason '() #f)]
      [(status reason headers)
       (make-http-response status reason headers #f)]
      [(status reason headers body)
       (pcheck ([string? reason])
               (%make-http-response (normalize-http-status who status)
                                    reason
                                    (normalize-http-headers who headers)
                                    (normalize-http-body who body)
                                    '()
                                    'http/1.1))]))

  #|proc:http-header-ref
The `http-header-ref` procedure returns the first matching header value using case-insensitive name comparison.
|#
  (define-who http-header-ref
    (case-lambda
      [(headers name)
       (http-header-ref headers name #f)]
      [(headers name default)
       (let ([headers (normalize-http-headers who headers)]
             [name (normalize-http-header-name who name)])
         (let loop ([rest headers])
           (cond
            [(null? rest) default]
            [(string-ci=? (caar rest) name) (cdar rest)]
            [else (loop (cdr rest))])))]))

  #|proc:http-header-set
The `http-header-set` procedure returns a header list with a single value for the named header.
|#
  (define-who http-header-set
    (lambda (headers name value)
      (pcheck ([string? value])
              (let ([headers (normalize-http-headers who headers)]
                    [name (normalize-http-header-name who name)])
                (let loop ([rest headers] [out '()] [seen? #f])
                  (cond
                   [(null? rest)
                    (reverse (cons (cons name value) out))]
                   [(string-ci=? (caar rest) name)
                    (if seen?
                        (loop (cdr rest) out seen?)
                        (loop (cdr rest) (cons (cons name value) out) #t))]
                   [else
                    (loop (cdr rest) (cons (car rest) out) seen?)]))))))

  #|proc:http-header-add
The `http-header-add` procedure returns a header list with an additional value appended for the named header.
|#
  (define-who http-header-add
    (lambda (headers name value)
      (pcheck ([string? value])
              (append (normalize-http-headers who headers)
                      (list (cons (normalize-http-header-name who name) value))))))

  ;;===----------------------------------------------------------------------===
  ;; Client API
  ;;===----------------------------------------------------------------------===

  #|proc:make-http-cookie
The `make-http-cookie` procedure creates a cookie named `name` with string `value`.
`domain` and `path` scope the cookie, and `secure?` restricts it to HTTPS requests.
The return value is a new HTTP cookie record.
|#
  (define make-http-cookie
    (lambda (name value domain path secure?)
      (pcheck ([string? name value domain path] [boolean? secure?])
        (%make-http-cookie name value (string-downcase domain) path secure?))))

  #|proc:make-http-cookie-jar
The `make-http-cookie-jar` procedure creates a synchronized jar containing optional `cookie*`.
`cookie*` is a list of HTTP cookie records. The return value is a new cookie jar.
|#
  (define make-http-cookie-jar
    (case-lambda
      [() (make-http-cookie-jar '())]
      [(cookie*)
       (pcheck ([list? cookie*])
         (unless (andmap http-cookie? cookie*)
           (errorf 'make-http-cookie-jar "expected a list of HTTP cookies"))
         (%make-http-cookie-jar cookie* (make-mutex)))]))

  #|proc:make-http-proxy
The `make-http-proxy` procedure creates a proxy configuration from HTTP URI `uri`.
The return value is a new HTTP proxy record.
|#
  (define make-http-proxy
    (lambda (uri)
      (let ([uri (normalize-http-uri 'make-http-proxy uri)])
        (unless (member (uri-scheme uri) '("http" "https"))
          (errorf 'make-http-proxy "proxy URI must use HTTP or HTTPS"))
        (%make-http-proxy uri))))

  #|proc:make-http-multipart-part
The `make-http-multipart-part` procedure creates form part `name` containing `value`.
`value` is a string or bytevector; optional `filename` and `content-type` are strings or `#f`.
The return value is a new multipart part record.
|#
  (define make-http-multipart-part
    (case-lambda
      [(name value) (make-http-multipart-part name value #f #f)]
      [(name value filename content-type)
       (pcheck ([string? name]
                [(lambda (item) (or (string? item) (bytevector? item))) value]
                [(lambda (item) (or (not item) (string? item))) filename content-type])
         (%make-http-multipart-part name value filename content-type))]))

  #|proc:make-http-multipart-body
The `make-http-multipart-body` procedure encodes the HTTP multipart `part*` list.
It returns two values: a bounded body source and its `Content-Type` header value.
|#
  (define make-http-multipart-body
    (lambda (part*)
      (pcheck ([list? part*])
        (unless (andmap http-multipart-part? part*)
          (errorf 'make-http-multipart-body "expected a list of multipart parts"))
        (let ([boundary "chezpp-7d9e4f6a2b1c"])
          (let-values ([(port get) (open-bytevector-output-port)])
            (for-each
             (lambda (part)
               (when (or (string-contains? (http-multipart-part-name part) "\r")
                         (string-contains? (http-multipart-part-name part) "\n"))
                 (errorf 'make-http-multipart-body "multipart name contains a newline"))
               (put-bytevector port (string->utf8 (string-append "--" boundary "\r\n")))
               (put-bytevector
                port
                (string->utf8
                 (string-append
                  "Content-Disposition: form-data; name=\""
                  (http-multipart-part-name part) "\""
                  (if (http-multipart-part-filename part)
                      (string-append "; filename=\""
                                     (http-multipart-part-filename part) "\"")
                      "")
                  "\r\n")))
               (when (http-multipart-part-content-type part)
                 (put-bytevector
                  port
                  (string->utf8
                   (string-append "Content-Type: "
                                  (http-multipart-part-content-type part) "\r\n"))))
               (put-bytevector port (string->utf8 "\r\n"))
               (put-bytevector port
                               (if (string? (http-multipart-part-value part))
                                   (string->utf8 (http-multipart-part-value part))
                                   (http-multipart-part-value part)))
               (put-bytevector port (string->utf8 "\r\n")))
             part*)
            (put-bytevector port (string->utf8 (string-append "--" boundary "--\r\n")))
            (let* ([bytes (get)] [index 0] [length (bytevector-length bytes)])
              (values
               (make-http-body-source
                (lambda (maximum-bytes)
                  (if (= index length)
                      (eof-object)
                      (let* ([stop (min length (+ index maximum-bytes))]
                             [chunk (bytevector-slice bytes index stop)])
                        (set! index stop)
                        chunk)))
                length)
               (string-append "multipart/form-data; boundary=" boundary))))))))

  #|proc:make-http-pool-policy
The `make-http-pool-policy` procedure configures `max-idle`, `max-active`, and `idle-timeout-ms`.
All parameters are nonnegative integers. The return value is a new pool policy record.
|#
  (define make-http-pool-policy
    (lambda (max-idle max-active idle-timeout-ms)
      (pcheck ([natural? max-idle max-active idle-timeout-ms])
        (%make-http-pool-policy max-idle max-active idle-timeout-ms))))

  #|proc:http-client-cookie-jar-set!
The `http-client-cookie-jar-set!` procedure assigns cookie `jar` or `#f` to `client`.
The return value is `client`.
|#
  (define http-client-cookie-jar-set!
    (lambda (client jar)
      (pcheck ([http-client? client]
               [(lambda (value) (or (not value) (http-cookie-jar? value))) jar])
        (ensure-client-open 'http-client-cookie-jar-set! client)
        (http-client-cookie-jar-set-internal! client jar)
        client)))

  #|proc:http-client-auth-set!
The `http-client-auth-set!` procedure assigns authentication to `client`.
`scheme` is `basic`, `bearer`, `#f`, or a procedure with signature
`(request response-or-#f) -> request`; `credential` supplies scheme-specific data.
The return value is `client`.
|#
  (define http-client-auth-set!
    (case-lambda
      [(client scheme) (http-client-auth-set! client scheme #f)]
      [(client scheme credential)
       (pcheck ([http-client? client])
         (unless (or (not scheme) (procedure? scheme) (memq scheme '(basic bearer)))
           (errorf 'http-client-auth-set! "expected basic, bearer, procedure, or #f"))
         (when (eq? scheme 'basic)
           (unless (and (pair? credential) (string? (car credential))
                        (string? (cdr credential)))
             (errorf 'http-client-auth-set! "basic credential must be (user . password)")))
         (when (eq? scheme 'bearer)
           (unless (string? credential)
             (errorf 'http-client-auth-set! "bearer credential must be a string")))
         (http-client-auth-set-internal! client (and scheme (cons scheme credential)))
         client)]))

  #|proc:http-client-proxy-set!
The `http-client-proxy-set!` procedure assigns proxy record `proxy` or `#f` to `client`.
The return value is `client`.
|#
  (define http-client-proxy-set!
    (lambda (client proxy)
      (pcheck ([http-client? client]
               [(lambda (value) (or (not value) (http-proxy? value))) proxy])
        (http-client-proxy-set-internal! client proxy)
        client)))

  #|proc:http-client-pool-policy-set!
The `http-client-pool-policy-set!` procedure assigns pool `policy` to `client`.
The return value is `client`.
|#
  (define http-client-pool-policy-set!
    (lambda (client policy)
      (pcheck ([http-client? client] [http-pool-policy? policy])
        (http-client-pool-policy-set-internal! client policy)
        client)))

  #|proc:http-client-version-set!
The `http-client-version-set!` procedure sets `client` policy to `auto`, `http/1.1`, or `h2`.
The return value is `client`.
|#
  (define http-client-version-set!
    (lambda (client version)
      (pcheck ([http-client? client] [symbol? version])
        (unless (memq version '(auto http/1.1 h2))
          (errorf 'http-client-version-set! "expected auto, http/1.1, or h2"))
        (when (and (eq? version 'h2)
                   (not (optional-library-available?
                         (optional-library-info 'nghttp2))))
          (errorf 'http-client-version-set! "nghttp2 support is unavailable"))
        (http-client-version-set-internal! client version)
        client)))

  #|proc:http-open
The `http-open` procedure constructs an HTTP client with optional TLS context state for HTTPS requests.
|#
  (define-who http-open
    (case-lambda
      [()
       (%make-http-client '() #f http-default-timeout-ms #f #f #f #f
                          (%make-http-pool-policy 1 1 30000) 'auto #f #f #f #f)]
      [(tls-context)
       (pcheck ([tls-context? tls-context])
         (%make-http-client '() #f http-default-timeout-ms tls-context #f #f #f
                            (%make-http-pool-policy 1 1 30000) 'auto #f #f #f #f))]))

  #|proc:http-close
The `http-close` procedure marks an HTTP client as closed.
|#
  (define-who http-close
    (lambda (client)
      (pcheck ([http-client? client])
              (let ([pending (http-client-pending client)])
                (when pending
                  (cancel-pending! client pending)))
              (let ([conn (http-client-cached-connection client)])
                (when conn
                  (uncache-http-connection! client conn)
                  (close-http-connection conn)))
              (http-client-closed?-set! client #t)
              client)))

  #|proc:http-follow-redirects!
The `http-follow-redirects!` procedure enables or disables automatic redirect handling on an HTTP client.
|#
  (define-who http-follow-redirects!
    (lambda (client follow?)
      (pcheck ([http-client? client] [boolean? follow?])
              (ensure-client-open who client)
              (http-client-follow-redirects?-set! client follow?)
              follow?)))

  #|proc:http-set-header!
The `http-set-header!` procedure sets a default header on an HTTP client.
|#
  (define-who http-set-header!
    (lambda (client name value)
      (pcheck ([http-client? client] [string? value])
              (ensure-client-open who client)
              (http-client-default-headers-set!
               client
               (http-header-set (http-client-default-headers client) name value))
              client)))

  #|proc:http-set-timeout!
The `http-set-timeout!` procedure records a client timeout value in milliseconds for future request operations.
|#
  (define-who http-set-timeout!
    (lambda (client timeout-ms)
      (pcheck ([http-client? client] [fixnum? timeout-ms])
              (check-timeout-ms who timeout-ms)
              (ensure-client-open who client)
              (http-client-timeout-ms-set! client timeout-ms)
              timeout-ms)))

  #|proc:http-cancel-pending!
The `http-cancel-pending!` procedure cancels the pending request on `client`, if any.
The `client` parameter is an open HTTP client.
The return value is `client`.
|#
  (define-who http-cancel-pending!
    (lambda (client)
      (pcheck ([http-client? client])
              (ensure-client-open who client)
              (let ([pending (http-client-pending client)])
                (when pending
                  (cancel-pending! client pending)))
              client)))

  #|proc:http-send
The `http-send` procedure sends an HTTP request with a configured client and returns an HTTP response.
|#
  (define-who http-send
    (lambda (client request)
      (pcheck ([http-client? client] [http-request? request])
              (ensure-client-open who client)
              (net-operation-wait
               (http-transfer/nonblocking
                who client 'http-send request (lambda (response) response))))))

  #|proc:http-send/nonblocking
The `http-send/nonblocking` procedure constructs an HTTP request operation.
The `client` parameter is an open HTTP client.
The `request` parameter is the HTTP request to send.
The return value is a `net-operation` whose successful result is an HTTP response.
|#
  (define-who http-send/nonblocking
    (lambda (client request)
      (pcheck ([http-client? client] [http-request? request])
              (http-transfer/nonblocking
               who
               client
               'http-send
               request
               (lambda (response) response)))))

  #|proc:http-request
The `http-request` procedure sends a one-shot HTTP request without manually managing a client object.
|#
  (define-who http-request
    (case-lambda
      [(method uri)
       (http-request method uri '() #f)]
      [(method uri headers)
       (http-request method uri headers #f)]
      [(method uri headers body)
       (let ([client (http-open)])
         (dynamic-wind
           void
           (lambda ()
             (http-send client (make-http-request method uri headers body)))
           (lambda ()
             (http-close client))))]))

  #|proc:http-request/nonblocking
The `http-request/nonblocking` procedure constructs a request operation for `client`.
The `method`, `uri`, `headers`, and `body` parameters describe the HTTP request.
The return value is a `net-operation` whose successful result is an HTTP response.
|#
  (define-who http-request/nonblocking
    (case-lambda
      [(client method uri)
       (http-request/nonblocking client method uri '() #f)]
      [(client method uri headers)
       (http-request/nonblocking client method uri headers #f)]
      [(client method uri headers body)
       (pcheck ([http-client? client])
               (let ([request (make-http-request method uri headers body)])
                 (http-transfer/nonblocking
                 who
                  client
                  'http-request
                  request
                  (lambda (response) response))))]))

  (define make-http-verb
    (lambda (method)
      (case-lambda
        [(uri)
         (http-request method uri '() #f)]
        [(client uri)
         (pcheck ([http-client? client])
                 (http-send client (make-http-request method uri '() #f)))]
        [(client uri body)
         (pcheck ([http-client? client])
                 (http-send client (make-http-request method uri '() body)))]
        [(client uri headers body)
         (pcheck ([http-client? client])
                 (http-send client (make-http-request method uri headers body)))])))

  #|proc:http-get
The `http-get` procedure sends an HTTP GET request either with a supplied client or as a one-shot operation.
|#
  (define http-get (make-http-verb 'get))

  #|proc:http-head
The `http-head` procedure sends an HTTP HEAD request either with a supplied client or as a one-shot operation.
|#
  (define http-head (make-http-verb 'head))

  #|proc:http-post
The `http-post` procedure sends an HTTP POST request either with a supplied client or as a one-shot operation.
|#
  (define http-post (make-http-verb 'post))

  #|proc:http-put
The `http-put` procedure sends an HTTP PUT request either with a supplied client or as a one-shot operation.
|#
  (define http-put (make-http-verb 'put))

  #|proc:http-delete
The `http-delete` procedure sends an HTTP DELETE request either with a supplied client or as a one-shot operation.
|#
  (define http-delete (make-http-verb 'delete))

  #|proc:http-download
The `http-download` procedure streams a response body to `path` and returns the HTTP response.
The response body is `#f` because received bytes are written directly to the destination file.
|#
  (define-who http-download
    (case-lambda
      [(uri path)
       (let ([client (http-open)])
         (dynamic-wind
           void
           (lambda ()
             (http-download client uri path))
           (lambda ()
             (http-close client))))]
      [(client uri path)
       (pcheck ([http-client? client] [string? path])
         (net-operation-wait (http-download/nonblocking client uri path)))]))

  #|proc:http-download/nonblocking
The `http-download/nonblocking` procedure constructs a download operation for `client`.
The `uri` parameter identifies the resource and `path` is the destination pathname.
The return value is a `net-operation` whose successful result is an HTTP response.
|#
  (define-who http-download/nonblocking
    (lambda (client uri path)
      (pcheck ([http-client? client] [string? path])
        (let* ([request (make-http-request 'get uri '() #f)]
               [pending (http-client-pending client)])
          (ensure-client-open who client)
          (ensure-no-pending-mismatch who client 'http-download (request-key request))
          (if (and pending (eq? 'pending (net-operation-state pending)))
              pending
              (let* ([temporary-path (string-append path ".chezpp-part")]
                     [sink (make-http-file-body-sink temporary-path)])
                (http-transfer/nonblocking
                 who client 'http-download request
                 (lambda (response)
                   (rename-file temporary-path path)
                   response)
                 sink)))))))

  #|proc:http-upload
The `http-upload` procedure uploads a file as a PUT request body and returns the HTTP response.
|#
  (define-who http-upload
    (case-lambda
      [(uri path)
       (let ([client (http-open)])
         (dynamic-wind
           void
           (lambda ()
             (http-upload client uri path))
           (lambda ()
             (http-close client))))]
      [(client uri path)
       (pcheck ([http-client? client] [string? path])
               (http-put client
                         uri
                         '(("Content-Type" . "application/octet-stream"))
                         (make-http-file-body-source path)))]))

  #|proc:http-upload/nonblocking
The `http-upload/nonblocking` procedure constructs an upload operation for `client`.
The `uri` parameter identifies the resource and `path` is the source pathname.
The return value is a `net-operation` whose successful result is an HTTP response.
|#
  (define-who http-upload/nonblocking
    (lambda (client uri path)
      (pcheck ([http-client? client] [string? path])
              (http-transfer/nonblocking
               who
               client
               'http-upload
               (make-http-request
                'put
                uri
                '(("Content-Type" . "application/octet-stream"))
                (make-http-file-body-source path))
               (lambda (response) response)))))

  ;;===----------------------------------------------------------------------===
  ;; Server API
  ;;===----------------------------------------------------------------------===

  #|proc:http-listen
The `http-listen` procedure opens a listening HTTP server on `host` and `port`, optionally wrapping accepted connections with TLS.
|#
  (define-who http-listen
    (case-lambda
      [(host port)
       (http-listen host port #f 128)]
      [(host port tls-context)
       (http-listen host port tls-context 128)]
      [(host port tls-context backlog)
       (pcheck ([string? host] [fixnum? port] [fixnum? backlog])
               (check-port who port)
               (check-backlog who backlog)
               (unless (or (not tls-context) (tls-context? tls-context))
                 (errorf who "expected #f or TLS context, given ~s" tls-context))
               (let ([server-socket (open-socket 'inet 'stream)])
                 (guard (c [else
                            (guard (x [else #f])
                              (close-socket server-socket))
                            (raise c)])
                   (socket-set-option! server-socket 'reuse-address #t)
                   (socket-bind! server-socket (make-socket-address 'inet host port))
                   (socket-listen! server-socket backlog)
                   (%make-http-server server-socket
                                      host
                                      (socket-address-port
                                       (socket-local-address server-socket))
                                      tls-context
                                      (make-hashtable equal-hash equal?)
                                      '()
                                      #f
                                      (make-mutex 'http-server-close)))))]))

  #|proc:http-server-close
The `http-server-close` procedure closes the listening socket owned by an HTTP server.
|#
  (define-who http-server-close
    (lambda (server)
      (pcheck ([http-server? server])
              (with-mutex (http-server-close-mutex server)
                (unless (http-server-closed? server)
                  (for-each net-operation-cancel!
                            (http-server-operations server))
                  (http-server-operations-set! server '())
                  (close-socket (http-server-socket server))
                  (http-server-closed?-set! server #t)))
              server)))

  #|proc:http-register-handler!
The `http-register-handler!` procedure registers `proc` on `server` for `path` and optional
`method`. The `proc` parameter has signature `(http-request) -> http-response`.
The return value is the replaced handler or `#f` when no handler was replaced.
|#
  (define-who http-register-handler!
    (case-lambda
      [(server path proc)
       (pcheck ([http-server? server] [string? path] [procedure? proc])
               (ensure-server-open who server)
               (let* ([key (make-handler-key path)]
                      [old (hashtable-ref (http-server-handlers server) key #f)])
                 (hashtable-set! (http-server-handlers server) key proc)
                 old))]
      [(server method path proc)
       (pcheck ([http-server? server] [string? path] [procedure? proc])
               (ensure-server-open who server)
               (let* ([key (make-handler-key method path)]
                      [old (hashtable-ref (http-server-handlers server) key #f)])
                 (hashtable-set! (http-server-handlers server) key proc)
                 old))]))

  #|proc:http-handler-ref
The `http-handler-ref` procedure returns the handler registered for `method` and `path`.
The `server` parameter is an open HTTP server and `default` is the missing-handler value.
The return value is the method-specific handler, path handler, or `default`.
|#
  (define-who http-handler-ref
    (lambda (server method path default)
      (pcheck ([http-server? server] [string? path])
              (ensure-server-open who server)
              (or (hashtable-ref (http-server-handlers server)
                                 (make-handler-key method path) #f)
                  (hashtable-ref (http-server-handlers server) path default)))))

  #|proc:http-unregister-handler!
The `http-unregister-handler!` procedure removes a handler from `server` for `path` and optional
`method`. The return value is the removed handler or `#f` when no handler was registered.
|#
  (define-who http-unregister-handler!
    (case-lambda
      [(server path)
       (pcheck ([http-server? server] [string? path])
               (ensure-server-open who server)
               (let ([old (hashtable-ref (http-server-handlers server) path #f)])
                 (hashtable-delete! (http-server-handlers server) path)
                 old))]
      [(server method path)
       (pcheck ([http-server? server] [string? path])
               (ensure-server-open who server)
               (let* ([key (make-handler-key method path)]
                      [old (hashtable-ref (http-server-handlers server) key #f)])
                 (hashtable-delete! (http-server-handlers server) key)
                 old))]))

  #|proc:http-accept
The `http-accept` procedure accepts a client connection from an HTTP server and returns an HTTP connection object.
|#
  (define-who http-accept
    (lambda (server)
      (pcheck ([http-server? server])
              (ensure-server-open who server)
              (let-values ([(sock peer)
                            (socket-accept (http-server-socket server))])
                (make-server-connection sock (http-server-tls-context server))))))

  #|proc:http-accept/nonblocking
The `http-accept/nonblocking` procedure accepts an HTTP connection if one is ready and returns `#f` otherwise.
|#
  (define-who http-accept/nonblocking
    (lambda (server)
      (pcheck ([http-server? server])
              (ensure-server-open who server)
              (call-with-values
               (lambda ()
                 (socket-accept/nonblocking (http-server-socket server)))
               (case-lambda
                 [(sock peer)
                  (make-server-connection sock (http-server-tls-context server))]
                 [(value)
                  (and (not value) #f)])))))

  #|proc:http-connection-close
The `http-connection-close` procedure closes an HTTP connection and all resources it owns.
|#
  (define-who http-connection-close
    (lambda (conn)
      (pcheck ([http-connection? conn])
              (close-http-connection conn)
              conn)))

  #|proc:http-read-request
The `http-read-request` procedure reads one HTTP request from an accepted connection.
|#
  (define-who http-read-request
    (lambda (conn)
      (pcheck ([http-connection? conn])
              (ensure-connection-open who conn)
              (let ([line (read-http-line (http-connection-input-port conn))])
                (when (eof-object? line)
                  (raise-net-error who 'http "unexpected EOF while reading HTTP request"))
                (let-values ([(method target version)
                              (parse-request-line who line)])
                  (let* ([headers (read-http-headers who (http-connection-input-port conn))]
                         [content-length (response-body-length headers)]
                         [body (cond
                                [(chunked-transfer? headers)
                                 (read-http-body/chunked who (http-connection-input-port conn))]
                                [content-length
                                 (read-http-body/exact who
                                                       (http-connection-input-port conn)
                                                       content-length)]
                                [else #f])]
                         [u (request-target->uri who conn target headers)])
                    (make-http-request method u headers body)))))))

  #|proc:http-read-request/nonblocking
The `http-read-request/nonblocking` procedure attempts to read one HTTP request if the connection is currently readable, and returns `#f` otherwise.
|#
  (define-who http-read-request/nonblocking
    (lambda (conn)
      (pcheck ([http-connection? conn])
              (ensure-connection-open who conn)
              (let ([ready (poll/nonblocking
                            (list (make-poll-target (http-connection-socket conn)
                                                    '(read))))])
                (if (memq 'read (poll-target-ready-events (car ready)))
                    (http-read-request conn)
                    #f)))))

  #|proc:http-write-response
The `http-write-response` procedure writes one HTTP response to an accepted connection.
|#
  (define-who http-write-response
    (lambda (conn response)
      (pcheck ([http-connection? conn] [http-response? response])
              (ensure-connection-open who conn)
              (write-response-port (http-connection-output-port conn) response)
              response)))

  #|proc:http-write-response/nonblocking
The `http-write-response/nonblocking` procedure writes an HTTP response if the connection is currently writable, and returns `#f` otherwise.
|#
  (define-who http-write-response/nonblocking
    (lambda (conn response)
      (pcheck ([http-connection? conn] [http-response? response])
              (ensure-connection-open who conn)
              (let ([ready (poll/nonblocking
                            (list (make-poll-target (http-connection-socket conn)
                                                    '(write))))])
                (if (memq 'write (poll-target-ready-events (car ready)))
                    (http-write-response conn response)
                    #f)))))

  #|proc:http-serve
The `http-serve` procedure accepts one connection, dispatches requests through the registered handler table, and keeps serving that connection until either side asks to close it.
|#
  (define-who http-serve
    (lambda (server)
      (pcheck ([http-server? server])
              (ensure-server-open who server)
              (let ([conn (http-accept server)])
                (serve-http-connection who server conn)))))

  #|proc:http-serve-loop
The `http-serve-loop` procedure repeatedly accepts and serves HTTP connections until `server`
is closed. The `server` parameter is an HTTP server. The return value is `server`.
|#
  (define-who http-serve-loop
    (lambda (server)
      (pcheck ([http-server? server])
              (let loop ()
                (unless (http-server-closed? server)
                  (let* ([operation*
                          (filter
                           (lambda (operation)
                             (eq? 'pending (net-operation-state operation)))
                           (http-server-operations server))]
                         [listener-target
                          (make-poll-target
                           (http-server-socket server)
                           '(read error hup invalid))]
                         [target*
                          (cons listener-target
                                (apply append
                                       (map net-operation-poll-targets operation*)))]
                         [ready (poll target* 100)]
                         [listener-events
                          (poll-target-ready-events (car ready))])
                    (when (and (not (http-server-closed? server))
                               (memq 'read listener-events))
                      (call-with-values
                       (lambda ()
                         (socket-accept/nonblocking (http-server-socket server)))
                       (case-lambda
                         [(sock peer)
                          (let ([operation
                                 (make-incremental-server-operation who server sock)])
                            (net-operation-step! operation)
                            (set! operation* (cons operation operation*)))]
                         [(value) (void)])))
                    (for-each
                     (lambda (operation)
                       (when (and
                              (eq? 'pending (net-operation-state operation))
                              (or
                               (fx= (net-operation-remaining-timeout-ms operation) 0)
                               (exists
                                (lambda (target)
                                  (let ([fd (poll-target-fd target)])
                                    (exists
                                     (lambda (ready-target)
                                       (and
                                        (= fd (poll-target-fd ready-target))
                                        (pair?
                                         (poll-target-ready-events ready-target))))
                                     ready)))
                                (net-operation-poll-targets operation))))
                         (net-operation-step! operation)))
                     operation*)
                    (http-server-operations-set!
                     server
                     (filter
                      (lambda (operation)
                        (eq? 'pending (net-operation-state operation)))
                      operation*)))
                  (loop)))
              server)))
  )
