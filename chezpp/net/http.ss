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
          http-register-handler!
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
          (chezpp net private)
          (chezpp net tls))

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
            (immutable body http-response-body)))

  (define-record-type (http-client %make-http-client http-client?)
    (sealed #t)
    (opaque #f)
    (fields (mutable default-headers http-client-default-headers http-client-default-headers-set!)
            (mutable follow-redirects? http-client-follow-redirects? http-client-follow-redirects?-set!)
            (mutable timeout-ms http-client-timeout-ms http-client-timeout-ms-set!)
            (immutable tls-context http-client-tls-context)
            (mutable cached-origin http-client-cached-origin http-client-cached-origin-set!)
            (mutable cached-connection http-client-cached-connection http-client-cached-connection-set!)
            (mutable pending http-client-pending http-client-pending-set!)
            (mutable closed? http-client-closed? http-client-closed?-set!)))

  (define-record-type (http-pending-op %make-http-pending-op http-pending-op?)
    (sealed #t)
    (opaque #f)
    (fields (immutable kind http-pending-kind)
            (immutable args http-pending-args)
            (immutable reader http-pending-reader)
            (immutable writer http-pending-writer)
            (immutable thread http-pending-thread)
            (mutable done? http-pending-done? http-pending-done?-set!)
            (mutable result http-pending-result http-pending-result-set!)
            (mutable cancelled? http-pending-cancelled? http-pending-cancelled?-set!)))

  (define-record-type (http-server %make-http-server http-server?)
    (sealed #t)
    (opaque #f)
    (fields (immutable socket http-server-socket)
            (immutable host http-server-host)
            (immutable port http-server-port)
            (immutable tls-context http-server-tls-context)
            (mutable handlers http-server-handlers http-server-handlers-set!)
            (mutable closed? http-server-closed? http-server-closed?-set!)))

  (define-record-type (http-connection %make-http-connection http-connection?)
    (sealed #t)
    (opaque #f)
    (fields (immutable socket http-connection-socket)
            (immutable tls-session http-connection-tls-session)
            (immutable deadline-cell http-connection-deadline-cell)
            (immutable input-port http-connection-input-port)
            (immutable output-port http-connection-output-port)
            (immutable secure? http-connection-secure?)
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
       [(or (not body) (string? body) (bytevector? body)) body]
       [else
        (errorf who "expected body to be #f, string, or bytevector, given ~s" body)])))

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
                   (or (not (eq? (http-pending-kind pending) kind))
                       (not (equal? (http-pending-args pending) args))))
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
      (let ([t (current-time)])
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

  (define close-pending-notifier!
    (lambda (pending)
      (guard (c [else #f])
        (close-socket (http-pending-reader pending)))
      (guard (c [else #f])
        (close-socket (http-pending-writer pending)))))

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

  (define start-pending!
    (lambda (client kind args thunk)
      (let-values ([(reader writer) (open-pending-notifier)])
        (letrec ([pending
                  (%make-http-pending-op
                   kind
                   args
                   reader
                   writer
                   (fork-thread
                    (lambda ()
                      (let ([result
                             (guard (c [else c])
                               (thunk pending))])
                        (when (http-pending-cancelled? pending)
                          (set! result #f))
                        (http-pending-result-set! pending result))
                      (http-pending-done?-set! pending #t)
                      (guard (c [else #f])
                        (socket-send-all writer #vu8(1)))))
                   #f
                   #f
                   #f)])
          (http-client-pending-set! client pending)
          pending))))

  (define pending-ready?
    (lambda (pending)
      (or (http-pending-done? pending)
          (let* ([target (make-poll-target (http-pending-reader pending)
                                           '(read error hup invalid))]
                 [ready (car (poll/nonblocking (list target)))])
            (pair? (poll-target-ready-events ready))))))

  (define finish-pending!
    (lambda (who client pending)
      (http-client-pending-set! client #f)
      (thread-join (http-pending-thread pending))
      (close-pending-notifier! pending)
      (let ([result (http-pending-result pending)])
        (if (condition? result)
            (raise result)
            result))))

  (define cancel-pending!
    (lambda (client pending)
      (http-pending-cancelled?-set! pending #t)
      (close-pending-notifier! pending)
      (http-client-pending-set! client #f)
      client))

  (define http-transfer/nonblocking
    (lambda (who client kind args thunk)
      (ensure-client-open who client)
      (ensure-no-pending-mismatch who client kind args)
      (let ([pending (or (http-client-pending client)
                         (start-pending! client kind args thunk))])
        (if (pending-ready? pending)
            (finish-pending! who client pending)
            #f))))

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

  (define merge-request-headers
    (lambda (request client)
      (let* ([body (body->bytevector (http-request-body request))]
             [headers (header-list-set-many (http-client-default-headers client)
                                            (http-request-headers request))]
             [headers (if (http-header-ref headers "Host" #f)
                          headers
                          (http-header-set headers "Host"
                                           (http-host-header (http-request-uri request))))]
             [headers (if (http-header-ref headers "Connection" #f)
                          headers
                          (http-header-set headers "Connection" "keep-alive"))])
        (if (http-header-ref headers "Content-Length" #f)
            headers
            (http-header-set headers "Content-Length"
                             (number->string (bytevector-length body)))))))

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
      (let ([bv (get-bytevector-n ip n)])
        (when (eof-object? bv)
          (errorf who "unexpected EOF while reading HTTP body"))
        bv)))

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
        (if (http-header-ref headers "Content-Length" #f)
            headers
            (http-header-set headers "Content-Length"
                             (number->string (bytevector-length body)))))))

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
        (unless (fx= 0 (bytevector-length body))
          (put-bytevector op body))
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
                               #f))))

  (define cache-http-connection!
    (lambda (client origin conn)
      (let ([old (http-client-cached-connection client)])
        (when (and old (not (eq? old conn)))
          (close-http-connection old)))
      (http-client-cached-origin-set! client origin)
      (http-client-cached-connection-set! client conn)
      conn))

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
             [cached-conn (http-client-cached-connection client)])
        (cond
         [(and cached-conn
               (equal? cached-origin origin)
               (not (http-connection-closed? cached-conn))
               (not (reusable-http-connection-stale? cached-conn)))
          (http-client-cached-origin-set! client #f)
          (http-client-cached-connection-set! client #f)
          (http-connection-deadline-ms-set! cached-conn deadline-ms)
          cached-conn]
         [else
          (when (and cached-conn
                     (or (http-connection-closed? cached-conn)
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
                                 (let ([ctx (make-tls-context 'client)])
                                   (tls-context-set-verify! ctx #f)
                                   ctx))])
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
      (or (not pending)
          (and (not (http-client-closed? client))
               (eq? (http-client-pending client) pending)
               (not (http-pending-cancelled? pending))))))

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
                       (cache-connection-allowed? client pending)))
                (when keep-open?
                  (cache-http-connection! client origin conn))
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
      [(path) path]
      [(method path)
       (cons (normalize-http-method 'make-handler-key method) path)]))

  (define lookup-handler
    (lambda (server request)
      (let* ([path (or (uri-path (http-request-uri request)) "/")]
             [path (if (string=? path "") "/" path)]
             [method-key (make-handler-key (http-request-method request) path)])
        (cond
         [(assoc method-key (http-server-handlers server)) => cdr]
         [(assoc path (http-server-handlers server)) => cdr]
         [else #f]))))

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
                                     #f)))
          (%make-http-connection sock
                                 #f
                                 (vector #f)
                                 (open-socket-input-port sock)
                                 (open-socket-output-port sock)
                                 #f
                                 #f))))

  ;;===----------------------------------------------------------------------===
  ;; Data Model API
  ;;===----------------------------------------------------------------------===

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
                                    (normalize-http-body who body)))]))

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

  #|proc:http-open
The `http-open` procedure constructs an HTTP client with optional TLS context state for HTTPS requests.
|#
  (define-who http-open
    (case-lambda
      [()
       (%make-http-client '() #f http-default-timeout-ms #f #f #f #f #f)]
      [(tls-context)
       (pcheck ([tls-context? tls-context])
               (%make-http-client '() #f http-default-timeout-ms tls-context #f #f #f #f))]))

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
The `http-cancel-pending!` procedure cancels and discards the currently pending non-blocking HTTP operation on a client, if any.
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
              (http-send* who
                          client
                          request
                          5
                          (timeout->deadline-ms
                           (http-client-timeout-ms client))))))

  #|proc:http-send/nonblocking
The `http-send/nonblocking` procedure progresses an HTTP request without blocking and returns `#f` while the response is still pending.
|#
  (define-who http-send/nonblocking
    (lambda (client request)
      (pcheck ([http-client? client] [http-request? request])
              (http-transfer/nonblocking
               who
               client
               'send
               (request-key request)
               (lambda (pending)
                 (http-send* who
                             client
                             request
                             5
                             (timeout->deadline-ms
                              (http-client-timeout-ms client))
                             pending))))))

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
The `http-request/nonblocking` procedure progresses a one-client HTTP request without blocking and returns `#f` while the response is still pending.
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
                  'request
                  (request-key request)
                  (lambda (pending)
                    (http-send* who
                                client
                                request
                                5
                                (timeout->deadline-ms
                                 (http-client-timeout-ms client))
                                pending)))))]))

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
The `http-download` procedure downloads a response body to `path` and returns the full HTTP response.
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
               (let ([response (http-get client uri)])
                 (when (bytevector? (http-response-body response))
                   (write-u8vec! path (http-response-body response)))
                 response))]))

  #|proc:http-download/nonblocking
The `http-download/nonblocking` procedure progresses a download without blocking and returns `#f` while the response is still pending.
|#
  (define-who http-download/nonblocking
    (lambda (client uri path)
      (pcheck ([http-client? client] [string? path])
              (http-transfer/nonblocking
               who
               client
               'download
               (list (if (uri? uri) (uri->string uri) uri) path)
               (lambda (pending)
                 (let ([response
                        (http-send* who
                                    client
                                    (make-http-request 'get uri '() #f)
                                    5
                                    (timeout->deadline-ms
                                     (http-client-timeout-ms client))
                                    pending)])
                   (when (bytevector? (http-response-body response))
                     (write-u8vec! path (http-response-body response)))
                   response))))))

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
                         (read-u8vec path)))]))

  #|proc:http-upload/nonblocking
The `http-upload/nonblocking` procedure progresses an upload without blocking and returns `#f` while the response is still pending.
|#
  (define-who http-upload/nonblocking
    (lambda (client uri path)
      (pcheck ([http-client? client] [string? path])
              (http-transfer/nonblocking
               who
               client
               'upload
               (list (if (uri? uri) (uri->string uri) uri) path)
               (lambda (pending)
                 (http-send* who
                             client
                             (make-http-request
                              'put
                              uri
                              '(("Content-Type" . "application/octet-stream"))
                              (read-u8vec path))
                             5
                             (timeout->deadline-ms
                              (http-client-timeout-ms client))
                             pending))))))

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
                                      '()
                                      #f))))]))

  #|proc:http-server-close
The `http-server-close` procedure closes the listening socket owned by an HTTP server.
|#
  (define-who http-server-close
    (lambda (server)
      (pcheck ([http-server? server])
              (unless (http-server-closed? server)
                (close-socket (http-server-socket server))
                (http-server-closed?-set! server #t))
              server)))

  #|proc:http-register-handler!
The `http-register-handler!` procedure registers a path-specific or method/path-specific request handler on an HTTP server.
|#
  (define-who http-register-handler!
    (case-lambda
      [(server path proc)
       (pcheck ([http-server? server] [string? path] [procedure? proc])
               (ensure-server-open who server)
               (http-server-handlers-set!
                server
                (cons (cons (make-handler-key path) proc)
                      (http-server-handlers server)))
               server)]
      [(server method path proc)
       (pcheck ([http-server? server] [string? path] [procedure? proc])
               (ensure-server-open who server)
               (http-server-handlers-set!
                server
                (cons (cons (make-handler-key method path) proc)
                      (http-server-handlers server)))
               server)]))

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
                         [body (and content-length
                                    (read-http-body/exact who
                                                          (http-connection-input-port conn)
                                                          content-length))]
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
                (dynamic-wind
                  void
                  (lambda ()
                    (let loop ()
                      (let* ([request (http-read-request conn)]
                             [handler (or (lookup-handler server request)
                                          default-handler)]
                             [response (handler request)])
                        (unless (http-response? response)
                          (errorf who "HTTP handler must return an HTTP response, given ~s"
                                  response))
                        (let-values ([(close? prepared)
                                      (server-prepare-response request response)])
                          (http-write-response conn prepared)
                          (if close?
                              prepared
                              (loop)))))
                    )
                  (lambda ()
                    (http-connection-close conn))))))))
