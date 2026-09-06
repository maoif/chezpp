(library (chezpp net http)
  (export http-request? make-http-request http-request-method http-request-uri http-request-headers http-request-body
          http-response? make-http-response http-response-status http-response-reason http-response-headers http-response-body http-response-trailers http-response-version
          http-body-source? make-http-body-source http-body-source-length http-body-source-read http-body-sink? make-http-body-sink http-body-sink-write! http-body-sink-finish!
          make-http-port-body-source make-http-file-body-source make-http-port-body-sink make-http-file-body-sink
          http-cookie? make-http-cookie http-cookie-name http-cookie-value http-cookie-domain http-cookie-path http-cookie-secure? http-cookie-jar? make-http-cookie-jar
          http-proxy? make-http-proxy http-proxy-uri http-multipart-part? make-http-multipart-part http-multipart-part-name http-multipart-part-value http-multipart-part-filename http-multipart-part-content-type
          http-pool-policy? make-http-pool-policy http-pool-policy-max-idle http-pool-policy-max-active http-pool-policy-idle-timeout-ms
          http-client-cookie-jar-set! http-client-auth-set! http-client-proxy-set! http-client-pool-policy-set! http-client-version-set! make-http-multipart-body
          http-header-ref http-header-set http-header-add http-client? http-open http-close http-send http-get http-head http-post http-put http-delete http-request http-download http-upload
          http-follow-redirects! http-set-header! http-set-timeout! http-cancel-pending! http-send/nonblocking http-request/nonblocking http-download/nonblocking http-upload/nonblocking
          http-server? http-listen http-server-close http-accept http-accept/nonblocking http-serve http-serve-loop http-register-handler! http-handler-ref http-unregister-handler!
          http-connection? http-connection-close http-read-request http-read-request/nonblocking http-write-response http-write-response/nonblocking)
  (import (chezpp chez) (chezpp utils) (chezpp file) (chezpp net uri) (chezpp net errors) (chezpp net operation) (chezpp net ffi) (chezpp net tls) (chezpp net http private) (chezpp net lws client) (chezpp net lws server))
  #|record:http-request-record
An immutable HTTP request containing a method, URI, header alist, and optional body.
|#
  (define-record-type (http-request-record %make-http-request http-request?) (sealed #t) (opaque #f)
    (fields (immutable method http-request-method) (immutable uri http-request-uri) (immutable headers http-request-headers) (immutable body http-request-body)))
  #|record:http-response-record
An immutable HTTP response containing status, reason, headers, body, trailers, and protocol version.
|#
  (define-record-type (http-response-record %make-http-response http-response?) (sealed #t) (opaque #f)
    (fields (immutable status http-response-status) (immutable reason http-response-reason) (immutable headers http-response-headers) (immutable body http-response-body) (immutable trailers http-response-trailers) (immutable version http-response-version)))
  #|record:http-body-source
A pull-based request body source with a producer, optional length, and one-shot closer.
|#
  (define-record-type (http-body-source %make-http-body-source http-body-source?) (sealed #t) (opaque #f)
    (fields (immutable producer http-body-source-producer) (immutable length http-body-source-length) (immutable closer http-body-source-closer) (mutable closed? http-body-source-closed? http-body-source-closed?-set!)))
  #|record:http-body-sink
A push-based response body sink with a consumer and one-shot finisher.
|#
  (define-record-type (http-body-sink %make-http-body-sink http-body-sink?) (sealed #t) (opaque #f)
    (fields (immutable consumer http-body-sink-consumer) (immutable finisher http-body-sink-finisher) (mutable finished? http-body-sink-finished? http-body-sink-finished?-set!)))
  #|record:http-cookie
An immutable cookie with name, value, domain, path, and secure transport flag.
|#
  (define-record-type (http-cookie %make-http-cookie http-cookie?) (sealed #t) (opaque #f) (fields (immutable name http-cookie-name) (immutable value http-cookie-value) (immutable domain http-cookie-domain) (immutable path http-cookie-path) (immutable secure? http-cookie-secure?)))
  #|record:http-cookie-jar
A mutable collection of cookies used by an HTTP client.
|#
  (define-record-type (http-cookie-jar %make-http-cookie-jar http-cookie-jar?) (sealed #t) (opaque #f) (fields (mutable cookies http-cookie-jar-cookies http-cookie-jar-cookies-set!)))
  #|record:http-proxy
An HTTP proxy configuration containing its proxy URI.
|#
  (define-record-type (http-proxy %make-http-proxy http-proxy?) (sealed #t) (opaque #f) (fields (immutable uri http-proxy-uri)))
  #|record:http-multipart-part
A multipart field with name, value, optional filename, and content type.
|#
  (define-record-type (http-multipart-part %make-http-multipart-part http-multipart-part?) (sealed #t) (opaque #f) (fields (immutable name http-multipart-part-name) (immutable value http-multipart-part-value) (immutable filename http-multipart-part-filename) (immutable content-type http-multipart-part-content-type)))
  #|record:http-pool-policy
Connection pool limits: maximum idle and active connections and idle timeout in milliseconds.
|#
  (define-record-type (http-pool-policy %make-http-pool-policy http-pool-policy?) (sealed #t) (opaque #f) (fields (immutable max-idle http-pool-policy-max-idle) (immutable max-active http-pool-policy-max-active) (immutable idle-timeout-ms http-pool-policy-idle-timeout-ms)))
  #|record:http-client-record
Mutable HTTP client configuration and transport state.
|#
  (define-record-type (http-client-record %make-http-client http-client?) (sealed #t) (opaque #f)
    (fields (mutable closed? http-client-closed? http-client-closed?-set!) (mutable headers http-client-headers http-client-headers-set!) (mutable timeout-ms http-client-timeout-ms http-client-timeout-ms-set!) (mutable follow-redirects? http-client-follow-redirects? http-client-follow-redirects?-set!) (mutable cookie-jar http-client-cookie-jar %http-client-cookie-jar-set!) (mutable auth http-client-auth %http-client-auth-set!) (mutable proxy http-client-proxy %http-client-proxy-set!) (mutable pool-policy http-client-pool-policy %http-client-pool-policy-set!) (mutable version http-client-version %http-client-version-set!) (mutable transport http-client-transport http-client-transport-set!) (mutable active http-client-active http-client-active-set!)))
  #|record:http-server-record
An HTTP server handle containing its LWS transport and synchronized handler table.
|#
  (define-record-type (http-server-record %make-http-server http-server?) (sealed #t) (opaque #f)
    (fields (immutable transport http-server-transport)
            (immutable handlers http-server-handlers)
            (immutable mutex http-server-mutex)
            (mutable closed? http-server-closed? http-server-closed?-set!)))
  #|record:http-connection-record
An accepted HTTP connection handle managed by the server API.
|#
  (define-record-type (http-connection-record %make-http-connection http-connection?) (sealed #t) (opaque #f)
    (fields (immutable request-handle http-connection-request-handle)
            (mutable request http-connection-request http-connection-request-set!)
            (mutable closed? http-connection-closed? http-connection-closed?-set!)))
  (define normalize-headers (lambda (headers) (unless (list? headers) (errorf 'normalize-headers "expected a header list")) (map (lambda (e) (unless (and (pair? e) (string? (car e)) (string? (cdr e))) (errorf 'normalize-headers "invalid header ~s" e)) e) headers)))
  (define monotonic-ms (lambda () (let ([t (current-time 'time-monotonic)]) (+ (* (time-second t) 1000) (quotient (time-nanosecond t) 1000000)))))
  (define slice-bv (lambda (b i e) (let ([x (make-bytevector (- e i))]) (bytevector-copy! b i x 0 (- e i)) x)))
  (define request-path (lambda (u) (let ([p (or (uri-raw-path u) "/")] [q (uri-raw-query u)]) (if q (string-append (if (string=? p "") "/" p) "?" q) (if (string=? p "") "/" p)))))
  (define bytevector-slice
    (lambda (bv start stop)
      (let ([out (make-bytevector (- stop start) 0)])
        (bytevector-copy! bv start out 0 (- stop start))
        out)))
  (define bytevector-concatenate
    (lambda (bytevector*)
      (let ([out (make-bytevector
                  (fold-left (lambda (length bytes)
                               (+ length (bytevector-length bytes)))
                             0 bytevector*) 0)])
        (let loop ([rest bytevector*] [offset 0])
          (unless (null? rest)
            (let ([bytes (car rest)])
              (bytevector-copy! bytes 0 out offset (bytevector-length bytes))
              (loop (cdr rest) (+ offset (bytevector-length bytes))))))
        out)))
  #|proc:make-http-request
Builds a request from method, URI, headers, and optional body; returns a request record.
|#
  (define make-http-request
    (case-lambda [(method uri) (make-http-request method uri '() #f)] [(method uri headers) (make-http-request method uri headers #f)]
      [(method uri headers body) (pcheck ([(lambda (x) (or (string? x) (symbol? x))) method] [(lambda (x) (or (string? x) (uri? x))) uri] [list? headers] [(lambda (x) (or (not x) (string? x) (bytevector? x) (http-body-source? x))) body]) (let ([u (if (uri? uri) uri (string->uri uri))]) (unless u (errorf 'make-http-request "invalid URI ~s" uri)) (%make-http-request (if (symbol? method) (string-upcase (symbol->string method)) (string-upcase method)) u (normalize-headers headers) body)))]))
  #|proc:make-http-response
Builds a response from status, reason, headers, body, trailers, and version.
Returns a response record.
|#
  (define make-http-response (case-lambda [(s r h b) (make-http-response s r h b '() 'h1)] [(s r h b t v) (pcheck ([fixnum? s] [string? r] [list? h] [list? t] [symbol? v]) (%make-http-response s r (normalize-headers h) b t v))]))
  #|proc:make-http-body-source
Builds a pull body source from producer, length, and optional closer; returns a source record.
|#
  (define make-http-body-source (case-lambda [(p l) (make-http-body-source p l void)] [(p l c) (pcheck ([procedure? p] [procedure? c]) (%make-http-body-source p l c #f))]))
  #|proc:http-body-source-read
Reads at most `n` bytes from source `s`, returning bytes or end-of-file.
|#
  (define http-body-source-read (lambda (s n) (pcheck ([http-body-source? s] [positive? n]) ((http-body-source-producer s) n))))
  #|proc:make-http-body-sink
Creates a response sink from a consumer `(bytevector start count)` and optional finisher.
Returns a sink record.
|#
  (define make-http-body-sink (case-lambda [(c) (make-http-body-sink c void)] [(c f) (pcheck ([procedure? c f]) (%make-http-body-sink c f #f))]))
  #|proc:http-body-sink-write!
Writes `count` bytes from `body` at `start` to sink `sink`; returns an unspecified value.
|#
  (define http-body-sink-write! (lambda (s b i n) (pcheck ([http-body-sink? s] [bytevector? b] [natural? i n]) ((http-body-sink-consumer s) b i n))))
  #|proc:http-body-sink-finish!
Finishes sink `sink` once and returns an unspecified value. Later calls do not finish it again.
|#
  (define http-body-sink-finish! (lambda (s) (pcheck ([http-body-sink? s]) (unless (http-body-sink-finished? s) ((http-body-sink-finisher s)) (http-body-sink-finished?-set! s #t)))))
  #|proc:make-http-port-body-source
Creates a body source that reads from input port `port` with optional byte length `length`.
Returns the body source and leaves `port` open when the source finishes.
|#
  (define make-http-port-body-source (lambda (p l) (pcheck ([input-port? p]) (make-http-body-source (lambda (n) (get-bytevector-n p n)) l))))
  #|proc:make-http-file-body-source
Opens file `path` and returns a body source that closes the file after the body is consumed.
|#
  (define make-http-file-body-source (lambda (path) (pcheck ([string? path]) (let ([p (open-file-input-port path)]) (make-http-body-source (lambda (n) (get-bytevector-n p n)) (file-size path) (lambda () (close-port p)))))))
  #|proc:make-http-port-body-sink
Creates a body sink that writes to output port `port`.
Returns a sink that flushes but does not close the port.
|#
  (define make-http-port-body-sink (lambda (p) (pcheck ([output-port? p]) (make-http-body-sink (lambda (b i n) (put-bytevector p b i n)) (lambda () (flush-output-port p))))))
  #|proc:make-http-file-body-sink
Opens file `path` for replacement and returns a body sink that closes the file when finished.
|#
  (define make-http-file-body-sink (lambda (path) (pcheck ([string? path]) (let ([p (open-file-output-port path (file-options no-fail replace))]) (make-http-body-sink (lambda (b i n) (put-bytevector p b i n)) (lambda () (close-port p)))))))
  #|proc:make-http-cookie
Creates and returns a cookie. `name` and `value` are its pair, `domain` and `path` limit its scope,
and `secure?` requires secure transport.
|#
  (define make-http-cookie (lambda (n v d p s) (pcheck ([string? n v d p] [boolean? s]) (%make-http-cookie n v d p s))))
  #|proc:make-http-cookie-jar
Creates and returns an empty mutable HTTP cookie jar.
|#
  (define make-http-cookie-jar (lambda () (%make-http-cookie-jar '())))
  #|proc:make-http-proxy
Creates and returns a proxy configuration from URI or URI string `uri`.
|#
  (define make-http-proxy (lambda (u) (pcheck ([(lambda (x) (or (string? x) (uri? x))) u]) (%make-http-proxy (if (uri? u) u (string->uri u))))))
  #|proc:make-http-multipart-part
Creates and returns a multipart part. `name` identifies `value`; optional `filename` and
`content-type` describe file data.
|#
  (define make-http-multipart-part (case-lambda [(n v) (make-http-multipart-part n v #f #f)] [(n v f c) (pcheck ([string? n]) (%make-http-multipart-part n v f c))]))
  #|proc:make-http-pool-policy
Creates and returns a pool policy. `max-idle` and `max-active` bound connections, while
`idle-timeout-ms` sets their idle lifetime in milliseconds.
|#
  (define make-http-pool-policy (lambda (i a t) (pcheck ([natural? i a t]) (%make-http-pool-policy i a t))))
  (define header-name-string
    (lambda (name)
      (if (symbol? name) (symbol->string name) name)))
  #|proc:http-header-ref
Returns the case-insensitive value for header `name` in `headers`, or `default` when absent.
|#
  (define http-header-ref (case-lambda [(h n) (http-header-ref h n #f)] [(h n d) (pcheck ([list? h] [(lambda (x) (or (string? x) (symbol? x))) n]) (let ([n (header-name-string n)] [x (find (lambda (e) (string-ci=? (car e) n)) h)]) (if x (cdr x) d)))]))
  #|proc:http-header-set
Returns `headers` with case-insensitive header `name` replaced by string `value`.
|#
  (define http-header-set (lambda (h n v) (pcheck ([list? h] [(lambda (x) (or (string? x) (symbol? x))) n] [string? v]) (let ([n (header-name-string n)]) (cons (cons n v) (filter (lambda (e) (not (string-ci=? (car e) n))) h))))))
  #|proc:http-header-add
Returns `headers` with string `value` appended for header `name`, preserving existing fields.
|#
  (define http-header-add (lambda (h n v) (pcheck ([list? h] [(lambda (x) (or (string? x) (symbol? x))) n] [string? v]) (append h (list (cons (header-name-string n) v))))))
  #|proc:http-open
Opens and returns a new HTTP client. The optional argument is reserved configuration.
|#
  (define http-open (case-lambda [() (%make-http-client #f '() 30000 #t #f #f #f #f 'auto #f '())] [(x) (pcheck ([boolean? x]) (%make-http-client #f '() 30000 #t #f #f #f #f 'auto #f '()))]))
  (define ensure-open (lambda (c) (when (http-client-closed? c) (raise-net-error 'http 'closed "HTTP client is closed" c))))
  (define body-producer
    (lambda (b)
      (cond
       [(http-body-source? b)
       (let ([closed? #f])
          (letrec ([close!
                    (lambda ()
                      (unless closed?
                        (set! closed? #t)
                        ((http-body-source-closer b))))])
            (lambda (n)
              (guard (condition
                      [else (close!) (raise condition)])
                (let ([chunk ((http-body-source-producer b) n)])
                  (when (eof-object? chunk) (close!))
                  chunk)))))]
       [(string? b)
        (let ([v (string->utf8 b)] [i 0])
          (lambda (n)
            (if (>= i (bytevector-length v))
                (eof-object)
                (let ([e (min (bytevector-length v) (+ i n))])
                  (let ([x (slice-bv v i e)]) (set! i e) x)))))]
       [(bytevector? b)
        (let ([i 0])
          (lambda (n)
            (if (>= i (bytevector-length b))
                (eof-object)
                (let ([e (min (bytevector-length b) (+ i n))])
                  (let ([x (slice-bv b i e)]) (set! i e) x)))))]
       [else #f])))
  (define body-length
    (lambda (body)
      (cond
       [(string? body) (bytevector-length (string->utf8 body))]
       [(bytevector? body) (bytevector-length body)]
       [(and (http-body-source? body) (http-body-source-length body))
        (http-body-source-length body)]
       [else #f])))
  (define normalize-request-headers
    (lambda (headers body)
      (let* ([length (body-length body)]
             [content-length (http-header-ref headers "Content-Length" #f)]
             [transfer-encoding (http-header-ref headers "Transfer-Encoding" #f)])
        (when (and content-length transfer-encoding)
          (raise-net-error 'http 'framing
                           "conflicting Content-Length and Transfer-Encoding headers" headers))
        (if (and length (not content-length) (not transfer-encoding))
            (http-header-add headers "Content-Length" (number->string length))
            headers))))
  (define base64-encode
    (lambda (bytes)
      (let ([alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"]
            [length (bytevector-length bytes)])
        (let loop ([index 0] [out '()])
          (if (>= index length)
              (apply string-append (reverse out))
              (let* ([remaining (- length index)]
                     [a (bytevector-u8-ref bytes index)]
                     [b (if (> remaining 1) (bytevector-u8-ref bytes (+ index 1)) 0)]
                     [c (if (> remaining 2) (bytevector-u8-ref bytes (+ index 2)) 0)])
                (loop (+ index 3)
                      (cons
                       (string (string-ref alphabet (fxsra a 2))
                               (string-ref alphabet
                                           (fxior (fxsll (fxand a 3) 4) (fxsra b 4)))
                               (if (> remaining 1)
                                   (string-ref alphabet
                                               (fxior (fxsll (fxand b 15) 2) (fxsra c 6)))
                                   #\=)
                               (if (> remaining 2)
                                   (string-ref alphabet (fxand c 63)) #\=))
                       out))))))))
  (define request-policy-headers
    (lambda (client request headers)
      (define string-prefix-of?
        (lambda (prefix value)
          (and (<= (string-length prefix) (string-length value))
               (string=? prefix (substring value 0 (string-length prefix))))))
      (define join-cookie-values
        (lambda (value*)
          (if (null? value*) ""
              (let loop ([rest (cdr value*)] [answer (car value*)])
                (if (null? rest) answer
                    (loop (cdr rest) (string-append answer "; " (car rest))))))))
      (let* ([auth (http-client-auth client)]
             [headers
              (cond
               [(and auth (eq? (car auth) 'basic))
                (http-header-set
                 headers "Authorization"
                 (string-append "Basic "
                                (base64-encode
                                 (string->utf8
                                  (string-append (car (cdr auth)) ":" (cdr (cdr auth)))))))]
               [(and auth (eq? (car auth) 'bearer))
                (http-header-set headers "Authorization"
                                 (string-append "Bearer " (cdr auth)))]
               [else headers])]
             [jar (http-client-cookie-jar client)]
             [uri (http-request-uri request)]
             [host (or (uri-host uri) "")]
             [path (or (uri-raw-path uri) "/")]
             [secure? (string-ci=? (or (uri-scheme uri) "http") "https")]
             [cookie* (if jar
                          (filter (lambda (cookie)
                                    (and (or (string=? (http-cookie-domain cookie) "")
                                             (string-ci=? host (http-cookie-domain cookie)))
                                         (string-prefix-of? (http-cookie-path cookie) path)
                                         (or (not (http-cookie-secure? cookie)) secure?)))
                                  (http-cookie-jar-cookies jar))
                          '())])
        (if (null? cookie*) headers
            (http-header-set
             headers "Cookie"
             (join-cookie-values
              (map (lambda (cookie)
                     (string-append (http-cookie-name cookie) "="
                                    (http-cookie-value cookie))) cookie*)))))))
  (define store-response-cookies!
    (lambda (client request headers)
      (let ([jar (http-client-cookie-jar client)]
            [host (or (uri-host (http-request-uri request)) "")])
        (when jar
          (for-each
           (lambda (header)
             (when (string-ci=? (car header) "Set-Cookie")
               (let* ([value (cdr header)]
                      [semi (let loop ([index 0])
                              (cond [(= index (string-length value)) #f]
                                    [(char=? (string-ref value index) #\;) index]
                                    [else (loop (fx1+ index))]))]
                      [pair (if semi (substring value 0 semi) value)]
                      [equals (let loop ([index 0])
                                (cond [(= index (string-length pair)) #f]
                                      [(char=? (string-ref pair index) #\=) index]
                                      [else (loop (fx1+ index))]))])
                 (when (and equals (positive? equals))
                   (let ([cookie (%make-http-cookie
                                  (substring pair 0 equals)
                                  (substring pair (fx1+ equals) (string-length pair))
                                  host "/" #f)])
                     (http-cookie-jar-cookies-set!
                      jar
                      (cons cookie
                            (filter (lambda (old)
                                      (not (and (string=? (http-cookie-name old)
                                                          (http-cookie-name cookie))
                                                (string-ci=? (http-cookie-domain old) host))))
                                    (http-cookie-jar-cookies jar)))))))))
           headers)))))
  (define decode-response-body
    (lambda (response)
      (let* ([headers (http-response-headers response)]
             [encoding (http-header-ref headers "Content-Encoding" #f)]
             [body (http-response-body response)])
        (if (and (bytevector? body) encoding
                 (or (string-ci=? encoding "gzip") (string-ci=? encoding "deflate")))
            (let ([handle (ffi-zlib-stream-open 0 (if (string-ci=? encoding "gzip") 1 0))])
              (when (zero? handle)
                (raise-net-error 'http 'unsupported "zlib decompression is unavailable" response))
              (dynamic-wind
                void
                (lambda ()
                  (let ([answer (ffi-zlib-stream-process handle body 0
                                                         (bytevector-length body) 1
                                                         (* 256 1024 1024))])
                    (unless (and (vector? answer) (= (vector-length answer) 2)
                                 (bytevector? (vector-ref answer 0)))
                      (raise-net-error 'http 'compression
                                       "HTTP response decompression failed" answer))
                    (%make-http-response
                     (http-response-status response) (http-response-reason response)
                     (filter (lambda (header)
                               (not (or (string-ci=? (car header) "Content-Encoding")
                                        (string-ci=? (car header) "Content-Length")))) headers)
                     (vector-ref answer 0) (http-response-trailers response)
                     (http-response-version response))))
                (lambda () (ffi-zlib-stream-close handle))))
            response))))
  (define dispatch
    (case-lambda
     [(c req sink)
      (dispatch c req sink (+ (monotonic-ms) (http-client-timeout-ms c)))]
     [(c req sink deadline)
      (ensure-open c)
      (let* ([u (http-request-uri req)]
             [tls? (string=? (string-downcase (or (uri-scheme u) "http")) "https")]
             [host (or (uri-host u) "")]
             [port (or (uri-port u) (if tls? 443 80))]
             [proxy (http-client-proxy c)]
             [proxy-uri (and proxy (http-proxy-uri proxy))]
             [proxy-host (and proxy-uri (or (uri-host proxy-uri) ""))]
             [proxy-port (and proxy-uri (or (uri-port proxy-uri) 8080))]
             [policy (http-client-pool-policy c)]
             [max-active (if policy (http-pool-policy-max-active policy) 64)]
             [max-idle (if policy (http-pool-policy-max-idle policy) 8)]
             [idle-timeout-ms (if policy (http-pool-policy-idle-timeout-ms policy) 30000)]
             ;; Capture mutable client policy before the operation becomes visible.
             [follow-redirects? (http-client-follow-redirects? c)]
             [source (body-producer (http-request-body req))]
             [headers (request-policy-headers
                       c req
                       (normalize-request-headers
                        (append (http-client-headers c) (http-request-headers req))
                        (http-request-body req)))]
             [policy-snapshot
              (make-http-request-policy
               headers (http-client-auth c) (http-client-cookie-jar c)
               (http-client-proxy c) #f (http-client-version c)
               policy follow-redirects? 10 deadline)]
             [vec (make-normalized-http-request
                   (http-request-method req) u (or (uri-scheme u) "http") host port tls?
                   (request-path u) headers source (body-length (http-request-body req))
                   policy-snapshot)]
             [sv (and sink
                      (vector (lambda (b i n) (http-body-sink-write! sink b i (+ i n)))
                              (lambda () (http-body-sink-finish! sink))))]
             [transport (or (http-client-transport c)
                            (let ([new (if (eq? (http-client-version c) 'h2)
                                           (make-lws-client-transport 'h2 64 65536 64 0
                                                                       (or proxy-host "")
                                                                       (or proxy-port 0) max-active
                                                                       max-idle idle-timeout-ms)
                                           (make-lws-client-transport 'http1 64 65536 64 0
                                                                       (or proxy-host "")
                                                                       (or proxy-port 0) max-active
                                                                       max-idle idle-timeout-ms))])
                              (http-client-transport-set! c new)
                              new))]
             [inner (lws-client-request/nonblocking transport vec sv)]
             [outer #f])
        (set! outer
              (make-net-operation
               'http
               (lambda ()
                 (net-operation-step! inner)
                 (case (net-operation-state inner)
                   [(completed)
                   (let* ([transport-response (net-operation-result inner)]
                          [response
                           (make-http-response
                            (transport-response-status transport-response)
                            (transport-response-reason transport-response)
                            (transport-response-headers transport-response)
                            (transport-response-body transport-response)
                            (transport-response-trailers transport-response)
                            (transport-response-version transport-response))])
                      (store-response-cookies! c req (http-response-headers response))
                      (http-client-active-set! c (remq outer (http-client-active c)))
                      (net-operation-completed (decode-response-body response)))]
                   [(failed cancelled)
                    (when sink
                      (guard (ignored [else (void)])
                        (http-body-sink-finish! sink)))
                    (http-client-active-set! c (remq outer (http-client-active c)))
                    (net-operation-failed (net-operation-condition inner))]
                   [else
                    (net-operation-pending (net-operation-poll-targets inner)
                                           (net-operation-deadline-ms inner))]))
               (lambda () (net-operation-cancel! inner))
               (lambda ()
                 (lws-client-release-operation! transport inner))))
        (http-client-active-set! c (cons outer (http-client-active c)))
        outer)]))
  #|proc:http-send/nonblocking
Starts request `request` on client `client`, optionally streaming response bytes to `sink`.
Returns a distinct nonblocking network operation covering redirects and retries. A sink consumer
has signature `(bytevector start count) -> unspecified` and its finisher has signature
`() -> unspecified`.
|#
  (define http-send/nonblocking
    (case-lambda
      [(c r) (http-send/nonblocking c r #f)]
      [(c r s)
       (pcheck ([http-client? c] [http-request? r]
                [(lambda (x) (or (not x) (http-body-sink? x))) s])
         (ensure-open c)
         (dispatch-with-redirects c r s (+ (monotonic-ms) (http-client-timeout-ms c))))]))
  (define redirect-status?
    (lambda (status)
      (memv status '(301 302 303 307 308))))
  (define redirected-request
    (lambda (request response)
      (let ([location (http-header-ref (http-response-headers response) "Location" #f)])
        (and location
             (let ([method (http-request-method request)])
               (when (and (memv (http-response-status response) '(307 308))
                          (http-body-source? (http-request-body request)))
                 (errorf 'http-send
                         "cannot replay a one-shot body source across a ~a redirect"
                         (http-response-status response)))
               (make-http-request
                (if (and (memv (http-response-status response) '(301 302 303))
                         (not (string-ci=? method "GET"))
                         (not (string-ci=? method "HEAD")))
                    "GET"
                    method)
                (uri-resolve (http-request-uri request) location)
                (http-request-headers request)
                (if (memv (http-response-status response) '(301 302 303)) #f
                    (http-request-body request))))))))
  (define dispatch-with-redirects
    (lambda (c request sink deadline)
      (let ([current request] [remaining 10] [child #f] [outer #f]
            [follow-redirects? (http-client-follow-redirects? c)])
        (set! child (dispatch c current sink deadline))
        (set! outer
              (make-net-operation
               'http
               (lambda ()
                 (net-operation-step! child)
                 (case (net-operation-state child)
                   [(completed)
                    (let* ([response (net-operation-result child)]
                           [next (and follow-redirects?
                                      (positive? remaining)
                                      (redirect-status? (http-response-status response))
                                      (redirected-request current response))])
                      (if next
                          (begin
                            (set! current next)
                            (set! remaining (fx1- remaining))
                            (set! child (dispatch c current sink deadline))
                            (net-operation-pending
                             (net-operation-poll-targets child) deadline))
                          (net-operation-completed response)))]
                   [(failed cancelled) (net-operation-failed (net-operation-condition child))]
                   [else (net-operation-pending (net-operation-poll-targets child)
                                                (net-operation-deadline-ms child))]))
               (lambda () (net-operation-cancel! child))
               (lambda ()
                 ;; Removal is idempotent so completion, cancellation, and close race safely.
                 (http-client-active-set! c (remq outer (http-client-active c))))))
        (http-client-active-set! c (cons outer (http-client-active c)))
        outer)))
  #|proc:http-send
Sends `request` through client `client`, waits for completion, and returns the HTTP response.
|#
  (define http-send
    (lambda (c r)
      (pcheck ([http-client? c] [http-request? r])
        (let ([deadline (+ (monotonic-ms) (http-client-timeout-ms c))])
          (net-operation-wait (dispatch-with-redirects c r #f deadline))))))
  (define one-shot (lambda (m u h b) (let ([c (http-open)]) (dynamic-wind void (lambda () (http-send c (make-http-request m u h b))) (lambda () (http-close c))))))
  (define make-verb (lambda (m) (case-lambda [(u) (one-shot m u '() #f)] [(c u) (http-send c (make-http-request m u '() #f))] [(c u b) (http-send c (make-http-request m u '() b))] [(c u h b) (http-send c (make-http-request m u h b))])))
  (define http-get (make-verb 'get)) (define http-head (make-verb 'head)) (define http-post (make-verb 'post)) (define http-put (make-verb 'put)) (define http-delete (make-verb 'delete))
  #|proc:http-request
Sends a one-shot request with `method`, `uri`, optional `headers`, and optional `body`.
Returns the HTTP response.
|#
  (define http-request (case-lambda [(m u) (one-shot m u '() #f)] [(m u h) (one-shot m u h #f)] [(m u h b) (one-shot m u h b)]))
  #|proc:http-request/nonblocking
Starts a request on `client` with `method`, `uri`, optional `headers`, and optional `body`.
Returns a distinct nonblocking network operation.
|#
  (define http-request/nonblocking (case-lambda [(c m u) (http-request/nonblocking c m u '() #f)] [(c m u h) (http-request/nonblocking c m u h #f)] [(c m u h b) (http-send/nonblocking c (make-http-request m u h b))]))
  #|proc:http-download
Downloads `uri` to file `path`, using optional `client`, and returns the HTTP response.
|#
  (define http-download
    (case-lambda
      [(u p)
       (let ([c (http-open)])
         (dynamic-wind
           void
           (lambda ()
             (http-download c u p))
           (lambda () (http-close c))))]
      [(c u p)
       (let ([sink (make-http-file-body-sink p)])
         (net-operation-wait
          (http-send/nonblocking c (make-http-request 'get u '() #f) sink)))]))
  #|proc:http-upload
Uploads file `path` to `uri` with PUT, using optional `client`, and returns the HTTP response.
|#
  (define http-upload (case-lambda [(u p) (one-shot 'put u '() (make-http-file-body-source p))] [(c u p) (http-send c (make-http-request 'put u '() (make-http-file-body-source p)))]))
  #|proc:http-download/nonblocking
Starts downloading `uri` through `client` to file `path`; returns a nonblocking network operation.
|#
  (define http-download/nonblocking
    (lambda (c u p)
      (pcheck ([http-client? c] [string? p])
        (http-send/nonblocking c (make-http-request 'get u '() #f)
                               (make-http-file-body-sink p)))))
  #|proc:http-upload/nonblocking
Starts uploading file `path` through `client` to `uri`; returns a nonblocking network operation.
|#
  (define http-upload/nonblocking
    (lambda (c u p)
      (pcheck ([http-client? c] [string? p])
        (http-request/nonblocking c 'put u '() (make-http-file-body-source p)))))
  #|proc:http-close
Closes client `client`, cancels its active operations, and returns `client`. Closing is idempotent.
|#
  (define http-close
    (lambda (c)
      (pcheck ([http-client? c])
        (unless (http-client-closed? c)
          ;; Reject new work before cancelling the snapshot of active operations.
          (http-client-closed?-set! c #t)
          (let ([active (http-client-active c)])
            (http-client-active-set! c '())
            (for-each net-operation-cancel! active))
          (when (http-client-transport c)
            (lws-client-close! (http-client-transport c))))
        c)))
  #|proc:http-follow-redirects!
Sets whether client `client` follows redirects to boolean `follow?` and returns `client`.
|#
  (define http-follow-redirects! (lambda (c x) (pcheck ([http-client? c] [boolean? x]) (http-client-follow-redirects?-set! c x) c)))
  #|proc:http-set-header!
Sets default header `name` to string `value` on client `client` and returns `client`.
|#
  (define http-set-header! (lambda (c n v) (pcheck ([http-client? c] [string? n v]) (http-client-headers-set! c (http-header-set (http-client-headers c) n v)) c)))
  #|proc:http-set-timeout!
Sets client `client`'s request timeout to `timeout-ms` milliseconds and returns `client`.
|#
  (define http-set-timeout! (lambda (c n) (pcheck ([http-client? c] [natural? n]) (http-client-timeout-ms-set! c n) c)))
  #|proc:http-cancel-pending!
Cancels a snapshot of all active operations on client `client` and returns `client`.
|#
  (define http-cancel-pending!
    (lambda (c)
      (pcheck ([http-client? c])
        (let ([active (http-client-active c)])
          (http-client-active-set! c '())
          (for-each net-operation-cancel! active)
          c))))
  #|proc:http-client-cookie-jar-set!
Sets client `client`'s cookie jar to `cookie-jar` or `#f` and returns `client`.
|#
  (define http-client-cookie-jar-set!
    (lambda (c x) (pcheck ([http-client? c] [(lambda (v) (or (not v) (http-cookie-jar? v))) x])
                    (%http-client-cookie-jar-set! c x) c)))
  #|proc:http-client-auth-set!
Sets authentication on client `client` and returns `client`. `scheme` is `basic`, `bearer`, a
procedure, or `#f`; `credential` is a user/password pair for Basic or a token for Bearer. An auth
procedure has signature `(request response-or-#f) -> request`.
|#
  (define http-client-auth-set!
    (case-lambda
      [(c scheme) (http-client-auth-set! c scheme #f)]
      [(c scheme credential)
       (pcheck ([http-client? c])
         (unless (or (not scheme) (procedure? scheme) (memq scheme '(basic bearer)))
           (errorf 'http-client-auth-set! "expected basic, bearer, procedure, or #f"))
         (when (eq? scheme 'basic)
           (unless (and (pair? credential) (string? (car credential))
                        (string? (cdr credential)))
             (errorf 'http-client-auth-set! "basic credential must be (user . password)")))
         (when (eq? scheme 'bearer)
           (unless (string? credential)
             (errorf 'http-client-auth-set! "bearer credential must be a string")))
         (%http-client-auth-set! c (and scheme (cons scheme credential)))
         c)]))
  #|proc:http-client-proxy-set!
Sets client `client`'s proxy to `proxy` or `#f`, retires incompatible transport, and returns client.
|#
  (define http-client-proxy-set!
    (lambda (c x) (pcheck ([http-client? c] [(lambda (v) (or (not v) (http-proxy? v))) x])
                    (unless (eq? x (http-client-proxy c))
                      (when (http-client-transport c)
                        (lws-client-close! (http-client-transport c))
                        (http-client-transport-set! c #f)))
                    (%http-client-proxy-set! c x) c)))
  #|proc:http-client-pool-policy-set!
Sets `pool-policy` for future operations on client `client` and returns `client`.
|#
  (define http-client-pool-policy-set!
    (lambda (c x) (pcheck ([http-client? c] [http-pool-policy? x])
                    (%http-client-pool-policy-set! c x) c)))
  #|proc:http-client-version-set!
Sets client `client`'s protocol policy to `version` and returns `client`. `version` is `auto`,
`http/1.1`, or `h2`; incompatible transport is retired.
|#
  (define http-client-version-set!
    (lambda (c x) (pcheck ([http-client? c] [symbol? x])
                    (unless (memq x '(auto http/1.1 h2))
                      (errorf 'http-client-version-set!
                              "expected auto, http/1.1, or h2"))
                    (unless (eq? x (http-client-version c))
                      (when (http-client-transport c)
                        (lws-client-close! (http-client-transport c))
                        (http-client-transport-set! c #f)))
                    (%http-client-version-set! c x) c)))
  #|proc:make-http-multipart-body
Encodes multipart parts `parts`. Returns a body source and its multipart content-type string.
|#
  (define make-http-multipart-body
    (lambda (part*)
      (pcheck ([list? part*])
        (unless (andmap http-multipart-part? part*)
          (errorf 'make-http-multipart-body "expected a list of multipart parts"))
        (let ([boundary "chezpp-7d9e4f6a2b1c"])
          (define contains-newline?
            (lambda (value)
              (let loop ([index 0])
                (and (< index (string-length value))
                     (let ([char (string-ref value index)])
                       (or (char=? char #\return) (char=? char #\newline)
                           (loop (fx1+ index))))))))
          (let* ([pieces
                  (append
                   (fold-right
                    append '()
                    (map
                     (lambda (part)
                       (let ([name (http-multipart-part-name part)]
                             [filename (http-multipart-part-filename part)]
                             [value (http-multipart-part-value part)])
                         (when (or (contains-newline? name)
                                   (and filename (contains-newline? filename)))
                           (errorf 'make-http-multipart-body
                                   "multipart name or filename contains a newline"))
                         (list
                          (string->utf8
                           (string-append
                            "--" boundary "\r\nContent-Disposition: form-data; name=\""
                            name "\"" (if filename (string-append "; filename=\"" filename "\"") "")
                            "\r\n" (if (http-multipart-part-content-type part)
                                       (string-append "Content-Type: "
                                                      (http-multipart-part-content-type part) "\r\n") "")
                            "\r\n"))
                          (if (string? value) (string->utf8 value) value)
                          (string->utf8 "\r\n")))) part*))
                   (list (string->utf8 (string-append "--" boundary "--\r\n"))))]
                 [body (bytevector-concatenate pieces)])
            (values (make-http-body-source (body-producer body) (bytevector-length body))
                    (string-append "multipart/form-data; boundary=" boundary)))))))
  (define ensure-http-server-open
    (lambda (who server)
      (when (http-server-closed? server) (errorf who "HTTP server is closed"))))

  (define handler-key
    (case-lambda
      [(path) path]
      [(method path)
       (cons (if (symbol? method) (string-upcase (symbol->string method))
                 (string-upcase method)) path)]))

  #|proc:http-listen
The `http-listen` procedure opens an LWS HTTP server on `host` and `port`. `tls-context` is `#f`
or a server TLS context, and `backlog` is retained for API compatibility. It returns the server.
|#
  (define-who http-listen
    (case-lambda
      [(host port) (http-listen host port #f 128)]
      [(host port tls-context) (http-listen host port tls-context 128)]
      [(host port tls-context backlog)
       (pcheck ([string? host] [fixnum? port backlog])
         (unless (and (fxpositive? port) (fx<= port 65535))
           (errorf who "invalid port ~s" port))
         (unless (fxpositive? backlog) (errorf who "invalid backlog ~s" backlog))
         (unless (or (not tls-context) (tls-context? tls-context))
           (errorf who "expected #f or a TLS context"))
         (%make-http-server
          (make-lws-http-server host port (if tls-context
                                              (tls-context-native-handle tls-context) 0))
          (make-hashtable equal-hash equal?) (make-mutex 'http-server) #f))]))

  #|proc:http-server-close
The `http-server-close` procedure closes `server` and active logical requests. It returns `server`.
|#
  (define http-server-close
    (lambda (server)
      (pcheck ([http-server? server])
        (unless (http-server-closed? server)
          (http-server-closed?-set! server #t)
          (lws-http-server-close! (http-server-transport server)))
        server)))

  #|proc:http-register-handler!
The `http-register-handler!` procedure registers `handler` for `path` and optional `method` on
`server`. A handler has signature `(http-request) -> http-response`. It returns the old handler.
|#
  (define http-register-handler!
    (case-lambda
      [(server path handler)
       (http-register-handler! server #f path handler)]
      [(server method path handler)
       (pcheck ([http-server? server] [string? path] [procedure? handler])
         (ensure-http-server-open 'http-register-handler! server)
         (with-mutex (http-server-mutex server)
           (let* ([key (if method (handler-key method path) path)]
                  [old (hashtable-ref (http-server-handlers server) key #f)])
             (hashtable-set! (http-server-handlers server) key handler)
             old)))]))

  #|proc:http-handler-ref
The `http-handler-ref` procedure returns the handler for `method` and `path` on `server`, or
`default` when no method-specific or path handler exists.
|#
  (define http-handler-ref
    (lambda (server method path default)
      (pcheck ([http-server? server] [string? path])
        (with-mutex (http-server-mutex server)
          (or (hashtable-ref (http-server-handlers server) (handler-key method path) #f)
              (hashtable-ref (http-server-handlers server) path default))))))

  #|proc:http-unregister-handler!
The `http-unregister-handler!` procedure removes the handler for `path` and optional `method` from
`server`. It returns the removed handler or `#f`.
|#
  (define http-unregister-handler!
    (case-lambda
      [(server path) (http-unregister-handler! server #f path)]
      [(server method path)
       (pcheck ([http-server? server] [string? path])
         (with-mutex (http-server-mutex server)
           (let* ([key (if method (handler-key method path) path)]
                  [old (hashtable-ref (http-server-handlers server) key #f)])
             (hashtable-delete! (http-server-handlers server) key)
             old)))]))

  (define wrap-server-request
    (lambda (handle) (and handle (%make-http-connection handle #f #f))))

  #|proc:http-accept/nonblocking
The `http-accept/nonblocking` procedure returns the next logical request connection from `server`,
or `#f` when no complete request headers are ready.
|#
  (define http-accept/nonblocking
    (lambda (server)
      (pcheck ([http-server? server])
        (ensure-http-server-open 'http-accept/nonblocking server)
        (wrap-server-request
         (lws-http-server-accept/nonblocking (http-server-transport server))))))

  #|proc:http-accept
The `http-accept` procedure waits for the next logical request on `server` and returns its
connection handle.
|#
  (define http-accept
    (lambda (server)
      (pcheck ([http-server? server])
        (ensure-http-server-open 'http-accept server)
        (wrap-server-request (lws-http-server-accept (http-server-transport server))))))

  #|proc:http-connection-close
The `http-connection-close` procedure cancels logical `connection` without closing HTTP/2 siblings.
It returns `connection` and is idempotent.
|#
  (define http-connection-close
    (lambda (connection)
      (pcheck ([http-connection? connection])
        (unless (http-connection-closed? connection)
          (http-connection-closed?-set! connection #t)
          (lws-http-request-close! (http-connection-request-handle connection)))
        connection)))

  #|proc:http-read-request
The `http-read-request` procedure materializes and returns the request represented by `connection`.
|#
  (define http-read-request
    (lambda (connection)
      (pcheck ([http-connection? connection])
        (when (http-connection-closed? connection)
          (errorf 'http-read-request "HTTP connection is closed"))
        (or (http-connection-request connection)
            (let* ([handle (http-connection-request-handle connection)]
                   [request (make-http-request
                             (lws-http-request-method handle)
                             (lws-http-request-path handle) '()
                             (and (lws-http-request-has-body? handle)
                                  (lws-http-request-read-body handle)))])
              (http-connection-request-set! connection request)
              request)))))

  #|proc:http-read-request/nonblocking
The `http-read-request/nonblocking` procedure returns the already accepted request from
`connection`; accepted LWS requests always have complete headers.
|#
  (define http-read-request/nonblocking http-read-request)

  (define response-bytes
    (lambda (body)
      (cond [(not body) #vu8()]
            [(bytevector? body) body]
            [(string? body) (string->utf8 body)]
            [else (errorf 'http-write-response "unsupported response body ~s" body)])))

  #|proc:http-write-response/nonblocking
The `http-write-response/nonblocking` procedure queues `response` for logical `connection`. It
returns `response` when accepted and `#f` when reactor backpressure rejects the command.
|#
  (define http-write-response/nonblocking
    (lambda (connection response)
      (pcheck ([http-connection? connection] [http-response? response])
        (and (not (http-connection-closed? connection))
             (lws-http-request-write-response!
              (http-connection-request-handle connection) (http-response-status response)
              (response-bytes (http-response-body response)) #t)
             response))))

  #|proc:http-write-response
The `http-write-response` procedure queues `response` for logical `connection` and returns it.
It raises an error if the bounded reactor command queue is full.
|#
  (define http-write-response
    (lambda (connection response)
      (pcheck ([http-connection? connection] [http-response? response])
        (or (http-write-response/nonblocking connection response)
            (errorf 'http-write-response "server response queue is full")))))

  (define serve-one
    (lambda (server connection)
      (let* ([request (http-read-request connection)]
             [path (or (uri-raw-path (http-request-uri request)) "/")]
             [handler (http-handler-ref server (http-request-method request) path #f)]
             [response
              (guard (condition [else (make-http-response 500 "Internal Server Error" '()
                                                         #vu8() '() 'h1)])
                (if handler (handler request)
                    (make-http-response 404 "Not Found" '() #vu8() '() 'h1)))])
        (unless (http-response? response)
          (set! response (make-http-response 500 "Internal Server Error" '() #vu8() '() 'h1)))
        (http-write-response connection response)
        response)))

  #|proc:http-serve
The `http-serve` procedure accepts and dispatches one logical request on `server`. Handlers run
outside reactor and native locks. It returns the handler response.
|#
  (define http-serve
    (lambda (server)
      (pcheck ([http-server? server]) (serve-one server (http-accept server)))))

  #|proc:http-serve-loop
The `http-serve-loop` procedure dispatches requests until `server` closes and then returns it.
|#
  (define http-serve-loop
    (lambda (server)
      (pcheck ([http-server? server])
        (let loop ()
          (unless (http-server-closed? server)
            (let ([connection (http-accept/nonblocking server)])
              (if connection (serve-one server connection)
                  ($sleep (make-time 'time-duration 1000000 0))))
            (loop)))
        server)))
  )
