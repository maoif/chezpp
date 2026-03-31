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
          http-header-add)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp net uri))

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

  (define normalize-http-method
    (lambda (who method)
      (cond
       [(string? method) (string-upcase method)]
       [(symbol? method) (string-upcase (symbol->string method))]
       [else (errorf who "expected method string or symbol, given ~s" method)])))

  (define normalize-http-uri
    (lambda (who u)
      (cond
       [(uri? u) u]
       [(string? u)
        (or (string->uri u)
            (errorf who "invalid URI string ~s" u))]
       [else (errorf who "expected URI object or string, given ~s" u)])))

  (define normalize-http-headers
    (lambda (who headers)
      (unless (list? headers)
        (errorf who "expected header association list, given ~s" headers))
      (map (lambda (entry)
             (unless (pair? entry)
               (errorf who "expected header pair, given ~s" entry))
             (let ([name (car entry)] [value (cdr entry)])
               (unless (or (string? name) (symbol? name))
                 (errorf who "expected header name string or symbol, given ~s" name))
               (unless (string? value)
                 (errorf who "expected header value string, given ~s" value))
               (cons (if (symbol? name) (symbol->string name) name)
                     value)))
           headers)))

  (define normalize-http-status
    (lambda (who status)
      (unless (and (integer? status) (exact? status) (<= 100 status) (<= status 599))
        (errorf who "expected HTTP status in [100, 599], given ~s" status))
      status))

  (define default-reason
    (lambda (status)
      (cond
       [(= status 200) "OK"]
       [(= status 201) "Created"]
       [(= status 204) "No Content"]
       [(= status 301) "Moved Permanently"]
       [(= status 302) "Found"]
       [(= status 400) "Bad Request"]
       [(= status 401) "Unauthorized"]
       [(= status 403) "Forbidden"]
       [(= status 404) "Not Found"]
       [(= status 500) "Internal Server Error"]
       [(= status 502) "Bad Gateway"]
       [(= status 503) "Service Unavailable"]
       [else ""])))

  (define normalize-http-header-name
    (lambda (who name)
      (cond
       [(string? name) name]
       [(symbol? name) (symbol->string name)]
       [else (errorf who "expected header name string or symbol, given ~s" name)])))

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
       (%make-http-request
        (normalize-http-method who method)
        (normalize-http-uri who uri)
        (normalize-http-headers who headers)
        body)]))

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
               (%make-http-response
                (normalize-http-status who status)
                reason
                (normalize-http-headers who headers)
                body))]))

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
         (let loop ([headers headers])
           (if (null? headers)
               default
               (if (string-ci=? (caar headers) name)
                   (cdar headers)
                   (loop (cdr headers))))))]))

  #|proc:http-header-set
The `http-header-set` procedure returns a header list with a single value for the named header.
|#
  (define-who http-header-set
    (lambda (headers name value)
      (pcheck ([string? value])
              (let ([headers (normalize-http-headers who headers)]
                    [name (normalize-http-header-name who name)])
                (let loop ([headers headers] [out '()] [seen? #f])
                  (cond
                   [(null? headers)
                    (reverse (cons (cons name value) out))]
                   [(string-ci=? (caar headers) name)
                    (if seen?
                        (loop (cdr headers) out seen?)
                        (loop (cdr headers) (cons (cons name value) out) #t))]
                   [else
                    (loop (cdr headers) (cons (car headers) out) seen?)]))))))

  #|proc:http-header-add
The `http-header-add` procedure returns a header list with an additional value appended for the named header.
|#
  (define-who http-header-add
    (lambda (headers name value)
      (pcheck ([string? value])
              (append (normalize-http-headers who headers)
                      (list (cons (normalize-http-header-name who name) value))))))
  )
