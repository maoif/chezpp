(import (except (chezpp) http-download))

(load "examples/net/net-example-common.ss")

#|proc:http-download
The `http-download` procedure fetches `uri` with protocol policy `version`, which is `auto`,
`http/1.1`, or `h2`. When omitted, `version` defaults to `auto`. It writes the response body to
standard output, writes the negotiated response version to standard error, and returns the response.
|#
(define http-download
  (case-lambda
    [(uri) (http-download uri 'auto)]
    [(uri version)
     (pcheck ([string? uri] [symbol? version])
       (let ([client (http-open)])
         (dynamic-wind
           void
           (lambda ()
             (http-client-version-set! client version)
             (let* ([response (http-get client uri)]
                    [body (http-response-body response)])
               (call-with-example-output-ports
                (lambda (out err)
                  (cond
                   [(bytevector? body)
                    (put-bytevector out body)]
                   [(string? body)
                    (put-bytevector out (string->utf8 body))]
                   [else
                    (errorf 'http-download
                            "unexpected HTTP body ~s"
                            body)])
                  (fprintf err "Negotiated HTTP version: ~a~n"
                           (http-response-version response))
                  (flush-output-port out)
                  (flush-output-port err)))
               response))
           (lambda () (http-close client)))))]))

(let ([arg* (command-line-arguments)])
  (case (length arg*)
    [(1) (http-download (car arg*))]
    [(2) (http-download (car arg*) (string->symbol (cadr arg*)))]
    [else
     (errorf 'http-download
             "expected URI and optional auto, http/1.1, or h2 argument, given ~s"
             arg*)]))
