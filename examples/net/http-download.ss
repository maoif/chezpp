(import (except (chezpp) http-download))

(load "examples/net/net-example-common.ss")

#|proc:http-download
The `http-download` procedure fetches the body at `uri` via HTTP or HTTPS and
writes the received bytes to standard output.
|#
(define http-download
  (lambda (uri)
    (pcheck ([string? uri])
      (let ([response (http-get uri)])
        (let ([body (http-response-body response)])
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
             (flush-output-port out))))
        response))))

(let ([arg* (command-line-arguments)])
  (unless (= (length arg*) 1)
    (errorf 'http-download
            "expected exactly one URI argument, given ~s"
            arg*))
  (http-download (car arg*)))
