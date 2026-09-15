(import (chezpp)
        (chezpp net))

(load "examples/net/net-example-common.ss")

(define grpc-echo-method "/chezpp.examples.Echo/Unary")

#|proc:grpc-echo-client
The `grpc-echo-client` procedure sends a fixed set of common Scheme values to a
gRPC echo server and prints each echoed value.
|#
(define grpc-echo-client
  (lambda (host port)
    (pcheck ([string? host] [fixnum? port])
      (with-grpc-example-env
       (lambda ()
         (let ([client (grpc-open-channel host port)])
           (dynamic-wind
             void
             (lambda ()
               (for-each
                (lambda (value)
                  (let* ([response (grpc-call client
                                              grpc-echo-method
                                              (scheme-value->string value)
                                              '()
                                              30000)]
                         [echoed (string->scheme-value
                                  (utf8->string
                                   (grpc-response-payload response)))])
                    (printf "grpc echo: ~s~n" echoed)))
                example-scheme-value*))
             (lambda ()
               (grpc-close-channel client)))))))))

(let ([arg* (command-line-arguments)])
  (cond
   [(null? arg*)
    (grpc-echo-client grpc-echo-example-host grpc-echo-example-port)]
   [(= (length arg*) 2)
    (grpc-echo-client (car arg*)
                      (parse-port-argument 'grpc-echo-client (cadr arg*)))]
   [else
    (errorf 'grpc-echo-client
            "expected zero arguments or host/port, given ~s"
            arg*)]))
