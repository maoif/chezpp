(import (cspp)
        (cspp net))

(load "examples/net/legacy-example-common.ss")
(net-example-help! 'grpc-echo-client "Echo Scheme values over gRPC. Positional arguments: [HOST PORT].")

(load "examples/net/net-example-common.ss")

(define grpc-echo-method "/cspp.examples.Echo/Unary")

#|proc:grpc-echo-client
The `grpc-echo-client` procedure sends a fixed set of common Scheme values to a
gRPC echo server and prints each echoed value.
|#
(define grpc-echo-client
  (lambda (host port)
    (pcheck ([string? host] [fixnum? port])
      (with-grpc-example-env
       (lambda ()
         ;; Open the gRPC channel used by the unary echo calls.
         (let ([client (grpc-open-channel host port)])
           (dynamic-wind
             void
             (lambda ()
               (for-each
                (lambda (value)
                  ;; Send the serialized Scheme value to the unary echo method.
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
               ;; Release the channel after all echo calls or an error.
               (grpc-close-channel client)))))))))

(call-with-legacy-example-log
 'grpc-echo-client
 (lambda ()
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
               arg*)]))))
