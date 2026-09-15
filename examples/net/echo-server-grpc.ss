(import (chezpp)
        (chezpp net))

(load "examples/net/net-example-common.ss")

(define grpc-echo-method "/chezpp.examples.Echo/Unary")

#|proc:grpc-echo-server
The `grpc-echo-server` procedure listens for unary gRPC echo requests, prints
each received Scheme value with a monotonically increasing count, and replies
with the same value.
|#
(define grpc-echo-server
  (lambda (host port)
    (pcheck ([string? host] [fixnum? port])
      (with-grpc-example-env
       (lambda ()
         (let ([server (grpc-open-channel 'server host port)]
               [count 0])
           (grpc-register-service!
            server
            grpc-echo-method
            (lambda (request)
              (let ([value (string->scheme-value
                            (utf8->string
                             (grpc-request-payload request)))])
                (set! count (+ count 1))
                (printf "grpc message ~a: ~s~n" count value)
                (scheme-value->string value))))
           (dynamic-wind
             void
             (lambda ()
               (let loop ()
                 (grpc-serve server)
                 (loop)))
             (lambda ()
               (grpc-close-channel server)))))))))

(let ([arg* (command-line-arguments)])
  (cond
   [(null? arg*)
    (grpc-echo-server grpc-echo-example-host grpc-echo-example-port)]
   [(= (length arg*) 2)
    (grpc-echo-server (car arg*)
                      (parse-port-argument 'grpc-echo-server (cadr arg*)))]
   [else
    (errorf 'grpc-echo-server
            "expected zero arguments or host/port, given ~s"
            arg*)]))
