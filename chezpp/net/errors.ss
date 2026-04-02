(library (chezpp net errors)
  (export make-net-error
          net-error?
          net-error-who
          net-error-kind
          net-error-message
          net-error-data
          raise-net-error)
  (import (chezpp chez)
          (chezpp utils))

  (define-condition-type &net-error &error
    %make-net-error
    net-error?
    (who net-error-who)
    (kind net-error-kind)
    (message net-error-message)
    (data net-error-data))

  #|proc:make-net-error
The `make-net-error` procedure constructs a structured network error condition.
|#
  (define-who make-net-error
    (case-lambda
      [(who kind message)
       (make-net-error who kind message #f)]
      [(who kind message data)
       (pcheck ([symbol? kind] [string? message])
               (unless (or (symbol? who) (eq? who #f))
                 (errorf who "expected symbol or #f for error source"))
               (%make-net-error who kind message data))]))

  #|proc:raise-net-error
The `raise-net-error` procedure raises a structured network error condition.
|#
  (define-who raise-net-error
    (case-lambda
      [(who kind message)
       (raise-net-error who kind message #f)]
      [(who kind message data)
       (raise (make-net-error who kind message data))]))
  )
