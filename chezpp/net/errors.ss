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

  (define-record-type (net-error-record %make-net-error net-error?)
    (sealed #t)
    (opaque #f)
    (fields (immutable who net-error-who)
            (immutable kind net-error-kind)
            (immutable message net-error-message)
            (immutable data net-error-data)))

  #|proc:make-net-error
The `make-net-error` procedure constructs a structured network error record.
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
The `raise-net-error` procedure raises a structured network error record.
|#
  (define-who raise-net-error
    (case-lambda
      [(who kind message)
       (raise-net-error who kind message #f)]
      [(who kind message data)
       (raise (make-net-error who kind message data))]))
  )
