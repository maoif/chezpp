(library (chezpp system common)
  (export system-error?
          make-system-error
          system-unsupported-error?
          make-system-unsupported-error
          system-not-found-error?
          make-system-not-found-error
          system-permission-error?
          make-system-permission-error
          system-timeout-error?
          make-system-timeout-error
          system-exit-error?
          make-system-exit-error
          system-error-operation
          system-error-code
          system-error-message
          system-error-context
          raise-system-error
          raise-system-unsupported
          ffi-result-ref)
  (import (chezpp chez)
          (chezpp utils))

  #|proc:system-error?
The `system-error?` procedure returns `#t` when its argument is a system error condition, otherwise `#f`.
The `condition` parameter is the object to test.
|#
  #|proc:system-error-operation
The `system-error-operation` procedure returns the operation stored in a system error condition.
The `condition` parameter is a system error condition.
|#
  #|proc:system-error-code
The `system-error-code` procedure returns the error code stored in a system error condition, or `#f` when no code is available.
The `condition` parameter is a system error condition.
|#
  #|proc:system-error-message
The `system-error-message` procedure returns the message stored in a system error condition.
The `condition` parameter is a system error condition.
|#
  #|proc:system-error-context
The `system-error-context` procedure returns the association-list context stored in a system error condition.
The `condition` parameter is a system error condition.
|#
  (define-condition-type &system-error &error
    %make-system-error
    system-error?
    (operation system-error-operation)
    (code system-error-code)
    (message system-error-message)
    (context system-error-context))

  #|proc:system-unsupported-error?
The `system-unsupported-error?` procedure returns `#t` when its argument is an unsupported system operation condition, otherwise `#f`.
The `condition` parameter is the object to test.
|#
  (define-condition-type &system-unsupported-error &system-error
    %make-system-unsupported-error
    system-unsupported-error?)

  #|proc:system-not-found-error?
The `system-not-found-error?` procedure returns `#t` when its argument is a system not-found condition, otherwise `#f`.
The `condition` parameter is the object to test.
|#
  (define-condition-type &system-not-found-error &system-error
    %make-system-not-found-error
    system-not-found-error?)

  #|proc:system-permission-error?
The `system-permission-error?` procedure returns `#t` when its argument is a system permission condition, otherwise `#f`.
The `condition` parameter is the object to test.
|#
  (define-condition-type &system-permission-error &system-error
    %make-system-permission-error
    system-permission-error?)

  #|proc:system-timeout-error?
The `system-timeout-error?` procedure returns `#t` when its argument is a system timeout condition, otherwise `#f`.
The `condition` parameter is the object to test.
|#
  (define-condition-type &system-timeout-error &system-error
    %make-system-timeout-error
    system-timeout-error?)

  #|proc:system-exit-error?
The `system-exit-error?` procedure returns `#t` when its argument is a system process-exit condition, otherwise `#f`.
The `condition` parameter is the object to test.
|#
  (define-condition-type &system-exit-error &system-error
    %make-system-exit-error
    system-exit-error?)

  (define $system-operation?
    (lambda (x)
      (or (symbol? x) (eq? x #f))))

  (define $system-code?
    (lambda (x)
      (or (integer? x) (eq? x #f))))

  (define $system-context?
    (lambda (x)
      (list? x)))

  #|proc:make-system-error
The `make-system-error` procedure constructs a system error condition.
The `operation` parameter is a symbol naming the operation that failed, or `#f` when unknown.
The `code` parameter is an integer error code, or `#f` when no code is available.
The `message` parameter is a string describing the failure.
The `context` parameter is an association list with additional failure details.
|#
  (define make-system-error
    (lambda (operation code message context)
      (pcheck ([$system-operation? operation] [$system-code? code] [string? message] [$system-context? context])
              (%make-system-error operation code message context))))

  (define $make-special-system-error
    (lambda (make-condition operation message context)
      (pcheck ([$system-operation? operation] [string? message] [$system-context? context])
              (make-condition operation #f message context))))

  #|proc:make-system-unsupported-error
The `make-system-unsupported-error` procedure constructs a system error condition for an unsupported operation.
The `operation` parameter is a symbol naming the unsupported operation, or `#f` when unknown.
The `message` parameter is a string describing the failure.
The `context` parameter is an association list with additional failure details.
|#
  (define make-system-unsupported-error
    (case-lambda
      [(operation message)
       (make-system-unsupported-error operation message '())]
      [(operation message context)
       ($make-special-system-error %make-system-unsupported-error operation message context)]))

  #|proc:make-system-not-found-error
The `make-system-not-found-error` procedure constructs a system error condition for a missing system resource.
The `operation` parameter is a symbol naming the operation that failed, or `#f` when unknown.
The `message` parameter is a string describing the failure.
The `context` parameter is an association list with additional failure details.
|#
  (define make-system-not-found-error
    (case-lambda
      [(operation message)
       (make-system-not-found-error operation message '())]
      [(operation message context)
       ($make-special-system-error %make-system-not-found-error operation message context)]))

  #|proc:make-system-permission-error
The `make-system-permission-error` procedure constructs a system error condition for a permission failure.
The `operation` parameter is a symbol naming the operation that failed, or `#f` when unknown.
The `message` parameter is a string describing the failure.
The `context` parameter is an association list with additional failure details.
|#
  (define make-system-permission-error
    (case-lambda
      [(operation message)
       (make-system-permission-error operation message '())]
      [(operation message context)
       ($make-special-system-error %make-system-permission-error operation message context)]))

  #|proc:make-system-timeout-error
The `make-system-timeout-error` procedure constructs a system error condition for a timeout.
The `operation` parameter is a symbol naming the operation that failed, or `#f` when unknown.
The `message` parameter is a string describing the failure.
The `context` parameter is an association list with additional failure details.
|#
  (define make-system-timeout-error
    (case-lambda
      [(operation message)
       (make-system-timeout-error operation message '())]
      [(operation message context)
       ($make-special-system-error %make-system-timeout-error operation message context)]))

  #|proc:make-system-exit-error
The `make-system-exit-error` procedure constructs a system error condition for an unsuccessful process exit.
The `operation` parameter is a symbol naming the operation that failed, or `#f` when unknown.
The `message` parameter is a string describing the failure.
The `context` parameter is an association list with additional failure details.
|#
  (define make-system-exit-error
    (case-lambda
      [(operation message)
       (make-system-exit-error operation message '())]
      [(operation message context)
       ($make-special-system-error %make-system-exit-error operation message context)]))

  #|proc:raise-system-error
The `raise-system-error` procedure raises a system error condition.
The `operation` parameter is a symbol naming the operation that failed, or `#f` when unknown.
The `code` parameter is an integer error code, or `#f` when no code is available.
The `message` parameter is a string describing the failure.
The `context` parameter is an association list with additional failure details.
|#
  (define raise-system-error
    (lambda (operation code message context)
      (raise (make-system-error operation code message context))))

  #|proc:raise-system-unsupported
The `raise-system-unsupported` procedure raises a system error condition for an unsupported operation.
The `operation` parameter is a symbol naming the unsupported operation, or `#f` when unknown.
The `message` parameter is a string describing the failure.
|#
  (define raise-system-unsupported
    (lambda (operation message)
      (raise (make-system-unsupported-error operation message))))

  (define $context-or-empty
    (lambda (x)
      (if (list? x) x '())))

  (define $operation->symbol
    (lambda (x)
      (cond [(symbol? x) x]
            [(string? x) (string->symbol x)]
            [else #f])))

  (define $errno-condition
    (lambda (operation code message context)
      (if (memv code '(1 13))
          (raise (make-system-permission-error operation message context))
          (raise (make-system-error operation code message context)))))

  #|proc:ffi-result-ref
The `ffi-result-ref` procedure decodes a tagged C FFI result vector.
The `result` parameter is a vector tagged with a string: `"ok"` returns its value, `"errno"` raises a system error, `"not-found"` raises a not-found error, and `"unsupported"` raises an unsupported error.
|#
  (define ffi-result-ref
    (lambda (result)
      (pcheck ([vector? result])
              (let ([tag (and (fx< 0 (vector-length result)) (vector-ref result 0))])
                (cond
                  [(and (string? tag) (string=? tag "ok") (fx= 2 (vector-length result)))
                   (vector-ref result 1)]
                  [(and (string? tag) (string=? tag "errno") (fx= 5 (vector-length result)))
                   ($errno-condition ($operation->symbol (vector-ref result 1))
                                     (vector-ref result 2)
                                     (vector-ref result 3)
                                     ($context-or-empty (vector-ref result 4)))]
                  [(and (string? tag) (string=? tag "not-found") (fx= 3 (vector-length result)))
                   (raise (make-system-not-found-error ($operation->symbol (vector-ref result 1)) "not found"
                                                       ($context-or-empty (vector-ref result 2))))]
                  [(and (string? tag) (string=? tag "unsupported") (fx= 2 (vector-length result)))
                   (raise (make-system-unsupported-error ($operation->symbol (vector-ref result 1)) "unsupported"))]
                  [else (errorf 'ffi-result-ref "invalid FFI result: ~a" result)])))))
  )
