(library (chezpp parser wasm private)
  (export list->immutable-vector)
  (import (chezpp chez)
          (chezpp utils))

  #|proc:list->immutable-vector
  Converts list `value*` to an immutable vector and returns that vector.
  |#
  (define list->immutable-vector
    (lambda (value*)
      (pcheck ([list? value*])
              (vector->immutable-vector (list->vector value*)))))
  )
