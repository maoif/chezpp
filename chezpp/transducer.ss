(library (chezpp transducer)
  (export transducer? reducer? reduced reduced? unreduced ensure-reduced
          preserving-reduced completion
          make-transducer transducer-name tidentity tcompose tchain
          tmap tfilter tremove tkeep
          ttake tdrop ttake-while tdrop-while
          transduce into
          list-transduce vector-transduce bytevector-transduce
          string-transduce iter-transduce
          make-reducer reducer-name rflist rfreverselist rfvector
          rfcount rfsum rffxsum rfflsum
          eduction eduction? transducible?)
  (import (chezpp chez)
          (chezpp internal)
          (chezpp iter)
          (chezpp utils))

  (define-record-type ($reduced mk-$reduced $reduced?)
    (fields (immutable value))
    (opaque #t)
    (sealed #t))

  (define-record-type ($reducer mk-$reducer $reducer?)
    (fields (immutable name)
            (immutable proc)
            (immutable metadata))
    (opaque #t)
    (sealed #t))

  (define-record-type ($transducer mk-$transducer $transducer?)
    (fields (immutable name)
            (immutable reducer-proc)
            (immutable metadata))
    (opaque #t)
    (sealed #t))

  (define-record-type ($eduction mk-$eduction $eduction?)
    (fields (immutable xform)
            (immutable source))
    (opaque #t)
    (sealed #t))

  #|proc:transducer?
  The `transducer?` procedure returns `#t` when `x` is a transducer record.
  |#
  (define transducer? $transducer?)

  #|proc:reducer?
  The `reducer?` procedure returns `#t` when `x` is a reducer record.
  |#
  (define reducer? $reducer?)

  #|proc:reduced?
  The `reduced?` procedure returns `#t` when `x` is a reduced wrapper.
  |#
  (define reduced? $reduced?)

  #|proc:eduction?
  The `eduction?` procedure returns `#t` when `x` is an eduction record.
  |#
  (define eduction? $eduction?)

  (define all-transducers?
    (lambda (x*)
      (and (list? x*) (andmap transducer? x*))))

  (define byte?
    (lambda (x)
      (and (fixnum? x) (fx<= 0 x) (fx<= x 255))))

  (define source-end? reduced?)

  (define reducer-init
    (lambda (reducer)
      (($reducer-proc reducer))))

  (define reducer-complete
    (lambda (reducer acc)
      (($reducer-proc reducer) acc)))

  (define reducer-step
    (lambda (reducer acc x)
      (($reducer-proc reducer) acc x)))

  (define apply-transducer
    (lambda (xform reducer)
      (let ([next (($transducer-reducer-proc xform) reducer)])
        (if (reducer? next)
            next
            (errorf 'transduce "transducer ~a returned a non-reducer: ~a"
                    ($transducer-name xform)
                    next)))))

  (define run-list
    (lambda (reducer acc ls)
      (let loop ([acc acc] [ls ls])
        (cond [(source-end? acc) acc]
              [(null? ls) acc]
              [else (loop (reducer-step reducer acc (car ls)) (cdr ls))]))))

  (define run-vector
    (lambda (reducer acc vec)
      (let ([len (vector-length vec)])
        (let loop ([acc acc] [i 0])
          (cond [(source-end? acc) acc]
                [(fx= i len) acc]
                [else (loop (reducer-step reducer acc (vector-ref vec i))
                            (fx+ i 1))])))))

  (define run-bytevector
    (lambda (reducer acc bv)
      (let ([len (bytevector-length bv)])
        (let loop ([acc acc] [i 0])
          (cond [(source-end? acc) acc]
                [(fx= i len) acc]
                [else (loop (reducer-step reducer acc (bytevector-u8-ref bv i))
                            (fx+ i 1))])))))

  (define run-string
    (lambda (reducer acc str)
      (let ([len (string-length str)])
        (let loop ([acc acc] [i 0])
          (cond [(source-end? acc) acc]
                [(fx= i len) acc]
                [else (loop (reducer-step reducer acc (string-ref str i))
                            (fx+ i 1))])))))

  (define run-iter
    (lambda (reducer acc iter)
      (let loop ([acc acc])
        (if (source-end? acc)
            acc
            (let ([x (iter-next! iter)])
              (if (iter-end? x)
                  acc
                  (loop (reducer-step reducer acc x))))))))

  (define run-source
    (lambda (reducer acc source)
      (cond [(list? source) (run-list reducer acc source)]
            [(vector? source) (run-vector reducer acc source)]
            [(bytevector? source) (run-bytevector reducer acc source)]
            [(string? source) (run-string reducer acc source)]
            [(iter? source) (run-iter reducer acc source)]
            [else (errorf 'transduce "unsupported transducer source: ~a" source)])))

  (define finish-transduce
    (lambda (reducer acc)
      (reducer-complete reducer (unreduced acc))))

  (define transduce-explicit
    (lambda (xform reducer init source)
      (pcheck ([transducer? xform] [reducer? reducer])
              (cond [($eduction? source)
                     (let ([combined (tcompose ($eduction-xform source) xform)])
                       (transduce-explicit combined reducer init ($eduction-source source)))]
                    [else
                     (if (transducible? source)
                         (let ([effective (apply-transducer xform reducer)])
                           (finish-transduce effective
                                             (run-source effective init source)))
                         (errorf 'transduce "unsupported transducer source: ~a" source))]))))

  (define transduce-default
    (lambda (xform reducer source)
      (pcheck ([transducer? xform] [reducer? reducer])
              (transduce-explicit xform reducer (reducer-init reducer) source))))

  (define list->bytevector
    (lambda (ls)
      (let* ([len (length ls)]
             [bv (make-bytevector len)])
        (let loop ([ls ls] [i 0])
          (if (null? ls)
              bv
              (let ([x (car ls)])
                (if (byte? x)
                    (begin
                      (bytevector-u8-set! bv i x)
                      (loop (cdr ls) (fx+ i 1)))
                    (errorf 'rfbytevector "expected byte value, got ~a" x))))))))

  (define make-list-like-reducer
    (lambda (name complete)
      (mk-$reducer
       name
       (case-lambda
         [() '()]
         [(acc) (complete acc)]
         [(acc x) (cons x acc)])
       #f)))

  #|proc:reduced
  The `reduced` procedure wraps `x` to request early termination from a
  transducer driver.
  |#
  (define reduced
    (lambda (x)
      (mk-$reduced x)))

  #|proc:unreduced
  The `unreduced` procedure returns the wrapped value of a reduced result, or
  `x` unchanged when `x` is not reduced.
  |#
  (define unreduced
    (lambda (x)
      (if (reduced? x) ($reduced-value x) x)))

  #|proc:ensure-reduced
  The `ensure-reduced` procedure returns `x` unchanged when it is already a
  reduced value, otherwise it wraps `x` with `reduced`.
  |#
  (define ensure-reduced
    (lambda (x)
      (if (reduced? x) x (reduced x))))

  #|proc:preserving-reduced
  The `preserving-reduced` procedure returns a reducer that forwards to
  `reducer` while leaving an incoming reduced accumulator untouched.
  |#
  (define preserving-reduced
    (lambda (reducer)
      (pcheck ([reducer? reducer])
              (mk-$reducer
               ($reducer-name reducer)
               (case-lambda
                 [() (reducer-init reducer)]
                 [(acc) (reducer-complete reducer acc)]
                 [(acc x)
                  (if (reduced? acc)
                      acc
                      (reducer-step reducer acc x))])
               #f))))

  #|proc:completion
  The `completion` procedure returns the completing reducing procedure stored
  by `reducer`.
  |#
  (define completion
    (lambda (reducer)
      (pcheck ([reducer? reducer])
              ($reducer-proc reducer))))

  #|proc:make-transducer
  The `make-transducer` procedure returns a transducer record named `name`.
  `proc` must accept a reducer and return a reducer.
  |#
  (define make-transducer
    (lambda (name proc)
      (pcheck ([symbol? name] [procedure? proc])
              (mk-$transducer name proc #f))))

  #|proc:transducer-name
  The `transducer-name` procedure returns the symbolic name of `xform`.
  |#
  (define transducer-name
    (lambda (xform)
      (pcheck ([transducer? xform])
              ($transducer-name xform))))

  #|proc:tidentity
  The `tidentity` procedure returns a transducer that leaves a reducer
  unchanged.
  |#
  (define tidentity
    (lambda ()
      (make-transducer 'tidentity (lambda (reducer) reducer))))

  #|proc:tcompose
  The `tcompose` procedure composes transducers in left-to-right pipeline
  order.
  |#
  (define tcompose
    (lambda xform*
      (pcheck ([all-transducers? xform*])
              (if (null? xform*)
                  (tidentity)
                  (make-transducer
                   'tcompose
                   (lambda (reducer)
                     (let loop ([xform* xform*])
                       (if (null? xform*)
                           reducer
                           (apply-transducer (car xform*)
                                             (loop (cdr xform*)))))))))))

  #|proc:tchain
  The `tchain` procedure composes the transducers in `xform-list` in
  left-to-right pipeline order.
  |#
  (define tchain
    (lambda (xform-list)
      (pcheck ([all-transducers? xform-list])
              (apply tcompose xform-list))))

  #|proc:tmap
  The `tmap` procedure returns a transducer that emits `(proc x)` for each
  input value `x`.
  |#
  (define tmap
    (lambda (proc)
      (pcheck ([procedure? proc])
              (make-transducer
               'tmap
               (lambda (reducer)
                 (mk-$reducer
                  'tmap
                  (case-lambda
                    [() (reducer-init reducer)]
                    [(acc) (reducer-complete reducer acc)]
                    [(acc x) (reducer-step reducer acc (proc x))])
                  #f))))))

  #|proc:tfilter
  The `tfilter` procedure returns a transducer that emits only values accepted
  by `pred`.
  |#
  (define tfilter
    (lambda (pred)
      (pcheck ([procedure? pred])
              (make-transducer
               'tfilter
               (lambda (reducer)
                 (mk-$reducer
                  'tfilter
                  (case-lambda
                    [() (reducer-init reducer)]
                    [(acc) (reducer-complete reducer acc)]
                    [(acc x) (if (pred x)
                                 (reducer-step reducer acc x)
                                 acc)])
                  #f))))))

  #|proc:tremove
  The `tremove` procedure returns a transducer that emits only values rejected
  by `pred`.
  |#
  (define tremove
    (lambda (pred)
      (pcheck ([procedure? pred])
              (tfilter (lambda (x) (not (pred x)))))))

  #|proc:tkeep
  The `tkeep` procedure emits `(proc x)` for each input value whose result is
  not `#f`.
  |#
  (define tkeep
    (lambda (proc)
      (pcheck ([procedure? proc])
              (make-transducer
               'tkeep
               (lambda (reducer)
                 (mk-$reducer
                  'tkeep
                  (case-lambda
                    [() (reducer-init reducer)]
                    [(acc) (reducer-complete reducer acc)]
                    [(acc x)
                     (let ([y (proc x)])
                       (if y (reducer-step reducer acc y) acc))])
                  #f))))))

  #|proc:ttake
  The `ttake` procedure returns a stateful transducer that emits at most `n`
  input values.
  |#
  (define ttake
    (lambda (n)
      (pcheck ([natural? n])
              (make-transducer
               'ttake
               (lambda (reducer)
                 (let ([left n])
                   (mk-$reducer
                    'ttake
                    (case-lambda
                      [() (reducer-init reducer)]
                      [(acc) (reducer-complete reducer acc)]
                      [(acc x)
                       (if (fx= left 0)
                           (reduced acc)
                           (begin
                             (set! left (fx- left 1))
                             (let ([next (reducer-step reducer acc x)])
                               (if (or (fx= left 0) (reduced? next))
                                   (ensure-reduced next)
                                   next))))])
                    #f)))))))

  #|proc:tdrop
  The `tdrop` procedure returns a stateful transducer that skips the first `n`
  input values and emits the rest.
  |#
  (define tdrop
    (lambda (n)
      (pcheck ([natural? n])
              (make-transducer
               'tdrop
               (lambda (reducer)
                 (let ([left n])
                   (mk-$reducer
                    'tdrop
                    (case-lambda
                      [() (reducer-init reducer)]
                      [(acc) (reducer-complete reducer acc)]
                      [(acc x)
                       (if (fx> left 0)
                           (begin (set! left (fx- left 1)) acc)
                           (reducer-step reducer acc x))])
                    #f)))))))

  #|proc:ttake-while
  The `ttake-while` procedure emits input values while `pred` returns true,
  then requests early termination before emitting the first failing value.
  |#
  (define ttake-while
    (lambda (pred)
      (pcheck ([procedure? pred])
              (make-transducer
               'ttake-while
               (lambda (reducer)
                 (mk-$reducer
                  'ttake-while
                  (case-lambda
                    [() (reducer-init reducer)]
                    [(acc) (reducer-complete reducer acc)]
                    [(acc x)
                     (if (pred x)
                         (reducer-step reducer acc x)
                         (reduced acc))])
                  #f))))))

  #|proc:tdrop-while
  The `tdrop-while` procedure skips input values while `pred` returns true,
  then emits the first failing value and all remaining values.
  |#
  (define tdrop-while
    (lambda (pred)
      (pcheck ([procedure? pred])
              (make-transducer
               'tdrop-while
               (lambda (reducer)
                 (let ([dropping? #t])
                   (mk-$reducer
                    'tdrop-while
                    (case-lambda
                      [() (reducer-init reducer)]
                      [(acc) (reducer-complete reducer acc)]
                      [(acc x)
                       (if (and dropping? (pred x))
                           acc
                           (begin
                             (set! dropping? #f)
                             (reducer-step reducer acc x)))])
                    #f)))))))

  #|proc:transduce
  The `transduce` procedure runs `xform` over `source` using `reducer`. With
  three arguments, the reducer supplies the initial accumulator; with four
  arguments, `init` is used as the initial accumulator.
  |#
  (define transduce
    (case-lambda
      [(xform reducer source)
       (transduce-default xform reducer source)]
      [(xform reducer init source)
       (transduce-explicit xform reducer init source)]))

  #|proc:into
  The `into` procedure transduces `source` into the destination named by `to`.
  Phase 1 supports `'list`, `'reverse-list`, `'vector`, `'string`, and
  `'bytevector`.
  |#
  (define into
    (lambda (to xform source)
      (pcheck ([symbol? to] [transducer? xform])
              (case to
                [(list) (transduce xform (rflist) source)]
                [(reverse-list) (transduce xform (rfreverselist) source)]
                [(vector) (transduce xform (rfvector) source)]
                [(string)
                 (transduce xform
                            (make-list-like-reducer
                             'rfstring
                             (lambda (acc) (list->string (reverse acc))))
                            source)]
                [(bytevector)
                 (transduce xform
                            (make-list-like-reducer
                             'rfbytevector
                             (lambda (acc) (list->bytevector (reverse acc))))
                            source)]
                [else (errorf 'into "unsupported transducer destination: ~a" to)]))))

  #|proc:list-transduce
  The `list-transduce` procedure transduces list `source` with direct pair
  traversal.
  |#
  (define list-transduce
    (case-lambda
      [(xform reducer source)
       (pcheck ([list? source])
               (transduce-default xform reducer source))]
      [(xform reducer init source)
       (pcheck ([list? source])
               (transduce-explicit xform reducer init source))]))

  #|proc:vector-transduce
  The `vector-transduce` procedure transduces vector `source` with direct
  indexed traversal.
  |#
  (define vector-transduce
    (case-lambda
      [(xform reducer source)
       (pcheck ([vector? source])
               (transduce-default xform reducer source))]
      [(xform reducer init source)
       (pcheck ([vector? source])
               (transduce-explicit xform reducer init source))]))

  #|proc:bytevector-transduce
  The `bytevector-transduce` procedure transduces bytevector `source` by
  reading unsigned bytes.
  |#
  (define bytevector-transduce
    (case-lambda
      [(xform reducer source)
       (pcheck ([bytevector? source])
               (transduce-default xform reducer source))]
      [(xform reducer init source)
       (pcheck ([bytevector? source])
               (transduce-explicit xform reducer init source))]))

  #|proc:string-transduce
  The `string-transduce` procedure transduces string `source` by reading
  characters.
  |#
  (define string-transduce
    (case-lambda
      [(xform reducer source)
       (pcheck ([string? source])
               (transduce-default xform reducer source))]
      [(xform reducer init source)
       (pcheck ([string? source])
               (transduce-explicit xform reducer init source))]))

  #|proc:iter-transduce
  The `iter-transduce` procedure transduces iterator `source` by repeatedly
  calling `iter-next!`.
  |#
  (define iter-transduce
    (case-lambda
      [(xform reducer source)
       (pcheck ([iter? source])
               (transduce-default xform reducer source))]
      [(xform reducer init source)
       (pcheck ([iter? source])
               (transduce-explicit xform reducer init source))]))

  #|proc:make-reducer
  The `make-reducer` procedure returns a reducer record named `name`. `proc`
  must be a completing reducing procedure with zero, one, and two arguments.
  |#
  (define make-reducer
    (lambda (name proc)
      (pcheck ([symbol? name] [procedure? proc])
              (mk-$reducer name proc #f))))

  #|proc:reducer-name
  The `reducer-name` procedure returns the symbolic name of `reducer`.
  |#
  (define reducer-name
    (lambda (reducer)
      (pcheck ([reducer? reducer])
              ($reducer-name reducer))))

  #|proc:rflist
  The `rflist` procedure returns a reducer that accumulates values into a list
  in input order.
  |#
  (define rflist
    (lambda ()
      (make-list-like-reducer 'rflist reverse)))

  #|proc:rfreverselist
  The `rfreverselist` procedure returns a reducer that accumulates values into
  a list in reverse input order.
  |#
  (define rfreverselist
    (lambda ()
      (make-list-like-reducer 'rfreverselist id)))

  #|proc:rfvector
  The `rfvector` procedure returns a reducer that accumulates values into a
  vector in input order.
  |#
  (define rfvector
    (lambda ()
      (make-list-like-reducer
       'rfvector
       (lambda (acc) (list->vector (reverse acc))))))

  #|proc:rfcount
  The `rfcount` procedure returns a reducer that counts transformed values.
  |#
  (define rfcount
    (lambda ()
      (make-reducer
       'rfcount
       (case-lambda
         [() 0]
         [(acc) acc]
         [(acc x) (fx+ acc 1)]))))

  #|proc:rfsum
  The `rfsum` procedure returns a reducer that sums transformed numeric values
  with `+`.
  |#
  (define rfsum
    (lambda ()
      (make-reducer
       'rfsum
       (case-lambda
         [() 0]
         [(acc) acc]
         [(acc x) (+ acc x)]))))

  #|proc:rffxsum
  The `rffxsum` procedure returns a reducer that sums transformed fixnum values
  with `fx+`.
  |#
  (define rffxsum
    (lambda ()
      (make-reducer
       'rffxsum
       (case-lambda
         [() 0]
         [(acc) acc]
         [(acc x) (fx+ acc x)]))))

  #|proc:rfflsum
  The `rfflsum` procedure returns a reducer that sums transformed flonum values
  with `fl+`.
  |#
  (define rfflsum
    (lambda ()
      (make-reducer
       'rfflsum
       (case-lambda
         [() 0.0]
         [(acc) acc]
         [(acc x) (fl+ acc x)]))))

  #|proc:eduction
  The `eduction` procedure returns an eduction record containing `xform` and
  `source`. Traversing the eduction applies `xform` before the terminal
  operation's transducer.
  |#
  (define eduction
    (lambda (xform source)
      (pcheck ([transducer? xform])
              (if (transducible? source)
                  (mk-$eduction xform source)
                  (errorf 'eduction "unsupported transducer source: ~a" source)))))

  #|proc:transducible?
  The `transducible?` procedure returns `#t` for Phase 1 sources accepted by
  `transduce`.
  |#
  (define transducible?
    (lambda (x)
      (or (list? x)
          (vector? x)
          (bytevector? x)
          (string? x)
          (iter? x)
          (eduction? x)))))
