(library (chezpp dset)
  (export dset make-dset dset?
          dset-same? dset-union! dset-size)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp internal)
          (chezpp list)
          (chezpp vector))

  ;; A disjoint set implementation that supports union-find operations.
  ;; `dset` operates on natural numbers only.

  (define-record-type node
    (opaque #t)
    (fields (mutable parent)
            (mutable rank)))

  (define-record-type (dset mk-dset dset?)
    (fields vec))

  (define all-naturals? (lambda (args) (andmap natural? args)))

  (define check-index
    (lambda (who ds x)
      (let ([len (vector-length (dset-vec ds))])
        (when (fx>= x len)
          (errorf who "invalid index for dset of ~a items" len)))))

  #|doc
  Construct a dset (disjoint set) object with an initial number of `n` items.
  `n` must be a natural number.
  |#
  (define-who make-dset
    (lambda (n)
      (pcheck ([natural? n])
              (let ([vec (viota n)])
                (vmap! (lambda (i) (make-node i 0)) vec)
                (mk-dset vec)))))

  ;; return the index of the root
  (define find-root
    (lambda (vec i)
      ;; path compression
      (let* ([n (vector-ref vec i)] [pi (node-parent n)])
        (if (fx= i pi)
            i
            (let ([newpi (find-root vec pi)])
              (node-parent-set! n newpi)
              newpi)))))


  #|doc
  Given two or more items, check whether they belong to the same set.
  |#
  (define-who dset-same?
    (case-lambda
      [(ds x y)
       (pcheck ([dset? ds] [natural? x y])
               (check-index who ds x)
               (check-index who ds y)
               (let* ([vec (dset-vec ds)]
                      [ri0 (find-root vec x)]
                      [ri1 (find-root vec y)])
                 (fx= ri0 ri1)))]
      [(ds . x*)
       (pcheck ([dset? ds] [all-naturals? x*])
               (for-each (lambda (x) (check-index who ds x)) x*)
               (let* ([vec (dset-vec ds)]
                      [ri* (map (lambda (x) (find-root vec x)) x*)])
                 (apply fx= ri*)))]))

  (define link!
    (lambda (vec x y)
      ;; union by rank
      (let* ([nx (vector-ref vec x)] [rx (node-rank nx)]
             [ny (vector-ref vec y)] [ry (node-rank ny)])
        (if (> rx ry)
            (node-parent-set! ny x)
            (begin (node-parent-set! nx y)
                   (when (fx= rx ry)
                     (node-rank-set! ny (fx1+ ry))))))))

  #|doc
  Mark two or more items as belonging to the same set the dset `ds`.
  After this operation, `(dset-same? ds x ...)` always evaluates to #t.
  |#
  (define-who dset-union!
    (case-lambda
      [(ds x y)
       (pcheck ([dset? ds] [natural? x y])
               (check-index who ds x)
               (check-index who ds y)
               (let ([vec (dset-vec ds)])
                 (link! vec (find-root vec x) (find-root vec y))))]
      [(ds . x*)
       (pcheck ([dset? ds] [all-naturals? x*])
               (for-each (lambda (x) (check-index who ds x)) x*)
               (let* ([vec (dset-vec ds)] [x (car x*)])
                 (for-each (lambda (y)
                             (link! vec (find-root vec x) (find-root vec y)))
                           (cdr x*))))]))


  #|doc
  Return the number of disjoint sets in the dset.
  |#
  (define-who dset-size
    (lambda (ds)
      (vfold-left/i (lambda (i acc n)
                      (if (fx= i (node-parent n)) (add1 acc) acc))
                    0 (dset-vec ds))))


  (record-writer (type-descriptor dset)
                 (lambda (r p wr)
                   (display "#[dset " p)
                   (wr (dset-size r) p)
                   (display "/" p)
                   (wr (vector-length (dset-vec r)) p)
                   (display "]" p)))

  )
