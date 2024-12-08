(library (chezpp hashset)
  (export hashset hashset? make-hashset make-eq-hashset make-eqv-hashset
          hashset-add! hashset-delete! hashset-size hashset-contains? hashset-contains/p?
          hashset-filter hashset-filter! hashset-partition
          hashset-search hashset-search*

          hashset-map hashset-for-each

          hashset+ hashset- hashset& hashset^

          hashset->list hashset->vector
          list->eq-hashset vector->eq-hashset
          list->eqv-hashset vector->eqv-hashset)
  (import (chezpp chez)
          (chezpp internal)
          (chezpp utils)
          (chezpp list))


  ;; dummy value for all keys
  (define V #f)

  (define-record-type ($hashset mk-hashset hashset?)
    (nongenerative) (opaque #t)
    (fields (immutable ht hashset-ht)
            ;; 'eq, 'eqv, 'others
            (immutable type hashset-type)))


  #|doc
  Return a newly allocated mutable hashset using `hash` as the hash function
  and `=?` as the equivalence function used to compare values.
  If a third argument is given, the initial capacity of the hashset is
  set to approximately `k` elements.

  `hash` and `=?` must be procedures.
  `hash` should accept a single value as an argument and
  should return a non-negative exact integer object (the hash).
  `=?` should accept two values as arguments and return a single value.
  Neither procedure should mutate the hashset returned by this procedure.

  If neither `hash` nor `=?` is given, an eqv-hashset is returned.
  |#
  (define-who make-hashset
    (case-lambda
      [()          (mk-hashset (make-eqv-hashtable)       'eqv)]
      [(hash =?)   (mk-hashset (make-hashtable hash =?)   'others)]
      [(hash =? k) (mk-hashset (make-hashtable hash =? k) 'others)]))


  #|doc
  Make a mutable eq-hashset that accepts arbirary objects as values,
  and compares those values using `eq?`.

  If an argument is given, the initial capacity of the
  eq-hashset is set to approximately `k` elements.
  |#
  (define make-eq-hashset
    (case-lambda
      [()  (mk-hashset (make-eq-hashtable)   'eq)]
      [(k) (mk-hashset (make-eq-hashtable k) 'eq)]))


  #|doc
  Make a mutable eqv-hashset that accepts arbirary objects as values,
  and compares those values using `eqv?`.

  If an argument is given, the initial capacity of the
  eqv-hashset is set to approximately `k` elements.
  |#
  (define make-eqv-hashset
    (case-lambda
      [()  (mk-hashset (make-eqv-hashtable)   'eqv)]
      [(k) (mk-hashset (make-eqv-hashtable k) 'eqv)]))


  #|doc
  Make a mutable symbol-hashset that accepts symbols as values.

  If an argument is given, the initial capacity of the
  symbol-hashset is set to approximately `k` elements.
  |#
  (define make-symbol-hashset
    (case-lambda
      [()  (mk-hashset (make-hashtable symbol-hash)   'others)]
      [(k) (mk-hashset (make-hashtable symbol-hash k) 'others)]))


  #|doc
  Make a mutable eqv-hashset and add all arguments to the hashset.
  |#
  (define-who hashset
    (lambda args
      (let ([hs (make-eqv-hashset (length args))])
        (for-each (lambda (x) (hashset-add! hs x)) args)
        hs)))


  #|doc
  Add a new item `v` to the hashset `hs` if `v` does not exist yet.
  |#
  (define-who hashset-add!
    (lambda (hs v)
      (pcheck ([hashset? hs])
              (hashtable-set! (hashset-ht hs) v V))))

  #|doc
  Remove the value `v` from the hashset `hs`.
  |#
  (define-who hashset-delete!
    (lambda (hs v)
      (pcheck ([hashset? hs])
              (hashtable-delete! (hashset-ht hs) v))))


  #|doc
  Remove all items from the hashset `hs`.
  |#
  (define-who hashset-clear!
    (lambda (hs)
      (pcheck ([hashset? hs])
              (hashtable-clear! (hashset-ht hs)))))


  #|doc
  Return the number of items in the hashset.
  |#
  (define-who hashset-size
    (lambda (hs)
      (pcheck ([hashset? hs])
              (hashtable-size (hashset-ht hs)))))

  (define $newhs
    (lambda (hs)
      (let* ([ht (hashset-ht hs)] [k (hashtable-size ht)]
             [newht (case (hashset-type hs)
                      [eq  (make-eq-hashtable  k)]
                      [eqv (make-eqv-hashtable k)]
                      [others (make-hashtable
                               (hashtable-hash-function ht)
                               (hashtable-equivalence-function ht)
                               k)]
                      [else (assert-unreachable)])])
        (mk-hashset newht (hashset-type hs)))))


  #|doc
  Return a new hashset `h` such that for each item `x` in `h`,
  (pred x) returns #t.
  |#
  (define-who hashset-filter
    (lambda (pred hs)
      (pcheck ([hashset? hs] [procedure? pred])
              (let* ([ht (hashset-ht hs)] [k (hashtable-size ht)]
                     [newhs ($newhs hs)] [newht (hashset-ht newhs)]
                     [v* (hashtable-keys ht)])
                (vector-for-each (lambda (v) (when (pred v) (hashtable-set! newht v V))) v*)
                newhs))))


  #|doc
  Remove all items in `hs` that do not satisfy the predicate `pred`.
  |#
  (define-who hashset-filter!
    (lambda (pred hs)
      (pcheck ([hashset? hs] [procedure? pred])
              (let* ([ht (hashset-ht hs)] [v* (hashtable-keys ht)])
                (vector-for-each (lambda (v) (unless (pred v) (hashtable-delete! ht v))) v*)
                hs))))


  #|doc
  Apply `pred` to every item in hashset `hs` and return two values,
  the first one a hashset of the items of `hs` for which `(pred v)` returns #t,
  the second one a hashset of the items of `hs` for which `(pred v)` returns #f.
  |#
  (define-who hashset-partition
    (lambda (pred hs)
      (pcheck ([hashset? hs] [procedure? pred])
              (let* ([ht (hashset-ht hs)] [k (hashtable-size ht)]
                     [T ($newhs hs)] [ht-T (hashset-ht T)]
                     [F ($newhs hs)] [ht-F (hashset-ht F)]
                     [v* (hashtable-keys ht)])
                (vector-for-each (lambda (v) (if (pred v)
                                                 (hashtable-set! ht-T v V)
                                                 (hashtable-set! ht-F v V)))
                                 v*)
                (values T F)))))


  #|doc
  Return whether the hashset `hs` already contains the value `v`.
  |#
  (define-who hashset-contains?
    (lambda (hs v)
      (pcheck ([hashset? hs])
              (hashtable-contains? (hashset-ht hs) v))))


  #|doc
  Return whether the hashset `hs` contains the item `v`
  such that `(pred v)` returns #t.
  |#
  (define-who hashset-contains/p?
    (lambda (hs pred)
      (pcheck ([hashset? hs] [procedure? pred])
              (let* ([ht (hashset-ht hs)] [v* (hashtable-keys ht)])
                (let loop ([i 0])
                  (if (fx= i (vector-length v*))
                      #f
                      (if (pred (vector-ref v* i))
                          #t
                          (loop (fx1+ i)))))))))


  #|doc
  Return the 1st item in the hashset `hs` that satisfies the predicate `pred`.
  If no such item exists, #f is returned.
  |#
  (define-who hashset-search
    (lambda (hs pred)
      (pcheck ([hashset? hs] [procedure? pred])
              (let* ([ht (hashset-ht hs)] [v* (hashtable-keys ht)])
                (let loop ([i 0])
                  (if (fx= i (vector-length v*))
                      #f
                      (if (pred (vector-ref v* i))
                          (vector-ref v* i)
                          (loop (fx1+ i)))))))))


  #|doc
  Return the the list of items in the hashset `hs` that satify the predicate `pred`.

  By default the items satisfying `pred` are returned in a list.

  If `collect` is given, it is applied to every item that satisfies `pred`
  in the hashset. This is useful when collecting the desired items in custom
  data structures.
  |#
  (define-who hashset-search*
    (case-lambda
      [(hs pred)
       (pcheck ([hashset? hs] [procedure? pred])
               (let ([lb (make-list-builder)])
                 (let* ([ht (hashset-ht hs)] [v* (hashtable-keys ht)])
                   (let loop ([i 0])
                     (if (fx= i (vector-length v*))
                         (lb)
                         (begin (when (pred (vector-ref v* i))
                                  (lb (vector-ref v* i)))
                                (loop (fx1+ i))))))))]
      [(hs pred collect)
       (pcheck ([hashset? hs] [procedure? pred])
               (let* ([ht (hashset-ht hs)] [v* (hashtable-keys ht)])
                 (let loop ([i 0])
                   (unless (fx= i (vector-length v*))
                     (begin (when (pred (vector-ref v* i))
                              (collect (vector-ref v* i)))
                            (loop (fx1+ i)))))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   set operations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; TODO containment relations


  #|doc
  Compute the union of the hashsets, i.e., the hashset that contains all items
  in all the given hashsets.
  If only one hashset is given, it is returned immediately.

  The returned hashset has the same type (i.e., the same `hash` and `=?` procedures)
  as that of the first input hashset.
  |#
  (define-who hashset+
    (lambda (hs . hs*)
      (pcheck ([hashset? hs])
              (if (null? hs*)
                  hs
                  (pcheck ([all-hashsets? hs*])
                          (let* ([newhs ($newhs hs)]
                                 [newht (hashset-ht newhs)])
                            (for-each (lambda (hs)
                                        (let ([ht (hashset-ht hs)])
                                          (vector-for-each (lambda (v) (hashtable-set! newht v V))
                                                           (hashtable-keys ht))))
                                      (cons hs hs*))
                            newhs))))))


  #|doc
  Compute the difference of the hashsets, i.e., the hashset that contains those items
  that are in the first hashset, but are not in the rest of the hashsets.
  If only one hashset is given, it is returned immediately.

  The returned hashset has the same type (i.e., the same `hash` and `=?` procedures)
  as that of the first input hashset.
  |#
  (define-who hashset-
    (lambda (hs . hs*)
      (pcheck ([hashset? hs])
              (if (null? hs*)
                  hs
                  (pcheck ([all-hashsets? hs*])
                          (let* ([newhs ($newhs hs)]
                                 [newht (hashset-ht newhs)])
                            (vector-for-each (lambda (v) (hashtable-set! newht v V))
                                             (hashtable-keys (hashset-ht hs)))
                            (for-each (lambda (hs)
                                        (let ([ht (hashset-ht hs)])
                                          (vector-for-each (lambda (v)
                                                             (when (hashtable-contains? newht v)
                                                               (hashtable-delete! newht v)))
                                                           (hashtable-keys ht))))
                                      hs*)
                            newhs))))))


  #|doc
  Compute the intersection of the hashsets, i.e., the hashset whose items are contained
  in all given hashsets.
  If only one hashset is given, it is returned immediately.

  The returned hashset has the same type (i.e., the same `hash` and `=?` procedures)
  as that of the first input hashset.
  |#
  (define-who hashset&
    (lambda (hs . hs*)
      (pcheck ([hashset? hs])
              (if (null? hs*)
                  hs
                  (pcheck ([all-hashsets? hs*])
                          (let* ([newhs (apply hashset+ hs hs*)]
                                 [newht (hashset-ht newhs)]
                                 [v* (hashtable-keys newht)])
                            (vector-for-each (lambda (v)
                                               (unless (andmap (lambda (hs)
                                                                 (hashtable-contains? (hashset-ht hs) v))
                                                               (cons hs hs*))
                                                 (hashtable-delete! newht v)))
                                             v*)
                            newhs))))))


  #|doc
  Compute the symmetric difference of the hashsets, i.e., the difference of the union
  and the intersection of the hashsets.
  If only one hashset is given, it is returned immediately.

  The returned hashset has the same type (i.e., the same `hash` and `=?` procedures)
  as that of the first input hashset.
  |#
  (define-who hashset^
    (lambda (hs . hs*)
      (pcheck ([hashset? hs])
              (if (null? hs*)
                  hs
                  (pcheck ([all-hashsets? hs*])
                          (let* ([newhs (apply hashset+ hs hs*)]
                                 [newht (hashset-ht newhs)]
                                 [v* (hashtable-keys newht)])
                            (vector-for-each (lambda (v)
                                               (when (andmap (lambda (hs)
                                                               (hashtable-contains? (hashset-ht hs) v))
                                                             (cons hs hs*))
                                                 (hashtable-delete! newht v)))
                                             v*)
                            newhs))))))


;;;; imperative versions


  #|doc
  Union.
  |#
  (define-who hashset+!
    (lambda (hs . hs*)
      (pcheck ([hashset? hs])
              (if (null? hs*)
                  hs
                  (todo)))))


  #|doc
  Difference.
  |#
  (define-who hashset-!
    (lambda (hs . hs*)
      (pcheck ([hashset? hs])
              (if (null? hs*)
                  hs
                  (todo)))))


  #|doc
  Intersection.
  |#
  (define-who hashset&!
    (lambda (hs . hs*)
      (pcheck ([hashset? hs])
              (if (null? hs*)
                  hs
                  (todo)))))


  #|doc
  symmetric difference
  |#
  (define-who hashset^!
    (lambda (hs . hs*)
      (pcheck ([hashset? hs])
              (if (null? hs*)
                  hs
                  (todo)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   iterations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define all-hashsets? (lambda (x*) (andmap hashset? x*)))
  (define check-size
    (case-lambda
      [(who x0 x1)
       (unless (fx= (hashset-size x0) (hashset-size x1))
         (errorf who "hashsets are not of the same size"))]
      [(who x0 . x*)
       (unless (null? x*)
         (unless (apply fx= (hashset-size x0) (map hashset-size x*))
           (errorf who "hashsets are not of the same size")))]))


  (define-who hashset-map
    (case-lambda
      [(proc hs0)
       (pcheck ([procedure? proc] [hashset? hs0])
               (let* ([k* (hashtable-keys (hashset-ht hs0))]
                      [newhs ($newhs hs0)])
                 (vector-for-each (lambda (x) (hashset-add! newhs (proc x))) k*)
                 newhs))]
      [(proc hs0 hs1)
       (pcheck ([procedure? proc] [hashset? hs0] [hashset? hs1])
               (check-size who hs0 hs1)
               (let* ([k0* (hashtable-keys (hashset-ht hs0))]
                      [k1* (hashtable-keys (hashset-ht hs1))]
                      [newhs ($newhs hs0)])
                 (vector-for-each (lambda (x0 x1) (hashset-add! newhs (proc x0 x1))) k0* k1*)
                 newhs))]
      [(proc hs0 . hs*)
       (pcheck ([procedure? proc] [hashset? hs0] [all-hashsets? hs*])
               (apply check-size who hs0 hs*)
               (let* ([k0* (hashtable-keys (hashset-ht hs0))] [len (vector-length k0*)]
                      [k** (map hashtable-keys (map hashset-ht hs*))]
                      [newhs ($newhs hs0)])
                 (let loop ([i 0])
                   (if (fx= i len)
                       newhs
                       (begin (hashset-add! newhs (apply proc (vector-ref k0* i)
                                                         (map (lambda (v) (vector-ref v i)) k**)))
                              (loop (fx1+ i)))))))]))


  (define-who hashset-for-each
    (case-lambda
      [(proc hs0)
       (pcheck ([procedure? proc] [hashset? hs0])
               (let ([k* (hashtable-keys (hashset-ht hs0))])
                 (vector-for-each proc k*)))]
      [(proc hs0 hs1)
       (pcheck ([procedure? proc] [hashset? hs0] [hashset? hs1])
               (check-size who hs0 hs1)
               (let ([k0* (hashtable-keys (hashset-ht hs0))]
                     [k1* (hashtable-keys (hashset-ht hs1))])
                 (vector-for-each proc k0* k1*)))]
      [(proc hs0 . hs*)
       (pcheck ([procedure? proc] [hashset? hs0] [all-hashsets? hs*])
               (apply check-size who hs0 hs*)
               (let* ([k0* (hashtable-keys (hashset-ht hs0))] [len (vector-length k0*)]
                      [k** (map hashtable-keys (map hashset-ht hs*))])
                 (let loop ([i 0])
                   (unless (fx= i len)
                     (apply proc (vector-ref k0* i)
                            (map (lambda (v) (vector-ref v i)) k**))
                     (loop (fx1+ i))))))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   conversions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  #|doc
  Convert a hashset `hs` into a list.
  |#
  (define-who hashset->list
    (lambda (hs)
      (pcheck ([hashset? hs])
              (vector->list (hashtable-keys (hashset-ht hs))))))


  #|doc
  Convert a hashset `hs` into a vector.
  |#
  (define-who hashset->vector
    (lambda (hs)
      (pcheck ([hashset? hs])
              (vector-copy (hashtable-keys (hashset-ht hs))))))


  #|doc
  Convert a list `ls` into an eq-hashset.
  |#
  (define-who list->eq-hashset
    (lambda (ls)
      (pcheck ([list? ls])
              (let* ([len (length ls)]
                     [hs (make-eq-hashset len)])
                (for-each (lambda (x) (hashset-add! hs x)) ls)
                hs))))


  #|doc
  Convert a vector `vec` into an eq-hashset.
  |#
  (define-who vector->eq-hashset
    (lambda (vec)
      (pcheck ([vector? vec])
              (let* ([len (vector-length vec)]
                     [hs (make-eq-hashset len)])
                (vector-for-each (lambda (x) (hashset-add! hs x)) vec)
                hs))))


  #|doc
  Convert a list `ls` into an eqv-hashset.
  |#
  (define-who list->eqv-hashset
    (lambda (ls)
      (pcheck ([list? ls])
              (let* ([len (length ls)]
                     [hs (make-eqv-hashset len)])
                (for-each (lambda (x) (hashset-add! hs x)) ls)
                hs))))


  #|doc
  Convert a vector `vec` into an eqv-hashset.
  |#
  (define-who vector->eqv-hashset
    (lambda (vec)
      (pcheck ([vector? vec])
              (let* ([len (vector-length vec)]
                     [hs (make-eqv-hashset len)])
                (vector-for-each (lambda (x) (hashset-add! hs x)) vec)
                hs))))


  (record-writer (type-descriptor $hashset)
                 (lambda (r p wr)
                   (display "#[hashset " p)
                   (display (hashset->list r) p)
                   (display "]" p)))


  (record-type-equal-procedure
   (type-descriptor $hashset)
   (lambda (hs1 hs2 =?)
     (let* ([t1 (hashset-type hs1)] [t2 (hashset-type hs2)]
            [ht1 (hashset-ht hs1)]  [ht2 (hashset-ht hs2)])
       (and (eq? t1 t2)
            (fx= (hashtable-size ht1) (hashtable-size ht2))
            (let ([v1 (hashtable-keys ht1)])
              (let loop ([i 0])
                (if (fx= i (vector-length v1))
                    #t
                    (if (hashtable-contains? ht2 (vector-ref v1 i))
                        (loop (fx1+ i))
                        #f))))))))

  )
