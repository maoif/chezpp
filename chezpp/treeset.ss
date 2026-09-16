(library (chezpp treeset)
  (export make-treeset make-fixnum-treeset fixnum-treeset fixnum-treeset? treeset treeset? treeset-empty? treeset-size
          treeset-add! treeset-delete! treeset-clear!

          treeset-contains? treeset-contains/p?
          treeset-filter treeset-filter! treeset-partition
          treeset-search treeset-search*

          treeset-successor treeset-predecessor
          treeset-min treeset-max

          treeset-andmap treeset-ormap
          treeset-map treeset-map/i
          treeset-for-each treeset-for-each/i
          treeset-fold-left treeset-fold-left/i
          treeset-fold-right treeset-fold-right/i

          treeset+ treeset- treeset& treeset^
          treeset+! treeset-! treeset&! treeset^!

          treeset->list list->treeset
          treeset->vector vector->treeset
          list->fixnum-treeset vector->fixnum-treeset)
  (import (chezpp chez)
          (chezpp list)
          (chezpp internal)
          (chezpp utils)
          (chezpp private rbtree)
          (only (chezpp iter) iter-register-source! make-iter iter-end))


  (define-record-type ($treeset mk-treeset treeset-record?)
    (parent rbtree) (nongenerative) (opaque #t)
    (protocol (lambda (pnew)
                (lambda (=? <? size)
                  ((pnew =? <? size #f #f))))))

  #|proc:fixnum-treeset?
  Return whether `object` is a fixnum treeset. Any object may be tested.
  |#
  (define-record-type ($fixnum-treeset mk-fixnum-treeset fixnum-treeset?)
    (parent rbtree) (nongenerative) (opaque #t)
    (protocol (lambda (pnew) (lambda (=? <? size) ((pnew =? <? size #t #f))))))
  #|proc:treeset?
  Return whether `object` is a generic or fixnum treeset. Any object may be tested.
  |#
  (define treeset? (lambda (object) (or (treeset-record? object) (fixnum-treeset? object))))

  #|proc:make-fixnum-treeset
  Construct a treeset whose items are exact fixnums.
  `=?` compares items for equality and `<?` orders items. Returns an empty treeset.
  |#
  (define make-fixnum-treeset
    (lambda (=? <?)
      (pcheck ([procedure? =? <?]) (mk-fixnum-treeset =? <? 0))))

  ;; dummy value for all keys
  (define V #f)



  (define make-treeset-like
    (lambda (source)
      ((if (fixnum-treeset? source) make-fixnum-treeset make-treeset)
       (rbtree-=? source) (rbtree-<? source))))

  #|proc:make-treeset
  Construct a treeset object.
  `=?` is used by the treeset internally to do equality comparison of items;
  `<?` is used by the treeset internally to do order comparison.
  |#
  (define make-treeset
    (lambda (=? <?)
      (pcheck ([procedure? =? <?])
              (mk-treeset =? <? 0))))


  #|proc:treeset
  Create a new treeset, and add the arguments to the treeset.
  `=?` is used by the treeset internally to do equality comparison of items;
  `<?` is used by the treeset internally to do order comparison.
  |#
  (define-who treeset
    (lambda (=? <? . args)
      (pcheck ([procedure? =? <?])
              (let ([ts (make-treeset =? <?)])
                (for-each (lambda (x) (rbtree-set! who ts x V)) args)
                ts))))

  #|proc:fixnum-treeset
  Create a fixnum-key treeset initialized with the supplied fixnums.
  `=?` compares items and `<?` orders items. Returns the populated treeset.
  |#
  (define-who fixnum-treeset
    (lambda (=? <? . args)
      (pcheck ([procedure? =? <?])
              (let ([ts (make-fixnum-treeset =? <?)])
                (for-each (lambda (x)
                            (unless (fixnum? x)
                              (errorf who "not a fixnum treeset item: ~a" x))
                            (treeset-add! ts x))
                          args)
                ts))))


  #|proc:treeset-empty?
  Return whether the treeset is empty.
  |#
  (define-who treeset-empty?
    (lambda (ts)
      (pcheck ([treeset? ts])
              (fx= 0 (rbtree-size ts)))))


  #|proc:treeset-add!
  Add the new value `v` to the treeset `ts`.
  |#
  (define-who treeset-add!
    (lambda (ts v)
      (pcheck ([treeset? ts])
              (when (and (fixnum-treeset? ts) (not (fixnum? v)))
                (errorf who "fixnum treeset item is not a fixnum: ~a" v))
              (rbtree-set! who ts v V))))


  #|proc:treeset-delete!
  Remove the value `v` from the treeset `ts`.
  If `v` is absent, the treeset is unchanged.
  |#
  (define-who treeset-delete!
    (lambda (ts v)
      (pcheck ([treeset? ts])
              (when (rbtree-contains? who ts v)
                (rbtree-delete! who ts v)))))


  #|proc:treeset-clear!
  Remove all items from the treeset `ts`.
  |#
  (define-who treeset-clear!
    (lambda (ts)
      (pcheck ([treeset? ts])
              (rbtree-clear! who ts))))


  #|proc:treeset-size
  Return the number of items in the treeset `ts`.
  |#
  (define-who treeset-size
    (lambda (ts)
      (pcheck ([treeset? ts])
              (rbtree-size ts))))


  #|proc:treeset-contains?
  Return whether the treeset `ts` contains the value `v`.
  |#
  (define-who treeset-contains?
    (lambda (ts v)
      (pcheck ([treeset? ts])
              (rbtree-contains? who ts v))))


  #|proc:treeset-contains/p?
  Return whether the treeset `ts` contains the item `v`
  such that `(pred v)` returns #t.
  |#
  (define-who treeset-contains/p?
    (lambda (ts pred)
      (pcheck ([treeset? ts] [procedure? pred])
              (rbtree-contains/p? who ts (lambda (k v) (pred k))))))


  (define K? (lambda (n) (if (pair? n) (car n) n)))


  #|proc:treeset-search
  Return the 1st item in the treeset `ts` that satisfies the predicate `pred`.
  If no such item exists, #f is returned.
  |#
  (define-who treeset-search
    (lambda (ts pred)
      (pcheck ([treeset? ts] [procedure? pred])
              (K? (rbtree-search who ts (lambda (k v) (pred k)))))))


  #|proc:treeset-search*
  Return the the list of items in the treeset `ts` that satify the predicate `pred`.

  By default the items satisfying `pred` are returned in a list.

  If `collect` is given, it is applied to every item that satisfies `pred`
  in the treeset. This is useful when collecting the desired items in custom
  data structures.
  |#
  (define-who treeset-search*
    (case-lambda
      [(ts pred)
       (pcheck ([treeset? ts] [procedure? pred])
               (let ([lb (make-list-builder)])
                 (rbtree-visit who (lambda (k v) (when (pred k) (lb k))) ts)
                 (lb)))]
      [(ts pred collect)
       (pcheck ([treeset? ts] [procedure? pred collect])
               (rbtree-visit who (lambda (k v) (when (pred k) (collect k))) ts))]))


  #|proc:treeset-successor
  Return the successor of `v` in the treeset `ts`.

  If the successor of `v` does not exist, #f is returned.
  |#
  (define-who treeset-successor
    (lambda (ts v)
      (pcheck ([treeset? ts])
              (K? (rbtree-successor who ts v)))))


  #|proc:treeset-predecessor
  Return the predecessor of `v` in the treeset `ts`.

  If the predecessor of `v` does not exist, #f is returned.
  |#
  (define-who treeset-predecessor
    (lambda (ts v)
      (pcheck ([treeset? ts])
              (K? (rbtree-predecessor who ts v)))))


  #|proc:treeset-min
  Return the minimum value in the treeset `ts`.

  If the treeset is empty, #f is returned.
  |#
  (define-who treeset-min
    (lambda (ts)
      (pcheck ([treeset? ts])
              (K? (rbtree-min who ts)))))


  #|proc:treeset-max
  Return the maximum value in the treeset `ts`.

  If the treeset is empty, #f is returned.
  |#
  (define-who treeset-max
    (lambda (ts)
      (pcheck ([treeset? ts])
              (K? (rbtree-max who ts)))))


  #|proc:treeset-filter
  Return a new treeset whose items are those in `ts`
  such that `(pred x)` returns #t, where `x` is an item in `ts`.
  |#
  (define-who treeset-filter
    (lambda (pred ts)
      (pcheck ([procedure? pred] [treeset? ts])
              (let ([newts (make-treeset-like ts)])
                (rbtree-visit who (lambda (k v) (when (pred k) (rbtree-set! who newts k V))) ts)
                newts))))


  #|proc:treeset-filter!
  Filter the treeset so that after the operation, `ts` only contains
  items `x` such that `(pred x)` returns #t.
  |#
  (define-who treeset-filter!
    (lambda (pred ts)
      (pcheck ([procedure? pred] [treeset? ts])
              (let ([lb (make-list-builder)])
                (rbtree-visit who (lambda (k v) (lb k)) ts)
                (for-each (lambda (v)
                            (unless (pred v)
                              (rbtree-delete! who ts v)))
                          (lb))
                ts))))


  #|proc:treeset-partition
  Apply `pred` to every item in treeset `ts` and return two values,
  the first one a treeset of items for which `(pred item)` is true,
  the second one a treeset of the remaining items. Both preserve the input backend.
  |#
  (define-who treeset-partition
    (lambda (pred ts)
      (pcheck ([procedure? pred] [treeset? ts])
              (let ([T (make-treeset-like ts)]
                    [F (make-treeset-like ts)])
                (rbtree-visit who (lambda (k v) (if (pred k)
                                                    (rbtree-set! who T k V)
                                                    (rbtree-set! who F k V)))
                              ts)
                (values T F)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   set operations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  #|proc:treeset+
  Compute the union of the treesets, i.e., the treeset that contains all items
  in all the given treesets.
  If only one treeset is given, it is returned immediately.

  The `=?` and `<?` procedures of the returned treeset is taken from the first input treeset.
  |#
  (define-who treeset+
    (lambda (ts . ts*)
      (pcheck ([treeset? ts])
              (if (null? ts*)
                  ts
                  (pcheck ([all-treesets? ts*])
                          (let ([newts (make-treeset-like ts)])
                            (for-each (lambda (ts)
                                        (rbtree-visit who
                                                      (lambda (k v)
                                                        (rbtree-set! who newts k V))
                                                      ts))
                                      (cons ts ts*))
                            newts))))))


  #|proc:treeset-
  Compute the difference of the treesets, i.e., the treeset that contains those items
  that are in the first treeset, but are not in the rest of the treesets.
  If only one treeset is given, it is returned immediately.

  The `=?` and `<?` procedures of the returned treeset is taken from the first input treeset.
  |#
  (define-who treeset-
    (lambda (ts . ts*)
      (pcheck ([treeset? ts])
              (if (null? ts*)
                  ts
                  (pcheck ([all-treesets? ts*])
                          (let ([newts (make-treeset-like ts)])
                            (rbtree-visit who (lambda (k v) (rbtree-set! who newts k V)) ts)
                            (for-each (lambda (ts)
                                        (rbtree-visit who
                                                      (lambda (k v)
                                                        (when (rbtree-contains? who newts k)
                                                          (rbtree-delete! who newts k)))
                                                      ts))
                                      ts*)
                            newts))))))


  #|proc:treeset&
  Compute the intersection of the treesets, i.e., the treeset whose items are contained
  in all given treesets.
  If only one treeset is given, it is returned immediately.

  The `=?` and `<?` procedures of the returned treeset is taken from the first input treeset.
  |#
  (define-who treeset&
    (lambda (ts . ts*)
      (pcheck ([treeset? ts])
              (if (null? ts*)
                  ts
                  (pcheck ([all-treesets? ts*])
                          (let ([newts (apply treeset+ ts ts*)] [lb (make-list-builder)])
                            (rbtree-visit who
                                          (lambda (k v)
                                            (unless (andmap (lambda (ts) (rbtree-contains? who ts k))
                                                            (cons ts ts*))
                                              (lb k)))
                                          newts)
                            (for-each (lambda (k) (rbtree-delete! who newts k)) (lb))
                            newts))))))


  #|proc:treeset^
  Compute the symmetric difference of the treesets, i.e., the difference of the union
  and the intersection of the treesets.
  If only one treeset is given, it is returned immediately.

  The `=?` and `<?` procedures of the returned treeset is taken from the first input treeset.
  |#
  (define-who treeset^
    (lambda (ts . ts*)
      (pcheck ([treeset? ts])
              (if (null? ts*)
                  ts
                  (let ([newts (make-treeset-like ts)]
                        [lb (make-list-builder)])
                    ;; union
                    (for-each (lambda (ts)
                                (rbtree-visit who
                                              (lambda (k v)
                                                (rbtree-set! who newts k V))
                                              ts))
                              (cons ts ts*))
                    ;; intersect
                    (rbtree-visit who
                                  (lambda (k v)
                                    (when (andmap (lambda (ts) (rbtree-contains? who ts k))
                                                  (cons ts ts*))
                                      (lb k)))
                                  newts)
                    ;; diff
                    (for-each (lambda (k) (rbtree-delete! who newts k)) (lb))
                    newts)))))


;;;; imperative versions

  #|proc:treeset+!
  Replace treeset `ts` with the union of `ts` and the additional treesets `ts*`.
  Return `ts`; without additional sets it is unchanged. Aliased operands are supported.
  Compute using the first set's comparators and preserve its generic or fixnum backend.
  |#  (define-who treeset+!
    (lambda (ts . ts*)
      (pcheck ([treeset? ts] [all-treesets? ts*])
              (unless (null? ts*)
                (let ([result (apply treeset+ ts ts*)])
                  (treeset-clear! ts)
                  (for-each (lambda (item) (treeset-add! ts item)) (treeset->list result))))
              ts)))


  #|proc:treeset-!
  Replace treeset `ts` with the difference of `ts` and the additional treesets `ts*`.
  Return `ts`; without additional sets it is unchanged. Aliased operands are supported.
  Compute using the first set's comparators and preserve its generic or fixnum backend.
  |#  (define-who treeset-!
    (lambda (ts . ts*)
      (pcheck ([treeset? ts] [all-treesets? ts*])
              (unless (null? ts*)
                (let ([result (apply treeset- ts ts*)])
                  (treeset-clear! ts)
                  (for-each (lambda (item) (treeset-add! ts item)) (treeset->list result))))
              ts)))


  #|proc:treeset&!
  Replace treeset `ts` with the intersection of `ts` and the additional treesets `ts*`.
  Return `ts`; without additional sets it is unchanged. Aliased operands are supported.
  Compute using the first set's comparators and preserve its generic or fixnum backend.
  |#  (define-who treeset&!
    (lambda (ts . ts*)
      (pcheck ([treeset? ts] [all-treesets? ts*])
              (unless (null? ts*)
                (let ([result (apply treeset& ts ts*)])
                  (treeset-clear! ts)
                  (for-each (lambda (item) (treeset-add! ts item)) (treeset->list result))))
              ts)))


  #|proc:treeset^!
  Replace treeset `ts` with the union minus intersection of `ts` and the additional treesets
  `ts*`.
  Return `ts`; without additional sets it is unchanged. Aliased operands are supported.
  Compute using the first set's comparators and preserve its generic or fixnum backend.
  |#  (define-who treeset^!
    (lambda (ts . ts*)
      (pcheck ([treeset? ts] [all-treesets? ts*])
              (unless (null? ts*)
                (let ([result (apply treeset^ ts ts*)])
                  (treeset-clear! ts)
                  (for-each (lambda (item) (treeset-add! ts item)) (treeset->list result))))
              ts)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   iterations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; no in-place maps since we can't modify the tree structure

  (define all-treesets? (lambda (x*) (andmap treeset? x*)))
  (define check-size
    (case-lambda
      [(who x0 x1)
       (unless (fx= (treeset-size x0) (treeset-size x1))
         (errorf who "treesets are not of the same size"))]
      [(who x0 . x*)
       (unless (null? x*)
         (unless (apply fx= (treeset-size x0) (map treeset-size x*))
           (errorf who "treesets are not of the same size")))]))


  #|proc:treeset-andmap
  Traverse the input treesets in ascending comparator order.
  `proc` has signature (item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return #f at the first false callback result; otherwise return #t.
  |#
  (define-who treeset-andmap
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-andmap1 who proc ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (check-size who ts0 ts1)
               (rbtree-andmap1 who proc ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply check-size who ts0 ts*)
               (apply rbtree-andmap1 who proc ts0 ts*))]))


  #|proc:treeset-ormap
  Traverse the input treesets in ascending comparator order.
  `proc` has signature (item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return the first true callback result, or #f if no result is true.
  |#
  (define-who treeset-ormap
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-ormap1 who proc ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (check-size who ts0 ts1)
               (rbtree-ormap1 who proc ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply check-size who ts0 ts*)
               (apply rbtree-ormap1 who proc ts0 ts*))]))


  #|proc:treeset-map
  Traverse the input treesets in ascending comparator order.
  `proc` has signature (item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return a new treeset using the first input's comparators and backend.
  The callback returns the new item.
  |#
  (define-who treeset-map
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-map1 who proc (make-treeset-like ts0) ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (check-size who ts0 ts1)
               (rbtree-map1 who proc (make-treeset-like ts0) ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply check-size who ts0 ts*)
               (apply rbtree-map1 who proc (make-treeset-like ts0) ts0 ts*))]))


  #|proc:treeset-map/i
  Traverse the input treesets in ascending comparator order.
  `proc` has signature (index item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Return a new treeset using the first input's comparators and backend.
  The callback returns the new item.
  |#
  (define-who treeset-map/i
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-map/i1 who proc (make-treeset-like ts0) ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (check-size who ts0 ts1)
               (rbtree-map/i1 who proc (make-treeset-like ts0) ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply check-size who ts0 ts*)
               (apply rbtree-map/i1 who proc (make-treeset-like ts0) ts0 ts*))]))


  #|proc:treeset-for-each
  Traverse the input treesets in ascending comparator order.
  `proc` has signature (item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return an unspecified value.
  |#
  (define-who treeset-for-each
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-for-each1 who proc ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (check-size who ts0 ts1)
               (rbtree-for-each1 who proc ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply check-size who ts0 ts*)
               (apply rbtree-for-each1 who proc ts0 ts*))]))


  #|proc:treeset-for-each/i
  Traverse the input treesets in ascending comparator order.
  `proc` has signature (index item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Return an unspecified value.
  |#
  (define-who treeset-for-each/i
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-for-each/i1 who proc ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (check-size who ts0 ts1)
               (rbtree-for-each/i1 who proc ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply check-size who ts0 ts*)
               (apply rbtree-for-each/i1 who proc ts0 ts*))]))


;;;; folds


  #|proc:treeset-fold-left
  Traverse the input treesets in ascending comparator order.
  `proc` has signature (acc item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return the accumulated value; `acc` is its initial value.
  |#
  (define-who treeset-fold-left
    (case-lambda
      [(proc acc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-fold-left1 who proc acc ts0))]
      [(proc acc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (check-size who ts0 ts1)
               (rbtree-fold-left1 who proc acc ts0 ts1))]
      [(proc acc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply check-size who ts0 ts*)
               (apply rbtree-fold-left1 who proc acc ts0 ts*))]))


  #|proc:treeset-fold-left/i
  Traverse the input treesets in ascending comparator order.
  `proc` has signature (index acc item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Return the accumulated value; `acc` is its initial value.
  |#
  (define-who treeset-fold-left/i
    (case-lambda
      [(proc acc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-fold-left/i1 who proc acc ts0))]
      [(proc acc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (check-size who ts0 ts1)
               (rbtree-fold-left/i1 who proc acc ts0 ts1))]
      [(proc acc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply check-size who ts0 ts*)
               (apply rbtree-fold-left/i1 who proc acc ts0 ts*))]))


  #|proc:treeset-fold-right
  Traverse the input treesets in descending comparator order.
  `proc` has signature (item0 item1 ... acc); collections supply ordered positions.
  Input collections must have equal size.
  Return the accumulated value; `acc` is its initial value.
  |#
  (define-who treeset-fold-right
    (case-lambda
      [(proc acc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-fold-right1 who proc acc ts0))]
      [(proc acc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (check-size who ts0 ts1)
               (rbtree-fold-right1 who proc acc ts0 ts1))]
      [(proc acc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply check-size who ts0 ts*)
               (apply rbtree-fold-right1 who proc acc ts0 ts*))]))


  #|proc:treeset-fold-right/i
  Traverse the input treesets in descending comparator order.
  `proc` has signature (index item0 item1 ... acc); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Return the accumulated value; `acc` is its initial value.
  |#
  (define-who treeset-fold-right/i
    (case-lambda
      [(proc acc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-fold-right/i1 who proc acc ts0))]
      [(proc acc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (check-size who ts0 ts1)
               (rbtree-fold-right/i1 who proc acc ts0 ts1))]
      [(proc acc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply check-size who ts0 ts*)
               (apply rbtree-fold-right/i1 who proc acc ts0 ts*))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   conversions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  #|proc:treeset->list
  Convert a treeset into a list.
  By default, the treeset is converted in order.

  `order` can be 'in, 'pre or 'post, so the items are collected in
  in-order, pre- and post-order, respectively.
  |#
  (define-who treeset->list
    (case-lambda
      [(ts)
       (treeset->list ts 'in)]
      [(ts order)
       (pcheck ([treeset? ts])
               (let ([lb (make-list-builder)])
                 (case order
                   [in   (rbtree-visit-inorder   who (lambda (k v) (lb k)) ts)]
                   [pre  (rbtree-visit-preorder  who (lambda (k v) (lb k)) ts)]
                   [post (rbtree-visit-postorder who (lambda (k v) (lb k)) ts)]
                   [else (errorf who "invalid traversal order: ~a, should be one of 'in, 'pre and 'post" order)])
                 (lb)))]))


  #|proc:treeset->vector
  Convert a treeset into a vector.
  By default, the treeset is converted in order.

  `order` can be 'in, 'pre or 'post, so the items are collected in
  in-order, pre- and post-order, respectively.
  |#
  (define-who treeset->vector
    (case-lambda
      [(ts)
       (treeset->vector ts 'in)]
      [(ts order)
       (pcheck ([treeset? ts])
               (let* ([vec (make-vector (treeset-size ts) #f)] [i 0]
                      [add! (lambda (k v) (vector-set! vec i k) (set! i (fx1+ i)))])
                 (case order
                   [in   (rbtree-visit-inorder   who add! ts)]
                   [pre  (rbtree-visit-preorder  who add! ts)]
                   [post (rbtree-visit-postorder who add! ts)]
                   [else (errorf who "invalid traversal order: ~a, should be one of 'in, 'pre and 'post" order)])
                 vec))]))


  #|proc:list->treeset
  Convert a list `ls` to a treeset.
  `=?` and `<?` are the same as in `treeset`.
  |#
  (define-who list->treeset
    (lambda (=? <? ls)
      (pcheck ([procedure? =? <?] [list? ls])
              (apply treeset =? <? ls))))


  #|proc:vector->treeset
  Convert a vector `vec` to a treeset.
  `=?` and `<?` are the same as in `treeset`.
  |#
  (define-who vector->treeset
    (lambda (=? <? vec)
      (pcheck ([procedure? =? <?] [vector? vec])
              (let ([ts (make-treeset =? <?)])
                (vector-for-each (lambda (x) (treeset-add! ts x)) vec)
                ts))))



  #|proc:list->fixnum-treeset
  Return a new fixnum treeset containing the fixnums in list `items`.
  Equality predicate `equal?` and ordering predicate `less?` each take two fixnum items.
  Duplicate items are stored once.
  |#
  (define list->fixnum-treeset
    (lambda (equal? less? items)
      (pcheck ([procedure? equal? less?] [list? items])
              (let ([result (make-fixnum-treeset equal? less?)])
                (for-each (lambda (item) (treeset-add! result item)) items)
                result))))

  #|proc:vector->fixnum-treeset
  Return a new fixnum treeset containing the fixnums in vector `items`.
  Equality predicate `equal?` and ordering predicate `less?` each take two fixnum items.
  Duplicate items are stored once.
  |#
  (define vector->fixnum-treeset
    (lambda (equal? less? items)
      (pcheck ([procedure? equal? less?] [vector? items])
              (let ([result (make-fixnum-treeset equal? less?)])
                (vector-for-each (lambda (item) (treeset-add! result item)) items)
                result))))

  (define write-treeset
                 (lambda (r p wr)
                   (display "#[treeset (" p)
                   (if (treeset-empty? r)
                       (display ")]" p)
                       (begin
                         (let ([n (treeset-size r)] [i 0])
                           (rbtree-visit 'treeset-writer
                                         (lambda (k v)
                                           (if (fx= i (fx1- n))
                                               (wr k p)
                                               (begin
                                                 (wr k p)
                                                 (display " " p)))
                                           (set! i (fx1+ i)))
                                         r)
                           (display ")]" p))))))

;;;;===----------------------------------------------------------------------===
;;;; Iterator extension registration
;;;;===----------------------------------------------------------------------===

  (iter-register-source!
   treeset?
   (lambda (ts)
     (let* ([items (treeset->list ts)] [remaining items])
       (make-iter
        (lambda ()
          (if (null? remaining)
              iter-end
              (let ([item (car remaining)])
                (set! remaining (cdr remaining))
                item)))
        (lambda () (set! remaining items))))))

  (record-writer (type-descriptor $treeset) write-treeset)
  (record-writer (type-descriptor $fixnum-treeset) write-treeset)

  )
