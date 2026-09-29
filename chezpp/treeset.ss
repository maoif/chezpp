(library (chezpp treeset)
  (export make-treeset make-fxtreeset fxtreeset fxtreeset? treeset treeset? treeset-empty? treeset-size
          fxtreeset-empty? fxtreeset-size
          treeset-add! treeset-delete! treeset-clear!
          fxtreeset-add! fxtreeset-delete! fxtreeset-clear!

          treeset-contains? treeset-contains/p?
          fxtreeset-contains? fxtreeset-contains/p?
          treeset-filter treeset-filter! treeset-partition
          fxtreeset-filter fxtreeset-filter! fxtreeset-partition
          treeset-search treeset-search*
          fxtreeset-search fxtreeset-search*

          treeset-successor treeset-predecessor
          treeset-min treeset-max
          fxtreeset-successor fxtreeset-predecessor
          fxtreeset-min fxtreeset-max

          treeset-andmap treeset-ormap
          fxtreeset-andmap fxtreeset-ormap
          treeset-map treeset-map/i
          fxtreeset-map fxtreeset-map/i
          treeset-for-each treeset-for-each/i
          fxtreeset-for-each fxtreeset-for-each/i
          treeset-fold-left treeset-fold-left/i
          fxtreeset-fold-left fxtreeset-fold-left/i
          treeset-fold-right treeset-fold-right/i
          fxtreeset-fold-right fxtreeset-fold-right/i

          treeset+ treeset- treeset& treeset^
          fxtreeset+ fxtreeset- fxtreeset& fxtreeset^
          treeset+! treeset-! treeset&! treeset^!
          fxtreeset+! fxtreeset-! fxtreeset&! fxtreeset^!

          treeset->list list->treeset
          treeset->vector vector->treeset
          fxtreeset->list fxtreeset->vector
          list->fxtreeset vector->fxtreeset)
  (import (chezpp chez)
          (chezpp list)
          (chezpp internal)
          (chezpp utils)
          (chezpp private rbtree)
          (only (chezpp iter) iter-register-source! make-iter iter-end)
          (only (chezpp navigator) nav-register-set!))

  ;; Generate paired generic/fixnum treeset procedures from a common
  ;; operation specification.  The family predicate, constructor and item
  ;; validator are fixed at expansion time; no runtime family dispatch is
  ;; introduced by the generator.
  (define-syntax define-treeset-procedure
    (syntax-rules ()
      [(_ (name fxname)
          (predicate fxpredicate)
          (constructor fxconstructor)
          ((ts arg ...) body ...)
          ((fts farg ...) fxbody ...))
       (begin
         (define name
           (lambda (ts arg ...)
             (pcheck ([predicate ts]) body ...)))
         (define fxname
           (lambda (fts farg ...)
             (pcheck ([fxpredicate fts]) fxbody ...))))]))


  (define-record-type ($treeset mk-treeset treeset-record?)
    (parent rbtree) (nongenerative) (opaque #t)
    (protocol (lambda (pnew)
                (lambda (=? <? size)
                  ((pnew =? <? size #f))))))

  #|proc:fxtreeset?
  Return whether `object` is a fixnum treeset. Any object may be tested.
  |#
  #|record:$fxtreeset
  Ordered set record restricted to fixnum members.
  |#
  (define-record-type ($fxtreeset mk-fxtreeset fxtreeset?)
    (parent rbtree) (nongenerative) (opaque #t)
    (protocol (lambda (pnew) (lambda (=? <? size) ((pnew =? <? size #t))))))
  #|proc:treeset?
  Return whether `object` is a generic or fixnum treeset. Any object may be tested.
  |#
  (define treeset? treeset-record?)

  #|proc:make-fxtreeset
  Construct a treeset whose items are exact fixnums.
  `=?` compares items for equality and `<?` orders items. Returns an empty treeset.
  |#
  (define make-fxtreeset
    (lambda (=? <?)
      (pcheck ([procedure? =? <?]) (mk-fxtreeset =? <? 0))))

  (define make-treeset-like
    (lambda (source)
      ((if (fxtreeset? source) make-fxtreeset make-treeset)
       (rbtree-=? source) (rbtree-<? source))))

  (define check-fxtreeset-map-proc
    (lambda (who proc)
      (lambda args
        (let ([item (apply proc args)])
          (pcheck ([fixnum? item]) item)))))



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
                (for-each (lambda (x) (rbtree-set! who ts #f x *dummy-v*)) args)
                ts))))

  #|proc:fxtreeset
  Create a fixnum-key treeset initialized with the supplied fixnums.
  `=?` compares items and `<?` orders items. Returns the populated treeset.
  |#
  (define-who fxtreeset
    (lambda (=? <? . args)
      (pcheck ([procedure? =? <?])
              (let ([ts (make-fxtreeset =? <?)])
                (for-each (lambda (x)
                            (unless (fixnum? x)
                              (errorf who "not a fixnum treeset item: ~a" x))
                            (fxtreeset-add! ts x))
                          args)
                ts))))


  #|proc:treeset-empty?
  Return whether the treeset is empty.
  |#
  (define-who %treeset-empty?
    (lambda (ts)
      (pcheck ([treeset? ts])
              (fx= 0 (rbtree-size ts)))))


  #|proc:treeset-add!
  Add the new value `v` to the treeset `ts`.
  |#
  (define-who treeset-add!
    (lambda (ts v)
      (pcheck ([treeset? ts])
              (rbtree-set! who ts #f v *dummy-v*))))


  #|proc:treeset-delete!
  Remove the value `v` from the treeset `ts`.
  If `v` is absent, the treeset is unchanged.
  |#
  (define-who treeset-delete!
    (lambda (ts v)
      (pcheck ([treeset? ts])
              (when (rbtree-contains? who ts v)
                (rbtree-delete! who ts #f v)))))


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
  (define-who %treeset-size
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
  If no such item exists, `default` is returned; it defaults to #f. Supply a
  unique default when #f is a valid item.
  |#
  (define-who treeset-search
    (case-lambda
      [(ts pred) (treeset-search ts pred #f)]
      [(ts pred default)
       (pcheck ([treeset? ts] [procedure? pred])
               (call-with-values (lambda () (rbtree-search who ts (lambda (k v) (pred k))))
                 (lambda (k v) (if (eq? k *dummy-v*) default k))))]))


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

  If the successor of `v` does not exist, `default` is returned; it defaults to #f.
  |#
  (define-who treeset-successor
    (case-lambda
      [(ts v) (treeset-successor ts v #f)]
      [(ts v default)
       (pcheck ([treeset? ts])
               (call-with-values (lambda () (rbtree-successor who ts v))
                 (lambda (k value) (if (eq? k *dummy-v*) default k))))]))


  #|proc:treeset-predecessor
  Return the predecessor of `v` in the treeset `ts`.

  If the predecessor of `v` does not exist, `default` is returned; it defaults to #f.
  |#
  (define-who treeset-predecessor
    (case-lambda
      [(ts v) (treeset-predecessor ts v #f)]
      [(ts v default)
       (pcheck ([treeset? ts])
               (call-with-values (lambda () (rbtree-predecessor who ts v))
                 (lambda (k value) (if (eq? k *dummy-v*) default k))))]))


  #|proc:treeset-min
  Return the minimum value in the treeset `ts`.

  If the treeset is empty, `default` is returned; it defaults to #f.
  |#
  (define-who treeset-min
    (case-lambda
      [(ts) (treeset-min ts #f)]
      [(ts default)
       (pcheck ([treeset? ts])
               (call-with-values (lambda () (rbtree-min who ts))
                 (lambda (k value) (if (eq? k *dummy-v*) default k))))]))


  #|proc:treeset-max
  Return the maximum value in the treeset `ts`.

  If the treeset is empty, `default` is returned; it defaults to #f.
  |#
  (define-who treeset-max
    (case-lambda
      [(ts) (treeset-max ts #f)]
      [(ts default)
       (pcheck ([treeset? ts])
               (call-with-values (lambda () (rbtree-max who ts))
                 (lambda (k value) (if (eq? k *dummy-v*) default k))))]))


  #|proc:treeset-filter
  Return a new treeset whose items are those in `ts`
  such that `(pred x)` returns #t, where `x` is an item in `ts`.
  |#
  (define-who treeset-filter
    (lambda (pred ts)
      (pcheck ([procedure? pred] [treeset? ts])
              (let ([newts (make-treeset-like ts)])
                (rbtree-visit who (lambda (k v) (when (pred k) (rbtree-set! who newts (fxtreeset? newts) k *dummy-v*))) ts)
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
                              (rbtree-delete! who ts #f v)))
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
                                                    (rbtree-set! who T (fxtreeset? T) k *dummy-v*)
                                                    (rbtree-set! who F (fxtreeset? F) k *dummy-v*)))
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
                                                        (rbtree-set! who newts (fxtreeset? newts) k *dummy-v*))
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
                            (rbtree-visit who (lambda (k v) (rbtree-set! who newts (fxtreeset? newts) k *dummy-v*)) ts)
                            (for-each (lambda (ts)
                                        (rbtree-visit who
                                                      (lambda (k v)
                                                        (when (rbtree-contains? who newts k)
                                                          (rbtree-delete! who newts (fxtreeset? newts) k)))
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
                            (for-each (lambda (k) (rbtree-delete! who newts (fxtreeset? newts) k)) (lb))
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
                                                (rbtree-set! who newts (fxtreeset? newts) k *dummy-v*))
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
                    (for-each (lambda (k) (rbtree-delete! who newts (fxtreeset? newts) k)) (lb))
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



  #|proc:list->fxtreeset
  Return a new fixnum treeset containing the fixnums in list `items`.
  Equality predicate `equal?` and ordering predicate `less?` each take two fixnum items.
  Duplicate items are stored once.
  |#
  (define list->fxtreeset
    (lambda (equal? less? items)
      (pcheck ([procedure? equal? less?] [list? items])
              (let ([result (make-fxtreeset equal? less?)])
                (for-each (lambda (item) (fxtreeset-add! result item)) items)
                result))))

  #|proc:vector->fxtreeset
  Return a new fixnum treeset containing the fixnums in vector `items`.
  Equality predicate `equal?` and ordering predicate `less?` each take two fixnum items.
  Duplicate items are stored once.
  |#
  (define vector->fxtreeset
    (lambda (equal? less? items)
      (pcheck ([procedure? equal? less?] [vector? items])
              (let ([result (make-fxtreeset equal? less?)])
                (vector-for-each (lambda (item) (fxtreeset-add! result item)) items)
                result))))

  #|proc:fxtreeset-empty?
  Return whether the treeset is empty.
  |#
  (define-who %fxtreeset-empty?
    (lambda (ts)
      (pcheck ([fxtreeset? ts])
              (fx= 0 (rbtree-size ts)))))


  #|proc:fxtreeset-add!
  Add the new value `v` to the treeset `ts`.
  |#
  (define-who fxtreeset-add!
    (lambda (ts v)
      (pcheck ([fxtreeset? ts] [fixnum? v])
              (rbtree-set! who ts #t v *dummy-v*))))


  #|proc:fxtreeset-delete!
  Remove the value `v` from the treeset `ts`.
  If `v` is absent, the treeset is unchanged.
  |#
  (define-who fxtreeset-delete!
    (lambda (ts v)
      (pcheck ([fxtreeset? ts] [fixnum? v])
              (when (rbtree-contains? who ts v)
                (rbtree-delete! who ts #t v)))))


  #|proc:fxtreeset-clear!
  Remove all items from the treeset `ts`.
  |#
  (define-who fxtreeset-clear!
    (lambda (ts)
      (pcheck ([fxtreeset? ts])
              (rbtree-clear! who ts))))


  #|proc:fxtreeset-size
  Return the number of items in the treeset `ts`.
  |#
  (define-who %fxtreeset-size
    (lambda (ts)
      (pcheck ([fxtreeset? ts])
              (rbtree-size ts))))


  #|proc:fxtreeset-contains?
  Return whether the treeset `ts` contains the value `v`.
  |#
  (define-who fxtreeset-contains?
    (lambda (ts v)
      (pcheck ([fxtreeset? ts] [fixnum? v])
              (rbtree-contains? who ts v))))


  #|proc:fxtreeset-contains/p?
  Return whether the treeset `ts` contains the item `v`
  such that `(pred v)` returns #t.
  |#
  (define-who fxtreeset-contains/p?
    (lambda (ts pred)
      (pcheck ([fxtreeset? ts] [procedure? pred])
              (rbtree-contains/p? who ts (lambda (k v) (pred k))))))




  #|proc:fxtreeset-search
  Return the 1st item in the treeset `ts` that satisfies the predicate `pred`.
  If no such item exists, `default` is returned; it defaults to #f. Supply a
  unique default when #f is a valid item.
  |#
  (define-who fxtreeset-search
    (case-lambda
      [(ts pred) (fxtreeset-search ts pred #f)]
      [(ts pred default)
       (pcheck ([fxtreeset? ts] [procedure? pred])
               (call-with-values (lambda () (rbtree-search who ts (lambda (k v) (pred k))))
                 (lambda (k v) (if (eq? k *dummy-v*) default k))))]))


  #|proc:fxtreeset-search*
  Return the the list of items in the treeset `ts` that satify the predicate `pred`.

  By default the items satisfying `pred` are returned in a list.

  If `collect` is given, it is applied to every item that satisfies `pred`
  in the treeset. This is useful when collecting the desired items in custom
  data structures.
  |#
  (define-who fxtreeset-search*
    (case-lambda
      [(ts pred)
       (pcheck ([fxtreeset? ts] [procedure? pred])
               (let ([lb (make-list-builder)])
                 (rbtree-visit who (lambda (k v) (when (pred k) (lb k))) ts)
                 (lb)))]
      [(ts pred collect)
       (pcheck ([fxtreeset? ts] [procedure? pred collect])
               (rbtree-visit who (lambda (k v) (when (pred k) (collect k))) ts))]))


  #|proc:fxtreeset-successor
  Return the successor of `v` in the treeset `ts`.

  If the successor of `v` does not exist, `default` is returned; it defaults to #f.
  |#
  (define-who fxtreeset-successor
    (case-lambda
      [(ts v) (fxtreeset-successor ts v #f)]
      [(ts v default)
       (pcheck ([fxtreeset? ts] [fixnum? v])
               (call-with-values (lambda () (rbtree-successor who ts v))
                 (lambda (k value) (if (eq? k *dummy-v*) default k))))]))


  #|proc:fxtreeset-predecessor
  Return the predecessor of `v` in the treeset `ts`.

  If the predecessor of `v` does not exist, `default` is returned; it defaults to #f.
  |#
  (define-who fxtreeset-predecessor
    (case-lambda
      [(ts v) (fxtreeset-predecessor ts v #f)]
      [(ts v default)
       (pcheck ([fxtreeset? ts] [fixnum? v])
               (call-with-values (lambda () (rbtree-predecessor who ts v))
                 (lambda (k value) (if (eq? k *dummy-v*) default k))))]))


  #|proc:fxtreeset-min
  Return the minimum value in the treeset `ts`.

  If the treeset is empty, `default` is returned; it defaults to #f.
  |#
  (define-who fxtreeset-min
    (case-lambda
      [(ts) (fxtreeset-min ts #f)]
      [(ts default)
       (pcheck ([fxtreeset? ts])
               (call-with-values (lambda () (rbtree-min who ts))
                 (lambda (k value) (if (eq? k *dummy-v*) default k))))]))


  #|proc:fxtreeset-max
  Return the maximum value in the treeset `ts`.

  If the treeset is empty, `default` is returned; it defaults to #f.
  |#
  (define-who fxtreeset-max
    (case-lambda
      [(ts) (fxtreeset-max ts #f)]
      [(ts default)
       (pcheck ([fxtreeset? ts])
               (call-with-values (lambda () (rbtree-max who ts))
                 (lambda (k value) (if (eq? k *dummy-v*) default k))))]))


  #|proc:fxtreeset-filter
  Return a new treeset whose items are those in `ts`
  such that `(pred x)` returns #t, where `x` is an item in `ts`.
  |#
  (define-who fxtreeset-filter
    (lambda (pred ts)
      (pcheck ([procedure? pred] [fxtreeset? ts])
              (let ([newts (make-fxtreeset-like ts)])
                (rbtree-visit who (lambda (k v) (when (pred k) (rbtree-set! who newts (fxtreeset? newts) k *dummy-v*))) ts)
                newts))))


  #|proc:fxtreeset-filter!
  Filter the treeset so that after the operation, `ts` only contains
  items `x` such that `(pred x)` returns #t.
  |#
  (define-who fxtreeset-filter!
    (lambda (pred ts)
      (pcheck ([procedure? pred] [fxtreeset? ts])
              (let ([lb (make-list-builder)])
                (rbtree-visit who (lambda (k v) (lb k)) ts)
                (for-each (lambda (v)
                            (unless (pred v)
                              (rbtree-delete! who ts #t v)))
                          (lb))
                ts))))


  #|proc:fxtreeset-partition
  Apply `pred` to every item in treeset `ts` and return two values,
  the first one a treeset of items for which `(pred item)` is true,
  the second one a treeset of the remaining items. Both preserve the input backend.
  |#
  (define-who fxtreeset-partition
    (lambda (pred ts)
      (pcheck ([procedure? pred] [fxtreeset? ts])
              (let ([T (make-fxtreeset-like ts)]
                    [F (make-fxtreeset-like ts)])
                (rbtree-visit who (lambda (k v) (if (pred k)
                                                    (rbtree-set! who T (fxtreeset? T) k *dummy-v*)
                                                    (rbtree-set! who F (fxtreeset? F) k *dummy-v*)))
                              ts)
                (values T F)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   set operations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  #|proc:fxtreeset+
  Compute the union of the treesets, i.e., the treeset that contains all items
  in all the given treesets.
  If only one treeset is given, it is returned immediately.

  The `=?` and `<?` procedures of the returned treeset is taken from the first input treeset.
  |#
  (define-who fxtreeset+
    (lambda (ts . ts*)
      (pcheck ([fxtreeset? ts])
              (if (null? ts*)
                  ts
                  (pcheck ([all-fxtreesets? ts*])
                          (let ([newts (make-fxtreeset-like ts)])
                            (for-each (lambda (ts)
                                        (rbtree-visit who
                                                      (lambda (k v)
                                                        (rbtree-set! who newts (fxtreeset? newts) k *dummy-v*))
                                                      ts))
                                      (cons ts ts*))
                            newts))))))


  #|proc:fxtreeset-
  Compute the difference of the treesets, i.e., the treeset that contains those items
  that are in the first treeset, but are not in the rest of the treesets.
  If only one treeset is given, it is returned immediately.

  The `=?` and `<?` procedures of the returned treeset is taken from the first input treeset.
  |#
  (define-who fxtreeset-
    (lambda (ts . ts*)
      (pcheck ([fxtreeset? ts])
              (if (null? ts*)
                  ts
                  (pcheck ([all-fxtreesets? ts*])
                          (let ([newts (make-fxtreeset-like ts)])
                            (rbtree-visit who (lambda (k v) (rbtree-set! who newts (fxtreeset? newts) k *dummy-v*)) ts)
                            (for-each (lambda (ts)
                                        (rbtree-visit who
                                                      (lambda (k v)
                                                        (when (rbtree-contains? who newts k)
                                                          (rbtree-delete! who newts (fxtreeset? newts) k)))
                                                      ts))
                                      ts*)
                            newts))))))


  #|proc:fxtreeset&
  Compute the intersection of the treesets, i.e., the treeset whose items are contained
  in all given treesets.
  If only one treeset is given, it is returned immediately.

  The `=?` and `<?` procedures of the returned treeset is taken from the first input treeset.
  |#
  (define-who fxtreeset&
    (lambda (ts . ts*)
      (pcheck ([fxtreeset? ts])
              (if (null? ts*)
                  ts
                  (pcheck ([all-fxtreesets? ts*])
                          (let ([newts (apply fxtreeset+ ts ts*)] [lb (make-list-builder)])
                            (rbtree-visit who
                                          (lambda (k v)
                                            (unless (andmap (lambda (ts) (rbtree-contains? who ts k))
                                                            (cons ts ts*))
                                              (lb k)))
                                          newts)
                            (for-each (lambda (k) (rbtree-delete! who newts (fxtreeset? newts) k)) (lb))
                            newts))))))


  #|proc:fxtreeset^
  Compute the symmetric difference of the treesets, i.e., the difference of the union
  and the intersection of the treesets.
  If only one treeset is given, it is returned immediately.

  The `=?` and `<?` procedures of the returned treeset is taken from the first input treeset.
  |#
  (define-who fxtreeset^
    (lambda (ts . ts*)
      (pcheck ([fxtreeset? ts] [all-fxtreesets? ts*])
              (if (null? ts*)
                  ts
                  (let ([newts (make-fxtreeset-like ts)]
                        [lb (make-list-builder)])
                    ;; union
                    (for-each (lambda (ts)
                                (rbtree-visit who
                                              (lambda (k v)
                                                (rbtree-set! who newts (fxtreeset? newts) k *dummy-v*))
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
                    (for-each (lambda (k) (rbtree-delete! who newts (fxtreeset? newts) k)) (lb))
                    newts)))))


;;;; imperative versions

  #|proc:fxtreeset+!
  Replace treeset `ts` with the union of `ts` and the additional treesets `ts*`.
  Return `ts`; without additional sets it is unchanged. Aliased operands are supported.
  Compute using the first set's comparators and preserve its generic or fixnum backend.
  |#  (define-who fxtreeset+!
    (lambda (ts . ts*)
      (pcheck ([fxtreeset? ts] [all-fxtreesets? ts*])
              (unless (null? ts*)
                (let ([result (apply fxtreeset+ ts ts*)])
                  (fxtreeset-clear! ts)
                  (for-each (lambda (item) (fxtreeset-add! ts item)) (fxtreeset->list result))))
              ts)))


  #|proc:fxtreeset-!
  Replace treeset `ts` with the difference of `ts` and the additional treesets `ts*`.
  Return `ts`; without additional sets it is unchanged. Aliased operands are supported.
  Compute using the first set's comparators and preserve its generic or fixnum backend.
  |#  (define-who fxtreeset-!
    (lambda (ts . ts*)
      (pcheck ([fxtreeset? ts] [all-fxtreesets? ts*])
              (unless (null? ts*)
                (let ([result (apply fxtreeset- ts ts*)])
                  (fxtreeset-clear! ts)
                  (for-each (lambda (item) (fxtreeset-add! ts item)) (fxtreeset->list result))))
              ts)))


  #|proc:fxtreeset&!
  Replace treeset `ts` with the intersection of `ts` and the additional treesets `ts*`.
  Return `ts`; without additional sets it is unchanged. Aliased operands are supported.
  Compute using the first set's comparators and preserve its generic or fixnum backend.
  |#  (define-who fxtreeset&!
    (lambda (ts . ts*)
      (pcheck ([fxtreeset? ts] [all-fxtreesets? ts*])
              (unless (null? ts*)
                (let ([result (apply fxtreeset& ts ts*)])
                  (fxtreeset-clear! ts)
                  (for-each (lambda (item) (fxtreeset-add! ts item)) (fxtreeset->list result))))
              ts)))


  #|proc:fxtreeset^!
  Replace treeset `ts` with the union minus intersection of `ts` and the additional treesets
  `ts*`.
  Return `ts`; without additional sets it is unchanged. Aliased operands are supported.
  Compute using the first set's comparators and preserve its generic or fixnum backend.
  |#  (define-who fxtreeset^!
    (lambda (ts . ts*)
      (pcheck ([fxtreeset? ts] [all-fxtreesets? ts*])
              (unless (null? ts*)
                (let ([result (apply fxtreeset^ ts ts*)])
                  (fxtreeset-clear! ts)
                  (for-each (lambda (item) (fxtreeset-add! ts item)) (fxtreeset->list result))))
              ts)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   iterations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; no in-place maps since we can't modify the tree structure

  (define all-fxtreesets? (lambda (x*) (andmap fxtreeset? x*)))
  (define fx-check-size
    (case-lambda
      [(who x0 x1)
       (unless (fx= (fxtreeset-size x0) (fxtreeset-size x1))
         (errorf who "treesets are not of the same size"))]
      [(who x0 . x*)
       (unless (null? x*)
         (unless (apply fx= (fxtreeset-size x0) (map fxtreeset-size x*))
           (errorf who "treesets are not of the same size")))]))


  #|proc:fxtreeset-andmap
  Traverse the input treesets in ascending comparator order.
  `proc` has signature (item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return #f at the first false callback result; otherwise return #t.
  |#
  (define-who fxtreeset-andmap
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [fxtreeset? ts0])
               (rbtree-andmap1 who proc ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [fxtreeset? ts0 ts1])
               (fx-check-size who ts0 ts1)
               (rbtree-andmap1 who proc ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [fxtreeset? ts0] [all-fxtreesets? ts*])
               (apply fx-check-size who ts0 ts*)
               (apply rbtree-andmap1 who proc ts0 ts*))]))


  #|proc:fxtreeset-ormap
  Traverse the input treesets in ascending comparator order.
  `proc` has signature (item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return the first true callback result, or #f if no result is true.
  |#
  (define-who fxtreeset-ormap
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [fxtreeset? ts0])
               (rbtree-ormap1 who proc ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [fxtreeset? ts0 ts1])
               (fx-check-size who ts0 ts1)
               (rbtree-ormap1 who proc ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [fxtreeset? ts0] [all-fxtreesets? ts*])
               (apply fx-check-size who ts0 ts*)
               (apply rbtree-ormap1 who proc ts0 ts*))]))


  #|proc:fxtreeset-map
  Traverse the input treesets in ascending comparator order.
  `proc` has signature (item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return a new treeset using the first input's comparators and backend.
  The callback returns the new item.
  |#
  (define-who fxtreeset-map
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [fxtreeset? ts0])
               (rbtree-map1 who (check-fxtreeset-map-proc who proc)
                            (make-fxtreeset-like ts0) ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [fxtreeset? ts0 ts1])
               (fx-check-size who ts0 ts1)
               (rbtree-map1 who (check-fxtreeset-map-proc who proc)
                            (make-fxtreeset-like ts0) ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [fxtreeset? ts0] [all-fxtreesets? ts*])
               (apply fx-check-size who ts0 ts*)
               (apply rbtree-map1 who (check-fxtreeset-map-proc who proc)
                      (make-fxtreeset-like ts0) ts0 ts*))]))


  #|proc:fxtreeset-map/i
  Traverse the input treesets in ascending comparator order.
  `proc` has signature (index item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Return a new treeset using the first input's comparators and backend.
  The callback returns the new item.
  |#
  (define-who fxtreeset-map/i
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [fxtreeset? ts0])
               (rbtree-map/i1 who (check-fxtreeset-map-proc who proc)
                              (make-fxtreeset-like ts0) ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [fxtreeset? ts0 ts1])
               (fx-check-size who ts0 ts1)
               (rbtree-map/i1 who (check-fxtreeset-map-proc who proc)
                              (make-fxtreeset-like ts0) ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [fxtreeset? ts0] [all-fxtreesets? ts*])
               (apply fx-check-size who ts0 ts*)
               (apply rbtree-map/i1 who (check-fxtreeset-map-proc who proc)
                      (make-fxtreeset-like ts0) ts0 ts*))]))


  #|proc:fxtreeset-for-each
  Traverse the input treesets in ascending comparator order.
  `proc` has signature (item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return an unspecified value.
  |#
  (define-who fxtreeset-for-each
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [fxtreeset? ts0])
               (rbtree-for-each1 who proc ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [fxtreeset? ts0 ts1])
               (fx-check-size who ts0 ts1)
               (rbtree-for-each1 who proc ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [fxtreeset? ts0] [all-fxtreesets? ts*])
               (apply fx-check-size who ts0 ts*)
               (apply rbtree-for-each1 who proc ts0 ts*))]))


  #|proc:fxtreeset-for-each/i
  Traverse the input treesets in ascending comparator order.
  `proc` has signature (index item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Return an unspecified value.
  |#
  (define-who fxtreeset-for-each/i
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [fxtreeset? ts0])
               (rbtree-for-each/i1 who proc ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [fxtreeset? ts0 ts1])
               (fx-check-size who ts0 ts1)
               (rbtree-for-each/i1 who proc ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [fxtreeset? ts0] [all-fxtreesets? ts*])
               (apply fx-check-size who ts0 ts*)
               (apply rbtree-for-each/i1 who proc ts0 ts*))]))


;;;; folds


  #|proc:fxtreeset-fold-left
  Traverse the input treesets in ascending comparator order.
  `proc` has signature (acc item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size.
  Return the accumulated value; `acc` is its initial value.
  |#
  (define-who fxtreeset-fold-left
    (case-lambda
      [(proc acc ts0)
       (pcheck ([procedure? proc] [fxtreeset? ts0])
               (rbtree-fold-left1 who proc acc ts0))]
      [(proc acc ts0 ts1)
       (pcheck ([procedure? proc] [fxtreeset? ts0 ts1])
               (fx-check-size who ts0 ts1)
               (rbtree-fold-left1 who proc acc ts0 ts1))]
      [(proc acc ts0 . ts*)
       (pcheck ([procedure? proc] [fxtreeset? ts0] [all-fxtreesets? ts*])
               (apply fx-check-size who ts0 ts*)
               (apply rbtree-fold-left1 who proc acc ts0 ts*))]))


  #|proc:fxtreeset-fold-left/i
  Traverse the input treesets in ascending comparator order.
  `proc` has signature (index acc item0 item1 ...); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Return the accumulated value; `acc` is its initial value.
  |#
  (define-who fxtreeset-fold-left/i
    (case-lambda
      [(proc acc ts0)
       (pcheck ([procedure? proc] [fxtreeset? ts0])
               (rbtree-fold-left/i1 who proc acc ts0))]
      [(proc acc ts0 ts1)
       (pcheck ([procedure? proc] [fxtreeset? ts0 ts1])
               (fx-check-size who ts0 ts1)
               (rbtree-fold-left/i1 who proc acc ts0 ts1))]
      [(proc acc ts0 . ts*)
       (pcheck ([procedure? proc] [fxtreeset? ts0] [all-fxtreesets? ts*])
               (apply fx-check-size who ts0 ts*)
               (apply rbtree-fold-left/i1 who proc acc ts0 ts*))]))


  #|proc:fxtreeset-fold-right
  Traverse the input treesets in descending comparator order.
  `proc` has signature (item0 item1 ... acc); collections supply ordered positions.
  Input collections must have equal size.
  Return the accumulated value; `acc` is its initial value.
  |#
  (define-who fxtreeset-fold-right
    (case-lambda
      [(proc acc ts0)
       (pcheck ([procedure? proc] [fxtreeset? ts0])
               (rbtree-fold-right1 who proc acc ts0))]
      [(proc acc ts0 ts1)
       (pcheck ([procedure? proc] [fxtreeset? ts0 ts1])
               (fx-check-size who ts0 ts1)
               (rbtree-fold-right1 who proc acc ts0 ts1))]
      [(proc acc ts0 . ts*)
       (pcheck ([procedure? proc] [fxtreeset? ts0] [all-fxtreesets? ts*])
               (apply fx-check-size who ts0 ts*)
               (apply rbtree-fold-right1 who proc acc ts0 ts*))]))


  #|proc:fxtreeset-fold-right/i
  Traverse the input treesets in descending comparator order.
  `proc` has signature (index item0 item1 ... acc); collections supply ordered positions.
  Input collections must have equal size. Indices are zero-based inorder positions.
  Return the accumulated value; `acc` is its initial value.
  |#
  (define-who fxtreeset-fold-right/i
    (case-lambda
      [(proc acc ts0)
       (pcheck ([procedure? proc] [fxtreeset? ts0])
               (rbtree-fold-right/i1 who proc acc ts0))]
      [(proc acc ts0 ts1)
       (pcheck ([procedure? proc] [fxtreeset? ts0 ts1])
               (fx-check-size who ts0 ts1)
               (rbtree-fold-right/i1 who proc acc ts0 ts1))]
      [(proc acc ts0 . ts*)
       (pcheck ([procedure? proc] [fxtreeset? ts0] [all-fxtreesets? ts*])
               (apply fx-check-size who ts0 ts*)
               (apply rbtree-fold-right/i1 who proc acc ts0 ts*))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

  (define make-fxtreeset-like
    (lambda (source)
      (make-fxtreeset (rbtree-=? source) (rbtree-<? source))))
  #|proc:fxtreeset->list Convert a fixnum treeset into a list.|#
  (define-who fxtreeset->list
    (case-lambda
      [(ts) (fxtreeset->list ts 'in)]
      [(ts order)
       (pcheck ([fxtreeset? ts])
         (let ([lb (make-list-builder)])
           (case order
             [in (rbtree-visit-inorder who (lambda (k v) (lb k)) ts)]
             [pre (rbtree-visit-preorder who (lambda (k v) (lb k)) ts)]
             [post (rbtree-visit-postorder who (lambda (k v) (lb k)) ts)]
             [else (errorf who "invalid traversal order: ~a" order)])
           (lb))) ]))
  #|proc:fxtreeset->vector Convert a fixnum treeset into a vector.|#
  (define-who fxtreeset->vector
    (case-lambda
      [(ts) (fxtreeset->vector ts 'in)]
      [(ts order)
       (pcheck ([fxtreeset? ts])
         (let* ([vec (make-vector (fxtreeset-size ts) 0)] [i 0]
                [add! (lambda (k v) (vector-set! vec i k) (set! i (fx1+ i)))])
           (case order
             [in (rbtree-visit-inorder who add! ts)]
             [pre (rbtree-visit-preorder who add! ts)]
             [post (rbtree-visit-postorder who add! ts)]
             [else (errorf who "invalid traversal order: ~a" order)]) vec))]))
  (define write-treeset
                 (lambda (r p wr)
                   (display "#[treeset (" p)
                   (if (fx= 0 (rbtree-size r))
                       (display ")]" p)
                       (begin
                         (let ([n (rbtree-size r)] [i 0])
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
  ;; Route the core predicates through the generator.  The original
  ;; implementations are retained as private workers so the generated
  ;; procedures share one expansion shape while preserving their exact
  ;; validation and behavior.
  
  
  (define-treeset-procedure
    (treeset-empty? fxtreeset-empty?)
    (treeset? fxtreeset?)
    (make-treeset make-fxtreeset)
    ((ts) (%treeset-empty? ts))
    ((ts) (%fxtreeset-empty? ts)))

  
  
  (define-treeset-procedure
    (treeset-size fxtreeset-size)
    (treeset? fxtreeset?)
    (make-treeset make-fxtreeset)
    ((ts) (%treeset-size ts))
    ((ts) (%fxtreeset-size ts)))


;;;; Iterator extension registration
;;;;===----------------------------------------------------------------------===

  (iter-register-source!
   treeset?
   (lambda (ts)
     (let ([cursor (rbtree-inorder-cursor ts)])
       (make-iter
        (lambda ()
          (call-with-values cursor
            (lambda (key value)
              (if (eq? key *dummy-v*) iter-end key))))
        (lambda () (set! cursor (rbtree-inorder-cursor ts)))))))

;;;;===----------------------------------------------------------------------===
;;;; Navigator extension registration
;;;;===----------------------------------------------------------------------===

  (nav-register-set!
   treeset? treeset->list
   (lambda (ts members)
     (let ([copy (treeset-map (lambda (item) item) ts)])
       (treeset-clear! copy)
       (for-each (lambda (member) (treeset-add! copy member)) members)
       copy))
   (lambda (ts members)
     (treeset-clear! ts)
     (for-each (lambda (member) (treeset-add! ts member)) members)
     ts)
   (lambda (ts member)
     (let ([copy (treeset-map (lambda (item) item) ts)])
       (treeset-delete! copy member)
       copy))
   (lambda (ts member)
     (treeset-delete! ts member)
     ts))

  (record-writer (type-descriptor $treeset) write-treeset)

  (iter-register-source!
   fxtreeset?
   (lambda (ts)
     (let ([cursor (rbtree-inorder-cursor ts)])
       (make-iter
        (lambda ()
          (call-with-values cursor
            (lambda (key value) (if (eq? key *dummy-v*) iter-end key))))
        (lambda () (set! cursor (rbtree-inorder-cursor ts)))))))

  (nav-register-set!
   fxtreeset? fxtreeset->list
   (lambda (ts members)
     (let ([copy (fxtreeset-map (lambda (item) item) ts)])
       (fxtreeset-clear! copy)
       (for-each (lambda (member) (fxtreeset-add! copy member)) members) copy))
   (lambda (ts members)
     (fxtreeset-clear! ts)
     (for-each (lambda (member) (fxtreeset-add! ts member)) members) ts)
   (lambda (ts member)
     (let ([copy (fxtreeset-map (lambda (item) item) ts)])
       (fxtreeset-delete! copy member) copy))
   (lambda (ts member) (fxtreeset-delete! ts member) ts))
  (record-writer (type-descriptor $fxtreeset) write-treeset)

  )
