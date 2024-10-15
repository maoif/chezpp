(library (chezpp treeset)
  (export make-treeset treeset treeset-empty? treeset-size
          treeset-add! treeset-delete! treeset-clear!

          treeset-contains? treeset-contains/p?
          treeset-filter treeset-filter! treeset-partition

          treeset-successor treeset-predecessor
          treeset-min treeset-max

          treeset-map treeset-map/i
          treeset-for-each treeset-for-each/i
          treeset-fold-left treeset-fold-left/i
          treeset-fold-right treeset-fold-right/i

          treeset->list list->treeset
          treeset->vector vector->treeset)
  (import (chezpp chez)
          (chezpp list)
          (chezpp internal)
          (chezpp utils)
          (chezpp private rbtree))


  (define-record-type ($treeset mk-treeset treeset?)
    (parent rbtree) (nongenerative) (opaque #t)
    (protocol (lambda (pnew)
                (lambda (=? <? size)
                  ((pnew =? <? size))))))

  ;; dummy value for all keys
  (define V #f)


  #|doc
  Construct a treeset object.
  `=?` is used by the treeset internally to do equality comparison of items;
  `<?` is used by the treeset internally to do order comparison.
  |#
  (define make-treeset
    (lambda (=? <?)
      (mk-treeset =? <? 0)))


  #|doc
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


  #|doc
  Return whether the treeset is empty.
  |#
  (define-who treeset-empty?
    (lambda (ts)
      (pcheck ([treeset? ts])
              (fx= 0 (rbtree-size ts)))))


  #|doc
  Add the new value `v` to the treeset `ts`.
  |#
  (define-who treeset-add!
    (lambda (ts v)
      (pcheck ([treeset? ts])
              (rbtree-set! who ts v V))))


  #|doc
  Remove the value `v` from the treeset `ts`.

  An error is raised if `v` does not exist.
  |#
  (define-who treeset-delete!
    (lambda (ts v)
      (pcheck ([treeset? ts])
              (rbtree-delete! who ts v))))


  #|doc
  Remove all items from the treeset `ts`.
  |#
  (define-who treeset-clear!
    (lambda (ts)
      (pcheck ([treeset? ts])
              (rbtree-clear! who ts))))


  #|doc
  Return the number of items in the treeset `ts`.
  |#
  (define-who treeset-size
    (lambda (ts)
      (pcheck ([treeset? ts])
              (rbtree-size ts))))


  #|doc
  Return whether the treeset `ts` contains the value `v`.
  |#
  (define-who treeset-contains?
    (lambda (ts v)
      (pcheck ([treeset? ts])
              (rbtree-contains? who ts v))))


  #|doc
  Return whether the treeset `ts` contains the item `v`
  such that `(pred v)` returns #t.
  |#
  (define-who treeset-contains/p?
    (lambda (ts pred)
      (pcheck ([treeset? ts] [procedure? pred])
              (rbtree-contains/p? who ts pred))))


  (define K? (lambda (n) (if (pair? n) (car n) n)))


  #|doc
  Return the successor of `v` in the treeset `ts`.

  If the successor of `v` does not exist, #f is returned.
  |#
  (define-who treeset-successor
    (lambda (ts v)
      (pcheck ([treeset? ts])
              (K? (rbtree-successor who ts v)))))


  #|doc
  Return the predecessor of `v` in the treeset `ts`.

  If the predecessor of `v` does not exist, #f is returned.
  |#
  (define-who treeset-predecessor
    (lambda (ts v)
      (pcheck ([treeset? ts])
              (K? (rbtree-predecessor who ts v)))))


  #|doc
  Return the minimum value in the treeset `ts`.

  If the treeset is empty, #f is returned.
  |#
  (define-who treeset-min
    (lambda (ts)
      (pcheck ([treeset? ts])
              (K? (rbtree-min who ts)))))


  #|doc
  Return the maximum value in the treeset `ts`.

  If the treeset is empty, #f is returned.
  |#
  (define-who treeset-max
    (lambda (ts)
      (pcheck ([treeset? ts])
              (K? (rbtree-max who ts)))))


  #|doc
  Return a new treeset whose items are those in `ts`
  such that `(pred x)` returns #t, where `x` is an item in `ts`.
  |#
  (define-who treeset-filter
    (lambda (pred ts)
      (pcheck ([procedure? pred] [treeset? ts])
              (let ([newts (make-treeset (rbtree-=? ts) (rbtree-<? ts))])
                (rbtree-visit who (lambda (k v) (when (pred k) (rbtree-set! who newts k V))) ts)
                newts))))


  #|doc
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


  #|doc
  Apply `pred` to every item in `tm` and return two values,
  the first one a treemap of the keys/values of `tm` for which `(pred k v)` returns #t,
  the second one a treemap of the keys/values of `tm` for which `(pred k v)` returns #f.
  |#
  (define-who treeset-partition
    (lambda (pred ts)
      (pcheck ([procedure? pred] [treeset? ts])
              (let ([T (make-treeset (rbtree-=? ts) (rbtree-<? ts))]
                    [F (make-treeset (rbtree-=? ts) (rbtree-<? ts))])
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


  #|doc
  Union.
  |#
  (define-who treeset+
    (lambda (ts . ts*)
      (pcheck ([treeset? ts])
              (if (null? ts*)
                  ts
                  (todo)))))


  #|doc
  Difference.
  |#
  (define-who treeset-
    (lambda (ts . ts*)
      (pcheck ([treeset? ts])
              (if (null? ts*)
                  ts
                  (todo)))))


  #|doc
  Intersection.
  |#
  (define-who treeset&
    (lambda (ts . ts*)
      (pcheck ([treeset? ts])
              (if (null? ts*)
                  ts
                  (todo)))))


  #|doc
  Disjoint union.
  |#
  (define-who treeset^
    (lambda (ts . ts*)
      (pcheck ([treeset? ts])
              (if (null? ts*)
                  ts
                  (todo)))))


;;;; imperative versions

  #|doc
  Union.
  |#
  (define-who treeset+!
    (lambda (ts . ts*)
      (pcheck ([treeset? ts])
              (if (null? ts*)
                  ts
                  (todo)))))


  #|doc
  Difference.
  |#
  (define-who treeset-!
    (lambda (ts . ts*)
      (pcheck ([treeset? ts])
              (if (null? ts*)
                  ts
                  (todo)))))


  #|doc
  Intersection.
  |#
  (define-who treeset&!
    (lambda (ts . ts*)
      (pcheck ([treeset? ts])
              (if (null? ts*)
                  ts
                  (todo)))))


  #|doc
  Disjoint union.
  |#
  (define-who treeset^!
    (lambda (ts . ts*)
      (pcheck ([treeset? ts])
              (if (null? ts*)
                  ts
                  (todo)))))



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


  (define-who treeset-map
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-map1 who proc (make-treeset (rbtree-=? ts0) (rbtree-<? ts0)) ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (rbtree-map1 who proc (make-treeset (rbtree-=? ts0) (rbtree-<? ts0)) ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply rbtree-map1 who proc (make-treeset (rbtree-=? ts0) (rbtree-<? ts0)) ts0 ts*))]))


  (define-who treeset-map/i
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-map/i1 who proc (make-treeset (rbtree-=? ts0) (rbtree-<? ts0)) ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (rbtree-map/i1 who proc (make-treeset (rbtree-=? ts0) (rbtree-<? ts0)) ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply rbtree-map/i1 who proc (make-treeset (rbtree-=? ts0) (rbtree-<? ts0)) ts0 ts*))]))


  (define-who treeset-for-each
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-for-each1 who proc ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (rbtree-for-each1 who proc ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply rbtree-for-each1 who proc ts0 ts*))]))


  (define-who treeset-for-each/i
    (case-lambda
      [(proc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-for-each/i1 who proc ts0))]
      [(proc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (rbtree-for-each/i1 who proc ts0 ts1))]
      [(proc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply rbtree-for-each/i1 who proc ts0 ts*))]))


;;;; folds


  (define-who treeset-fold-left
    (case-lambda
      [(proc acc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-fold-left1 who proc acc ts0))]
      [(proc acc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (rbtree-fold-left1 who proc acc ts0 ts1))]
      [(proc acc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply rbtree-fold-left1 who proc acc ts0 ts*))]))


  (define-who treeset-fold-left/i
    (case-lambda
      [(proc acc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-fold-left/i1 who proc acc ts0))]
      [(proc acc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (rbtree-fold-left/i1 who proc acc ts0 ts1))]
      [(proc acc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply rbtree-fold-left/i1 who proc acc ts0 ts*))]))


  (define-who treeset-fold-right
    (case-lambda
      [(proc acc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-fold-right1 who proc acc ts0))]
      [(proc acc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (rbtree-fold-right1 who proc acc ts0 ts1))]
      [(proc acc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply rbtree-fold-right1 who proc acc ts0 ts*))]))


  (define-who treeset-fold-right/i
    (case-lambda
      [(proc acc ts0)
       (pcheck ([procedure? proc] [treeset? ts0])
               (rbtree-fold-right/i1 who proc acc ts0))]
      [(proc acc ts0 ts1)
       (pcheck ([procedure? proc] [treeset? ts0 ts1])
               (rbtree-fold-right/i1 who proc acc ts0 ts1))]
      [(proc acc ts0 . ts*)
       (pcheck ([procedure? proc] [treeset? ts0] [all-treesets? ts*])
               (apply rbtree-fold-right/i1 who proc acc ts0 ts*))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   conversions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  #|doc
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


  #|doc
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


  #|doc
  Convert a list `ls` to a treeset.
  `=?` and `<?` are the same as in `treeset`.
  |#
  (define-who list->treeset
    (lambda (=? <? ls)
      (pcheck ([procedure? =? <?] [list? ls])
              (apply treeset =? <? ls))))


  #|doc
  Convert a vector `vec` to a treeset.
  `=?` and `<?` are the same as in `treeset`.
  |#
  (define-who vector->treeset
    (lambda (=? <? vec)
      (pcheck ([procedure? =? <?] [vector? vec])
              (let ([ts (make-treeset =? <?)])
                (vector-for-each (lambda (x) (treeset-add! ts x)) vec)
                ts))))



  (record-writer (type-descriptor $treeset)
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

  )
