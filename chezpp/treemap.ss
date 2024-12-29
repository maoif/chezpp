(library (chezpp treemap)
  (export make-treemap treemap treemap? treemap-empty?
          treemap-set! treemap-ref treemap-size
          treemap-delete! treemap-clear!

          treemap-keys treemap-values treemap-cells
          treemap-search treemap-search*
          treemap-contains? treemap-contains/p?
          treemap-filter treemap-filter! treemap-partition

          treemap-successor treemap-predecessor
          treemap-min treemap-max

          treemap-map treemap-map/i treemap-map! treemap-map/i!
          treemap-for-each treemap-for-each/i
          treemap-fold-left treemap-fold-left/i
          treemap-fold-right treemap-fold-right/i

          treemap->list hashtable->treemap

          $rbtree-verify)
  (import (chezpp chez)
          (chezpp internal)
          (chezpp utils)
          (chezpp list)
          (chezpp private rbtree))


  (define-record-type ($treemap mk-treemap treemap?)
    (parent rbtree) (nongenerative) (opaque #t)
    (protocol (lambda (pnew)
                (lambda (=? <? size)
                  ((pnew =? <? size))))))


  #|doc
  Construct a treemap object.
  `=?` is used by the treemap internally to do equality comparison of keys;
  `<?` is used by the treemap internally to do order comparison.
  |#
  (define-who make-treemap
    (lambda (=? <?)
      (pcheck ([procedure? =? <?])
              (mk-treemap =? <? 0))))


  #|doc
  Create a new treemap, and add the arguments to the treemap.

  `args` must be a list of pairs in which each pair's car field will be the key,
  and each pair's cdr field will be the value.

  `=?` is used by the treemap internally to do equality comparison of keys;
  `<?` is used by the treemap internally to do order comparison.
  |#
  (define-who treemap
    (lambda (=? <? . args)
      (let ([tm (make-treemap =? <?)])
        (for-each (lambda (x) (unless (pair? x) (errorf who "not a pair: ~a" x))) args)
        (for-each (lambda (x) (rbtree-set! who tm (car x) (cdr x))) args)
        tm)))


  #|doc
  Return whether the treemap is empty.
  |#
  (define-who treemap-empty?
    (lambda (tm)
      (pcheck ([treemap? tm])
              (fx= 0 (rbtree-size tm)))))


  #|doc
  Associate key `k` with value `v` in the treemap `tm`.
  If `k` already exists, its original value is replaced by `v`.
  |#
  (define-who treemap-set!
    (lambda (tm k v)
      (pcheck ([treemap? tm])
              (rbtree-set! who tm k v))))


  #|doc
  Return the value keyed by `k` in the treemap `tm`.

  If `default` is given and `k` does not exist in the treemap, `default` is returned.
  If `default` is not given and `k` does not exist, an error is raised.
  |#
  (define-who treemap-ref
    (case-lambda
      [(tm k)
       (pcheck ([treemap? tm])
               (rbtree-ref who tm k))]
      [(tm k default)
       (pcheck ([treemap? tm])
               (rbtree-ref who tm k default))]))


  #|doc
  Remove the key `k` along with its value from the treemap `tm`.
  An error is raised if `k` does not exist.
  |#
  (define-who treemap-delete!
    (lambda (tm k)
      (pcheck ([treemap? tm])
              (rbtree-delete! who tm k))))


  #|doc
  Remove all keys and values from the treemap `tm`.
  |#
  (define-who treemap-clear!
    (lambda (tm)
      (pcheck ([treemap? tm])
              (rbtree-clear! who tm))))


  #|doc
  Return the number of keys in the treemap `tm`.
  |#
  (define-who treemap-size
    (lambda (tm)
      (pcheck ([treemap? tm])
              (rbtree-size tm))))


  #|doc
  Return whether the treemap `tm` contains the key `k`.
  Comparison is performed using `=` pass to `make-treemap`.
  |#
  (define-who treemap-contains?
    (lambda (tm k)
      (pcheck ([treemap? tm])
              (rbtree-contains? who tm k))))


  #|doc
  Return whether treemap `tm` contains at least one key/value pair such that
  (pred key value) returns true.
  |#
  (define-who treemap-contains/p?
    (lambda (tm pred)
      (pcheck ([treemap? tm] [procedure? pred])
              (rbtree-contains/p? who tm pred))))


  #|doc
  Return a pair consisting of the 1st key and value in the treemap such that (pred key value) returns #t.
  |#
  (define-who treemap-search
    (lambda (tm pred)
      (pcheck ([treemap? tm] [procedure? pred])
              (rbtree-search who tm pred))))


  #|doc
  Return the the list of all key/value pairs in the treemap such that
  for each pair of key and value, (pred key value) returns #t.

  By default the items satisfying `pred` are returned in a list.

  If `collect` is given, it is applied to every key and value pair that satisfies `pred`
  in the treemap. This is useful when collecting the desired key and value pairs in custom
  data structures.
  |#
  (define-who treemap-search*
    (case-lambda
      [(tm pred)
       (pcheck ([treemap? tm] [procedure? pred])
               (let ([lb (make-list-builder)])
                 (rbtree-visit who (lambda (k v) (when (pred k v) (lb (cons k v)))) tm)
                 (lb)))]
      [(tm pred collect)
       (pcheck ([treemap? tm] [procedure? pred collect])
               (rbtree-visit who (lambda (k v) (when (pred k v) (collect k v))) tm))]))


  #|doc
  Return all keys in the treemap in a vector.
  |#
  (define-who treemap-keys
    (case-lambda
      [(tm)
       (pcheck ([treemap? tm])
               (let ([lb (make-list-builder)])
                 (rbtree-visit who (lambda (k v) (lb k)) tm)
                 (list->vector (lb))))]
      [(tm collect)
       (pcheck ([treemap? tm] [procedure? collect])
               (rbtree-visit who (lambda (k v) (collect k)) tm))]))

  #|doc
  Return all values in the treemap in a vector,
  or the values are collected using a custom collector procedure.
  |#
  (define-who treemap-values
    (case-lambda
      [(tm)
       (pcheck ([treemap? tm])
               (let ([lb (make-list-builder)])
                 (rbtree-visit who (lambda (k v) (lb v)) tm)
                 (list->vector (lb))))]
      [(tm collect)
       (pcheck ([treemap? tm] [procedure? collect])
               (rbtree-visit who (lambda (k v) (collect v)) tm))]))


  #|doc
  Return all key-value pairs in the treemap in a vector,
  or the key-value pairs are collected using a custom collector procedure.

  Mutating the returned key-value pairs has no effect on the treemap.
  |#
  (define-who treemap-cells
    (case-lambda
      [(tm)
       (pcheck ([treemap? tm])
               (let ([lb (make-list-builder)])
                 (rbtree-visit who (lambda (k v) (lb (cons k v))) tm)
                 (list->vector (lb))))]
      [(tm collect)
       (pcheck ([treemap? tm] [procedure? collect])
               (rbtree-visit who collect tm))]))


  #|doc
  Return a pair consisting of a key and its value,
  where the key is the successor of `k` in the treemap `tm`.

  If the successor of `k` does not exist, #f is returned.
  |#
  (define-who treemap-successor
    (lambda (tm k)
      (pcheck ([treemap? tm])
              (rbtree-successor who tm k))))


  #|doc
  Return a pair consisting of a key and its value,
  where the key is the predecessor of `k` in the treemap `tm`.

  If the predecessor of `k` does not exist, #f is returned.
  |#
  (define-who treemap-predecessor
    (lambda (tm k)
      (pcheck ([treemap? tm])
              (rbtree-predecessor who tm k))))


  #|doc
  Return a pair consisting of the minimum (leftmost) key and its value in the treemap `tm`.

  An error is raised if the treemap is empty.
  If the treemap is empty, #f is returned.
  |#
  (define-who treemap-min
    (lambda (tm)
      (pcheck ([treemap? tm])
              (rbtree-min who tm))))


  #|doc
  Return a pair consisting of the maximum (rightmost) key and its value in the treemap `tm`.

  An error is raised if the treemap is empty.
  If the treemap is empty, #f is returned.
  |#
  (define-who treemap-max
    (lambda (tm)
      (pcheck ([treemap? tm])
              (rbtree-max who tm))))


  #|doc
  Apply `pred` to each pair of keys and values in the treemap `tm`,
  if the result is #t, the respective key and value are added to a new
  treemap. Then the new treemap is returned.
  |#
  (define-who treemap-filter
    (lambda (pred tm)
      (pcheck ([procedure? pred] [treemap? tm])
              (let ([newtm (make-treemap (rbtree-=? tm) (rbtree-<? tm))])
                (rbtree-visit who (lambda (k v) (when (pred k v) (rbtree-set! who newtm k v))) tm)
                newtm))))


  #|doc
  Apply `pred` to each pair of keys and values in the treemap `tm`,
  if the result is #f, the respective key and value are removed from the treemap.
  |#
  (define-who treemap-filter!
    (lambda (pred tm)
      (pcheck ([procedure? pred] [treemap? tm])
              (let ([lb (make-list-builder)])
                (rbtree-visit who (lambda (k v) (lb (cons k v))) tm)
                (for-each (lambda (kv)
                            (let ([k (car kv)])
                              (unless (pred k (cdr kv))
                                (rbtree-delete! who tm k))))
                          (lb))
                tm))))


  #|doc
  Apply `pred` to every pair of keys and values in `tm` and return two values,
  the first one a treemap of the keys/values of `tm` for which `(pred k v)` returns #t,
  the second one a treemap of the keys/values of `tm` for which `(pred k v)` returns #f.
  |#
  (define-who treemap-partition
    (lambda (pred tm)
      (pcheck ([procedure? pred] [treemap? tm])
              (let ([T (make-treemap (rbtree-=? tm) (rbtree-<? tm))]
                    [F (make-treemap (rbtree-=? tm) (rbtree-<? tm))])
                (rbtree-visit who (lambda (k v) (if (pred k v)
                                                    (rbtree-set! who T k v)
                                                    (rbtree-set! who F k v)))
                              tm)
                (values T F)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   iterations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define all-treemaps? (lambda (x*) (andmap treemap? x*)))
  (define check-size
    (case-lambda
      [(who x0 x1)
       (unless (fx= (treemap-size x0) (treemap-size x1))
         (errorf who "treemaps are not of the same size"))]
      [(who x0 . x*)
       (unless (null? x*)
         (unless (apply fx= (treemap-size x0) (map treemap-size x*))
           (errorf who "treemaps are not of the same size")))]))

  ;; maps return new treemaps.
  ;; =? and <? of the new treemap are taken from the first treemap argument.
  ;; Procs in maps return two values.
  ;; procs should take twice as many args (k + v) as #trees.
  ;; All do inorder traversal.


  (define-who treemap-map
    (case-lambda
      [(proc tm0)
       (pcheck ([procedure? proc] [treemap? tm0])
               (rbtree-map who proc (make-treemap (rbtree-=? tm0) (rbtree-<? tm0)) tm0))]
      [(proc tm0 tm1)
       (pcheck ([procedure? proc] [treemap? tm0 tm1])
               (check-size who tm0 tm1)
               (rbtree-map who proc (make-treemap (rbtree-=? tm0) (rbtree-<? tm0)) tm0 tm1))]
      [(proc tm0 . tm*)
       (pcheck ([procedure? proc] [treemap? tm0] [all-treemaps? tm*])
               (apply check-size who tm0 tm*)
               (apply rbtree-map who proc (make-treemap (rbtree-=? tm0) (rbtree-<? tm0)) tm0 tm*))]))


  (define-who treemap-map/i
    (case-lambda
      [(proc tm0)
       (pcheck ([procedure? proc] [treemap? tm0])
               (rbtree-map/i who proc (make-treemap (rbtree-=? tm0) (rbtree-<? tm0)) tm0))]
      [(proc tm0 tm1)
       (pcheck ([procedure? proc] [treemap? tm0 tm1])
               (check-size who tm0 tm1)
               (rbtree-map/i who proc (make-treemap (rbtree-=? tm0) (rbtree-<? tm0)) tm0 tm1))]
      [(proc tm0 . tm*)
       (pcheck ([procedure? proc] [treemap? tm0] [all-treemaps? tm*])
               (apply check-size who tm0 tm*)
               (apply rbtree-map/i who proc (make-treemap (rbtree-=? tm0) (rbtree-<? tm0)) tm0 tm*))]))


  ;; `proc` in in-place maps should return only one value
  (define-who treemap-map!
    (case-lambda
      [(proc tm0)
       (pcheck ([procedure? proc] [treemap? tm0])
               (rbtree-map! who proc tm0))]
      [(proc tm0 tm1)
       (pcheck ([procedure? proc] [treemap? tm0 tm1])
               (check-size who tm0 tm1)
               (rbtree-map! who proc tm0 tm1))]
      [(proc tm0 . tm*)
       (pcheck ([procedure? proc] [treemap? tm0] [all-treemaps? tm*])
               (apply check-size who tm0 tm*)
               (apply rbtree-map! who proc tm0 tm*))]))


  (define-who treemap-map/i!
    (case-lambda
      [(proc tm0)
       (pcheck ([procedure? proc] [treemap? tm0])
               (rbtree-map/i! who proc tm0))]
      [(proc tm0 tm1)
       (pcheck ([procedure? proc] [treemap? tm0 tm1])
               (check-size who tm0 tm1)
               (rbtree-map/i! who proc tm0 tm1))]
      [(proc tm0 . tm*)
       (pcheck ([procedure? proc] [treemap? tm0] [all-treemaps? tm*])
               (apply check-size who tm0 tm*)
               (apply rbtree-map/i! who proc tm0 tm*))]))


  (define-who treemap-for-each
    (case-lambda
      [(proc tm0)
       (pcheck ([procedure? proc] [treemap? tm0])
               (rbtree-for-each who proc tm0))]
      [(proc tm0 tm1)
       (pcheck ([procedure? proc] [treemap? tm0 tm1])
               (check-size who tm0 tm1)
               (rbtree-for-each who proc tm0 tm1))]
      [(proc tm0 . tm*)
       (pcheck ([procedure? proc] [treemap? tm0] [all-treemaps? tm*])
               (apply check-size who tm0 tm*)
               (apply rbtree-for-each who proc tm0 tm*))]))


  (define-who treemap-for-each/i
    (case-lambda
      [(proc tm0)
       (pcheck ([procedure? proc] [treemap? tm0])
               (rbtree-for-each/i who proc tm0))]
      [(proc tm0 tm1)
       (pcheck ([procedure? proc] [treemap? tm0 tm1])
               (check-size who tm0 tm1)
               (rbtree-for-each/i who proc tm0 tm1))]
      [(proc tm0 . tm*)
       (pcheck ([procedure? proc] [treemap? tm0] [all-treemaps? tm*])
               (apply check-size who tm0 tm*)
               (apply rbtree-for-each/i who proc tm0 tm*))]))


;;;; folds

  ;; The treemaps' <? procedure defines an ordering of the keys.
  ;; fold-left folds from the leftmost key-value as defined by <?,
  ;; fold-right folds from the rightmost one.

  (define-who treemap-fold-left
    (case-lambda
      [(proc acc tm0)
       (pcheck ([procedure? proc] [treemap? tm0])
               (rbtree-fold-left who proc acc tm0))]
      [(proc acc tm0 tm1)
       (pcheck ([procedure? proc] [treemap? tm0 tm1])
               (check-size who tm0 tm1)
               (rbtree-fold-left who proc acc tm0 tm1))]
      [(proc acc tm0 . tm*)
       (pcheck ([procedure? proc] [treemap? tm0] [all-treemaps? tm*])
               (apply check-size who tm0 tm*)
               (rbtree-fold-left who proc acc tm0 tm*))]))


  (define-who treemap-fold-left/i
    (case-lambda
      [(proc acc tm0)
       (pcheck ([procedure? proc] [treemap? tm0])
               (rbtree-fold-left/i who proc acc tm0))]
      [(proc acc tm0 tm1)
       (pcheck ([procedure? proc] [treemap? tm0 tm1])
               (check-size who tm0 tm1)
               (rbtree-fold-left/i who proc acc tm0 tm1))]
      [(proc acc tm0 . tm*)
       (pcheck ([procedure? proc] [treemap? tm0] [all-treemaps? tm*])
               (apply check-size who tm0 tm*)
               (rbtree-fold-left/i who proc acc tm0 tm*))]))


  (define-who treemap-fold-right
    (case-lambda
      [(proc acc tm0)
       (pcheck ([procedure? proc] [treemap? tm0])
               (rbtree-fold-right who proc acc tm0))]
      [(proc acc tm0 tm1)
       (pcheck ([procedure? proc] [treemap? tm0 tm1])
               (check-size who tm0 tm1)
               (rbtree-fold-right who proc acc tm0 tm1))]
      [(proc acc tm0 . tm*)
       (pcheck ([procedure? proc] [treemap? tm0] [all-treemaps? tm*])
               (apply check-size who tm0 tm*)
               (rbtree-fold-right who proc acc tm0 tm*))]))


  (define-who treemap-fold-right/i
    (case-lambda
      [(proc acc tm0)
       (pcheck ([procedure? proc] [treemap? tm0])
               (rbtree-fold-right/i who proc acc tm0))]
      [(proc acc tm0 tm1)
       (pcheck ([procedure? proc] [treemap? tm0 tm1])
               (check-size who tm0 tm1)
               (rbtree-fold-right/i who proc acc tm0 tm1))]
      [(proc acc tm0 . tm*)
       (pcheck ([procedure? proc] [treemap? tm0] [all-treemaps? tm*])
               (apply check-size who tm0 tm*)
               (rbtree-fold-right/i who proc acc tm0 tm*))]))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   conversions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  #|doc
  Convert a treemap to an association list, in in-order by default.

  `order` can be 'in, 'pre or 'post, so the items are collected in
  in-order, pre- and post-order, respectively.
  |#
  (define-who treemap->list
    (case-lambda
      [(tm)
       (treemap->list tm 'in)]
      [(tm order)
       (pcheck ([treemap? tm])
               (let ([lb (make-list-builder)])
                 (case order
                   [in   (rbtree-visit-inorder   who (lambda (k v) (lb (cons k v))) tm)]
                   [pre  (rbtree-visit-preorder  who (lambda (k v) (lb (cons k v))) tm)]
                   [post (rbtree-visit-postorder who (lambda (k v) (lb (cons k v))) tm)]
                   [else (errorf who "invalid traversal order: ~a, should be one of 'in, 'pre and 'post" order)])
                 (lb)))]))


  #|doc
  Convert a hashtable to a treemap.
  `=?` and `<?` are as in `treeemap`.
  |#
  (define-who hashtable->treemap
    (lambda (=? <? ht)
      (pcheck ([hashtable? ht] [procedure? =? <?])
              (let ([tm (make-treemap =? <?)])
                (vector-for-each (lambda (kv) (rbtree-set! who tm (car kv) (cdr kv)))
                                 (hashtable-cells ht))
                tm))))


  (record-writer (type-descriptor $treemap)
                 (lambda (r p wr)
                   (display "#[treemap (" p)
                   (if (treemap-empty? r)
                       (display ")]" p)
                       (begin
                         (let ([n (treemap-size r)] [i 0])
                           (rbtree-visit 'treemap-writer
                                         (lambda (k v)
                                           (if (fx= i (fx1- n))
                                               (wr (cons k v) p)
                                               (begin
                                                 (wr (cons k v) p)
                                                 (display " " p)))
                                           (set! i (fx1+ i)))
                                         r)
                           (display ")]" p))))))
  )
