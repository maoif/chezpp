(library (chezpp dlist)
  (export dlist make-dlist dlist? dlist-length dlist-empty?
          dlist-ref dlist-set! dlist-add! dlist-delete! dlist-clear!
          dlist-reverse dlist-reverse!
          dlist-filter dlist-filter! dlist-partition
          dlist-contains? dlist-contains/p? dlist-search dlist-search*
          dlist-append dlist-append!
          dlist-slice dlist-slice! dlist-copy dlist-copy!

          dlist-push! dlist-pop! dlist-push-back! dlist-pop-back!

          dlist-map dlist-map/i dlist-map! dlist-map/i!
          dlist-andmap dlist-ormap
          dlist-for-each dlist-for-each/i
          dlist-map-rev dlist-map/i-rev dlist-for-each-rev dlist-for-each/i-rev
          dlist-fold-left dlist-fold-left/i dlist-fold-right dlist-fold-right/i

          dlist-sorted? dlist-sort dlist-sort!
          dlist-iota dlist-nums

          dlist->list list->dlist
          dlist->vector vector->dlist)
  (import (chezpp chez)
          (chezpp internal)
          (chezpp utils)
          (chezpp list)
          (chezpp vector))


  (define-record-type dnode
    (nongenerative) (sealed #t) (opaque #f)
    ;; head node: left is null-dnode
    ;; tail node: right is null-dnode
    (fields (mutable value) (mutable left) (mutable right)))

  (define null-dnode (make-dnode #f #f #f))
  (define null-dnode? (lambda (x) (eq? x null-dnode)))

  (define-record-type ($dlist mk-dlist dlist?)
    (nongenerative) (sealed #t)
    ;; When empty, `first` and `last` is null-dnode.
    (fields (mutable first  dlist-first  dlist-first-set!)
            (mutable last   dlist-last   dlist-last-set!)
            (mutable length dlist-length dlist-length-set!)))

  (define all-dlists? (lambda (dl*) (andmap dlist? dl*)))


  #|doc
  Create a doubly-linked list (dlist) object.

  If no arguments are given, an empty dlist is created.
  If `len` is given, a dlist with `len` items all set to #f is returned.
  If both `len` and `v` are given, a dlist with `len` items all set to `v` is returned.
  |#
  (define-who make-dlist
    (case-lambda
      [() (mk-dlist null-dnode null-dnode 0)]
      [(len) (make-dlist len #f)]
      [(len v)
       (pcheck ([natural? len])
               (let ([dl (mk-dlist null-dnode null-dnode 0)])
                 (let loop ([i len])
                   (if (fx= i 0)
                       dl
                       (begin (dlist-add! dl v)
                              (loop (sub1 i)))))))]))


  #|doc
  Create a dlist from the given arguments.
  |#
  (define-who dlist
    (lambda args
      (let ([dl (make-dlist)])
        (for-each (lambda (x) (dlist-add! dl x)) args)
        dl)))


  #|doc
  Return whether the dlist is empty.
  |#
  (define-who dlist-empty?
    (lambda (dl)
      (pcheck ([dlist? dl])
              (fx= 0 (dlist-length dl)))))


  #|doc
  Return the value at the specified index in the dlist.

  TODO default value?
  |#
  (define-who dlist-ref
    (lambda (dl i)
      (pcheck ([dlist? dl] [natural? i])
              (let ([len (dlist-length dl)])
                (if (fx>= i len)
                    (errorf who "index ~a out of range ~a" i len)
                    (cond [(fx= i 0) (dnode-value (dlist-first dl))]
                          [(fx= i (fx- len 1)) (dnode-value (dlist-last dl))]
                          [else (let next ([i i] [n (dlist-first dl)])
                                  ;; TODO optimize: proceed from left/right based on i and length
                                  (if (fx= i 0)
                                      (dnode-value n)
                                      (next (fx- i 1) (dnode-right n))))]))))))


  #|doc
  Update the item at the specified index in the dlist.
  |#
  (define-who dlist-set!
    (lambda (dl i v)
      (pcheck ([dlist? dl] [natural? i])
              (let ([len (dlist-length dl)])
                (if (fx< i len)
                    (let next ([i i] [n (dlist-first dl)])
                      ;; TODO optimize: proceed from left/right based on i and length
                      (if (fx= i 0)
                          (dnode-value-set! n v)
                          (next (fx- i 1) (dnode-right n))))
                    (errorf who "index ~a out of range ~a" i len))))))


  (define $dlist-add0
    (lambda (dl v)
      (let ([n (make-dnode v null-dnode null-dnode)])
        (dlist-first-set!  dl n)
        (dlist-last-set!   dl n)
        (dlist-length-set! dl 1))))


  #|doc
  Add an item to the dlist.
  |#
  (define-who dlist-add!
    (case-lambda
      [(dl v)
       (pcheck ([dlist? dl])
               (let ([len (dlist-length dl)])
                 ;; add to tail
                 (cond [(fx= len 0)
                        ($dlist-add0 dl v)]
                       [(fx= len 1)
                        (let* ([n1 (dlist-first dl)]
                               [n2 (make-dnode v n1 null-dnode)])
                          (dnode-right-set!  n1 n2)
                          (dlist-last-set!   dl n2)
                          (dlist-length-set! dl 2))]
                       [else (let* ([last (dlist-last dl)]
                                    [n (make-dnode v last null-dnode)])
                               (dnode-right-set!  last n)
                               (dlist-last-set!   dl   n)
                               (dlist-length-set! dl (fx+ len 1)))])))]
      [(dl i v)
       (pcheck ([dlist? dl] [natural? i])
               (let ([len (dlist-length dl)])
                 (cond
                  [(fx= i 0)
                   (if (fx= len 0)
                       ($dlist-add0 dl v)
                       ;; add to head
                       (let* ([n0 (dlist-first dl)]
                              [n (make-dnode v null-dnode n0)])
                         (dnode-left-set!   n0 n)
                         (dlist-first-set!  dl n)
                         (dlist-length-set! dl (fx+ (dlist-length dl) 1))))]
                  [(fx= i len) (dlist-add! dl v)]
                  ;; TODO optimize: proceed from left/right based on i and length
                  [(fx< i len)
                   (let next ([i i] [n (dlist-first dl)])
                     (if (fx= i 0)
                         (let* ([L (dnode-left n)]
                                [N (make-dnode v L n)])
                           (dnode-right-set! L N)
                           (dlist-length-set! dl (fx+ (dlist-length dl) 1)))
                         (next (fx1- i) (dnode-right n))))]
                  [else (errorf who "index ~a out of range ~a" i len)])))]))


  #|doc
  Remove an item at the specified index in the dlist.
  |#
  (define-who dlist-delete!
    (lambda (dl i)
      (pcheck ([dlist? dl] [natural? i])
              (let ([len (dlist-length dl)])
                (if (fx>= i len)
                    (errorf who "index ~a out of range ~a" i len)
                    (if (fx> len 1)
                        (cond [(fx= i 0)
                               (let* ([n (dlist-first dl)]
                                      [R (dnode-right n)])
                                 (dlist-first-set!  dl R)
                                 (dlist-length-set! dl (fx1- len)))]
                              [(fx= i (fx- len 1))
                               (let* ([n (dlist-last dl)]
                                      [L (dnode-left n)])
                                 (dnode-right-set!  L null-dnode)
                                 (dlist-last-set!   dl L)
                                 (dlist-length-set! dl (fx1- len)))]
                              [else (let next ([i i] [n (dlist-first dl)])
                                      (if (fx= i 0)
                                          (let* ([L (dnode-left  n)]
                                                 [R (dnode-right n)])
                                            (dnode-right-set! L R)
                                            (dnode-left-set!  R L)
                                            (dlist-length-set! dl (fx1- len)))
                                          (next (fx- i 1) (dnode-right n))))])
                        (begin (assert (and (fx= len 1) (fx= i 0)))
                               (dlist-first-set!  dl null-dnode)
                               (dlist-last-set!   dl null-dnode)
                               (dlist-length-set! dl 0))))))))



  #|doc
  Remove all items in the dlist.
  |#
  (define-who dlist-clear!
    (case-lambda
      [(dl) (pcheck ([dlist? dl])
                    (dlist-first-set!  dl null-dnode)
                    (dlist-last-set!   dl null-dnode)
                    (dlist-length-set! dl 0))]))


  #|doc
  Return a newly allocated dlist consisting of the items of `dl` in reverse order.
  |#
  (define-who dlist-reverse
    (lambda (dl)
      (pcheck ([dlist? dl])
              (let ([len (dlist-length dl)] [newdl (make-dlist)])
                (cond [(fx= 0 len) newdl]
                      [(fx= 1 len)
                       (dlist-add! newdl (dnode-value (dlist-first dl)))
                       newdl]
                      ;; TODO maybe build dnodes first
                      [else (let loop ([n (dlist-last dl)])
                              (if (null-dnode? n)
                                  newdl
                                  (begin (dlist-add! newdl (dnode-value n))
                                         (loop (dnode-left n)))))])))))


  #|doc
  Reverse the items in the dlist in place.
  |#
  (define-who dlist-reverse!
    (lambda (dl)
      (pcheck ([dlist? dl])
              (when (fx> (dlist-length dl) 1)
                (let ([first (dlist-first dl)] [last (dlist-last dl)])
                  (let loop ([n first] [prev (dnode-left first)])
                    (let ([R (dnode-right n)])
                      (if (eq? n last)
                          (begin (dnode-right-set! n prev)
                                 (dnode-left-set!  n R)
                                 (dlist-first-set! dl last)
                                 (dlist-last-set!  dl first))
                          (begin (dnode-right-set! n prev)
                                 (dnode-left-set!  n R)
                                 (loop R n))))))))))


  #|doc
  Return two dlists, the first dlist contains values `x` such that `(proc x)` returns #t,
  the second contains values `x` such that `(proc x)` returns #f.
  |#
  (define-who dlist-partition
    (lambda (proc dl)
      (pcheck ([procedure? proc] [dlist? dl])
              (let ([T (make-dlist)] [F (make-dlist)])
                (let loop ([n (dlist-first dl)])
                  (if (null-dnode? n)
                      (values T F)
                      (let ([v (dnode-value n)])
                        (if (proc v)
                            (dlist-add! T v)
                            (dlist-add! F v))
                        (loop (dnode-right n)))))))))


  #|doc
  Return a new dlist whose items are those from the given dlists, in the given order.
  |#
  (define-who dlist-append
    (lambda (dl . dl*)
      (pcheck ([dlist? dl] [all-dlists? dl*])
              (let ([newdl (make-dlist)])
                (let next ([dl* (cons dl dl*)])
                  (if (null? dl*)
                      newdl
                      (let ([dl (car dl*)])
                        (let loop ([n (dlist-first dl)])
                          (if (null-dnode? n)
                              (next (cdr dl*))
                              (begin (dlist-add! newdl (dnode-value n))
                                     (loop (dnode-right n))))))))))))


  #|doc
  Append given dlists to dlist `dl`.
  After this operation, operations on the dlists can be observed in `dl`.
  |#
  (define-who dlist-append!
    (lambda (dl . dl*)
      (pcheck ([dlist? dl] [all-dlists? dl*])
              (let loop ([dl* dl*] [last-node (dlist-last dl)])
                (unless (null? dl*)
                  (let* ([dl (car dl*)] [firstn (dlist-first dl)])
                    (if (null-dnode? firstn)
                        (loop (cdr dl*) last-node)
                        (begin (dnode-right-set! last-node firstn)
                               (dnode-left-set!  firstn last-node)
                               (loop (cdr dl*) (dlist-last dl))))))))))


  #|doc
  Return whether the dlist contains the given item.
  If it does, the procedure returns the index of the given item;
  otheriwse it returns #f.
  Items are compared using `equal?`.
  |#
  (define-who dlist-contains?
    (lambda (dl v)
      (pcheck ([dlist? dl])
              (if (fx= 0 (dlist-length dl))
                  #f
                  (let loop ([i 0] [n (dlist-first dl)])
                    (if (null-dnode? n)
                        #f
                        (if (equal? v (dnode-value n))
                            i
                            (loop (fx1+ i) (dnode-right n)))))))))


  #|doc
  Return whether the dlist contains an item that satisfies the predicate `=?`.
  If it does, the procedure returns the index of the given item;
  otheriwse it returns #f.
  |#
  (define-who dlist-contains/p?
    (lambda (dl =?)
      (pcheck ([dlist? dl] [procedure? =?])
              (if (fx= 0 (dlist-length dl))
                  #f
                  (let loop ([i 0] [n (dlist-first dl)])
                    (if (null-dnode? n)
                        #f
                        (if (=? (dnode-value n))
                            i
                            (loop (fx1+ i) (dnode-right n)))))))))


  #|doc
  Apply `pred` to every item of dlist `dl` and return a new dlist
  of the items of `dl` for which `pred` returns #t.
  |#
  (define-who dlist-filter
    (lambda (pred dl)
      (pcheck ([dlist? dl] [procedure? pred])
              (let ([newdl (make-dlist)])
                (let loop ([n (dlist-first dl)])
                  (if (null-dnode? n)
                      newdl
                      (let ([v (dnode-value n)])
                        (when (pred v)
                          (dlist-add! newdl v))
                        (loop (dnode-right n)))))))))


  #|doc
  Similar to `dlist-filter`, but dist `dl` is modified in place to contain
  only items `x` such that `(pred x)` returns #t.
  |#
  (define-who dlist-filter!
    (lambda (pred dl)
      (pcheck ([dlist? dl] [procedure? pred])
              (let ([link! (lambda (L R)
                             (dnode-right-set! L R)
                             (dnode-left-set!  R L))]
                    [first #f])
                ;; link the nodes first, then update
                (let loop ([n (dlist-first dl)] [prev #f])
                  (if (null-dnode? n)
                      (begin (dlist-first-set! dl first)
                             (dlist-last-set!  dl prev)
                             (dnode-left-set!  first null-dnode)
                             (dnode-right-set! prev  null-dnode))
                      (if (pred (dnode-value n))
                          (begin
                            (unless first (set! first n))
                            (when prev (link! prev n))
                            (loop (dnode-right n) n))
                          (loop (dnode-right n) prev))))))))


  #|doc
  Return the first item in the dlist that satisfies the predicate `pred`.
  If no such item is found, #f is returned.
  |#
  (define-who dlist-search
    (lambda (dl pred)
      (pcheck ([dlist? dl] [procedure? pred])
              (let loop ([n (dlist-first dl)])
                (if (null-dnode? n)
                    #f
                    (let ([v (dnode-value n)])
                      (if (pred v)
                          v
                          (loop (dnode-right n)))))))))


  #|doc
  Search for items in dlist `dl` that satisfies the predicate `pred`.

  By default the items satisfying `pred` are returned in a list.

  If `collect` is given, it is applied to every item that satisfies `pred`
  in the dlist. This is useful when collecting the satisfying items in custom
  data structures.
  |#
  (define-who dlist-search*
    (case-lambda
      [(dl pred)
       (pcheck ([dlist? dl] [procedure? pred])
               (let ([lb (make-list-builder)])
                 (dlist-search* dl pred (lambda (x) (lb x)))
                 (lb)))]
      [(dl pred collect)
       (pcheck ([dlist? dl] [procedure? pred collect])
               (let loop ([n (dlist-first dl)])
                 (unless (null-dnode? n)
                   (let ([v (dnode-value n)])
                     (when (pred v) (collect v))
                     (loop (dnode-right n))))))]))


  #|doc
  Return a slice (sub-dlist) of the dlist `dl` specified by `start`, `end` and `step`.

  Meanings of `start`, `end` and `step` are the same as in list:slice.

  If the indices are out of range in any way, an empty dlist is returned.
  |#
  (define-who dlist-slice
    (case-lambda
      [(dl end) (dlist-slice dl 0 end 1)]
      [(dl start end) (dlist-slice dl start end 1)]
      [(dl start end step)
       (pcheck ([dlist? dl] [fixnum? start end step])
               (when (fx= step 0) (errorf who "step cannot be 0"))
               (let* ([len (dlist-length dl)] [newdl (make-dlist)]
                      [s (let ([s (if (fx>= start 0) start (fx+ len start))])
                           (cond [(fx< s 0) 0]
                                 [(fx> s len) (fx1- len)]
                                 [else s]))]
                      [e (let ([e (if (fx>= end 0) end (fx+ len end))])
                           (cond [(fx<= e -1) -1]
                                 [(fx>= e len) len]
                                 [else e]))])
                 (if (fx= len 0)
                     newdl
                     (cond [(and (fx< s e) (fx> step 0))
                            ;; forward
                            ;; get to `s`
                            (let f ([i 0] [n (dlist-first dl)])
                              (if (fx= i s)
                                  ;; collect items
                                  (let loop ([i s] [n n])
                                    (dlist-add! newdl (dnode-value n))
                                    ;; get to the next item
                                    (let next ([i i] [n n] [k step])
                                      (cond [(fx>= i e) newdl]
                                            [(fx= k 0)  (loop i n)]
                                            [else       (next (fx1+ i) (dnode-right n) (fx1- k))])))
                                  (f (fx1+ i) (dnode-right n))))]
                           [(and (fx> s e) (fx< step 0))
                            ;; backward
                            (let f ([i 0] [n (dlist-first dl)])
                              (if (fx= i s)
                                  (let loop ([i s] [n n])
                                    (dlist-add! newdl (dnode-value n))
                                    (let next ([i i] [n n] [k step])
                                      (cond [(fx<= i e) newdl]
                                            [(fx= k 0)  (loop i n)]
                                            [else       (next (fx1- i) (dnode-left n) (fx1+ k))])))
                                  (f (fx1+ i) (dnode-right n))))]
                           [else newdl]))))]))


  #|doc
  Imperatively slice the dlist `dl` to the range specified by `start`, `end` and `step`.

  Meanings of `start`, `end` and `step` are the same as in list:slice.

  If the indices are out of range in any way, this procedure has no effect on the dlist.

  After the operation, `dl` is returned.
  |#
  (define-who dlist-slice!
    (case-lambda
      [(dl end) (dlist-slice! dl 0 end 1)]
      [(dl start end) (dlist-slice! dl start end 1)]
      [(dl start end step)
       (pcheck ([dlist? dl] [fixnum? start end step])
               (when (fx= step 0) (errorf who "step cannot be 0"))
               ;; create linked notes first, then update `dl`
               (let* ([len (dlist-length dl)]
                      [s (let ([s (if (fx>= start 0) start (fx+ len start))])
                           (cond [(fx< s 0) 0]
                                 [(fx> s len) (fx1- len)]
                                 [else s]))]
                      [e (let ([e (if (fx>= end 0) end (fx+ len end))])
                           (cond [(fx<= e -1) -1]
                                 [(fx>= e len) len]
                                 [else e]))]
                      [link! (lambda (L R)
                               (dnode-right-set! L R)
                               (dnode-left-set!  R L))]
                      [nlen (ceiling (/ (fx- e s) step))])
                 ;;(printf "~a: s: ~a, e: ~a, step: ~a~n" who s e step)
                 (when (fx> len 0)
                   (cond [(and (fx< s e) (fx> step 0))
                          ;; forward
                          ;; get to `s`
                          (let f ([i 0] [n (dlist-first dl)])
                            (if (fx= i s)
                                ;; collect items
                                (let loop ([i s] [prev n])
                                  ;; get to the next item
                                  (let next ([i i] [n1 prev] [k step])
                                    (cond [(fx>= i e)
                                           (dnode-left-set!   n    null-dnode)
                                           (dnode-right-set!  prev null-dnode)
                                           (dlist-first-set!  dl n)
                                           (dlist-last-set!   dl prev)
                                           (dlist-length-set! dl nlen)]
                                          [(fx= k 0)
                                           (link! prev n1)
                                           (loop i n1)]
                                          [else (next (fx1+ i) (dnode-right n1) (fx1- k))])))
                                (f (fx1+ i) (dnode-right n))))]
                         [(and (fx> s e) (fx< step 0))
                          ;; backward
                          (let f ([i 0] [n (dlist-first dl)])
                            (if (fx= i s)
                                (let loop ([i s] [curr n] [prev n] [sp step])
                                  (let next ([i i] [n1 curr] [k sp])
                                    (cond [(fx<= i e)
                                           (dnode-left-set!   n    null-dnode)
                                           (dnode-right-set!  prev null-dnode)
                                           (dlist-first-set!  dl n)
                                           (dlist-last-set!   dl prev)
                                           (dlist-length-set! dl nlen)]
                                          [(fx= k 0)
                                           ;; record L since after linking, n1's left is updated
                                           (let ([L (dnode-left n1)])
                                             (link! prev n1)
                                             ;; continue from L, hence (fx1+ step)
                                             (loop (fx1- i) L n1 (fx1+ step)))]
                                          [else (next (fx1- i) (dnode-left n1) (fx1+ k))])))
                                (f (fx1+ i) (dnode-right n))))]))
                 dl))]))


  #|doc
  Make a copy of the dlist `dl`.
  |#
  (define-who dlist-copy
    (lambda (dl)
      (pcheck ([dlist? dl])
              (let ([newdl (make-dlist)])
                (let loop ([n (dlist-first dl)])
                  (if (null-dnode? n)
                      newdl
                      (begin (dlist-add! newdl (dnode-value n))
                             (loop (dnode-right n)))))))))


  (define find-node
    (lambda (dl i)
      (let ([len (dlist-length dl)])
        ;; TODO check corner
        (if (< i (ash len -1))
            ;; left-to-right
            (let loop ([j 0] [n (dlist-first dl)])
              (if (= i j)
                  n
                  (loop (add1 j) (dnode-right n))))
            ;; right-to-left
            (let loop ([j (sub1 len)] [n (dlist-last dl)])
              (if (= i j)
                  n
                  (loop (sub1 j) (dnode-left n))))))))


  #|doc
  Copy items in `src` from indices src-start, ..., src-start + k - 1
  to consecutive indices in `tgt` starting at `tgt-start`.

  `src` and `tgt` must be dlists.
  `src-start`, `tgt-start`, and `k` must be exact nonnegative integers.
  The sum of `src-start` and `k` must not exceed the length of `src`,
  and the sum of `tgt-start` and `k` must not exceed the length of `tgt`.

  `src` and `tgt` may or may not be the same dlist.
  |#
  (define-who dlist-copy!
    (lambda (src src-start tgt tgt-start k)
      (pcheck ([dlist? src tgt] [natural? src-start tgt-start k])
              (let ([len1 (dlist-length src)]
                    [len2 (dlist-length tgt)])
                (when (> (fx+ src-start k) len1)
                  (errorf who "range ~a is too large in source dlist" k))
                (when (> (fx+ tgt-start k) len2)
                  (errorf who "range ~a is too large in target dlist" k))
                (if (eq? src tgt)
                    (let ([src-end (fx+ src-start k)] [tgt-end (fx+ tgt-start k)])
                      (cond
                       [(or
                         ;; disjoint, left to right
                         (fx<= src-end tgt-start)
                         ;; disjoint, right to left
                         (fx<= tgt-end src-start)
                         ;; overlapping, right to left
                         (fx<= tgt-start src-start))
                        (let loop ([k k] [i (find-node src src-start)] [j (find-node tgt tgt-start)])
                          (unless (fx= k 0)
                            (dnode-value-set! j (dnode-value i))
                            (loop (fx1- k) (dnode-right i) (dnode-right j))))]
                       [(fx< src-start tgt-start)
                        ;; overlapping, left to right, copy from last to first
                        (let loop ([k k] [i (find-node src (fx1- src-end))] [j (find-node tgt (fx1- tgt-end))])
                          (unless (fx= k 0)
                            (dnode-value-set! j (dnode-value i))
                            (loop (fx1- k) (dnode-left i) (dnode-left j))))]
                       [else (assert-unreachable)]))
                    (let loop ([k k] [i (find-node src src-start)] [j (find-node tgt tgt-start)])
                      (unless (fx= k 0)
                        (dnode-value-set! j (dnode-value i))
                        (loop (fx1- k) (dnode-right i) (dnode-right j)))))))))


  #|doc
  Check whether the given dlist is sorted according to comparison procedure `<?`.
  If `stop` is given, only the items with indices [0, stop) are checked;
  If both `start` and `stop` are given, only the items with indices [start, stop) are checked.

  `start` and `stop` must satisfy the requirement that `0 <= start <= stop <= length of dl`.
  |#
  (define-who dlist-sorted?
    (case-lambda
      [(<? dl)
       (pcheck ([dlist? dl])
               (dlist-sorted? <? dl 0 (dlist-length dl)))]
      [(<? dl stop)
       (pcheck ([dlist? dl])
               (dlist-sorted? <? dl 0 stop))]
      [(<? dl start stop)
       (pcheck ([procedure? <?] [dlist? dl] [natural? start stop])
               (let ([len (dlist-length dl)])
                 (when (fx> stop len)
                   (errorf who "stop index ~a out of bound ~a" stop len))
                 (when (fx> start stop)
                   (errorf who "start index ~a greater than stop index ~a" start stop))
                 (if (fx<= len 1)
                     #t
                     (let loop ([i start] [curr (find-node dl start)])
                       (if (fx= i (fx1- stop))
                           #t
                           (let ([next (dnode-right curr)])
                             (if (null-dnode? next)
                                 #t
                                 (and (<? (dnode-value curr) (dnode-value next))
                                      (loop (fx1+ i) next)))))))))]))

  (define $dlist->vector
    (lambda (dl start stop)
      (let ([vec (make-vector (- stop start))])
        (let loop ([i 0] [j start] [n (find-node dl start)])
          (if (fx= j stop)
              vec
              (begin (vector-set! vec i (dnode-value n))
                     (loop (fx1+ i) (fx1+ j) (dnode-right n))))))))


  #|doc
  The `dlist-sort` procedure uses the binary comparison procedure `<?` to sort the dlist `dl`.
  If only two arguments are given, the entire dlist is sorted;
  If the `stop` argument is given, the range from 0 to `stop-1` in `dl` is sorted;
  If both `start` and `stop` are given, the range from `start` to `stop-1` in `dl` is sorted.

  `start` and `stop` must satisfy the requirement that `0 <= start <= stop <= length of dl`.

  The `dlist-sort` procedure returns the sorted dlist or the subdlist.
  |#
  (define-who dlist-sort
    (case-lambda
      [(<? dl)
       (dlist-sort <? dl 0 (dlist-length dl))]
      [(<? dl stop)
       (dlist-sort <? dl 0 stop)]
      [(<? dl start stop)
       (pcheck ([dlist? dl] [procedure? <?] [natural? start stop])
               (let ([len (dlist-length dl)])
                 (when (fx> stop len)
                   (errorf who "stop index ~a out of bound ~a" stop len))
                 (when (fx> start stop)
                   (errorf who "start index ~a greater than stop index ~a" start stop))
                 ;; TODO optimize
                 (cond
                  [(fx= start stop)        (dlist)]
                  [(fx= (fx1+ start) stop) (dlist-ref dl start)]
                  [else (vector->dlist (vsort <? ($dlist->vector dl start stop)))])))]))


  #|doc
  The `dlist-sort!` procedure uses the binary comparison procedure `<?` to sort the dlist `dl`, in place.
  If only two arguments are given, the entire dlist is sorted;
  If the `stop` argument is given, the range from 0 to `stop-1` in `dl` is sorted;
  If both `start` and `stop` are given, the range from `start` to `stop-1` in `dl` is sorted.

  `start` and `stop` must satisfy the requirement that `0 <= start <= stop <= length of dl`.
  |#
  (define-who dlist-sort!
    (case-lambda
      [(<? dl)
       (dlist-sort! <? dl 0 (dlist-length dl))]
      [(<? dl stop)
       (dlist-sort! <? dl 0 stop)]
      [(<? dl start stop)
       (pcheck ([dlist? dl] [procedure? <?] [natural? start stop])
               (let ([len (dlist-length dl)])
                 (when (fx> stop len)
                   (errorf who "stop index ~a out of bound ~a" stop len))
                 (when (fx> start stop)
                   (errorf who "start index ~a greater than stop index ~a" start stop))
                 (when (not (or (fx= start stop) (fx= (fx1+ start) stop)))
                   ;; TODO optimize
                   (let ([vec (vsort <? ($dlist->vector dl start stop))])
                     (let loop ([i 0] [j start] [n (find-node dl start)])
                       (when (fx< j stop)
                         (dnode-value-set! n (vector-ref vec i))
                         (loop (fx1+ i) (fx1+ j) (dnode-right n))))))))]))


  #|doc
  `n` must be a natural number.
  This procedure creates a dlist that contains numbers ranging from 0 to n-1, inclusive.
  This is similar to `iota` for lists.
  |#
  (define-who dlist-iota
    (lambda (n)
      (pcheck ([natural? n])
              (let ([dl (make-dlist)])
                (let loop ([i 0])
                  (if (= i n)
                      dl
                      (begin (dlist-add! dl i)
                             (loop (add1 i)))))))))


  #|doc
  Generate a dlist of of numbers: start, start+step*1, start+step*2, ...

  `start`, `stop` and `step` must be numbers that meet the following requirements:
  If `start` is less than `stop`, then `step` must be greater than 0,
  in which case the sequence terminates when the value is greater than or equal to `stop`;
  If `start` is greater than `stop`, then `step` must be less than 0,
  in which case the sequence terminates when the value is less than or equal to `stop`.
  |#
  (define-who dlist-nums
    (case-lambda
      [(stop) (dlist-nums 0 stop 1)]
      [(start stop) (dlist-nums start stop 1)]
      [(start stop step)
       (pcheck ([number? start stop step])
               (if (or (and (<= start stop) (> step 0))
                       (and (>= start stop) (< step 0)))
                   (let ([len (exact (ceiling (/ (- stop start) step)))]
                         [dl (dlist)])
                     (let loop ([i 0] [n start])
                       (if (fx= i len)
                           dl
                           (begin (dlist-add! dl n)
                                  (loop (fx1+ i) (+ n step))))))
                   (errorf who "invalid range: ~a, ~a, ~a" start stop step)))]))



;;;; stack ops


  #|doc
  Add the item `v` to the front of the dlist `dl`.
  |#
  (define-who dlist-push!
    (lambda (dl v)
      (pcheck ([dlist? dl]) (dlist-add! dl 0 v))))


  #|doc
  Remove the first item from the dlist `dl` and return it.
  It is an error if the dlist is empty.
  |#
  (define-who dlist-pop!
    (lambda (dl)
      (pcheck ([dlist? dl])
              (if (fx= 0 (dlist-length dl))
                  (errorf who "dlist is empty")
                  (let ([v (dlist-ref dl 0)])
                    (dlist-delete! dl 0)
                    v)))))


  #|doc
  Add the item `v` to the back of the dlist `dl`.
  |#
  (define-who dlist-push-back!
    (lambda (dl v) (pcheck ([dlist? dl]) (dlist-add! dl v))))


  #|doc
  Remove the last item from the dlist `dl` and return it.
  It is an error if the dlist is empty.
  |#
  (define-who dlist-pop-back!
    (lambda (dl)
      (pcheck ([dlist? dl])
              (let* ([len (dlist-length dl)] [i (fx1- len)])
                (if (fx= 0 len)
                    (errorf who "dlist is empty")
                    (let ([v (dlist-ref dl i)])
                      (dlist-delete! dl i)
                      v))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   iterations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; TODO iter API

  (define check-length
    (case-lambda
      [(who dl0 dl1)
       (unless (fx= (dlist-length dl0) (dlist-length dl1))
         (errorf who "dlists are not of the same length"))]
      [(who dl0 . dl*)
       (unless (null? dl*)
         (unless (apply fx= (dlist-length dl0) (map dlist-length dl*))
           (errorf who "dlists are not of the same length")))]))

  (define-who dlist-map
    (case-lambda
      [(proc dl0)
       (pcheck ([procedure? proc] [dlist? dl0])
               (let ([newdl (make-dlist)])
                 (let loop ([n0 (dlist-first dl0)])
                   (if (null-dnode? n0)
                       newdl
                       (begin (dlist-add! newdl (proc (dnode-value n0)))
                              (loop (dnode-right n0)))))))]
      [(proc dl0 dl1)
       (pcheck ([procedure? proc] [dlist? dl0] [dlist? dl1])
               (check-length who dl0 dl1)
               (let ([newdl (make-dlist)])
                 (let loop ([n0 (dlist-first dl0)] [n1 (dlist-first dl1)])
                   (if (null-dnode? n0)
                       newdl
                       (begin (dlist-add! newdl (proc (dnode-value n0) (dnode-value n1)))
                              (loop (dnode-right n0) (dnode-right n1)))))))]
      [(proc dl0 . dl*)
       (pcheck ([procedure? proc] [dlist? dl0] [all-dlists? dl*])
               (apply check-length who dl0 dl*)
               (let ([newdl (make-dlist)])
                 (let loop ([n0 (dlist-first dl0)] [n* (map dlist-first dl*)])
                   (if (null-dnode? n0)
                       newdl
                       (begin (dlist-add! newdl (apply proc (dnode-value n0) (map dnode-value n*)))
                              (loop (dnode-right n0) (map dnode-right n*)))))))]))


  (define-who dlist-map/i
    (case-lambda
      [(proc dl0)
       (pcheck ([procedure? proc] [dlist? dl0])
               (let ([newdl (make-dlist)])
                 (let loop ([i 0] [n0 (dlist-first dl0)])
                   (if (null-dnode? n0)
                       newdl
                       (begin (dlist-add! newdl (proc i (dnode-value n0)))
                              (loop (fx1+ i) (dnode-right n0)))))))]
      [(proc dl0 dl1)
       (pcheck ([procedure? proc] [dlist? dl0] [dlist? dl1])
               (check-length who dl0 dl1)
               (let ([newdl (make-dlist)])
                 (let loop ([i 0] [n0 (dlist-first dl0)] [n1 (dlist-first dl1)])
                   (if (null-dnode? n0)
                       newdl
                       (begin (dlist-add! newdl (proc i (dnode-value n0) (dnode-value n1)))
                              (loop (fx1+ i) (dnode-right n0) (dnode-right n1)))))))]
      [(proc dl0 . dl*)
       (pcheck ([procedure? proc] [dlist? dl0] [all-dlists? dl*])
               (apply check-length who dl0 dl*)
               (let ([newdl (make-dlist)])
                 (let loop ([i 0] [n0 (dlist-first dl0)] [n* (map dlist-first dl*)])
                   (if (null-dnode? n0)
                       newdl
                       (begin (dlist-add! newdl (apply proc i (dnode-value n0) (map dnode-value n*)))
                              (loop (fx1+ i) (dnode-right n0) (map dnode-right n*)))))))]))


  (define-who dlist-map!
    (case-lambda
      [(proc dl0)
       (pcheck ([procedure? proc] [dlist? dl0])
               (let loop ([n0 (dlist-first dl0)])
                 (if (null-dnode? n0)
                     dl0
                     (begin (dnode-value-set! n0 (proc (dnode-value n0)))
                            (loop (dnode-right n0))))))]
      [(proc dl0 dl1)
       (pcheck ([procedure? proc] [dlist? dl0] [dlist? dl1])
               (check-length who dl0 dl1)
               (let loop ([n0 (dlist-first dl0)] [n1 (dlist-first dl1)])
                 (if (null-dnode? n0)
                     dl0
                     (begin (dnode-value-set! n0 (proc (dnode-value n0) (dnode-value n1)))
                            (loop (dnode-right n0) (dnode-right n1))))))]
      [(proc dl0 . dl*)
       (pcheck ([procedure? proc] [dlist? dl0] [all-dlists? dl*])
               (apply check-length who dl0 dl*)
               (let loop ([n0 (dlist-first dl0)] [n* (map dlist-first dl*)])
                 (if (null-dnode? n0)
                     dl0
                     (begin (dnode-value-set! n0 (apply proc (dnode-value n0) (map dnode-value n*)))
                            (loop (dnode-right n0) (map dnode-right n*))))))]))


  (define-who dlist-map/i!
    (case-lambda
      [(proc dl0)
       (pcheck ([procedure? proc] [dlist? dl0])
               (let loop ([i 0] [n0 (dlist-first dl0)])
                 (if (null-dnode? n0)
                     dl0
                     (begin (dnode-value-set! n0 (proc i (dnode-value n0)))
                            (loop (fx1+ i) (dnode-right n0))))))]
      [(proc dl0 dl1)
       (pcheck ([procedure? proc] [dlist? dl0] [dlist? dl1])
               (check-length who dl0 dl1)
               (let loop ([i 0] [n0 (dlist-first dl0)] [n1 (dlist-first dl1)])
                 (if (null-dnode? n0)
                     dl0
                     (begin (dnode-value-set! n0 (proc i (dnode-value n0) (dnode-value n1)))
                            (loop (fx1+ i) (dnode-right n0) (dnode-right n1))))))]
      [(proc dl0 . dl*)
       (pcheck ([procedure? proc] [dlist? dl0] [all-dlists? dl*])
               (apply check-length who dl0 dl*)
               (let loop ([i 0] [n0 (dlist-first dl0)] [n* (map dlist-first dl*)])
                 (if (null-dnode? n0)
                     dl0
                     (begin (dnode-value-set! n0 (apply proc i (dnode-value n0) (map dnode-value n*)))
                            (loop (fx1+ i) (dnode-right n0) (map dnode-right n*))))))]))


  (define-who dlist-for-each
    (case-lambda
      [(proc dl0)
       (pcheck ([procedure? proc] [dlist? dl0])
               (let loop ([n0 (dlist-first dl0)])
                 (unless (null-dnode? n0)
                   (begin (proc (dnode-value n0))
                          (loop (dnode-right n0))))))]
      [(proc dl0 dl1)
       (pcheck ([procedure? proc] [dlist? dl0] [dlist? dl1])
               (check-length who dl0 dl1)
               (let loop ([n0 (dlist-first dl0)] [n1 (dlist-first dl1)])
                 (unless (null-dnode? n0)
                   (begin (proc (dnode-value n0) (dnode-value n1))
                          (loop (dnode-right n0) (dnode-right n1))))))]
      [(proc dl0 . dl*)
       (pcheck ([procedure? proc] [dlist? dl0] [all-dlists? dl*])
               (apply check-length who dl0 dl*)
               (let loop ([n0 (dlist-first dl0)] [n* (map dlist-first dl*)])
                 (unless (null-dnode? n0)
                   (begin (apply proc (dnode-value n0) (map dnode-value n*))
                          (loop (dnode-right n0) (map dnode-right n*))))))]))


  (define-who dlist-for-each/i
    (case-lambda
      [(proc dl0)
       (pcheck ([procedure? proc] [dlist? dl0])
               (let loop ([i 0] [n0 (dlist-first dl0)])
                 (unless (null-dnode? n0)
                   (begin (proc i (dnode-value n0))
                          (loop (fx1+ i) (dnode-right n0))))))]
      [(proc dl0 dl1)
       (pcheck ([procedure? proc] [dlist? dl0] [dlist? dl1])
               (check-length who dl0 dl1)
               (let loop ([i 0] [n0 (dlist-first dl0)] [n1 (dlist-first dl1)])
                 (unless (null-dnode? n0)
                   (begin (proc i (dnode-value n0) (dnode-value n1))
                          (loop (fx1+ i) (dnode-right n0) (dnode-right n1))))))]
      [(proc dl0 . dl*)
       (pcheck ([procedure? proc] [dlist? dl0] [all-dlists? dl*])
               (apply check-length who dl0 dl*)
               (let loop ([i 0] [n0 (dlist-first dl0)] [n* (map dlist-first dl*)])
                 (unless (null-dnode? n0)
                   (begin (apply proc i (dnode-value n0) (map dnode-value n*))
                          (loop (fx1+ i) (dnode-right n0) (map dnode-right n*))))))]))


  (define-who dlist-andmap
    (case-lambda
      [(proc dl0)
       (pcheck ([procedure? proc] [dlist? dl0])
               (let loop ([n0 (dlist-first dl0)])
                 (if (null-dnode? n0)
                     #t
                     (and (proc (dnode-value n0))
                          (loop (dnode-right n0))))))]
      [(proc dl0 dl1)
       (pcheck ([procedure? proc] [dlist? dl0] [dlist? dl1])
               (check-length who dl0 dl1)
               (let loop ([n0 (dlist-first dl0)] [n1 (dlist-first dl1)])
                 (if (null-dnode? n0)
                     #t
                     (and (proc (dnode-value n0) (dnode-value n1))
                          (loop (dnode-right n0) (dnode-right n1))))))]
      [(proc dl0 . dl*)
       (pcheck ([procedure? proc] [dlist? dl0] [all-dlists? dl*])
               (apply check-length who dl0 dl*)
               (let loop ([n0 (dlist-first dl0)] [n* (map dlist-first dl*)])
                 (if (null-dnode? n0)
                     #t
                     (and (apply proc (dnode-value n0) (map dnode-value n*))
                          (loop (dnode-right n0) (map dnode-right n*))))))]))


  (define-who dlist-ormap
    (case-lambda
      [(proc dl0)
       (pcheck ([procedure? proc] [dlist? dl0])
               (let loop ([n0 (dlist-first dl0)])
                 (if (null-dnode? n0)
                     #f
                     (or (proc (dnode-value n0))
                         (loop (dnode-right n0))))))]
      [(proc dl0 dl1)
       (pcheck ([procedure? proc] [dlist? dl0] [dlist? dl1])
               (check-length who dl0 dl1)
               (let loop ([n0 (dlist-first dl0)] [n1 (dlist-first dl1)])
                 (if (null-dnode? n0)
                     #f
                     (or (proc (dnode-value n0) (dnode-value n1))
                         (loop (dnode-right n0) (dnode-right n1))))))]
      [(proc dl0 . dl*)
       (pcheck ([procedure? proc] [dlist? dl0] [all-dlists? dl*])
               (apply check-length who dl0 dl*)
               (let loop ([n0 (dlist-first dl0)] [n* (map dlist-first dl*)])
                 (if (null-dnode? n0)
                     #f
                     (or (apply proc (dnode-value n0) (map dnode-value n*))
                         (loop (dnode-right n0) (map dnode-right n*))))))]))


;;;; reverse order

  #|doc
  `proc` is applied to items of given dlist(s) in reverse order,
  the result of which is collected into a new dlist.
  |#
  (define-who dlist-map-rev
    (case-lambda
      [(proc dl0)
       (pcheck ([procedure? proc] [dlist? dl0])
               (let ([newdl (make-dlist)])
                 (let loop ([n0 (dlist-last dl0)])
                   (if (null-dnode? n0)
                       newdl
                       (begin (dlist-add! newdl (proc (dnode-value n0)))
                              (loop (dnode-left n0)))))))]
      [(proc dl0 dl1)
       (pcheck ([procedure? proc] [dlist? dl0] [dlist? dl1])
               (check-length who dl0 dl1)
               (let ([newdl (make-dlist)])
                 (let loop ([n0 (dlist-last dl0)] [n1 (dlist-last dl1)])
                   (if (null-dnode? n0)
                       newdl
                       (begin (dlist-add! newdl (proc (dnode-value n0) (dnode-value n1)))
                              (loop (dnode-left n0) (dnode-left n1)))))))]
      [(proc dl0 . dl*)
       (pcheck ([procedure? proc] [dlist? dl0] [all-dlists? dl*])
               (apply check-length who dl0 dl*)
               (let ([newdl (make-dlist)])
                 (let loop ([n0 (dlist-last dl0)] [n* (map dlist-last dl*)])
                   (if (null-dnode? n0)
                       newdl
                       (begin (dlist-add! newdl (apply proc (dnode-value n0) (map dnode-value n*)))
                              (loop (dnode-left n0) (map dnode-left n*)))))))]))


  #|doc
  Note that the index starts from the dlist length minus 1.
  |#
  (define-who dlist-map/i-rev
    (case-lambda
      [(proc dl0)
       (pcheck ([procedure? proc] [dlist? dl0])
               (let ([newdl (make-dlist)])
                 (let loop ([i (fx1- (dlist-length dl0))] [n0 (dlist-last dl0)])
                   (if (null-dnode? n0)
                       newdl
                       (begin (dlist-add! newdl (proc i (dnode-value n0)))
                              (loop (fx1- i) (dnode-left n0)))))))]
      [(proc dl0 dl1)
       (pcheck ([procedure? proc] [dlist? dl0] [dlist? dl1])
               (check-length who dl0 dl1)
               (let ([newdl (make-dlist)])
                 (let loop ([i (fx1- (dlist-length dl0))] [n0 (dlist-last dl0)] [n1 (dlist-last dl1)])
                   (if (null-dnode? n0)
                       newdl
                       (begin (dlist-add! newdl (proc i (dnode-value n0) (dnode-value n1)))
                              (loop (fx1- i) (dnode-left n0) (dnode-left n1)))))))]
      [(proc dl0 . dl*)
       (pcheck ([procedure? proc] [dlist? dl0] [all-dlists? dl*])
               (apply check-length who dl0 dl*)
               (let ([newdl (make-dlist)])
                 (let loop ([i (fx1- (dlist-length dl0))] [n0 (dlist-last dl0)] [n* (map dlist-last dl*)])
                   (if (null-dnode? n0)
                       newdl
                       (begin (dlist-add! newdl (apply proc i (dnode-value n0) (map dnode-value n*)))
                              (loop (fx1- i) (dnode-left n0) (map dnode-left n*)))))))]))


  (define-who dlist-for-each-rev
    (case-lambda
      [(proc dl0)
       (pcheck ([procedure? proc] [dlist? dl0])
               (let loop ([n0 (dlist-last dl0)])
                 (unless (null-dnode? n0)
                   (begin (proc (dnode-value n0))
                          (loop (dnode-left n0))))))]
      [(proc dl0 dl1)
       (pcheck ([procedure? proc] [dlist? dl0] [dlist? dl1])
               (check-length who dl0 dl1)
               (let loop ([n0 (dlist-last dl0)] [n1 (dlist-last dl1)])
                 (unless (null-dnode? n0)
                   (begin (proc (dnode-value n0) (dnode-value n1))
                          (loop (dnode-left n0) (dnode-left n1))))))]
      [(proc dl0 . dl*)
       (pcheck ([procedure? proc] [dlist? dl0] [all-dlists? dl*])
               (apply check-length who dl0 dl*)
               (let loop ([n0 (dlist-last dl0)] [n* (map dlist-last dl*)])
                 (unless (null-dnode? n0)
                   (begin (apply proc (dnode-value n0) (map dnode-value n*))
                          (loop (dnode-left n0) (map dnode-left n*))))))]))


  (define-who dlist-for-each/i-rev
    (case-lambda
      [(proc dl0)
       (pcheck ([procedure? proc] [dlist? dl0])
               (let loop ([i (fx1- (dlist-length dl0))] [n0 (dlist-last dl0)])
                 (unless (null-dnode? n0)
                   (begin (proc i (dnode-value n0))
                          (loop (fx1- i) (dnode-left n0))))))]
      [(proc dl0 dl1)
       (pcheck ([procedure? proc] [dlist? dl0] [dlist? dl1])
               (check-length who dl0 dl1)
               (let loop ([i (fx1- (dlist-length dl0))] [n0 (dlist-last dl0)] [n1 (dlist-last dl1)])
                 (unless (null-dnode? n0)
                   (begin (proc i (dnode-value n0) (dnode-value n1))
                          (loop (fx1- i) (dnode-left n0) (dnode-left n1))))))]
      [(proc dl0 . dl*)
       (pcheck ([procedure? proc] [dlist? dl0] [all-dlists? dl*])
               (apply check-length who dl0 dl*)
               (let loop ([i (fx1- (dlist-length dl0))] [n0 (dlist-last dl0)] [n* (map dlist-last dl*)])
                 (unless (null-dnode? n0)
                   (begin (apply proc i (dnode-value n0) (map dnode-value n*))
                          (loop (fx1- i) (dnode-left n0) (map dnode-left n*))))))]))


;;;; folds


  (define-who dlist-fold-left
    (case-lambda
      [(proc acc dl0)
       (pcheck ([procedure? proc] [dlist? dl0])
               (let loop ([acc acc] [n0 (dlist-first dl0)])
                 (if (null-dnode? n0)
                     acc
                     (loop (proc acc (dnode-value n0))
                           (dnode-right n0)))))]
      [(proc acc dl0 dl1)
       (pcheck ([procedure? proc] [dlist? dl0] [dlist? dl1])
               (check-length who dl0 dl1)
               (let loop ([acc acc] [n0 (dlist-first dl0)] [n1 (dlist-first dl1)])
                 (if (null-dnode? n0)
                     acc
                     (loop (proc acc (dnode-value n0) (dnode-value n1))
                           (dnode-right n0) (dnode-right n1)))))]
      [(proc acc dl0 . dl*)
       (pcheck ([procedure? proc] [dlist? dl0] [all-dlists? dl*])
               (apply check-length who dl0 dl*)
               (let loop ([acc acc] [n0 (dlist-first dl0)] [n* (map dlist-first dl*)])
                 (if (null-dnode? n0)
                     acc
                     (loop (apply proc acc (dnode-value n0) (map dnode-value n*))
                           (dnode-right n0) (map dnode-right n*)))))]))


  (define-who dlist-fold-left/i
    (case-lambda
      [(proc acc dl0)
       (pcheck ([procedure? proc] [dlist? dl0])
               (let loop ([i 0] [acc acc] [n0 (dlist-first dl0)])
                 (if (null-dnode? n0)
                     acc
                     (loop (fx1+ i) (proc i acc (dnode-value n0))
                           (dnode-right n0)))))]
      [(proc acc dl0 dl1)
       (pcheck ([procedure? proc] [dlist? dl0] [dlist? dl1])
               (check-length who dl0 dl1)
               (let loop ([i 0] [acc acc] [n0 (dlist-first dl0)] [n1 (dlist-first dl1)])
                 (if (null-dnode? n0)
                     acc
                     (loop (fx1+ i) (proc i acc (dnode-value n0) (dnode-value n1))
                           (dnode-right n0) (dnode-right n1)))))]
      [(proc acc dl0 . dl*)
       (pcheck ([procedure? proc] [dlist? dl0] [all-dlists? dl*])
               (apply check-length who dl0 dl*)
               (let loop ([i 0] [acc acc] [n0 (dlist-first dl0)] [n* (map dlist-first dl*)])
                 (if (null-dnode? n0)
                     acc
                     (loop (fx1+ i) (apply proc i acc (dnode-value n0) (map dnode-value n*))
                           (dnode-right n0) (map dnode-right n*)))))]))


  (define-who dlist-fold-right
    (case-lambda
      [(proc acc dl0)
       (pcheck ([procedure? proc] [dlist? dl0])
               (let loop ([acc acc] [n0 (dlist-last dl0)])
                 (if (null-dnode? n0)
                     acc
                     (loop (proc (dnode-value n0) acc)
                           (dnode-left n0)))))]
      [(proc acc dl0 dl1)
       (pcheck ([procedure? proc] [dlist? dl0] [dlist? dl1])
               (check-length who dl0 dl1)
               (let loop ([acc acc] [n0 (dlist-last dl0)] [n1 (dlist-last dl1)])
                 (if (null-dnode? n0)
                     acc
                     (loop (proc (dnode-value n0) (dnode-value n1) acc)
                           (dnode-left n0) (dnode-left n1)))))]
      [(proc acc dl0 . dl*)
       (pcheck ([procedure? proc] [dlist? dl0] [all-dlists? dl*])
               (apply check-length who dl0 dl*)
               (let loop ([acc acc] [n0 (dlist-last dl0)] [n* (map dlist-last dl*)])
                 (if (null-dnode? n0)
                     acc
                     (loop (apply proc (dnode-value n0) `(,@(map dnode-value n*) ,acc))
                           (dnode-left n0) (map dnode-left n*)))))]))


  (define-who dlist-fold-right/i
    (case-lambda
      [(proc acc dl0)
       (pcheck ([procedure? proc] [dlist? dl0])
               (let loop ([i (fx1- (dlist-length dl0))] [acc acc] [n0 (dlist-last dl0)])
                 (if (null-dnode? n0)
                     acc
                     (loop (fx1- i) (proc i (dnode-value n0) acc)
                           (dnode-left n0)))))]
      [(proc acc dl0 dl1)
       (pcheck ([procedure? proc] [dlist? dl0] [dlist? dl1])
               (check-length who dl0 dl1)
               (let loop ([i (fx1- (dlist-length dl0))] [acc acc] [n0 (dlist-last dl0)] [n1 (dlist-last dl1)])
                 (if (null-dnode? n0)
                     acc
                     (loop (fx1- i) (proc i (dnode-value n0) (dnode-value n1) acc)
                           (dnode-left n0) (dnode-left n1)))))]
      [(proc acc dl0 . dl*)
       (pcheck ([procedure? proc] [dlist? dl0] [all-dlists? dl*])
               (apply check-length who dl0 dl*)
               (let loop ([i (fx1- (dlist-length dl0))] [acc acc] [n0 (dlist-last dl0)] [n* (map dlist-last dl*)])
                 (if (null-dnode? n0)
                     acc
                     (loop (fx1- i) (apply proc i (dnode-value n0) `(,@(map dnode-value n*) ,acc))
                           (dnode-left n0) (map dnode-left n*)))))]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   conversions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  #|doc
  Convert a doubly-linked list to a list.
  |#
  (define-who dlist->list
    (lambda (dl)
      (pcheck ([dlist? dl])
              (let ([len (dlist-length dl)])
                (if (fx= len 0)
                    '()
                    (let ([lb (make-list-builder)])
                      (let loop ([n (dlist-first dl)])
                        (if  (null-dnode? n)
                             (lb)
                             (begin (lb (dnode-value n))
                                    (loop (dnode-right n)))))))))))


  #|doc
  Convert a list to a doubly-linked list.
  |#
  (define-who list->dlist
    (lambda (ls)
      (pcheck-list (ls)
                   (apply dlist ls))))


  #|doc
  Convert a doubly-linked list to a vector.
  |#
  (define-who dlist->vector
    (lambda (dl)
      (pcheck ([dlist? dl])
              (let ([len (dlist-length dl)])
                (if (fx= len 0)
                    (vector)
                    (let ([vec (make-vector len)])
                      (let loop ([i 0] [n (dlist-first dl)])
                        (if (fx= i len)
                            vec
                            (begin (vector-set! vec i (dnode-value n))
                                   (loop (fx1+ i) (dnode-right n)))))))))))


  #|doc
  Convert a vector to a doubly-linked list.
  |#
  (define-who vector->dlist
    (lambda (vec)
      (pcheck-vector (vec)
                     (let ([dl (dlist)])
                       (vector-for-each (lambda (x)
                                          (dlist-add! dl x))
                                        vec)
                       dl))))



  (record-writer (type-descriptor $dlist)
                 (lambda (r p wr)
                   (display "#[dlist " p)
                   ;; TODO visit dnodes directly
                   (wr (dlist->list r) p)
                   (display "]" p)))

  (record-writer (type-descriptor dnode)
                 (lambda (r p wr)
                   (display "#[dnode " p)
                   (wr (dnode-value r) p)
                   (display "]" p)))

  (record-type-equal-procedure (type-descriptor $dlist)
                               (lambda (dl1 dl2 =?)
                                 (and (fx= (dlist-length dl1) (dlist-length dl2))
                                      (let loop ([n1 (dlist-first dl1)] [n2 (dlist-first dl2)])
                                        (if (null-dnode? n1)
                                            #t
                                            (and (=? (dnode-value n1) (dnode-value n2))
                                                 (loop (dnode-right n1) (dnode-right n2))))))))

  )
