(library (chezpp private rbtree1)
  (export rbtree1 make-rbtree1 rbtree1-=? rbtree1-<?
          rbtree1-ref rbtree1-set! rbtree1-delete!
          rbtree1-clear! rbtree1-size
          rbtree1-contains? rbtree1-contains/p?
          rbtree1-search

          rbtree1-successor rbtree1-predecessor
          rbtree1-min rbtree1-max

          rbtree1-andmap rbtree1-ormap
          rbtree1-map rbtree1-map/i rbtree1-map! rbtree1-map/i!
          rbtree1-for-each rbtree1-for-each/i
          rbtree1-fold-left rbtree1-fold-left/i
          rbtree1-fold-right rbtree1-fold-right/i

          rbtree1-andmap1 rbtree1-ormap1
          rbtree1-map1 rbtree1-map/i1
          rbtree1-for-each1 rbtree1-for-each/i1
          rbtree1-fold-left1 rbtree1-fold-left/i1
          rbtree1-fold-right1 rbtree1-fold-right/i1

          rbtree1-visit rbtree1-visit-preorder rbtree1-visit-postorder rbtree1-visit-inorder

          $rbtree1-verify rbtree1->dot)
  (import (chezpp chez)
          (chezpp internal)
          (chezpp utils))


  ;; The red-black tree implementation is based on the text in
  ;; Introduction to Algorithms, by Cormen, Leiserson et al.,
  ;; with the difference that in the textbook, nil nodes are defined per tree,
  ;; here however, the nil node is global.

  ;; Nodes contain key, parent, left, right and color; no value slot is allocated.
  ;; Private compatibility visitors supply #f to their unused value argument.
  ;; No type checking is performed here.
  ;; It is performed in treemap and treeset code.

  (define RED   0)
  (define BLACK 1)

  (define mk-rbnode (lambda (k v p) (vector k p null-rbnode null-rbnode RED)))

  ;; used as parent of root and children of leaves
  (define null-rbnode  '())
  (define null-rbnode? null?)

  (define rbnode-key    (lambda (n) (vector-ref n 0)))
  (define rbnode-value  (lambda (n) #f))
  (define rbnode-parent (lambda (n) (if (null-rbnode? n) n     (vector-ref n 1))))
  (define rbnode-left   (lambda (n) (if (null-rbnode? n) n     (vector-ref n 2))))
  (define rbnode-right  (lambda (n) (if (null-rbnode? n) n     (vector-ref n 3))))
  (define rbnode-color  (lambda (n) (if (null-rbnode? n) BLACK (vector-ref n 4))))

  (define rbnode-key-set!    (lambda (n v) (vector-set! n 0 v)))
  (define rbnode-value-set!  (lambda (n v) (void)))
  (define rbnode-parent-set! (lambda (n v) (unless (null-rbnode? n) (vector-set! n 1 v))))
  (define rbnode-left-set!   (lambda (n v) (unless (null-rbnode? n) (vector-set! n 2 v))))
  (define rbnode-right-set!  (lambda (n v) (unless (null-rbnode? n) (vector-set! n 3 v))))
  (define rbnode-color-set!  (lambda (n v) (unless (null-rbnode? n) (vector-set-fixnum! n 4 v))))

  (define rbnode-set-red!    (lambda (n) (unless (null-rbnode? n) (vector-set-fixnum! n 4 RED))))
  (define rbnode-set-black!  (lambda (n) (unless (null-rbnode? n) (vector-set-fixnum! n 4 BLACK))))
  (define rbnode-red?   (lambda (n) (if (null-rbnode? n) #f (fx= (vector-ref n 4) RED))))
  (define rbnode-black? (lambda (n) (if (null-rbnode? n) #t (fx= (vector-ref n 4) BLACK))))

  (define K rbnode-key)
  (define V rbnode-value)
  (define P rbnode-parent)
  (define R rbnode-right)
  (define L rbnode-left)
  (define C rbnode-color)

  (define K! rbnode-key-set!)
  (define V! rbnode-value-set!)
  (define P! rbnode-parent-set!)
  (define R! rbnode-right-set!)
  (define L! rbnode-left-set!)
  (define C! rbnode-color-set!)

  (define RED?   rbnode-red?)
  (define BLACK? rbnode-black?)
  (define RED!   rbnode-set-red!)
  (define BLACK! rbnode-set-black!)


  (define-record-type (rbtree1 mk-rbtree1 rbtree1?)
    (nongenerative) (opaque #t)
    (fields (mutable root) (immutable =?) (immutable <?) (mutable size) (immutable fixnum-keys?))
    (protocol
     (lambda (new)
       (case-lambda
         [(=? <? size) (new null-rbnode =? <? size #f)]
         [(=? <? size fixnum-keys?) (new null-rbnode =? <? size fixnum-keys?)]))))


  (define make-rbtree1
    (case-lambda
      [(who =? <?) (mk-rbtree1 =? <? 0 #f)]
      [(who =? <? fixnum-keys?) (mk-rbtree1 =? <? 0 fixnum-keys?)]))


  (define rotate-left!
    (lambda (rbt p)
      (unless (null-rbnode? p)
        (let* ([r (R p)] [rL (L r)])
          (R! p rL)
          (unless (null-rbnode? rL) (P! rL p))
          (let ([pP (P p)])
            (P! r pP)
            (cond [(null-rbnode? pP) (rbtree1-root-set! rbt r)]
                  [(eq? p (L pP))    (L! pP r)]
                  [else              (R! pP r)])
            (L! r p)
            (P! p r))))))
  (define rotate-right!
    (lambda (rbt p)
      (unless (null-rbnode? p)
        (let* ([l (L p)] [lR (R l)])
          (L! p lR)
          (unless (null-rbnode? lR) (P! lR p))
          (let ([pP (P p)])
            (P! l pP)
            (cond [(null-rbnode? pP) (rbtree1-root-set! rbt l)]
                  [(eq? p (R pP))    (R! pP l)]
                  [else              (L! pP l)])
            (R! l p)
            (P! p l))))))
  (define minimum
    (lambda (n)
      (let loop ([n n])
        (let ([l (L n)])
          (if (null-rbnode? l)
              n
              (loop l))))))
  (define maximum
    (lambda (n)
      (let loop ([n n])
        (let ([r (R n)])
          (if (null-rbnode? r)
              n
              (loop r))))))


  (define rbtree1-ref
    (case-lambda
      [(who rbt k)
       (rbtree1-check-key who rbt k)
       (let ([=? (rbtree1-=? rbt)] [<? (rbtree1-<? rbt)])
         (let loop ([n (rbtree1-root rbt)])
           (if (null-rbnode? n)
               (errorf who "key not found: ~a" k)
               (cond [(=? k (K n)) (V n)]
                     [(<? k (K n)) (loop (L n))]
                     [else  (loop (R n))]))))]
      [(who rbt k default)
       (rbtree1-check-key who rbt k)
       (let ([=? (rbtree1-=? rbt)] [<? (rbtree1-<? rbt)])
         (let loop ([n (rbtree1-root rbt)])
           (if (null-rbnode? n)
               default
               (cond [(=? k (K n)) (V n)]
                     [(<? k (K n)) (loop (L n))]
                     [else  (loop (R n))]))))]))


  (define rbtree1-check-key
    (lambda (who tree key)
      (when (rbtree1-fixnum-keys? tree)
        (pcheck ([fixnum? key]) (void)))))

  (define rbtree1-set!
    (lambda (who tree key value)
      (if (rbtree1-fixnum-keys? tree)
          (pcheck ([fixnum? key]) (rbtree1-set/fixnum! who tree key value))
          (rbtree1-set/generic! who tree key value))))

  (define rbtree1-set/generic!
    (lambda (who rbt k v)
      (define fix!
        (lambda (n)
          (let loop ([z n])
            (when (and (not (null-rbnode? z)) (RED? (P z)))
              (if (eq? (P z) (L (P (P z))))
                  (let ([y (R (P (P z)))])
                    (if (RED? y)
                        (begin (BLACK! (P z))
                               (BLACK! y)
                               (RED! (P (P z)))
                               (loop (P (P z))))
                        (let ([z (if (eq? z (R (P z)))
                                     (let ([zP (P z)])
                                       (rotate-left! rbt zP)
                                       zP)
                                     z)])
                          (BLACK! (P z))
                          (RED!   (P (P z)))
                          (rotate-right! rbt (P (P z)))
                          (loop z))))
                  ;; symmetric case
                  (let ([y (L (P (P z)))])
                    (if (RED? y)
                        (begin (BLACK! (P z))
                               (BLACK! y)
                               (RED! (P (P z)))
                               (loop (P (P z))))
                        (let ([z (if (eq? z (L (P z)))
                                     (let ([zP (P z)])
                                       (rotate-right! rbt zP)
                                       zP)
                                     z)])
                          (BLACK! (P z))
                          (RED!   (P (P z)))
                          (rotate-left! rbt (P (P z)))
                          (loop z))))))
            (BLACK! (rbtree1-root rbt)))))

      (let ([root (rbtree1-root rbt)] [=? (rbtree1-=? rbt)] [<? (rbtree1-<? rbt)])
        ;; x: current node, y: parent of x
        (let loop ([x root] [y null-rbnode])
          (if (null-rbnode? x)
              ;; z is by default RED
              (let ([z (mk-rbnode k v y)])
                (cond [(null-rbnode? y) (rbtree1-root-set! rbt z)]
                      [(<? k (K y))     (L! y z)]
                      [else             (R! y z)])
                (fix! z)
                (rbtree1-size-set! rbt (fx1+ (rbtree1-size rbt))))
              (let ([kk (K x)])
                (cond [(=? k kk) (V! x v)]
                      [(<? k kk) (loop (L x) x)]
                      [else      (loop (R x) x)])))))))


  (define rbtree1-set/fixnum!
    (lambda (who rbt k v)
      (define fix!
        (lambda (n)
          (let loop ([z n])
            (when (and (not (null-rbnode? z)) (RED? (P z)))
              (if (eq? (P z) (L (P (P z))))
                  (let ([y (R (P (P z)))])
                    (if (RED? y)
                        (begin (BLACK! (P z))
                               (BLACK! y)
                               (RED! (P (P z)))
                               (loop (P (P z))))
                        (let ([z (if (eq? z (R (P z)))
                                     (let ([zP (P z)])
                                       (rotate-left! rbt zP)
                                       zP)
                                     z)])
                          (BLACK! (P z))
                          (RED!   (P (P z)))
                          (rotate-right! rbt (P (P z)))
                          (loop z))))
                  ;; symmetric case
                  (let ([y (L (P (P z)))])
                    (if (RED? y)
                        (begin (BLACK! (P z))
                               (BLACK! y)
                               (RED! (P (P z)))
                               (loop (P (P z))))
                        (let ([z (if (eq? z (L (P z)))
                                     (let ([zP (P z)])
                                       (rotate-right! rbt zP)
                                       zP)
                                     z)])
                          (BLACK! (P z))
                          (RED!   (P (P z)))
                          (rotate-left! rbt (P (P z)))
                          (loop z))))))
            (BLACK! (rbtree1-root rbt)))))

      (let ([root (rbtree1-root rbt)] [=? (rbtree1-=? rbt)] [<? (rbtree1-<? rbt)])
        ;; x: current node, y: parent of x
        (let loop ([x root] [y null-rbnode])
          (if (null-rbnode? x)
              ;; z is by default RED
              (let ([z (let ([node (vector 0 y null-rbnode null-rbnode RED)])
                         (vector-set-fixnum! node 0 k)
                         node)])
                (cond [(null-rbnode? y) (rbtree1-root-set! rbt z)]
                      [(<? k (K y))     (L! y z)]
                      [else             (R! y z)])
                (fix! z)
                (rbtree1-size-set! rbt (fx1+ (rbtree1-size rbt))))
              (let ([kk (K x)])
                (cond [(=? k kk) (V! x v)]
                      [(<? k kk) (loop (L x) x)]
                      [else      (loop (R x) x)])))))))


  (define rbtree1-delete!
    (lambda (who tree key)
      (if (rbtree1-fixnum-keys? tree)
          (pcheck ([fixnum? key]) (rbtree1-delete/fixnum! who tree key))
          (rbtree1-delete/generic! who tree key))))

  (define rbtree1-delete/generic!
    (lambda (who rbt k)
      (define fix!
        (lambda (x)
          (let loop ([x x])
            (if (and (not (eq? x (rbtree1-root rbt))) (BLACK? x))
                (if (eq? x (L (P x)))
                    (let ([w (let ([w (R (P x))])
                               (if (RED? w)
                                   (begin (BLACK! w)
                                          (RED!   (P x))
                                          (rotate-left! rbt (P x))
                                          (R (P x)))
                                   w))])
                      (if (and (BLACK? (L w)) (BLACK? (R w)))
                          (begin (RED! w)
                                 (loop (P x)))
                          (let ([w (if (BLACK? (R w))
                                       (begin (BLACK! (L w))
                                              (RED!   w)
                                              (rotate-right! rbt w)
                                              (R (P x)))
                                       w)])
                            (C! w (C (P x)))
                            (BLACK! (P x))
                            (BLACK! (R w))
                            (rotate-left! rbt (P x))
                            (loop (rbtree1-root rbt)))))
                    ;; symmetric case
                    (let ([w (let ([w (L (P x))])
                               (if (RED? w)
                                   (begin (BLACK! w)
                                          (RED!   (P x))
                                          (rotate-right! rbt (P x))
                                          (L (P x)))
                                   w))])
                      (if (and (BLACK? (R w)) (BLACK? (L w)))
                          (begin (RED! w)
                                 (loop (P x)))
                          (let ([w (if (BLACK? (L w))
                                       (begin (BLACK! (R w))
                                              (RED!   w)
                                              (rotate-left! rbt w)
                                              (L (P x)))
                                       w)])
                            (C! w (C (P x)))
                            (BLACK! (P x))
                            (BLACK! (L w))
                            (rotate-right! rbt (P x))
                            (loop (rbtree1-root rbt))))))
                ;; must do this inside the loop
                (BLACK! x)))))
      ;; from Java
      (define delete!
        (lambda (p)
          (let* ([p (if (and (not (null-rbnode? (L p)))
                             (not (null-rbnode? (R p))))
                        (let ([s (minimum (R p))])
                          (K! p (K s))
                          (V! p (V s))
                          s)
                        p)]
                 [replacement (if (not (null-rbnode? (L p)))
                                  (L p)
                                  ;; (R p) could also be null
                                  (R p))])
            (cond
             [(not (null-rbnode? replacement))
              ;; transplant
              (P! replacement (P p))
              (cond
               [(null-rbnode? (P p)) (rbtree1-root-set! rbt replacement)]
               [(eq? p (L (P p)))    (L! (P p) replacement)]
               [else                 (R! (P p) replacement)])
              (L! p null-rbnode)
              (R! p null-rbnode)
              (P! p null-rbnode)
              (when (BLACK? p) (fix! replacement))]
             [(null-rbnode? (P p))
              (rbtree1-root-set! rbt null-rbnode)]
             [else (when (BLACK? p) (fix! p))
                   (unless (null-rbnode? (P p))
                     (cond [(eq? p (L (P p)))
                            (L! (P p) null-rbnode)]
                           [(eq? p (R (P p)))
                            (R! (P p) null-rbnode)]
                           [else (assert-unreachable)])
                     (P! p null-rbnode))]))))

      (let ([root (rbtree1-root rbt)] [=? (rbtree1-=? rbt)] [<? (rbtree1-<? rbt)])
        (let loop ([x root])
          (if (null-rbnode? x)
              (errorf who "key not found: ~a" k)
              (let ([kk (K x)])
                (cond [(=? k kk)
                       (delete! x)
                       (rbtree1-size-set! rbt (fx1- (rbtree1-size rbt)))]
                      [(<? k kk) (loop (L x))]
                      [else      (loop (R x))])))))))


  (define rbtree1-delete/fixnum!
    (lambda (who rbt k)
      (define fix!
        (lambda (x)
          (let loop ([x x])
            (if (and (not (eq? x (rbtree1-root rbt))) (BLACK? x))
                (if (eq? x (L (P x)))
                    (let ([w (let ([w (R (P x))])
                               (if (RED? w)
                                   (begin (BLACK! w)
                                          (RED!   (P x))
                                          (rotate-left! rbt (P x))
                                          (R (P x)))
                                   w))])
                      (if (and (BLACK? (L w)) (BLACK? (R w)))
                          (begin (RED! w)
                                 (loop (P x)))
                          (let ([w (if (BLACK? (R w))
                                       (begin (BLACK! (L w))
                                              (RED!   w)
                                              (rotate-right! rbt w)
                                              (R (P x)))
                                       w)])
                            (C! w (C (P x)))
                            (BLACK! (P x))
                            (BLACK! (R w))
                            (rotate-left! rbt (P x))
                            (loop (rbtree1-root rbt)))))
                    ;; symmetric case
                    (let ([w (let ([w (L (P x))])
                               (if (RED? w)
                                   (begin (BLACK! w)
                                          (RED!   (P x))
                                          (rotate-right! rbt (P x))
                                          (L (P x)))
                                   w))])
                      (if (and (BLACK? (R w)) (BLACK? (L w)))
                          (begin (RED! w)
                                 (loop (P x)))
                          (let ([w (if (BLACK? (L w))
                                       (begin (BLACK! (R w))
                                              (RED!   w)
                                              (rotate-left! rbt w)
                                              (L (P x)))
                                       w)])
                            (C! w (C (P x)))
                            (BLACK! (P x))
                            (BLACK! (L w))
                            (rotate-right! rbt (P x))
                            (loop (rbtree1-root rbt))))))
                ;; must do this inside the loop
                (BLACK! x)))))
      ;; from Java
      (define delete!
        (lambda (p)
          (let* ([p (if (and (not (null-rbnode? (L p)))
                             (not (null-rbnode? (R p))))
                        (let ([s (minimum (R p))])
                          (vector-set-fixnum! p 0 (K s))
                          (V! p (V s))
                          s)
                        p)]
                 [replacement (if (not (null-rbnode? (L p)))
                                  (L p)
                                  ;; (R p) could also be null
                                  (R p))])
            (cond
             [(not (null-rbnode? replacement))
              ;; transplant
              (P! replacement (P p))
              (cond
               [(null-rbnode? (P p)) (rbtree1-root-set! rbt replacement)]
               [(eq? p (L (P p)))    (L! (P p) replacement)]
               [else                 (R! (P p) replacement)])
              (L! p null-rbnode)
              (R! p null-rbnode)
              (P! p null-rbnode)
              (when (BLACK? p) (fix! replacement))]
             [(null-rbnode? (P p))
              (rbtree1-root-set! rbt null-rbnode)]
             [else (when (BLACK? p) (fix! p))
                   (unless (null-rbnode? (P p))
                     (cond [(eq? p (L (P p)))
                            (L! (P p) null-rbnode)]
                           [(eq? p (R (P p)))
                            (R! (P p) null-rbnode)]
                           [else (assert-unreachable)])
                     (P! p null-rbnode))]))))

      (let ([root (rbtree1-root rbt)] [=? (rbtree1-=? rbt)] [<? (rbtree1-<? rbt)])
        (let loop ([x root])
          (if (null-rbnode? x)
              (errorf who "key not found: ~a" k)
              (let ([kk (K x)])
                (cond [(=? k kk)
                       (delete! x)
                       (rbtree1-size-set! rbt (fx1- (rbtree1-size rbt)))]
                      [(<? k kk) (loop (L x))]
                      [else      (loop (R x))])))))))


  (define rbtree1-clear!
    (lambda (who rbt)
      (rbtree1-root-set! rbt null-rbnode)
      (rbtree1-size-set! rbt 0)))


  (define rbtree1-contains?
    (lambda (who rbt k)
      (rbtree1-check-key who rbt k)
      (let ([=? (rbtree1-=? rbt)] [<? (rbtree1-<? rbt)])
        (let loop ([n (rbtree1-root rbt)])
          (if (null-rbnode? n)
              #f
              (cond [(=? k (K n)) #t]
                    [(<? k (K n)) (loop (L n))]
                    [else  (loop (R n))]))))))


  (define rbtree1-contains/p?
    (lambda (who rbt pred)
      (let ([=? (rbtree1-=? rbt)] [<? (rbtree1-<? rbt)])
        (let loop ([n (rbtree1-root rbt)])
          (if (null-rbnode? n)
              #f
              (or (bool (pred (K n) (V n)))
                  (loop (L n))
                  (loop (R n))))))))


  (define rbtree1-search
    (lambda (who rbt pred)
      (let loop ([n (rbtree1-root rbt)])
        (if (null-rbnode? n)
            #f
            (or (let ([k (K n)] [v (V n)])
                  (if (pred k v) (cons k v) #f))
                (loop (L n))
                (loop (R n)))))))


  (define rbtree1-successor
    (lambda (who rbt k)
      (define successor
        (lambda (n)
          ;; either min of the right subtree,
          ;; or the nearest ancestor whose left child is also an ancestor of n
          (let ([r (R n)])
            (if (null-rbnode? r)
                (let loop ([x n] [xP (P n)])
                  (cond [(null-rbnode? xP) #f]
                        [(eq? x (R xP))    (loop xP (P xP))]
                        [else              xP]))
                (minimum r)))))
      (rbtree1-check-key who rbt k)
      (let ([=? (rbtree1-=? rbt)] [<? (rbtree1-<? rbt)])
        (let loop ([n (rbtree1-root rbt)])
          (if (null-rbnode? n)
              (errorf who "key not found: ~a" k)
              (cond [(=? k (K n)) (let ([n (successor n)])
                                    (if n (cons (K n) (V n)) n))]
                    [(<? k (K n)) (loop (L n))]
                    [else  (loop (R n))]))))))


  (define rbtree1-predecessor
    (lambda (who rbt k)
      (define predecessor
        (lambda (n)
          ;; either max of the left subtree,
          ;; or the nearest ancestor whose right child is also an ancestor of n
          (let ([l (L n)])
            (if (null-rbnode? l)
                (let loop ([x n] [xP (P n)])
                  (cond [(null-rbnode? xP) #f]
                        [(eq? x (L xP))    (loop xP (P xP))]
                        [else              xP]))
                (maximum l)))))
      (rbtree1-check-key who rbt k)
      (let ([=? (rbtree1-=? rbt)] [<? (rbtree1-<? rbt)])
        (let loop ([n (rbtree1-root rbt)])
          (if (null-rbnode? n)
              (errorf who "key not found: ~a" k)
              (cond [(=? k (K n)) (let ([n (predecessor n)])
                                    (if n (cons (K n) (V n)) n))]
                    [(<? k (K n)) (loop (L n))]
                    [else  (loop (R n))]))))))


  (define rbtree1-min
    (lambda (who rbt)
      (let ([root (rbtree1-root rbt)])
        (if (null-rbnode? root)
            #f
            (let ([n (minimum root)])
              (cons (K n) (V n)))))))


  (define rbtree1-max
    (lambda (who rbt)
      (let ([root (rbtree1-root rbt)])
        (if (null-rbnode? root)
            #f
            (let ([n (maximum root)])
              (cons (K n) (V n)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   iterations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define rbtree1-visit-preorder
    (lambda (who proc rbt)
      (let loop ([n (rbtree1-root rbt)])
        (unless (null-rbnode? n)
          (proc (K n) (V n))
          (loop (L n))
          (loop (R n))))))


  (define rbtree1-visit-postorder
    (lambda (who proc rbt)
      (let loop ([n (rbtree1-root rbt)])
        (unless (null-rbnode? n)
          (loop (L n))
          (loop (R n))
          (proc (K n) (V n))))))


  (define rbtree1-visit-inorder
    (lambda (who proc rbt)
      (let loop ([n (rbtree1-root rbt)])
        (unless (null-rbnode? n)
          (loop (L n))
          (proc (K n) (V n))
          (loop (R n))))))

  (define rbtree1-visit rbtree1-visit-inorder)


  ;; return a procedure that when called, either return a node in order,
  ;; or #f if all nodes are visited
  (define single-step-rbtree1-left
    (lambda (rbt)
      ;; stack: '((n1 L) (n2 R) ...),
      ;; records the remainder of the tree to be visited
      (let ([stack '()])
        ;; locate the first node
        (let ([n (rbtree1-root rbt)])
          (unless (null-rbnode? n)
            (let loop ([n n] [stk stack])
              (if (null-rbnode? n)
                  (set! stack stk)
                  (loop (L n) (cons (cons n 'L) stk))))))

        (lambda ()
          (if (null? stack)
              #f
              (let* ([T (car stack)] [n (car T)])
                (if (eq? 'L (cdr T))
                    ;; we were on the left subtree, update the state for the right tree
                    ;; and return the node
                    (begin (set-cdr! T 'R)
                           n)
                    ;; left subtree has been visited, continue with the right one
                    (let loop ([n (R n)] [stk (cons (cons n 'R) (cdr stack))])
                      (if (null-rbnode? n)
                          (let ([T (car stk)])
                            (if (eq? 'L (cdr T))
                                ;; we are on the left, just update the state and return the top node
                                (begin (set! stack stk)
                                       (set-cdr! T 'R)
                                       (car T))
                                ;; we are on the right subtree,
                                ;; pop the stack until we are on the left of an ancestor
                                (let next ([stk (cdr stk)])
                                  (if (null? stk)
                                      ;; terminate when the stack becomes empty when popping
                                      (begin (set! stack '())
                                             #f)
                                      (let ([T (car stk)])
                                        (if (eq? 'L (cdr T))
                                            (begin (set! stack stk)
                                                   (set-cdr! T 'R)
                                                   (car T))
                                            (next (cdr stk))))))))
                          (loop (L n) (cons (cons n 'L) stk)))))))))))

  ;; symmetric case: walk the tree from the rightmost node
  (define single-step-rbtree1-right
    (lambda (rbt)
      (let ([stack '()])
        (let ([n (rbtree1-root rbt)])
          (unless (null-rbnode? n)
            (let loop ([n n] [stk stack])
              (if (null-rbnode? n)
                  (set! stack stk)
                  (loop (R n) (cons (cons n 'R) stk))))))

        (lambda ()
          (if (null? stack)
              #f
              (let* ([T (car stack)] [n (car T)])
                (if (eq? 'R (cdr T))
                    (begin (set-cdr! T 'L)
                           n)
                    (let loop ([n (L n)] [stk (cons (cons n 'L) (cdr stack))])
                      (if (null-rbnode? n)
                          (let ([T (car stk)])
                            (if (eq? 'R (cdr T))
                                (begin (set! stack stk)
                                       (set-cdr! T 'L)
                                       (car T))
                                (let next ([stk (cdr stk)])
                                  (if (null? stk)
                                      (begin (set! stack '())
                                             #f)
                                      (let ([T (car stk)])
                                        (if (eq? 'R (cdr T))
                                            (begin (set! stack stk)
                                                   (set-cdr! T 'L)
                                                   (car T))
                                            (next (cdr stk))))))))
                          (loop (R n) (cons (cons n 'R) stk)))))))))))

  ;; TODO deduplicate this
  ;; currently placed here to minimize dependency
  (define make-list-builder
    (lambda args
      (let ([res args])
        (let ([current-cell (if (null? res)
                                (cons #f '())
                                (let loop ([res res])
                                  (if (null? (cdr res))
                                      res
                                      (loop (cdr res)))))]
              [next-cell (cons #f '())])
          (define add-item!
            (lambda (item)
              (if (null? res)
                  (begin (set-car! current-cell item)
                         (set! res current-cell))
                  (begin
                    (set-car! next-cell item)
                    (set-cdr! current-cell next-cell)
                    (set! current-cell next-cell)
                    (set! next-cell (cons #f '()))))))
          (rec lb
            (case-lambda
              [() res]
              [(x) (add-item! x)]
              [x* (for-each lb x*)]))))))
  (define kv*
    (lambda (n*)
      (let ([lb (make-list-builder)])
        (for-each (lambda (n) (lb (K n)) (lb (V n))) n*)
        (lb))))
  (define k*
    (lambda (n*)
      (let ([lb (make-list-builder)])
        (for-each (lambda (n) (lb (K n))) n*)
        (lb))))
  (define exe (lambda (x) (x)))

  ;; assume all args are checked
  ;; use inorder traversal


;;;; for treemap

  (define rbtree1-andmap
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbtree1-root rbt0)])
         (if (null-rbnode? n)
             #t
             (and (loop (L n))
                  (proc (K n) (V n))
                  (loop (R n)))))]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               #t
               (and (proc (K n0) (V n0) (K n1) (V n1))
                    (loop (iter0) (iter1))))))]
      [(who proc  rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               #t
               (and (apply proc (K n0) (V n0) (kv* n*))
                    (loop (iter0) (map exe iter*))))))]))


  (define rbtree1-ormap
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbtree1-root rbt0)])
         (if (null-rbnode? n)
             #f
             (or (loop (L n))
                 (proc (K n) (V n))
                 (loop (R n)))))]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               #f
               (or (proc (K n0) (V n0) (K n1) (V n1))
                   (loop (iter0) (iter1))))))]
      [(who proc  rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               #f
               (or (apply proc (K n0) (V n0) (kv* n*))
                   (loop (iter0) (map exe iter*))))))]))


  ;; newrbt: the new tree to be returned
  (define rbtree1-map
    (case-lambda
      [(who proc newrbt rbt0)
       (let loop ([n (rbtree1-root rbt0)])
         (unless (null-rbnode? n)
           (loop (L n))
           (let-values ([(k v) (proc (K n) (V n))])
             (rbtree1-set! who newrbt k v))
           (loop (R n))))
       newrbt]
      [(who proc newrbt rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               newrbt
               (let-values ([(k v) (proc (K n0) (V n0) (K n1) (V n1))])
                 (rbtree1-set! who newrbt k v)
                 (loop (iter0) (iter1))))))]
      [(who proc newrbt rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               newrbt
               (let-values ([(k v) (apply proc (K n0) (V n0) (kv* n*))])
                 (rbtree1-set! who newrbt k v)
                 (loop (iter0) (map exe iter*))))))]))


  (define rbtree1-map/i
    (case-lambda
      [(who proc newrbt rbt0)
       (let loop ([n (rbtree1-root rbt0)] [i 0])
         (if (null-rbnode? n)
             i
             (let ([i (loop (L n) i)])
               (let-values ([(k v) (proc i (K n) (V n))])
                 (rbtree1-set! who newrbt k v))
               (loop (R n) (fx1+ i)))))
       newrbt]
      [(who proc newrbt rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([i 0] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               newrbt
               (let-values ([(k v) (proc i (K n0) (V n0) (K n1) (V n1))])
                 (rbtree1-set! who newrbt k v)
                 (loop (fx1+ i) (iter0) (iter1))))))]
      [(who proc newrbt rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([i 0] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               newrbt
               (let-values ([(k v) (apply proc i (K n0) (V n0) (kv* n*))])
                 (rbtree1-set! who newrbt k v)
                 (loop (fx1+ i) (iter0) (map exe iter*))))))]))


  (define rbtree1-map!
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbtree1-root rbt0)])
         (unless (null-rbnode? n)
           (loop (L n))
           (V! n (proc (K n) (V n)))
           (loop (R n))))
       rbt0]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (unless (not (or n0 n1))
             (let ([v (proc (K n0) (V n0) (K n1) (V n1))])
               (V! n0 v)
               (loop (iter0) (iter1))))))
       rbt0]
      [(who proc rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (unless (not (or n0 (ormap id n*)))
             (let ([v (apply proc (K n0) (V n0) (kv* n*))])
               (V! n0 v)
               (loop (iter0) (map exe iter*))))))
       rbt0]))


  (define rbtree1-map/i!
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbtree1-root rbt0)] [i 0])
         (if (null-rbnode? n)
             i
             (let ([i (loop (L n) i)])
               (V! n (proc i (K n) (V n)))
               (loop (R n) (fx1+ i)))))
       rbt0]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([i 0] [n0 (iter0)] [n1 (iter1)])
           (unless (not (or n0 n1))
             (let ([v (proc i (K n0) (V n0) (K n1) (V n1))])
               (V! n0 v)
               (loop (fx1+ i) (iter0) (iter1))))))
       rbt0]
      [(who proc rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([i 0] [n0 (iter0)] [n* (map exe iter*)])
           (unless (not (or n0 (ormap id n*)))
             (let ([v (apply proc i (K n0) (V n0) (kv* n*))])
               (V! n0 v)
               (loop (fx1+ i) (iter0) (map exe iter*))))))
       rbt0]))


  (define rbtree1-for-each
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbtree1-root rbt0)])
         (unless (null-rbnode? n)
           (loop (L n))
           (proc (K n) (V n))
           (loop (R n))))]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (unless (not (or n0 n1))
             (proc (K n0) (V n0) (K n1) (V n1))
             (loop (iter0) (iter1)))))]
      [(who proc rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (unless (not (or n0 (ormap id n*)))
             (apply proc (K n0) (V n0) (kv* n*))
             (loop (iter0) (map exe iter*)))))]))


  (define rbtree1-for-each/i
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbtree1-root rbt0)] [i 0])
         (if (null-rbnode? n)
             i
             (let ([i (loop (L n) i)])
               (proc i (K n) (V n))
               (loop (R n) (fx1+ i)))))]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([i 0] [n0 (iter0)] [n1 (iter1)])
           (unless (not (or n0 n1))
             (proc i (K n0) (V n0) (K n1) (V n1))
             (loop (fx1+ i) (iter0) (iter1)))))]
      [(who proc rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([i 0] [n0 (iter0)] [n* (map exe iter*)])
           (unless (not (or n0 (ormap id n*)))
             (apply proc i (K n0) (V n0) (kv* n*))
             (loop (fx1+ i) (iter0) (map exe iter*)))))]))


  (define rbtree1-fold-left
    (case-lambda
      [(who proc acc rbt0)
       (let loop ([n (rbtree1-root rbt0)] [acc acc])
         (if (null-rbnode? n)
             acc
             (let ([acc (loop (L n) acc)])
               (loop (R n) (proc acc (K n) (V n))))))]
      [(who proc acc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([acc acc] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               acc
               (let ([acc (proc acc (K n0) (V n0) (K n1) (V n1))])
                 (loop acc (iter0) (iter1))))))]
      [(who proc acc rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([acc acc] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               acc
               (let ([acc (apply proc acc (K n0) (V n0) (kv* n*))])
                 (loop acc (iter0) (map exe iter*))))))]))


  (define rbtree1-fold-left/i
    (case-lambda
      [(who proc acc rbt0)
       (let-values ([(acc i)
                     (let loop ([n (rbtree1-root rbt0)] [acc acc] [i 0])
                       (if (null-rbnode? n)
                           (values acc i)
                           (let-values ([(acc i) (loop (L n) acc i)])
                             (loop (R n) (proc i acc (K n) (V n)) (fx1+ i)))))])
         acc)]
      [(who proc acc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([i 0] [acc acc] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               acc
               (let ([acc (proc i acc (K n0) (V n0) (K n1) (V n1))])
                 (loop (fx1+ i) acc (iter0) (iter1))))))]
      [(who proc acc rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([i 0] [acc acc] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               acc
               (let ([acc (apply proc i acc (K n0) (V n0) (kv* n*))])
                 (loop (fx1+ i) acc (iter0) (map exe iter*))))))]))


  (define rbtree1-fold-right
    (case-lambda
      [(who proc acc rbt0)
       (let loop ([n (rbtree1-root rbt0)] [acc acc])
         (if (null-rbnode? n)
             acc
             (let ([acc (loop (R n) acc)])
               (loop (L n) (proc (K n) (V n) acc)))))]
      [(who proc acc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-right rbt0)] [iter1 (single-step-rbtree1-right rbt1)])
         (let loop ([acc acc] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               acc
               (let ([acc (proc (K n0) (V n0) (K n1) (V n1) acc)])
                 (loop acc (iter0) (iter1))))))]
      [(who proc acc rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-right rbt0)] [iter* (map single-step-rbtree1-right rbt*)])
         (let loop ([acc acc] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               acc
               (let ([acc (apply proc (K n0) (V n0) `(,@(kv* n*) ,acc))])
                 (loop acc (iter0) (map exe iter*))))))]))


  (define rbtree1-fold-right/i
    (case-lambda
      [(who proc acc rbt0)
       (let-values ([(acc i)
                     (let loop ([n (rbtree1-root rbt0)] [acc acc] [i (fx1- (rbtree1-size rbt0))])
                       (if (null-rbnode? n)
                           (values acc i)
                           (let-values ([(acc i) (loop (R n) acc i)])
                             (loop (L n) (proc i (K n) (V n) acc) (fx1- i)))))])
         acc)]
      [(who proc acc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-right rbt0)] [iter1 (single-step-rbtree1-right rbt1)])
         (let loop ([i 0] [acc acc] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               acc
               (let ([acc (proc i (K n0) (V n0) (K n1) (V n1) acc)])
                 (loop (fx1+ i) acc (iter0) (iter1))))))]
      [(who proc acc rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-right rbt0)] [iter* (map single-step-rbtree1-right rbt*)])
         (let loop ([i 0] [acc acc] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               acc
               (let ([acc (apply proc i (K n0) (V n0) `(,@(kv* n*) ,acc))])
                 (loop (fx1+ i) acc (iter0) (map exe iter*))))))]))



;;;; for treeset (only keys)

  (define SV #f)


  (define rbtree1-andmap1
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbtree1-root rbt0)])
         (if (null-rbnode? n)
             #t
             (and (loop (L n))
                  (proc (K n))
                  (loop (R n)))))]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               #t
               (and (proc (K n0) (K n1))
                    (loop (iter0) (iter1))))))]
      [(who proc  rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               #t
               (and (apply proc (K n0) (k* n*))
                    (loop (iter0) (map exe iter*))))))]))


  (define rbtree1-ormap1
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbtree1-root rbt0)])
         (if (null-rbnode? n)
             #f
             (or (loop (L n))
                 (proc (K n))
                 (loop (R n)))))]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               #f
               (or (proc (K n0) (K n1))
                   (loop (iter0) (iter1))))))]
      [(who proc  rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               #f
               (or (apply proc (K n0) (k* n*))
                   (loop (iter0) (map exe iter*))))))]))


  (define rbtree1-map1
    (case-lambda
      [(who proc newrbt rbt0)
       (let loop ([n (rbtree1-root rbt0)])
         (unless (null-rbnode? n)
           (loop (L n))
           (let ([v (proc (K n))])
             (rbtree1-set! who newrbt v SV))
           (loop (R n))))
       newrbt]
      [(who proc newrbt rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               newrbt
               (let ([v (proc (K n0) (K n1))])
                 (rbtree1-set! who newrbt v SV)
                 (loop (iter0) (iter1))))))]
      [(who proc newrbt rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               newrbt
               (let ([v (apply proc (K n0) (k* n*))])
                 (rbtree1-set! who newrbt v SV)
                 (loop (iter0) (map exe iter*))))))]))


  (define rbtree1-map/i1
    (case-lambda
      [(who proc newrbt rbt0)
       (let loop ([n (rbtree1-root rbt0)] [i 0])
         (if (null-rbnode? n)
             i
             (let ([i (loop (L n) i)])
               (let ([v (proc i (K n))])
                 (rbtree1-set! who newrbt v SV))
               (loop (R n) (fx1+ i)))))
       newrbt]
      [(who proc newrbt rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([i 0] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               newrbt
               (let ([v (proc i (K n0) (K n1))])
                 (rbtree1-set! who newrbt v SV)
                 (loop (fx1+ i) (iter0) (iter1))))))]
      [(who proc newrbt rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([i 0] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               newrbt
               (let ([v (apply proc i (K n0) (k* n*))])
                 (rbtree1-set! who newrbt v SV)
                 (loop (fx1+ i) (iter0) (map exe iter*))))))]))


  (define rbtree1-for-each1
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbtree1-root rbt0)])
         (unless (null-rbnode? n)
           (loop (L n))
           (proc (K n))
           (loop (R n))))]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (unless (not (or n0 n1))
             (proc (K n0) (K n1))
             (loop (iter0) (iter1)))))]
      [(who proc rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (unless (not (or n0 (ormap id n*)))
             (apply proc (K n0) (k* n*))
             (loop (iter0) (map exe iter*)))))]))


  (define rbtree1-for-each/i1
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbtree1-root rbt0)] [i 0])
         (if (null-rbnode? n)
             i
             (let ([i (loop (L n) i)])
               (proc i (K n))
               (loop (R n) (fx1+ i)))))]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([i 0] [n0 (iter0)] [n1 (iter1)])
           (unless (not (or n0 n1))
             (proc i (K n0) (K n1))
             (loop (fx1+ i) (iter0) (iter1)))))]
      [(who proc rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([i 0] [n0 (iter0)] [n* (map exe iter*)])
           (unless (not (or n0 (ormap id n*)))
             (apply proc i (K n0) (k* n*))
             (loop (fx1+ i) (iter0) (map exe iter*)))))]))


  (define rbtree1-fold-left1
    (case-lambda
      [(who proc acc rbt0)
       (let loop ([n (rbtree1-root rbt0)] [acc acc])
         (if (null-rbnode? n)
             acc
             (let ([acc (loop (L n) acc)])
               (loop (R n) (proc acc (K n))))))]
      [(who proc acc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([acc acc] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               acc
               (let ([acc (proc acc (K n0) (K n1))])
                 (loop acc (iter0) (iter1))))))]
      [(who proc acc rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([acc acc] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               acc
               (let ([acc (apply proc acc (K n0) (k* n*))])
                 (loop acc (iter0) (map exe iter*))))))]))


  (define rbtree1-fold-left/i1
    (case-lambda
      [(who proc acc rbt0)
       (let-values ([(acc i)
                     (let loop ([n (rbtree1-root rbt0)] [acc acc] [i 0])
                       (if (null-rbnode? n)
                           (values acc i)
                           (let-values ([(acc i) (loop (L n) acc i)])
                             (loop (R n) (proc i acc (K n)) (fx1+ i)))))])
         acc)]
      [(who proc acc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter1 (single-step-rbtree1-left rbt1)])
         (let loop ([i 0] [acc acc] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               acc
               (let ([acc (proc i acc (K n0) (K n1))])
                 (loop (fx1+ i) acc (iter0) (iter1))))))]
      [(who proc acc rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-left rbt0)] [iter* (map single-step-rbtree1-left rbt*)])
         (let loop ([i 0] [acc acc] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               acc
               (let ([acc (apply proc i acc (K n0) (k* n*))])
                 (loop (fx1+ i) acc (iter0) (map exe iter*))))))]))


  (define rbtree1-fold-right1
    (case-lambda
      [(who proc acc rbt0)
       (let loop ([n (rbtree1-root rbt0)] [acc acc])
         (if (null-rbnode? n)
             acc
             (let ([acc (loop (R n) acc)])
               (loop (L n) (proc (K n) acc)))))]
      [(who proc acc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-right rbt0)] [iter1 (single-step-rbtree1-right rbt1)])
         (let loop ([acc acc] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               acc
               (let ([acc (proc (K n0) (K n1) acc)])
                 (loop acc (iter0) (iter1))))))]
      [(who proc acc rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-right rbt0)] [iter* (map single-step-rbtree1-right rbt*)])
         (let loop ([acc acc] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               acc
               (let ([acc (apply proc (K n0) `(,@(k* n*) ,acc))])
                 (loop acc (iter0) (map exe iter*))))))]))


  (define rbtree1-fold-right/i1
    (case-lambda
      [(who proc acc rbt0)
       (let-values ([(acc i)
                     (let loop ([n (rbtree1-root rbt0)] [acc acc] [i (fx1- (rbtree1-size rbt0))])
                       (if (null-rbnode? n)
                           (values acc i)
                           (let-values ([(acc i) (loop (R n) acc i)])
                             (loop (L n) (proc i (K n) acc) (fx1- i)))))])
         acc)]
      [(who proc acc rbt0 rbt1)
       (let ([iter0 (single-step-rbtree1-right rbt0)] [iter1 (single-step-rbtree1-right rbt1)])
         (let loop ([i 0] [acc acc] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               acc
               (let ([acc (proc i (K n0) (K n1) acc)])
                 (loop (fx1+ i) acc (iter0) (iter1))))))]
      [(who proc acc rbt0 . rbt*)
       (let ([iter0 (single-step-rbtree1-right rbt0)] [iter* (map single-step-rbtree1-right rbt*)])
         (let loop ([i 0] [acc acc] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               acc
               (let ([acc (apply proc i (K n0) `(,@(k* n*) ,acc))])
                 (loop (fx1+ i) acc (iter0) (map exe iter*))))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   conversions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define rbtree1->dot
    (lambda (T path)
      ;; if file exists, error
      (call-with-output-file path
        (lambda (p)
          (printf "dot file at ~a~n" path)
          (put-string p "digraph {")
          (fresh-line p)
          (put-string p "node [style=filled,color=black,fontcolor=white,fontname=monospace];")
          (fresh-line p)
          (let ([nodes '()])
            (let ([tree (rbtree1-root T)])
              (unless (null-rbnode? tree)
                (let loop ([tree tree])
                  (let ([left (L tree)]
                        [right (R tree)])
                    (set! nodes (cons tree nodes))
                    (if (eq? null-rbnode left)
                        (put-string p (format "~a -> ~a;~n" (K tree) "NIL"))
                        (begin (put-string p (format "~a -> ~a;~n" (K tree) (K left)))
                               (loop left)))
                    (if (eq? null-rbnode right)
                        (put-string p (format "~a -> ~a;~n" (K tree) "NIL"))
                        (begin (put-string p (format "~a -> ~a;~n" (K tree) (K right)))
                               (loop right)))))))
            ;; set node color
            (for-each (lambda (x)
                        (put-string p (format "~a [fillcolor=~a];~n"
                                              (K x)
                                              (if (RED? x) "red" "black")))) nodes)
            (put-string p "NIL [fillcolor=black];")
            (fresh-line p))
          (put-string p "}")))))


  #|doc
  Verify that the red-black tree meet all the properties:

  1. Every node is either red or black.
  2. The root is black.
  3. Every leaf (null-rbnode) is black.
  4. If a node is red, then both its children are black.
  5. For each node, all simple paths from the node to descendant leaves contain the same number of black nodes.
  |#
  (define-who $rbtree1-verify
    (lambda (tree)
      (let ([seen (make-eq-hashtable)] [count 0] [less? (rbtree1-<? tree)])
        (define walk
          (lambda (node parent lower? lower upper? upper)
            (if (null-rbnode? node)
                1
                (begin
                  (unless (and (vector? node) (fx= (vector-length node) 5))
                    (errorf who "invalid node layout"))
                  (when (hashtable-contains? seen node) (errorf who "cycle or shared child"))
                  (hashtable-set! seen node #t)
                  (unless (eq? (P node) parent) (errorf who "invalid parent link"))
                  (when (and lower? (not (less? lower (K node))))
                    (errorf who "key violates lower bound"))
                  (when (and upper? (not (less? (K node) upper)))
                    (errorf who "key violates upper bound"))
                  (when (rbtree1-fixnum-keys? tree)
                    (unless (fixnum? (K node)) (errorf who "non-fixnum key")))
                  (unless (or (RED? node) (BLACK? node)) (errorf who "invalid color"))
                  (when (and (RED? node) (or (RED? (L node)) (RED? (R node))))
                    (errorf who "red parent has red child"))
                  (set! count (fx1+ count))
                  (let ([left-height (walk (L node) node lower? lower #t (K node))]
                        [right-height (walk (R node) node #t (K node) upper? upper)])
                    (unless (fx= left-height right-height)
                      (errorf who "unequal black heights"))
                    (if (BLACK? node) (fx1+ left-height) left-height))))))
        (unless (BLACK? (rbtree1-root tree)) (errorf who "root is not black"))
        (walk (rbtree1-root tree) null-rbnode #f #f #f #f)
        (unless (fx= count (rbtree1-size tree)) (errorf who "incorrect size"))
        #t)))

  (record-type-equal-procedure
   (type-descriptor rbtree1)
   (lambda (rbt1 rbt2 =?)
     (let ([=?1 (rbtree1-=? rbt1)] [=?2 (rbtree1-=? rbt2)]
           [<?1 (rbtree1-<? rbt1)] [<?2 (rbtree1-<? rbt2)])
       (and (eq? =?1 =?2)
            (eq? <?1 <?2)
            (fx= (rbtree1-size rbt1) (rbtree1-size rbt2))
            ;; trees are equal if they contain the same kvs in order
            (let ([iter1 (single-step-rbtree1-left rbt1)] [iter2 (single-step-rbtree1-left rbt2)])
              (let loop ([n1 (iter1)] [n2 (iter2)])
                (if (not (or n1 n2))
                    #t
                    ;; compare keys using =?1, compare values using provided =?
                    (and (=?1 (K n1) (K n2))
                         (=?  (V n1) (V n2))
                         (loop (iter1) (iter2))))))))))

  )
