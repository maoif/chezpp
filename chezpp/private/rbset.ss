(library (chezpp private rbset)
  (export rbset make-rbset rbset-=? rbset-<?
          rbset-ref rbset-set! rbset-delete!
          rbset-clear! rbset-size
          rbset-contains? rbset-contains/p?
          rbset-search

          rbset-successor rbset-predecessor
          rbset-min rbset-max

          rbset-andmap rbset-ormap
          rbset-map rbset-map/i rbset-map! rbset-map/i!
          rbset-for-each rbset-for-each/i
          rbset-fold-left rbset-fold-left/i
          rbset-fold-right rbset-fold-right/i

          rbset-andmap1 rbset-ormap1
          rbset-map1 rbset-map/i1
          rbset-for-each1 rbset-for-each/i1
          rbset-fold-left1 rbset-fold-left/i1
          rbset-fold-right1 rbset-fold-right/i1

          rbset-visit rbset-visit-preorder rbset-visit-postorder rbset-visit-inorder

          $rbset-verify rbset->dot)
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


  (define-record-type (rbset mk-rbset rbset?)
    (nongenerative) (opaque #t)
    (fields (mutable root) (immutable =?) (immutable <?) (mutable size) (immutable fixnum-keys?))
    (protocol
     (lambda (new)
       (case-lambda
         [(=? <? size) (new null-rbnode =? <? size #f)]
         [(=? <? size fixnum-keys?) (new null-rbnode =? <? size fixnum-keys?)]))))


  (define make-rbset
    (case-lambda
      [(who =? <?) (mk-rbset =? <? 0 #f)]
      [(who =? <? fixnum-keys?) (mk-rbset =? <? 0 fixnum-keys?)]))


  (define rotate-left!
    (lambda (rbt p)
      (unless (null-rbnode? p)
        (let* ([r (R p)] [rL (L r)])
          (R! p rL)
          (unless (null-rbnode? rL) (P! rL p))
          (let ([pP (P p)])
            (P! r pP)
            (cond [(null-rbnode? pP) (rbset-root-set! rbt r)]
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
            (cond [(null-rbnode? pP) (rbset-root-set! rbt l)]
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


  (define rbset-ref
    (case-lambda
      [(who rbt k)
       (rbset-check-key who rbt k)
       (let ([=? (rbset-=? rbt)] [<? (rbset-<? rbt)])
         (let loop ([n (rbset-root rbt)])
           (if (null-rbnode? n)
               (errorf who "key not found: ~a" k)
               (cond [(=? k (K n)) (V n)]
                     [(<? k (K n)) (loop (L n))]
                     [else  (loop (R n))]))))]
      [(who rbt k default)
       (rbset-check-key who rbt k)
       (let ([=? (rbset-=? rbt)] [<? (rbset-<? rbt)])
         (let loop ([n (rbset-root rbt)])
           (if (null-rbnode? n)
               default
               (cond [(=? k (K n)) (V n)]
                     [(<? k (K n)) (loop (L n))]
                     [else  (loop (R n))]))))]))


  (define rbset-check-key
    (lambda (who tree key)
      (when (rbset-fixnum-keys? tree)
        (pcheck ([fixnum? key]) (void)))))

  (define rbset-set!
    (lambda (who tree key value)
      (if (rbset-fixnum-keys? tree)
          (pcheck ([fixnum? key]) (rbset-set/fixnum! who tree key value))
          (rbset-set/generic! who tree key value))))

  (define rbset-set/generic!
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
            (BLACK! (rbset-root rbt)))))

      (let ([root (rbset-root rbt)] [=? (rbset-=? rbt)] [<? (rbset-<? rbt)])
        ;; x: current node, y: parent of x
        (let loop ([x root] [y null-rbnode])
          (if (null-rbnode? x)
              ;; z is by default RED
              (let ([z (mk-rbnode k v y)])
                (cond [(null-rbnode? y) (rbset-root-set! rbt z)]
                      [(<? k (K y))     (L! y z)]
                      [else             (R! y z)])
                (fix! z)
                (rbset-size-set! rbt (fx1+ (rbset-size rbt))))
              (let ([kk (K x)])
                (cond [(=? k kk) (V! x v)]
                      [(<? k kk) (loop (L x) x)]
                      [else      (loop (R x) x)])))))))


  (define rbset-set/fixnum!
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
            (BLACK! (rbset-root rbt)))))

      (let ([root (rbset-root rbt)] [=? (rbset-=? rbt)] [<? (rbset-<? rbt)])
        ;; x: current node, y: parent of x
        (let loop ([x root] [y null-rbnode])
          (if (null-rbnode? x)
              ;; z is by default RED
              (let ([z (let ([node (vector 0 y null-rbnode null-rbnode RED)])
                         (vector-set-fixnum! node 0 k)
                         node)])
                (cond [(null-rbnode? y) (rbset-root-set! rbt z)]
                      [(<? k (K y))     (L! y z)]
                      [else             (R! y z)])
                (fix! z)
                (rbset-size-set! rbt (fx1+ (rbset-size rbt))))
              (let ([kk (K x)])
                (cond [(=? k kk) (V! x v)]
                      [(<? k kk) (loop (L x) x)]
                      [else      (loop (R x) x)])))))))


  (define rbset-delete!
    (lambda (who tree key)
      (if (rbset-fixnum-keys? tree)
          (pcheck ([fixnum? key]) (rbset-delete/fixnum! who tree key))
          (rbset-delete/generic! who tree key))))

  (define rbset-delete/generic!
    (lambda (who rbt k)
      (define fix!
        (lambda (x)
          (let loop ([x x])
            (if (and (not (eq? x (rbset-root rbt))) (BLACK? x))
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
                            (loop (rbset-root rbt)))))
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
                            (loop (rbset-root rbt))))))
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
               [(null-rbnode? (P p)) (rbset-root-set! rbt replacement)]
               [(eq? p (L (P p)))    (L! (P p) replacement)]
               [else                 (R! (P p) replacement)])
              (L! p null-rbnode)
              (R! p null-rbnode)
              (P! p null-rbnode)
              (when (BLACK? p) (fix! replacement))]
             [(null-rbnode? (P p))
              (rbset-root-set! rbt null-rbnode)]
             [else (when (BLACK? p) (fix! p))
                   (unless (null-rbnode? (P p))
                     (cond [(eq? p (L (P p)))
                            (L! (P p) null-rbnode)]
                           [(eq? p (R (P p)))
                            (R! (P p) null-rbnode)]
                           [else (assert-unreachable)])
                     (P! p null-rbnode))]))))

      (let ([root (rbset-root rbt)] [=? (rbset-=? rbt)] [<? (rbset-<? rbt)])
        (let loop ([x root])
          (if (null-rbnode? x)
              (errorf who "key not found: ~a" k)
              (let ([kk (K x)])
                (cond [(=? k kk)
                       (delete! x)
                       (rbset-size-set! rbt (fx1- (rbset-size rbt)))]
                      [(<? k kk) (loop (L x))]
                      [else      (loop (R x))])))))))


  (define rbset-delete/fixnum!
    (lambda (who rbt k)
      (define fix!
        (lambda (x)
          (let loop ([x x])
            (if (and (not (eq? x (rbset-root rbt))) (BLACK? x))
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
                            (loop (rbset-root rbt)))))
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
                            (loop (rbset-root rbt))))))
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
               [(null-rbnode? (P p)) (rbset-root-set! rbt replacement)]
               [(eq? p (L (P p)))    (L! (P p) replacement)]
               [else                 (R! (P p) replacement)])
              (L! p null-rbnode)
              (R! p null-rbnode)
              (P! p null-rbnode)
              (when (BLACK? p) (fix! replacement))]
             [(null-rbnode? (P p))
              (rbset-root-set! rbt null-rbnode)]
             [else (when (BLACK? p) (fix! p))
                   (unless (null-rbnode? (P p))
                     (cond [(eq? p (L (P p)))
                            (L! (P p) null-rbnode)]
                           [(eq? p (R (P p)))
                            (R! (P p) null-rbnode)]
                           [else (assert-unreachable)])
                     (P! p null-rbnode))]))))

      (let ([root (rbset-root rbt)] [=? (rbset-=? rbt)] [<? (rbset-<? rbt)])
        (let loop ([x root])
          (if (null-rbnode? x)
              (errorf who "key not found: ~a" k)
              (let ([kk (K x)])
                (cond [(=? k kk)
                       (delete! x)
                       (rbset-size-set! rbt (fx1- (rbset-size rbt)))]
                      [(<? k kk) (loop (L x))]
                      [else      (loop (R x))])))))))


  (define rbset-clear!
    (lambda (who rbt)
      (rbset-root-set! rbt null-rbnode)
      (rbset-size-set! rbt 0)))


  (define rbset-contains?
    (lambda (who rbt k)
      (rbset-check-key who rbt k)
      (let ([=? (rbset-=? rbt)] [<? (rbset-<? rbt)])
        (let loop ([n (rbset-root rbt)])
          (if (null-rbnode? n)
              #f
              (cond [(=? k (K n)) #t]
                    [(<? k (K n)) (loop (L n))]
                    [else  (loop (R n))]))))))


  (define rbset-contains/p?
    (lambda (who rbt pred)
      (let ([=? (rbset-=? rbt)] [<? (rbset-<? rbt)])
        (let loop ([n (rbset-root rbt)])
          (if (null-rbnode? n)
              #f
              (or (bool (pred (K n) (V n)))
                  (loop (L n))
                  (loop (R n))))))))


  (define rbset-search
    (lambda (who rbt pred)
      (let loop ([n (rbset-root rbt)])
        (if (null-rbnode? n)
            #f
            (or (let ([k (K n)] [v (V n)])
                  (if (pred k v) (cons k v) #f))
                (loop (L n))
                (loop (R n)))))))


  (define rbset-successor
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
      (rbset-check-key who rbt k)
      (let ([=? (rbset-=? rbt)] [<? (rbset-<? rbt)])
        (let loop ([n (rbset-root rbt)])
          (if (null-rbnode? n)
              (errorf who "key not found: ~a" k)
              (cond [(=? k (K n)) (let ([n (successor n)])
                                    (if n (cons (K n) (V n)) n))]
                    [(<? k (K n)) (loop (L n))]
                    [else  (loop (R n))]))))))


  (define rbset-predecessor
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
      (rbset-check-key who rbt k)
      (let ([=? (rbset-=? rbt)] [<? (rbset-<? rbt)])
        (let loop ([n (rbset-root rbt)])
          (if (null-rbnode? n)
              (errorf who "key not found: ~a" k)
              (cond [(=? k (K n)) (let ([n (predecessor n)])
                                    (if n (cons (K n) (V n)) n))]
                    [(<? k (K n)) (loop (L n))]
                    [else  (loop (R n))]))))))


  (define rbset-min
    (lambda (who rbt)
      (let ([root (rbset-root rbt)])
        (if (null-rbnode? root)
            #f
            (let ([n (minimum root)])
              (cons (K n) (V n)))))))


  (define rbset-max
    (lambda (who rbt)
      (let ([root (rbset-root rbt)])
        (if (null-rbnode? root)
            #f
            (let ([n (maximum root)])
              (cons (K n) (V n)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   iterations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define rbset-visit-preorder
    (lambda (who proc rbt)
      (let loop ([n (rbset-root rbt)])
        (unless (null-rbnode? n)
          (proc (K n) (V n))
          (loop (L n))
          (loop (R n))))))


  (define rbset-visit-postorder
    (lambda (who proc rbt)
      (let loop ([n (rbset-root rbt)])
        (unless (null-rbnode? n)
          (loop (L n))
          (loop (R n))
          (proc (K n) (V n))))))


  (define rbset-visit-inorder
    (lambda (who proc rbt)
      (let loop ([n (rbset-root rbt)])
        (unless (null-rbnode? n)
          (loop (L n))
          (proc (K n) (V n))
          (loop (R n))))))

  (define rbset-visit rbset-visit-inorder)


  ;; return a procedure that when called, either return a node in order,
  ;; or #f if all nodes are visited
  (define single-step-rbset-left
    (lambda (rbt)
      ;; stack: '((n1 L) (n2 R) ...),
      ;; records the remainder of the tree to be visited
      (let ([stack '()])
        ;; locate the first node
        (let ([n (rbset-root rbt)])
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
  (define single-step-rbset-right
    (lambda (rbt)
      (let ([stack '()])
        (let ([n (rbset-root rbt)])
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

  (define rbset-andmap
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbset-root rbt0)])
         (if (null-rbnode? n)
             #t
             (and (loop (L n))
                  (proc (K n) (V n))
                  (loop (R n)))))]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               #t
               (and (proc (K n0) (V n0) (K n1) (V n1))
                    (loop (iter0) (iter1))))))]
      [(who proc  rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               #t
               (and (apply proc (K n0) (V n0) (kv* n*))
                    (loop (iter0) (map exe iter*))))))]))


  (define rbset-ormap
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbset-root rbt0)])
         (if (null-rbnode? n)
             #f
             (or (loop (L n))
                 (proc (K n) (V n))
                 (loop (R n)))))]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               #f
               (or (proc (K n0) (V n0) (K n1) (V n1))
                   (loop (iter0) (iter1))))))]
      [(who proc  rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               #f
               (or (apply proc (K n0) (V n0) (kv* n*))
                   (loop (iter0) (map exe iter*))))))]))


  ;; newrbt: the new tree to be returned
  (define rbset-map
    (case-lambda
      [(who proc newrbt rbt0)
       (let loop ([n (rbset-root rbt0)])
         (unless (null-rbnode? n)
           (loop (L n))
           (let-values ([(k v) (proc (K n) (V n))])
             (rbset-set! who newrbt k v))
           (loop (R n))))
       newrbt]
      [(who proc newrbt rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               newrbt
               (let-values ([(k v) (proc (K n0) (V n0) (K n1) (V n1))])
                 (rbset-set! who newrbt k v)
                 (loop (iter0) (iter1))))))]
      [(who proc newrbt rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               newrbt
               (let-values ([(k v) (apply proc (K n0) (V n0) (kv* n*))])
                 (rbset-set! who newrbt k v)
                 (loop (iter0) (map exe iter*))))))]))


  (define rbset-map/i
    (case-lambda
      [(who proc newrbt rbt0)
       (let loop ([n (rbset-root rbt0)] [i 0])
         (if (null-rbnode? n)
             i
             (let ([i (loop (L n) i)])
               (let-values ([(k v) (proc i (K n) (V n))])
                 (rbset-set! who newrbt k v))
               (loop (R n) (fx1+ i)))))
       newrbt]
      [(who proc newrbt rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([i 0] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               newrbt
               (let-values ([(k v) (proc i (K n0) (V n0) (K n1) (V n1))])
                 (rbset-set! who newrbt k v)
                 (loop (fx1+ i) (iter0) (iter1))))))]
      [(who proc newrbt rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([i 0] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               newrbt
               (let-values ([(k v) (apply proc i (K n0) (V n0) (kv* n*))])
                 (rbset-set! who newrbt k v)
                 (loop (fx1+ i) (iter0) (map exe iter*))))))]))


  (define rbset-map!
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbset-root rbt0)])
         (unless (null-rbnode? n)
           (loop (L n))
           (V! n (proc (K n) (V n)))
           (loop (R n))))
       rbt0]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (unless (not (or n0 n1))
             (let ([v (proc (K n0) (V n0) (K n1) (V n1))])
               (V! n0 v)
               (loop (iter0) (iter1))))))
       rbt0]
      [(who proc rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (unless (not (or n0 (ormap id n*)))
             (let ([v (apply proc (K n0) (V n0) (kv* n*))])
               (V! n0 v)
               (loop (iter0) (map exe iter*))))))
       rbt0]))


  (define rbset-map/i!
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbset-root rbt0)] [i 0])
         (if (null-rbnode? n)
             i
             (let ([i (loop (L n) i)])
               (V! n (proc i (K n) (V n)))
               (loop (R n) (fx1+ i)))))
       rbt0]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([i 0] [n0 (iter0)] [n1 (iter1)])
           (unless (not (or n0 n1))
             (let ([v (proc i (K n0) (V n0) (K n1) (V n1))])
               (V! n0 v)
               (loop (fx1+ i) (iter0) (iter1))))))
       rbt0]
      [(who proc rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([i 0] [n0 (iter0)] [n* (map exe iter*)])
           (unless (not (or n0 (ormap id n*)))
             (let ([v (apply proc i (K n0) (V n0) (kv* n*))])
               (V! n0 v)
               (loop (fx1+ i) (iter0) (map exe iter*))))))
       rbt0]))


  (define rbset-for-each
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbset-root rbt0)])
         (unless (null-rbnode? n)
           (loop (L n))
           (proc (K n) (V n))
           (loop (R n))))]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (unless (not (or n0 n1))
             (proc (K n0) (V n0) (K n1) (V n1))
             (loop (iter0) (iter1)))))]
      [(who proc rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (unless (not (or n0 (ormap id n*)))
             (apply proc (K n0) (V n0) (kv* n*))
             (loop (iter0) (map exe iter*)))))]))


  (define rbset-for-each/i
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbset-root rbt0)] [i 0])
         (if (null-rbnode? n)
             i
             (let ([i (loop (L n) i)])
               (proc i (K n) (V n))
               (loop (R n) (fx1+ i)))))]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([i 0] [n0 (iter0)] [n1 (iter1)])
           (unless (not (or n0 n1))
             (proc i (K n0) (V n0) (K n1) (V n1))
             (loop (fx1+ i) (iter0) (iter1)))))]
      [(who proc rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([i 0] [n0 (iter0)] [n* (map exe iter*)])
           (unless (not (or n0 (ormap id n*)))
             (apply proc i (K n0) (V n0) (kv* n*))
             (loop (fx1+ i) (iter0) (map exe iter*)))))]))


  (define rbset-fold-left
    (case-lambda
      [(who proc acc rbt0)
       (let loop ([n (rbset-root rbt0)] [acc acc])
         (if (null-rbnode? n)
             acc
             (let ([acc (loop (L n) acc)])
               (loop (R n) (proc acc (K n) (V n))))))]
      [(who proc acc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([acc acc] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               acc
               (let ([acc (proc acc (K n0) (V n0) (K n1) (V n1))])
                 (loop acc (iter0) (iter1))))))]
      [(who proc acc rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([acc acc] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               acc
               (let ([acc (apply proc acc (K n0) (V n0) (kv* n*))])
                 (loop acc (iter0) (map exe iter*))))))]))


  (define rbset-fold-left/i
    (case-lambda
      [(who proc acc rbt0)
       (let-values ([(acc i)
                     (let loop ([n (rbset-root rbt0)] [acc acc] [i 0])
                       (if (null-rbnode? n)
                           (values acc i)
                           (let-values ([(acc i) (loop (L n) acc i)])
                             (loop (R n) (proc i acc (K n) (V n)) (fx1+ i)))))])
         acc)]
      [(who proc acc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([i 0] [acc acc] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               acc
               (let ([acc (proc i acc (K n0) (V n0) (K n1) (V n1))])
                 (loop (fx1+ i) acc (iter0) (iter1))))))]
      [(who proc acc rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([i 0] [acc acc] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               acc
               (let ([acc (apply proc i acc (K n0) (V n0) (kv* n*))])
                 (loop (fx1+ i) acc (iter0) (map exe iter*))))))]))


  (define rbset-fold-right
    (case-lambda
      [(who proc acc rbt0)
       (let loop ([n (rbset-root rbt0)] [acc acc])
         (if (null-rbnode? n)
             acc
             (let ([acc (loop (R n) acc)])
               (loop (L n) (proc (K n) (V n) acc)))))]
      [(who proc acc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-right rbt0)] [iter1 (single-step-rbset-right rbt1)])
         (let loop ([acc acc] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               acc
               (let ([acc (proc (K n0) (V n0) (K n1) (V n1) acc)])
                 (loop acc (iter0) (iter1))))))]
      [(who proc acc rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-right rbt0)] [iter* (map single-step-rbset-right rbt*)])
         (let loop ([acc acc] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               acc
               (let ([acc (apply proc (K n0) (V n0) `(,@(kv* n*) ,acc))])
                 (loop acc (iter0) (map exe iter*))))))]))


  (define rbset-fold-right/i
    (case-lambda
      [(who proc acc rbt0)
       (let-values ([(acc i)
                     (let loop ([n (rbset-root rbt0)] [acc acc] [i (fx1- (rbset-size rbt0))])
                       (if (null-rbnode? n)
                           (values acc i)
                           (let-values ([(acc i) (loop (R n) acc i)])
                             (loop (L n) (proc i (K n) (V n) acc) (fx1- i)))))])
         acc)]
      [(who proc acc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-right rbt0)] [iter1 (single-step-rbset-right rbt1)])
         (let loop ([i 0] [acc acc] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               acc
               (let ([acc (proc i (K n0) (V n0) (K n1) (V n1) acc)])
                 (loop (fx1+ i) acc (iter0) (iter1))))))]
      [(who proc acc rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-right rbt0)] [iter* (map single-step-rbset-right rbt*)])
         (let loop ([i 0] [acc acc] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               acc
               (let ([acc (apply proc i (K n0) (V n0) `(,@(kv* n*) ,acc))])
                 (loop (fx1+ i) acc (iter0) (map exe iter*))))))]))



;;;; for treeset (only keys)

  (define SV #f)


  (define rbset-andmap1
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbset-root rbt0)])
         (if (null-rbnode? n)
             #t
             (and (loop (L n))
                  (proc (K n))
                  (loop (R n)))))]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               #t
               (and (proc (K n0) (K n1))
                    (loop (iter0) (iter1))))))]
      [(who proc  rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               #t
               (and (apply proc (K n0) (k* n*))
                    (loop (iter0) (map exe iter*))))))]))


  (define rbset-ormap1
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbset-root rbt0)])
         (if (null-rbnode? n)
             #f
             (or (loop (L n))
                 (proc (K n))
                 (loop (R n)))))]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               #f
               (or (proc (K n0) (K n1))
                   (loop (iter0) (iter1))))))]
      [(who proc  rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               #f
               (or (apply proc (K n0) (k* n*))
                   (loop (iter0) (map exe iter*))))))]))


  (define rbset-map1
    (case-lambda
      [(who proc newrbt rbt0)
       (let loop ([n (rbset-root rbt0)])
         (unless (null-rbnode? n)
           (loop (L n))
           (let ([v (proc (K n))])
             (rbset-set! who newrbt v SV))
           (loop (R n))))
       newrbt]
      [(who proc newrbt rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               newrbt
               (let ([v (proc (K n0) (K n1))])
                 (rbset-set! who newrbt v SV)
                 (loop (iter0) (iter1))))))]
      [(who proc newrbt rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               newrbt
               (let ([v (apply proc (K n0) (k* n*))])
                 (rbset-set! who newrbt v SV)
                 (loop (iter0) (map exe iter*))))))]))


  (define rbset-map/i1
    (case-lambda
      [(who proc newrbt rbt0)
       (let loop ([n (rbset-root rbt0)] [i 0])
         (if (null-rbnode? n)
             i
             (let ([i (loop (L n) i)])
               (let ([v (proc i (K n))])
                 (rbset-set! who newrbt v SV))
               (loop (R n) (fx1+ i)))))
       newrbt]
      [(who proc newrbt rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([i 0] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               newrbt
               (let ([v (proc i (K n0) (K n1))])
                 (rbset-set! who newrbt v SV)
                 (loop (fx1+ i) (iter0) (iter1))))))]
      [(who proc newrbt rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([i 0] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               newrbt
               (let ([v (apply proc i (K n0) (k* n*))])
                 (rbset-set! who newrbt v SV)
                 (loop (fx1+ i) (iter0) (map exe iter*))))))]))


  (define rbset-for-each1
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbset-root rbt0)])
         (unless (null-rbnode? n)
           (loop (L n))
           (proc (K n))
           (loop (R n))))]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([n0 (iter0)] [n1 (iter1)])
           (unless (not (or n0 n1))
             (proc (K n0) (K n1))
             (loop (iter0) (iter1)))))]
      [(who proc rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([n0 (iter0)] [n* (map exe iter*)])
           (unless (not (or n0 (ormap id n*)))
             (apply proc (K n0) (k* n*))
             (loop (iter0) (map exe iter*)))))]))


  (define rbset-for-each/i1
    (case-lambda
      [(who proc rbt0)
       (let loop ([n (rbset-root rbt0)] [i 0])
         (if (null-rbnode? n)
             i
             (let ([i (loop (L n) i)])
               (proc i (K n))
               (loop (R n) (fx1+ i)))))]
      [(who proc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([i 0] [n0 (iter0)] [n1 (iter1)])
           (unless (not (or n0 n1))
             (proc i (K n0) (K n1))
             (loop (fx1+ i) (iter0) (iter1)))))]
      [(who proc rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([i 0] [n0 (iter0)] [n* (map exe iter*)])
           (unless (not (or n0 (ormap id n*)))
             (apply proc i (K n0) (k* n*))
             (loop (fx1+ i) (iter0) (map exe iter*)))))]))


  (define rbset-fold-left1
    (case-lambda
      [(who proc acc rbt0)
       (let loop ([n (rbset-root rbt0)] [acc acc])
         (if (null-rbnode? n)
             acc
             (let ([acc (loop (L n) acc)])
               (loop (R n) (proc acc (K n))))))]
      [(who proc acc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([acc acc] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               acc
               (let ([acc (proc acc (K n0) (K n1))])
                 (loop acc (iter0) (iter1))))))]
      [(who proc acc rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([acc acc] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               acc
               (let ([acc (apply proc acc (K n0) (k* n*))])
                 (loop acc (iter0) (map exe iter*))))))]))


  (define rbset-fold-left/i1
    (case-lambda
      [(who proc acc rbt0)
       (let-values ([(acc i)
                     (let loop ([n (rbset-root rbt0)] [acc acc] [i 0])
                       (if (null-rbnode? n)
                           (values acc i)
                           (let-values ([(acc i) (loop (L n) acc i)])
                             (loop (R n) (proc i acc (K n)) (fx1+ i)))))])
         acc)]
      [(who proc acc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter1 (single-step-rbset-left rbt1)])
         (let loop ([i 0] [acc acc] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               acc
               (let ([acc (proc i acc (K n0) (K n1))])
                 (loop (fx1+ i) acc (iter0) (iter1))))))]
      [(who proc acc rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-left rbt0)] [iter* (map single-step-rbset-left rbt*)])
         (let loop ([i 0] [acc acc] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               acc
               (let ([acc (apply proc i acc (K n0) (k* n*))])
                 (loop (fx1+ i) acc (iter0) (map exe iter*))))))]))


  (define rbset-fold-right1
    (case-lambda
      [(who proc acc rbt0)
       (let loop ([n (rbset-root rbt0)] [acc acc])
         (if (null-rbnode? n)
             acc
             (let ([acc (loop (R n) acc)])
               (loop (L n) (proc (K n) acc)))))]
      [(who proc acc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-right rbt0)] [iter1 (single-step-rbset-right rbt1)])
         (let loop ([acc acc] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               acc
               (let ([acc (proc (K n0) (K n1) acc)])
                 (loop acc (iter0) (iter1))))))]
      [(who proc acc rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-right rbt0)] [iter* (map single-step-rbset-right rbt*)])
         (let loop ([acc acc] [n0 (iter0)] [n* (map exe iter*)])
           (if (not (or n0 (ormap id n*)))
               acc
               (let ([acc (apply proc (K n0) `(,@(k* n*) ,acc))])
                 (loop acc (iter0) (map exe iter*))))))]))


  (define rbset-fold-right/i1
    (case-lambda
      [(who proc acc rbt0)
       (let-values ([(acc i)
                     (let loop ([n (rbset-root rbt0)] [acc acc] [i (fx1- (rbset-size rbt0))])
                       (if (null-rbnode? n)
                           (values acc i)
                           (let-values ([(acc i) (loop (R n) acc i)])
                             (loop (L n) (proc i (K n) acc) (fx1- i)))))])
         acc)]
      [(who proc acc rbt0 rbt1)
       (let ([iter0 (single-step-rbset-right rbt0)] [iter1 (single-step-rbset-right rbt1)])
         (let loop ([i 0] [acc acc] [n0 (iter0)] [n1 (iter1)])
           (if (not (or n0 n1))
               acc
               (let ([acc (proc i (K n0) (K n1) acc)])
                 (loop (fx1+ i) acc (iter0) (iter1))))))]
      [(who proc acc rbt0 . rbt*)
       (let ([iter0 (single-step-rbset-right rbt0)] [iter* (map single-step-rbset-right rbt*)])
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


  (define rbset->dot
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
            (let ([tree (rbset-root T)])
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
  (define-who $rbset-verify
    (lambda (tree)
      (let ([seen (make-eq-hashtable)] [count 0] [less? (rbset-<? tree)])
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
                  (when (rbset-fixnum-keys? tree)
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
        (unless (BLACK? (rbset-root tree)) (errorf who "root is not black"))
        (walk (rbset-root tree) null-rbnode #f #f #f #f)
        (unless (fx= count (rbset-size tree)) (errorf who "incorrect size"))
        #t)))

  (record-type-equal-procedure
   (type-descriptor rbset)
   (lambda (rbt1 rbt2 =?)
     (let ([=?1 (rbset-=? rbt1)] [=?2 (rbset-=? rbt2)]
           [<?1 (rbset-<? rbt1)] [<?2 (rbset-<? rbt2)])
       (and (eq? =?1 =?2)
            (eq? <?1 <?2)
            (fx= (rbset-size rbt1) (rbset-size rbt2))
            ;; trees are equal if they contain the same kvs in order
            (let ([iter1 (single-step-rbset-left rbt1)] [iter2 (single-step-rbset-left rbt2)])
              (let loop ([n1 (iter1)] [n2 (iter2)])
                (if (not (or n1 n2))
                    #t
                    ;; compare keys using =?1, compare values using provided =?
                    (and (=?1 (K n1) (K n2))
                         (=?  (V n1) (V n2))
                         (loop (iter1) (iter2))))))))))

  )
