(import (chezpp treemap)
        (chezpp list)
        (chezpp vector)
        (chezpp private rbtree))


;; TODO errors

(define v10     (fxvshuffle! (fxviota 10)))
(define v100    (fxvshuffle! (fxviota 100)))
(define v1000   (fxvshuffle! (fxviota 1000)))
(define v10000  (fxvshuffle! (fxviota 10000)))
(define v20000  (fxvshuffle! (fxviota 10000)))
(define v100000  (fxvshuffle! (fxviota 100000)))

(define tm10 (apply treemap = < (map cons (iota 10) (iota 10))))



(mat rbtree-verify

     (let ([tm (make-treemap fx= fx<)])
       (= 0 (treemap-size tm)))


     (let ([tm (make-treemap fx= fx<)])
       (fxvfor-each (lambda (x)
                      (treemap-set! tm x x)
                      (rbtree-verify tm))
                    v10000)
       (and (= (treemap-size tm) (fxvector-length v10000))
            (fxvandmap (lambda (x) (= x (treemap-ref tm x))) v10000)))
     ;; again
     (let ([tm (make-treemap fx= fx<)])
       (fxvshuffle! v10000)
       (fxvfor-each (lambda (x)
                      (treemap-set! tm x x)
                      (rbtree-verify tm))
                    v10000)
       (and (= (treemap-size tm) (fxvector-length v10000))
            (fxvandmap (lambda (x) (= x (treemap-ref tm x))) v10000)))

     (begin (define run! (lambda (v)
                           (let ([tm (make-treemap fx= fx<)])
                             (fxvfor-each (lambda (x)
                                            (treemap-set! tm x x)
                                            (rbtree-verify tm))
                                          v)
                             (fxvfor-each (lambda (x)
                                            (when (odd? x)
                                              (treemap-delete! tm x)
                                              (rbtree-verify tm)))
                                          v)
                             (fxvfor-each (lambda (x)
                                            (when (odd? x)
                                              (treemap-set! tm x x)
                                              (rbtree-verify tm)))
                                          v)
                             (fxvfor-each (lambda (x)
                                            (when (even? x)
                                              (treemap-delete! tm x)
                                              (rbtree-verify tm)))
                                          v)
                             (fxvfor-each (lambda (x)
                                            (when (even? x)
                                              (treemap-set! tm x x)
                                              (rbtree-verify tm)))
                                          v)
                             (and (= (treemap-size tm) (fxvector-length v))
                                  (fxvandmap (lambda (x) (= x (treemap-ref tm x))) v)))))
            #t)

     (run! v10000)
     ;; this is slower
     ;;(run! v20000)

     )


(mat treemap-equal?

     (let ([tm (apply treemap = < (map cons (iota 10) (iota 10)))])
       (and (not (eq? tm tm10))
            (equal? tm tm10)))

     (let ([tm1 (make-treemap fx= fx<)] [tm2 (make-treemap fx= fx<)])
       (fxvfor-each (lambda (x) (treemap-set! tm1 x x)) v100000)
       (fxvfor-each (lambda (x) (treemap-set! tm2 x x)) (fxvshuffle! v100000))
       (equal? tm1 tm2))

     ;; sizes differ
     (let ([tm15 (apply treemap = < (map cons (iota 15) (iota 15)))])
       (not (equal? tm10 tm15)))

     ;; procs differ
     (let ([tm (apply treemap = > (map cons (iota 10) (iota 10)))])
       (not (equal? tm tm10)))
     (let ([tm (apply treemap fx= < (map cons (iota 10) (iota 10)))])
       (not (equal? tm tm10)))

     )


(mat treemap-set!

     (let ([tm (make-treemap fx= fx<)])
       (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100)
       (and (= (treemap-size tm) (fxvector-length v100))
            (fxvandmap (lambda (x) (= x (treemap-ref tm x))) v100)))

     (let ([tm (make-treemap fx= fx<)])
       (fxvfor-each (lambda (x) (treemap-set! tm x x)) v10000)
       (and (= (treemap-size tm) (fxvector-length v10000))
            (fxvandmap (lambda (x) (= x (treemap-ref tm x))) v10000)))

     (let ([tm (make-treemap fx= fx<)])
       (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100000)
       (and (= (treemap-size tm) (fxvector-length v100000))
            (fxvandmap (lambda (x) (= x (treemap-ref tm x))) v10000)))

     )


(mat treemap-delete!


     (let ([tm (make-treemap fx= fx<)])
       (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100)
       (fxvfor-each (lambda (x) (when (odd? x) (treemap-delete! tm x))) v100)
       (and (= (treemap-size tm) (fx/ (fxvector-length v100) 2))
            (fxvandmap (lambda (x) (if (even? x) (= x (treemap-ref tm x)) #t))
                       v100)))

     (let ([tm (make-treemap fx= fx<)])
       (fxvfor-each (lambda (x) (treemap-set! tm x x)) v10000)
       (fxvfor-each (lambda (x) (when (odd? x) (treemap-delete! tm x))) v10000)
       (and (= (treemap-size tm) (fx/ (fxvector-length v10000) 2))
            (fxvandmap (lambda (x) (if (even? x) (= x (treemap-ref tm x)) #t))
                       v10000)))

     (let ([tm (make-treemap fx= fx<)])
       (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100000)
       (fxvfor-each (lambda (x) (when (odd? x) (treemap-delete! tm x))) v100000)
       (and (= (treemap-size tm) (fx/ (fxvector-length v100000) 2))
            (fxvandmap (lambda (x) (if (even? x) (= x (treemap-ref tm x)) #t))
                       v100000)))
     )


(mat treemap-search

     

     #t)


(mat treemap-search*

     (error? (treemap-search* 'bla 'bla))
     (error? (treemap-search* (treemap = <) 'bla))

     (begin (define n100 (iota 100))
            (define evens (nums 0 100 2))
            (define odds  (nums 1 100 2))
            (define pn100 (zip n100 n100))
            (define pevens (zip evens evens))
            (define podds  (zip odds odds))
            #t)

     (let ([tm (apply treemap = < pn100)])
       (and (equal? (treemap-search* tm (lambda (k v) (odd? k)))
                    podds)
            (equal? (treemap-search* tm (lambda (k v) (even? k)))
                    pevens)))


     (error? (treemap-search* 'bla 'bla 'bla))
     (error? (treemap-search* (treemap = <) odd? 'bla))

     (let ([tm (apply treemap = < pn100)]
           [tm1 (treemap = <)] [tm2 (treemap = <)])
       (treemap-search* tm (lambda (k v) (odd? k))  (lambda (k v) (treemap-set! tm1 k v)))
       (treemap-search* tm (lambda (k v) (even? k)) (lambda (k v) (treemap-set! tm2 k v)))
       (and (equal? tm1 (apply treemap = < podds))
            (equal? tm2 (apply treemap = < pevens))))

     )


(mat treemap-contains?
     #t)


(mat treemap-contains/p?
     #t)


(mat treemap-keys

     #t
     )


(mat treemap-values

     #t
     )


(mat treemap-cells

     #t
     )


(mat treemap-max/min

     (begin (define tm0 (let ([tm (make-treemap fx= fx<)])
                          (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100000)
                          tm))
            #t)

     (= (fxvmin v100000)
        (car (treemap-min tm0)))

     (= (fxvmax v100000)
        (car (treemap-max tm0)))
     )


(mat treemap-successor

     (let* ([ls (iota 1000)] [tm (apply treemap = < (map cons ls ls))])
       (andmap (lambda (x) (fx= (car (treemap-successor tm x)) (fx1+ x))) (iota 999)))

     )


(mat treemap-predecessor

     (let* ([ls (iota 1000)] [tm (apply treemap = < (map cons ls ls))])
       (andmap (lambda (x) (fx= (car (treemap-predecessor tm x)) (fx1- x))) (cdr ls)))

     )


(mat treemap-filter

     (let ([tm (make-treemap fx= fx<)])
       (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100000)
       (vandmap odd? (treemap-keys (treemap-filter (lambda (k v) (odd? k)) tm))))

     (let ([tm (make-treemap fx= fx<)])
       (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100000)
       (vandmap odd? (treemap-values (treemap-filter (lambda (k v) (odd? v)) tm))))

     )


(mat treemap-filter!

     (let ([tm (make-treemap fx= fx<)])
       (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100000)
       (treemap-filter! (lambda (k v) (odd? k)) tm)
       (vandmap odd? (treemap-keys tm)))

     (let ([tm (make-treemap fx= fx<)])
       (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100000)
       (treemap-filter! (lambda (k v) (odd? v)) tm)
       (vandmap odd? (treemap-values tm)))

     )


(mat treemap-partition

     (let* ([ls (iota 1000)] [odds (filter odd? ls)] [evens (filter even? ls)]
            [tm (apply treemap = < (map cons ls ls))]
            [tm-odd (apply treemap = < (map cons odds odds))]
            [tm-even (apply treemap = < (map cons evens evens))])
       (let-values ([(o e) (treemap-partition (lambda (k v) (odd? k)) tm)])
         (and (equal? o tm-odd)
              (equal? e tm-even))))

     )


(mat treemap->list

     (let* ([ls (iota 100)]
            [lsls (map cons ls ls)]
            [tm (apply treemap = < lsls)])
       (equal? lsls
               (treemap->list tm 'in)))

     )



;; iterations
;; test index
;; TODO errors


(mat treemap-map


     (not (equal? (treemap-map (lambda (k v) (values k v)) (treemap = < '((1 . 1))))
                  (treemap-map (lambda (k v) (values k v)) (treemap = <))))


     (equal? (treemap-map (lambda (k v) (values k v)) (treemap = <))
             (treemap-map (lambda (k v) (values k v)) (treemap = <)))

     (begin (define tm0 (let ([tm (make-treemap fx= fx<)])
                          (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100000)
                          tm))
            (define tm1 (let ([tm (make-treemap fx= fx>)])
                          (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100000)
                          tm))
            #t)

     ;; one tree
     (equal? (treemap-map (lambda (k v) (values k v))
                          tm0)
             tm0)

     (equal? (treemap-map (lambda (k v) (values (fx1+ k) (fx1+ v)))
                          tm0)
             (let ([tm (make-treemap fx= fx<)])
               (fxvfor-each (lambda (x) (treemap-set! tm (fx1+ x) (fx1+ x))) v100000)
               tm))


     ;; two trees
     (equal? (treemap-map (lambda (k v kk vv) (values (fx+ k kk) (fx+ v vv)))
                          tm0 tm1)
             (apply treemap fx= fx<
                    (map (lambda (kv kkvv) (cons (fx+ (car kv) (car kkvv)) (fx+ (cdr kv) (cdr kkvv))))
                         (treemap->list tm0) (treemap->list tm1))))

     ;; four trees
     (equal? (treemap-map (lambda (k v kk vv kkk vvv kkkk vvvv)
                            (values (fx+ k kk kkk kkkk) (fx+ v vv vvv vvvv)))
                          tm0 tm1 tm0 tm1)
             (apply treemap fx= fx<
                    (map (lambda (kv kkvv kkkvvv kkkkvvvv)
                           (cons (fx+ (car kv) (car kkvv) (car kkkvvv) (car kkkkvvvv))
                                 (fx+ (cdr kv) (cdr kkvv) (cdr kkkvvv) (cdr kkkkvvvv))))
                         (treemap->list tm0)
                         (treemap->list tm1)
                         (treemap->list tm0)
                         (treemap->list tm1))))

     )


(mat treemap-map/i

     (begin (define tm0 (let ([tm (make-treemap fx= fx<)])
                          (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100000)
                          tm))
            (define tm1 (let ([tm (make-treemap fx= fx>)])
                          (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100000)
                          tm))
            #t)

     ;; one tree
     (equal? (treemap-map/i (lambda (i k v) (values i k))
                            tm0)
             (let ([tm (make-treemap fx= fx<)])
               (fxvfor-each/i (lambda (i x) (treemap-set! tm i i)) v100000)
               tm))


     ;; two trees
     (equal? (treemap-map/i (lambda (i k v kk vv) (values i (fx+ k kk)))
                            tm0 tm1)
             (apply treemap fx= fx<
                    (map/i (lambda (i kv kkvv) (cons i (fx+ (car kv) (car kkvv))))
                           (treemap->list tm0) (treemap->list tm1))))
     ;; switched order
     (equal? (treemap-map/i (lambda (i k v kk vv) (values i (fx+ k kk)))
                            tm1 tm0)
             (apply treemap fx= fx>
                    (map/i (lambda (i kv kkvv) (cons i (fx+ (car kv) (car kkvv))))
                           (treemap->list tm0) (treemap->list tm1))))

     ;; four trees
     (equal? (treemap-map/i (lambda (i k v kk vv kkk vvv kkkk vvvv)
                              (values i (fx+ k kk kkk kkkk)))
                            tm0 tm1 tm0 tm1)
             (apply treemap fx= fx<
                    (map/i (lambda (i kv kkvv kkkvvv kkkkvvvv)
                             (cons i (fx+ (car kv) (car kkvv) (car kkkvvv) (car kkkkvvvv))))
                           (treemap->list tm0)
                           (treemap->list tm1)
                           (treemap->list tm0)
                           (treemap->list tm1))))

     )


(mat treemap-map!

     ;; return the 1st treemap


     #t)


(mat treemap-map!/i

     ;; return the 1st treemap


     #t)


(mat treemap-for-each

     (begin (define tm0 (let ([tm (make-treemap fx= fx<)])
                          (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100000)
                          tm))
            (define tm1 (let ([tm (make-treemap fx= fx>)])
                          (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100000)
                          tm))
            #t)

     (equal? tm0
             (let ([tm (make-treemap fx= fx<)])
               (treemap-for-each (lambda (k v) (treemap-set! tm k v)) tm0)
               tm))

     (equal? (treemap-map (lambda (k v kk vv) (values (+ k kk) (+ v vv))) tm0 tm1)
             (let ([tm (make-treemap fx= fx<)])
               (treemap-for-each (lambda (k v kk vv) (treemap-set! tm (+ k kk) (+ v vv))) tm0 tm1)
               tm))

     (equal? (treemap-map (lambda (k v kk vv kkk vvv kkkk vvvv)
                            (values (fx+ k kk kkk kkkk) (fx+ v vv vvv vvvv)))
                          tm0 tm1 tm0 tm1)
             (let ([tm (make-treemap fx= fx<)])
               (treemap-for-each (lambda (k v kk vv kkk vvv kkkk vvvv)
                                   (treemap-set! tm (fx+ k kk kkk kkkk) (fx+ v vv vvv vvvv)))
                                 tm0 tm1 tm0 tm1)
               tm))

     )


(mat treemap-for-each/i

     (begin (define tm0 (let ([tm (make-treemap fx= fx<)])
                          (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100000)
                          tm))
            (define tm1 (let ([tm (make-treemap fx= fx>)])
                          (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100000)
                          tm))
            #t)

     (equal? (treemap-map/i (lambda (i k v kk vv) (values i (fx+ k kk))) tm0 tm1)
             (let ([tm (make-treemap fx= fx<)])
               (treemap-for-each/i (lambda (i k v kk vv) (treemap-set! tm i (fx+ k kk))) tm0 tm1)
               tm))

     (equal? (treemap-map/i (lambda (i k v kk vv kkk vvv kkkk vvvv) (values i (fx+ k kk kkk kkkk)))
                          tm0 tm1 tm0 tm1)
             (let ([tm (make-treemap fx= fx<)])
               (treemap-for-each/i (lambda (i k v kk vv kkk vvv kkkk vvvv)
                                     (treemap-set! tm i (fx+ k kk kkk kkkk)))
                                 tm0 tm1 tm0 tm1)
               tm))

     )


(mat treemap-fold-left

     (begin (define tm0 (let ([tm (make-treemap fx= fx<)])
                          (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100000)
                          tm))
            (define tm1 (let ([tm (make-treemap fx= fx>)])
                          (fxvfor-each (lambda (x) (treemap-set! tm x x)) v100000)
                          tm))
            #t)

     (= (fxvsum v100000)
        (treemap-fold-left (lambda (acc k v) (fx+ k acc)) 0 tm0))

     (= (fxvmax v100000)
        (treemap-fold-left (lambda (acc k v) (if (fx> k acc) k acc)) 0 tm0))

     (= (* 2 (fxvsum v100000))
      (treemap-fold-left (lambda (acc k v kk vv) (fx+ k kk acc)) 0 tm0 tm1))
     )


(mat treemap-fold-left/i

     #t)


(mat treemap-fold-right

     #t)


(mat treemap-fold-right/i

     #t)
