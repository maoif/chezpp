(import (chezpp treeset)
        (chezpp list)
        (chezpp vector)
        (chezpp private rbtree))


(define v100    (fxvshuffle! (fxviota 100)))
(define v10000  (fxvshuffle! (fxviota 10000)))
(define v100000 (fxvshuffle! (fxviota 100000)))


(mat treeset-equal?

     (equal? (apply treeset = < (iota 100))
             (apply treeset = < (iota 100)))

     (not (equal? (apply treeset = < (iota 20))
                  (apply treeset = < (iota 10))))

     (not (equal? (apply treeset = fx< (iota 100))
                  (apply treeset = < (iota 100))))

     (let ([ts1 (make-treeset fx= fx<)] [ts2 (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts1 x)) v100000)
       (fxvfor-each (lambda (x) (treeset-add! ts2 x)) (fxvshuffle! v100000))
       (equal? ts1 ts1))


     )


(mat treeset-add!

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v100)
       (and (fxvandmap (lambda (x) (treeset-contains? ts x)) v100)
            (= (treeset-size ts) (fxvector-length v100))))

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v10000)
       (and (fxvandmap (lambda (x) (treeset-contains? ts x)) v10000)
            (= (treeset-size ts) (fxvector-length v10000))))

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v100000)
       (and (fxvandmap (lambda (x) (treeset-contains? ts x)) v10000)
            (= (treeset-size ts) (fxvector-length v100000))))

     )


(mat treeset-delete!

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v100)
       (fxvfor-each (lambda (x) (when (odd? x) (treeset-delete! ts x))) v100)
       (and (= (treeset-size ts) (fx/ (fxvector-length v100) 2))
            (fxvandmap (lambda (x)
                         (if (even? x) (treeset-contains? ts x) (not (treeset-contains? ts x))))
                       v100)))

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v10000)
       (fxvfor-each (lambda (x) (when (odd? x) (treeset-delete! ts x))) v10000)
       (and (= (treeset-size ts) (fx/ (fxvector-length v10000) 2))
            (fxvandmap (lambda (x)
                         (if (even? x) (treeset-contains? ts x) (not (treeset-contains? ts x))))
                       v10000)))

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v100000)
       (fxvfor-each (lambda (x) (when (odd? x) (treeset-delete! ts x))) v100000)
       (and (= (treeset-size ts) (fx/ (fxvector-length v100000) 2))
            (fxvandmap (lambda (x)
                         (if (even? x) (treeset-contains? ts x) (not (treeset-contains? ts x))))
                       v100000)))
     )


(mat treeset-search

     (error? (treeset-search 'bla 'bla))
     (error? (treeset-search (treeset = <) 'bla))

     (let ([ts1 (treeset = < 10 11 12)])
       (and (= 11 (treeset-search ts1 odd?))
            (= 10 (treeset-search ts1 even?))))

     (let ([ts1 (treeset = < 0 2 4 6)])
       (not (treeset-search ts1 odd?)))

     )


(mat treeset-search*

     (error? (treeset-search* 'bla 'bla))
     (error? (treeset-search* (treeset = <) 'bla))

     (let ([ts (apply treeset = < (nums 0 100))])
       (and (equal? (treeset-search* ts odd?)
                    (nums 1 100 2))
            (equal? (treeset-search* ts even?)
                    (nums 0 100 2))))


     (error? (treeset-search* 'bla 'bla 'bla))
     (error? (treeset-search* (treeset = <) odd? 'bla))

     (let ([ts (apply treeset = < (nums 0 100))]
           [ts1 (treeset = <)] [ts2 (treeset = <)])
       (treeset-search* ts odd? (lambda (x) (treeset-add! ts1 x)))
       (treeset-search* ts even? (lambda (x) (treeset-add! ts2 x)))
       (and (equal? ts1 (apply treeset = < (nums 1 100 2)))
            (equal? ts2 (apply treeset = < (nums 0 100 2)))))

     )


(mat treeset-contains/p?
     #t)


(mat treeset-values
     #t
     )


(mat treeset-max/min

     ;; empty
     (not (treeset-max (treeset = <)))
     (not (treeset-min (treeset = <)))

     (begin (define ts0 (let ([ts (make-treeset fx= fx<)])
                          (fxvfor-each (lambda (x) (treeset-add! ts x)) v100000)
                          ts))
            #t)

     (= (fxvmin v100000)
        (treeset-min ts0))

     (= (fxvmax v100000)
        (treeset-max ts0))

     )


(mat treeset-successor

     (let* ([ls (iota 1000)] [ts (apply treeset = < ls)])
       (andmap (lambda (x) (fx= (treeset-successor ts x) (fx1+ x))) (iota 999)))

     )


(mat treeset-predecessor

     (let* ([ls (iota 1000)] [ts (apply treeset = < ls)])
       (andmap (lambda (x) (fx= (treeset-predecessor ts x) (fx1- x))) (cdr ls)))

     )


(mat treeset-filter

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v100000)
       (andmap odd? (treeset->list (treeset-filter odd? ts))))

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v100000)
       (andmap odd? (treeset->list (treeset-filter odd? ts))))

     )


(mat treeset-filter!

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v100000)
       (treeset-filter! odd? ts)
       (andmap odd? (treeset->list ts)))

     (let ([ts (make-treeset fx= fx<)])
       (fxvfor-each (lambda (x) (treeset-add! ts x)) v100000)
       (treeset-filter! odd? ts)
       (andmap odd? (treeset->list ts)))

     )


(mat treeset-partition

     (error? (treeset-partition))
     (error? (treeset-partition 'a))
     (error? (treeset-partition 'a 'b))
     (error? (treeset-partition 'a (treeset = <)))


     (let* ([ls (iota 1000)] [odds (filter odd? ls)] [evens (filter even? ls)]
            [ts (apply treeset = < ls)]
            [ts-odd (apply treeset = < odds)]
            [ts-even (apply treeset = < evens)])
       (let-values ([(o e) (treeset-partition odd? ts)])
         (and (equal? o ts-odd)
              (equal? e ts-even))))

     )


(mat treeset->list

     (error? (treeset->list))
     (error? (treeset->list '()))
     (error? (treeset->list (treeset = <) 'bla))

     (let* ([ls (iota 100)]
            [ts (apply treeset = < ls)])
       (equal? ls (treeset->list ts)))

     (let* ([ls (iota 100)]
            [ts (apply treeset = > ls)])
       (equal? ls (reverse (treeset->list ts))))

     )


(mat treeset-map

     (equal? (treeset-map id (treeset = <))
             (treeset-map id (treeset = <)))

     (equal? (treeset-map fx1+ (apply treeset = < (iota 100)))
             (apply treeset = < (map fx1+ (iota 100))))

     (equal? (treeset-map + (apply treeset = < (iota 100)) (apply treeset = < (iota 100)))
             (apply treeset = < (map + (iota 100) (iota 100))))

     )


(mat treeset-map/i


     #t)


(mat treeset-for-each
     #t)


(mat treeset-for-each/i
     #t)


(mat treeset-fold-left
     #t)


(mat treeset-fold-left/i
     #t)


(mat treeset-fold-right
     #t)


(mat treeset-fold-right/i
     #t)


(mat treeset-set-ops

     ;; union
     (let ([ts1 (treeset = < 1 2 3 4 5 6)]
           [ts2 (treeset = < 4 5 6 7 8 9)]
           [ts3 (treeset = < 4 6 5 4 8 9)])
       (equal? (treeset+ ts1 ts2 ts3)
               (treeset = < 1 2 3 4 5 6 7 8 9)))

     (let ([ts1 (apply treeset = < (nums 0  50))]
           [ts2 (apply treeset = < (nums 30 80))]
           [ts3 (apply treeset = < (nums 40 90))])
       (equal? (apply treeset = < (nums 0 90))
               (treeset+ ts1 ts2 ts3)))

     ;; difference
     (let ([ts1 (treeset = < 1 2 3 4 5 6)]
           [ts2 (treeset = < 4 5 6 7 8 9)])
       (equal? (treeset- ts1 ts2)
               (treeset = < 1 2 3)))

     (let ([ts1 (treeset = < 1 2 3 4 5 6)]
           [ts2 (treeset = < 4 5 6 7 8 9)]
           [ts3 (treeset = < 4 6 5 4 8 9 1)])
       (equal? (treeset- ts1 ts2 ts3)
               (treeset = < 2 3)))

     (let ([ts1 (apply treeset = < (nums 0  50))]
           [ts2 (apply treeset = < (nums 30 80))]
           [ts3 (apply treeset = < (nums 40 90))])
       (equal? (apply treeset = < (nums 0 30))
               (treeset- ts1 ts2 ts3)))

     ;; intersection
     (let ([ts1 (treeset = < 1 2 3 4 5 6)]
           [ts2 (treeset = < 4 5 6 7 8 9)]
           [ts3 (treeset = < 4 6 5 4 8 9)])
       (equal? (treeset& ts1 ts2 ts3)
               (treeset = < 4 5 6)))

     (let ([ts1 (apply treeset = < (nums 0  50))]
           [ts2 (apply treeset = < (nums 30 80))]
           [ts3 (apply treeset = < (nums 40 90))])
       (equal? (apply treeset = < (nums 40 50))
               (treeset& ts1 ts2 ts3)))

     ;; symmetric difference
     (let ([ts1 (treeset = < 1 2 3 4 5 6)]
           [ts2 (treeset = < 4 5 6 7 8 9)])
       (equal? (treeset^ ts1 ts2)
               (treeset = < 1 2 3 7 8 9)))


     ;; equations
     (let ([ts1 (treeset = < 1 2 3 4 5 6)]
           [ts2 (treeset = < 4 5 6 7 8 9)]
           [ts3 (treeset = < 7 8 9 10 11 12 4 5 6)]
           [ts4 (treeset = < 4 6 7 8 2 5)])
       (equal? (treeset^ ts1 ts2 ts3 ts4)
               (treeset- (treeset+ ts1 ts2 ts3 ts4) (treeset& ts1 ts2 ts3 ts4))))

     (let* ([ls* (map (lambda (i) (random-list (lambda () (random 999)) 1000 2000)) (iota 10))]
            [ts* (map (lambda (ls) (apply treeset = < ls)) ls*)])
       (equal? (apply treeset^ ts*)
               (treeset- (apply treeset+ ts*) (apply treeset& ts*))))

     )