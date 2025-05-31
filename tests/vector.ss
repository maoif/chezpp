 (import (chezpp))


(mat iterations

     (error? (vormap odd? 1))
     (error? (vormap odd? 1 1))
     (error? (vormap odd? 1 1 1 1))
     (error? (vandmap odd? 1))
     (error? (vandmap odd? 1 1))
     (error? (vandmap odd? 1 1 1 1))

     (error? (fxvormap odd? 1))
     (error? (fxvormap odd? 1 1))
     (error? (fxvormap odd? 1 1 1 1))
     (error? (fxvandmap odd? 1))
     (error? (fxvandmap odd? 1 1))
     (error? (fxvandmap odd? 1 1 1 1))

     (error? (flvormap odd? 1))
     (error? (flvormap odd? 1 1))
     (error? (flvormap odd? 1 1 1 1))
     (error? (flvandmap odd? 1))
     (error? (flvandmap odd? 1 1))
     (error? (flvandmap odd? 1 1 1 1))

     (equal? '#()
             (vector-map/i (lambda (i x y) (list i x y)) '#() '#()))

     (equal? '#((0 1 a) (1 2 b) (2 3 c))
             (vector-map/i (lambda (i x y) (list i x y)) '#(1 2 3) '#(a b c)))


     (not (vormap odd? '#()))
     (not (vormap odd? '#() '#()))
     (not (vormap odd? '#() '#() '#()))
     (not (vormap odd? '#() '#() '#() '#()))
     (vormap odd? '#(0 1 2 3))
     (not (vormap odd? '#(0 8 6 4)))

     (not (fxvormap odd? '#vfx()))
     (not (fxvormap odd? '#vfx() '#vfx()))
     (not (fxvormap odd? '#vfx() '#vfx() '#vfx()))
     (not (fxvormap odd? '#vfx() '#vfx() '#vfx() '#vfx()))
     (fxvormap odd? '#vfx(0 1 2 3))
     (not (fxvormap odd? '#vfx(0 8 6 4)))

     (not (flvormap odd? '#vfl()))
     (not (flvormap odd? '#vfl() '#vfl()))
     (not (flvormap odd? '#vfl() '#vfl() '#vfl()))
     (not (flvormap odd? '#vfl() '#vfl() '#vfl() '#vfl()))
     (flvormap (sect > _ 2) '#vfl(0.0 1.1 2.2 3.3))
     (not (flvormap (sect = _ 8.8) '#vfl(0.1 8.7 6.4 4.5)))

     (vandmap odd? '#())
     (vandmap odd? '#() '#())
     (vandmap odd? '#() '#() '#())
     (vandmap odd? '#() '#() '#() '#())
     (vandmap odd? '#(1 3 7 9))
     (not (vandmap odd? '#(0 2 4 6)))

     (fxvandmap odd? '#vfx())
     (fxvandmap odd? '#vfx() '#vfx())
     (fxvandmap odd? '#vfx() '#vfx() '#vfx())
     (fxvandmap odd? '#vfx() '#vfx() '#vfx() '#vfx())
     (fxvandmap odd? '#vfx(1 3 7 9))
     (not (fxvandmap odd? '#vfx(0 2 4 6)))

     (flvandmap odd? '#vfl())
     (flvandmap odd? '#vfl() '#vfl())
     (flvandmap odd? '#vfl() '#vfl() '#vfl())
     (flvandmap odd? '#vfl() '#vfl() '#vfl() '#vfl())
     (flvandmap (sect > _ 0.1) '#vfl(1.1 3.3 7.3 9.1))
     (not (flvandmap integer? '#vfl(0.5 2.5 4.5 6.5)))

     (not (vexists odd? '#()))
     (not (vexists odd? '#() '#()))
     (not (vexists odd? '#() '#() '#()))
     (not (vexists odd? '#() '#() '#() '#()))
     (vexists odd? '#(0 1 2 3))
     (not (vexists odd? '#(0 8 6 4)))

     (vfor-all odd? '#())
     (vfor-all odd? '#() '#())
     (vfor-all odd? '#() '#() '#())
     (vfor-all odd? '#() '#() '#() '#())
     (vfor-all odd? '#(1 3 7 9))
     (not (vfor-all odd? '#(0 2 4 6)))

     ;; not vector
     (error? (vormap odd? '(1)))
     (error? (fxvormap odd? '(1)))
     (error? (flvormap odd? '(1)))

     ;; lengths differ
     (error? (vormap odd? '#(1) '#()))
     (error? (fxvormap odd? '#vfx() '#vfx(1)))
     (error? (flvormap odd? '#vfl() '#vfl(1.0)))


     ;; vfor-each/i
     (let ([acc 0])
       (vfor-each/i (lambda (i x) (set! acc (+ i acc))) (random-vector 10))
       (equal? acc (apply + (iota 10))))

     (let* ([lb (make-list-builder)]
            [l (iota 10)]
            [v (list->vector l)])
       (vfor-each/i (lambda (i x1 x2 x3) (lb (list i x2 x3 x3))) v v v)
       (equal? (zip l l l l) (lb)))


     ;; vector-map!
     (let* ([v (random-vector 10 100)]
            [v+ (vmap add1 v)])
       (equal? (vector-map! add1 v) v+))
     (let* ([v (random-vector 10 100)]
            [v1 (random-vector 10 100)]
            [v+ (vmap + v v1)])
       (equal? (vector-map! + v v1) v+))
     (let* ([v (random-vector 10 100)]
            [v1 (random-vector 10 100)]
            [v2 (random-vector 10 100)]
            [v+ (vmap + v v1 v2)])
       (equal? (vector-map! + v v1 v2) v+))
     (let* ([v (random-vector 10 100)]
            [v1 (random-vector 10 100)]
            [v2 (random-vector 10 100)]
            [v3 (random-vector 10 100)]
            [v+ (vmap + v v1 v2 v3)])
       (equal? (vector-map! + v v1 v2 v3) v+))

     ;; lengths differ
     (error? (vector-map! + (random-vector 10) (random-vector 11)))
     (error? (fxvector-map! + (random-fxvector 10) (random-fxvector 11)))
     (error? (flvector-map! + (random-flvector 10) (random-flvector 11)))

     (error? (vector-map!/i + (random-vector 10) (random-vector 11)))
     (error? (fxvector-map!/i + (random-fxvector 10) (random-fxvector 11)))
     (error? (flvector-map!/i + (random-flvector 10) (random-flvector 11)))

     (error? (vfor-each + (random-vector 10) (random-vector 11)))
     (error? (fxvfor-each + (random-fxvector 10) (random-fxvector 11)))
     (error? (flvfor-each + (random-flvector 10) (random-flvector 11)))

     (error? (vfor-each/i + (random-vector 10) (random-vector 11)))
     (error? (fxvfor-each/i + (random-fxvector 10) (random-fxvector 11)))
     (error? (flvfor-each/i + (random-flvector 10) (random-flvector 11)))


     (eq? '#() (vmap/i id '#()))
     (eq? '#vfx() (fxvmap/i id '#vfx()))
     (eq? '#vfl() (flvmap/i id '#vfl()))
     (equal? (list->vector (iota 10))
             (vmap/i (lambda (i x) i) (random-vector 10)))
     (equal? (list->fxvector (iota 10))
             (fxvmap/i (lambda (i x) i) (random-fxvector 10)))
     (equal? (list->flvector (map inexact (iota 10)))
             (flvmap/i (lambda (i x) (inexact i)) (random-flvector 10)))

     )


(mat folds

     (error? (vfold-left (lambda (acc x) (+ acc x)) 0 '()))
     (error? (vfold-left (lambda (acc x) (+ acc x)) 0 '() '()))
     (error? (vfold-left (lambda (acc x) (+ acc x)) 0 '() '() '() '()))
     (error? (vfold-left/i (lambda (i acc x) (+ acc x)) 0 '()))
     (error? (vfold-left/i (lambda (i acc x) (+ acc x)) 0 '() '()))
     (error? (vfold-left/i (lambda (i acc x) (+ acc x)) 0 '() '() '() '()))
     (error? (vfold-right (lambda (i x acc) (+ acc x)) 0 '()))
     (error? (vfold-right (lambda (i x acc) (+ acc x)) 0 '() '()))
     (error? (vfold-right (lambda (i x acc) (+ acc x)) 0 '() '() '() '()))
     (error? (vfold-right/i (lambda (i x acc) (+ acc x)) 0 '()))
     (error? (vfold-right/i (lambda (i x acc) (+ acc x)) 0 '() '()))
     (error? (vfold-right/i (lambda (i x acc) (+ acc x)) 0 '() '() '() '()))

     (error? (fxvfold-left (lambda (acc x) (+ acc x)) 0 '()))
     (error? (fxvfold-left (lambda (acc x) (+ acc x)) 0 '() '()))
     (error? (fxvfold-left (lambda (acc x) (+ acc x)) 0 '() '() '() '()))
     (error? (fxvfold-left/i (lambda (i acc x) (+ acc x)) 0 '()))
     (error? (fxvfold-left/i (lambda (i acc x) (+ acc x)) 0 '() '()))
     (error? (fxvfold-left/i (lambda (i acc x) (+ acc x)) 0 '() '() '() '()))
     (error? (fxvfold-right (lambda (i x acc) (+ acc x)) 0 '()))
     (error? (fxvfold-right (lambda (i x acc) (+ acc x)) 0 '() '()))
     (error? (fxvfold-right (lambda (i x acc) (+ acc x)) 0 '() '() '() '()))
     (error? (fxvfold-right/i (lambda (i x acc) (+ acc x)) 0 '()))
     (error? (fxvfold-right/i (lambda (i x acc) (+ acc x)) 0 '() '()))
     (error? (fxvfold-right/i (lambda (i x acc) (+ acc x)) 0 '() '() '() '()))

     (error? (fxvfold-left (lambda (acc x) (+ acc x)) 0 (make-vector 10)))
     (error? (fxvfold-right (lambda (i x acc) (+ acc x)) 0 (make-vector 10) (make-vector 10)))

     (error? (flvfold-left (lambda (acc x) (+ acc x)) 0 '()))
     (error? (flvfold-left (lambda (acc x) (+ acc x)) 0 '() '()))
     (error? (flvfold-left (lambda (acc x) (+ acc x)) 0 '() '() '() '()))
     (error? (flvfold-left/i (lambda (i acc x) (+ acc x)) 0 '()))
     (error? (flvfold-left/i (lambda (i acc x) (+ acc x)) 0 '() '()))
     (error? (flvfold-left/i (lambda (i acc x) (+ acc x)) 0 '() '() '() '()))
     (error? (flvfold-right (lambda (i x acc) (+ acc x)) 0 '()))
     (error? (flvfold-right (lambda (i x acc) (+ acc x)) 0 '() '()))
     (error? (flvfold-right (lambda (i x acc) (+ acc x)) 0 '() '() '() '()))
     (error? (flvfold-right/i (lambda (i x acc) (+ acc x)) 0 '()))
     (error? (flvfold-right/i (lambda (i x acc) (+ acc x)) 0 '() '()))
     (error? (flvfold-right/i (lambda (i x acc) (+ acc x)) 0 '() '() '() '()))

     (error? (flvfold-left (lambda (acc x) (+ acc x)) 0 (make-vector 10)))
     (error? (flvfold-right (lambda (i x acc) (+ acc x)) 0 (make-vector 10) (make-vector 10)))


     ;; vfold-left
     (let* ([v (random-vector 9999 99999)]
            [res (vfold-left (lambda (acc x) (+ acc x)) 0 v)])
       (= res (vsum v)))
     (let* ([v (random-fxvector 9999 99999)]
            [res (fxvfold-left (lambda (acc x) (fx+ acc x)) 0 v)])
       (= res (fxvsum v)))
     (let* ([v (random-flvector 9999 99999.9)]
            [res (flvfold-left (lambda (acc x) (fl+ acc x)) 0.0 v)])
       (= res (flvsum v)))

     ;; vfold-left/i
     (= (apply + (iota 10))
        (vfold-left/i (lambda (i acc x) (+ i acc)) 0 (make-vector 10)))


     ;; vfold-right
     (let* ([v (random-vector 9999 99999)]
            [res (vfold-right (lambda (acc x) (+ acc x)) 0 v)])
       (= res (vsum v)))

     ;; lengths differ
     (error? (let ([v0 (random-vector 999 99999)]
                   [v1 (random-vector 99 99999)])
               (vfold-right (lambda (x1 x2 acc) (+ x1 acc)) 0 v0 v1)))
     (error? (let ([v0 (random-vector 999 99999)]
                   [v1 (random-vector 99 99999)]
                   [v2 (random-vector 9 99999)])
               (vfold-right (lambda (x1 x2 x3 acc) (+ x1 acc)) 0 v0 v1 v2)))
     (error? (let ([v0 (random-vector 9 99999)]
                   [v1 (random-vector 99 99999)]
                   [v2 (random-vector 999 99999)]
                   [v3 (random-vector 19 99999)])
               (vfold-right (lambda (x1 x2 x3 x4 acc) (+ x1 acc)) 0 v0 v1 v2 v3)))


     ;; vfold-right/i
     ;; lengths differ
     (error? (let ([v0 (random-vector 999 99999)]
                   [v1 (random-vector 99 99999)])
               (vfold-right/i (lambda (i x1 x2 acc) (+ i acc)) 0 v0 v1)))
     (error? (let ([v0 (random-vector 999 99999)]
                   [v1 (random-vector 99 99999)]
                   [v2 (random-vector 9 99999)])
               (vfold-right/i (lambda (i x1 x2 x3 acc) (+ i acc)) 0 v0 v1 v2)))
     (error? (let ([v0 (random-vector 9 99999)]
                   [v1 (random-vector 99 99999)]
                   [v2 (random-vector 999 99999)]
                   [v3 (random-vector 19 99999)])
               (vfold-right/i (lambda (i x1 x2 x3 x4 acc) (+ i acc)) 0 v0 v1 v2 v3)))


     (let* ([v (random-vector 999 99999)]
            [res (vfold-right (lambda (x acc) (+ acc x)) 0 v)])
       (= res (vsum v)))

     (= (apply + (iota 10))
        (vfold-right/i (lambda (i x acc) (+ i acc)) 0 (make-vector 10)))
     (= (apply + (iota 10))
        (fxvfold-right/i (lambda (i x acc) (+ i acc)) 0 (make-fxvector 10)))
     (= (apply + (nums 0.5 10))
        (flvfold-right/i (lambda (i x acc) (+ i acc 0.5)) 0.0 (make-flvector 10)))

     (= 1024 (vfold-left  (lambda (acc x) (+ acc acc)) 1 (random-vector 10 100)))
     (= 1024 (vfold-right (lambda (x acc) (+ acc acc)) 1 (random-vector 10 100)))

     )



(mat reverse

     (begin (define v (random-vector 999 999))
            (define fxv (random-fxvector 999 999))
            (define flv (random-flvector 999 999))

            (define v1 (vector-copy v))
            (define fxv1 (fxvector-copy fxv))
            (define flv1 (flvector-copy flv))
            #t)

     (begin (vreverse! v)
            (vreverse! v)

            (fxvreverse! fxv)
            (fxvreverse! fxv)

            (flvreverse! flv)
            (flvreverse! flv)

            #t)

     (equal? v v1)
     (equal? fxv fxv1)
     (equal? flv flv1)

     (equal? (vreverse (vreverse v1)) v1)
     (equal? (fxvreverse (fxvreverse fxv1)) fxv1)
     (equal? (flvreverse (flvreverse flv1)) flv1)

     ;; not vector
     (error? (vreverse '()))
     (error? (fxvreverse '()))
     (error? (flvreverse '()))
     (error? (vreverse! '()))
     (error? (fxvreverse! '()))
     (error? (flvreverse! '()))

     )


(mat memp

     (begin (define v '#(0 1 2 3 4 5))
            (define fxv '#vfx(0 1 2 3 4 5))
            (define flv '#vfl(0.0 1.2 2.3 3.4 4.5 5.6))
            #t)

     (not (vmemq 0 '#()))
     (not (vmemq 6 v))
     (= 1 (vmemp odd? v))
     (= 3 (vmember 3 v))

     (not (fxvmemq 0 '#vfx()))
     (not (fxvmemq 6 fxv))
     (= 1 (fxvmemp odd? fxv))
     (= 3 (fxvmember 3 fxv))

     (not (flvmemq 0 '#vfl()))
     (not (flvmemq 6 flv))
     (= 1 (flvmemp (sect = _ 1.2) flv))
     (= 3 (flvmember 3.4 flv))


     ;; not procedure
     (error? (vmemp 1 '#()))
     (error? (fxvmemp 1 '#()))
     (error? (flvmemp 1 '#()))
     ;; not vector
     (error? (vmemp   odd? '()))
     (error? (fxvmemp odd? '()))
     (error? (flvmemp odd? '()))

     )


(mat zip

     ;; arity error
     (error? (vzip '#()))
     (error? (fxvzip '#vfx()))
     (error? (flvzip '#vfl()))
     (error? (vzipv '#()))
     (error? (fxvzipv '#vfx()))
     (error? (flvzipv '#vfl()))

     (equal? '#() (vzip '#() '#()))
     (equal? '#() (fxvzip '#vfx() '#vfx()))
     (equal? '#() (flvzip '#vfl() '#vfl()))
     (equal? '#() (vzipv '#() '#()))
     (equal? '#() (fxvzipv '#vfx() '#vfx()))
     (equal? '#() (flvzipv '#vfl() '#vfl()))

     (equal? '#() (vzip '#() '#() '#()))
     (equal? '#() (fxvzip '#vfx() '#vfx() '#vfx()))
     (equal? '#() (flvzip '#vfl() '#vfl() '#vfl()))
     (equal? '#() (vzipv '#() '#() '#()))
     (equal? '#() (fxvzipv '#vfx() '#vfx() '#vfx()))
     (equal? '#() (flvzipv '#vfl() '#vfl() '#vfl()))

     ;; *vzip
     (let* ([l (iota 20)]
            [v (list->vector l)])
       (equal? (zip l l) (vector->list (vzip v v))))
     (let* ([l (iota 20)]
            [v (list->vector l)])
       (equal? (zip l l l l) (vector->list (vzip v v v v))))

     (let* ([l (iota 20)]
            [v (list->fxvector l)])
       (equal? (zip l l) (vector->list (fxvzip v v))))
     (let* ([l (iota 20)]
            [v (list->fxvector l)])
       (equal? (zip l l l l) (vector->list (fxvzip v v v v))))

     (let* ([l (nums 1.2 20)]
            [v (list->flvector l)])
       (equal? (zip l l) (vector->list (flvzip v v))))
     (let* ([l (nums 1.2 20)]
            [v (list->flvector l)])
       (equal? (zip l l l l) (vector->list (flvzip v v v v))))

     ;; *vzipv
     (let* ([l (iota 20)]
            [v (list->vector l)])
       (equal? (zip l l) (vector->list (vmap vector->list (vzipv v v)))))
     (let* ([l (iota 20)]
            [v (list->vector l)])
       (equal? (zip l l l l) (vector->list (vmap vector->list (vzipv v v v v)))))

     (let* ([l (iota 20)]
            [v (list->fxvector l)])
       (equal? (zip l l) (vector->list (vmap fxvector->list (fxvzipv v v)))))
     (let* ([l (iota 20)]
            [v (list->fxvector l)])
       (equal? (zip l l l l) (vector->list (vmap fxvector->list (fxvzipv v v v v)))))

     (let* ([l (nums 1.2 20)]
            [v (list->flvector l)])
       (equal? (zip l l) (vector->list (vmap flvector->list (flvzipv v v)))))
     (let* ([l (nums 1.2 20)]
            [v (list->flvector l)])
       (equal? (zip l l l l) (vector->list (vmap flvector->list (flvzipv v v v v)))))

     )


(mat misc

     ;; type error
     (error? (vmax '()))
     (error? (fxvmax '()))
     (error? (flvmax '()))
     (error? (vmin '()))
     (error? (fxvmin '()))
     (error? (flvmin '()))

     ;; arity error
     (error? (vmax '#() '#()))
     (error? (fxvmax '#vfx() '#vfx()))
     (error? (flvmax '#vfl() '#vfl()))
     (error? (vmin '#() '#()))
     (error? (fxvmin '#vfx() '#vfx()))
     (error? (flvmin '#vfl() '#vfl()))

     (not (vmax '#()))
     (not (fxvmax '#vfx()))
     (not (flvmax '#vfl()))
     (not (vmin '#()))
     (not (fxvmin '#vfx()))
     (not (flvmin '#vfl()))

     (let* ([l (iota 10)]
            [v (list->vector l)])
       (= (apply max l) (vmax v)))
     (let* ([l (iota 10)]
            [v (list->fxvector l)])
       (= (apply max l) (fxvmax v)))
     (let* ([l (nums 1.1 10)]
            [v (list->flvector l)])
       (= (apply max l) (flvmax v)))

     (let* ([l (iota 10)]
            [v (list->vector l)])
       (= (apply min l) (vmin v)))
     (let* ([l (iota 10)]
            [v (list->fxvector l)])
       (= (apply min l) (fxvmin v)))
     (let* ([l (nums 1.1 10)]
            [v (list->flvector l)])
       (= (apply min l) (flvmin v)))

     )


(mat vslice

     (equal? '#() (vslice '#() 1))
     (equal? '#() (vslice '#() 0))
     (equal? '#() (vslice '#() -1))
     (equal? '#() (vslice '#() 3 1))
     (equal? '#() (vslice '#(1) 0 1 -1))

     (equal? '#(1) (vslice '#(1) 0 1))
     (equal? '#(1) (vslice '#(1) 1))
     (equal? '#(1) (vslice '#(1) 2))
     (equal? '#(1) (vslice '#(1) 3))
     (equal? '#(1) (vslice '#(1) 2 -4 -4))

     (equal? '#() (vslice '#(1) 2 -4))
     (equal? '#() (vslice '#(1) 1 5))
     (equal? '#() (vslice '#(1) 3 0 3))

     (begin (define vec '#(0 1 2 3 4 5 6 7 8 9))
            #t)

     (equal? '#(8 5 2)
             (vslice vec 8 0 -3))
     (equal? '#(9 6 3)
             (vslice vec 9 0 -3))

     ;; positive index, forward
     (equal? '#(0 1 2 3 4)
             (vslice vec 5))
     (equal? '#(0 2 4 6 8)
             (vslice vec 0 9 2))
     (equal? '#(2 5 8)
             (vslice vec 2 9 3))

     ;; positive index, backward
     (equal? '#(3 2)
             (vslice vec 3 1 -1))
     (equal? '#(8 6 4)
             (vslice vec 8 2 -2))
     (equal? '#(9 6 3)
             (vslice vec 9 1 -3))
     (equal? '#(9 5)
             (vslice vec 9 1 -4))
     (equal? '#(9 5 1)
             (vslice vec 9 0 -4))

     ;; negative index, forward
     (equal? '#(0 1 2 3 4)
             (vslice vec -5))
     (equal? (list->vector (iota 9))
             (vslice vec -1))
     (equal? '#(5 6 7 8)
             (vslice vec -5 -1))
     (equal? '#(9)
             (vslice vec -1 -2 -1))
     (equal? '#(1 2 3 4 5 6 7 8)
             (vslice vec -9 -1))
     (equal? '#(1 4 7)
             (vslice vec -9 -1 3))

     ;; negative index, backward
     (equal? '#(9 8 7 6)
             (vslice vec -1 -5 -1))
     (equal? '#(9 7 5 3)
             (vslice vec -1 -9 -2))
     (equal? '#(8 4)
             (vslice vec -2 -9 -4))
     (equal? '#(1)
             (vslice '#(1) -1 -2 -1))
     )


(mat copy!

     (begin (define v (make-vector 10 #t))
            (define fxv (make-fxvector 10 0))
            (define flv (make-flvector 10 0.0))
            #t)

     ;; errors
     (error? (vcopy! 42 42 42 42))
     (error? (vcopy! v -1 v 0  3))
     (error? (vcopy! v 0  v -1 3))
     (error? (vcopy! v 11 v 0  3))
     (error? (vcopy! v 0  v 11 3))
     (error? (vcopy! v 1  v 0  10))
     (error? (vcopy! v 0  v 9  3))

     (error? (fxvcopy! 42 42 42 42))
     (error? (fxvcopy! fxv -1 fxv 0  3))
     (error? (fxvcopy! fxv 0  fxv -1 3))
     (error? (fxvcopy! fxv 11 fxv 0  3))
     (error? (fxvcopy! fxv 0  fxv 11 3))
     (error? (fxvcopy! fxv 1  fxv 0  10))
     (error? (fxvcopy! fxv 0  fxv 9  3))

     (error? (flvcopy! 42 42 42 42))
     (error? (flvcopy! flv -1 flv 0  3))
     (error? (flvcopy! flv 0  flv -1 3))
     (error? (flvcopy! flv 11 flv 0  3))
     (error? (flvcopy! flv 0  flv 11 3))
     (error? (flvcopy! flv 1  flv 0  10))
     (error? (flvcopy! flv 0  flv 9  3))



;;;; same vector

     ;; disjoint, left to right
     (let ([v (apply vector (iota 10))])
       (vcopy! v 0 v 3 3)
       (equal? '#(0 1 2 0 1 2 6 7 8 9) v))

     (let ([v (apply vector (iota 10))])
       (vcopy! v 0 v 5 3)
       (equal? '#(0 1 2 3 4 0 1 2 8 9) v))

     (let ([v (apply vector (iota 10))])
       (vcopy! v 0 v 5 5)
       (equal? '#(0 1 2 3 4 0 1 2 3 4) v))

     ;; disjoint, right to left
     (let ([v (apply vector (iota 10))])
       (vcopy! v 5 v 0 3)
       (equal? '#(5 6 7 3 4 5 6 7 8 9) v))

     (let ([v (apply vector (iota 10))])
       (vcopy! v 3 v 0 3)
       (equal? '#(3 4 5 3 4 5 6 7 8 9) v))

     (let ([v (apply vector (iota 10))])
       (vcopy! v 5 v 0 5)
       (equal? '#(5 6 7 8 9 5 6 7 8 9) v))

     ;; overlapping, left to right
     (let ([v (apply vector (iota 10))])
       (vcopy! v 0 v 2 3)
       (equal? '#(0 1 0 1 2 5 6 7 8 9) v))

     (let ([v (apply vector (iota 10))])
       (vcopy! v 0 v 0 5)
       (equal? '#(0 1 2 3 4 5 6 7 8 9) v))

     (let ([v (apply vector (iota 10))])
       (vcopy! v 0 v 1 5)
       (equal? '#(0 0 1 2 3 4 6 7 8 9) v))

     (let ([v (apply vector (iota 10))])
       (vcopy! v 0 v 3 5)
       (equal? '#(0 1 2 0 1 2 3 4 8 9) v))

     ;; overlapping , right to left
     (let ([v (apply vector (iota 10))])
       (vcopy! v 2 v 0 3)
       (equal? '#(2 3 4 3 4 5 6 7 8 9) v))

     (let ([v (apply vector (iota 10))])
       (vcopy! v 2 v 0 5)
       (equal? '#(2 3 4 5 6 5 6 7 8 9) v))

     (let ([v (apply vector (iota 10))])
       (vcopy! v 5 v 3 5)
       (equal? '#(0 1 2 5 6 7 8 9 8 9) v))



;;;; different vector

     (let ([v (apply vector (iota 10))]
           [vv (make-vector 10 #f)])
       (vcopy! v 0 vv 0 3)
       (equal? '#(0 1 2) (vslice vv 0 3)))

     (let ([v (apply vector (iota 10))]
           [vv (make-vector 10 #f)])
       (vcopy! v 0 vv 0 5)
       (equal? '#(0 1 2 3 4) (vslice vv 0 5)))

     (let ([v (apply vector (iota 10))]
           [vv (make-vector 10 #f)])
       (vcopy! v 0 vv 3 5)
       (equal? '#(#f #f #f 0 1 2 3 4) (vslice vv 0 8)))

     (let ([v (apply vector (iota 10))]
           [vv (make-vector 10 #f)])
       (vcopy! v 2 vv 0 5)
       (equal? '#(2 3 4 5 6) (vslice vv 0 5)))

     )


(mat *vfilter

     (error? (vfilter))
     (error? (fxvfilter))
     (error? (flvfilter))

     (error? (vfilter   'bla))
     (error? (fxvfilter 'bla))
     (error? (flvfilter 'bla))

     (error? (vfilter   'bla (vector)))
     (error? (fxvfilter 'bla (fxvector)))
     (error? (flvfilter 'bla (flvector)))

     (error? (vfilter   (lambda (x) #t) 42))
     (error? (fxvfilter (lambda (x) #t) 42))
     (error? (flvfilter (lambda (x) #t) 42))

     (let* ([v (random-vector 1000)] [ls (vector->list v)])
       (equal? (vfilter odd? v)
               (list->vector (filter odd? ls))))

     (let* ([v (random-fxvector 1000)] [ls (fxvector->list v)])
       (equal? (fxvfilter odd? v)
               (list->fxvector (filter odd? ls))))

     (let* ([v (random-flvector 1000)] [ls (flvector->list v)] [r (random 99999)]
            [pred (lambda (x) (< x r))])
       (equal? (flvfilter pred v)
               (list->flvector (filter pred ls))))

     )


(mat *viota

     (error? (viota 'x))
     (error? (viota #f))

     (error? (fxviota 'x))
     (error? (fxviota #f))

     (equal? (vector->list (viota 10)) (iota 10))
     (equal? (vector->list (viota 100)) (iota 100))

     (equal? (fxvector->list (fxviota 10)) (iota 10))
     (equal? (fxvector->list (fxviota 100)) (iota 100))
     )


(mat *vnums

     (error? (vnums 'x 'x 'x))
     (error? (vnums 0 10  -1))
     (error? (vnums 0 -10 1))

     (error? (fxvnums 'x 'x 'x))
     (error? (fxvnums 0 10  -1))
     (error? (fxvnums 0 -10 1))

     (equal? (vnums 0 10) (viota 10))
     (equal? (fxvnums 0 10) (fxviota 10))

     ;; v
     (equal? (vnums 0 100 2)
             (list->vector (nums 0 100 2)))
     (equal? (vnums 0 100 4)
             (list->vector (nums 0 100 4)))
     (equal? (vnums 50 100 4)
             (list->vector (nums 50 100 4)))
     (equal? (vnums 0 -100 -2)
             (list->vector (nums 0 -100 -2)))
     (equal? (vnums 0 -100 -4)
             (list->vector (nums 0 -100 -4)))
     (equal? (vnums -50 -100 -4)
             (list->vector (nums -50 -100 -4)))

     ;; fx
     (equal? (fxvnums 0 100 2)
             (list->fxvector (nums 0 100 2)))
     (equal? (fxvnums 0 100 4)
             (list->fxvector (nums 0 100 4)))
     (equal? (fxvnums 50 100 4)
             (list->fxvector (nums 50 100 4)))
     (equal? (fxvnums 0 -100 -2)
             (list->fxvector (nums 0 -100 -2)))
     (equal? (fxvnums 0 -100 -4)
             (list->fxvector (nums 0 -100 -4)))
     (equal? (fxvnums -50 -100 -4)
             (list->fxvector (nums -50 -100 -4)))

     ;; fl
     (equal? (flvnums 0.0 100.2 2.5)
             (list->flvector (nums 0.0 100.2 2.5)))
     (equal? (flvnums 0.0 100.0 4.0)
             (list->flvector (nums 0.0 100.0 4.0)))
     (equal? (flvnums 50.0 100.0 4.0)
             (list->flvector (nums 50.0 100.0 4.0)))
     (equal? (flvnums 0.0 -100.7 -2.7)
             (list->flvector (nums 0.0 -100.7 -2.7)))
     (equal? (flvnums 0.0 -100.3 -4.3)
             (list->flvector (nums 0.0 -100.3 -4.3)))
     (equal? (flvnums -50.0 -100.0 -4.0)
             (list->flvector (nums -50.0 -100.0 -4.0)))

     )


(mat *vpartition

     (error? (vpartition   1 (vector)))
     (error? (fxvpartition 1 (fxvector)))
     (error? (flvpartition 1 (flvector)))

     (error? (vpartition   odd? '()))
     (error? (fxvpartition odd? '()))
     (error? (flvpartition odd? '()))

     (let-values ([(t f) (vpartition odd? (viota 20))])
       (and (vandmap odd? t)
            (vandmap even? f)
            (= 20 (+ (vector-length t) (vector-length f)))))
     (let-values ([(t f) (fxvpartition odd? (fxviota 20))])
       (and (fxvandmap odd? t)
            (fxvandmap even? f)
            (= 20 (+ (fxvector-length t) (fxvector-length f)))))
     (let-values ([(t f) (flvpartition odd? (list->flvector (map! inexact (iota 20))))])
       (and (flvandmap odd? t)
            (flvandmap even? f)
            (= 20 (+ (flvector-length t) (flvector-length f)))))

     )


(mat *vsorted?

     (error? (vsorted?))
     (error? (vsorted? 1 (vector)))

     (vsorted? <  '#())
     (vsorted? <= '#())
     (vsorted? >  '#())
     (vsorted? < (viota 10))
     (vsorted? <= (viota 10))
     (not (vsorted? < (vreverse! (viota 10))))
     (vsorted? > (vreverse! (viota 10)))

     (error? (fxvsorted?))
     (error? (fxvsorted? 1 (fxvector)))

     (fxvsorted? fx<  '#vfx())
     (fxvsorted? fx<= '#vfx())
     (fxvsorted? fx>  '#vfx())
     (fxvsorted? fx< (fxviota 10))
     (fxvsorted? fx<= (fxviota 10))
     (not (fxvsorted? fx< (fxvreverse! (fxviota 10))))
     (fxvsorted? fx> (fxvreverse! (fxviota 10)))

     (error? (flvsorted?))
     (error? (flvsorted? 1 (flvector)))

     (flvsorted? fl<  '#vfl())
     (flvsorted? fl<= '#vfl())
     (flvsorted? fl>  '#vfl())
     (flvsorted? fl< (flvnums 0.0 20.0 2.2))
     (flvsorted? fl<= (flvnums 0.0 20.0 2.2))
     (not (flvsorted? fl< (flvreverse! (flvnums 0.0 20.0 2.2))))
     (flvsorted? fl> (flvreverse! (flvnums 0.0 20.0 2.2)))
     ;; type error
     (error? (fxvsorted? fl> (flvnums 0.0 20.0 2.2)))

     )


(mat *vsort
     ;; bad <?
     (error? (vsort!   1 (vector)))
     (error? (fxvsort! 1 (fxvector)))
     (error? (flvsort! 1 (flvector)))
     ;; not vectors
     (error? (vsort!   <= '(1 2 3)))
     (error? (fxvsort! <= '(1 2 3)))
     (error? (flvsort! <= '(1 2 3)))
     ;; bad range
     (error? (vsort!   <= (vector) 3))
     (error? (fxvsort! <= (fxvector) 3))
     (error? (flvsort! <= (flvector) 3))
     (error? (vsort!   <= (vector   2 2 2 2 2) 6 3))
     (error? (fxvsort! <= (fxvector 2 2 2 2 2) 6 3))
     (error? (flvsort! <= (flvector 2.0 2.0 2.0 2.0) 6 3))

     ;; bad <?
     (error? (vsort   1 (vector)))
     (error? (fxvsort 1 (fxvector)))
     (error? (flvsort 1 (flvector)))
     ;; not vectors
     (error? (vsort   <= '(1 2 3)))
     (error? (fxvsort <= '(1 2 3)))
     (error? (flvsort <= '(1 2 3)))
     ;; bad range
     (error? (vsort   <= (vector) 3))
     (error? (fxvsort <= (fxvector) 3))
     (error? (flvsort <= (flvector) 3))
     (error? (vsort   <= (vector   2 2 2 2 2) 6 3))
     (error? (fxvsort <= (fxvector 2 2 2 2 2) 6 3))
     (error? (flvsort <= (flvector 2.0 2.0 2.0 2.0) 6 3))

     ;; full range, in place
     (begin (define (test1 rand <? sort! sorted?)
              (andmap (lambda (bd)
                        (andmap (lambda (i)
                                  (let ([v (rand #e1e6 bd)])
                                    (sort! <? v)
                                    (sorted? <? v)))
                                (iota 3)))
                      '(100 1000 10000 100000)))
            #t)
     (test1 random-vector   fx<= vsort!   vsorted?)
     (test1 random-fxvector fx<= fxvsort! fxvsorted?)
     (test1 random-flvector fl<= flvsort! flvsorted?)

     ;; full range, return new
     (begin (define (test2 rand <? sort sorted?)
              (andmap (lambda (bd)
                        (andmap (lambda (i)
                                  (let ([v (rand #e1e6 bd)])
                                    (sorted? <? (sort <? v))))
                                (iota 3)))
                      '(100 1000 10000 100000)))
            #t)
     (test2 random-vector   fx<= vsort   vsorted?)
     (test2 random-fxvector fx<= fxvsort fxvsorted?)
     (test2 random-flvector fl<= flvsort flvsorted?)


     ;; ranged, in place
     (begin (define (test3 rand <? sort! vref sorted?)
              (andmap (lambda (bd)
                        (andmap (lambda (i)
                                  (let ([v (rand #e1e6 bd)]
                                        [mid (fx/ #e1e6 2)])
                                    (sort! <? v mid)
                                    (sort! <? v mid #e1e6)
                                    (and (sorted? <? v 0 mid)
                                         (sorted? <? v mid #e1e6))))
                                (iota 3)))
                      '(100 1000 10000 100000)))
            #t)
     (test3 random-vector   fx<= vsort!   vector-ref vsorted?)
     (test3 random-fxvector fx<= fxvsort! fxvector-ref fxvsorted?)
     (test3 random-flvector fl<= flvsort! flvector-ref flvsorted?)

     ;; ranged, return new
     (begin (define (test4 rand <? sort sorted?)
              (andmap (lambda (bd)
                        (andmap (lambda (i)
                                  (let ([v (rand #e1e6 bd)]
                                        [mid (fx/ #e1e6 2)])
                                    (and (sorted? <? (sort <? v mid))
                                         (sorted? <? (sort <? v mid #e1e6)))))
                                (iota 3)))
                      '(100 1000 10000 100000)))
            #t)
     (test4 random-vector   fx<= vsort   vsorted?)
     (test4 random-fxvector fx<= fxvsort fxvsorted?)
     (test4 random-flvector fl<= flvsort flvsorted?)

     )


(mat *vscan-left-ex

     ;; arity
     (error? (vscan-left-ex + 0))
     ;; not vector
     (error? (vscan-left-ex + 0 '()))
     ;; not proc
     (error? (vscan-left-ex 'bla 0 '#()))

     (error? (fxvscan-left-ex + 0))
     (error? (fxvscan-left-ex + 0 '()))
     (error? (fxvscan-left-ex 'bla 0 '#()))
     ;; not fxvector
     (error? (fxvscan-left-ex + 0 '#()))

     (error? (flvscan-left-ex + 0))
     (error? (flvscan-left-ex + 0 '()))
     (error? (flvscan-left-ex 'bla 0 '#()))
     ;; not flvector
     (error? (flvscan-left-ex + 0.0 '#()))

;;;; vector
;;; 1 arg
     (equal? '#() (vscan-left-ex + 0 '#()))
     (equal? '#(0 1 3 6)
             (vscan-left-ex + 0 '#(1 2 3 4)))
     (equal? '#(1 1 2 6) (vscan-left-ex * 1 '#(1 2 3 4)))

;;; 2 args
     (equal? '#() (vscan-left-ex + 0 '#() '#()))
     (equal? '#(1 3 7 13)
             (vscan-left-ex (lambda (acc x y) (+ acc x y))
                            1 '#(1 2 3 4) '#(1 2 3 4)))

;;; 3 args
     (equal? '#() (vscan-left-ex + 0 '#() '#() '#()))
     (equal? '#(1 4 10 19)
             (vscan-left-ex (lambda (acc x y z) (+ acc x y z))
                            1 '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4)))

;;; more args
     (equal? '#() (vscan-left-ex + 0 '#() '#() '#() '#() '#()))
     (equal? '#(1 (1 1 1 1 1 1) ((1 1 1 1 1 1) 2 2 2 2 2)
                  (((1 1 1 1 1 1) 2 2 2 2 2) 3 3 3 3 3))
             (vscan-left-ex (lambda (acc . x*) (cons acc x*))
                            1 '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4)))


;;;; fxvector
;;; 1 arg
     (equal? '#vfx() (fxvscan-left-ex + 0 '#vfx()))
     (equal? '#vfx(0 1 3 6)
             (fxvscan-left-ex + 0 '#vfx(1 2 3 4)))
     (equal? '#vfx(1 1 2 6) (fxvscan-left-ex * 1 '#vfx(1 2 3 4)))

;;; 2 args
     (equal? '#vfx() (fxvscan-left-ex + 0 '#vfx() '#vfx()))
     (equal? '#vfx(1 3 7 13)
             (fxvscan-left-ex (lambda (acc x y) (+ acc x y))
                              1 '#vfx(1 2 3 4) '#vfx(1 2 3 4)))

;;; 3 args
     (equal? '#vfx() (fxvscan-left-ex + 0 '#vfx() '#vfx() '#vfx()))
     (equal? '#vfx(1 4 10 19)
             (fxvscan-left-ex (lambda (acc x y z) (+ acc x y z))
                              1 '#vfx(1 2 3 4) '#vfx(1 2 3 4) '#vfx(1 2 3 4)))

;;; more args
     (equal? '#vfx() (fxvscan-left-ex + 0 '#vfx() '#vfx() '#vfx() '#vfx() '#vfx()))
     (equal? '#vfx(1 6 16 31)
             (fxvscan-left-ex (lambda (acc . x*) (+ acc (apply + x*)))
                              1 '#vfx(1 2 3 4) '#vfx(1 2 3 4) '#vfx(1 2 3 4) '#vfx(1 2 3 4) '#vfx(1 2 3 4)))


;;;; flvector
;;; 1 arg
     (equal? '#vfl() (flvscan-left-ex + 0.0 '#vfl()))
     (equal? '#vfl(0.0 1.0 3.0 6.0)
             (flvscan-left-ex + 0.0 '#vfl(1.0 2.0 3.0 4.0)))
     (equal? '#vfl(1.0 1.0 2.0 6.0) (flvscan-left-ex * 1.0 '#vfl(1.0 2.0 3.0 4.0)))

;;; 2 args
     (equal? '#vfl() (flvscan-left-ex + 0.0 '#vfl() '#vfl()))
     (equal? '#vfl(1.0 3.0 7.0 13.0)
             (flvscan-left-ex (lambda (acc x y) (+ acc x y))
                              1.0 '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0)))

;;; 3 args
     (equal? '#vfl() (flvscan-left-ex + 0.0 '#vfl() '#vfl() '#vfl()))
     (equal? '#vfl(1.0 4.0 10.0 19.0)
             (flvscan-left-ex (lambda (acc x y z) (+ acc x y z))
                              1.0 '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0)))

;;; more args
     (equal? '#vfl() (flvscan-left-ex + 0.0 '#vfl() '#vfl() '#vfl() '#vfl() '#vfl()))
     (equal? '#vfl(1.0 6.0 16.0 31.0)
             (flvscan-left-ex (lambda (acc . x*) (+ acc (apply + x*)))
                              1.0 '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0)))

     )


(mat *vscan-left-in

     (error? (vscan-left-in + 0))
     (error? (vscan-left-in + 0 '()))
     (error? (vscan-left-in 'bla 0 '#()))

     (error? (fxvscan-left-in + 0))
     (error? (fxvscan-left-in + 0 '()))
     (error? (fxvscan-left-in 'bla 0 '#()))
     ;; not fxvector
     (error? (fxvscan-left-in + 0 '#()))

     (error? (flvscan-left-in + 0))
     (error? (flvscan-left-in + 0 '()))
     (error? (flvscan-left-in 'bla 0 '#()))
     ;; not flvector
     (error? (flvscan-left-in + 0.0 '#()))

;;;; vector
;;; 1 arg
     (equal? '#() (vscan-left-in + 0 '#()))
     (equal? '#(1 3 6 10)
             (vscan-left-in + 0 '#(1 2 3 4)))
     (equal? '#(1 2 6 24) (vscan-left-in * 1 '#(1 2 3 4)))

;;; 2 args
     (equal? '#() (vscan-left-in + 0 '#() '#()))
     (equal? '#(3 7 13 21)
             (vscan-left-in (lambda (acc x y) (+ acc x y))
                            1 '#(1 2 3 4) '#(1 2 3 4)))

;;; 3 args
     (equal? '#() (vscan-left-in + 0 '#() '#() '#()))
     (equal? '#(4 10 19 31)
             (vscan-left-in (lambda (acc x y z) (+ acc x y z))
                            1 '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4)))

;;; more args
     (equal? '#() (vscan-left-in + 0 '#() '#() '#() '#() '#()))
     (equal? '#((1 1 1 1 1 1) ((1 1 1 1 1 1) 2 2 2 2 2)
                (((1 1 1 1 1 1) 2 2 2 2 2) 3 3 3 3 3)
                ((((1 1 1 1 1 1) 2 2 2 2 2) 3 3 3 3 3) 4 4 4 4 4))
             (vscan-left-in (lambda (acc . x*) (cons acc x*))
                            1 '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4)))


;;;; fxvector
;;; 1 arg
     (equal? '#vfx() (fxvscan-left-in + 0 '#vfx()))
     (equal? '#vfx(1 3 6 10)
             (fxvscan-left-in + 0 '#vfx(1 2 3 4)))
     (equal? '#vfx(1 2 6 24) (fxvscan-left-in * 1 '#vfx(1 2 3 4)))

;;; 2 args
     (equal? '#vfx() (fxvscan-left-in + 0 '#vfx() '#vfx()))
     (equal? '#vfx(3 7 13 21)
             (fxvscan-left-in (lambda (acc x y) (+ acc x y))
                              1 '#vfx(1 2 3 4) '#vfx(1 2 3 4)))

;;; 3 args
     (equal? '#vfx() (fxvscan-left-in + 0 '#vfx() '#vfx() '#vfx()))
     (equal? '#vfx(4 10 19 31)
             (fxvscan-left-in (lambda (acc x y z) (+ acc x y z))
                              1 '#vfx(1 2 3 4) '#vfx(1 2 3 4) '#vfx(1 2 3 4)))

;;; more args
     (equal? '#vfx() (fxvscan-left-in + 0 '#vfx() '#vfx() '#vfx() '#vfx() '#vfx()))
     (equal? '#vfx(6 16 31 51)
             (fxvscan-left-in (lambda (acc . x*) (+ acc (apply + x*)))
                              1 '#vfx(1 2 3 4) '#vfx(1 2 3 4) '#vfx(1 2 3 4) '#vfx(1 2 3 4) '#vfx(1 2 3 4)))


;;;; flvector
;;; 1 arg
     (equal? '#vfl() (flvscan-left-in + 0.0 '#vfl()))
     (equal? '#vfl(1.0 3.0 6.0 10.0)
             (flvscan-left-in + 0.0 '#vfl(1.0 2.0 3.0 4.0)))
     (equal? '#vfl(1.0 2.0 6.0 24.0) (flvscan-left-in * 1.0 '#vfl(1.0 2.0 3.0 4.0)))

;;; 2 args
     (equal? '#vfl() (flvscan-left-in + 0.0 '#vfl() '#vfl()))
     (equal? '#vfl(3.0 7.0 13.0 21.0)
             (flvscan-left-in (lambda (acc x y) (+ acc x y))
                              1.0 '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0)))

;;; 3 args
     (equal? '#vfl() (flvscan-left-in + 0.0 '#vfl() '#vfl() '#vfl()))
     (equal? '#vfl(4.0 10.0 19.0 31.0)
             (flvscan-left-in (lambda (acc x y z) (+ acc x y z))
                              1.0 '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0)))

;;; more args
     (equal? '#vfl() (flvscan-left-in + 0.0 '#vfl() '#vfl() '#vfl() '#vfl() '#vfl()))
     (equal? '#vfl(6.0 16.0 31.0 51.0)
             (flvscan-left-in (lambda (acc . x*) (+ acc (apply + x*)))
                              1.0 '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0)))

     )


(mat *vscan-right-ex

     (error? (vscan-right-ex + 0))
     (error? (vscan-right-ex + 0 '()))
     (error? (vscan-right-ex 'bla 0 '#()))

     (error? (fxvscan-right-ex + 0))
     (error? (fxvscan-right-ex + 0 '()))
     (error? (fxvscan-right-ex 'bla 0 '#()))
     ;; not fxvector
     (error? (fxvscan-right-ex + 0 '#()))

     (error? (flvscan-right-ex + 0))
     (error? (flvscan-right-ex + 0 '()))
     (error? (flvscan-right-ex 'bla 0 '#()))
     ;; not flvector
     (error? (flvscan-right-ex + 0.0 '#()))

;;;; vector
;;; 1 arg
     (equal? '#() (vscan-right-ex + 0 '#()))
     (equal? '#(0 4 7 9)
             (vscan-right-ex + 0 '#(1 2 3 4)))
     (equal? '#(1 4 12 24) (vscan-right-ex * 1 '#(1 2 3 4)))

;;; 2 args
     (equal? '#() (vscan-right-ex + 0 '#() '#()))
     (equal? '#(1 9 15 19)
             (vscan-right-ex (lambda (acc x y) (+ acc x y))
                             1 '#(1 2 3 4) '#(1 2 3 4)))

;;; 3 args
     (equal? '#() (vscan-right-ex + 0 '#() '#() '#()))
     (equal? '#(1 13 22 28)
             (vscan-right-ex (lambda (acc x y z) (+ acc x y z))
                             1 '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4)))

;;; more args
     (equal? '#() (vscan-right-ex + 0 '#() '#() '#() '#() '#()))
     (equal? '#(1 (4 4 4 4 4 1) (3 3 3 3 3 (4 4 4 4 4 1))
                  (2 2 2 2 2 (3 3 3 3 3 (4 4 4 4 4 1))))
             (vscan-right-ex (lambda (acc . x*) (cons acc x*))
                             1 '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4)))


;;;; fxvector
;;; 1 arg
     (equal? '#vfx() (fxvscan-right-ex + 0 '#vfx()))
     (equal? '#vfx(0 4 7 9)
             (fxvscan-right-ex + 0 '#vfx(1 2 3 4)))
     (equal? '#vfx(1 4 12 24) (fxvscan-right-ex * 1 '#vfx(1 2 3 4)))

;;; 2 args
     (equal? '#vfx() (fxvscan-right-ex + 0 '#vfx() '#vfx()))
     (equal? '#vfx(1 9 15 19)
             (fxvscan-right-ex (lambda (acc x y) (+ acc x y))
                               1 '#vfx(1 2 3 4) '#vfx(1 2 3 4)))

;;; 3 args
     (equal? '#vfx() (fxvscan-right-ex + 0 '#vfx() '#vfx() '#vfx()))
     (equal? '#vfx(1 13 22 28)
             (fxvscan-right-ex (lambda (acc x y z) (+ acc x y z))
                               1 '#vfx(1 2 3 4) '#vfx(1 2 3 4) '#vfx(1 2 3 4)))

;;; more args
     (equal? '#vfx() (fxvscan-right-ex + 0 '#vfx() '#vfx() '#vfx() '#vfx() '#vfx()))
     (equal? '#vfx(1 21 36 46)
             (fxvscan-right-ex (lambda (acc . x*) (+ acc (apply + x*)))
                               1 '#vfx(1 2 3 4) '#vfx(1 2 3 4) '#vfx(1 2 3 4) '#vfx(1 2 3 4) '#vfx(1 2 3 4)))


;;;; flvector
;;; 1 arg
     (equal? '#vfl() (flvscan-right-ex + 0.0 '#vfl()))
     (equal? '#vfl(0.0 4.0 7.0 9.0)
             (flvscan-right-ex + 0.0 '#vfl(1.0 2.0 3.0 4.0)))
     (equal? '#vfl(1.0 4.0 12.0 24.0) (flvscan-right-ex * 1.0 '#vfl(1.0 2.0 3.0 4.0)))

;;; 2 args
     (equal? '#vfl() (flvscan-right-ex + 0.0 '#vfl() '#vfl()))
     (equal? '#vfl(1.0 9.0 15.0 19.0)
             (flvscan-right-ex (lambda (acc x y) (+ acc x y))
                               1.0 '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0)))

;;; 3 args
     (equal? '#vfl() (flvscan-right-ex + 0.0 '#vfl() '#vfl() '#vfl()))
     (equal? '#vfl(1.0 13.0 22.0 28.0)
             (flvscan-right-ex (lambda (acc x y z) (+ acc x y z))
                               1.0 '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0)))

;;; more args
     (equal? '#vfl() (flvscan-right-ex + 0.0 '#vfl() '#vfl() '#vfl() '#vfl() '#vfl()))
     (equal? '#vfl(1.0 21.0 36.0 46.0)
             (flvscan-right-ex (lambda (acc . x*) (+ acc (apply + x*)))
                               1.0 '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0)))

     )


(mat *vscan-right-in

     (error? (vscan-right-in + 0))
     (error? (vscan-right-in + 0 '()))
     (error? (vscan-right-in 'bla 0 '#()))

     (error? (fxvscan-right-in + 0))
     (error? (fxvscan-right-in + 0 '()))
     (error? (fxvscan-right-in 'bla 0 '#()))
     ;; not fxvector
     (error? (fxvscan-right-in + 0 '#()))

     (error? (flvscan-right-in + 0))
     (error? (flvscan-right-in + 0 '()))
     (error? (flvscan-right-in 'bla 0 '#()))
     ;; not flvector
     (error? (flvscan-right-in + 0.0 '#()))

;;;; vector
;;; 1 arg
     (equal? '#() (vscan-right-in + 0 '#()))
     (equal? '#(4 7 9 10)
             (vscan-right-in + 0 '#(1 2 3 4)))
     (equal? '#(4 12 24 24) (vscan-right-in * 1 '#(1 2 3 4)))

;;; 2 args
     (equal? '#() (vscan-right-in + 0 '#() '#()))
     (equal? '#(9 15 19 21)
             (vscan-right-in (lambda (acc x y) (+ acc x y))
                             1 '#(1 2 3 4) '#(1 2 3 4)))

;;; 3 args
     (equal? '#() (vscan-right-in + 0 '#() '#() '#()))
     (equal? '#(13 22 28 31)
             (vscan-right-in (lambda (acc x y z) (+ acc x y z))
                             1 '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4)))

;;; more args
     (equal? '#() (vscan-right-in + 0 '#() '#() '#() '#() '#()))
     (equal? '#((4 4 4 4 4 1) (3 3 3 3 3 (4 4 4 4 4 1))
                (2 2 2 2 2 (3 3 3 3 3 (4 4 4 4 4 1)))
                (1 1 1 1 1 (2 2 2 2 2 (3 3 3 3 3 (4 4 4 4 4 1)))))
             (vscan-right-in (lambda (acc . x*) (cons acc x*))
                             1 '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4) '#(1 2 3 4)))


;;;; fxvector
;;; 1 arg
     (equal? '#vfx() (fxvscan-right-in + 0 '#vfx()))
     (equal? '#vfx(4 7 9 10)
             (fxvscan-right-in + 0 '#vfx(1 2 3 4)))
     (equal? '#vfx(4 12 24 24) (fxvscan-right-in * 1 '#vfx(1 2 3 4)))

;;; 2 args
     (equal? '#vfx() (fxvscan-right-in + 0 '#vfx() '#vfx()))
     (equal? '#vfx(9 15 19 21)
             (fxvscan-right-in (lambda (acc x y) (+ acc x y))
                               1 '#vfx(1 2 3 4) '#vfx(1 2 3 4)))

;;; 3 args
     (equal? '#vfx() (fxvscan-right-in + 0 '#vfx() '#vfx() '#vfx()))
     (equal? '#vfx(13 22 28 31)
             (fxvscan-right-in (lambda (acc x y z) (+ acc x y z))
                               1 '#vfx(1 2 3 4) '#vfx(1 2 3 4) '#vfx(1 2 3 4)))

;;; more args
     (equal? '#vfx() (fxvscan-right-in + 0 '#vfx() '#vfx() '#vfx() '#vfx() '#vfx()))
     (equal? '#vfx(21 36 46 51)
             (fxvscan-right-in (lambda (acc . x*) (+ acc (apply + x*)))
                               1 '#vfx(1 2 3 4) '#vfx(1 2 3 4) '#vfx(1 2 3 4) '#vfx(1 2 3 4) '#vfx(1 2 3 4)))


;;;; flvector
;;; 1 arg
     (equal? '#vfl() (flvscan-right-in + 0.0 '#vfl()))
     (equal? '#vfl(4.0 7.0 9.0 10.0)
             (flvscan-right-in + 0.0 '#vfl(1.0 2.0 3.0 4.0)))
     (equal? '#vfl(4.0 12.0 24.0 24.0) (flvscan-right-in * 1.0 '#vfl(1.0 2.0 3.0 4.0)))

;;; 2 args
     (equal? '#vfl() (flvscan-right-in + 0.0 '#vfl() '#vfl()))
     (equal? '#vfl(9.0 15.0 19.0 21.0)
             (flvscan-right-in (lambda (acc x y) (+ acc x y))
                               1.0 '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0)))

;;; 3 args
     (equal? '#vfl() (flvscan-right-in + 0.0 '#vfl() '#vfl() '#vfl()))
     (equal? '#vfl(13.0 22.0 28.0 31.0)
             (flvscan-right-in (lambda (acc x y z) (+ acc x y z))
                               1.0 '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0)))

;;; more args
     (equal? '#vfl() (flvscan-right-in + 0.0 '#vfl() '#vfl() '#vfl() '#vfl() '#vfl()))
     (equal? '#vfl(21.0 36.0 46.0 51.0)
             (flvscan-right-in (lambda (acc . x*) (+ acc (apply + x*)))
                               1.0 '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0) '#vfl(1.0 2.0 3.0 4.0)))


     )
