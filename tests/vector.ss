(import (chezpp vector)
        (chezpp list)
        (chezpp control)
        (chezpp string)
        (chezpp utils))

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
       (println v)
       (vcopy! v 0 v 3 3)
       (println v)
       (equal? '#(0 1 2 0 1 2 6 7 8 9) v))

     (let ([v (apply vector (iota 10))])
       (println v)
       (vcopy! v 0 v 5 3)
       (println v)
       (equal? '#(0 1 2 3 4 0 1 2 8 9) v))

     (let ([v (apply vector (iota 10))])
       (println v)
       (vcopy! v 0 v 5 5)
       (println v)
       (equal? '#(0 1 2 3 4 0 1 2 3 4) v))

     ;; disjoint, right to left
     (let ([v (apply vector (iota 10))])
       (println v)
       (vcopy! v 5 v 0 3)
       (println v)
       (equal? '#(5 6 7 3 4 5 6 7 8 9) v))

     (let ([v (apply vector (iota 10))])
       (println v)
       (vcopy! v 3 v 0 3)
       (println v)
       (equal? '#(3 4 5 3 4 5 6 7 8 9) v))

     (let ([v (apply vector (iota 10))])
       (println v)
       (vcopy! v 5 v 0 5)
       (println v)
       (equal? '#(5 6 7 8 9 5 6 7 8 9) v))

     ;; overlapping, left to right
     (let ([v (apply vector (iota 10))])
       (println v)
       (vcopy! v 0 v 2 3)
       (println v)
       (equal? '#(0 1 0 1 2 5 6 7 8 9) v))

     (let ([v (apply vector (iota 10))])
       (println v)
       (vcopy! v 0 v 0 5)
       (println v)
       (equal? '#(0 1 2 3 4 5 6 7 8 9) v))

     (let ([v (apply vector (iota 10))])
       (println v)
       (vcopy! v 0 v 1 5)
       (println v)
       (equal? '#(0 0 1 2 3 4 6 7 8 9) v))

     (let ([v (apply vector (iota 10))])
       (println v)
       (vcopy! v 0 v 3 5)
       (println v)
       (equal? '#(0 1 2 0 1 2 3 4 8 9) v))

     ;; overlapping , right to left
     (let ([v (apply vector (iota 10))])
       (println v)
       (vcopy! v 2 v 0 3)
       (println v)
       (equal? '#(2 3 4 3 4 5 6 7 8 9) v))

     (let ([v (apply vector (iota 10))])
       (println v)
       (vcopy! v 2 v 0 5)
       (println v)
       (equal? '#(2 3 4 5 6 5 6 7 8 9) v))

     (let ([v (apply vector (iota 10))])
       (println v)
       (vcopy! v 5 v 3 5)
       (println v)
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
