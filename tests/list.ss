(import (chezpp list)
        (chezpp string))

(mat make-list-builder

     (equal? '() ((make-list-builder)))

     (equal? '(1 2 3)
             ((make-list-builder 1 2 3)))

     (equal? '(1 2 3)
             (let ([lb (make-list-builder)])
               (lb 1)
               (lb 2)
               (lb 3)
               (lb)))

     (equal? '(1 2 3)
             (let ([lb (make-list-builder)])
               (lb 1 2 3)
               (lb)))


     )



(mat unique

     ;; test whether `b` is a permutation of `a`
     (begin (define (permutation-of? a b)
              (if (and (= (length a) (length b))
                       (andmap (lambda (x) (memq x b)) a))
                  #t #f))
            #t)

     (error? (unique?))
     (error? (unique? 1 2))
     (error? (unique? #t))

     (unique? '())
     (unique? (iota 10))
     (not (unique? '(1 1)))
     (not (unique? '(#f #f)))
     (not (unique? '(() ())))


     (error? (unique))
     (error? (unique 1 2))
     (error? (unique #t))

     (eq? '() (unique '()))
     (permutation-of? (iota 10) (unique (iota 10)))
     (permutation-of? '(#f) (unique '(#f #f #f)))
     (permutation-of? '(#f #t) (unique '(#f #t #f #f #t)))
     (permutation-of? '(a c b d) (unique '(a b c d c a b d c d a b)))

     )


(mat iterations

     ;; map!
     (error? (map!))
     (error? (map! +))
     (error? (map! + '() '(1)))
     (error? (map! cons '(1) '(1) '()))

     (eq? '() (map! + '()))
     (let ([l (iota 10)])
       (eq? l (map! add1 l)))
     (let ([l (iota 10)])
       (map! add1 l)
       (equal? l (map add1 (iota 10))))


     ;; map!/i
     (error? (map!/i))
     (error? (map!/i +))

     (eq? '() (map!/i + '()))
     (equal? '((0 . 1)) (map!/i cons '(1)))
     (let ([l (iota 10)])
       (eq? l (map!/i list l)))
     (let ([l (iota 10)])
       (map!/i list l)
       (equal? l (zip (iota 10) (iota 10))))
     (let ([l (iota 10)])
       (map!/i list l l l)
       (equal? l (zip (iota 10) (iota 10) (iota 10) (iota 10))))


     ;; for-each/i
     (error? (for-each/i))
     (error? (for-each/i +))

     (eq? '()
          (let ([lb (make-list-builder)])
            (for-each/i (lambda (i x) (lb i)) '())
            (lb)))

     (equal? '(0 1 2)
             (let ([lb (make-list-builder)])
               (for-each/i (lambda (i x) (lb i)) '(a b c))
               (lb)))

     (equal? (zip (iota 10) (iota 10))
             (let ([lb (make-list-builder)])
               (for-each/i (lambda (i x) (lb (list i x))) (iota 10))
               (lb)))

     )


(mat folds

     (error? (fold-left/i))
     (error? (fold-left/i +))
     (error? (fold-left/i + '()))
     ;; proc arity error
     (error? (fold-left/i (lambda (i acc) (+ i acc)) 0
                          (iota 10)))
     (error? (fold-left/i (lambda (i acc a b) (+ i acc)) 0
                          (iota 9) (iota 10)))
     (error? (fold-left/i (lambda (i acc a b c d) (+ i acc)) 0
                          (iota 7) (iota 8) (iota 9) (iota 10)))
     ;; improper list
     (error? (fold-left/i (lambda (i acc x) (+ i acc)) 0 '(1 2 3 . 4)))

     (fold-left/i + #t '())
     (fold-left/i + #t '() '())
     (fold-left/i + #t '() '() '() '() '())
     (= (apply + (iota 10))
        (fold-left/i (lambda (i acc x) (+ i acc)) 0 (iota 10)))
     (equal? '(2 (1 (0 0 a) b) c)
             (fold-left/i (lambda (i acc x) (list i acc x)) 0
                          '(a b c)))
     (error? (fold-left/i (lambda (i acc x) (list i acc x)) 0
                          '(a b c) '(a b c) '(a b c) '(a b c)))
     (equal? '(2 (1 (0 #f a a a a) b b b b) c c c c)
             (fold-left/i (lambda (i acc a b c d) (list i acc a b c d)) #f
                          '(a b c) '(a b c) '(a b c) '(a b c)))
     (equal? (reverse (iota 10))
             (fold-left/i (lambda (i acc x) (cons x acc)) '() (iota 10)))


     (error? (fold-right/i))
     (error? (fold-right/i +))
     (error? (fold-right/i + '()))
     (error? (fold-right/i (lambda (i a) (+ i acc)) 0 (iota 10)))
     (error? (fold-right/i (lambda (i a b acc) (+ i acc)) 0 (iota 9) (iota 10)))
     (error? (fold-right/i (lambda (i a b c d acc) (+ i acc)) 0 (iota 7) (iota 8) (iota 9) (iota 10)))
     ;; improper list
     (error? (fold-right/i (lambda (i a acc) (+ i acc)) 0 '(1 2 3 . 4)))

     (fold-right/i + #t '())
     (fold-right/i + #t '() '())
     (fold-right/i + #t '() '() '() '() '())
     (= (apply + (iota 10))
        (fold-right/i (lambda (i a acc) (+ i acc)) 0 (iota 10)))
     (equal? '(0 (1 (2 0 c) b) a)
             (fold-right/i (lambda (i x acc) (list i acc x)) 0 '(a b c)))
     (equal? '(0 (1 (2 #f c c c c) b b b b) a a a a)
             (fold-right/i (lambda (i a b c d acc) (list i acc a b c d)) #f
                           '(a b c) '(a b c) '(a b c) '(a b c)))

     (equal? '((((((((((#f))))))))))
             (fold-left/i (lambda (i acc x) (list acc)) #f (iota 10)))
     (equal? '((((((((((#f))))))))))
             (fold-right/i (lambda (i x acc) (list acc)) #f (iota 10)))
     (equal? (iota 10)
             (fold-right/i (lambda (i x acc) (cons x acc)) '() (iota 10)))

     )


(mat slice


     (equal? '() (slice '() 1))
     (equal? '() (slice '() 0))
     (equal? '() (slice '() -1))
     (equal? '() (slice '() 3 1))
     (equal? '() (slice '(1) 0 1 -1))

     (equal? '(1) (slice '(1) 1))
     (equal? '(1) (slice '(1) 2))
     (equal? '(1) (slice '(1) 3))
     (equal? '(1) (slice '(1) 2 -4 -4))

     (equal? '() (slice '(1) 2 -4))
     (equal? '() (slice '(1) 1 5))
     (equal? '() (slice '(1) 3 0 3))

     (begin (define ls (iota 10))
            #t)


     (equal? '(8 5 2)
             (slice ls 8 0 -3))
     (equal? '(9 6 3)
             (slice ls 9 0 -3))

     ;; positive index, forward
     (equal? '(0 1 2 3 4)
             (slice ls 5))
     (equal? '(0 2 4 6 8)
             (slice ls 0 9 2))
     (equal? '(2 5 8)
             (slice ls 2 9 3))

     ;; positive index, backward
     (equal? '(3 2)
             (slice ls 3 1 -1))
     (equal? '(8 6 4)
             (slice ls 8 2 -2))
     (equal? '(9 6 3)
             (slice ls 9 1 -3))
     (equal? '(9 5)
             (slice ls 9 1 -4))
     (equal? '(9 5 1)
             (slice ls 9 0 -4))

     ;; negative index, forward
     (equal? '(0 1 2 3 4)
             (slice ls -5))
     (equal? (iota 9)
             (slice ls -1))
     (equal? '(5 6 7 8)
             (slice ls -5 -1))
     (equal? '(9)
             (slice ls -1 -2 -1))
     (equal? '(1 2 3 4 5 6 7 8)
             (slice ls -9 -1))
     (equal? '(1 4 7)
             (slice ls -9 -1 3))

     ;; negative index, backward
     (equal? '(9 8 7 6)
             (slice ls -1 -5 -1))
     (equal? '(9 7 5 3)
             (slice ls -1 -9 -2))
     (equal? '(8 4)
             (slice ls -2 -9 -4))
     ;; single item
     (equal? '(1)
             (slice '(1) -1 -2 -1))


     ;; errors
     (error? (slice 42))
     (error? (slice (iota 10) 1 7 0))
     )


(mat zip!

     (error? (zip!))
     (error? (zip! 1))
     (error? (zip! '()))
     (error? (zip! '() '(1)))

     (equal? '() (zip! '() '()))

     (let* ([ls1 (iota 10)]
            [ls2 (iota 10)]
            [ls (zip! ls1 ls2)])
       (and (equal? (zip ls2 ls2)
                    ls)
            (eq? ls ls1)))

     (let* ([ls1 (iota 10)]
            [ls2 (iota 10)]
            [ls (zip! ls1 ls2 ls2 ls2 ls2)])
       (and (equal? (zip ls2 ls2 ls2 ls2 ls2)
                    ls)
            (eq? ls ls1)))

     )


(mat nums

     (error? (nums 'x 'x 'x))
     (error? (nums 0 10  -1))
     (error? (nums 0 -10 1))

     (equal? (nums 0 10) (iota 10))
     (equal? (nums 5 10) '(5 6 7 8 9))
     (equal? (nums 5 15 3) '(5 8 11 14))
     (equal? (nums 0 -10 -1) '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))
     (equal? (nums 0 -10 -2) '(0 -2 -4 -6 -8))
     (equal? (nums 0 5 0.5) '(0 0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5))
     (equal? (nums 0 -5 -0.5) '(0 -0.5 -1.0 -1.5 -2.0 -2.5 -3.0 -3.5 -4.0 -4.5))

     )


(mat list-last

     (error? (list-last 1))
     (error? (list-last '()))

     (equal? 1 (list-last '(1)))
     (equal? 1 (list-last '(2 1)))
     (equal? 1 (list-last '(3 2 1)))

     )


(mat list-set!

     (error? (list-set! '()))
     (error? (list-set! '(1) -1 0))
     (error? (list-set! '(1) 1  0))
     (error? (list-set! '(1 2) 2 0))

     (let ([ls (iota 5)])
       (list-set! ls 4 55)
       (println ls)
       (equal? ls '(0 1 2 3 55)))

     (let ([ls (list 1 2 3)])
       (list-set! ls 0 11)
       (list-set! ls 2 33)
       (println ls)
       (equal? ls '(11 2 33)))

     ;; literal list not equal
     (let ([ls '(1 2 3)])
       (list-set! ls 0 11)
       (list-set! ls 2 33)
       (not (equal? ls '(11 2 33))))


     )


(mat set-ops

     ;; relations
     (begin (define ls1 (iota 3))
            (define ls2 (iota 5))
            (define ls3 (iota 7))
            #t)

     (error? (list=?))
     (list=? '())
     (list=? '() '())
     (list=? '() '() '())
     (list=? ls1)
     (list=? ls1 ls1)
     (list=? ls1 '(1 2 0) '(0 0 1 1 2 2))
     (not (list=? ls1 ls2))
     (not (list=? ls1 ls2 ls3))

     (error? (list<?))
     (list<? '())
     (not (list<? '() '()))
     (not (list<? '() '() '()))
     (list<? ls1)
     (list<? ls1 ls2)
     (list<? ls1 '(0 1 2 3 4 4 3 2 1))
     (list<? ls1 ls2 ls3)
     (not (list<? ls1 ls1))
     (not (list<? ls1 ls2 ls2))
     (not (list<? ls2 ls1))

     (error? (list<=?))
     (list<=? '())
     (list<=? '() '())
     (list<=? '() '() '())
     (list<=? ls1)
     (list<=? ls1 ls1)
     (list<=? ls1 ls2)
     (list<=? ls1 '(0 1 2 3 4 4 3 2 1))
     (list<=? ls1 ls2 ls3)
     (not (list<=? ls2 ls1))
     (not (list<=? '(0 1 2 3 4 4 3 2 1) ls1))
     (not (list<=? ls3 ls2 ls1))

     (error? (list>?))
     (list<=? '())
     (list<=? '() '())
     (list<=? '() '() '())
     (list>? ls1)
     (list>? ls2 ls1)
     (list>? '(0 1 2 3 4 4 3 2 1) ls1)
     (list>? ls3 ls2 ls1)
     (not (list>? ls1 ls1))
     (not (list>? ls1 ls2 ls2))
     (not (list>? ls1 ls2))

     (error? (list>=?))
     (list>=? '())
     (list>=? '() '())
     (list>=? '() '() '())
     (list>=? ls1)
     (list>=? ls1 ls1)
     (list>=? ls2 ls1)
     (list>=? '(0 1 2 3 4 4 3 2 1) ls1)
     (list>=? ls3 ls2 ls1)
     (not (list>=? ls1 ls2))
     (not (list>=? ls1 '(0 1 2 3 4 4 3 2 1)))
     (not (list>=? ls1 '(0 1 2 3 4 4 3 2 1) ls1))
     (not (list>=? ls1 ls2 ls3))


     ;; union
     (let ([ls1 (list 1 2 3 4 5 6)]
           [ls2 (list 4 5 6 7 8 9)]
           [ls3 (list 4 6 5 4 8 9)])
       (list=? (list+ ls1 ls2 ls3)
               (list 1 2 3 4 5 6 7 8 9)))

     (let ([ls1 (apply list (nums 0  50))]
           [ls2 (apply list (nums 30 80))]
           [ls3 (apply list (nums 40 90))])
       (list=? (apply list (nums 0 90))
               (list+ ls1 ls2 ls3)))

     ;; difference
     (let ([ls1 (list 1 2 3 4 5 6)]
           [ls2 (list 4 5 6 7 8 9)])
       (list=? (list- ls1 ls2)
               (list 1 2 3)))

     (let ([ls1 (list 1 2 3 4 5 6)]
           [ls2 (list 4 5 6 7 8 9)]
           [ls3 (list 4 6 5 4 8 9 1)])
       (list=? (list- ls1 ls2 ls3)
               (list 2 3)))

     (let ([ls1 (apply list (nums 0  50))]
           [ls2 (apply list (nums 30 80))]
           [ls3 (apply list (nums 40 90))])
       (list=? (apply list (nums 0 30))
               (list- ls1 ls2 ls3)))

     ;; intersection
     (let ([ls1 (list 1 2 3 4 5 6)]
           [ls2 (list 4 5 6 7 8 9)]
           [ls3 (list 4 6 5 4 8 9)])
       (list=? (list& ls1 ls2 ls3)
               (list 4 5 6)))

     (let ([ls1 (apply list (nums 0  50))]
           [ls2 (apply list (nums 30 80))]
           [ls3 (apply list (nums 40 90))])
       (list=? (apply list (nums 40 50))
               (list& ls1 ls2 ls3)))

     ;; symmetric difference
     (let ([ls1 (list 1 2 3 4 5 6)]
           [ls2 (list 4 5 6 7 8 9)])
       (list=? (list^ ls1 ls2)
               (list 1 2 3 7 8 9)))


     ;; equations
     (let ([ls1 (list 1 2 3 4 5 6)]
           [ls2 (list 4 5 6 7 8 9)]
           [ls3 (list 7 8 9 10 11 12 4 5 6)]
           [ls4 (list 4 6 7 8 2 5)])
       (println (list=? (append ls1 ls2 ls3 ls4) (list+ ls1 ls2 ls3 ls4)))
       (list=? (list^ ls1 ls2 ls3 ls4)
               (list- (list+ ls1 ls2 ls3 ls4) (list& ls1 ls2 ls3 ls4))))

     (let* ([ls* (map (lambda (i) (random-list (lambda () (random 999)) 1000 2000)) (iota 10))]
            [ls* (map (lambda (ls) (apply list ls)) ls*)])
       (list=? (apply list^ ls*)
               (list- (apply list+ ls*) (apply list& ls*))))

     )
