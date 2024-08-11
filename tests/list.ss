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