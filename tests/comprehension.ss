(import (chezpp comprehension)
        (chezpp iter)
        (chezpp string)
        (chezpp utils)
        (chezpp list))

(mat for

     ;; `for` returns void
     (eq? (void)
          (for ([i (nums 10 2)])
            #t))

     ;; single clause
     (equal? '(9 8 7 6 5 4 3 2 1 0)
             (let ([res '()])
               (for ([i (nums 10)])
                 (cons! i res))
               res))


     ;; multi-clause
     (equal?
      '((0 1) (2 3) (4 5) (6 7) (8 9))
      (let ([res '()])
        (for ([i (nums 0 10 2)]
              [j (range 1 10 2 )])
          (cons! (list i j) res))
        (reverse res)))

     (let ([res '()] [^2 (sect expt _ 2)])
       (for ([i (range 1 100)])
         (for ([j (range 1 100)])
           (for ([k (range 1 100)
                    (guard (fx= (+ (^2 i) (^2 j)) (^2 k)))])
             (cons! (list i j k) res))))
       (andmap (lambda (ns)
                 (let ([i (car ns)] [j (cadr ns)] [k (caddr ns)])
                   (fx= (+ (^2 i) (^2 j))
                        (^2 k))))
               res))

     )


(mat for/list

     (equal? (iota 10)
             (for/list ([i (range 10)]) i))

     (equal? '((0 0) (1 1) (2 2))
             (for/list ([i (range 10)]
                        [j (range 3)])
               (list i j)))

     (equal? '((1 0) (3 2) (5 4) (7 6) (9 8))
             (for/list ([i (range 10) (guard (odd? i))]
                        [j (range 10) (guard (even? j))])
               (list i j)))

     (equal? '(0 1)
             (for/list ([i (range 10)])
               (break (= i 2))
               i))

     (equal? '(#\b #\l #\a)
             (for/list ([i "bla"]) i))
     (equal? '(a b c)
             (for/list ([i '#(a b c)]) i))

     (equal? '((#\b a) (#\l b) (#\a c))
             (for/list ([i "bla"]
                        [j '#(a b c)])
               (list i j)))

     (equal? '((#\b a) (#\l b))
              (for/list ([i "bl"]
                         [j '#(a b c)])
                (list i j)))

     )


(mat for/xxx

     (= (for/sum ([i (range 10)]) i)
        45)
     (fx= (for/fxsum ([i (range 10)]) i)
          45)
     (fl= (for/flsum ([i (range 0.0 10)]) i)
          45.0)

     (= (for/product ([i (range 1 10)]) i)
        362880)
     (fx= (for/fxproduct ([i (range 1 10)]) i)
          362880)
     (fl= (for/flproduct ([i (range 1.0 10)]) i)
          362880.0)

     )


(mat for*

     (let* ([^2 (sect expt _ 2)]
            [py* (for*/list ([i (range 100)]
                             [j (range 100)]
                             [k (range 100)
                                ;; var ref to previous vars in guards
                                (guard (= (+ (^2 i) (^2 j))
                                          (^2 k)))])
                   (list i j k))])
       (andmap (lambda (ns)
                 (let ([i (car ns)] [j (cadr ns)] [k (caddr ns)])
                   (fx= (+ (^2 i) (^2 j))
                        (^2 k))))
               py*))

     (let* ([^2 (sect expt _ 2)]
            [py* (for*/list ([i (range 1 15)]
                             [j (range 1 15)]
                             [k (range 1 15) (guard (= (+ (^2 i) (^2 j))
                                                       (^2 k)))])
                   (break (>= i 13))
                   (list i j k))])
       (equal? '((3 4 5) (4 3 5) (5 12 13) (6 8 10) (8 6 10) (12 5 13))
               py*))


     ;; make sure iter expressions are evaluated only once
     (let* ([x 0]
            [y (for*/list ([i (range 2)]
                           [j (begin (incr! x) (range 2))])
                 (list i j))])
       (and (= x 1)
            (equal? '((0 0) (0 1) (1 0) (1 1)) y)))

     )



(mat for*/xxx

     #t

     )
