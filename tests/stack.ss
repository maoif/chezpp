(import (chezpp)
        (chezpp stack))

(mat stack

     (stack? (make-stack))
     (stack? (stack 1 2 3 4 5))
     (stack-empty? (make-stack))
     (not (stack-empty? (stack 1 2 3)))
     (= 10 (stack-size (apply stack (iota 10))))
     (= 1 (stack-peek (stack 3 2 1)))

     (error? (stack-peek (stack)))
     (error? (stack-pop! (stack)))

     (let ([s (make-stack)])
       (stack-push! s 1)
       (stack-push! s 2)
       (stack-push! s 3)
       (and (= 3 (stack-pop! s))
            (= 2 (stack-pop! s))
            (= 1 (stack-pop! s))))

     (let ([s (make-stack)])
       (stack-push! s 1 2 3)
       (and (= 3 (stack-pop! s))
            (= 2 (stack-pop! s))
            (= 1 (stack-pop! s))))

     (let ([s (stack 1 2 3)])
       (and (= 3 (stack-pop! s))
            (= 2 (stack-pop! s))
            (= 1 (stack-pop! s))))

     (let ([s (stack 1 2 3)])
       (= 3 (stack-peek s)))

     (let* ([n (iota 10)]
            [s (apply stack n)])
       (equal? (stack-pop-all! s)
               (reverse n)))

     (let* ([n (iota 10)]
            [s (make-stack)])
       (apply stack-push! s n)
       (equal? (stack-pop-all! s)
               (reverse n)))

     (let ([stk (stack 3 2 1)])
       (stack-clear! stk)
       (and (= 0 (stack-size stk))
            (stack-empty? stk)))

     )


(mat stack-copy

     (error? (stack-copy))
     (error? (stack-copy '()))

     (let* ([stk (apply stack (iota 100))]
            [newstk (stack-copy stk)])
       (and (equal? stk newstk)
            (not (eq? stk newstk))))

     )


(mat stack-contains?

     (error? (stack-contains? '() 1))
     (not (stack-contains? (stack) 1))


     (let* ([n* (iota 100)]
            [stk (apply stack n*)])
       (andmap (lambda (n)
                 (stack-contains? stk n))
               n*))

     )


(mat stack-contains/p?

     (error? (stack-contains/p? '() 1))
     (error? (stack-contains/p? (stack) 1))

     (let* ([n* (iota 100)]
            [stk (apply stack n*)])
       (andmap (lambda (n)
                 (stack-contains/p? stk (lambda (x) (= x n))))
               n*))

     )
