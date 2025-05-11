(import (chezpp)
        (chezpp queue))

(mat queue

     (queue? (make-queue))
     (queue? (queue 1 2 3 4 5))
     (queue-empty? (make-queue))
     (not (queue-empty? (queue 1 2 3)))
     (= 10 (queue-size (apply queue (iota 10))))
     (= 3 (queue-peek (queue 3 2 1)))

     (error? (queue-peek (queue)))
     (error? (queue-pop! (queue)))

     (let ([s (make-queue)])
       (queue-push! s 1)
       (queue-push! s 2)
       (queue-push! s 3)
       (and (= 1 (queue-pop! s))
            (= 2 (queue-pop! s))
            (= 3 (queue-pop! s))))

     (let ([s (make-queue)])
       (queue-push! s 1 2 3)
       (and (= 1 (queue-pop! s))
            (= 2 (queue-pop! s))
            (= 3 (queue-pop! s))))

     (let ([s (queue 1 2 3)])
       (and (= 1 (queue-pop! s))
            (= 2 (queue-pop! s))
            (= 3 (queue-pop! s))))

     (let ([s (queue 1 2 3)])
       (= 1 (queue-peek s)))

     (let* ([n (iota 10)]
            [s (apply queue n)])
       (equal? (queue-pop-all! s)
               n))

     (let* ([n (iota 10)]
            [s (make-queue)])
       (apply queue-push! s n)
       (equal? (queue-pop-all! s)
               n))

     (let ([q (queue 3 2 1)])
       (queue-clear! q)
       (and (= 0 (queue-size q))
            (queue-empty? q)))

     )


(mat queue-copy

     (error? (queue-copy))
     (error? (queue-copy '()))

     (let* ([q (apply queue (iota 100))]
            [newq (queue-copy q)])
       (and (equal? q newq)
            (not (eq? q newq))))

     )


(mat queue-contains?

     (error? (queue-contains? '() 1))
     (not (queue-contains? (queue) 1))


     (let* ([n* (iota 100)]
            [q (apply queue n*)])
       (andmap (lambda (n)
                 (queue-contains? q n))
               n*))

     )


(mat queue-contains/p?

     (error? (queue-contains/p? '() 1))
     (error? (queue-contains/p? (queue) 1))

     (let* ([n* (iota 100)]
            [q (apply queue n*)])
       (andmap (lambda (n)
                 (queue-contains/p? q (lambda (x) (= x n))))
               n*))

     )
