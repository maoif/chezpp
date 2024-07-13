(import (chezpp iter)
        (chezpp control)
        (chezpp string)
        (chezpp utils))

(mat iterators

     ;; range
     (let ([r (range 4)])
       (and (eq? 0 (iter-next! r))
            (eq? 1 (iter-next! r))
            (eq? 2 (iter-next! r))
            (eq? 3 (iter-next! r))
            (eq? iter-end (iter-next! r))))

     (let ([r (range 4 8)])
       (and (eq? 4 (iter-next! r))
            (eq? 5 (iter-next! r))
            (eq? 6 (iter-next! r))
            (eq? 7 (iter-next! r))
            (eq? iter-end (iter-next! r))))

     (let ([r (range 4 10 2)])
       (and (eq? 4 (iter-next! r))
            (eq? 6 (iter-next! r))
            (eq? 8 (iter-next! r))
            (eq? iter-end (iter-next! r))))

     (eq? iter-end (iter-next! (range 10 0)))


     ;; nums
     (eq? '() (nums 10 0))
     (equal? (iota 10) (nums 10))
     (equal? '(1 3 5 7 9) (nums 1 10 2))
     (equal? '(0.0 0.5 1.0 1.5) (nums 0.0 2.0 0.5))

     ;; conversions
     (eq? iter-end (iter-next! (list->iter '())))
     (eq? iter-end (iter-next! (list->iter '(1 2) 0 0)))
     (eq? iter-end (iter-next! (list->iter '(1 2 3) 3 0)))

     (eq? iter-end (iter-next! (vector->iter '#())))
     (eq? iter-end (iter-next! (vector->iter '#(1 2) 0 0)))
     (eq? iter-end (iter-next! (vector->iter '#(1 2 3) 3 0)))

     (eq? iter-end (iter-next! (string->iter "")))
     (eq? iter-end (iter-next! (string->iter "123" 0 0)))
     (eq? iter-end (iter-next! (string->iter "1234" 3 0)))

     (equal? (iter->list (range 0 100 5))
             (iter->list (list->iter (iota 100) 0 100 5)))
     (equal? (nums 0 100 2)
             (iter->list (list->iter (iota 100) 0 100 2)))
     (equal? (nums 1 100 2)
             (iter->list (list->iter (iota 100) 1 100 2)))

     (equal? (iter->list (range 0 100 5))
             (iter->list (vector->iter (list->vector (iota 100)) 0 100 5)))
     (equal? (nums 0 100 2)
             (iter->list (vector->iter (list->vector (iota 100)) 0 100 2)))
     (equal? (nums 1 100 2)
             (iter->list (vector->iter (list->vector (iota 100)) 1 100 2)))

     (let ([s "abc123345jskljdla"])
       (equal? (string->list s) (iter->list (string->iter s))))
     (let ([s "abc123345jskljdla"])
       (equal? (iter->list (list->iter (string->list s) 1 30 3))
               (iter->list (string->iter s 1 30 3))))

     )


(mat iter-ops

     (equal? '(0 1 2 3)
             (iter->list (range 4)))

     (equal? '(0 1 4 9)
             (iter->list (iter-map (sect expt _ 2) (range 4))))

     (equal? '(0 2 4 6 8)
             (iter->list (iter-filter even? (range 10))))

     (let ([it (range 10)]
           [it1 (range 10)])
       (equal? '((1 0) (3 2) (5 4) (7 6) (9 8))
               (iter->list (iter-zip (iter-filter odd? it)
                                     (iter-filter even? it1)))))

     (let ([it (range 5)]
           [it1 (range 5)])
       (equal? '(1 3 0 2 4)
               (iter->list (iter-append (iter-filter odd? it)
                                        (iter-filter even? it1)))))

     (equal? '(0 1 2) (iter->list (iter-take 3 (range 20))))
     (equal? '(17 18 19) (iter->list (iter-drop 17 (range 20))))

     (let ([it (range 50)]
           [it1 (range 50)])
       (equal? '((1 0) (9 4) (25 16))
               (iter->list (iter-zip (iter-take 3 (iter-filter odd? (iter-map (sect expt _ 2) it)))
                                     (iter-take 3 (iter-filter even? (iter-map (sect expt _ 2) it1)))))))

     (= 6
        (iter-fold (lambda (acc x) (+ acc x)) 0 (range 4)))

     (= 999
        (iter-fold (lambda (acc x) (if acc (if (> x acc) x acc) x))
                   #f
                   (range 1000)))

     (= 999 (iter-max (range 1000)))
     (= 0 (iter-min (range 1000)))
     (= 999 (iter-min > (range 1000)))
     (= 0 (iter-max < (range 1000)))

     (= (apply + (iota 10000))
        (iter-sum (range 10000)))
     (= (apply * (iota 10000))
        (iter-product (range 10000)))
     (= (/ (apply + (iota 10000)) 10000)
        (iter-avg (range 10000)))

     (fx= (apply fx+ (iota 10000))
          (iter-fxsum (range 10000)))
     (fx= (apply * (iota 10000))
          (iter-fxproduct (range 10000)))
     (= (fx/ (apply fx+ (iota 10000)) 10000)
        (iter-fxavg (range 10000)))


     (fl= (apply fl+ (nums 0.0 10000))
          (iter-flsum (range 0.0 10000)))
     (fl= (apply fl* (nums 0.0 10000))
          (iter-flproduct (range 0.0 10000)))
     (fl= (/ (apply fl+ (nums 0.0 10000)) 10000)
          (iter-flavg (range 0.0 10000)))

     (and (not (iter-avg (range 10 0)))
          (not (iter-fxavg (range 10 0)))
          (not (iter-flavg (range 10.0 0))))

     )


