(import (chezpp)
        (chezpp dset))


(mat dset

     (begin (define (test1 n)
              (let ([ds (make-dset n)])
                (= n (dset-size ds))))
            #t)
     (test1 0)
     (test1 10)
     (test1 100)

     (begin (define (test2 n)
              (let ([ds (make-dset n)])
                (for-each (lambda (x)
                            (when (< (+ x 5) n)
                              (dset-union! ds x (+ x 5))))
                          (nums 0 n 5))
                (for-each (lambda (x)
                            (when (< (+ x 5) n)
                              (dset-union! ds x (+ x 5))))
                          (nums 1 n 5))
                (for-each (lambda (x)
                            (when (< (+ x 5) n)
                              (dset-union! ds x (+ x 5))))
                          (nums 2 n 5))
                (for-each (lambda (x)
                            (when (< (+ x 5) n)
                              (dset-union! ds x (+ x 5))))
                          (nums 3 n 5))
                (for-each (lambda (x)
                            (when (< (+ x 5) n)
                              (dset-union! ds x (+ x 5))))
                          (nums 4 n 5))
                (and (= 5 (dset-size ds))
                     (apply dset-same? ds (nums 0 n 5))
                     (apply dset-same? ds (nums 1 n 5))
                     (apply dset-same? ds (nums 2 n 5))
                     (apply dset-same? ds (nums 3 n 5))
                     (apply dset-same? ds (nums 4 n 5)))))
            #t)
     (test2 5)
     (test2 10)
     (test2 100)
     (test2 1000)

     (begin (define (test3 n)
              (let ([ds (make-dset n)])
                (let loop ([i 0])
                  (if (fx= i n)
                      (and (= 2 (dset-size ds))
                           (apply dset-same? ds 1 (nums 1 n 2))
                           (apply dset-same? ds 0 (nums 0 n 2)))
                      (begin (if (odd? i)
                                 (dset-union! ds 1 i)
                                 (dset-union! ds 0 i))
                             (loop (fx1+ i)))))))
            #t)
     (test3 10)
     (test3 100)
     (test3 1000)

     ;; union the same item
     (begin (define ds (make-dset 5))
            (dset-union! ds 0 0)
            (dset-union! ds 0 0)
            (dset-union! ds 4 4)
            (dset-union! ds 4 4)
            (dset-union! ds 0 4)
            (dset-union! ds 2 3)

            (and (= 3 (dset-size ds))
                 (dset-same? ds 4 0)
                 (dset-same? ds 2 3)))

     )
