(import (chezpp))


(mat array

     (let ([arr (apply array (iota 10))])
       (fx= (array-length arr) 10))

     (let ([arr (apply fxarray (iota 10))])
       (fx= (fxarray-length arr) 10))

     (let ([arr (apply u8array (iota 10))])
       (fx= (u8array-length arr) 10))



     )


(mat array-add!

     (error? (array-add! (array) 1 1))
     (error? (array-add! (array 1) 2 1))
     (error? (array-add! (array) -1 1))

     (error? (fxarray-add! (fxarray) 'c))
     (error? (fxarray-add! (fxarray) 0.0))
     (error? (u8array-add! (u8array) 'c))
     (error? (u8array-add! (u8array) 0.0))

     ;; `array` uses `mincap`, hence `make-array`
     (let ([arr (make-array 0 0)])
       (println arr)
       (array-add! arr 0)
       (array-add! arr 1)
       (array-add! arr 2)
       (array-add! arr 0 -1)
       (array-add! arr 0 -2)
       (array-add! arr 1 100)
       (array-add! arr 1 200)
       (array-add! arr 3 300)
       (array-add! arr 4 400)
       (println arr)
       (and (= 9 (array-length arr))
            (equal? '(-2 200 100 300 400 -1 0 1 2) (array->list arr))))

     (let ([arr (array)] [n 9999])
       (let loop ([i 0])
         (if (fx= i n)
             (and (fx= n (array-length arr))
                  (equal? (iota n) (array->list arr)))
             (begin (array-add! arr i)
                    (loop (fx1+ i))))))

     (let ([arr (make-array 0 0)] [n 9999])
       (let loop ([i 0])
         (if (fx= i n)
             (and (fx= n (array-length arr))
                  (equal? (iota n) (array->list arr)))
             (begin (array-add! arr i)
                    (loop (fx1+ i))))))

     )


(mat array-set!

     (error? (array-set! (array) 1 1))
     (error? (array-set! (array 1) 2 1))
     (error? (array-set! (array) -1 1))

     (let* ([v (random-vector 100 200)]
            [arr (make-array (vector-length v))])
       (vfor-each/i (lambda (i x) (array-set! arr i x)) v)
       (let loop ([i 0])
         (if (fx= i (array-length arr))
             #t
             (and (equal? (vector-ref v i) (array-ref arr i))
                  (loop (fx1+ i))))))

     )


(mat array-clear!

     (error? (array-clear! 42))

     (let ([arr (array 1)])
       (array-clear! arr)
       (array-empty? arr))

     (let ([arr (apply array (iota 10))])
       (array-clear! arr)
       (array-empty? arr))
     )


(mat array-delete!

     (error? (array-delete! 42 42))
     (error? (array-delete! (array) 0))
     (error? (array-delete! (array) 1))
     (error? (array-delete! (array 1) 1))

     ;; delete first
     (let ([arr (array 1)])
       (array-delete! arr 0)
       (array-empty? arr))

     (let ([arr (array 0 1)])
       (array-delete! arr 0)
       (and (= 1 (array-length arr))
            (= 1 (array-ref arr 0))))

     (let ([arr (array 0 1 2)])
       (array-delete! arr 0)
       (and (= 2 (array-length arr))
            (= 1 (array-ref arr 0))))

     ;; delete last
     (let ([arr (array 0 1)])
       (array-delete! arr 1)
       (and (= 1 (array-length arr))
            (= 0 (array-ref arr 0))))

     (let ([arr (array 0 1 2)])
       (array-delete! arr 2)
       (and (= 2 (array-length arr))
            (= 0 (array-ref arr 0))))

     )


(mat array-contains?


     (error? (array-contains? 42 42))
     (error? (array-contains? (array)))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (bool (andmap (lambda (x) (array-contains? arr x)) ls)))

     (let* ([ls (random-list 20 30)]
            [arr (apply array ls)])
       (bool (andmap (lambda (x) (array-contains? arr x)) ls)))

     )


(mat array-contains/p?

     (error? (array-contains/p? = 42))
     (error? (array-contains/p? odd? (array)))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (bool (and (array-contains/p? arr odd?)
                  (array-contains/p? arr even?))))

     )


(mat array-search

     (error? (array-search 42 42))
     (error? (array-search odd? (array)))

     (let* ([ls (iota 10)]
            [arr (apply array ls)])
       (and (= 0 (array-search arr even?))
            (= 1 (array-search arr odd?))))

     (let ([arr (array "1" "11" "111" "1111")])
       (and (string=? "1"   (array-search arr (lambda (x) (= 1 (string-length x)))))
            (string=? "111" (array-search arr (lambda (x) (= 3 (string-length x)))))
            (string=? "11"  (array-search arr (lambda (x) (<= 2 (string-length x)))))))

     )


(mat array-search*

     (error? (array-search* 42 42))
     (error? (array-search* odd? (array)))


     (let ([arr (array "1" "11" "111" "1111")])
       (and (equal? '("1")
                    (array-search* arr (lambda (x) (= 1 (string-length x)))))
            (equal? '("111")
                    (array-search* arr (lambda (x) (= 3 (string-length x)))))
            (equal? '("11" "111" "1111")
                    (array-search* arr (lambda (x) (<= 2 (string-length x)))))))

     ;; custom collector
     (let ([arr (array "1" "11" "111" "1111")])
       (and (equal? (array "1")
                    (let ([col (array)])
                      (array-search* arr (lambda (x) (= 1 (string-length x))) (lambda (x) (array-add! col x)))
                      col))
            (equal? (array "111")
                    (let ([col (array)])
                      (array-search* arr (lambda (x) (= 3 (string-length x))) (lambda (x) (array-add! col x)))
                      col))
            (equal? (array "11" "111" "1111")
                    (let ([col (array)])
                      (array-search* arr (lambda (x) (<= 2 (string-length x))) (lambda (x) (array-add! col x)))
                      col))))

     )


(mat array-slice


     #t)


(mat array-slice!


     #t)


(mat array-copy

     (error? (array-copy))
     (error? (array-copy #f))

     (error? (fxarray-copy))
     (error? (fxarray-copy #f))

     (error? (u8array-copy))
     (error? (u8array-copy #f))

     (let* ([arr (apply array (iota 10))]
            [newarr (array-copy arr)])
       (and (equal? arr newarr)
            (not (eq? arr newarr))))

     (let* ([arr (apply fxarray (iota 10))]
            [newarr (fxarray-copy arr)])
       (and (equal? arr newarr)
            (not (eq? arr newarr))))

     (let* ([arr (apply u8array (iota 10))]
            [newarr (u8array-copy arr)])
       (and (equal? arr newarr)
            (not (eq? arr newarr))))

     )


(mat array-stack-ops

     (error? (array-pop! (array)))
     (error? (array-pop-back! (array)))

     (begin (define ls (iota 10000))
            #t)

     (let* ([arr (array)])
       (for-each (lambda (x) (array-push! arr x)) ls)
       (equal? (array->list arr) (reverse ls)))

     (let* ([arr (array)])
       (for-each (lambda (x) (array-push! arr x)) ls)
       (equal? (let loop ([res '()])
                 (if (array-empty? arr)
                     res
                     (loop (cons (array-pop! arr) res))))
               ls))


     (let* ([arr (array)])
       (for-each (lambda (x) (array-push-back! arr x)) ls)
       (equal? (array->list arr) ls))

     (let* ([arr (array)])
       (for-each (lambda (x) (array-push-back! arr x)) ls)
       (equal? (let loop ([res '()])
                 (if (array-empty? arr)
                     res
                     (loop (cons (array-pop-back! arr) res))))
               ls))


     )


(mat array-copy!


;;;; same array

     ;; disjoint, left to right
     (let ([arr (apply array (iota 10))])
       (array-copy! arr 0 arr 3 3)
       (equal? (array 0 1 2 0 1 2 6 7 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 0 arr 5 3)
       (equal? (array 0 1 2 3 4 0 1 2 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 0 arr 5 5)
       (equal? (array 0 1 2 3 4 0 1 2 3 4) arr))

     ;; disjoint, right to left
     (let ([arr (apply array (iota 10))])
       (array-copy! arr 5 arr 0 3)
       (equal? (array 5 6 7 3 4 5 6 7 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 3 arr 0 3)
       (equal? (array 3 4 5 3 4 5 6 7 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 5 arr 0 5)
       (equal? (array 5 6 7 8 9 5 6 7 8 9) arr))

     ;; overlapping, left to right
     (let ([arr (apply array (iota 10))])
       (array-copy! arr 0 arr 2 3)
       (equal? (array 0 1 0 1 2 5 6 7 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 0 arr 0 5)
       (equal? (array 0 1 2 3 4 5 6 7 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 0 arr 1 5)
       (equal? (array 0 0 1 2 3 4 6 7 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 0 arr 3 5)
       (equal? (array 0 1 2 0 1 2 3 4 8 9) arr))

     ;; overlapping , right to left
     (let ([arr (apply array (iota 10))])
       (array-copy! arr 2 arr 0 3)
       (equal? (array 2 3 4 3 4 5 6 7 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 2 arr 0 5)
       (equal? (array 2 3 4 5 6 5 6 7 8 9) arr))

     (let ([arr (apply array (iota 10))])
       (array-copy! arr 5 arr 3 5)
       (equal? (array 0 1 2 5 6 7 8 9 8 9) arr))



;;;; different array

     (let ([arr (apply array (iota 10))]
           [arr1 (make-array 10 #f)])
       (array-copy! arr 0 arr1 0 3)
       (equal? (array 0 1 2 #f #f #f #f #f #f #f) arr1))

     (let ([arr (apply array (iota 10))]
           [arr1 (make-array 10 #f)])
       (array-copy! arr 0 arr1 0 5)
       (equal? (array 0 1 2 3 4 #f #f #f #f #f) arr1))

     (let ([arr (apply array (iota 10))]
           [arr1 (make-array 10 #f)])
       (array-copy! arr 0 arr1 3 5)
       (equal? (array #f #f #f 0 1 2 3 4 #f #f) arr1))

     (let ([arr (apply array (iota 10))]
           [arr1 (make-array 10 #f)])
       (array-copy! arr 2 arr1 0 5)
       (equal? (array 2 3 4 5 6 #f #f #f #f #f) arr1))


     )


(mat fxarray-copy!


;;;; same fxarray

     ;; disjoint, left to right
     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 0 arr 3 3)
       (equal? (fxarray 0 1 2 0 1 2 6 7 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 0 arr 5 3)
       (equal? (fxarray 0 1 2 3 4 0 1 2 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 0 arr 5 5)
       (equal? (fxarray 0 1 2 3 4 0 1 2 3 4) arr))

     ;; disjoint, right to left
     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 5 arr 0 3)
       (equal? (fxarray 5 6 7 3 4 5 6 7 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 3 arr 0 3)
       (equal? (fxarray 3 4 5 3 4 5 6 7 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 5 arr 0 5)
       (equal? (fxarray 5 6 7 8 9 5 6 7 8 9) arr))

     ;; overlapping, left to right
     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 0 arr 2 3)
       (equal? (fxarray 0 1 0 1 2 5 6 7 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 0 arr 0 5)
       (equal? (fxarray 0 1 2 3 4 5 6 7 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 0 arr 1 5)
       (equal? (fxarray 0 0 1 2 3 4 6 7 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 0 arr 3 5)
       (equal? (fxarray 0 1 2 0 1 2 3 4 8 9) arr))

     ;; overlapping , right to left
     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 2 arr 0 3)
       (equal? (fxarray 2 3 4 3 4 5 6 7 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 2 arr 0 5)
       (equal? (fxarray 2 3 4 5 6 5 6 7 8 9) arr))

     (let ([arr (apply fxarray (iota 10))])
       (fxarray-copy! arr 5 arr 3 5)
       (equal? (fxarray 0 1 2 5 6 7 8 9 8 9) arr))



;;;; different fxarray

     (let ([arr (apply fxarray (iota 10))]
           [arr1 (make-fxarray 10 -1)])
       (fxarray-copy! arr 0 arr1 0 3)
       (equal? (fxarray 0 1 2 -1 -1 -1 -1 -1 -1 -1) arr1))

     (let ([arr (apply fxarray (iota 10))]
           [arr1 (make-fxarray 10 -1)])
       (fxarray-copy! arr 0 arr1 0 5)
       (equal? (fxarray 0 1 2 3 4 -1 -1 -1 -1 -1) arr1))

     (let ([arr (apply fxarray (iota 10))]
           [arr1 (make-fxarray 10 -1)])
       (fxarray-copy! arr 0 arr1 3 5)
       (equal? (fxarray -1 -1 -1 0 1 2 3 4 -1 -1) arr1))

     (let ([arr (apply fxarray (iota 10))]
           [arr1 (make-fxarray 10 -1)])
       (fxarray-copy! arr 2 arr1 0 5)
       (equal? (fxarray 2 3 4 5 6 -1 -1 -1 -1 -1) arr1))


     )


(mat *array-sorted?

     (array-sorted? < (array))
     (fxarray-sorted? < (fxarray))
     (u8array-sorted? < (u8array))

     (array-sorted? < (array 42))
     (fxarray-sorted? < (fxarray 42))
     (u8array-sorted? < (u8array 42))

     (error? (array-sorted? 1 (array)))
     (error? (fxarray-sorted? 1 (fxarray)))
     (error? (u8array-sorted? 1 (u8array)))
     (error? (array-sorted? 1 (array 42)))
     (error? (fxarray-sorted? 1 (fxarray 42)))
     (error? (u8array-sorted? 1 (u8array 42)))

     (error? (array-sorted? < '()))
     (error? (fxarray-sorted? < '()))
     (error? (u8array-sorted? < '()))
     (error? (array-sorted? < '()))
     (error? (fxarray-sorted? < '()))
     (error? (u8array-sorted? < '()))

     (begin (define (test n)
              (let ([arr (apply array (iota n))])
                (and (array-sorted? < arr)
                     (not (array-sorted? > arr)))))
            (define (fxtest n)
              (let ([arr (apply fxarray (iota n))])
                (and (fxarray-sorted? < arr)
                     (not (fxarray-sorted? > arr)))))
            (define (u8test n)
              (let ([arr (apply u8array (iota n))])
                (and (u8array-sorted? < arr)
                     (not (u8array-sorted? > arr)))))
            #t)

     (test 10)
     (test 100)
     (test 1000)

     (fxtest 10)
     (fxtest 100)
     (fxtest 1000)

     (u8test 10)
     (u8test 100)
     (u8test 255)


     )


(mat *array-sort

     ;; bad <?
     (error? (array-sort!   1 (array)))
     (error? (fxarray-sort! 1 (fxarray)))
     ;; not arrays
     (error? (array-sort!   <= '(1 2 3)))
     (error? (fxarray-sort! <= '(1 2 3)))
     ;; bad range
     (error? (array-sort!   <= (array) 3))
     (error? (fxarray-sort! <= (fxarray) 3))
     (error? (array-sort!   <= (array   2 2 2 2 2) 6 3))
     (error? (fxarray-sort! <= (fxarray 2 2 2 2 2) 6 3))

     ;; bad <?
     (error? (array-sort   1 (array)))
     (error? (fxarray-sort 1 (fxarray)))
     ;; not arrays
     (error? (array-sort   <= '(1 2 3)))
     (error? (fxarray-sort <= '(1 2 3)))
     ;; bad range
     (error? (array-sort   <= (array) 3))
     (error? (fxarray-sort <= (fxarray) 3))
     (error? (array-sort   <= (array   2 2 2 2 2) 6 3))
     (error? (fxarray-sort <= (fxarray 2 2 2 2 2) 6 3))


     ;; full range, in place
     (begin (define (test1 rand <? sort! sorted? v->a)
              (andmap (lambda (bd)
                        (andmap (lambda (i)
                                  (let* ([v (rand #e1e6 bd)] [arr (v->a v)])
                                    (sort! <? arr)
                                    (sorted? <? arr)))
                                (iota 3)))
                      '(100 1000 10000 100000)))
            #t)
     (test1 random-vector   fx<= array-sort!   array-sorted?   vector->array)
     (test1 random-fxvector fx<= fxarray-sort! fxarray-sorted? fxvector->fxarray)


     ;; full range, return new
     (begin (define (test2 rand <? sort sorted? v->a)
              (andmap (lambda (bd)
                        (andmap (lambda (i)
                                  (let* ([v (rand #e1e6 bd)] [arr (v->a v)])
                                    (sorted? <? (sort <? arr))))
                                (iota 3)))
                      '(100 1000 10000 100000)))
            #t)
     (test2 random-vector   fx<= array-sort   array-sorted?   vector->array)
     (test2 random-fxvector fx<= fxarray-sort fxarray-sorted? fxvector->fxarray)



     ;; ranged, in place
     (begin (define (test3 rand <? sort! sorted? v->a)
              (andmap (lambda (bd)
                        (andmap (lambda (i)
                                  (let* ([v (rand #e1e6 bd)]
                                         [mid (fx/ #e1e6 2)]
                                         [arr (v->a v)])
                                    (sort! <? arr mid)
                                    (sort! <? arr mid #e1e6)
                                    (and (sorted? <? arr 0 mid)
                                         (sorted? <? arr mid #e1e6))))
                                (iota 3)))
                      '(100 1000 10000 100000)))
            #t)
     (test3 random-vector   fx<= array-sort!   array-sorted?   vector->array)
     (test3 random-fxvector fx<= fxarray-sort! fxarray-sorted? fxvector->fxarray)


     ;; ranged, return new
     (begin (define (test4 rand <? sort sorted? v->a)
              (andmap (lambda (bd)
                        (andmap (lambda (i)
                                  (let* ([v (rand #e1e6 bd)]
                                         [mid (fx/ #e1e6 2)]
                                         [arr (v->a v)])
                                    (and (sorted? <? (sort <? arr mid))
                                         (sorted? <? (sort <? arr mid #e1e6)))))
                                (iota 3)))
                      '(100 1000 10000 100000)))
            #t)
     (test4 random-vector   fx<= array-sort   array-sorted?   vector->array)
     (test4 random-fxvector fx<= fxarray-sort fxarray-sorted? fxvector->fxarray)

     )


(mat array<->vector

     (error? (array->vector))
     (error? (fxarray->fxvector))
     (error? (u8array->u8vector))

     (error? (array->vector '()))
     (error? (fxarray->fxvector '()))
     (error? (u8array->u8vector '()))

     (error? (array->vector '#()))
     (error? (fxarray->fxvector '#()))
     (error? (u8array->u8vector '#()))

     (let ([v (random-vector 100)])
       (equal? v (array->vector (vector->array v))))

     (let ([v (random-fxvector 100)])
       (equal? v (fxarray->fxvector (fxvector->fxarray v))))

     (let ([v (random-u8vec 100 200)])
       (equal? v (u8array->u8vector (u8vector->u8array v))))


     (let ([v (random-vector 10000)])
       (equal? v (array->vector (vector->array v))))

     (let ([v (random-fxvector 10000)])
       (equal? v (fxarray->fxvector (fxvector->fxarray v))))

     (let ([v (random-u8vec 100 20000)])
       (equal? v (u8array->u8vector (u8vector->u8array v))))

     )


(mat array-map

     ;; type error
     (error? (array-map #f (array)))
     (error? (array-map odd? (fxarray 1)))
     (error? (fxarray-map odd? (array 1)))

     (array-empty? (array-map   + (array)))
     (array-empty? (fxarray-map + (fxarray)))
     (array-empty? (u8array-map + (u8array)))

     (array-empty? (array-map   + (array) (array)))
     (array-empty? (fxarray-map + (fxarray) (fxarray)))
     (array-empty? (u8array-map + (u8array) (u8array)))

     (array-empty? (array-map   + (array) (array) (array) (array) (array)))
     (array-empty? (fxarray-map + (fxarray) (fxarray) (fxarray) (fxarray) (fxarray)))
     (array-empty? (u8array-map + (u8array) (u8array) (u8array) (u8array) (u8array)))

     ;; length not equal
     (error? (array-map   + (array) (array 1)))
     (error? (fxarray-map + (fxarray) (fxarray 1)))
     (error? (u8array-map + (u8array) (u8array 1)))

     (error? (array-map   + (array) (array 1) (array) (array 1 1) (array)))
     (error? (fxarray-map + (fxarray) (fxarray 1) (fxarray) (fxarray 1 1) (fxarray)))
     (error? (u8array-map + (u8array) (u8array 1) (u8array) (u8array 1 1) (u8array)))


     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (apply array (map add1 ls0))
               (array-map add1 arr0)))


     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (apply array (map + ls0 ls0))
               (array-map + arr0 arr0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (apply array (map + ls0 ls0))
               (array-map + arr0 arr1)))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (apply array (map + ls0 ls0 ls0 ls0 ls0))
               (array-map + arr0 arr1 arr2 arr3 arr4)))
     )


(mat array-map/i

     ;; type error
     (error? (array-map/i #f (array)))
     (error? (array-map/i odd? (fxarray 1)))
     (error? (fxarray-map/i odd? (array 1)))

     ;; arity error
     (error? (fxarray-map/i odd? (fxarray 1)))

     (array-empty? (array-map/i   + (array)))
     (array-empty? (fxarray-map/i + (fxarray)))
     (array-empty? (u8array-map/i + (u8array)))

     (array-empty? (array-map/i   + (array) (array)))
     (array-empty? (fxarray-map/i + (fxarray) (fxarray)))
     (array-empty? (u8array-map/i + (u8array) (u8array)))

     (array-empty? (array-map/i   + (array) (array) (array) (array) (array)))
     (array-empty? (fxarray-map/i + (fxarray) (fxarray) (fxarray) (fxarray) (fxarray)))
     (array-empty? (u8array-map/i + (u8array) (u8array) (u8array) (u8array) (u8array)))

     ;; length not equal
     (error? (array-map/i   + (array) (array 1)))
     (error? (fxarray-map/i + (fxarray) (fxarray 1)))
     (error? (u8array-map/i + (u8array) (u8array 1)))

     (error? (array-map/i   + (array) (array 1) (array) (array 1 1) (array)))
     (error? (fxarray-map/i + (fxarray) (fxarray 1) (fxarray) (fxarray 1 1) (fxarray)))
     (error? (u8array-map/i + (u8array) (u8array 1) (u8array) (u8array 1 1) (u8array)))


     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-map/i (lambda (i x) (list i x)) arr0)
               (apply array (zip ls0 ls0))))


     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-map/i (lambda (i x y) (list i x y)) arr0 arr1)
               (apply array (zip ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-map/i (lambda (i x y) (list i x y)) arr0 arr0)
               (apply array (zip ls0 ls0 ls0))))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (array-map/i (lambda (i x0 x1 x2 x3 x4) (list i x0 x1 x2 x3 x4)) arr0 arr1 arr2 arr3 arr4)
               (apply array (zip ls0 ls0 ls0 ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-map/i (lambda (i x0 x1 x2 x3 x4) (list i x0 x1 x2 x3 x4)) arr0 arr0 arr0 arr0 arr0)
               (apply array (zip ls0 ls0 ls0 ls0 ls0 ls0))))


     )


(mat array-map!

     ;; type error
     (error? (array-map! #f (array)))
     (error? (array-map! odd? (fxarray 1)))
     (error? (fxarray-map! odd? (array 1)))

     (array-empty? (array-map!   + (array)))
     (array-empty? (fxarray-map! + (fxarray)))
     (array-empty? (u8array-map! + (u8array)))

     (array-empty? (array-map!   + (array) (array)))
     (array-empty? (fxarray-map! + (fxarray) (fxarray)))
     (array-empty? (u8array-map! + (u8array) (u8array)))

     (array-empty? (array-map!   + (array) (array) (array) (array) (array)))
     (array-empty? (fxarray-map! + (fxarray) (fxarray) (fxarray) (fxarray) (fxarray)))
     (array-empty? (u8array-map! + (u8array) (u8array) (u8array) (u8array) (u8array)))

     ;; length not equal
     (error? (array-map!   + (array) (array 1)))
     (error? (fxarray-map! + (fxarray) (fxarray 1)))
     (error? (u8array-map! + (u8array) (u8array 1)))

     (error? (array-map!   + (array) (array 1) (array) (array 1 1) (array)))
     (error? (fxarray-map! + (fxarray) (fxarray 1) (fxarray) (fxarray 1 1) (fxarray)))
     (error? (u8array-map! + (u8array) (u8array 1) (u8array) (u8array 1 1) (u8array)))


     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (apply array (map add1 ls0))
               (array-map! add1 arr0)))


     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (apply array (map + ls0 ls0))
               (array-map! + arr0 arr1)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (array-map! + arr0 arr0)
       (equal? (apply array (map + ls0 ls0))
               arr0))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (apply array (map + ls0 ls0 ls0 ls0 ls0))
               (array-map! + arr0 arr1 arr2 arr3 arr4)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (apply array (map + ls0 ls0 ls0 ls0 ls0))
               (array-map! + arr0 arr0 arr0 arr0 arr0)))

     )


(mat array-map/i!

     ;; type error
     (error? (array-map/i! #f (array)))
     (error? (array-map/i! odd? (fxarray 1)))
     (error? (fxarray-map/i! odd? (array 1)))

     ;; arity error
     (error? (fxarray-map/i! odd? (fxarray 1)))

     (array-empty? (array-map/i!   + (array)))
     (array-empty? (fxarray-map/i! + (fxarray)))
     (array-empty? (u8array-map/i! + (u8array)))

     (array-empty? (array-map/i!   + (array) (array)))
     (array-empty? (fxarray-map/i! + (fxarray) (fxarray)))
     (array-empty? (u8array-map/i! + (u8array) (u8array)))

     (array-empty? (array-map/i!   + (array) (array) (array) (array) (array)))
     (array-empty? (fxarray-map/i! + (fxarray) (fxarray) (fxarray) (fxarray) (fxarray)))
     (array-empty? (u8array-map/i! + (u8array) (u8array) (u8array) (u8array) (u8array)))

     ;; length not equal
     (error? (array-map/i!   + (array) (array 1)))
     (error? (fxarray-map/i! + (fxarray) (fxarray 1)))
     (error? (u8array-map/i! + (u8array) (u8array 1)))

     (error? (array-map/i!   + (array) (array 1) (array) (array 1 1) (array)))
     (error? (fxarray-map/i! + (fxarray) (fxarray 1) (fxarray) (fxarray 1 1) (fxarray)))
     (error? (u8array-map/i! + (u8array) (u8array 1) (u8array) (u8array 1 1) (u8array)))

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-map/i! (lambda (i x) (list i x)) arr0)
               (apply array (zip ls0 ls0))))


     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-map/i! (lambda (i x y) (list i x y)) arr0 arr1)
               (apply array (zip ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-map/i! (lambda (i x y) (list i x y)) arr0 arr0)
               (apply array (zip ls0 ls0 ls0))))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (array-map/i! (lambda (i x0 x1 x2 x3 x4) (list i x0 x1 x2 x3 x4)) arr0 arr1 arr2 arr3 arr4)
               (apply array (zip ls0 ls0 ls0 ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-map/i! (lambda (i x0 x1 x2 x3 x4) (list i x0 x1 x2 x3 x4)) arr0 arr0 arr0 arr0 arr0)
               (apply array (zip ls0 ls0 ls0 ls0 ls0 ls0))))

     )


(mat array-for-each

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [ls '()])
       (array-for-each (lambda (x) (set! ls (cons x ls)))
                       arr0)
       (equal? ls (reverse ls0)))


     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [ls '()])
       (array-for-each (lambda (x y) (set! ls (cons (list x y) ls)))
                       arr0 arr1)
       (equal? ls (reverse (zip ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [ls '()])
       (array-for-each (lambda (x y) (set! ls (cons (list x y) ls)))
                       arr0 arr0)
       (equal? ls (reverse (zip ls0 ls0))))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)]
            [ls '()])
       (array-for-each (lambda (x0 x1 x2 x3 x4) (set! ls (cons (list x0 x1 x2 x3 x4) ls)))
                       arr0 arr1 arr2 arr3 arr4)
       (equal? ls (reverse (zip ls0 ls0 ls0 ls0 ls0))))

     )


(mat array-for-each/i

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [ls '()])
       (array-for-each/i (lambda (i x) (set! ls (cons (list i x) ls)))
                         arr0)
       (equal? ls (reverse (zip ls0 ls0))))

     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [ls '()])
       (array-for-each/i (lambda (i x y) (set! ls (cons (list i x y) ls)))
                         arr0 arr1)
       (equal? ls (reverse (zip ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [ls '()])
       (array-for-each/i (lambda (i x y) (set! ls (cons (list i x y) ls)))
                         arr0 arr0)
       (equal? ls (reverse (zip ls0 ls0 ls0))))

     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)]
            [ls '()])
       (array-for-each/i (lambda (i x0 x1 x2 x3 x4) (set! ls (cons (list i x0 x1 x2 x3 x4) ls)))
                         arr0 arr1 arr2 arr3 arr4)
       (equal? ls (reverse (zip ls0 ls0 ls0 ls0 ls0 ls0))))

     )


(mat array-map-rev

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (apply array (reverse (map add1 ls0)))
               (array-map-rev add1 arr0)))


     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (apply array (reverse (map + ls0 ls0)))
               (array-map-rev + arr0 arr0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (apply array (reverse (map + ls0 ls0)))
               (array-map-rev + arr0 arr1)))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (apply array (reverse (map + ls0 ls0 ls0 ls0 ls0)))
               (array-map-rev + arr0 arr1 arr2 arr3 arr4)))
     )


(mat array-map/i-rev

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (apply array (reverse (map + ls0 ls0)))
               (array-map/i-rev + arr0)))


     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (apply array (reverse (map + ls0 ls0 ls0)))
               (array-map/i-rev + arr0 arr0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (apply array (reverse (map + ls0 ls0 ls0)))
               (array-map/i-rev + arr0 arr1)))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (apply array (reverse (map + ls0 ls0 ls0 ls0 ls0 ls0)))
               (array-map/i-rev + arr0 arr1 arr2 arr3 arr4)))

     )


(mat array-for-each-rev

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [ls '()])
       (array-for-each-rev (lambda (x) (set! ls (cons x ls)))
                           arr0)
       (equal? ls ls0))


     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [ls '()])
       (array-for-each-rev (lambda (x y) (set! ls (cons (list x y) ls)))
                           arr0 arr1)
       (equal? ls (zip ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [ls '()])
       (array-for-each-rev (lambda (x y) (set! ls (cons (list x y) ls)))
                           arr0 arr0)
       (equal? ls (zip ls0 ls0)))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)]
            [ls '()])
       (array-for-each-rev (lambda (x0 x1 x2 x3 x4) (set! ls (cons (list x0 x1 x2 x3 x4) ls)))
                           arr0 arr1 arr2 arr3 arr4)
       (equal? ls (zip ls0 ls0 ls0 ls0 ls0)))

     )


(mat array-for-each/i-rev

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [ls '()])
       (array-for-each/i-rev (lambda (i x) (set! ls (cons (list i x) ls)))
                             arr0)
       (equal? ls (zip ls0 ls0)))

     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [ls '()])
       (array-for-each/i-rev (lambda (i x y) (set! ls (cons (list i x y) ls)))
                             arr0 arr1)
       (equal? ls (zip ls0 ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [ls '()])
       (array-for-each/i-rev (lambda (i x y) (set! ls (cons (list i x y) ls)))
                             arr0 arr0)
       (equal? ls (zip ls0 ls0 ls0)))

     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)]
            [ls '()])
       (array-for-each/i-rev (lambda (i x0 x1 x2 x3 x4) (set! ls (cons (list i x0 x1 x2 x3 x4) ls)))
                             arr0 arr1 arr2 arr3 arr4)
       (equal? ls (zip ls0 ls0 ls0 ls0 ls0 ls0)))

     )


(mat array-andmap

     (error? (array-andmap #f (array)))
     (error? (array-andmap odd? (fxarray 1)))
     (error? (fxarray-andmap odd? (array 1)))

     (array-andmap   odd? (array))
     (fxarray-andmap odd? (fxarray))
     (u8array-andmap odd? (u8array))

     (array-andmap   odd? (array) (array))
     (fxarray-andmap odd? (fxarray) (fxarray))
     (u8array-andmap odd? (u8array) (u8array))

     (array-andmap   odd? (array) (array) (array) (array) (array))
     (fxarray-andmap odd? (fxarray) (fxarray) (fxarray) (fxarray) (fxarray))
     (u8array-andmap odd? (u8array) (u8array) (u8array) (u8array) (u8array))

     (error? (array-andmap   odd? (array) (array 1)))
     (error? (fxarray-andmap odd? (fxarray) (fxarray 1)))
     (error? (u8array-andmap odd? (u8array) (u8array 1)))

     (error? (array-andmap   odd? (array) (array 1) (array) (array 1 1) (array)))
     (error? (fxarray-andmap odd? (fxarray) (fxarray 1) (fxarray) (fxarray 1 1) (fxarray)))
     (error? (u8array-andmap odd? (u8array) (u8array 1) (u8array) (u8array 1 1) (u8array)))

     ;; 1 arr
     (begin (define (test1 arr-proc andmap-proc)
              (let* ([n* (nums 1 100 2)]
                     [arr0 (apply arr-proc n*)])
                (andmap-proc odd? arr0)))
            #t)

     (test1 array   array-andmap)
     (test1 fxarray fxarray-andmap)
     (test1 u8array u8array-andmap)

     ;; 2 arrs
     (begin (define (test2 arr-proc andmap-proc)
              (let* ([n* (nums 1 100 2)]
                     [arr0 (apply arr-proc n*)]
                     [arr1 (apply arr-proc n*)])
                (andmap-proc = arr0 arr1)))
            #t)
     (test2 array   array-andmap)
     (test2 fxarray fxarray-andmap)
     (test2 u8array u8array-andmap)

     ;; more arrs
     (begin (define (test* arr-proc andmap-proc map-proc)
              (let* ([n* (nums 1 50 2)]
                     [arr0 (apply arr-proc n*)]
                     [arr1 (apply arr-proc n*)]
                     [arr2 (apply arr-proc n*)]
                     [arr3 (map-proc + arr0 arr1 arr2)])
                (andmap-proc (lambda (a b c d) (= d (+ a b c)))
                             arr0 arr1 arr2 arr3)))
            #t)
     (test* array   array-andmap   array-map)
     (test* fxarray fxarray-andmap fxarray-map)
     (test* u8array u8array-andmap u8array-map)

     )


(mat array-ormap

     (error? (array-ormap #f (array)))
     (error? (array-ormap odd? (fxarray 1)))
     (error? (fxarray-ormap odd? (array 1)))

     (not (array-ormap   odd? (array)))
     (not (fxarray-ormap odd? (fxarray)))
     (not (u8array-ormap odd? (u8array)))

     (not (array-ormap   odd? (array) (array)))
     (not (fxarray-ormap odd? (fxarray) (fxarray)))
     (not (u8array-ormap odd? (u8array) (u8array)))

     (not (array-ormap   odd? (array) (array) (array) (array) (array)))
     (not (fxarray-ormap odd? (fxarray) (fxarray) (fxarray) (fxarray) (fxarray)))
     (not (u8array-ormap odd? (u8array) (u8array) (u8array) (u8array) (u8array)))

     (error? (array-ormap   odd? (array) (array 1)))
     (error? (fxarray-ormap odd? (fxarray) (fxarray 1)))
     (error? (u8array-ormap odd? (u8array) (u8array 1)))

     (error? (array-ormap   odd? (array) (array 1) (array) (array 1 1) (array)))
     (error? (fxarray-ormap odd? (fxarray) (fxarray 1) (fxarray) (fxarray 1 1) (fxarray)))
     (error? (u8array-ormap odd? (u8array) (u8array 1) (u8array) (u8array 1 1) (u8array)))


     ;; 1 arr
     (begin (define (test1 arr-proc ormap-proc)
              (let* ([n* (snoc! (nums 1 100 2) 2)]
                     [arr0 (apply arr-proc n*)])
                (ormap-proc even? arr0)))
            #t)

     (test1 array   array-ormap)
     (test1 fxarray fxarray-ormap)
     (test1 u8array u8array-ormap)

     ;; 2 arrs
     (begin (define (test2 arr-proc ormap-proc)
              (let* ([n* (nums 1 100 2)]
                     [arr0 (apply arr-proc n*)]
                     [arr1 (apply arr-proc n*)])
                (ormap-proc (lambda (a b) (= (+ 49 49) (+ a b)))
                            arr0 arr1)))
            #t)
     (test2 array   array-ormap)
     (test2 fxarray fxarray-ormap)
     (test2 u8array u8array-ormap)

     ;; more arrs
     (begin (define (test* arr-proc ormap-proc map-proc)
              (let* ([n* (nums 1 50 2)]
                     [arr0 (apply arr-proc n*)]
                     [arr1 (apply arr-proc n*)]
                     [arr2 (apply arr-proc n*)]
                     [arr3 (apply arr-proc n*)])
                (ormap-proc (lambda (a b c d) (= (+ 33 33 33 33) (+ a b c d)))
                             arr0 arr1 arr2 arr3)))
            #t)
     (test* array   array-ormap   array-map)
     (test* fxarray fxarray-ormap fxarray-map)
     (test* u8array u8array-ormap u8array-map)

     )


(mat array-fold-left

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-left (lambda (acc x) (cons x acc))
                                '() arr0)
               (reverse ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-left (lambda (acc x) (fx+ acc x))
                                0 arr0)
               (apply fx+ ls0)))

     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-fold-left (lambda (acc x y) (cons (list x y) acc))
                                '() arr0 arr1)
               (reverse (zip ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-left (lambda (acc x y) (cons (list x y) acc))
                                '() arr0 arr0)
               (reverse (zip ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-fold-left (lambda (acc x y) (fx+ acc x y))
                                0 arr0 arr1)
               (apply fx+ (map fx+ ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-left (lambda (acc x y) (fx+ acc x y))
                                0 arr0 arr0)
               (apply fx+ (map fx+ ls0 ls0))))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)]
            [ls '()])
       (equal? (array-fold-left (lambda (acc x0 x1 x2 x3 x4) (cons (list x0 x1 x2 x3 x4) acc))
                                '() arr0 arr1 arr2 arr3 arr4)
               (reverse (zip ls0 ls0 ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)]
            [ls '()])
       (equal? (array-fold-left (lambda (acc x0 x1 x2 x3 x4) (fx+ acc x0 x1 x2 x3 x4))
                                0 arr0 arr1 arr2 arr3 arr4)
               (apply fx+ (map fx+ ls0 ls0 ls0 ls0 ls0))))

     )


(mat array-fold-left/i

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-left/i (lambda (i acc x) (cons (list i x) acc))
                                  '() arr0)
               (reverse (zip ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-left/i (lambda (i acc x) (fx+ i acc x))
                                  0 arr0)
               (apply fx+ (map fx+ ls0 ls0))))

     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-fold-left/i (lambda (i acc x y) (cons (list i x y) acc))
                                  '() arr0 arr1)
               (reverse (zip ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-left/i (lambda (i acc x y) (cons (list i x y) acc))
                                  '() arr0 arr0)
               (reverse (zip ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-fold-left/i (lambda (i acc x y) (fx+ i acc x y))
                                  0 arr0 arr1)
               (apply fx+ (map fx+ ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-left/i (lambda (i acc x y) (fx+ i acc x y))
                                  0 arr0 arr0)
               (apply fx+ (map fx+ ls0 ls0 ls0))))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (array-fold-left/i (lambda (i acc x0 x1 x2 x3 x4) (cons (list i x0 x1 x2 x3 x4) acc))
                                  '() arr0 arr1 arr2 arr3 arr4)
               (reverse (zip ls0 ls0 ls0 ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (array-fold-left/i (lambda (i acc x0 x1 x2 x3 x4) (fx+ i acc x0 x1 x2 x3 x4))
                                  0 arr0 arr1 arr2 arr3 arr4)
               (apply fx+ (map fx+ ls0 ls0 ls0 ls0 ls0 ls0))))

     )


(mat array-fold-right

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-right (lambda (x acc) (cons x acc))
                                 '() arr0)
               ls0))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-right (lambda (x acc) (fx+ acc x))
                                 0 arr0)
               (apply fx+ ls0)))

     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-fold-right (lambda (x y acc) (cons (list x y) acc))
                                 '() arr0 arr1)
               (zip ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-right (lambda (x y acc) (cons (list x y) acc))
                                 '() arr0 arr0)
               (zip ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-fold-right (lambda (x y acc) (fx+ acc x y))
                                 0 arr0 arr1)
               (apply fx+ (map fx+ ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-right (lambda (x y acc) (fx+ acc x y))
                                 0 arr0 arr0)
               (apply fx+ (map fx+ ls0 ls0))))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (array-fold-right (lambda (x0 x1 x2 x3 x4 acc) (cons (list x0 x1 x2 x3 x4) acc))
                                 '() arr0 arr1 arr2 arr3 arr4)
               (zip ls0 ls0 ls0 ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (array-fold-right (lambda (x0 x1 x2 x3 x4 acc) (fx+ acc x0 x1 x2 x3 x4))
                                 0 arr0 arr1 arr2 arr3 arr4)
               (apply fx+ (map fx+ ls0 ls0 ls0 ls0 ls0))))

     )


(mat array-fold-right/i

     ;; one array
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-right/i (lambda (i x acc) (cons (list i x) acc))
                                   '() arr0)
               (zip ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-right/i (lambda (i x acc) (fx+ i acc x))
                                   0 arr0)
               (apply fx+ (map fx+ ls0 ls0))))

     ;; two arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-fold-right/i (lambda (i x y acc) (cons (list i x y) acc))
                                   '() arr0 arr1)
               (zip ls0 ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-right/i (lambda (i x y acc) (cons (list i x y) acc))
                                   '() arr0 arr0)
               (zip ls0 ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)])
       (equal? (array-fold-right/i (lambda (i x y acc) (fx+ i acc x y))
                                   0 arr0 arr1)
               (apply fx+ (map fx+ ls0 ls0 ls0))))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)])
       (equal? (array-fold-right/i (lambda (i x y acc) (fx+ i acc x y))
                                   0 arr0 arr0)
               (apply fx+ (map fx+ ls0 ls0 ls0))))


     ;; five arrays
     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (array-fold-right/i (lambda (i x0 x1 x2 x3 x4 acc) (cons (list i x0 x1 x2 x3 x4) acc))
                                   '() arr0 arr1 arr2 arr3 arr4)
               (zip ls0 ls0 ls0 ls0 ls0 ls0)))

     (let* ([ls0 (iota 10)]
            [arr0 (apply array ls0)]
            [arr1 (apply array ls0)]
            [arr2 (apply array ls0)]
            [arr3 (apply array ls0)]
            [arr4 (apply array ls0)])
       (equal? (array-fold-right/i (lambda (i x0 x1 x2 x3 x4 acc) (fx+ i acc x0 x1 x2 x3 x4))
                                   0 arr0 arr1 arr2 arr3 arr4)
               (apply fx+ (map fx+ ls0 ls0 ls0 ls0 ls0 ls0))))

     )
