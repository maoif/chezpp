(import (chezpp))

(define collect-iter
  (lambda (iter)
    (let loop ([acc '()])
      (let ([value (iter-next! iter)])
        (if (iter-end? value)
            (reverse acc)
            (loop (cons value acc)))))))

(define reset-sequence?
  (lambda (expected source)
    (let ([iter (source->iter source)])
      (and (equal? expected (collect-iter iter))
           (begin
             (iter-reset! iter)
             (equal? expected (collect-iter iter)))))))


(mat registered-source-iterators

     (reset-sequence? '(1 2 3) (array 1 2 3))
     (reset-sequence? '(1 2 3) (fxarray 1 2 3))
     (reset-sequence? '(1 2 3) (bytearray 1 2 3))
     (reset-sequence? '(1 2 3) (dlist 1 2 3))
     (reset-sequence? '(1 2 3) (queue 1 2 3))
     (reset-sequence? '(3 2 1) (stack 1 2 3))
     (reset-sequence? '(1 2 3) (heap < 3 1 2))
     (reset-sequence? '(1 2 3) (treeset = < 3 1 2))
     (reset-sequence? '((1 . a) (2 . b))
                      (treemap = < '(2 . b) '(1 . a)))
     (reset-sequence? '(1 3 5) (bitvec 5 1 3))
     (reset-sequence? '(1 3 5) (bittree 5 1 3))
     (reset-sequence? '(0 1 2 3) (make-dset 4))

     (let ([source (hashset 3 1 2)]
           [iter #f])
       (set! iter (source->iter source))
       (let ([first (sort < (collect-iter iter))])
         (iter-reset! iter)
         (and (equal? '(1 2 3) first)
              (equal? '(1 2 3) (sort < (collect-iter iter))))))

     ;; invalid registration callbacks are rejected at the public boundary
     (error? (iter-register-source! 1 (lambda (source) source)))
     (error? (iter-register-source! pair? 1)))


(mat registered-array-iterators-are-live

     (let* ([source (array 0 1)]
            [iter (source->iter source)])
       (array-set! source 1 9)
       (and (equal? '(0 9) (collect-iter iter))
            (begin
              (array-push-back! source 2)
              (iter-reset! iter)
              (equal? '(0 9 2) (collect-iter iter)))))
     (let* ([source (fxarray 0 1)]
            [iter (source->iter source)])
       (fxarray-set! source 1 9)
       (and (equal? '(0 9) (collect-iter iter))
            (begin
              (fxarray-push-back! source 2)
              (iter-reset! iter)
              (equal? '(0 9 2) (collect-iter iter)))))
     (let* ([source (bytearray 0 1)]
            [iter (source->iter source)])
       (bytearray-set! source 1 9)
       (and (equal? '(0 9) (collect-iter iter))
            (begin
              (bytearray-push-back! source 2)
              (iter-reset! iter)
              (equal? '(0 9 2) (collect-iter iter)))))

     )

(mat direct-dlist-iterator
     ;; A dlist iterator reads live node values and reset sees current contents.
     (let* ([dl (dlist 1 2)]
            [iter (dlist->iter dl)])
       (dlist-set! dl 0 9)
       (and (equal? '(9 2) (collect-iter iter))
            (begin
              (dlist-push-back! dl 3)
              (iter-reset! iter)
              (equal? '(9 2 3) (collect-iter iter))))))

(mat registered-source-iterators-see-reset-mutations
     (let* ([arr (array 1 2)] [iter (iter-source->iter arr)])
       (and (equal? '(1 2) (collect-iter iter))
            (begin (array-push-back! arr 3)
                   (iter-reset! iter)
                   (equal? '(1 2 3) (collect-iter iter)))))
     (let* ([dl (dlist 1 2)] [iter (iter-source->iter dl)])
       (and (equal? '(1 2) (collect-iter iter))
            (begin (dlist-push-back! dl 3)
                   (iter-reset! iter)
                   (equal? '(1 2 3) (collect-iter iter)))))
     (let* ([q (queue 1 2)] [iter (iter-source->iter q)])
       (and (equal? '(1 2) (collect-iter iter))
            (begin (queue-push! q 3)
                   (iter-reset! iter)
                   (equal? '(1 2 3) (collect-iter iter)))))
     (let* ([stk (stack 1 2)] [iter (iter-source->iter stk)])
       (and (equal? '(2 1) (collect-iter iter))
            (begin (stack-push! stk 3)
                   (iter-reset! iter)
                   (equal? '(3 2 1) (collect-iter iter)))))
     (let* ([hp (heap < 2 1)] [iter (iter-source->iter hp)])
       (and (equal? '(1 2) (collect-iter iter))
            (begin (heap-push! hp 0)
                   (iter-reset! iter)
                   (equal? '(0 1 2) (collect-iter iter)))))
     (let* ([hs (hashset 1 2)] [iter (iter-source->iter hs)])
       (and (equal? '(1 2) (sort < (collect-iter iter)))
            (begin (hashset-add! hs 3)
                   (iter-reset! iter)
                   (equal? '(1 2 3) (sort < (collect-iter iter))))))
     (let* ([ts (treeset = < 2 1)] [iter (iter-source->iter ts)])
       (and (equal? '(1 2) (collect-iter iter))
            (begin (treeset-add! ts 3)
                   (iter-reset! iter)
                   (equal? '(1 2 3) (collect-iter iter)))))
     (let* ([tm (treemap = < '(2 . b) '(1 . a))]
            [iter (iter-source->iter tm)])
       (and (equal? '((1 . a) (2 . b)) (collect-iter iter))
            (begin (treemap-set! tm 3 'c)
                   (iter-reset! iter)
                   (equal? '((1 . a) (2 . b) (3 . c)) (collect-iter iter)))))
     (let* ([bv (bitvec 1 3)] [iter (iter-source->iter bv)])
       (and (equal? '(1 3) (collect-iter iter))
            (begin (bitvec-set! bv 2)
                   (iter-reset! iter)
                   (equal? '(1 2 3) (collect-iter iter)))))
     (let* ([bt (bittree 1 3)] [iter (iter-source->iter bt)])
       (and (equal? '(1 3) (collect-iter iter))
            (begin (bittree-set! bt 2)
                   (iter-reset! iter)
                   (equal? '(1 2 3) (collect-iter iter)))))
     (let* ([ds (make-dset 4)] [iter (iter-source->iter ds)])
       (and (equal? '(0 1 2 3) (collect-iter iter))
            (begin (dset-union! ds 0 1)
                   (iter-reset! iter)
                   (equal? '(0 1 2 3) (collect-iter iter))))))


(mat iterator-dispatch

     (let ([ht (make-hashtable equal-hash equal?)])
       (hashtable-set! ht 'a 1)
       (hashtable-set! ht 'b 2)
       (equal? '(1 2) (sort < (collect-iter (iter-source->iter ht)))))

     (let ([arr (array 1 2 3)])
       (equal? '(1 2 3) (collect-iter (get-iter 'test arr))))

     (let ([ht (make-hashtable equal-hash equal?)])
       (hashtable-set! ht 'a 1)
       (let ([iter (iter-source->iter ht)])
         (and (equal? '(1) (collect-iter iter))
              (begin
                (hashtable-delete! ht 'a)
                (hashtable-set! ht 'b 2)
                (iter-reset! iter)
                (equal? '(2) (collect-iter iter))))))

     )

(mat iterator-lifecycle
     (error? (let ([it (range 1)])
               (iter->list it)
               (iter-next! it)))
     (error? (let ([it (range 1)])
               (iter->list it)
               (iter-reset! it)))
     (let ([p (open-input-string "a\nb\n")])
       (let ([it (port-lines->iter p)])
         (iter->list it)
         (not (port-closed? p))))
     (let ([path "iter-owned-port-test.txt"])
       (call-with-output-file path
         (lambda (p) (put-string p "a\nb\n")))
       (iter->list (file-lines->iter path))
       (delete-file path)
       #t))

(mat duplicate-source-registration
     (let* ([source 424242]
            [predicate (lambda (x) (and (number? x) (= x source)))])
       (iter-register-source!
        predicate
        (lambda (_) (make-iter (lambda () 'old) (lambda () void))))
       (and (guard (condition [else #t])
              (iter-register-source!
               predicate
               (lambda (_) (make-iter (lambda () 'new) (lambda () void))))
              #f)
            (eq? 'old (iter-next! (iter-source->iter source)))))
     (let* ([source 434343]
            [first? (lambda (x) (and (number? x) (= x source)))]
            [second? (lambda (x) (and (number? x) (= x source)))])
       (iter-register-source!
        first?
        (lambda (_) (make-iter (lambda () 'old) (lambda () void))))
       (iter-register-source!
        second?
        (lambda (_) (make-iter (lambda () 'new) (lambda () void))))
       (eq? 'new (iter-next! (iter-source->iter source)))))


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

     ;; A descending range requires an explicitly negative step.
     (error? (range 10 0))


     ;; nums
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

     (eq? iter-end (iter-next! (bytevector->iter #vu8())))
     (eq? iter-end (iter-next! (fxvector->iter '#vfx())))
     (eq? iter-end (iter-next! (flvector->iter '#vfl())))

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

     (equal? '(1 2 3)
             (iter->list (bytevector->iter #vu8(1 2 3))))
     (equal? '(2 4)
             (iter->list (bytevector->iter #vu8(0 1 2 3 4 5) 2 6 2)))

     (equal? '(1 2 3)
             (iter->list (fxvector->iter '#vfx(1 2 3))))
     (equal? '(2 4)
             (iter->list (fxvector->iter '#vfx(0 1 2 3 4 5) 2 6 2)))

     (equal? '(1.0 2.5)
             (iter->list (flvector->iter '#vfl(1.0 2.5))))
     (equal? '(1.5 3.5)
             (iter->list (flvector->iter '#vfl(0.5 1.5 2.5 3.5) 1 4 2)))

     )


(mat iter-directional-indexes

     (equal? '(8 6 4) (iter->list (vector->iter '#(0 1 2 3 4 5 6 7 8 9)
                                                   8 2 -2)))
     (equal? '(#\8 #\6 #\4) (iter->list (string->iter "0123456789" 8 2 -2)))
     (equal? '(8 6 4) (iter->list (bytevector->iter #vu8(0 1 2 3 4 5 6 7 8 9)
                                                       8 2 -2)))
     (equal? '(8 6 4) (iter->list (fxvector->iter '#vfx(0 1 2 3 4 5 6 7 8 9)
                                                       8 2 -2)))
     (equal? '(8.0 6.0 4.0)
             (iter->list
              (flvector->iter '#vfl(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0)
                              8 2 -2)))
     (equal? '(8 6 4) (iter->list (vector->iter '#(0 1 2 3 4 5 6 7 8 9)
                                                   -2 -8 -2)))
     (equal? '() (iter->list (vector->iter '#(0 1 2) 1 1 -1)))
     (equal? '() (iter->list (vector->iter '#() 1 2)))

     ;; A zero step cannot advance a vector iterator.
     (error? (vector->iter '#(1 2) 0 2 0))

     ;; A zero step cannot advance a string iterator.
     (error? (string->iter "12" 0 2 0))

     ;; List iterators support forward traversal only.
     (error? (list->iter '(1 2) 0 2 -1)))


(mat iter-directional-ranges

     (equal? '(5 3 1) (iter->list (range 5 0 -2)))
     (equal? '(0 2 4) (iter->list (range 0 5 2)))
     (reset-sequence? '(5 3 1) (range 5 0 -2))
     (reset-sequence? '(0 2 4) (range 0 5 2))
     (equal? '() (iter->list (range 2 2 -1)))

     ;; A zero step cannot advance a range.
     (error? (range 0 5 0))

     ;; An ascending range requires a positive step.
     (error? (range 0 5 -1))

     ;; A descending range requires a negative step.
     (error? (range 5 0 1)))


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

     ;; iter-interleave
     ;; same length
     (equal? '(1 0 3 2 5 4 7 6 9 8 11 10 13 12 15 14 17 16 19 18)
             (let ([it1 (range 20)]
                   [it2 (range 20)])
               (iter->list (iter-interleave (iter-filter odd? it1)
                                            (iter-filter even? it2)))))
     ;; one shorter
     (equal? '(1 0 3 2 5 4 7 6 9 8 10 12 14 16 18)
             (let ([it1 (range 10)]
                   [it2 (range 20)])
               (iter->list (iter-interleave (iter-filter odd? it1)
                                            (iter-filter even? it2)))))
     (equal? '(1 0 -20.0 3 2 -17.0 5 4 -14.0 7 6 -11.0 9 8 -8.0 10 -5.0 12
                 -2.0 14 1.0 16 4.0 18 7.0 10.0 13.0 16.0 19.0)
             (let ([it1 (range 10)]
                   [it2 (range 20)]
                   [it3 (range -20.0 20.0 3)])
               (iter->list (iter-interleave (iter-filter odd? it1)
                                            (iter-filter even? it2)
                                            it3))))

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

     (and (not (iter-avg (range 0)))
          (not (iter-fxavg (range 0)))
          (not (iter-flavg (range 0.0))))

     )


(mat iter-finalize


     (let* ([it (range 10)]
            [ls (iter->list it)])
       (and (equal? ls (iota 10))
            (iter-finalized? it)))

     (let* ([it (range 10)]
            [i 0])
       (iter-for-each (lambda (x) (incr! i)) it)
       (= i 10))

     (error? (let ([r (range 10)])
               (iter->list r)
               (iter-finalize! r)))

     )


(mat iter-ports/files

     ;; port->iter
     (let* ([ls (iter->list (iter-zip (range 100)
                                      (range 100)
                                      (range 100)))]
            [str* (map (lambda (x) (format "~a~n" x)) ls)]
            [file (format "testfile_~a_~a" (random 9999) (time-nanosecond (current-time)))])
       (call-with-output-file file
         (lambda (p)
           (let loop ([str* str*])
             (unless (null? str*)
               (put-string p (car str*))
               (loop (cdr str*))))))
       (let ([res (call-with-input-file file
                    (lambda (p)
                      (let ([s (iter->list (iter-map (lambda (x) (format "~a~n" x))
                                                     (port->iter p)))])
                        (equal? str* s))))])
         (delete-file file)
         res))
     ;;port-chars->iter
     (let* ([ls (iter->list (iter-zip (range 100)
                                      (range 100)
                                      (range 100)))]
            [str (format "~a" ls)]
            [file (format "testfile_~a_~a" (random 9999) (time-nanosecond (current-time)))])
       (call-with-output-file file
         (lambda (p)
           (put-string p str)))
       (let ([res (call-with-input-file file
                    (lambda (p)
                      (let ([s (iter->list (port-chars->iter p))])
                        (equal? str (apply string s)))))])
         (delete-file file)
         res))
     ;; port-data->iter
     (let* ([ls (iter->list (iter-zip (range 100)
                                      (range 100)
                                      (range 100)))]
            [file (format "testfile_~a_~a" (random 9999) (time-nanosecond (current-time)))])
       (call-with-output-file file
         (lambda (p)
           (let loop ([ls ls])
             (unless (null? ls)
               (put-datum p (car ls))
               (loop (cdr ls))))))
       (let ([res (call-with-input-file file
                    (lambda (p)
                      (let ([s (iter->list (port-data->iter p))])
                        (equal? ls s))))])
         (delete-file file)
         res))


     ;; file->iter
     (let* ([ls (iter->list (iter-zip (range 100)
                                      (range 100)
                                      (range 100)))]
            [str* (map (lambda (x) (format "~a~n" x)) ls)]
            [file (format "testfile_~a_~a" (random 9999) (time-nanosecond (current-time)))])
       (call-with-output-file file
         (lambda (p)
           (let loop ([str* str*])
             (unless (null? str*)
               (put-string p (car str*))
               (loop (cdr str*))))))
       (let ([res (let ([s (iter->list (iter-map (lambda (x) (format "~a~n" x))
                                                 (file->iter file)))])
                    (equal? str* s))])
         (delete-file file)
         res))
     ;;file-chars->iter
     (let* ([ls (iter->list (iter-zip (range 100)
                                      (range 100)
                                      (range 100)))]
            [str (format "~a" ls)]
            [file (format "testfile_~a_~a" (random 9999) (time-nanosecond (current-time)))])
       (call-with-output-file file
         (lambda (p)
           (put-string p str)))
       (let ([res (let ([s (iter->list (file-chars->iter file))])
                    (equal? str (apply string s)))])
         (delete-file file)
         res))
     ;; file-data->iter
     (let* ([ls (iter->list (iter-zip (range 100)
                                      (range 100)
                                      (range 100)))]
            [file (format "testfile_~a_~a" (random 9999) (time-nanosecond (current-time)))])
       (call-with-output-file file
         (lambda (p)
           (let loop ([ls ls])
             (unless (null? ls)
               (put-datum p (car ls))
               (loop (cdr ls))))))
       (let ([res (equal? ls (iter->list (file-data->iter file)))])
         (delete-file file)
         res))




     )
