(import (chezpp))


(mat transducer-records

     (transducer? (tidentity))
     (eq? 'tidentity (transducer-name (tidentity)))
     (eq? 'tidentity (transducer-name (tcompose)))
     (reducer? (rflist))
     (eq? 'rflist (reducer-name (rflist)))
     (procedure? (completion (rflist)))

     (let ([x (reduced 10)])
       (and (reduced? x)
            (= 10 (unreduced x))
            (eq? x (ensure-reduced x))))
     (= 11 (unreduced 11))
     (reduced? (ensure-reduced 12))

     (eduction? (eduction (tidentity) '(1 2 3)))
     (transducible? '(1 2 3))
     (transducible? '#(1 2 3))
     (transducible? #vu8(1 2 3))
     (transducible? "abc")
     (transducible? (list->iter '(1 2 3)))
     (not (transducible? 123))

     ;; reducer boundary rejects a bare completing procedure.
     (error? (completion (case-lambda [() '()] [(acc) acc] [(acc x) (cons x acc)])))
     ;; transducer boundary rejects a bare procedure.
     (error? (transducer-name (lambda (r) r)))
     ;; constructors require symbolic names and procedures.
     (error? (make-transducer "map" (lambda (r) r)))
     (error? (make-reducer "list" (case-lambda [() '()] [(acc) acc] [(acc x) (cons x acc)]))))


(mat transducer-basic-pipelines

     (equal? '(2 4 6 8 10)
             (into 'list
                   (tcompose (tmap add1) (tfilter even?) (ttake 5))
                   '(0 1 2 3 4 5 6 7 8 9 10 11)))

     (equal? '#(2 4 6)
             (into 'vector
                   (tcompose (tmap add1) (tfilter even?))
                   '(1 2 3 4 5)))

     (= 6 (transduce (tmap string-length)
                     (rffxsum)
                     '("aa" "bbb" "c")))

     (equal? '(1 3 5)
             (transduce (tfilter odd?)
                        (rflist)
                        '()
                        '#(1 2 3 4 5)))

     (= 2 (transduce (tfilter positive?)
                     (rfcount)
                     '#(-2 0 4 6 -1)))

     (equal? '(2 4)
             (bytevector-transduce (tfilter even?)
                                   (rflist)
                                   #vu8(1 2 3 4 5)))

     (string=? "CHEZ"
               (string-transduce (tmap char-upcase)
                                 (make-reducer
                                  'chars
                                  (case-lambda
                                    [() '()]
                                    [(acc) (list->string (reverse acc))]
                                    [(acc x) (cons x acc)]))
                                 "chez"))

     (equal? '(11 21 31)
             (vector-transduce (tcompose (tmap add1) (ttake 3))
                               (rflist)
                               '#(10 20 30 40)))

     (equal? '(0 2 4)
             (list-transduce (tcompose (tdrop 2) (ttake 3))
                             (rflist)
                             '(9 8 0 2 4 6)))

     (equal? '(2 4 6)
             (iter-transduce (tcompose (tremove odd?) (ttake 3))
                             (rflist)
                             (list->iter '(1 2 3 4 5 6 7 8))))

     ;; unknown output destination is rejected.
     (error? (into 'set (tidentity) '(1 2 3)))
     ;; source boundary rejects unsupported source values.
     (error? (transduce (tidentity) (rflist) 12)))


(mat transducer-control-flow

     (equal? '(3 4 5)
             (into 'list
                   (tcompose (tdrop-while (lambda (x) (< x 3)))
                             (ttake-while (lambda (x) (< x 6))))
                   '(0 1 2 3 4 5 6 7)))

     (equal? '(1 2 3)
             (into 'list
                   (tchain (list (tmap add1) (ttake 3)))
                   '(0 1 2 3 4)))

     (let ([xf (ttake 2)])
       (and (equal? '(1 2) (into 'list xf '(1 2 3 4)))
            (equal? '(5 6) (into 'list xf '(5 6 7 8)))))

     (equal? '(#\a #\c)
             (into 'list
                   (tkeep (lambda (x)
                            (and (number? x) (integer->char (+ x 96)))))
                   '(1 #f a 3)))

     (= 6.5 (transduce (tidentity) (rfflsum) '#(1.0 2.5 3.0)))
     (= 10 (transduce (tidentity) (rfsum) '(1 2 3 4)))
     (= 4 (transduce (tidentity) (rfcount) "chez"))
     (equal? '#(a b c) (transduce (tidentity) (rfvector) '(a b c)))
     (equal? '(c b a) (transduce (tidentity) (rfreverselist) '(a b c)))
     (equal? '(2 3 4)
             (transduce (tidentity)
                        (rflist)
                        (eduction (tmap add1) '(1 2 3)))))
