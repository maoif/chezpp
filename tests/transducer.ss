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


(mat transducer-phase2-transforms

     (equal? '((0 . a) (1 . b) (2 . c))
             (into 'list (tmap/i cons) '(a b c)))

     (equal? '(a c)
             (into 'list
                   (tfilter/i (lambda (i x) (even? i)))
                   '(a b c d)))

     (equal? '(0 2)
             (into 'list
                   (tkeep/i (lambda (i x) (and (even? i) i)))
                   '(a b c d)))

     (equal? '(uno 2 tres)
             (into 'list
                   (treplace '((1 . uno) (3 . tres)))
                   '(1 2 3)))

     (equal? '(1 2 3 4 #\a #\b)
             (into 'list
                   (tcat)
                   '((1 2) #(3 4) "ab")))

     (equal? '(1 1 2 2 3 3)
             (into 'list
                   (tmapcat (lambda (x) (list x x)))
                   '(1 2 3)))

     (equal? '(1 2 3 4 #\a)
             (into 'list
                   (tflatten)
                   '(1 (2 #(3 (4))) "a")))

     ;; replacement tables must be association lists.
     (error? (treplace '#((1 . one)))))


(mat transducer-phase2-stateful

     (equal? '(#(1 2) #(3 4))
             (into 'list (tpartition 2) '(1 2 3 4 5)))

     (equal? '(#(1 2) #(3 4) #(5))
             (into 'list (tpartition-all 2) '(1 2 3 4 5)))

     (equal? '(#(1 1) #(2 2 2) #(3))
             (into 'list (tpartition-by id) '(1 1 2 2 2 3)))

     (equal? '(1 2 1)
             (into 'list (tdedupe) '(1 1 2 2 1 1)))

     (equal? '(-1 2 3)
             (into 'list (tdedupe-by abs) '(-1 1 2 -2 3)))

     (equal? '(1 2 3)
             (into 'list (tdistinct) '(1 2 1 3 2)))

     (equal? '(-1 -2 3)
             (into 'list (tdistinct-by abs) '(-1 1 -2 2 3)))

     (equal? '(a x b x c)
             (into 'list (tinterpose 'x) '(a b c)))

     (let ([seen '()])
       (and (equal? '(1 2 3)
                    (into 'list
                          (ttap (lambda (x) (set! seen (cons x seen))))
                          '(1 2 3)))
            (equal? '(3 2 1) seen)))

     (let ([seen '()])
       (and (equal? '(a b)
                    (into 'list
                          (tinspect 'phase2
                                    (lambda (who x)
                                      (set! seen (cons (list who x) seen))))
                          '(a b)))
            (equal? '((phase2 b) (phase2 a)) seen)))

     (let ([seen '()])
       (tfor-each (tmap add1)
                  (lambda (x) (set! seen (cons x seen)))
                  '(1 2 3))
       (equal? '(4 3 2) seen))

     ;; partition sizes must be positive.
     (error? (tpartition 0))
     ;; partition-by requires a procedure.
     (error? (tpartition-by 12)))


(mat transducer-phase2-sources

     (string=? "ABC"
               (transduce (tmap char-upcase) (rfstring) "abc"))

     (equal? #vu8(2 4 6)
             (transduce (tmap (lambda (x) (fx* x 2)))
                        (rfbytevector)
                        #vu8(1 2 3)))

     (equal? '(2 3 4)
             (fxvector-transduce (tmap add1)
                                 (rflist)
                                 '#vfx(1 2 3)))

     (= 6.5 (flvector-transduce (tidentity)
                                (rfflsum)
                                '#vfl(1.0 2.5 3.0)))

     (let ([h (make-eq-hashtable)])
       (hashtable-set! h 'a 10)
       (hashtable-set! h 'b 20)
       (= 30 (hashtable-transduce (tidentity) (rfsum) h)))

     (let ([p (open-input-string "aa\nbbb\nc")])
       (let ([res (port-lines-transduce (tmap string-length) (rflist) p)]
             [open? (not (port-closed? p))])
         (close-port p)
         (and open? (equal? '(2 3 1) res))))

     (let ([p (open-bytevector-input-port #vu8(1 2 3))])
       (let ([res (port-bytes-transduce (tmap add1) (rflist) p)]
             [open? (not (port-closed? p))])
         (close-port p)
         (and open? (equal? '(2 3 4) res))))

     (equal? '(0 1 2)
             (list-transduce (tidentity) (rflist) '(0 1 2 3 4) 3))

     (equal? '(1 3)
             (vector-transduce (tidentity) (rflist) '#(0 1 2 3 4) 1 5 2))

     (equal? '(2 4)
             (fxvector-transduce (tidentity) (rflist) '#vfx(0 1 2 3 4 5) 2 6 2))

     (equal? '(1.5 3.5)
             (flvector-transduce (tidentity) (rflist) '#vfl(0.5 1.5 2.5 3.5) 1 4 2))

     (equal? '(2 4)
             (bytevector-transduce (tidentity) (rflist) #vu8(0 1 2 3 4 5) 2 6 2))

     (equal? '(#\b #\d)
             (string-transduce (tidentity) (rflist) "abcdef" 1 5 2))

     (iter? (source->iter '(1 2 3)))
     (eq? 'direct (current-transducer-source-mode))
     (equal? '(2 3 4)
             (parameterize ([current-transducer-source-mode 'iter])
               (transduce (tmap add1) (rflist) '#(1 2 3))))

     ;; slice step must be positive.
     (error? (vector-transduce (tidentity) (rflist) '#(1 2 3) 0 3 0))
     ;; caller-owned textual ports are required for line traversal.
     (error? (port-lines-transduce (tidentity) (rflist) 12)))


(mat transducer-phase3-sequence

     (let ([pulled 0])
       (let ([iter (sequence (tcompose (ttap (lambda (x) (set! pulled (+ pulled 1))))
                                       (ttake 2))
                             '(1 2 3))])
         (and (= 0 pulled)
              (= 1 (iter-next! iter))
              (= 1 pulled)
              (= 2 (iter-next! iter))
              (= 2 pulled)
              (iter-end? (iter-next! iter))
              (= 2 pulled))))

     (let ([iter (sequence (tcompose (tdrop 2) (ttake 3))
                           '(0 1 2 3 4 5 6))])
       (and (= 2 (iter-next! iter))
            (= 3 (iter-next! iter))
            (= 4 (iter-next! iter))
            (iter-end? (iter-next! iter))))

     (equal? '(2 4)
             (iter->list
              (sequence (tmap add1)
                        (eduction (tfilter odd?) '(1 2 3)))))

     (equal? '(2 3 4)
             (iter->list (source->iter (eduction (tmap add1) '(1 2 3)))))

     (= 8 (transduce1 (tfilter odd?) (rfsum) '(2 3 4 5)))
     (= 6.5 (transduce1 (tidentity) (rfflsum) '#vfl(1.0 2.5 3.0)))

     ;; transduce1 rejects an empty transformed stream.
     (error? (transduce1 (tfilter odd?) (rfsum) '(2 4 6))))


(mat transducer-phase3-reducers

     (= 1 (transduce (tidentity) (rfmin <) '(3 1 2)))
     (= 3 (transduce (tidentity) (rfmax <) '(3 1 2)))
     (eq? 'yes
          (transduce (tidentity)
                     (rfany (lambda (x) (and (symbol? x) 'yes)))
                     '(1 2 ok 3)))
     (not (transduce (tidentity) (rfany odd?) '(2 4 6)))
     (transduce (tidentity) (rfevery positive?) '(1 2 3))
     (not (transduce (tidentity) (rfevery positive?) '(1 -2 3)))

     ;; rfmin errors when no values are reduced.
     (error? (transduce (tidentity) (rfmin <) '()))
     ;; reducer predicates are required.
     (error? (rfany 12))
     (error? (rfevery 12))
     (error? (rfmin 12)))
