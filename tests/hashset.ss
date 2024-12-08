(import (chezpp hashset)
        (chezpp list)
        (chezpp vector))


;; TODO add, clear, delete
;; TODO eq,eqv,symbol hashset



(mat hashset-equal?

     (equal? (apply hashset (iota 100))
             (apply hashset (iota 100)))

     (not (equal? (apply hashset (iota 20))
                  (apply hashset (iota 10))))

     (let ([ts1 (make-hashset)] [ts2 (make-hashset)])
       (fxvfor-each (lambda (x) (treeset-add! ts1 x)) v100000)
       (fxvfor-each (lambda (x) (treeset-add! ts2 x)) (fxvshuffle! v100000))
       (equal? ts1 ts1))


     )


(mat hashset-contains?
     #t)


(mat hashset-contains/p?
     #t)


(mat hashset-search
     #t)


(mat hashset-search*
     #t)


(mat hashset-filter
     #t)


(mat hashset-filter!
     #t)


(mat hashset-partition
     #t)


(mat hashset-map

     (equal? (hashset-map id (hashset))
             (hashset-map id (hashset)))

     (equal? (hashset-map fx1+ (apply hashset (iota 100)))
             (apply hashset (map fx1+ (iota 100))))

     (equal? (hashset-map + (apply hashset (iota 100)) (apply hashset (iota 100)))
             (apply hashset (map + (iota 100) (iota 100))))

     )


(mat hashset-for-each
     #t)


(mat hashset<->list

     (error? (hashset->list))
     (error? (hashset->list '()))

     (let ([hs (apply hashset (iota 20))])
       (equal? hs (list->eqv-hashset (iota 20))))

     )


(mat hashset<->vector

     (error? (hashset->vector))
     (error? (hashset->vector '()))

     (let* ([v (random-vector 50)]
            [hs (vector->eqv-hashset v)])
       (equal? hs (vector->eqv-hashset (hashset->vector hs))))


     )



(mat hashset-set-ops

     ;; union
     (let ([hs1 (hashset 1 2 3 4 5 6)]
           [hs2 (hashset 4 5 6 7 8 9)]
           [hs3 (hashset 4 6 5 4 8 9)])
       (equal? (hashset+ hs1 hs2 hs3)
               (hashset 1 2 3 4 5 6 7 8 9)))

     (let ([hs1 (apply hashset (nums 0  50))]
           [hs2 (apply hashset (nums 30 80))]
           [hs3 (apply hashset (nums 40 90))])
       (equal? (apply hashset (nums 0 90))
               (hashset+ hs1 hs2 hs3)))

     ;; difference
     (let ([hs1 (hashset 1 2 3 4 5 6)]
           [hs2 (hashset 4 5 6 7 8 9)])
       (equal? (hashset- hs1 hs2)
               (hashset 1 2 3)))

     (let ([hs1 (hashset 1 2 3 4 5 6)]
           [hs2 (hashset 4 5 6 7 8 9)]
           [hs3 (hashset 4 6 5 4 8 9 1)])
       (equal? (hashset- hs1 hs2 hs3)
               (hashset 2 3)))

     (let ([hs1 (apply hashset (nums 0  50))]
           [hs2 (apply hashset (nums 30 80))]
           [hs3 (apply hashset (nums 40 90))])
       (equal? (apply hashset (nums 0 30))
               (hashset- hs1 hs2 hs3)))

     ;; intersection
     (let ([hs1 (hashset 1 2 3 4 5 6)]
           [hs2 (hashset 4 5 6 7 8 9)]
           [hs3 (hashset 4 6 5 4 8 9)])
       (equal? (hashset& hs1 hs2 hs3)
               (hashset 4 5 6)))

     (let ([hs1 (apply hashset (nums 0  50))]
           [hs2 (apply hashset (nums 30 80))]
           [hs3 (apply hashset (nums 40 90))])
       (equal? (apply hashset (nums 40 50))
               (hashset& hs1 hs2 hs3)))

     ;; symmetric difference
     (let ([hs1 (hashset 1 2 3 4 5 6)]
           [hs2 (hashset 4 5 6 7 8 9)])
       (equal? (hashset^ hs1 hs2)
               (hashset 1 2 3 7 8 9)))


     ;; equations
     (let ([hs1 (hashset 1 2 3 4 5 6)]
           [hs2 (hashset 4 5 6 7 8 9)]
           [hs3 (hashset 7 8 9 10 11 12 4 5 6)]
           [hs4 (hashset 4 6 7 8 2 5)])
       (equal? (hashset^ hs1 hs2 hs3 hs4)
               (hashset- (hashset+ hs1 hs2 hs3 hs4) (hashset& hs1 hs2 hs3 hs4))))

     (let* ([ls* (map (lambda (i) (random-list (lambda () (random 999)) 1000 2000)) (iota 10))]
            [hs* (map (lambda (ls) (apply hashset ls)) ls*)])
       (equal? (apply hashset^ hs*)
               (hashset- (apply hashset+ hs*) (apply hashset& hs*))))

     )
