(import (chezpp) (only (chezpp private rbset) $rbset-verify))

;; Exercise rotations and deletion repair on the actual public treeset backend.
(mat rbset-balanced-mutations
     (let ([items (fxvshuffle! (fxviota 1000))]
           [set (make-treeset fx= fx<)])
       (do ([i 0 (fx1+ i)]) ((fx= i 1000))
         (treeset-add! set i)
         ($rbset-verify set))
       (fxvfor-each (lambda (item)
                     (treeset-delete! set item)
                     ($rbset-verify set))
                   items)
       (treeset-empty? set)))

(mat rbset-preserves-pair-items
     (let ([set (treeset equal? (lambda (a b) (< (car a) (car b))) '(1 . a) '(2 . b))])
       (and (equal? '(1 . a) (treeset-min set))
            (equal? '(2 . b) (treeset-successor set '(1 . a))))))

(mat fixnum-backend-derived-results
     (let* ([set (fixnum-treeset fx= fx< 3 1 2)]
            [map (fixnum-treemap fx= fx< '(3 . c) '(1 . a) '(2 . b))])
       (and (fixnum-treeset? (treeset-filter odd? set))
            (fixnum-treeset? (treeset-map fx1+ set))
            (fixnum-treemap? (treemap-filter (lambda (key value) (odd? key)) map))
            (fixnum-treemap? (treemap-map (lambda (key value) (values key value)) map))
            (begin
              (treeset-delete! set 2)
              (treemap-delete! map 2)
              (and ($rbset-verify set) ($rbtree-verify map))))))

;; Error cases: empty specialized trees and callback-generated keys must be checked.
(mat fixnum-backend-validation
     (error? (treeset-contains? (make-fixnum-treeset fx= fx<) 'bad))

     (error? (treemap-contains? (make-fixnum-treemap fx= fx<) 'bad))

     (error? (treeset-map (lambda (item) 'bad) (fixnum-treeset fx= fx< 1)))

     (error? (treemap-map (lambda (key value) (values 'bad value))
                          (fixnum-treemap fx= fx< '(1 . a)))))

(mat fixnum-backend-balanced-mutations
     (let ([items (fxvshuffle! (fxviota 500))]
           [set (make-fixnum-treeset fx= fx<)]
           [map (make-fixnum-treemap fx= fx<)])
       (fxvfor-each
         (lambda (item)
           (treeset-add! set item)
           (treemap-set! map item (vector item))
           ($rbset-verify set)
           ($rbtree-verify map))
         items)
       (fxvfor-each
         (lambda (item)
           (treeset-delete! set item)
           (treemap-delete! map item)
           ($rbset-verify set)
           ($rbtree-verify map))
         items)
       (and (treeset-empty? set) (treemap-empty? map))))

(mat fixnum-boundaries-and-mixed-folds
     (let ([set (fixnum-treeset fx= fx> (most-negative-fixnum) 0 (most-positive-fixnum))]
           [one (fixnum-treeset fx= fx< 1 2)]
           [two (treeset fx= fx< 3 4)]
           [three (fixnum-treeset fx= fx< 5 6)])
       (and ($rbset-verify set)
            (= (most-positive-fixnum) (treeset-min set))
            (= 21 (treeset-fold-left + 0 one two three))
            (= 21 (treeset-fold-right + 0 one two three))
            (fixnum-treeset? (treeset+ one two three)))))
