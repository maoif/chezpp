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
