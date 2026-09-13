(import (chezpp private rbtree))

(mat specialized-node-layouts
     (and (= 5 (vector-length (rbset-node 1 '())))
          (= 5 (vector-length (fxrbnode 1 '())))
          (= 6 (vector-length (vector 1 2 '() '() '() 0)))))

(mat specialized-key-writes
     (let ([n (fxrbnode 1 '())])
       (fxrbnode-key-set! n 42)
       (= 42 (fxrbnode-key n))))

(mat rbset-basic
     (let ([s (make-rbset #f fx= fx< 0)])
       (for-each (lambda (x) (rbset-set! s x)) '(4 1 3 2))
       (rbset-delete! s 3)
       (and (rbset? s)
            (= 3 (rbset-size s))
            (rbset-contains? s 2)
            (equal? '(1 2 4) (rbset->list s))
            (begin (rbset-clear! s) (= 0 (rbset-size s))))))
