(import (chezpp private rbtree))

(mat specialized-node-layouts
     (and (= 5 (vector-length (rbset-node 1 '())))
          (= 5 (vector-length (fxrbnode 1 '())))
          (= 6 (vector-length (vector 1 2 '() '() '() 0)))))

(mat specialized-key-writes
     (let ([n (fxrbnode 1 '())])
       (fxrbnode-key-set! n 42)
       (= 42 (fxrbnode-key n))))
