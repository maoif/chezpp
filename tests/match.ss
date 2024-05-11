(import (eXtra match))


;;;; simple list

(mat simple-list
     (match '()
       [() #t]
       [else #f])

     (match '()
       [() #t])

     (equal? 1
             (match '(1 . 2)
               [(,a . ,b) a]))
     (equal? 2
             (match '(1 . 2)
               [(,a . ,b) b]))
     (match '(1 . 2)
       [(,a . ,b) #t])

     (match '(#t . #f)
       [(,t . ,f) t])

     (not
      (match '(#t . #f)
        [(,t . ,f) f]))

     (equal? 2
             (match '(1 2 3)
               [(_ ,x _) x]))

     (equal? '(2 . 22)
             (match '(1 (2 . 22) 3)
               [(_ ,x  _) x]))

     )




;;;; list with ellipses


;;;; vector



;;;; record



;;;; datatype




;;;; catamorphism



;;;; in-place update





;;;; error tests
