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

     (equal? 2
             (match '(1 2 3)
               [() #f]
               [(,a _) #f]
               [(_ ,x _) x]))

     (match '(1 2 3)
       [() #f]
       [(,a _) #f]
       [(_ ,x _ _) x]
       [else #t])

     (equal? 'x
             (match '(unquote x)
               [(unquote ,y) y]))


     ;; TODO guards

     )




;;;; list with ellipses


;;;; vector

(mat simple-vector

     (match '#()
       [#() #t]
       [else #f])

     (= 1 (match '#(1)
            [#(,a) a]
            [else #f]))

     (equal? (list 3 2 1)
             (match '#(1 2 3)
               [#(,a ,b ,c) (list c b a)]
               [else #f]))

     ;;nested

     (equal? (list "bad" "good" 3)
             (match '#(1 ("good" "bad") 3)
               [#() 'none]
               [#(,a _) #f]
               [#(_ (,a ,b) ,c) (list b a c)]
               [else #f]))

     (equal? '(11 22 33 44)
             (match '#(1 ((11 22) (33 44)) 3)
               [#(_ ((,a ,b) (,x ,y)) _) (list a b x y)]
               [else #f]))


     )





;;;; record



;;;; datatype




;;;; catamorphism



;;;; in-place update





;;;; error tests
