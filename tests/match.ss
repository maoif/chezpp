(import (chezpp match))


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
(mat list-with-ellipses-in-the-end
     (match '(1)
       [(,a ...) #t]
       [else #f])

     (equal? '(1 2 3)
             (match '(1 2 3)
               [(,a ...) a]
               [else #f]))

     (equal? '(2 3)
             (match '(1 2 3)
               [(,a ,b ...) b]
               [else #f]))

     (equal? '(1 2 3 4)
             (match '((1 11) (2 22) (3 33) (4 44))
               [((,a ,b) ...) a]))

     (equal? '(11 22 33 44)
             (match '((1 11) (2 22) (3 33) (4 44))
               [((,a ,b) ...) b]))

     (equal? '((1 11) (2 22) (3 33) (4 44))
             (match '((1 11) (2 22) (3 33) (4 44))
               [((,a ...) ...) a]))

     )

(mat list-with-ellipses-in-the-middle

     (equal? '(4 44)
             (match '((1 11) (2 22) (3 33) (4 44))
               [((,a ...) ... ,b) b]))

     ;; full match after ...
     (equal? '()
             (match '((1 11) (2 22) (3 33) (4 44))
               [((,a ...) ... ,b ,c ,d ,e) a]))

     )

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
