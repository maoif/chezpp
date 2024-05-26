(import (chezpp match))

(mat else-clause

     (match 1
       [else #t])

     )


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


(mat vector-with-ellipses-in-the-end

     (equal? '((33 333) (22 222) (11 111))
             (match '#(#(1 2 3) #(11 22 33) #(111 222 333))
               [#(_ #(,a ,b ,c) ...) (list c b a)]))

     (equal? '((22 33) (222 333))
             (match '#(#(1 2 3) #(11 22 33) #(111 222 333))
               [#(_ #(,a ,b ...) ...) b]))

     (equal? '((1 4 #()) (2 5 #()) (3 6 #()))
             (match '(#(1 2 3) #(4 5 6) #(#() #() #()))
               [(#(,a 3 _)) #f]
               [(#(,a ,b ,c) ...)
                (list a b c)]))

     )


(mat vector-with-ellipses-in-the-middle

     (equal? '((22 33) (222 333))
             (match '#(#(1 2 3) #(11 22 33) #(111 222 333) #(1111 2222 3333 4444))
               [#(_ #(,a ,b ...) ... ,c) b]))

     (equal? '#(1111 2222 3333 4444)
             (match '#(#(1 2 3) #(11 22 33) #(111 222 333) #(1111 2222 3333 4444))
               [#(_ #(,a ,b ...) ... ,c) c]))

     (equal?
      '(((a a) (a a) (a a) (a a))
        (((1 2) (1 2)) ((1 2) (1 2)) ((1 2) (1 2)) ((1 2) (1 2)))
        ((3 3) (3 3) (3 3) (3 3)))
      (match '#((#(a (1 2 3) c) #(a (1 2 3) c) #(a (1 2 3) c)) (#(a (1 2 3) c) #(a (1 2 3) c) #(a (1 2 3) c))
                (#(a (1 2 3) c) #(a (1 2 3) c) #(a (1 2 3) c)) (#(a (1 2 3) c) #(a (1 2 3) c) #(a (1 2 3) c)))
        [#((#(,x (,y ... ,z) _) ... _) ...)
         (list x y z)]))

     )


(mat simple-box

     (equal? 1
             (match (box 1)
               [,($b ,a) a]))

     (equal? (box 222)
             (match (box (box 222))
               [,($b ,a) a]))

     (equal? 222
             (match (box (box 222))
               [,($b ,($box ,a)) a]))

     )


;;;; record



;;;; datatype




;;;; catamorphism



;;;; in-place update





;;;; error tests
