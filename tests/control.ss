(import (chezpp control)
        (chezpp string)
        (chezpp utils))


(mat compose

     ;; not proc
     (error? (compose 1))
     (error? (compose 1 #\A))
     (error? (compose ""))
     (error? (compose zip list 'a))


     ;; wrong #ret val
     (error? ((compose sub1 (lambda (x) (values x x)) random) 100))
     (error? ((compose
               list
               (lambda (x y a) (values b a y))
               (lambda (x y) (values (* x x) (* y y) x y))
               (lambda (x y) (values (add1 x) (add1 y))))
              1 2))

     (eq? cons (compose cons))
     (eq? list (compose list))

     (equal? ((compose vreverse list->vector string->list) "123456")
             '#(#\6 #\5 #\4 #\3 #\2 #\1))
     (equal? ((compose
               list
               (lambda (x y a b) (values b a y x))
               (lambda (x y) (values (* x x) (* y y) x y))
               (lambda (x y) (values (add1 x) (add1 y))))
              1 2)
             '(3 2 9 4))
     )


(mat compose1

     ;; not proc
     (error? (compose1 1))
     (error? (compose1 1 #\A))
     (error? (compose1 ""))
     (error? (compose1 zip list 'a))

     ;; wrong #ret val
     (error? ((compose1 sub1 (lambda (x) (values x x)) random) 100))
     (error? ((compose1
               list
               (lambda (x y a) (values b a y))
               (lambda (x y) (values (* x x) (* y y) x y))
               (lambda (x y) (values (add1 x) (add1 y))))
              1 2))

     (eq? cons (compose cons))
     (eq? list (compose list))

     (equal? ((compose1 vreverse list->vector string->list) "123456")
             '#(#\6 #\5 #\4 #\3 #\2 #\1))
     )


(mat >>>

     (error? (>>> 1))
     (error? (>>> 1 #\A))
     (error? (>>> "" 'a))
     (error? (>>> zip list 'a))

     ;; wrong arity
     (error? (>>> 10 random (lambda (x) (values x x)) sub1))

     (fx= 2 (>>> 1 (lambda (x) (values x x)) +))
     (equal? (>>> "123456" string->list list->vector vreverse)
             ((compose1 vreverse list->vector string->list) "123456"))
     )


(mat >>>1

     (error? (>>>1 1))
     (error? (>>>1 1 #\A))
     (error? (>>>1 "" 'a))
     (error? (>>>1 zip list 'a))

     ;; wrong arity
     (error? (>>>1 1 (lambda (x) (values x x)) +))

     (equal? (>>>1 "123456" string->list list->vector vreverse)
             ((compose1 vreverse list->vector string->list) "123456"))
     )


(mat sect

     (eq? (let ([add2 (sect + 2 _)])
            (add2 2))
          4)

     (eq? (let ([add2 (sect + _ 2)])
            (add2 2))
          4)

     (eq? (let ([add2* (sect + _ 2 ...)])
            (add2* 2 2 2))
          8)

     (string-contains?
      (condition-message
       (guard (e [#t e])
         (expand '(sect + ... _))))
      "... must appear last")

     (string-contains?
      (condition-message
       (guard (e [#t e])
         (expand '(sect + _ ... _))))
      "... must appear last")

     )


(mat sect+

     (eq? (let* ([raise (sect+ expt _ _)]
                 [^2 (raise 2)])
            (^2 2))
          4)

     (eq? (let ([f (sect+ + _ _ ...)])
            (((f 1) 1) 1 1 1))
          5)

     (string-contains?
      (condition-message
       (guard (e [#t e])
         (expand '(sect+ + ... _))))
      "... must appear last")

     (string-contains?
      (condition-message
       (guard (e [#t e])
         (expand '(sect+ + _ ... _))))
      "... must appear last")

     )
