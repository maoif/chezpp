(import (chezpp))


(mat simple-datatype

     (begin (datatype Term
                      [Var   var]
                      [Const const]
                      [Abs   binder body]
                      [App   rator rand])
            #t)

     (procedure? Var)
     (procedure? Const)
     (procedure? Abs)
     (procedure? App)
     (procedure? Term-Var-var)
     (procedure? Term-Const-const)
     (procedure? Term-Abs-binder)
     (procedure? Term-Abs-body)
     (procedure? Term-App-rator)
     (procedure? Term-App-rand)


     (Term? (Var 'v))
     (Term? (Const 42))
     (Term? (Abs 'x 'body))
     (Term? (App 'f 'x))
     (Term? (Abs 'x (Abs 'y (Const 'f))))
     (Term? (App (Abs 'x (Var 'f)) (Const 42)))
     (not (Term? 42))
     (not (Term? (list)))
     (not (Term? (vector)))


     ;; singleton
     (begin (datatype Color [R] [G] [B])
            #t)

     (Color? R)
     (Color? G)
     (Color? B)
     (not (Color? 42))
     (not (procedure? R))
     (not (procedure? G))
     (not (procedure? B))


     ;; mixed
     (begin (datatype Direction
                      [X length]
                      [Y length]
                      [Still])
            #t)

     (procedure? X)
     (procedure? Y)
     (not (procedure? Still))
     (Direction? (X 10))
     (Direction? (Y 10))
     (Direction? Still)

     )


(mat datatype-with-custom-type-predicate

     (begin (datatype Term A-Term?
                      [Var   var]
                      [Const const]
                      [Abs   binder body]
                      [App   rator rand])
            #t)

     (A-Term? (Var 'v))
     (A-Term? (Const 42))
     (A-Term? (Abs 'x 'body))
     (A-Term? (App 'f 'x))
     (A-Term? (Abs 'x (Abs 'y (Const 'f))))
     (A-Term? (App (Abs 'x (Var 'f)) (Const 42)))
     (not (A-Term? 42))
     (not (A-Term? (list)))
     (not (A-Term? (vector)))


     (begin (datatype Color Prim-Color? [R] [G] [B])
            #t)

     (Prim-Color? R)
     (Prim-Color? G)
     (Prim-Color? B)
     (not (Prim-Color? 42))

     )


(mat datatype-with-variant-field-predicate

     (begin (datatype Term Term?
                      [Var
                       (var symbol?)]
                      [Const
                       (const number?)]
                      [Abs
                       (binder symbol?)
                       (body Term?)]
                      [App
                       (rator Term?)
                       (rand  Term?)])
            #t)
     (error? (Var 42))
     (error? (Const 'bla))
     (error? (Abs 'v 'bla))
     (error? (Abs 42 (Var 'v)))
     (error? (App 'v (Const 42)))
     (error? (App (Const 42) 'v))

     (procedure? Var)
     (procedure? Const)
     (procedure? Abs)
     (procedure? App)
     (Term? (Var 'v))
     (Term? (Const 42))
     (Term? (Abs 'x (Abs 'y (Const 42))))
     (Term? (App (Abs 'x (Var 'f)) (Const 42)))
     (not (Term? 42))
     (not (Term? (list)))
     (not (Term? (vector)))

     )


(mat datatype-with-mutability

     (begin (datatype Term Term?
                      [Var
                       (mutable var symbol?)]
                      [Const
                       (mutable const number?)]
                      [Abs
                       (immutable binder symbol?)
                       (immutable body Term?)]
                      [App
                       (immutable rator Term?)
                       (immutable rand  Term?)])
            #t)
     (error? (Var 42))
     (error? (Const 'bla))
     (error? (Abs 'v 'bla))
     (error? (Abs 42 (Var 'v)))
     (error? (App 'v (Const 42)))
     (error? (App (Const 42) 'v))

     (procedure? Var)
     (procedure? Const)
     (procedure? Abs)
     (procedure? App)
     (procedure? Term-Var-var-set!)
     (procedure? Term-Const-const-set!)

     (error? (let ([t (Var 'v)])
               (Term-Var-var-set! t 42)))
     (error? (let ([t (Const 42)])
               (Term-Const-const-set! t 'bla)))

     (let ([t (Var 'v)])
       (Term-Var-var-set! t 'vvv)
       (eq? 'vvv
            (Term-Var-var t)))
     (let ([t (Const 42)])
       (Term-Const-const-set! t 24)
       (= 24
          (Term-Const-const t)))

     )


(mat match-datatype-singleton

     (begin (datatype Color [R] [G] [B])
            #t)

     (= 1
        (match-Color R
                     [R 1]
                     [G 2]
                     [B 3]))
     (= 2
        (match-Color G
                     [R 1]
                     [G 2]
                     [B 3]))
     (= 3
        (match-Color B
                     [R 1]
                     [G 2]
                     [B 3]))

     (error? (match-Color 'bla
                          [R 1]
                          [G 2]
                          [B 3]))
     (error? (match-Color 42
                          [R 1]
                          [G 2]
                          [B 3]))

     )


(mat match-datatype

     (begin
       (datatype Tree [Nil] [Node val (left Tree?) (right Tree?)])
       #t)

     ;; match by position
     (eq? 'val
          (match-Tree (Node 'val Nil Nil)
                      [Nil 'nil]
                      [(Node ,x _ _) x]))

     ;; match by name
     (equal? 2
             (match-Tree (Node '(1 2 3) Nil Nil)
                         [Nil 'nil]
                         [(Node (val (_ ,x _))) x]))

     (eq? 42
          (match-Tree (Node 42 Nil Nil)
                      [Nil 'nil]
                      [(Node (val ,v)) v]))
     (eq? Nil
          (match-Tree (Node 42 Nil Nil)
                      [Nil 'nil]
                      [(Node (right ,v)) v]))
     (equal? (list 42 Nil)
             (match-Tree (Node 42 Nil Nil)
                         [Nil 'nil]
                         [(Node ,v _ ,y) (list v y)]))
     (eq? 'nil
          (match-Tree! Nil
                       [(Node (val ,v)) v]
                       [Nil 'nil]))

     (string-contains?
      (condition-message
       (guard (e [#t e])
         (expand '(match-Tree! Nil
                               [(Node (val ,v)) v]))))
      "incomplete variants for")

     )



;; good pred name


;; good variant name


;; ensure names are unique


;; variant field mutability


;; vfield predicate


;; (de)serialization
