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
        (match R
          [,(Color R) 1]
          [,(Color G) 2]
          [,(Color B) 3]))
     (= 2
        (match G
          [,(Color R) 1]
          [,(Color G) 2]
          [,(Color B) 3]))
     (= 3
        (match B
          [,(Color R) 1]
          [,(Color G) 2]
          [,(Color B) 3]))

     (error? (match 'bla
               [,(Color R) 1]
               [,(Color G) 2]
               [,(Color B) 3]))
     (error? (match 42
               [,(Color R) 1]
               [,(Color G) 2]
               [,(Color B) 3]))

     )


(mat match-datatype

     (begin
       (datatype Tree [Nil] [Node val (left Tree?) (right Tree?)])
       #t)

     ;; match by position
     (eq? 'val
          (match (Node 'val Nil Nil)
            [,(Tree Nil) 'nil]
            [,(Tree Node ,x _ _) x])) ;; FIXME

     ;; match by name
     (equal? 2
             (match (Node '(1 2 3) Nil Nil)
               [,(Tree Nil) 'nil]
               [,(Tree Node (val (_ ,x _))) x]))

     (eq? 42
          (match (Node 42 Nil Nil)
            [,(Tree Nil) 'nil]
            [,(Tree Node (val ,v)) v]))
     (eq? Nil
          (match (Node 42 Nil Nil)
            [,(Tree Nil) 'nil]
            [,(Tree Node (right ,v)) v]))
     (equal? (list 42 Nil)
             (match (Node 42 Nil Nil)
               [,(Tree Nil) 'nil]
               [,(Tree Node ,v _ ,y) (list v y)]))


     ;; nested patterns

     (begin (datatype Json
                      [JNull]
                      [JBool   b]
                      [JNumber num]
                      [JString str]
                      [JArray  arr]
                      [JObject obj])
            #t)

     (match JNull
       [,(Json JNull) #t])

     (match (JBool #t)
       [,(Json JBool ,x) x])

     (match (JBool #f)
       [,(Json JBool ,x) (not x)])

     (match (JNumber 42)
       [,(Json JNumber ,x)
        (= x 42)])

     (match (JString "jstr")
       [,(Json JString ,x)
        (string=? x "jstr")])

     (match (JArray (list JNull (JBool #t) (JBool #f) (JNumber 42) (JString "jstr")))
       [,(Json JArray (,(Json JNull)
                       ,(Json JBool   ,x0)
                       ,(Json JBool   ,x1)
                       ,(Json JNumber ,x2)
                       ,(Json JString ,x3)))
        (equal? (list x0 x1 x2 x3)
                (list #t #f 42 "jstr"))])

     (match (JObject (list
                      (cons "Null"   JNull)
                      (cons "True"   (JBool   #t))
                      (cons "False"  (JBool   #f))
                      (cons "Number" (JNumber 42))
                      (cons "String" (JString "jstr"))))
       [,(Json JObject ((,n0 . ,(Json JNull))
                        (,n1 . ,(Json JBool   ,x0))
                        (,n2 . ,(Json JBool   ,x1))
                        (,n3 . ,(Json JNumber ,x2))
                        (,n4 . ,(Json JString ,x3))))
        (equal? (list n0 n1 n2 n3 n4)
                (list "Null" "True" "False" "Number" "String"))])

     )



;; good pred name


;; good variant name


;; ensure names are unique


;; variant field mutability


;; vfield predicate


;; (de)serialization
