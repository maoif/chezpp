(import (chezpp adt)
        (chezpp match))

;; TODO how to detect syntax error?

;; good datatype name

(let ()
  (datatype Term Term?
            [Var
             (var symbol?)]
            [Const
             const]
            [Abs
             (binder symbol?)
             (body Term?)]
            [App
             (rator Term?)
             (rand  Term?)])

  #t)



(mat datatype-Tree

     (begin
       (datatype Tree [Nil] [Node val (left Tree?) (right Tree?)])
       #t)

     ;; match by position
     (eq? 'val
          (match-Tree (Node 'val Nil Nil)
                      [(Node ,x _ _) x]))

     ;; match by name
     (equal? 2
             (match-Tree (Node '(1 2 3) Nil Nil)
                         [(Node (val (_ ,x _))) x]))

     (eq? 42
          (match-Tree (Node 42 Nil Nil)
                      [(Node (val ,v)) v]))
     (eq? Nil
          (match-Tree (Node 42 Nil Nil)
                      [(Node (right ,v)) v]))
     (equal? (list 42 Nil)
             (match-Tree (Node 42 Nil Nil)
                         [(Node ,v _ ,y) (list v y)]))

     )


(mat record-Person

     (begin
       (record Person (name age sex))
       #t)

     (equal? "Jack"
             (match (Person "Jack" 16 'male)
               [,($rec Person (name ,x)) x]))

     (equal? (list "Jack" 16 'male)
             (match-Person (Person "Jack" 16 'male)
                           [(,name ,age ,sex) (list name age sex)]))
     (equal? (list "Jack" 'male)
             (match-Person(Person "Jack" 16 'male)
                          [(,name _ ,sex) (list name sex)]))
     (equal? (list 16 "Jack")
             (match-Person (Person "Jack" 16 'male)
                           [(,name ,age _) (list age name)]))

     (equal? 'small (match-Person (Person "Jack" 23 'male)
                                  [(,name ,age _)
                                   (guard (< age 24))
                                   'small]
                                  [(,name ,age _)
                                   (guard (< age 50))
                                   'big]
                                  [else 'old]))
     (equal? 'old (match-Person (Person "Jack" 51 'male)
                                [(,name ,age _)
                                 (guard (< age 24))
                                 'small]
                                [(,name ,age _)
                                 (guard (< age 50))
                                 'big]
                                [else 'old]))

     (equal? (list "Jack" 16 'male)
             (match (Person "Jack" 16 'male)
               [,($rec Person ,name ,age ,sex) (list name age sex)]))
     )


;; good pred name


;; good variant name


;; ensure names are unique


;; variant field mutability


;; vfield predicate


;; (de)serialization
