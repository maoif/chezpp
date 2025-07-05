(import (chezpp))




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


(mat record-match

     (begin
       (record Person (name age sex))
       #t)

     (Person? (Person "Jack" 16 'male))
     (not (Person? 1))

     (equal? "Jack"
             (match (Person "Jack" 16 'male)
               [,($rec Person (name ,x)) x]))

     (equal? (list "Jack" 16 'male)
             (match-Person (Person "Jack" 16 'male)
                           [(,name ,age ,sex) (list name age sex)]))
     (equal? (list "Jack" 'male)
             (match-Person (Person "Jack" 16 'male)
                           [(,name _ ,sex) (list name sex)]))
     (equal? (list 16 "Jack")
             (match-Person (Person "Jack" 16 'male)
                           [(,name ,age _) (list age name)]))

     (equal? 'small
             (match-Person (Person "Jack" 23 'male)
                           [(,name ,age _)
                            (guard (< age 24))
                            'small]
                           [(,name ,age _)
                            (guard (< age 50))
                            'big]
                           [else 'old]))
     (equal? 'old
             (match-Person (Person "Jack" 51 'male)
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


(mat record-with-custom-type-predicate

     (begin (record Dog A-Dog? [name age sex])
            #t)
     (A-Dog? (Dog "Furry" 3 'male))
     (not (A-Dog? #f))

     (begin (record Dog A-Dog? [(name string?) (age natural?) sex])
            #t)
     (A-Dog? (Dog "Furry" 3 'male))
     (not (A-Dog? #f))

     )


(mat record-with-predicate

     (begin (define sex? (lambda (x) (or (eq? x 'male) (eq? x 'female))))
            (record Person
                    [(name   string?)
                     (height natural?)
                     (sex    sex?)
                     (age    natural?)])
            #t)
     (error? (Person 'Jack 170 'male 19))
     (error? (Person "Jack" -170 'male 19))
     (error? (Person "Jack" 170 'mmmale 19))
     (error? (Person "Jack" 170 'male -19))

     (let ([p (Person "Jack" 170 'male 19)])
       (Person? p))

     )


(mat record-with-mutability

     (begin (define sex? (lambda (x) (or (eq? x 'male) (eq? x 'female))))
            (record Person
                    [(mutable   name   string?)
                     (mutable   height natural?)
                     (immutable sex    sex?)
                     (immutable age    natural?)])
            #t)
     (error? (Person 'Jack 170 'male 19))
     (error? (Person "Jack" -170 'male 19))
     (error? (Person "Jack" 170 'mmmale 19))
     (error? (Person "Jack" 170 'male -19))

     (let ([p (Person "Jack" 170 'male 19)])
       (Person? p))

     (error? (let ([p (Person "Jack" 170 'male 19)])
               (Person-name-set! p 'sym)))
     (error? (let ([p (Person "Jack" 170 'male 19)])
               (Person-height-set! p -60)))

     (let ([p (Person "Jack" 170 'male 19)])
       (Person-name-set! p "New Jack")
       (equal? (Person-name p) "New Jack"))
     (let ([p (Person "Jack" 170 'male 19)])
       (Person-height-set! p 60)
       (= (Person-height p) 60))

     )


;; good pred name


;; good variant name


;; ensure names are unique


;; variant field mutability


;; vfield predicate


;; (de)serialization
