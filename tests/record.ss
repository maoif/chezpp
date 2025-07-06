(import (chezpp))


(mat simple-record

     (begin (record Person (name age sex))
            #t)
     (procedure? Person)
     (procedure? Person-age)
     (procedure? Person-name)
     (procedure? Person-sex)
     (procedure? Person?)

     (Person? (Person "J" 14 'male))
     (not (Person? (list)))

     (= (Person-age (Person "J" 14 'male))
        14)
     (equal? (Person-name (Person "J" 14 'male))
             "J")
     (eq? (Person-sex (Person "J" 14 'male))
          'male)

     )

(mat record-with-custom-type-predicate

     (begin (record Dog A-Dog? (name age sex))
            #t)
     (A-Dog? (Dog "Furry" 3 'male))
     (not (A-Dog? #f))

     (begin (record Dog A-Dog? ([name string?] [age natural?] sex))
            #t)
     (A-Dog? (Dog "Furry" 3 'male))
     (not (A-Dog? #f))

     )


(mat record-with-predicate

     (begin (define sex? (lambda (x) (or (eq? x 'male) (eq? x 'female))))
            (record Person
                    ([name   string?]
                     [height natural?]
                     [sex    sex?]
                     [age    natural?]))
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
                    ([mutable   name   string?]
                     [mutable   height natural?]
                     [immutable sex    sex?]
                     [immutable age    natural?]))
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



;; good pred name


;; ensure names are unique


;; (de)serialization
