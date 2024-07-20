(library (chezpp list)
  (export map! map/i map!/i for-each/i fold-left/i fold-right/i
          unique? unique
          list-last
          make-list-builder
          zip
          nums)
  (import (chezscheme)
          (chezpp utils)
          (chezpp internal))


  (define check-length
    (lambda (who . ls*)
      (unless (null? ls*)
        (unless (apply fx= (map length ls*))
          (errorf who "lists are not of the same length")))))
  (define all-lists? (lambda (ls*) (andmap list? ls*)))


  #|doc
  Similar to `map`, but the values returned by `proc` are stored directly into `ls0`.
  Return the first list.
  |#
  (define map!
    (case-lambda
      [(proc ls0)
       (pcheck ([procedure? proc] [list? ls0])
               (let loop ([ls ls0])
                 (if (null? ls)
                     ls0
                     (begin (set-car! ls (proc (car ls)))
                            (loop (cdr ls))))))]
      [(proc ls0 ls1)
       (pcheck ([procedure? proc] [list? ls0] [list? ls1])
               (check-length 'map! ls0 ls1)
               (let loop ([ls ls0] [ls1 ls1])
                 (if (null? ls)
                     ls0
                     (begin (set-car! ls (proc (car ls) (car ls1)))
                            (loop (cdr ls) (cdr ls1))))))]
      [(proc ls0 ls1 ls2)
       (pcheck ([procedure? proc] [list? ls0] [list? ls1] [list? ls2])
               (check-length 'map! ls0 ls1 ls2)
               (let loop ([ls ls0] [ls1 ls1] [ls2 ls2])
                 (if (null? ls)
                     ls0
                     (begin (set-car! ls (proc (car ls) (car ls1) (car ls2)))
                            (loop (cdr ls) (cdr ls1) (cdr ls2))))))]
      [(proc ls0 . ls*)
       (let ([ls* (cons ls0 ls*)])
         (pcheck ([procedure? proc] [all-lists? ls*])
                 (apply check-length 'map! ls*)
                 (let loop ([ls ls0] [ls* ls*])
                   (if (null? ls)
                       ls0
                       (begin (set-car! ls (apply proc (map car ls*)))
                              (loop (cdr ls) (map cdr ls*)))))))]))


  #|doc
  Similar to `map!`, but `proc` takes as its first argument the index of the list items.
  Return the first list.
  |#
  (define map!/i
    (case-lambda
      [(proc ls0)
       (pcheck ([procedure? proc] [list? ls0])
               (let loop ([i 0] [ls ls0])
                 (if (null? ls)
                     ls0
                     (begin (set-car! ls (proc i (car ls)))
                            (loop (add1 i) (cdr ls))))))]
      [(proc ls0 ls1)
       (pcheck ([procedure? proc] [list? ls0] [list? ls1])
               (check-length 'map! ls0 ls1)
               (let loop ([i 0] [ls ls0] [ls1 ls1])
                 (if (null? ls)
                     ls0
                     (begin (set-car! ls (proc i (car ls) (car ls1)))
                            (loop (add1 i) (cdr ls) (cdr ls1))))))]
      [(proc ls0 ls1 ls2)
       (pcheck ([procedure? proc] [list? ls0] [list? ls1] [list? ls2])
               (check-length 'map! ls0 ls1 ls2)
               (let loop ([i 0] [ls ls0] [ls1 ls1] [ls2 ls2])
                 (if (null? ls)
                     ls0
                     (begin (set-car! ls (proc i (car ls) (car ls1) (car ls2)))
                            (loop (add1 i) (cdr ls) (cdr ls1) (cdr ls2))))))]
      [(proc ls0 . ls*)
       (let ([ls* (cons ls0 ls*)])
         (pcheck ([procedure? proc] [all-lists? ls*])
                 (apply check-length 'map! ls*)
                 (let loop ([i 0] [ls ls0] [ls* ls*])
                   (if (null? ls)
                       ls0
                       (begin (set-car! ls (apply proc i (map car ls*)))
                              (loop (add1 i) (cdr ls) (map cdr ls*)))))))]))


  #|doc
  Similar to `map`, but `proc` takes as its first argument the index of the list items.
  |#
  (define map/i
    (case-lambda
      [(proc ls0)
       (pcheck ([procedure? proc] [list? ls0])
               (let ([lb (make-list-builder)])
                 (let loop ([i 0] [ls ls0])
                   (if (null? ls)
                       (lb)
                       (begin (lb (proc i (car ls)))
                              (loop (add1 i) (cdr ls)))))))]
      [(proc ls0 ls1)
       (pcheck ([procedure? proc] [list? ls0] [list? ls1])
               (check-length 'map/i ls0 ls1)
               (let ([lb (make-list-builder)])
                 (let loop ([i 0] [ls ls0] [ls1 ls1])
                   (if (null? ls)
                       (lb)
                       (begin (lb (proc i (car ls) (car ls1)))
                              (loop (add1 i) (cdr ls) (cdr ls1)))))))]
      [(proc ls0 ls1 ls2)
       (pcheck ([procedure? proc] [list? ls0] [list? ls1] [list? ls2])
               (check-length 'map/i ls0 ls1 ls2)
               (let ([lb (make-list-builder)])
                 (let loop ([i 0] [ls ls0] [ls1 ls1] [ls2 ls2])
                   (if (null? ls)
                       (lb)
                       (begin (lb (proc i (car ls) (car ls1) (car ls2)))
                              (loop (add1 i) (cdr ls) (cdr ls1) (cdr ls2)))))))]
      [(proc ls0 . ls*)
       (let ([ls* (cons ls0 ls*)])
         (pcheck ([procedure? proc] [all-lists? ls*])
                 (apply check-length 'map/i ls*)
                 (let ([lb (make-list-builder)])
                   (let loop ([i 0] [ls ls0] [ls* ls*])
                     (if (null? ls)
                         (lb)
                         (begin (lb (apply proc i (map car ls*)))
                                (loop (add1 i) (cdr ls) (map cdr ls*))))))))]))


  #|doc
  Similar to `for-each`, but `proc` takes as its first argument the index of the list items.
  |#
  (define for-each/i
    (case-lambda
      [(proc ls0)
       (pcheck ([procedure? proc] [list? ls0])
               (let loop ([i 0] [ls ls0])
                 (unless (null? ls)
                   (begin (proc i (car ls))
                          (loop (add1 i) (cdr ls))))))]
      [(proc ls0 ls1)
       (pcheck ([procedure? proc] [list? ls0] [list? ls1])
               (check-length 'for-each/i ls0 ls1)
               (let loop ([i 0] [ls ls0] [ls1 ls1])
                 (unless (null? ls)
                   (begin (proc i (car ls) (car ls1))
                          (loop (add1 i) (cdr ls) (cdr ls1))))))]
      [(proc ls0 ls1 ls2)
       (pcheck ([procedure? proc] [list? ls0] [list? ls1] [list? ls2])
               (check-length 'for-each/i ls0 ls1 ls2)
               (let loop ([i 0] [ls ls0] [ls1 ls1] [ls2 ls2])
                 (unless (null? ls)
                   (begin (proc i (car ls) (car ls1) (car ls2))
                          (loop (add1 i) (cdr ls) (cdr ls1) (cdr ls2))))))]
      [(proc ls0 . ls*)
       (let ([ls* (cons ls0 ls*)])
         (pcheck ([procedure? proc] [all-lists? ls*])
                 (apply check-length 'for-each/i ls*)
                 (let loop ([i 0] [ls ls0] [ls* ls*])
                   (unless (null? ls)
                     (begin (apply proc i (map car ls*))
                            (loop (add1 i) (cdr ls) (map cdr ls*)))))))]))


  #|doc
  Similar to `fold-left`, but `proc` takes as its first argument the index of the list items.
  |#
  (define fold-left/i
    (case-lambda
      [(proc acc ls0)
       (pcheck ([procedure? proc] [list? ls0])
               (let loop ([i 0] [acc acc] [ls ls0])
                 (if (null? ls)
                     acc
                     (loop (add1 i) (proc i acc (car ls)) (cdr ls)))))]
      [(proc acc ls0 ls1)
       (pcheck ([procedure? proc] [list? ls0] [list? ls1])
               (check-length 'fold-left/i ls0 ls1)
               (let loop ([i 0] [acc acc] [ls ls0] [ls1 ls1])
                 (if (null? ls)
                     acc
                     (loop (add1 i) (proc i acc (car ls) (car ls1)) (cdr ls) (cdr ls1)))))]
      [(proc acc ls0 ls1 ls2)
       (pcheck ([procedure? proc] [list? ls0] [list? ls1] [list? ls2])
               (check-length 'fold-left/i ls0 ls1 ls2)
               (let loop ([i 0] [acc acc] [ls ls0] [ls1 ls1] [ls2 ls2])
                 (if (null? ls)
                     acc
                     (loop (add1 i) (proc i acc (car ls) (car ls1) (car ls2)) (cdr ls) (cdr ls1) (cdr ls2)))))]
      [(proc acc ls0 . ls*)
       (let ([ls* (cons ls0 ls*)])
         (pcheck ([procedure? proc] [all-lists? ls*])
                 (apply check-length 'fold-left/i ls*)
                 (let loop ([i 0] [acc acc] [ls ls0] [ls* ls*])
                   (if (null? ls)
                       acc
                       (loop (add1 i) (apply proc i acc (map car ls*)) (cdr ls) (map cdr ls*))))))]))


  #|doc
  Similar to `fold-right`, but `proc` takes as its first argument the index of the list items.
  |#
  ;; the index goes backwards
  (define fold-right/i
    (case-lambda
      [(proc acc ls0)
       (pcheck ([procedure? proc] [list? ls0])
               (let loop ([i 0] [ls ls0])
                 (if (null? ls)
                     acc
                     (proc i (car ls) (loop (add1 i) (cdr ls))))))]
      [(proc acc ls0 ls1)
       (pcheck ([procedure? proc] [list? ls0] [list? ls1])
               (check-length 'fold-right/i ls0 ls1)
               (let loop ([i 0] [ls ls0] [ls1 ls1])
                 (if (null? ls)
                     acc
                     (proc i (car ls) (car ls1) (loop (add1 i) (cdr ls) (cdr ls1))))))]
      [(proc acc ls0 ls1 ls2)
       (pcheck ([procedure? proc] [list? ls0] [list? ls1] [list? ls2])
               (check-length 'fold-right/i ls0 ls1 ls2)
               (let loop ([i 0] [ls ls0] [ls1 ls1] [ls2 ls2])
                 (if (null? ls)
                     acc
                     (proc i (car ls) (car ls1) (car ls2) (loop (add1 i) (cdr ls) (cdr ls1) (cdr ls2))))))]
      [(proc acc ls0 . ls*)
       (let ([ls* (cons ls0 ls*)])
         (pcheck ([procedure? proc] [all-lists? ls*])
                 (apply check-length 'fold-right/i ls*)
                 (let loop ([i 0] [ls ls0] [ls* ls*])
                   (if (null? ls)
                       acc
                       (apply proc i `(,@(map car ls*) ,(loop (add1 i) (cdr ls) (map cdr ls*))))))))]))

  #|doc
  Tests whether items in a list are unique (no duplicates).
  |#
  (define unique?
    (case-lambda
      [(ls) (unique? ls equal?)]
      [(ls eq)
       (pcheck ([list? ls] [procedure? eq])
               (let loop ([ls ls])
                 (if (null? ls)
                     #t
                     (if (memp (lambda (x) (eq x (car ls))) (cdr ls))
                         #f
                         (loop (cdr ls))))))]))

  #|doc
  Returnes a list where duplicates are removed.
  Note that the relative order of the items in the list is not preserved.
  |#
  (define unique
    (case-lambda
      [(ls) (unique ls equal?)]
      [(ls eq)
       (pcheck ([list? ls] [procedure? eq])
               (let loop ([ls ls] [res '()])
                 (if (null? ls)
                     res
                     (let ([item (car ls)])
                       (if (memp (lambda (x) (eq x item)) (cdr ls))
                           (loop (cdr ls) res)
                           (loop (cdr ls) (cons item res)))))))]))


  #|doc
  Return the last element of a list.
  |#
  (define list-last
    (lambda (ls)
      (pcheck-list (ls)
                   (if (null? ls)
                       (errorf 'list-last "list is null")
                       (let loop ([ls ls])
                         (if (null? (cdr ls))
                             (car ls)
                             (loop (cdr ls))))))))


  (define zip
    (lambda (ls1 ls2 . ls*)
      (apply map list ls1 ls2 ls*)))


  #|doc
  Build a list from left to right so you don't have to use cons and reverse.
  Return a procedure `p` such that `(p x ...)` adds the items to the list
  and `(p)` returns the list built.
  |#
  (define make-list-builder
    (lambda args
      (let ([res args])
        (let ([current-cell (if (null? res)
                                (cons #f '())
                                (let loop ([res res])
                                  (if (null? (cdr res))
                                      res
                                      (loop (cdr res)))))]
              [next-cell (cons #f '())])
          (define add-item!
            (lambda (item)
              (if (null? res)
                  (begin (set-car! current-cell item)
                         (set! res current-cell))
                  (begin
                    (set-car! next-cell item)
                    (set-cdr! current-cell next-cell)
                    (set! current-cell next-cell)
                    (set! next-cell (cons #f '()))))))
          (rec lb
            (case-lambda
              [() res]
              [(x) (add-item! x)]
              [x* (for-each lb x*)]))))))


  (define nums
    (case-lambda
      [(stop) (nums 0 stop 1)]
      [(start stop) (nums start stop 1)]
      [(start stop step)
       (let ([lb (make-list-builder)])
         (let loop ([n start])
           (if (>= n stop)
               (lb)
               (begin (lb n)
                      (loop (+ n step))))))]))



  )
