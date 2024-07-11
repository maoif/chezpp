(library (chezpp list)
  (export unique? unique
          list-last
          make-list-builder
          nums)
  (import (chezscheme)
          (chezpp utils)
          (chezpp internal))

  (define-syntax map/i
    (lambda (proc ls . ls*)
      (todo)))

  (define-syntax for-each/i
    (lambda (proc ls . ls*)
      (todo)))

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
