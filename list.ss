(library (chezpp list)
  (export unique? unique)
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


  (define zip
    (lambda (ls1 ls2 . ls*)
      (pcheck-list (ls1 ls2)
                   (todo))
      ))


  ;; list comprehension

  )
