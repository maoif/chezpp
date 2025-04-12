(library (chezpp list)
  (export map! map/i map!/i for-each/i fold-left/i fold-right/i
          scan-left-ex scan-left-in scan-right-ex scan-right-in
          unique? unique
          list-last list-set!
          make-list-builder
          zip zip! snoc!
          nums slice

          listp=? list=? listq=? listv=?
          listp<? list<? listq<? listv<?
          listp<=? list<=? listq<=? listv<=?
          listp>? list>? listq>? listv>?
          listp>=? list>=? listq>=? listv>=?

          list+ listp+ listq+ listv+
          list- listp- listq- listv-
          list& listp& listq& listv&
          list^ listp^ listq^ listv^)
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
      [(ls) (unique? equal? ls)]
      [(eq ls)
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
      [(ls) (unique equal? ls)]
      [(eq ls)
       (pcheck ([list? ls] [procedure? eq])
               (let ([lb (make-list-builder)])
                 (let loop ([ls ls] )
                   (if (null? ls)
                       (lb)
                       (let ([item (car ls)])
                         (if (memp (lambda (x) (eq x item)) (cdr ls))
                             (loop (cdr ls))
                             (begin (lb item)
                                    (loop (cdr ls)))))))))]))


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


  #|doc
  Imperatively store the value `v` into list `ls` at index `i`.
  It is an error if `i` is not a valid index of `ls`.
  |#
  (define-who list-set!
    (lambda (ls i v)
      (pcheck ([list? ls] [natural? i])
              (let ([len (length ls)])
                (when (>= i len)
                  (errorf who "~a is not a valid index for ~a" i ls))
                (let loop ([ls ls] [n 0])
                  (if (fx= n i)
                      (set-car! ls v)
                      (loop (cdr ls) (fx1+ n))))))))


  (define zip
    (lambda (ls1 ls2 . ls*)
      (apply map list ls1 ls2 ls*)))


  #|doc
  Similar to `zip`, but the zip result is stored directly in `ls1`,
  which is also the return value.
  |#
  (define-who zip!
    (case-lambda
      [(ls1 ls2)
       (pcheck-list (ls1 ls2)
                    (check-length who ls1 ls2)
                    (let ([ls ls1])
                      (let loop ([ls1 ls1] [ls2 ls2])
                        (if (null? ls1)
                            ls
                            (begin (set-car! ls1 (list (car ls1) (car ls2)))
                                   (loop (cdr ls1) (cdr ls2)))))))]
      [(ls1 ls2 . ls*)
       (pcheck ([list? ls1 ls2] [all-lists? ls*])
               (apply check-length who ls1 ls2 ls*)
               (let ([ls ls1])
                 (let loop ([ls1 ls1] [ls2 ls2] [ls* ls*])
                   (if (null? ls1)
                       ls
                       (begin (set-car! ls1 (apply list (car ls1) (car ls2) (map car ls*)))
                              (loop (cdr ls1) (cdr ls2) (map cdr ls*)))))))]))


  #|doc
  Imperatively append a value to the list.
  |#
  (define snoc!
    (lambda (ls x)
      (pcheck-list (ls)
                   (if (null? ls)
                       (list x)
                       (let loop ([l ls])
                         (if (null? (cdr l))
                             (begin (set-cdr! l (list x))
                                    ls)
                             (loop (cdr l))))))))


  (define-who scan-left-ex
    (case-lambda
      [(proc acc ls0)
       (pcheck ([procedure? proc] [list? ls0])
               (if (null? ls0)
                   '()
                   (let ([lb (make-list-builder acc)])
                     (let loop ([acc acc] [ls0 ls0])
                       (if (null? (cdr ls0)) ;; stop at the last
                           (lb)
                           (let ([nacc (proc acc (car ls0))])
                             (lb nacc)
                             (loop nacc (cdr ls0))))))))]
      [(proc acc ls0 ls1)
       (pcheck ([procedure? proc] [list? ls0 ls1])
               (check-length who ls0 ls1)
               (if (null? ls0)
                   '()
                   (let ([lb (make-list-builder acc)])
                     (let loop ([acc acc] [ls0 ls0] [ls1 ls1])
                       (if (null? (cdr ls0))
                           (lb)
                           (let ([nacc (proc acc (car ls0) (car ls1))])
                             (lb nacc)
                             (loop nacc (cdr ls0) (cdr ls1))))))))]
      [(proc acc ls0 ls1 ls2)
       (pcheck ([procedure? proc] [list? ls0 ls1 ls2])
               (check-length who ls0 ls1 ls2)
               (if (null? ls0)
                   '()
                   (let ([lb (make-list-builder acc)])
                     (let loop ([acc acc] [ls0 ls0] [ls1 ls1] [ls2 ls2])
                       (if (null? (cdr ls0))
                           (lb)
                           (let ([nacc (proc acc (car ls0) (car ls1) (car ls2))])
                             (lb nacc)
                             (loop nacc (cdr ls0) (cdr ls1) (cdr ls2))))))))]
      [(proc acc ls0 . ls*)
       (pcheck ([procedure? proc] [all-lists? ls*])
               (apply check-length who ls0 ls*)
               (if (null? ls0)
                   '()
                   (let ([lb (make-list-builder acc)])
                     (let loop ([acc acc] [ls* (cons ls0 ls*)])
                       (if (null? (cdr (car ls*)))
                           (lb)
                           (let ([nacc (apply proc acc (map car ls*))])
                             (lb nacc)
                             (loop nacc (map cdr ls*))))))))]))


  (define-who scan-left-in
    (case-lambda
      [(proc acc ls0)
       (pcheck ([procedure? proc] [list? ls0])
               (if (null? ls0)
                   '()
                   (let ([lb (make-list-builder)])
                     (let loop ([acc acc] [ls0 ls0])
                       (if (null? ls0)
                           (lb)
                           (let ([nacc (proc acc (car ls0))])
                             (lb nacc)
                             (loop nacc (cdr ls0))))))))]
      [(proc acc ls0 ls1)
       (pcheck ([procedure? proc] [list? ls0 ls1])
               (check-length who ls0 ls1)
               (if (null? ls0)
                   '()
                   (let ([lb (make-list-builder)])
                     (let loop ([acc acc] [ls0 ls0] [ls1 ls1])
                       (if (null? ls0)
                           (lb)
                           (let ([nacc (proc acc (car ls0) (car ls1))])
                             (lb nacc)
                             (loop nacc (cdr ls0) (cdr ls1))))))))]
      [(proc acc ls0 ls1 ls2)
       (pcheck ([procedure? proc] [list? ls0 ls1 ls2])
               (check-length who ls0 ls1 ls2)
               (if (null? ls0)
                   '()
                   (let ([lb (make-list-builder)])
                     (let loop ([acc acc] [ls0 ls0] [ls1 ls1] [ls2 ls2])
                       (if (null? ls0)
                           (lb)
                           (let ([nacc (proc acc (car ls0) (car ls1) (car ls2))])
                             (lb nacc)
                             (loop nacc (cdr ls0) (cdr ls1) (cdr ls2))))))))]
      [(proc acc ls0 . ls*)
       (pcheck ([procedure? proc] [all-lists? ls*])
               (apply check-length who ls0 ls*)
               (if (null? ls0)
                   '()
                   (let ([lb (make-list-builder)])
                     (let loop ([acc acc] [ls* (cons ls0 ls*)])
                       (if (null? (car ls*))
                           (lb)
                           (let ([nacc (apply proc acc (map car ls*))])
                             (lb nacc)
                             (loop nacc (map cdr ls*))))))))]))


  (define-who scan-right-ex
    (case-lambda
      [(proc acc ls0)
       (pcheck ([procedure? proc] [list? ls0])
               (if (null? ls0)
                   '()
                   (let ([lb (make-list-builder)])
                     (let loop ([ls0 (cdr ls0)]) ;; cdr: exclusive
                       (if (null? ls0)
                           (begin (lb acc)
                                  acc)
                           (let ([nacc (proc (car ls0)
                                             (loop (cdr ls0)))])
                             (lb nacc)
                             nacc)))
                     (lb))))]
      [(proc acc ls0 ls1)
       (pcheck ([procedure? proc] [list? ls0 ls1])
               (check-length who ls0 ls1)
               (if (null? ls0)
                   '()
                   (let ([lb (make-list-builder)])
                     (let loop ([ls0 (cdr ls0)] [ls1 (cdr ls1)])
                       (if (null? ls0)
                           (begin (lb acc)
                                  acc)
                           (let ([nacc (proc (car ls0) (car ls1)
                                             (loop (cdr ls0) (cdr ls1)))])
                             (lb nacc)
                             nacc)))
                     (lb))))]
      [(proc acc ls0 ls1 ls2)
       (pcheck ([procedure? proc] [list? ls0 ls1 ls2])
               (check-length who ls0 ls1 ls2)
               (if (null? ls0)
                   '()
                   (let ([lb (make-list-builder)])
                     (let loop ([ls0 (cdr ls0)] [ls1 (cdr ls1)] [ls2 (cdr ls2)])
                       (if (null? ls0)
                           (begin (lb acc)
                                  acc)
                           (let ([nacc (proc (car ls0) (car ls1) (car ls2)
                                             (loop (cdr ls0) (cdr ls1) (cdr ls2)))])
                             (lb nacc)
                             nacc)))
                     (lb))))]
      [(proc acc ls0 . ls*)
       (pcheck ([procedure? proc] [all-lists? ls*])
               (apply check-length who ls0 ls*)
               (if (null? ls0)
                   '()
                   (let ([lb (make-list-builder)])
                     (let loop ([ls* (map cdr (cons ls0 ls*))])
                       (if (null? (car ls*))
                           (begin (lb acc)
                                  acc)
                           (let ([nacc (apply proc `(,@(map car ls*) ,(loop (map cdr ls*))))])
                             (lb nacc)
                             nacc)))
                     (lb))))]))


  (define-who scan-right-in
    (case-lambda
      [(proc acc ls0)
       (pcheck ([procedure? proc] [list? ls0])
               (if (null? ls0)
                   '()
                   (let ([lb (make-list-builder)])
                     (let loop ([ls0 ls0])
                       (if (null? ls0)
                           acc
                           (let ([nacc (proc (car ls0)
                                             (loop (cdr ls0)))])
                             (lb nacc)
                             nacc)))
                     (lb))))]
      [(proc acc ls0 ls1)
       (pcheck ([procedure? proc] [list? ls0 ls1])
               (check-length who ls0 ls1)
               (if (null? ls0)
                   '()
                   (let ([lb (make-list-builder)])
                     (let loop ([ls0 ls0] [ls1 ls1])
                       (if (null? ls0)
                           acc
                           (let ([nacc (proc (car ls0) (car ls1)
                                             (loop (cdr ls0) (cdr ls1)))])
                             (lb nacc)
                             nacc)))
                     (lb))))]
      [(proc acc ls0 ls1 ls2)
       (pcheck ([procedure? proc] [list? ls0 ls1 ls2])
               (check-length who ls0 ls1 ls2)
               (if (null? ls0)
                   '()
                   (let ([lb (make-list-builder)])
                     (let loop ([ls0 ls0] [ls1 ls1] [ls2 ls2])
                       (if (null? ls0)
                           acc
                           (let ([nacc (proc (car ls0) (car ls1) (car ls2)
                                             (loop (cdr ls0) (cdr ls1) (cdr ls2)))])
                             (lb nacc)
                             nacc)))
                     (lb))))]
      [(proc acc ls0 . ls*)
       (pcheck ([procedure? proc] [all-lists? ls*])
               (apply check-length who ls0 ls*)
               (if (null? ls0)
                   '()
                   (let ([lb (make-list-builder)])
                     (let loop ([ls* (cons ls0 ls*)])
                       (if (null? (car ls*))
                           acc
                           (let ([nacc (apply proc `(,@(map car ls*) ,(loop (map cdr ls*))))])
                             (lb nacc)
                             nacc)))
                     (lb))))]))


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


  #|doc
  Generate a list of of numbers: start, start+step*1, start+step*2, ...

  `start`, `stop` and `step` must be numbers that meet the following requirements:
  If `start` is less than `stop`, then `step` must be greater than 0,
  in which case the sequence terminates when the value is greater than or equal to `stop`;
  If `start` is greater than `stop`, then `step` must be less than 0,
  in which case the sequence terminates when the value is less than or equal to `stop`.
  |#
  (define-who nums
    (case-lambda
      [(stop) (nums 0 stop 1)]
      [(start stop) (nums start stop 1)]
      [(start stop step)
       (pcheck ([number? start stop step])
               (let ([stop? (cond
                             [(and (<= start stop) (> step 0)) >=]
                             [(and (>= start stop) (< step 0)) <=]
                             [else (errorf who "invalid range: ~a, ~a, ~a" start stop step)])])
                 (let ([lb (make-list-builder)])
                   (let loop ([n start])
                     (if (stop? n stop)
                         (lb)
                         (begin (lb n)
                                (loop (+ n step))))))))]))


  #|doc
  Return a slice, or sublist of the original list.
  `start` and `end` specify the first and last item, respectively.
  `step` is the amount by which `start` is incremented every time an item is selected.

  By default, `start` is 0, and `step` is 1.
  `step` can be either positive or negative. If it is negative, items are selected backwards.
  It is an error if `step` is 0.

  If the indices are out of range in any way, an empty list is returned.
  |#
  (define-who slice
    (case-lambda
      [(ls end) (slice ls 0 end 1)]
      [(ls start end) (slice ls start end 1)]
      [(ls start end step)
       (pcheck ([list? ls] [fixnum? start end step])
               (when (fx= step 0) (errorf who "step cannot be 0"))
               (let* ([len (length ls)]
                      ;; inclusive
                      [s (let ([s (if (fx>= start 0) start (fx+ len start))])
                           (cond [(fx< s 0) 0]
                                 [(fx> s len) (fx1- len)]
                                 [else s]))]
                      ;; exclusive
                      [e (let ([e (if (fx>= end 0) end (fx+ len end))])
                           (cond [(fx<= e -1) -1]
                                 [(fx>= e len) len]
                                 [else e]))])
                 ;;(printf "~a: s: ~a, e: ~a, step: ~a~n" who s e step)
                 (if (fx= len 0)
                     '()
                     (cond [(and (fx< s e) (fx> step 0))
                            ;; get to index s first
                            (let ([lb (make-list-builder)])
                              (let loop ([i 0] [ls ls])
                                (if (fx= i s)
                                    (let loop ([s s] [ls ls])
                                      (lb (car ls))
                                      (let next ([s s] [ls ls] [k step])
                                        (cond [(or (fx>= s e) (null? ls)) (lb)]
                                              [(fx= k 0)      (loop s ls)]
                                              [else           (next (fx1+ s) (cdr ls) (fx1- k))])))
                                    (loop (fx1+ i) (cdr ls)))))]
                           [(and (fx> s e) (fx< step 0))
                            (let ([last (fx+ s (fx* step (fx1- (ceiling (/ (fx- s e) (fx- step))))))])
                              (let loop ([i 0] [ls ls])
                                (if (fx= i last)
                                    ;; build the result from last to first
                                    (let loop ([e i] [ls ls] [res (list (car ls))])
                                      (if (fx= s e)
                                          res
                                          (let next ([e e] [ls ls] [k step])
                                            (cond [(fx> e s) res]
                                                  [(fx= e s) (cons (car ls) res)]
                                                  [(fx= k 0) (loop e ls (cons (car ls) res))]
                                                  [else      (next (fx1+ e) (cdr ls) (fx1+ k))]))))
                                    (loop (fx1+ i) (cdr ls)))))]
                           [else '()]))))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   set operations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; By default use eqv? for comparison.
  ;; All take varargs.

  (define-syntax define-list-set-ops
    (lambda (stx)
      (syntax-case stx ()
        [(_ (p-p p-eqv p-eq p-equal) (name w p l l*) body)
         #'(begin
             (define name
               (lambda (w p l l*)
                 (pcheck ([procedure? p] [list? l] [all-lists? l*])
                         body)))
             (define-who p-p
               (lambda (pred ls . ls*)
                 (name who pred ls ls*)))
             (define-who p-eqv
               (lambda (ls . ls*)
                 (name who eqv? ls ls*)))
             (define-who p-eq
               (lambda (ls . ls*)
                 (name who eq? ls ls*)))
             (define-who p-equal
               (lambda (ls . ls*)
                 (name who equal? ls ls*))))])))

  (define check-subsets
    (lambda (ls* eq2)
      (let ([ls1* (snoc! (list-copy (cdr ls*)) #f)])
        ;; s1 s2 s3 s4 ->
        ;; ls*:  s1 s2 s3 s4
        ;; ls1*: s2 s3 s4 #f
        (andmap (lambda (s1 s2)
                  (if s2
                      (andmap (lambda (x) (memp (lambda (y) (eq2 x y)) s2))
                              s1)
                      #t))
                ls* ls1*))))

  #|doc
  Test whether given lists are equal sets.
  |#
  (define-list-set-ops (listp=? list=? listq=? listv=?)
    ($listp=? who eq2 ls ls*)
    (if (null? ls*)
        #t
        (let* ([ls*  (map (lambda (ls) (unique eq2 ls)) (cons ls ls*))]
               [len* (map length ls*)])
          (if (apply = len*)
              ;; If set1 and set2 have equal size, and set1 is subset of set2,
              ;; then set1 == set2.
              (check-subsets ls* eq2)
              #f))))

  #|doc
  Test whether given lists are proper subsets.
  |#
  (define-list-set-ops (listp<? list<? listq<? listv<?)
    ($listp<? who eq2 ls ls*)
    (if (null? ls*)
        #t
        (let* ([ls*  (map (lambda (ls) (unique eq2 ls)) (cons ls ls*))]
               [len* (map length ls*)])
          (if (apply < len*)
              (check-subsets ls* eq2)
              #f))))

  #|doc
  Test whether given lists are subsets.
  |#
  (define-list-set-ops (listp<=? list<=? listq<=? listv<=?)
    ($listp<=? who eq2 ls ls*)
    (if (null? ls*)
        #t
        (let* ([ls*  (map (lambda (ls) (unique eq2 ls)) (cons ls ls*))]
               [len* (map length ls*)])
          (if (apply <= len*)
              (check-subsets ls* eq2)
              #f))))

  #|doc
  Test whether given lists are proper supersets.
  |#
  (define-list-set-ops (listp>? list>? listq>? listv>?)
    ($listp>? who eq2 ls ls*)
    (if (null? ls*)
        #t
        (let* ([ls* (reverse
                     (map (lambda (ls) (unique eq2 ls)) (cons ls ls*)))]
               [len* (map length ls*)])
          (if (apply < len*)
              (check-subsets ls* eq2)
              #f))))

  #|doc
  Test whether given lists are supersets.
  |#
  (define-list-set-ops (listp>=? list>=? listq>=? listv>=?)
    ($listp>=? who eq2 ls ls*)
    (if (null? ls*)
        #t
        (let* ([ls* (reverse
                     (map (lambda (ls) (unique eq2 ls)) (cons ls ls*)))]
               [len* (map length ls*)])
          (if (apply <= len*)
              (check-subsets ls* eq2)
              #f))))

  #|doc
  Calculate the union of the list items.
  |#
  (define-list-set-ops (listp+ list+ listq+ listv+)
    ($listp+ who eq2 ls ls*)
    (if (null? ls*)
        ls
        (unique eq2 (apply append (cons ls ls*)))))

  #|doc
  Calculate the difference of the list items.
  If only `ls` is given, it is returned unchanged.
  No check of duplication is performed.
  |#
  (define-list-set-ops (listp- list- listq- listv-)
    ($listp- who eq2 ls ls*)
    (if (null? ls*)
        ls
        (let ([ls* (apply listp+ eq2 ls*)]
              [lb (make-list-builder)])
          (for-each (lambda (x) (when (not (memp (lambda (y) (eq2 x y)) ls*))
                                  (lb x)))
                    ls)
          (lb))))

  #|doc
  Calculate the intersection of the list items.
  |#
  (define-list-set-ops (listp& list& listq& listv&)
    ($listp& who eq2 ls ls*)
    (if (null? ls*)
        ls
        (let* ([lb (make-list-builder)]
               [ls* (cons ls ls*)]
               ;; union
               [u (unique eq2 (apply append ls*))])
          (for-each (lambda (x)
                      (when (andmap (lambda (ls) (memp (lambda (y) (eq2 x y)) ls))
                                    ls*)
                        (lb x)))
                    u)
          (lb))))

  #|doc
  Calculate the symmetric difference of the list items.
  |#
  (define-list-set-ops (listp^ list^ listq^ listv^)
    ($listp^ who eq2 ls ls*)
    ;; (- (+ ls ls*) (& ls*))
    (if (null? ls*)
        ls
        (let* ([lb (make-list-builder)]
               [ls* (cons ls ls*)]
               ;; union
               [u (unique eq2 (apply append ls*))])
          (for-each (lambda (x)
                      (unless (andmap (lambda (ls) (memp (lambda (y) (eq2 x y)) ls))
                                      ls*)
                        (lb x)))
                    u)
          (lb))))



  )
