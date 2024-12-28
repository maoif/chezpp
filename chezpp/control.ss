(library (chezpp control)
  (export forever
          compose compose1 >>> >>>1
          sect sect+
          let/cc let/1cc)
  (import (chezscheme)
          (chezpp utils)
          (chezpp internal)
          (chezpp list))


  (define-syntax forever
    (lambda (stx)
      (syntax-case stx ()
        [(k (index i) e e* ...)
         (and (eq? 'index (datum index)) (identifier? #'i))
         (with-implicit (k break)
           #'(call/1cc
              (lambda (break)
                (let loop ([i 0])
                  (begin e e* ...)
                  (loop (add1 i))))))]
        [(k e e* ...)
         (with-implicit (k break)
           #'(call/1cc
              (lambda (break)
                (let loop ()
                  (begin e e* ...)
                  (loop)))))])))


  #|doc
  Return the composed procedure P of given procedures p1, p2, p3, ..., such that
  (P x) = (p1 (p2 (p3 (... x))))

  Procedures are required to produce as many values as the preceding procedures consume.
  The number of return values is determined by the first procedure (i.e., the last procedure applied).
  |#
  (define-who compose
    (lambda (proc . proc*)
      (pcheck-proc (proc)
                   (if (null? proc*)
                       proc
                       (if (andmap procedure? proc*)
                           (let ([p* (cons proc proc*)])
                             (lambda args
                               (let loop ([p* p*])
                                 (if (null? p*)
                                     (apply values args)
                                     ;; recurse to the last proc first
                                     (call-with-values (lambda () (loop (cdr p*)))
                                       (car p*))))))
                           (errorf who "arguments contain non-procedure(s)"))))))


  #|doc
  Similar to `compose`, but only one value is passed among the procedures.
  The output arity of the first procedure and the input arity of
  the last procedure are unrestricted, though.

  This is generally faster than `compose`.
  |#
  (define-who compose1
    (lambda (proc . proc*)
      (pcheck-proc (proc)
                   (if (null? proc*)
                       proc
                       (if (andmap procedure? proc*)
                           (let* ([ps (cons proc proc*)]
                                  [p* (list-head ps (sub1 (length ps)))]
                                  [p1 (list-ref ps (sub1 (length ps)))])
                             (lambda args
                               (let loop ([p* p*])
                                 (if (null? p*)
                                     (apply p1 args)
                                     ((car p*) (loop (cdr p*)))))))
                           (errorf who "arguments contain non-procedure(s)"))))))


  #|doc
  Pipe the value `v` through the procedures, and return the value
  returned by the last procedure.

  The first procedure should be able to take one argument;
  Each procedure should consume as many values as its proceeding procedure returns.
  |#
  (define-who >>>
    (lambda (v proc . proc*)
      (pcheck-proc (proc)
                   (if (null? proc*)
                       (proc v)
                       (if (andmap procedure? proc*)
                           (let loop ([p* (reverse proc*)])
                             (if (null? p*)
                                 (proc v)
                                 (call-with-values (lambda () (loop (cdr p*)))
                                   (car p*))))
                           (errorf who "arguments contain non-procedure(s)"))))))


  #|doc
  Similar to `>>>`, with the difference that all but the last procedure should return
  one value only.

  This is generally faster than `>>>`.
  |#
  (define-who >>>1
    (lambda (v proc . proc*)
      (pcheck-proc (proc)
                   (if (null? proc*)
                       (proc v)
                       (if (andmap procedure? proc*)
                           (let loop ([p* proc*] [res (proc v)])
                             (if (null? (cdr p*))
                                 ((car p*) res)
                                 (loop (cdr p*) ((car p*) res))))
                           (errorf who "arguments contain non-procedure(s)"))))))


  (define-syntax $sect
    (lambda (stx)
      (define categorize-args
        (lambda (args ?curried)
          (let loop ([arg* (syntax->datum args)] [args args] [nslots 0] [rres '()] [dots? #f])
            (if (null? arg*)
                (values nslots (reverse rres) dots?)
                (let ([arg (car arg*)])
                  (cond
                   [(eq? arg '...)
                    (if (null? (cdr arg*))
                        (loop (cdr arg*) (cdr args) nslots (cons '(dots) rres) #t)
                        (syntax-error arg* (if ?curried "sect+" "sect") ": ... must appear last:"))]
                   [(eq? arg '_)
                    (loop (cdr arg*) (cdr args) (add1 nslots) (cons '(slot) rres) dots?)]
                   [else (loop (cdr arg*) (cdr args) nslots (cons (cons 'expr (car args)) rres) dots?)]))))))
      (syntax-case stx ()
        [(k ?curried proc e e* ...)
         ;; res*: ('expr . e) | '(slot) | '(dots)
         (let-values ([(nslots res* dots?) (categorize-args #'(e e* ...) (datum ?curried))])
           ;; x*: params for lambdas
           ;; p: bound to given procedure
           ;; varg*: bound to e*
           (with-syntax ([(x* ...) (generate-temporaries (iota nslots))]
                         [(p xdot) (generate-temporaries '(proc xdot))]
                         [(((varg*) e*) ...) (map
                                              (lambda (p) (list (generate-temporaries '(arg)) (cdr p)))
                                              (filter (lambda (x) (eq? 'expr (car x))) res*))])
             ;; generate the args that are eventually fed to the procedure
             ;; args: either given expressions bound in let (varg*) or formal parameters (x*)
             (with-syntax ([(args ...)
                            (let loop ([res* res*] [varg* #'(varg* ...)] [x* #'(x* ...)] [rres '()])
                              (if (null? res*)
                                  (reverse rres)
                                  (case (caar res*)
                                    [(slot) (loop (cdr res*) varg* (cdr x*) (cons (car x*) rres))]
                                    [(expr) (loop (cdr res*) (cdr varg*) x* (cons (car varg*) rres))]
                                    ;; don't care
                                    [(dots) (reverse rres)])))])
               (if (datum ?curried)
                   #`(let ([p proc] [varg* e*] ...)
                       #,(let gen-lambs ([x* #'(x* ...)])
                           (if (null? x*)
                               (if dots?
                                   #`(lambda xdot
                                       (apply p args ... xdot))
                                   #'(p args ...))
                               #`(lambda (#,(car x*))
                                   #,(gen-lambs (cdr x*))))))
                   #`(let ([p proc] [varg* e*] ...)
                       #,(if dots?
                             #'(lambda (x* ... . xdot)
                                 (apply p args ... xdot))
                             #'(lambda (x* ...)
                                 (p args ...))))))))])))

  (define-syntax sect
    (syntax-rules ()
      [(_ proc e e* ...) ($sect #f proc e e* ...)]))
  ;; curried section
  (define-syntax sect+
    (syntax-rules ()
      [(_ proc e e* ...) ($sect #t proc e e* ...)]))


  (define-syntax let/cc
    (lambda (stx)
      (syntax-case stx ()
        [(_ k e e* ...)
         (identifier? #'k)
         #'(call/cc (lambda (k) e e* ...))])))


  (define-syntax let/1cc
    (lambda (stx)
      (syntax-case stx ()
        [(_ k e e* ...)
         (identifier? #'k)
         #'(call/1cc (lambda (k) e e* ...))])))


  )
