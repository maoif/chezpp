(library (chezpp control)
  (export forever
          compose >>>
          sect sect+)
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

  (define compose
    (lambda (proc . proc*)
      (pcheck-proc (proc)
                   (if (null? proc*)
                       proc
                       (if (andmap procedure? proc*)
                           (todo)
                           (errorf 'compose "arguments contain non-procedure(s)"))))))

  ;; pipeline
  (define >>>
    (lambda (arg proc . proc*)
      ((apply compose proc proc*) (list arg))))


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


  )
