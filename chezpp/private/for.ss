(library (chezpp private for)
  (export literal? ->literal
          valid-iter-ty? kw-iter-clause?
          kw-for-config-clause? kw-for*-config-clause?
          kw:init?
          kw:index?
          kw:finish?
          kw:iter?
          kw:break?
          kw:stop?
          kw:guard?
          kw:bind?
          kw:let?
          kw:let-values?

          process-iter-clause process-iter-clauses)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp internal)
          (chezpp io))


  (trace-define (literal? lit)
    (let* ([x (syntax->datum lit)]
           [x (if (and (pair? x) (= 2 (length x)) (eq? 'quote (car x)))
                  (cadr x)
                  x)])
      (or (natural? x)
          (string? x)
          (vector? x))))

  (define (->literal lit)
    (let ([x (syntax->datum lit)])
      (if (and (pair? x) (= 2 (length x)) (eq? 'quote (car x)))
          (cadr x)
          x)))

  (define (valid-iter-ty? ty)
    (let ([ty (syntax->datum ty)])
      (or (eq? ty ':iota)
          (eq? ty ':nums)
          (eq? ty ':fxnums)
          (eq? ty ':string)
          (eq? ty ':list)
          (eq? ty ':vector)
          (eq? ty ':hashtable))))

  (define kw-iter-clause? valid-iter-ty?)

  (define (kw-for-config-clause? kw)
    (let ([kw (syntax->datum kw)])
      (or (eq? kw ':break)
          (eq? kw ':guard)
          (eq? kw ':let)
          (eq? kw ':let*)
          (eq? kw ':letrec)
          (eq? kw ':let-values))))
  (define (kw-for*-config-clause? kw)
    (let ([kw (syntax->datum kw)])
      (or (eq? kw ':break)
          (eq? kw ':guard)
          (eq? kw ':stop)
          (eq? kw ':let)
          (eq? kw ':let*)
          (eq? kw ':letrec)
          (eq? kw ':let-values))))

  (define (gen-kw? kw)
    (lambda (keyword) (eq? kw (syntax->datum keyword))))
  (define kw:init?       (gen-kw? ':init))
  (define kw:index?      (gen-kw? ':index))
  (define kw:finish?     (gen-kw? ':finish))
  (define kw:iter?       (gen-kw? ':iter))
  (define kw:break?      (gen-kw? ':break))
  (define kw:stop?       (gen-kw? ':stop))
  (define kw:guard?      (gen-kw? ':guard))
  (define kw:bind?       (gen-kw? ':bind))
  (define kw:let?        (gen-kw? ':let))
  (define kw:let-values? (gen-kw? ':let-values))


  #|
  For each iter clause, the following syntax/procedures are returned:
  - gen-preloop-proc (gen or not gen?): generate procs that outputs code that binds various variables needed in the loop
  - loop-var: induction variables that correspond to each loop variable
  - type-check: generate code that checks whether the loop variable has the right type
  - terminate: termination checks for the loop variable
  - next: code that advances the loop variable
  - gen-getter: generate code that binds the value to be used in the body
  |#
  (define (process-iter-clause cl)
    (trace-define (process-multi-var-iter-clause v* ty op*)
      (todo 'process-multi-var-iter-clause))
    (trace-define (process-single-var-iter-clause v ty op*)
      (let ([nops (length op*)])
        (case ty
          [:iota
           (unless (= 1 nops)
             (syntax-error op* "invalid number of options for iter type `:iota`:"))
           (with-syntax ([(t-stop) (generate-temporaries '(t-stop))])
             (list (lambda (e)
                     #`(let ([t-stop #,(car op*)])
                         (unless (natural? t-stop)
                           (errorf ':iota "not a natural number: ~a" t-stop))
                         #,e))
                   #`[#,v 0]
                   #`(natural? t-stop)
                   #`(fx= #,v t-stop)
                   #`(fx1+ #,v)
                   #f))]
          [:nums
           (when (not (<= 1 nops 3))
             (syntax-error op* "invalid number of options for iter type `:nums`:"))
           (with-syntax ([(t-start t-stop t-step) (generate-temporaries '(t-start t-stop t-step))])
             (case nops
               [1 (list (lambda (e)
                          #`(let ([t-stop #,(car op*)])
                              (unless (and (number? t-stop) (nonnegative? t-stop))
                                (errorf ':nums "not a nonnegative number: ~a" t-stop))
                              #,e))
                        #`[#,v 0]
                        #`(and (number? t-stop) (nonnegative? t-stop))
                        #`(= #,v t-stop)
                        #`(+ #,v 1)
                        #f)]
               [2 (list (lambda (e)
                          #`(let ([t-start #,(car op*)] [t-stop #,(cadr op*)])
                              (unless (and (number? t-start) (number? t-stop) (<= t-start t-stop))
                                (errorf ':nums "invalid range from ~a to ~a" t-start t-stop))
                              #,e))
                        #`[#,v t-start]
                        #`(and (number? t-start) (number? t-stop) (<= t-start t-stop))
                        #`(>= #,v t-stop)
                        #`(+ #,v 1)
                        #f)]
               [3 (list (lambda (e)
                          #`(let ([t-start #,(car op*)] [t-stop #,(cadr op*)] [t-step #,(caddr op*)])
                              (unless (and (number? t-start) (number? t-stop)
                                           (or (and (<= t-start t-stop) (> t-step 0))
                                               (and (> t-start t-stop)  (< t-step 0))))
                                (errorf ':nums "invalid range from ~a to ~a, step ~a" t-start t-stop t-step))
                              #,e))
                        #`[#,v t-start]
                        #`(and (number? t-start) (number? t-stop)
                               (or (and (<= t-start t-stop) (> t-step 0)))
                               (or (and (> t-start t-stop)  (< t-step 0))))
                        #`(if (<= t-start t-stop)
                              (>= #,v t-stop)
                              (<= #,v t-stop))
                        #`(+ #,v t-step)
                        #f)]))]
          [:list
           (when (< nops 1)
             (syntax-error op* "invalid number of options for iter type `:list`:"))
           (with-syntax ([(t-ls) (generate-temporaries '(t-ls))])
             (list (lambda (e)
                     #`(let ([t-ls #,(car op*)])
                         (unless (list? t-ls)
                           (errorf ':list "not a list: ~a" t-ls))
                         #,e))
                   #`[t-ls t-ls]
                   #`(list? t-ls)
                   #`(null? t-ls)
                   #`(cdr t-ls)
                   (lambda (e)
                     #`(let ([#,v (car t-ls)])
                         #,e))))]
          [:vector
           (when (< nops 1)
             (syntax-error op* "invalid number of options for iter type `:vector`:"))
           (with-syntax ([(t-vec t-len t-i) (generate-temporaries '(t-vec t-len t-i))])
             (list (lambda (e)
                     #`(let ([t-vec #,(car op*)]) ; it seems tycheck can be put here
                         (unless (vector? t-vec)
                           (errorf ':vector "not a vector: ~a" t-vec))
                         (let ([t-len (vector-length t-vec)])
                           #,e)))
                   #`[t-i 0]
                   #`(vector? t-vec)
                   #`(fx= t-i t-len)
                   #`(fx1+ t-i)
                   (lambda (e)
                     #`(let ([#,v (vector-ref t-vec t-i)])
                         #,e))))]
          [else (syntax-error ty "bad iter type:")])))
    (trace-define (process-literal-iter-clause v lit)
      (let ([lit (->literal lit)])
        (cond [(natural? lit)
               (list #f
                     #`[#,v 0]
                     #'#t
                     #`(fx= #,v #,lit)
                     #`(fx1+ #,v)
                     #f)]
              [(string? lit)
               (with-syntax ([(t-str t-i) (generate-temporaries '(t-str t-i))])
                 (list (lambda (e)
                         #`(let ([t-str #,lit])
                             #,e))
                       #'[t-i 0]
                       #'#t
                       #'(fx= t-i (string-length t-str))
                       #'(fx1+ t-i)
                       (lambda (e)
                         #`(let ([#,v (string-ref t-str t-i)])
                             #,e))))]
              [(vector? lit)
               (with-syntax ([(t-vec t-i) (generate-temporaries '(t-vec t-i))])
                 (list (lambda (e)
                         #`(let ([t-vec '#,(datum->syntax v lit)])
                             #,e))
                       #'[t-i 0]
                       #'#t
                       #'(fx= t-i (vector-length t-vec))
                       #'(fx1+ t-i)
                       (lambda (e)
                         #`(let ([#,v (vector-ref t-vec t-i)])
                             #,e))))]
              [else (syntax-error lit "bad iter literal type:")])))
    (syntax-case cl ()
      ;; no guards here, already checked
      [(v lit)
       (process-literal-iter-clause #'v (datum lit))]
      [((v* ...) iter-ty op* ...)
       (process-multi-var-iter-clause #'(v* ...) (datum iter-ty) #'(op* ...))]
      [(v iter-ty op* ...)
       (process-single-var-iter-clause #'v (datum iter-ty) #'(op* ...))]
      [_ (syntax-error cl "unknown iter clause:")]))


  (trace-define (process-iter-clauses cl*)
    (for-each println cl*)
    (map process-iter-clause cl*))


  )
