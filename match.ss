#|

Adapted from Andy Keep's implementation:
https://github.com/akeep/scheme-to-llvm/blob/main/src/main/scheme/match.sls

|#


(library (eXtra match)
  (export match ;; mlambda mcase-lambda mlet mletrec mlet* mletrec*
          ;; match-record match-datatype
          )



  (import (chezscheme)
          ;;(eXtra internal)
          )


  ;; match interface
  (define-syntax match
    (lambda (stx)
      (define literal?
        (lambda (lit)
          (let ([p (syntax->datum lit)])
            (or (number? p)
                (boolean? p)
                (vector? p)
                (pair? p)
                (string? p)
                (char? p)))))
      (define process-pattern
        (lambda (expr-id pat body fk)
          (define extract-pat-vars
            (lambda (pat)
              (let loop ([pat pat] [patvars '()])
                (syntax-case pat (unquote)
                  [,var (identifier? #'var) (cons #'var patvars)]
                  [(pat0 . pat1) (loop #'pat0 (loop #'pat1 patvars))]
                  [_ patvars]))))
          ;;(printf "process-pattern ~s ~s ~s ~s~n" expr-id pat body fk)
          (with-syntax ([expr-id expr-id])
            (syntax-case pat (unquote)
              [,var
               (identifier? #'var)
               #`(let ([var expr-id]) #,body)]
              [()
               #`(if (null? expr-id) #,body (#,fk))]
              ;; ... in the end
              [(pat dots)
               (eq? (datum dots) '...)
               #`(if (pair? expr-id)
                     ;; Get all pat vars in `pat`,
                     ;; iterate through the input list, for each item,
                     ;; destructure it and accumulate matched components in `bindings ...`.
                     #,(with-syntax ([(patvars ...) (extract-pat-vars #'pat)])
                         (with-syntax ([(bindings ...) (generate-temporaries #'(patvars ...))]
                                       [(iter first rest) (generate-temporaries '(iter first rest))])
                           #`(let iter ([ls expr-id] [bindings '()] ...)
                               (cond
                                [(null? ls)
                                 (let ([patvars (reverse bindings)] ...)
                                   #,body)]
                                [(pair? ls)
                                 (let ([first (car ls)] [rest (cdr ls)])
                                   #,(process-pattern #'first #'pat
                                                      #'(iter rest (cons patvars bindings) ...)
                                                      fk))]
                                [else (#,fk)]))))
                     (#,fk))]
              ;; ... in the middle, prefer `pat1`
              [(pat0 dots . pat1)
               (eq? (datum dots) '...)
               (with-syntax ([(patvars ...) (extract-pat-vars #'pat0)])
                 (with-syntax ([(bindings ...) (generate-temporaries #'(patvars ...))]
                               [(iter ls first rest new-fk) (generate-temporaries '(iter ls first rest new-fk))])
                   #`(let iter ([ls expr-id] [bindings '()] ...)
                       (let ([new-fk (lambda ()
                                       ;; give up one item in the list to be matched against `pat0`
                                       (if (pair? ls)
                                           (let ([first (car ls)] [rest (cdr ls)])
                                             #,(process-pattern #'first #'pat0
                                                                #'(iter rest (cons patvars bindings) ...)
                                                                fk))
                                           (#,fk)))])
                         ;; Match the input directly with `pat1` first, if it matches,
                         ;; `pat0` or all pats vars inside `pat0` is '().
                         ;; If the input fails to match `pat1`, call `new-fk`,
                         #,(process-pattern #'ls #'pat1
                                            #`(let ([patvars (reverse bindings)] ...)
                                                #,body)
                                            #'new-fk)))))]
              ;; encapsulates (pat ...), (pat . pat), (pat ... . pat)
              [(pat0 . pat1)
               (with-syntax ([(first rest) (generate-temporaries '(first rest))])
                 #`(if (pair? expr-id)
                       (let ([first (car expr-id)] [rest (cdr expr-id)])
                         #,(process-pattern #'first #'pat0
                                            (process-pattern #'rest #'pat1 body fk)
                                            fk))
                       (#,fk)))]
              [#()
               #`(if (and (vector? expr-id) (fx= 0 (vector-length expr-id)))
                     #,body
                     (#,fk))]
              ;; `pats` has at least one pattern
              [#(pats ...)
               #`(if (vector? expr-id)
                     #,(with-syntax ([(vlen) (generate-temporaries '(vlen))])
                         #`(let ([vlen (vector-length expr-id)])
                             (if (fx= 0 vlen)
                                 (#,fk)
                                 #,(let loop ([veci 0] [pats #'(pats ...)])
                                     (with-syntax ([vi (datum->syntax #'expr-id veci)])
                                       (syntax-case pats ()
                                         ;; here we use list pattern for ease of looping
                                         [()
                                          #`(if (fx= vi vlen)
                                                #,body
                                                (#,fk))]
                                         [(pat dots)
                                          (eq? (datum dots) '...)
                                          #'not-impl]
                                         [(pat0 dots pat1)
                                          (eq? (datum dots) '...)
                                          #'not-impl]
                                         [(pat0 pat1 ...)
                                          (with-syntax ([(vitem) (generate-temporaries '(vitem))])
                                            #`(let ([vitem (vector-ref expr-id vi)])
                                                #,(process-pattern #'vitem #'pat0
                                                                  (loop (add1 veci) (cdr pats))
                                                                  fk)))]))))))
                     (#,fk))]
              [lit
               (literal? #'lit)
               #`(if (equal? lit expr-id) #,body (#,fk))]
              [under
               (equal? (datum under) '_)
               body]
              [sym
               (identifier? #'sym)
               #`(if (eq? 'sym expr-id) #,body (#,fk))]))))
      (define process-clause
        (lambda (expr-id cl fk)
          ;;(printf "process-clause ~s ~s ~s~n" expr-id cl fk)
          (syntax-case cl (guard)
            [(pat (guard guard-e* ...) e0 e* ...)
             (process-pattern expr-id #'pat
                              #`(if (and guard-e* ...)
                                    (begin e0 e* ...)
                                    ;; call fail continuation
                                    (#,fk))
                              fk)]
            [(pat e0 e* ...)
             (process-clause expr-id #'[pat (guard #t) e0 e* ...] fk)])))
      (define generate-skeleton
        (lambda (expr-id cl* fk)
          ;;(printf "generate-skeleton: ~s ~s ~s~n" expr-id cl* fk)
          (let loop ([cl* cl*])
            (if (null? cl*)
                fk
                (with-syntax ([(fk) (generate-temporaries '(fk))])
                  #`(let ([fk (lambda () #,(loop (cdr cl*)))])
                      #,(process-clause expr-id (car cl*) #'fk)))))))
      (syntax-case stx (else)
        [(k e cl0 cl* ... [else e0 e* ...])
         ;; `match-loop` used for catamorphism
         (begin (printf "1~n")
                #`(let match-loop ([v e])
                    #,(generate-skeleton #'v (cons #'cl0 #'(cl* ...)) #'(begin e0 e* ...))))]
        [(k e cl0 cl* ...)
         (begin (printf "2~n")
                #'(let ([v e])
                    (match v cl0 cl* ...
                           [else (errorf 'match "no match found for: " v)])))]
        [_ (syntax-error 'match "bad match form")])))


  )
