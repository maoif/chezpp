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
          ;;(printf "process-pattern ~s ~s ~s ~s~n" expr-id pat body fk)
          (with-syntax ([expr-id expr-id])
            (syntax-case pat (unquote)
              [,var
               (identifier? #'var)
               #`(let ([var expr-id]) #,body)]
              [()
               #`(if (null? expr-id) #,body (#,fk))]
              [(pat dots)
               (eq? (datum dots) '...)
               #'not-impl]
              [(pat0 dots . pat1)
               (eq? (datum dots) '...)
               #'not-impl]
              ;; encapsulates (pat ...), (pat . pat), (pat ... . pat)
              [(pat0 . pat1)
               (with-syntax ([(first rest) (generate-temporaries '(first rest))])
                 #`(if (pair? expr-id)
                       (let ([first (car expr-id)] [rest (cdr expr-id)])
                         #,(process-pattern #'first #'pat0
                                            (process-pattern #'rest #'pat1 body fk)
                                            fk))
                       (#,fk)))]
              [lit
               (literal? #'lit)
               #`(if (equal? lit expr-id) #,body (#,fk))]
              [under
               (equal? (datum under) '_)
               body]))))
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
         (begin (printf "1~n")
                #'(let ([v e])
                    (match v cl0 cl* ...
                           [else (errorf 'match "no match found for: " v)])))]
        [_ (syntax-error 'match "bad match form")])))


  )
