(library (chezpp internal)
  (export todo $construct-name flag->mask)
  (import (chezscheme))


  ;; todo imple as a macro? and use `who`
  (define todo
    (case-lambda
      [() (warningf #f "not impl")]
      [(x) (warningf x "not impl")]))

  (define $construct-name
    (lambda (template-identifier . args)
      (datum->syntax
       template-identifier
       (string->symbol
        (apply string-append
               (map (lambda (x)
                      (if (string? x)
                          x
                          (symbol->string (syntax->datum x))))
                    args))))))

  ;; for utils: define-flags
  (define flag->mask
    (lambda (m e)
      (cond
       [(fixnum? m) m]
       [(and (symbol? m) (assq m e)) => cdr]
       [(and (list? m) (eq? (car m) 'or))
        (let f ((ls (cdr m)))
          (if (null? ls)
              0
              (fxlogor (flag->mask (car ls) e) (f (cdr ls)))))]
       ;; TODO better error report
       [else (errorf 'flag->mask "invalid mask ~s" m)])))


  )
