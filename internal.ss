(library (chezpp internal)
  (export todo $construct-name)
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


  )
