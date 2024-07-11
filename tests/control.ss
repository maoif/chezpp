(import (chezpp control)
        (chezpp string)
        (chezpp utils))

(mat sect

     (eq? (let ([add2 (sect + 2 _)])
            (add2 2))
          4)

     (eq? (let ([add2 (sect + _ 2)])
            (add2 2))
          4)

     (eq? (let ([add2* (sect + _ 2 ...)])
            (add2* 2 2 2))
          8)

     (string-contains?
      (condition-message
       (guard (e [#t e])
         (expand '(sect + ... _))))
      "... must appear last")

     (string-contains?
      (condition-message
       (guard (e [#t e])
         (expand '(sect + _ ... _))))
      "... must appear last")

     )


(mat sect+

     (eq? (let* ([raise (sect+ expt _ _)]
                 [^2 (raise 2)])
            (^2 2))
          4)

     (eq? (let ([f (sect+ + _ _ ...)])
            (((f 1) 1) 1 1 1))
          5)

     (string-contains?
      (condition-message
       (guard (e [#t e])
         (expand '(sect+ + ... _))))
      "... must appear last")

     (string-contains?
      (condition-message
       (guard (e [#t e])
         (expand '(sect+ + _ ... _))))
      "... must appear last")

     )
