(library (chezpp vector)
  (export vmemp vmember vmemq vmemv)
  (import (chezscheme)
          (chezpp utils)
          (chezpp internal))

  (define check-length
    (lambda (who . vecs)
      (unless (null? vecs)
        (unless (apply fx= (map vector-length vecs))
          (errorf who "strings are not of the same length")))))

  #|doc
  Map vectors with index.
  |#
  (define vector-map/i
    (lambda (proc vec . vecs)
      (todo)))

  #|doc
  Iterate through vectors with index.
  |#
  (define vector-for-each/i
    (lambda (proc vec . vecs)
      (todo)))

  (define vormap
    (lambda (proc vec)
      (todo)))

  (define vandmap
    (lambda (proc vec)
      (todo)))

  (define vexists  vormap)
  (define vfor-all vandmap)

  #|doc
  Return the index of the first vector item such that (proc item) => #t,
  otherwise return #f.
  |#
  (define vmemp
    (lambda (proc vec)
      (pcheck ([procedure? proc] [vector? vec])
              (let ([len (vector-length vec)])
                (let loop ([i 0])
                  (if (fx= i len)
                      #f
                      (if (proc (vector-ref vec i))
                          i
                          (loop (add1 i)))))))))
  (define vmember
    (lambda (obj vec)
      (vmemp (lambda (x) (equal? x obj)) vec)))
  (define vmemq
    (lambda (obj vec)
      (vmemp (lambda (x) (eq? x obj)) vec)))
  (define vmemv
    (lambda (obj vec)
      (vmemp (lambda (x) (eqv? x obj)) vec)))

  ;; binary search for sorted vectors
  (define vbmemp
    (lambda (proc vec)
      (todo)))
  (define vbmember
    (lambda (obj vec)
      (todo)))
  (define vbmemq
    (lambda (obj vec)
      (todo)))
  (define vbmemv
    (lambda (obj vec)
      (todo)))


  ;; vector comprehension
  )
