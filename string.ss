(library (chezpp string)
  (export string-for-each/i string-startswith? string-endswith?)
  (import (chezscheme)
          (chezpp internal)
          (chezpp utils))

  (define check-length
    (lambda (who . strs)
      (unless (null? strs)
        (unless (apply fx= (map string-length strs))
          (errorf who "strings are not of the same length")))))


  #|
  For each character in strings.
  |#
  (define-syntax string-for-each/i
    (lambda (stx)
      (syntax-case stx ()
        [(k proc str str* ...)
         (with-syntax ([(p s s* ...) (generate-temporaries #'(proc str str* ...))])
           #`(let ([p proc] [s str] [s* str*] ...)
               (pcheck-proc (p)
                            (pcheck-string (s s* ...)
                                           (check-length 'k s s* ...)
                                           (let ([len (string-length s)] [strs (list s s* ...)])
                                             (let loop ([i 0])
                                               (unless (fx= i len)
                                                 (apply p i (map (lambda (s) (string-ref s i)) strs))
                                                 (loop (add1 i)))))))))])))


  (define string-split
    (lambda (str delim)
      (todo)))

  (define string-trim
    (case-lambda
      [(str) (todo)]
      [(str c) (todo)]))

  #|doc

  |#
  (define string-replace
    (lambda (str old-char new-char)
      (todo)))

  (define string-replace!
    (lambda (str old-char new-char)
      (todo)))

  #|doc
  Returns the index of the first occurrence of the target in str.
  Returns #f if none.
  |#
  (define string-search
    (lambda (str target)
      (todo)))

  #|
  Returns a list of indices for all occurrences of the target in str.
  Returns #f if none.
  |#
  (define string-search-all
    (lambda (str target)
      (todo)))

  ;; Knuth-Morris-Pratt or Boyer-Moore?
  (define string-contains?
    (lambda (str . s)
      (pcheck ([string? str])
              (if (null? s)
                  #t
                  (let ([ss (map (lambda (s)
                                   (pcase s
                                          [string? s]
                                          [char? (string s)]))
                                 s)]
                        [slen (string-length str)])
                    (andmap (lambda (s)
                              ;; add break in for-eaches?
                              (string-for-each/i
                               (lambda (i c)
                                 (todo))
                               str))
                            ss))))))

  (define string-startswith?
    (lambda (str prefix)
      (pcheck ([string? str])
              (let ([prefix (pcase prefix
                                   [string? prefix]
                                   [char? (string prefix)])])
                (if (equal? "" prefix)
                    #t
                    (let ([slen (string-length str)]
                          [preflen (string-length prefix)])
                      (cond
                       [(= preflen slen) (equal? str prefix)]
                       [(< preflen slen) (let loop ([i 0])
                                           (if (fx= i preflen)
                                               #t
                                               (and (char=? (string-ref str i) (string-ref prefix i))
                                                    (loop (add1 i)))))]
                       [else #f])))))))

  (define string-endswith?
    (lambda (str suffix)
      (pcheck ([string? str])
              (let ([suffix (pcase suffix
                                   [string? suffix]
                                   [char? (string suffix)])])
                (if (equal? "" suffix)
                    #t
                    (let ([slen (string-length str)]
                          [suflen (string-length suffix)])
                      (cond
                       [(= suflen slen) (equal? str suffix)]
                       [(< suflen slen) (let loop ([i (fx- slen suflen)] [j 0])
                                          (if (fx= i slen)
                                              #t
                                              (and (char=? (string-ref str i) (string-ref suffix j))
                                                   (loop (add1 i) (add1 j)))))]
                       [else #f])))))))


  #|doc
  Returns the minimum number of operations (add, remove, replace) required to
  transform `s1` to `s2`.
  |#
  (define edit-distance
    (lambda (s1 s2)
      (pcheck ([string? s1 s2])
              (todo))))

  )
