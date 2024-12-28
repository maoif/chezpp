(library (chezpp string)
  (export string-for-each/i string-startswith? string-endswith?
          string-search string-search-all string-contains?
          string-split string-trim string-trim-left string-trim-right)
  (import (chezscheme)
          (chezpp internal)
          (chezpp utils)
          (chezpp list))

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

  #|doc
  Split a string into a list of substrings, using `delim` as delimiter.
  `delim` can be either a character or a non-empty string.
  |#
  (define-who string-split
    (lambda (str delim)
      (pcheck-string
       (str)
       (cond [(equal? str "") '("")]
             [(string? delim)
              (let ([dlen (string-length delim)] [len (string-length str)])
                (case dlen
                  [0 (errorf who "empty delimiter")]
                  [1 (string-split str (string-ref delim 0))]
                  [else (let ([i* (string-search-all str delim)])
                          (if i*
                              (let loop ([i* i*] [res '()] [lefti 0])
                                (if (null? i*)
                                    (if (fx= lefti len)
                                        ;; align with delim being char case:
                                        ;; add empty string when `delim` is on the side
                                        (reverse (cons "" res))
                                        (reverse (cons (substring str lefti len) res)))
                                    (let ([i (car i*)])
                                      (loop (cdr i*) (cons (substring str lefti i) res) (fx+ i dlen)))))
                              (list str)))]))]
             [(char? delim)
              (let ([lb (make-list-builder)] [len (string-length str)])
                (let loop-next ([leftcur 0])
                  (let loop ([i leftcur])
                    (if (fx= i len)
                        (begin (lb (substring str leftcur i))
                               (lb))
                        (if (char=? (string-ref str i) delim)
                            (begin (lb (substring str leftcur i))
                                   (loop-next (add1 i)))
                            (loop (add1 i)))))))]
             [else (errorf who "invalid delimiter: ~a" delim)]))))


  (define $string-trim
    (lambda (who str c left? right?)
      (pcheck ([string? str] [char? c])
              (let* ([len (string-length str)]
                     [lefti (if left?
                                (let lp ([i 0])
                                  ;; in case `str` consists entirely of `c`
                                  (if (fx< i len)
                                      (if (char=? c (string-ref str i))
                                          (lp (add1 i))
                                          i)
                                      len))
                                0)]
                     [righti (if right?
                                 (let lp ([i (sub1 len)])
                                   ;; ditto
                                   (if (fx>= i 0)
                                       (if (char=? c (string-ref str i))
                                           (lp (sub1 i))
                                           (add1 i))
                                       0))
                                 len)])
                (if (fx<= lefti righti)
                    (substring str lefti righti)
                    "")))))


  #|doc
  Trim the characters on both sides of the given `str`.
  If the character `c` to be trimmed is not given, it is #\space by default.
  |#
  (define-who string-trim
    (case-lambda
      [(str) (string-trim str #\space)]
      [(str c) ($string-trim who str c #t #t)]))


  #|doc
  Similar to `string-trim`, but only trim the left-hand side.
  |#
  (define-who string-trim-left
    (case-lambda
      [(str) (string-trim-left str #\space)]
      [(str c) ($string-trim who str c #t #f)]))


  #|doc
  Similar to `string-trim`, but only trim the right-hand side.
  |#
  (define-who string-trim-right
    (case-lambda
      [(str) (string-trim-right str #\space)]
      [(str c) ($string-trim who str c #f #t)]))


  ;; currently use brute force
  ;; TODO: Knuth-Morris-Pratt or Boyer-Moore?
  ;; Search for `target` from position `start` in `str`,
  ;; return the index if there's a match, or #f.
  ;; No error checking here.
  (define $string-search
    (lambda (str target start)
      (let ([slen (string-length str)] [tlen (string-length target)])
        (define str=?
          (lambda (i)
            (let loop ([i i] [j 0])
              (or (fx= j tlen)
                  (and (char=? (string-ref str i) (string-ref target j))
                       (loop (add1 i) (add1 j)))))))
        (cond [(fx> (fx+ start tlen) slen) #f]
              [(fx= (fx+ start tlen) slen) (and (str=? start) start)]
              [else (let ([end (fx- slen tlen)])
                      (let loop ([i start])
                        (if (fx> i end)
                            #f
                            (if (str=? i)
                                i
                                (loop (add1 i))))))]))))


  #|doc
  Returns the index of the first occurrence of the target in str.
  Returns #f if none.
  |#
  (define string-search
    (lambda (str target)
      (pcheck ([string? str])
              (let ([target (pcase target
                                   [string? target]
                                   [char? (string target)])])
                ($string-search str target 0)))))

  #|
  Returns a list of indices for all occurrences of the target in str.
  Returns #f if none.
  |#
  (define string-search-all
    (lambda (str target)
      (pcheck ([string? str])
              (let* ([target (pcase target
                                    [string? target]
                                    [char? (string target)])]
                     [end (- (string-length str)
                             (string-length target))])
                (cond [(string=? "" target) '(0)]
                      [(< end 0) #f]
                      [(= end 0) (and (string=? str target) '(0))]
                      [else (let loop ([i 0] [res '()])
                              (if (< end i)
                                  (and (not (null? res)) (reverse res))
                                  (let ([j ($string-search str target i)])
                                    (if j
                                        (loop (add1 j) (cons j res))
                                        (and (not (null? res)) (reverse res))))))])))))

  #|doc
  Checks whether `str` contains the list of strings in `s`.
  |#
  (define string-contains?
    (lambda (str . s)
      (pcheck ([string? str])
              (if (null? s)
                  #t
                  (let* ([ss (map (lambda (s)
                                    (pcase s
                                           [string? s]
                                           [char? (string s)]))
                                  s)]
                         [slen (string-length str)]
                         [contains1? (lambda (s)
                                       (let ([patlen (string-length s)])
                                         (cond
                                          [(> patlen slen) #f]
                                          [(= patlen slen) (string=? s str)]
                                          [else (and ($string-search str s 0) #t)])))])
                    (andmap contains1? ss))))))

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
