(library (chezpp parser toml)
  (export parse-toml parse-toml-file)
  (import (chezpp chez)
          (chezpp parser combinator)
          (chezpp internal)
          (chezpp list)
          (chezpp io)
          (chezpp match)
          (chezpp utils))


  #|
  raw syntax:
  (<root-table>? (<table>*))
  <root-table> := <kv>+

  <table> := <std-table>
          |  <array-table>

  <std-table>   := ((std-table <table-name>) (<kv>*))
  <array-table> := ((array-table <table-name>) (<kv>*))

  <table-name> := <string> | (<string>+)
  |#

#;
  (((("tbls" "t1")
     inline-table
     ("x1" . 1)
     ("x2" . 2)
     ("x3" . 3))
    (("arrs" "a1") array 3.3 4.4 5.5)
    ("a" . 0.1)
    ("b" . 1)
    ("c" . ""))
   (((std-table "t1" "t2" "t3") (("a" . 666)))
    ((std-table "t1" "t2" "t3")
     (("b" . 666) ("c" . 3.14) (("d" "d" "d") . "dd")))
    ((array-table "aa" "bb")
     ((("a" "b") . 1)
      (("a" "b") . 2)
      (("arrs" "a1") array 3.3 4.4 5.5)))))


  (define-who make-toml
    (lambda (val)
      (let ([root-kvs (car val)] [tables/arrays (cadr val)]
            [root-ht (make-hashtable string-hash string=?)])
        ;; error if the final value is not a table
        (define get-table
          (lambda (ht0 k*)
            ;; k*: ("k0" "k1" ...)
            (fold-left (lambda (ht k)
                         (if (hashtable-contains? ht k)
                             (let ([v (hashtable-ref ht k #f)])
                               (if (hashtable? v)
                                   v
                                   (errorf who "~a is not a table" v)))
                             (let ([newht (make-hashtable string-hash string=?)])
                               (hashtable-set! ht k newht)
                               newht)))
                       ht0 k*)))
        (define table-defined?
          (lambda (ht0 k*)
            (when (fold-left (lambda (ht k)
                               (if ht
                                   (if (hashtable-contains? ht k)
                                       (let ([v (hashtable-ref ht k #f)])
                                         (if (hashtable? v)
                                             v
                                             (errorf who "~a is not a table" v)))
                                       #f)
                                   #f))
                             ht0 k*)
              (errorf who "table(s) ~a already defined" k*))))
        (define table-names (lambda (k*) (list-head k* (sub1 (length k*)))))
        (define key-name    (lambda (k*) (list-last k*)))
        (define process-v   (lambda (v)
                              (todo)))
        #;
        (define process-kvs (lambda (tbl kv*) (map (lambda (kv) (process-kv tbl kv)) kv*)))
        #;
        (define process-kv
          (lambda (kv)
            (match kv
              [(,k inline-table . ,kv*)
               (let ([tbl (get-table root-ht (table-names k))]
                     [key (key-name k)])
                 (hashtable-set! k (cons 'inline-table (process-kvs tbl kv*))))]
              [(,k array . ,v*)
               (let ([tbl (get-table root-ht (table-names k))]
                     [key (key-name k)])
                 ;;(hashtable-set! k (cons 'array (process-kvs tbl kv*)))
                 (todo)
                 )]
              [(,k . ,v)
               (let ([tbl (get-table root-ht (table-names k))]
                     [key (key-name k)])
                 (hashtable-set! k (process-v tbl v)))]
              [else (errorf who "unknown raw toml type: ~a" kv)])))
        (println val)
        #;
        (for-each (lambda (kv)
                    (match kv
                      [(,k inline-table . ,kv*)
                       (let ([tbl (get-table root-ht (table-names k))]
                             [key (key-name k)])
                         (hashtable-set! k (process-kvs tbl kv*)))]
                      [(,k array . ,v*)
                       (todo)]
                      [(,k . ,v)
                       (let ([tbl (get-table root-ht (table-names k))]
                             [key (key-name k)])
                         (hashtable-set! k (process-v v)))]
                      [else (errorf who "unknown raw toml type: ~a" kv)]))
                  root-kvs)
        (for-each (lambda (tbl/arr)
                    (match tbl/arr
                      [((std-table . ,k) . ,kv*)
                       (todo)]
                      [((array-table . ,k) . ,kv*)
                       (todo)]
                      [else (errorf who "unknown raw toml type: ~a" tbl/arr)]))
                  tables/arrays)
        ;; TODO to list
        )))


  ;; https://toml.io/en/v1.0.0
  ;; https://github.com/toml-lang/toml/blob/1.0.0/toml.abnf
  (define parser-toml
    (let ()
      (define hexdigits->char
        (lambda (d*)
          (integer->char (hexdigits->num d*))))
      (define-who char->num
        (lambda (c)
          (if (char<=? #\0 c #\9)
              (fx- (char->integer c) 48)
              (errorf who "not a digit: ~a" c))))
      (define mk-ml-basic
        (lambda (val)
          (apply string (fold-right (lambda (v res)
                                      (cond [(char? v) (cons v res)]
                                            [(eq? v 'ignore) res]
                                            [else (append v res)]))
                                    '() val))))
      (define mk-ml-literal
        (lambda (val)
          (apply string
                 (fold-right (lambda (v res)
                               (if (char? v) (cons v res) (append v res)))
                             '() val))))
      (define mk-float
        (lambda (val)
          (println val)
          (let* ([s (car val)] [int (cadr val)]
                 [rest (cdddr val)]
                 [d* (cons (car rest) (cadr rest))]
                 [e (caddr rest)])
            (inexact (* s (expt 10 e)
                        (+ int (fold-left (lambda (n d) (+ (/ d 10) (/ n 10))) 0 (reverse d*))))))))

      (define <comment> (<~> (<char> #\#)
                             (<many> (<satisfy-char> (lambda (c) (not (char=? c #\newline)))))))
      (define <junk> (<~> (<many> <whitespace>)
                          (<many> (<~> <comment> (<many> <whitespace>)))
                          (<many> <whitespace>)))

      ;; Since some TOML constructs cannot span multiple lines (e.g., inline table),
      ;; we need to have newline-aware and -none-aware tokens.
      (define (<token> p) (<~ p <junk>))
      (define (<fully> p) (<~1> <junk> p <junk> <eof>))
      (define <newline> (<char> #\newline))
      (define <wschar> (<one-of> " \t"))
      (define <ws> (<many> <wschar>))
      (define <underscore> (<char> #\_))
      ;; token no newline
      (define (<token-nn> p) (<~ p <ws>))

      (define-parser <val>)
      (define-parser <keyval>)

      (define <basic-unescaped> (</> <wschar>
                                     (<satisfy-char> (lambda (c)
                                                       (or (char=? c #\x21)
                                                           (char<=? #\x23 c #\x5b)
                                                           (char<=? #\x5d c #\x7e)
                                                           (char>? c #\x7f))))))
      (define <escaped> (<~> (<char> #\\)
                             (</> (~> (<char> #\u) (<map> hexdigits->char (<rep> <digit16> 4)))
                                  (~> (<char> #\U) (<map> hexdigits->char (<rep> <digit16> 8)))
                                  (<as> #\" (<char> #\"))
                                  (<as> #\\ (<char> #\\))
                                  (<as> #\backspace (<char> #\b))
                                  (<as> #\linefeed  (<char> #\f))
                                  (<as> #\newline   (<char> #\n))
                                  (<as> #\return    (<char> #\r))
                                  (<as> #\tab       (<char> #\t)))))
      (define <basic-char> (</> <basic-unescaped> <escaped>))
      (define <basic-string>
        (<~1> (<string> "\"") (<as-string> (<many> <basic-char>)) (<string> "\"")))

      ;; currently disallow things like """" in the end
      (define <mlb-quotes> (</> (<as> '(#\" #\") (<not-followed-by> (<string> "\"\"") (<string> "\"")))
                                (<not-followed-by> (<char> #\")  (<string> "\"\""))))
      (define <mlb-escaped-nl> (<as> 'ignore (<~> (<char> #\\) <ws> <newline>
                                                  (<many> (</> <wschar> <newline>)))))
      (define <mlb-content> (</> <basic-char> <newline> <mlb-escaped-nl> <mlb-quotes>))
      (define <ml-basic-body> (<many> <mlb-content>))
      (define <ml-basic-string>
        (<map> mk-ml-basic
               (<~2> (<string> "\"\"\"") (<optional> <newline>) <ml-basic-body> (<string> "\"\"\""))))

      (define <literal-char> (<satisfy-char> (lambda (c)
                                               (or (char=? c #\x09)
                                                   (char<=? #\x20 c #\x26)
                                                   (char<=? #\x28 c #\x7e)
                                                   (char>? c #\x7f)))))
      (define <literal-string>
        (<~1> (<string> "'") (<as-string> (<many> <literal-char>)) (<string> "'")))

      ;; currently disallow things like '''' in the end
      (define <mll-quotes> (</> (<as> '(#\' #\') (<not-followed-by> (<string> "''") (<string> "'")))
                                (<not-followed-by> (<char> #\')  (<string> "''"))))
      (define <mll-content> (</> <literal-char> <newline> <mll-quotes>))
      (define <ml-literal-body> (<many> <mll-content>))
      (define <ml-literal-string>
        (<map> mk-ml-literal
               (<~2> (<string> "'''") (<optional> <newline>) <ml-literal-body> (<string> "'''"))))

      (define <t-string>
        (</> <ml-basic-string> <basic-string> <ml-literal-string> <literal-string>))

      (define <date-full-year> (<map> digits->num (<rep> <digit10> 4)))
      (define <date-month>     (<map> digits->num (<rep> <digit10> 2)))
      (define <date-mday>      (<map> digits->num (<rep> <digit10> 2)))
      (define <time-hour>      (<map> digits->num (<rep> <digit10> 2)))
      (define <time-minute>    (<map> digits->num (<rep> <digit10> 2)))
      (define <time-second>    (<map> digits->num (<rep> <digit10> 2)))
      (define <time-secfrac>   (<map> digits->num (~> (<char> #\.) (<some> <digit10>))))
      (define <time-numoffset> (<map> (lambda (val)
                                        (let ([s (car val)] [hour (list-ref val 1)] [minute (list-ref val 3)])
                                          ;; convert to secs
                                          (* s (+ (* hour 60 60) (* minute 60)))))
                                      (<~> (</> (<as>  1 (<char> #\+))
                                                (<as> -1 (<char> #\-)))
                                           <time-hour> (<char> #\:) <time-minute>)))
      (define <time-offset>    (</> (<as> 0 (<char> #\Z)) <time-numoffset>))
      (define <time-delim>     (<one-of> "Tt "))

      (define <partial-time>
        (<map> (lambda (val) `(,(car val) ,(list-ref val 2) ,(list-ref val 4) ,(list-ref val 5)))
               (<~> <time-hour> (<char> #\:) <time-minute> (<char> #\:) <time-second>
                    (</> <time-secfrac> (<result> 0)))))
      (define <time>
        (<map> (lambda (val) `(,(car val) ,(list-ref val 2) ,@(list-tail val 4)))
               (<~> <time-hour> (<char> #\:) <time-minute> (<char> #\:) <time-second>
                    (</> <time-secfrac> (<result> 0))
                    (</> <time-offset>  (<result> 0)))))
      (define <full-date>
        (<map> (lambda (val) `(,(car val) ,(list-ref val 2) ,(list-ref val 4)))
               (<~> <date-full-year> (<char> #\-) <date-month> (<char> #\-) <date-mday>)))
      (define <date-time>
        (<map> (lambda (val)
                 (let* ([no-offset (list-head val 7)] [offset (list-ref val 7)]
                        [arg `(,@(reverse no-offset) ,offset)])
                   (println val)
                   (println arg)
                   (apply make-date arg)))
               (</> (<map> (lambda (val)
                             (println val)
                             ;; val: '((yyyy mm dd) (hh mm ss frac off))
                             `(,@(car val) ,@(cadr val)))
                           (<~> <full-date>
                                (</> (~> <time-delim> <time>)
                                     (<result> '(0 0 0 0 0)))))
                    (<map> (lambda (val) (println val)
                                   ;; no date given, default to today
                                   (let ([d (current-date)])
                                     `(,(date-year d) ,(date-month d) ,(date-day d) ,@val 0)))
                           <partial-time>))))

      (define <sign> (</> (<as> 1 (<char> #\+)) (<as> -1 (<char> #\-)) (<result> 1)))
      (define <_digit10> (</> <digit10> (~> <underscore> <digit10>)))
      (define <integer-part> (</> (<map> (lambda (val)
                                           (digits->num (cons (char->num (car val)) (cadr val))))
                                         (<~> (<one-of> "123456789") (<many> <_digit10>)))
                                  <digit10>))
      (define <dec-int> (<map> (lambda (val) (* (car val) (cadr val)))
                               (<~> <sign> <integer-part>)))
      ;; only non-negatives are allowed in other formats
      (define <hex-int> (~> (<string> "0x")
                            (<map> (lambda (val) (println val) (hexdigits->num (cons (car val) (cadr val))))
                                   (<~> <digit16> (<many> (</> <digit16> (~> <underscore> <digit16>)))))))
      (define <oct-int> (~> (<string> "0o")
                            (<map> (lambda (val) (println val) (octdigits->num (cons (car val) (cadr val))))
                                   (<~> <digit8> (<many> (</> <digit8> (~> <underscore> <digit8>)))))))
      (define <bin-int> (~> (<string> "0b")
                            (<map> (lambda (val) (println val) (bindigits->num (cons (car val) (cadr val))))
                                   (<~> <digit2> (<many> (</> <digit2> (~> <underscore> <digit2>)))))))
      (define <integer> (</> <hex-int> (<msg-f> "int1")
                             <oct-int> (<msg-f> "int2")
                             <bin-int> (<msg-f> "int3")
                             <dec-int> (<msg-f> "int4")))
      (define <inf> (<map> (lambda (val) (case val [1 +inf.0] [-1 -inf.0] [else (assert-unreachable)]))
                           (<~ <sign> (<string> "inf"))))
      (define <nan> (<as> +nan.0 (<~> (<optional> (<one-of> "+-")) (<string> "nan"))))
      (define <expt> (<map> (lambda (val)
                              (* (list-ref val 1) (digits->num (cons (list-ref val 2) (list-ref val 3)))))
                            (<~> (</> (<char> #\e) (<char> #\E))
                                 <sign> <digit10> (<many> <_digit10>))))
      (define <float> (</> (<map> mk-float
                                  (<~> <sign> <integer-part> (<char> #\.) <digit10> (<many> <_digit10>)
                                       (</> <expt> (<result> 0))))
                           <inf> <nan>))

      (define <boolean> (</> (<as> #t (<string> "true"))
                             (<as> #f (<string> "false"))))

      ;; no newline allowed
      (define <inline-table-sep> (<~1> <ws> (<char> #\,) <ws>))
      (define <keyval-sep>       (<~1> <ws> (<char> #\=) <ws>))
      (define <dot-sep>          (<~1> <ws> (<char> #\.) <ws>))

      ;; trailing comma NOT permitted
      (define <inline-table> (<~1> (<token-nn> (<char> #\{))
                                   (<map> (lambda (val) (cons 'inline-table val))
                                          (<sep-by> (<token-nn> <keyval>) <inline-table-sep>))
                                   (<token> (<char> #\}))))

      ;; trailing comma permitted
      (define <array-sep> (<token> (<char> #\,)))
      (define <array> (<~1> (<token> (<char> #\[))
                            (<map> (lambda (val) (cons 'array val))
                                   (<sep-by> (<token> <val>) <array-sep>))
                            (<token> (</> (<char> #\])
                                          (<~> <array-sep> (<char> #\]))))))

      (define <quoted-key> (</> <basic-string> <literal-string>))
      (define <unquoted-key> (<as-string> (<some> (</> <letter> <digit> (<char> #\-) (<char> #\_)))))
      (define-who <simple-key> (</> <quoted-key> (<msg-f> who "sp-key1") <unquoted-key> (<msg-f> who "sp-key2")))
      ;; this subsumes the simple-key case
      (define <dotted-key> (<sep-by1> <simple-key> <dot-sep>))
      (define <key> (<map> (lambda (val)
                             val
                             #;(if (= 1 (length val)) (car val) val)
                             )
                           <dotted-key>))
      (define val-parser
        (</> <t-string> <boolean> <array>
             <inline-table>  (<msg-f> '<val> "val1")
             <date-time>     (<msg-f> '<val> "val2")
             <float>         (<msg-f> '<val> "val3")
             <integer>       (<msg-f> '<val> "val4")))
      (define keyval-parser
        (<map> (lambda (val) (cons (car val) (caddr val)))
               (<~> <key> <keyval-sep> (<token> <val>))))
      (define-parser <val>
        (parser-call val-parser inp state lvl))
      (define-parser <keyval>
        (parser-call keyval-parser inp state lvl))
      (define <kvs> (<many> <keyval>))

      (define <array-table> (<~1> (<token-nn> (<string> "[["))
                                  (<map> (lambda (val) (cons 'array-table val)) <dotted-key>)
                                  (<token> (<string> "]]"))))
      (define <std-table> (<~1> (<token-nn> (<char> #\[))
                                (<map> (lambda (val) (cons 'std-table val)) <dotted-key>)
                                (<token> (<char> #\]))))
      (define <table> (</> <array-table> <std-table>))

      (define <toml> (<map> make-toml
                            (<fully> (<~> <kvs> (<many> (<~> <table> <kvs>))))))

      <toml>))


  #|doc
  `str` must be a string representing a valid toml document.

  `parse-toml` tries to parse the toml document represented as `str`.
  If successful, the parsed toml document is returned;
  otherwise, an error with condition type &parser-error is raised.
  |#
  (define parse-toml
    (lambda (str)
      (pcheck ([string? str])
              (run-textual-parser parser-toml str))))


  #|doc
  `path` must be a path string that points to a valid toml document file.

  `parse-toml-file` tries to parse the toml file at `path`.
  If successful, the parsed toml document is returned;
  otherwise, an error with condition type &parser-error is raised.
  |#
  (define parse-toml-file
    (lambda (path)
      (pcheck ([string? path])
              (parse-textual-file parser-toml path))))

  )
