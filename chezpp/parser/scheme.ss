(library (chezpp parser scheme)
  (export parse-scheme-datum parse-scheme parse-scheme-file)
  (import (chezpp chez)
          (chezpp parser combinator)
          (chezpp internal)
          (chezpp list)
          (chezpp io)
          (chezpp vector)
          (chezpp utils))

  ;; TODO how to handle cycles?
  ;; no comments
  (define-values (parser-scheme-datum parser-scheme)
    (let ()
      (define cons-back!
        (lambda (ls v)
          (assert (not (null? ls)))
          (let loop ([ls1 ls])
            (if (null? (cdr ls1))
                (begin (set-cdr! ls1 v)
                       ls)
                (loop (cdr ls1))))))
      (define mk-improper-list
        (lambda (val)
          ;; val:  #\( ((datum ...) tail)
          ;; tail: #\) | datum
          (if (char? (list-ref val 2))
              (list-ref val 1)
              (cons-back! (list-ref val 1) (list-ref val 2)))))
      (define-who char->num
        (lambda (c)
          (if (char<=? #\0 c #\9)
              (fx- (char->integer c) 48)
              (errorf who "not a digit: ~a" c))))

      (define-parser <s-datum>)

      (define (<token> p) (<~ p (<many> <whitespace>)))
      (define (<fully> p) (<~n> 1 (<many> <whitespace>) p (<many> <whitespace>) <eof>))
      (define <nat>
        (<map> (lambda (val)
                 (fold-left (lambda (s n) (+ (* 10 s) (char->num n))) 0 val))
               (<some> <digit>)))

      (define <left-paren>    (<token> (<char> #\()))
      (define <right-paren>   (<token> (<char> #\))))
      (define <left-bracket>  (<token> (<char> #\[)))
      (define <right-bracket> (<token> (<char> #\])))
      (define <left-brace>    (<token> (<char> #\{)))
      (define <right-brace>   (<token> (<char> #\})))
      ;; TODO chez allows dot to precede none-digit
      (define <dot>           (<~> (<char> #\.) (<some> <whitespace>)))
      (define <left-vector-delim> (<token> (<string> "#(")))

      (define <quote>             (<string> "'"))
      (define <quasiquote>        (<string> "`"))
      (define <unquote>           (<string> ","))
      (define <unquote-splicing>  (<string> ",@"))
      (define <syntax>            (<string> "#'"))
      (define <quasisyntax>       (<string> "#`"))
      (define <unsyntax>          (<string> "#,"))
      (define <unsyntax-splicing> (<string> "#,@"))
      (define <abbrev-prefix>
        (<token> (</> (<as> 'quote <quote>)
                      (<as> 'quasiquote <quasiquote>)
                      ;; order matters
                      (<as> 'unquote-splicing <unquote-splicing>)
                      (<as> 'unquote <unquote>)
                      (<as> 'syntax <syntax>)
                      (<as> 'quasisyntax <quasisyntax>)
                      (<as> 'unsyntax-splicing <unsyntax-splicing>)
                      (<as> 'unsyntax <unsyntax>))))
      (define <s-abbrev> (<map> (lambda (val) (cons (car val) (list (cadr val))))
                                (<~> <abbrev-prefix> <s-datum>)))

      ;; simplified identifier parser
      (define <constituent> <letter>)
      (define <special-initial> (<one-of> "!$%&*/:<=>?^_~"))
      (define <special-subsequent> (<one-of> "+-.@"))
      (define <initial> (</> <constituent> <special-initial>))
      (define <subsequent> (</> <initial> <digit> <special-subsequent>))
      (define <peculiar-identifier>
        (</> (<map> (lambda (val)
                      (string-append (car val)
                                     ;; list of chars
                                     (apply string (cadr val))))
                    (<~> (<string> "->") (<many> <subsequent>)))
             ;; order matters: -> vs. -
             (<string> "+") (<string> "-") (<string> "...")))
      (define <identifier>
        (<map> string->symbol
               (</> (<map> (lambda (val)
                             (apply string (cons (car val) (cadr val))))
                           (<~> <initial> (<many> <subsequent>)))
                    <peculiar-identifier>)))
      (define <s-symbol> (<token> <identifier>))

      (define <s-bool> (<token> (</> (<as> #t (<string> "#t"))
                                     (<as> #t (<string> "#T"))
                                     (<as> #f (<string> "#f"))
                                     (<as> #f (<string> "#F")))))

      ;; no flonums, cflnums, exactness
      ;; TODO deforest
      (define <sign> (</> (<as> 1 (<char> #\+)) (<as> -1 (<char> #\-)) (<result> 1)))
      (define <s-decimal>
        (<map> (lambda (val) (* (car val) (string->number (cadr val) 10)))
               (</> (<~> <sign> (<as-string> (<some> <digit>)))
                    (<~n> 1
                          (</> (<string> "#d") (<string> "#D"))
                          (<~> <sign> (<as-string> (<some> <digit>)))))))
      (define <s-octal>
        (<map> (lambda (val) (* (car val) (string->number (cadr val) 8)))
               (<~n> 1
                     (</> (<string> "#o") (<string> "#O"))
                     (<~> <sign> (<as-string> (<some> <octdigit>))))))
      (define <s-hex>
        (<map> (lambda (val) (* (car val) (string->number (cadr val) 16)))
               (<~n> 1
                     (</> (<string> "#x") (<string> "#X"))
                     (<~> <sign> (<as-string> (<some> <hexdigit>))))))
      (define <s-bin>
        (<map> (lambda (val) (* (car val) (string->number (cadr val) 2)))
               (<~n> 1
                     (</> (<string> "#b") (<string> "#B"))
                     (<~> <sign> (<as-string> (<some> <bindigit>))))))
      (define <s-num> (<token> (</> <s-decimal> <s-octal> <s-hex> <s-bin>)))

      (define <s-special-char> (</> (<as> #\nul       (<string> "nul"))
                                    (<as> #\alarm     (<string> "alarm"))
                                    (<as> #\backspace (<string> "backspace"))
                                    (<as> #\tab       (<string> "tab"))
                                    (<as> #\linefeed  (<string> "linefeed"))
                                    (<as> #\newline   (<string> "newline"))
                                    (<as> #\vtab      (<string> "vtab"))
                                    (<as> #\page      (<string> "page"))
                                    (<as> #\return    (<string> "return"))
                                    (<as> #\esc       (<string> "esc"))
                                    (<as> #\space     (<string> "space"))
                                    ;; TODO refine
                                    (<as> #\space     (<string> " "))
                                    (<as> #\delete    (<string> "delete"))))
      (define <s-ordinary-char> <item>)
      ;; no hex char
      (define <s-char> (<token>
                        (~> (<string> "#\\") (</> <s-special-char> <s-ordinary-char>))))

      (define <escape-seq> (~> (<char> #\\) (</> (<as> #\" (<char> #\"))
                                                 (<as> #\\ (<char> #\\))
                                                 (<as> #\alarm     (<char> #\a))
                                                 (<as> #\backspace (<char> #\b))
                                                 (<as> #\linefeed  (<char> #\f))
                                                 (<as> #\newline   (<char> #\n))
                                                 (<as> #\return    (<char> #\r))
                                                 (<as> #\tab       (<char> #\t))
                                                 (<as> #\nul       (<char> #\0)))))
      ;; no inline hex escape
      (define <s-str-elements> (</> (<none-of> "\"\\") <escape-seq>))
      (define <s-string> (<token>
                          (</> (<as> "" (<string> "\"\""))
                               (<as-string>
                                (<~n> 1 (<char> #\") (<some> <s-str-elements>) (<char> #\"))))))

      (define <rest-paren>   (<~n> 1 <dot> <s-datum> <right-paren>))
      (define <rest-bracket> (<~n> 1 <dot> <s-datum> <right-bracket>))
      (define <s-list> (</> (<as> '() (<~> <left-paren>   (<many> <whitespace>) <right-paren>))
                            (<as> '() (<~> <left-bracket> (<many> <whitespace>) <right-bracket>))
                            (<map> mk-improper-list
                                   (<~> <left-paren> (<many> <s-datum>) (</> <right-paren>
                                                                             <rest-paren>)))
                            (<map> mk-improper-list
                                   (<~> <left-bracket> (<many> <s-datum>) (</> <right-bracket>
                                                                               <rest-bracket>)))))
      (define <s-vector>
        (</> (<as> '#() (<~> <left-vector-delim> (<many> <whitespace>) <right-paren>))
             (<map> list->vector
                    (<~n> 1 <left-vector-delim> (<many> <s-datum>) <right-paren>))))

      (define <s-lexeme-datum> (</> <s-bool> <s-char> <s-string> <s-num> <s-symbol>))
      (define <s-compound-datum> (</> <s-abbrev> <s-list> <s-vector>))
      (define s-datum-parser (</> <s-lexeme-datum> <s-compound-datum>))
      (define-parser <s-datum>
        (parser-call s-datum-parser inp state lvl))

      (values (<fully> <s-datum>) (<fully> (<many> <s-datum>)))))


  #|doc
  `str` must be a string representing a valid scheme datum.

  `parse-scheme-datum` tries to parse a single scheme datum represented as `str`.
  If successful, the parsed scheme datum is returned;
  otherwise, an error with condition type &parser-error is raised.
  |#
  (define parse-scheme-datum
    (lambda (str)
      (pcheck ([string? str])
              (run-textual-parser parser-scheme-datum str))))


  #|doc
  `str` must be a string representing one or more valid scheme values.

  `parse-scheme` tries to parse the scheme values represented as `str`.
  If successful, the list of parsed scheme values is returned;
  otherwise, an error with condition type &parser-error is raised.
  |#
  (define parse-scheme
    (lambda (str)
      (pcheck ([string? str])
              (run-textual-parser parser-scheme str))))


  #|doc
  `path` must be a path string that points to a valid scheme file.

  `parse-scheme-file` tries to parse the scheme file at `path`.
  If successful, the list of parsed scheme values is returned;
  otherwise, an error with condition type &parser-error is raised.
  |#
  (define parse-scheme-file
    (lambda (path)
      (pcheck ([string? path])
              (parse-textual-file parser-scheme path))))


  )
