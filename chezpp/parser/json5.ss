(library (chezpp parser json5)
  (export parse-json5 parse-json5-file)
  (import (chezpp chez)
          (chezpp parser combinator)
          (chezpp internal)
          (chezpp list)
          (chezpp io)
          (chezpp vector)
          (chezpp utils))

  ;; https://spec.json5.org/
  ;; no multi-line string,
  ;; no identifier as key
  (define parser-json5
    (let ()
      (define decimal-digits->num
        (lambda (w* d*)
          (+ (digits->num w*)
             (fold-left (lambda (n d) (+ (/ d 10) (/ n 10))) 0 (reverse d*)))))
      (define hexdigits->char
        (lambda (d*)
          (integer->char (hexdigits->num d*))))

      (declare-lazy-parser <j-value>)

      (define <line-comment> (<~> (<string> "//")
                                  (<many> (<satisfy-char> (lambda (c) (not (char=? c #\newline)))))))
      (define <block-comment> (</> (<~> (<string> "/**/"))
                                   (<~> (<string> "/*")
                                        (<many> (<not-followed-by> <item> (<string> "*/")))
                                        ;; the last char before "*/"
                                        <item>
                                        (<string> "*/"))))
      (define <comment> (</> <line-comment> <block-comment>))
      (define <junk>    (<~> (<many> <whitespace>)
                             (<many> (<~> <comment> (<many> <whitespace>)))
                             (<many> <whitespace>)))
      (define (<token> p) (<~ p <junk>))
      (define (<fully> p) (<~1> <junk> p <junk> <eof>))

      (define <trailing-comma> (<token> (<char> #\,)))
      (define <left-bracket>   (<token> (<char> #\[)))
      (define <right-bracket>  (<token> (<char> #\])))
      (define <left-brace>     (<token> (<char> #\{)))
      (define <right-brace>    (<token> (<char> #\})))

      ;; TODO not-followed-by for literals
      (define <j-null> (<token> (<as> '() (<string> "null"))))
      (define <j-bool>
        (<token> (</> (<as> #t (<string> "true"))
                      (<as> #f (<string> "false")))))

      (define <escape-seq> (~> (<char> #\\)
                               (</> (~> (<char> #\x) (<map> hexdigits->char (<rep> <digit16> 2)))
                                    (~> (<char> #\u) (<map> hexdigits->char (<rep> <digit16> 4)))
                                    (<as> #\' (<char> #\'))
                                    (<as> #\" (<char> #\"))
                                    (<as> #\\ (<char> #\\))
                                    (<as> #\backspace (<char> #\b))
                                    (<as> #\linefeed  (<char> #\f))
                                    (<as> #\newline   (<char> #\n))
                                    (<as> #\return    (<char> #\r))
                                    (<as> #\tab       (<char> #\t))
                                    (<as> #\nul       (<char> #\0)))))
      ;; TODO line continuation
      (define <j-double-string-char> (</> (<none-of> "\"\\\r\n") <escape-seq>))
      (define <j-single-string-char> (</> (<none-of> "'\\\r\n")  <escape-seq>))
      (define <j-string>
        (<token> (</> (<as> "" (<string> "\"\""))
                      (<as> "" (<string> "''"))
                      (<as-string> (<~1> (<char> #\")
                                         (<some> <j-double-string-char>)
                                         (<char> #\")))
                      (<as-string> (<~1> (<char> #\')
                                         (<some> <j-single-string-char>)
                                         (<char> #\'))))))
      ;; TODO deforest
      (define <sign> (</> (<as> 1 (<char> #\+)) (<as> -1 (<char> #\-)) (<result> 1)))
      (define <expt> (<map> (lambda (val) (* (cadr val) (digits->num (caddr val))))
                            (<~> (</> (<char> #\e) (<char> #\E))
                                 <sign>
                                 (<some> <digit10>))))
      ;; [sign] whole . decimal [exponent]
      ;; There must be at least one of whole and decimal.
      (define <dec> (<map> (lambda (val)
                             (let* ([e (let ([e (caddr val)])
                                         (if (null? e) 1 (expt 10 e)))]
                                    [n (* (car val) (cadr val) e)])
                               (if (integer? n) n (inexact n))))
                           (<~> <sign>
                                (</> (<map> (lambda (val)
                                              (decimal-digits->num (car val) (caddr val)))
                                            (<~> (<many> <digit10>)
                                                 (<char> #\.)
                                                 (<some> <digit10>)))
                                     (<map> (lambda (val)
                                              (decimal-digits->num (car val) (caddr val)))
                                            (<~> (<some> <digit10>)
                                                 (<char> #\.)
                                                 (<many> <digit10>)))
                                     (<map> digits->num (<some> <digit10>)))
                                (<optional> <expt>))))
      ;; TODO lift <sign>
      (define <hex> (<map> (lambda (val) (* (car val) (caddr val)))
                           (<~> <sign>
                                (</> (<string> "0x") (<string> "0X"))
                                (<map> hexdigits->num (<many> <digit16>)))))
      (define <inf> (<map> (lambda (val) (case val [1 +inf.0] [-1 -inf.0] [else (assert-unreachable)]))
                           (<~ <sign> (<string> "Infinity"))))
      (define <nan> (<as> +nan.0 (<~> (<optional> (<one-of> "+-")) (<string> "NaN"))))
      (define <j-number>     (<token> (</> <hex> <dec> <inf> <nan>)))
      ;; TODO as symbol?
      (define <j-identifier> (<token> <j-string>))
      (define <j-key>        (<token> (</> <j-identifier> <j-string>)))
      ;; TODO optimize
      (define <j-object>
        (</> (<as> '(jobject) (<~1> <left-brace> (<many> <whitespace>) <right-brace>))
             (<map> (lambda (val) (cons 'jobject val))
                    (<~1> <left-brace>
                          (<map> (lambda (val)
                                   ;; val: ((k #\: v) ...)
                                   (map (lambda (v)
                                          (cons (car v) (caddr v)))
                                        val))
                                 (<sep-by1> (<~> <j-key> (<token> (<char> #\:)) <j-value>)
                                            <trailing-comma>))
                          (</> <right-brace>
                               (<~> <trailing-comma> <right-brace>))))))
      ;; TODO optimize
      (define <j-array>
        (</> (<as> '(jarray) (<~1> <left-bracket> (<many> <whitespace>) <right-bracket>))
             (<map> (lambda (val) (cons 'jarray val))
                    (<~1> <left-bracket>
                          (<sep-by1> <j-value> <trailing-comma>)
                          (</> <right-bracket>
                               (<~> <trailing-comma> <right-bracket>))))))

      (define <json5> (<fully> <j-value>))
      (define-lazy-parser <j-value> (</> <j-object>
                                         <j-array>
                                         <j-null>
                                         <j-bool>
                                         <j-string>
                                         <j-number>))

      <json5>))


  #|doc
  `str` must be a string representing a valid json5 value.

  `parse-json5` tries to parse the json5 value represented as `str`.
  If successful, the parsed json5 value is returned;
  otherwise, an error with condition type &parser-error is raised.
  |#
  (define parse-json5
    (lambda (str)
      (pcheck ([string? str])
              (run-textual-parser parser-json5 str))))


  #|doc
  `path` must be a path string that points to a valid json5 file.

  `parse-json5-file` tries to parse the json5 file at `path`.
  If successful, the parsed json5 value is returned;
  otherwise, an error with condition type &parser-error is raised.
  |#
  (define parse-json5-file
    (lambda (path)
      (pcheck ([string? path])
              (parse-textual-file parser-json5 path))))

  )
