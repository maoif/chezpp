(library (chezpp parser csv)
  (export parse-csv parse-csv-file)
  (import (chezpp chez)
          (chezpp parser combinator)
          (chezpp internal)
          (chezpp string)
          (chezpp list)
          (chezpp vector)
          (chezpp file)
          (chezpp io)
          (chezpp utils))

  ;; TODO support string parsing
  (define <ws-nl> (<satisfy-char> (lambda (c) (and (char-whitespace? c) (not (char=? c #\newline))))))
  (define (<token-nl> p) (<~ p (<many> <ws-nl>)))

  (define <csv-by>
    (case-lambda
      [() (<csv-by> #\,)]
      [(sep)
       (pcheck ([char? sep])
               (<sep-by1>
                (<map> (lambda (val) (map (lambda (c*)
                                            (apply string c*))
                                          val))
                       (<sep-by1>
                        (<token-nl> (<many>
                                     (</> (<satisfy-char> (lambda (c) (and (not (char=? c sep)) (not (char-whitespace? c)))))
                                          (<not-followed-by> <ws-nl> (<~> (<many> <ws-nl>) (</> (<char> sep)
                                                                                                (<char> #\newline)
                                                                                                <eof>))))))
                        (<token-nl> (<char> sep))))
                (<token-nl> (<char> #\newline))))]))


  (define (parser-csv delim) (<fully> (<csv-by> delim)))


  #|doc
  `str` must be a string representing a valid csv text.

  `parse-csv` tries to parse the csv document represented as `str`.
  If successful, the parsed csv document is returned;
  otherwise, an error with condition type &parser-error is raised.
  |#
  (define parse-csv
    (case-lambda
      [(str) (parse-csv str #\,)]
      [(str delim)
       (pcheck ([string? str] [char? delim])
               (run-textual-parser (parser-csv delim) str))]))


  #|doc
  `path` must be a path string that points to a valid csv file.

  `parse-csv-file` tries to parse the csv file at `path`.
  If successful, the parsed csv document is returned;
  otherwise, an error with condition type &parser-error is raised.
  |#
  (define parse-csv-file
    (case-lambda
      [(path) (parse-csv-file path #\,)]
      [(path delim)
       (pcheck ([string? path] [char? delim])
               (parse-textual-file (parser-csv delim) path))]))

  )
