(library (chezpp regex)
  (export regex? make-regex string->regex sre->regex regex->irregex
          regex-search regex-match regex-match?
          regex-match-substring regex-match-start-index regex-match-end-index
          regex-match-num-submatches regex-match-names
          regex-extract regex-split regex-replace regex-replace/all
          regex-quote regex-opt regex-sre->string)
  (import (chezpp chez) (chezpp irregex) (chezpp utils))

  (define-record-type (regex %make-regex regex?)
    (fields (immutable irregex regex-irregex)))
  (define-record-type ($regex-match %make-regex-match regex-match?)
    (fields (immutable regex match-regex) (immutable data match-data)))

  #|proc:make-regex
  Creates a regular expression wrapper from an Irregex value. `irx` is the compiled
  Irregex object. Returns a regex wrapper.
  |#
  (define (make-regex irx)
    (pcheck ([irregex? irx]) (%make-regex irx)))

  #|proc:string->regex
  Parses `pattern`, a string regular expression, and returns a regex wrapper.
  |#
  (define (string->regex pattern)
    (pcheck ([string? pattern]) (make-regex (string->irregex pattern))))

  #|proc:sre->regex
  Compiles S-expression regular expression `sre` and returns a regex wrapper.
  |#
  (define (sre->regex sre)
    (make-regex (sre->irregex sre)))

  #|proc:regex->irregex
  Returns the underlying Irregex value held by `regex`.
  |#
  (define (regex->irregex regex)
    (pcheck ([regex? regex]) (regex-irregex regex)))

  #|proc:regex-search
  Searches string for regex. Returns a regex-match, or #f when no match exists.
  |#
  (define (regex-search regex string)
    (pcheck ([regex? regex] [string? string])
            (let ([m (irregex-search (regex-irregex regex) string)])
              (and m (%make-regex-match regex m)))))

  #|proc:regex-match
  Matches regex against string. Returns a regex-match, or #f when it fails.
  |#
  (define (regex-match regex string)
    (pcheck ([regex? regex] [string? string])
            (let ([m (irregex-match (regex-irregex regex) string)])
              (and m (%make-regex-match regex m)))))

  #|proc:regex-match-substring
  Returns the substring for capture index in match.
  |#
  (define (regex-match-substring match index)
    (pcheck ([regex-match? match] [(lambda (x) (and (integer? x) (exact? x))) index])
            (irregex-match-substring (match-data match) index)))
  #|proc:regex-match-start-index
  Returns the start character index for capture index in match.
  |#
  (define (regex-match-start-index match index)
    (pcheck ([regex-match? match] [(lambda (x) (and (integer? x) (exact? x))) index])
            (irregex-match-start-index (match-data match) index)))
  #|proc:regex-match-end-index
  Returns the end character index for capture index in match.
  |#
  (define (regex-match-end-index match index)
    (pcheck ([regex-match? match] [(lambda (x) (and (integer? x) (exact? x))) index])
            (irregex-match-end-index (match-data match) index)))
  #|proc:regex-match-num-submatches
  Returns the number of submatches in match.
  |#
  (define (regex-match-num-submatches match)
    (pcheck ([regex-match? match]) (irregex-match-num-submatches (match-data match))))
  #|proc:regex-match-names
  Returns the named captures in match.
  |#
  (define (regex-match-names match)
    (pcheck ([regex-match? match]) (irregex-match-names (match-data match))))
  (define (regex-extract regex string)
    (pcheck ([regex? regex] [string? string])
            (irregex-extract (regex-irregex regex) string)))
  (define (regex-split regex string)
    (pcheck ([regex? regex] [string? string])
            (irregex-split (regex-irregex regex) string)))
  (define (regex-replace regex string . replacement)
    (pcheck ([regex? regex] [string? string])
            (apply irregex-replace (regex-irregex regex) string replacement)))
  (define (regex-replace/all regex string . replacement)
    (pcheck ([regex? regex] [string? string])
            (apply irregex-replace/all (regex-irregex regex) string replacement)))
  (define (regex-quote string)
    (pcheck ([string? string]) (irregex-quote string)))
  (define (regex-opt strings)
    (pcheck ([list? strings]) (irregex-opt strings)))
  (define (regex-sre->string sre)
    (sre->string sre)))
