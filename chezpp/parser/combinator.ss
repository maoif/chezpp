(library (chezpp parser combinator)
  (export parser-error?
          parser-error-who parser-error-kind parser-error-message parser-error-source
          parser-error-offset parser-error-line parser-error-column parser-error-expected
          parser-error-found parser-error-context parser-error-causes parser-error->string
          parser? define-parser
          run-textual-parser run-binary-parser
          run-textual-parser/source run-binary-parser/source
          parse-textual-file parse-binary-file
          declare-lazy-parser define-lazy-parser
          ;; TODO remove these
          parser-call input-pos input-pos-set! input-len save-input binary-input-data
          bindigits->num octdigits->num digits->num hexdigits->num

          <fail> <fail-with> <eof> <result> <satisfy>
          <pos> <pos-at> <msg-t> <msg-f>

          <satisfy-char>
          <item> <char> <string> <whitespace>
          <letter> <upper> <lower>
          <digit>
          <bindigit> <octdigit> <hexdigit> <lower-hexdigit> <upper-hexdigit>
          <digit2> <digit8> <digit10> <digit16> <lower-digit16> <upper-digit16>
          <one-of> <none-of>

          <u8> <u16> <u32> <u64>
          <s8> <s16> <s32> <s64>
          <f32> <f64>

          <u16le> <u32le> <u64le>
          <s16le> <s32le> <s64le>
          <f32le> <f64le>

          <u16be> <u32be> <u64be>
          <s16be> <s32be> <s64be>
          <f32be> <f64be>

          <uimm8> <uimm16> <uimm32> <uimm64>
          <simm8> <simm16> <simm32> <simm64>
          <fimm32> <fimm64>

          <uimm16le> <uimm32le> <uimm64le>
          <simm16le> <simm32le> <simm64le>
          <fimm32le> <fimm64le>

          <uimm16be> <uimm32be> <uimm64be>
          <simm16be> <simm32be> <simm64be>
          <fimm32be> <fimm64be>

          <u8*> <bytes> <u8vec>
          <uleb128> <sleb128>

          <many> <some> <optional>
          <rep> <skip> <sep-by> <sep-by1>
          <~> <~n> <~ ~> <~0> <~1> <~2> <~3> <~4> <~5>
          </>
          <map> <map-st>
          <bind> <bind-st>
          <followed-by> <not-followed-by>
          <as> <as-string> <as-symbol> <as-integer>
          <token> <fully>)
  (import (chezpp chez)
          (chezpp internal)
          (chezpp string)
          (chezpp list)
          (chezpp vector)
          (chezpp io)
          (chezpp file)
          (chezpp utils))


#|
For simplicity, "PC" in the following documentation means "parser combinator".
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   infrastructure
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; TODO mrv stuff

  (define-condition-type &parser-error &error
    %make-parser-error
    parser-error?
    (who parser-error-who)
    (kind parser-error-kind)
    (message parser-error-message)
    (source parser-error-source)
    (offset parser-error-offset)
    (line parser-error-line)
    (column parser-error-column)
    (expected parser-error-expected)
    (found parser-error-found)
    (context parser-error-context)
    (causes parser-error-causes))

  (define-record-type parser-position
    (fields (immutable source)
            (immutable offset)
            (immutable line)
            (immutable column)
            (immutable kind)))

  (define-record-type parser-failure
    (fields (immutable kind)
            (immutable message)
            (immutable position)
            (immutable expected)
            (immutable found)
            (immutable context)
            (immutable causes)))

  (define parser-position-offset<?
    (lambda (a b)
      (< (parser-position-offset a) (parser-position-offset b))))

  (define parser-failure-offset
    (lambda (failure)
      (parser-position-offset (parser-failure-position failure))))

  (define byte?
    (lambda (x)
      (and (integer? x) (exact? x) (<= 0 x #xff))))

  (define unique-cons
    (lambda (x x*)
      (if (exists (lambda (y) (equal? x y)) x*) x* (append x* (list x)))))

  (define unique-append
    (lambda (x* y*)
      (fold-left (lambda (res x) (unique-cons x res)) x* y*)))

  (define write-to-string
    (lambda (x)
      (let ([p (open-output-string)])
        (write x p)
        (let ([s (get-output-string p)])
          (close-output-port p)
          s))))

  (define format-byte
    (lambda (b)
      (let ([s (string-downcase (number->string b 16))])
        (if (< b 16)
            (format "#x0~a" s)
            (format "#x~a" s)))))

  (define format-parser-item
    (lambda (x)
      (cond [(char? x) (write-to-string x)]
            [(string? x) (write-to-string x)]
            [(eq? x 'eof) "EOF"]
            [(symbol? x) (symbol->string x)]
            [(byte? x) (format-byte x)]
            [else (write-to-string x)])))

  (define format-parser-item-list
    (lambda (x*)
      (cond [(null? x*) ""]
            [(null? (cdr x*)) (format-parser-item (car x*))]
            [(null? (cddr x*))
             (format "~a or ~a" (format-parser-item (car x*))
                     (format-parser-item (cadr x*)))]
            [else
             (let loop ([x* x*] [res ""])
               (cond [(null? (cdr x*))
                      (format "~aor ~a" res (format-parser-item (car x*)))]
                     [else
                      (loop (cdr x*)
                            (format "~a~a, " res (format-parser-item (car x*))))]))])))

  (define parser-failure-message*
    (lambda (failure)
      (let ([message (parser-failure-message failure)]
            [expected (parser-failure-expected failure)]
            [found (parser-failure-found failure)])
        (cond [message message]
              [(and (pair? expected) found)
               (if (eq? found 'eof)
                   (format "unexpected EOF, expected ~a"
                           (format-parser-item-list expected))
                   (format "expected ~a, got ~a"
                           (format-parser-item-list expected)
                           (format-parser-item found)))]
              [(pair? expected)
               (format "expected ~a" (format-parser-item-list expected))]
              [found
               (if (eq? found 'eof)
                   "unexpected EOF"
                   (format "unexpected ~a" (format-parser-item found)))]
              [else "parser failed"]))))

  #|proc:parser-error->string
  The `parser-error->string` procedure formats parser error condition `err` as a
  stable one-line message with source and position.
  |#
  (define parser-error->string
    (lambda (err)
      (pcheck ([parser-error? err])
              (let ([source (parser-error-source err)]
                    [line (parser-error-line err)]
                    [column (parser-error-column err)]
                    [offset (parser-error-offset err)]
                    [message (parser-error-message err)])
                (if (and line column)
                    (format "~a:~a:~a: ~a" source (fx1+ line) (fx1+ column) message)
                    (format "~a:byte ~a: ~a" source offset message))))))

  (define parser-failure->condition
    (lambda (who failure)
      (let* ([failure (ensure-parser-failure failure #f)]
             [position (parser-failure-position failure)]
             [message (parser-failure-message* failure)])
        (%make-parser-error who
                            (parser-failure-kind failure)
                            message
                            (parser-position-source position)
                            (parser-position-offset position)
                            (parser-position-line position)
                            (parser-position-column position)
                            (parser-failure-expected failure)
                            (parser-failure-found failure)
                            (parser-failure-context failure)
                            (parser-failure-causes failure)))))

  (define raise-parser-error
    (lambda (who perr)
      (let ([err (parser-failure->condition who perr)])
        (raise (condition err
                          (make-who-condition who)
                          (make-message-condition (parser-error->string err)))))))


  (define $run-textual-parser
    (lambda (who p in source state)
      (pcheck ([parser? p] [string? in])
              (let-values ([(stt val inp/err)
                            (parser-call p
                                         (make-textual-input
                                          (string-length in) source 0 in 0 0)
                                         state 0)])
                (if stt val (raise-parser-error who inp/err))))))


  (define $run-binary-parser
    (lambda (who p in source state)
      (pcheck ([parser? p] [bytevector? in])
              (let-values ([(stt val inp/err)
                            (parser-call p
                                         (make-binary-input
                                          (bytevector-length in) source 0 in)
                                         state 0)])
                (if stt val (raise-parser-error who inp/err))))))

  #|proc:run-textual-parser/source
  The `run-textual-parser/source` procedure runs textual parser `p` over string `in`.
  The `source` parameter is `#f` or a string used in displayed parser errors. The
  optional `state` parameter is passed through to state-aware parser combinators.
  |#
  (define-who run-textual-parser/source
    (case-lambda
      [(p in source) (run-textual-parser/source p in source #f)]
      [(p in source state)
       (pcheck ([parser? p] [string? in])
               (unless (or (not source) (string? source))
                 (errorf who "source must be #f or a string"))
               ($run-textual-parser who p in (or source "<string>") state))]))

  #|proc:run-binary-parser/source
  The `run-binary-parser/source` procedure runs binary parser `p` over bytevector `in`.
  The `source` parameter is `#f` or a string used in displayed parser errors. The
  optional `state` parameter is passed through to state-aware parser combinators.
  |#
  (define-who run-binary-parser/source
    (case-lambda
      [(p in source) (run-binary-parser/source p in source #f)]
      [(p in source state)
       (pcheck ([parser? p] [bytevector? in])
               (unless (or (not source) (string? source))
                 (errorf who "source must be #f or a string"))
               ($run-binary-parser who p in (or source "<bytevector>") state))]))

  #|doc
  `p` must be a textual parser; `in` must be a string;
  `state`, if given, can be any value, and it can be accessed in argument functions
  of PCs suffixed by `-st`, such as `<map-st>` and `<bind-st>`.
  If `state` is not given, it defaults to #f.

  `run-textual-parser` runs the textual parser `p` over the input string `in`,
  and returns the result.
  If the parse process fails, an error with condition type &parser-error is raised.
  |#
  (define-who run-textual-parser
    (case-lambda
      [(p in) (run-textual-parser p in #f)]
      [(p in state)
       (pcheck ([parser? p] [string? in])
               ($run-textual-parser who p in "<string>" state))]))


  #|doc
  `p` must be a binary parser; `in` must be a bytevector;
  `state`, if given, can be any value, and it can be accessed in argument functions
  of PCs suffixed by `-st`, such as `<map-st>` and `<bind-st>`.
  If `state` is not given, it defaults to #f.

  `run-binary-parser` runs the binary parser `p` over the input bytevector `in`,
  and returns the result.
  If the parse process fails, an error with condition type &parser-error is raised.
  |#
  (define-who run-binary-parser
    (case-lambda
      [(p in) (run-binary-parser p in #f)]
      [(p in state)
       (pcheck ([parser? p] [bytevector? in])
               ($run-binary-parser who p in "<bytevector>" state))]))


  #|doc
  `p` must be a textual parser; `path` must be a string that is valid path to a text file;
  `state`, if given, can be any value, and it can be accessed in argument functions
  of PCs suffixed by `-st`, such as `<map-st>` and `<bind-st>`.
  If `state` is not given, it defaults to #f.

  `parse-textual-file` runs the textual parser `p` over the string read from the text file
  at `path`, and returns the result.
  If the parse process fails, an error with condition type &parser-error is raised.
  |#
  (define-who parse-textual-file
    (case-lambda
      [(p path) (parse-textual-file p path #f)]
      [(p path state)
       (pcheck ([parser? p] [file-regular? path])
               (let ([in (read-string path)])
                 ($run-textual-parser who p in path state)))]))


  #|doc
  `p` must be a binary parser; `path` must be a string that is valid path to a binary file;
  `state`, if given, can be any value, and it can be accessed in argument functions
  of PCs suffixed by `-st`, such as `<map-st>` and `<bind-st>`.
  If `state` is not given, it defaults to #f.

  `parse-binary-file` runs the binary parser `p` over the bytevector read from the binary file
  at `path`, and returns the result.
  If the parse process fails, an error with condition type &parser-error is raised.
  |#
  (define-who parse-binary-file
    (case-lambda
      [(p path) (parse-binary-file p path #f)]
      [(p path state)
       (pcheck ([parser? p] [file-regular? path])
               (let ([in (read-u8vec path)])
                 ($run-binary-parser who p in path state)))]))


  (define (mk-digits->num who r)
    (lambda (d*)
      (pcheck ([list? d*])
              (fold-left (lambda (n d)
                           (when (>= d r)
                             (errorf who "bad digit ~a for base ~a" d r))
                           (+ d (* n r))) 0 d*))))
  #|doc
  Convert a list of binary digits into a number.
  |#
  (define-who bindigits->num (mk-digits->num who 2))
  #|doc
  Convert a list of octal digits into a number.
  |#
  (define-who octdigits->num (mk-digits->num who 8))
  #|doc
  Convert a list of decimal digits into a number.
  |#
  (define-who digits->num    (mk-digits->num who 10))
  #|doc
  Convert a list of hexadecimal digits into a number.
  |#
  (define-who hexdigits->num (mk-digits->num who 16))



  (define-record-type (parser make-parser $parser?)
    (fields (immutable body parser-body)))

  #|proc:parser?
  The `parser?` procedure returns `#t` when `value` is a parser or legacy parser procedure.
  It returns `#f` otherwise. Procedure support is temporary during parser record migration.
  |#
  (define parser?
    (lambda (value)
      (or ($parser? value) (procedure? value))))

  (define-record-type (lazy-parser make-lazy-parser lazy-parser?)
    (parent parser)
    (fields))

  #|macro:define-parser
  The `define-parser` macro defines parser `name` with body expression `body`. Within
  `body`, `inp`, `state`, and `lvl` are the current input, parser state, and nesting level.
  When `body` is omitted, the macro declares a lazy parser that must be completed by a
  later `define-parser` form using the same lexical identifier.
  |#
  (define-syntax define-parser
    (let ([declared '()])
      (define consume-declaration!
        (lambda (name)
          (let loop ([rest declared] [kept '()])
            (cond [(null? rest) #f]
                  [(free-identifier=? (car rest) name)
                   (set! declared (append (reverse kept) (cdr rest)))
                   #t]
                  [else (loop (cdr rest) (cons (car rest) kept))]))))
      (lambda (stx)
        (syntax-case stx ()
          [(_ name)
           (identifier? #'name)
           (begin
             (set! declared (cons #'name declared))
             #'(define name
                 (make-lazy-parser
                  (let ([body (lambda (inp state lvl)
                                (errorf 'define-parser "parser body not defined"))])
                    (case-lambda
                      [(true-body) (set! body true-body)]
                      [(inp state lvl) (body inp state lvl)])))))]
          [(_ name body)
           (identifier? #'name)
           (with-syntax ([inp (datum->syntax #'name 'inp)]
                         [state (datum->syntax #'name 'state)]
                         [lvl (datum->syntax #'name 'lvl)])
             (if (consume-declaration! #'name)
                 (with-syntax ([initialized
                                (car (generate-temporaries '(initialized)))])
                   #'(define initialized
                       (let ([parser name])
                         (unless (lazy-parser? parser)
                           (errorf 'define-parser
                                   "not a declared lazy parser: ~a" parser))
                         ((parser-body parser)
                          (lambda (inp state lvl) body))
                         #t)))
                 #'(define name
                     (make-parser (lambda (inp state lvl) body)))))]))))

  ;; used internally for defining parser implementations

  (define-syntax parser-call
    (lambda (stx)
      (syntax-case stx ()
        [(k p args ...)
         #'(let ([pp p])
             (if ($parser? pp)
                 ((parser-body pp) args ...)
                 (pp args ...)))])))


  (define-syntax declare-lazy-parser
    (lambda (stx)
      (syntax-case stx ()
        [(k p)
         (identifier? #'p)
         #'(define p
             (make-lazy-parser
              (let ([body (lambda (inp state lvl)
                            (errorf 'p "parser code not defined"))])
                (case-lambda
                  [(true-body) (set! body true-body)]
                  [(inp state lvl) (body inp state lvl)]))))])))


  (define-syntax define-lazy-parser
    (lambda (stx)
      (syntax-case stx ()
        [(k p e)
         (identifier? #'p)
         #'(define dummy
             (let ([body e])
               (if (lazy-parser? p)
                   (if (parser? body)
                       ((parser-body p)
                        (lambda (inp state lvl)
                          (parser-call body inp state lvl)))
                       (errorf 'k "not a parser procedure: ~a" body))
                   (errorf 'k "not a declared lazy parser: ~a" p))
               #t))])))


;;;; input logic

  (define-record-type input
    (fields (immutable len)
            (immutable source)
            ;; position of next symbol to read
            (mutable pos)))

  (define-record-type textual-input
    (parent input)
    (fields (immutable str) (mutable line) (mutable col)))

  (define-record-type binary-input
    (parent input)
    ;; bytevector
    (fields (immutable data)))

  (define save-input
    (lambda (inp)
      (cond
       [(textual-input? inp)
        (make-textual-input
         (input-len inp)
         (input-source inp)
         (input-pos inp)
         (textual-input-str inp)
         (textual-input-line inp)
         (textual-input-col inp))]
       [(binary-input? inp)
        (make-binary-input
         (input-len inp)
         (input-source inp)
         (input-pos inp)
         (binary-input-data inp))]
       [else (assert-unreachable)])))

  (define recompute-text-position
    (lambda (str target)
      (let loop ([i 0] [line 0] [col 0])
        (if (fx= i target)
            (values line col)
            (let ([c (string-ref str i)])
              (if (char=? c #\newline)
                  (loop (fx1+ i) (fx1+ line) 0)
                  (loop (fx1+ i) line (fx1+ col))))))))

  (define-who set-input-position!
    (lambda (inp pos)
      (pcheck ([natural? pos])
              (when (> pos (input-len inp))
                (errorf who "position ~a is past input length ~a" pos (input-len inp)))
              (cond
               [(textual-input? inp)
                (let-values ([(line col)
                              (recompute-text-position (textual-input-str inp) pos)])
                  (input-pos-set! inp pos)
                  (textual-input-line-set! inp line)
                  (textual-input-col-set! inp col))]
               [(binary-input? inp)
                (input-pos-set! inp pos)]
               [else (assert-unreachable)]))))

  (define input-position-at
    (lambda (inp pos)
      (let ([inp1 (save-input inp)])
        (set-input-position! inp1 pos)
        (input->parser-position inp1))))

  (define input->parser-position
    (lambda (inp)
      (cond
       [(textual-input? inp)
        (make-parser-position (or (input-source inp) "<string>")
                              (input-pos inp)
                              (textual-input-line inp)
                              (textual-input-col inp)
                              'text)]
       [(binary-input? inp)
        (make-parser-position (or (input-source inp) "<bytevector>")
                              (input-pos inp)
                              #f
                              #f
                              'binary)]
       [else
        (make-parser-position "<unknown>" 0 #f #f 'binary)])))

  (define parser-failure-at
    (lambda (inp kind message expected found)
      (make-parser-failure kind message (input->parser-position inp)
                           expected found '() '())))

  (define parser-failure-expected-at
    (lambda (inp expected found)
      (parser-failure-at inp 'expected #f expected found)))

  (define parser-failure-eof
    (lambda (inp expected message)
      (parser-failure-at inp 'unexpected-eof message expected 'eof)))

  (define parser-failure-custom
    (lambda (inp message)
      (parser-failure-at inp 'custom message '() #f)))

  (define parser-failure-add-context
    (lambda (failure context)
      (let ([failure (ensure-parser-failure failure #f)])
        (make-parser-failure (parser-failure-kind failure)
                             (parser-failure-message failure)
                             (parser-failure-position failure)
                             (parser-failure-expected failure)
                             (parser-failure-found failure)
                             (cons context (parser-failure-context failure))
                             (parser-failure-causes failure)))))

  (define parser-failure-merge-choice
    (lambda (failure*)
      (cond [(null? failure*)
             (make-parser-failure 'empty-choice "empty choice"
                                  (make-parser-position "<unknown>" 0 #f #f 'binary)
                                  '() #f '() '())]
            [(null? (cdr failure*))
             (parser-failure-add-context (car failure*) '</>)]
            [else
             (let* ([first (car failure*)]
                    [expected (fold-left
                               (lambda (res failure)
                                 (unique-append res (parser-failure-expected failure)))
                               '()
                               failure*)]
                    [found (parser-failure-found first)])
               (make-parser-failure 'expected
                                    #f
                                    (parser-failure-position first)
                                    expected
                                    found
                                    '(</>)
                                    failure*))])))

  (define ensure-parser-failure
    (case-lambda
      [(err inp)
       (cond [(parser-failure? err) err]
             [(input? err)
              (parser-failure-custom err "parser failed")]
             [(string? err)
              (make-parser-failure 'custom err
                                   (if inp
                                       (input->parser-position inp)
                                       (make-parser-position "<unknown>" 0 #f #f 'binary))
                                   '() #f '() '())]
             [else
              (make-parser-failure 'custom (format "~a" err)
                                   (if inp
                                       (input->parser-position inp)
                                       (make-parser-position "<unknown>" 0 #f #f 'binary))
                                   '() #f '() '())])]
      [(err) (ensure-parser-failure err #f)]))

  (define current-found
    (lambda (inp)
      (if (end-of-input? inp)
          'eof
          (cond [(textual-input? inp)
                 (string-ref (textual-input-str inp) (input-pos inp))]
                [(binary-input? inp)
                 (bytevector-u8-ref (binary-input-data inp) (input-pos inp))]
                [else #f]))))

  (define failure-at-position
    (lambda (inp pos expected found)
      (make-parser-failure 'expected #f (input-position-at inp pos)
                           expected found '() '())))

  (define-who advance!
    (case-lambda
      [(inp) (advance! inp 1)]
      [(inp n)
       (pcheck ([natural? n])
               (let ([len (input-len inp)] [pos (input-pos inp)])
                 (cond [(zero? n) (void)]
                       [(= len pos) (errorf who "already at eof")]
                       [else (input-pos-set! inp (min len (+ pos n)))])))]))

  (define update-line/col!
    (lambda (inp c)
      (let ([line (textual-input-line inp)]
            [col  (textual-input-col  inp)])
        (if (char=? c #\newline)
            (begin (textual-input-line-set! inp (fx1+ line))
                   (textual-input-col-set!  inp 0))
            (textual-input-col-set! inp (fx1+ col))))))

  (define end-of-input?
    (lambda (inp)
      (let ([len (input-len inp)] [pos (input-pos inp)])
        (= len pos))))

  ;; return the next available character in the input
  ;; and advance the position,
  ;; or eof
  (define get-next!
    (lambda (inp)
      (let ([len (input-len inp)] [pos (input-pos inp)])
        (if (= len pos)
            (eof-object)
            (let ([c (string-ref (textual-input-str inp) pos)])
              (advance! inp)
              (update-line/col! inp c)
              c)))))

  ;; check whether the next input character is `c`,
  ;; if so, advance the position by 1,
  ;; otherwise return #f
  (define peek-char!
    (lambda (inp c)
      (let ([len (input-len inp)] [pos (input-pos inp)])
        (if (< pos len)
            (let ([cc (string-ref (textual-input-str inp) pos)])
              (if (char=? cc c)
                  (begin (advance! inp) (update-line/col! inp cc) #t)
                  #f))
            #f))))

  ;; check whether the input contains string `str`,
  ;; if so, advance the position by the length of `str`,
  ;; otherwise return #f
  (define peek-string!
    (lambda (inp str)
      ;; check whether the substring starting at `i` in `str` is `substr`,
      ;; also maintain line and col
      (define str=?
        (lambda (str substr i line col)
          (let ([end (+ i (string-length substr))])
            (let loop ([i i] [j 0] [line line] [col col])
              (if (= i end)
                  (values #t line col)
                  (let ([c1 (string-ref str i)] [c2 (string-ref substr j)])
                    (if (char=? c1 c2)
                        (if (char=? c1 #\newline)
                            (loop (fx1+ i) (fx1+ j) (fx1+ line) 0)
                            (loop (fx1+ i) (fx1+ j) line        (fx1+ col)))
                        (values #f #f #f))))))))
      (let ([len (input-len inp)] [pos (input-pos inp)] [strlen (string-length str)])
        (if (< (+ pos strlen -1) len)
            (let-values ([(?eq line col) (str=? (textual-input-str  inp) str pos
                                                (textual-input-line inp)
                                                (textual-input-col  inp))])
              (if ?eq
                  (begin (advance! inp strlen)
                         (textual-input-line-set! inp line)
                         (textual-input-col-set!  inp col)
                         #t)
                  #f))
            #f))))

  (define string-mismatch
    (lambda (inp str)
      (let ([len (input-len inp)]
            [pos (input-pos inp)]
            [strlen (string-length str)]
            [data (textual-input-str inp)])
        (let loop ([i 0])
          (cond [(fx= i strlen) (values pos #f)]
                [(>= (+ pos i) len) (values (+ pos i) 'eof)]
                [(char=? (string-ref data (+ pos i)) (string-ref str i))
                 (loop (fx1+ i))]
                [else (values (+ pos i) (string-ref data (+ pos i)))])))))


  ;; TODO report EOF in bin peeks

;;;; peek for given value
  (define-syntax gen-peek8!
    (syntax-rules ()
      [(_ name ref step)
       (define name
         (lambda (inp x)
           (let ([len (input-len inp)] [pos (input-pos inp)])
             (if (< (+ pos step -1) len)
                 (let ([y (ref (binary-input-data inp) pos)])
                   (if (fx=? x y)
                       (begin (advance! inp step) #t)
                       #f))
                 #f))))]))
  (gen-peek8! peek-u8! bytevector-u8-ref 1)
  (gen-peek8! peek-s8! bytevector-s8-ref 1)

  (define-syntax gen-peek!
    (syntax-rules ()
      [(_ name ref step endian)
       (define name
         (lambda (inp x)
           (let ([len (input-len inp)] [pos (input-pos inp)])
             (if (< (+ pos step -1) len)
                 (let ([y (ref (binary-input-data inp) pos endian)])
                   (if (fx=? x y)
                       (begin (advance! inp step) #t)
                       #f))
                 #f))))]))
  (define-syntax gen-peek/generic!
    (syntax-rules ()
      [(_ name ref step endian)
       (define name
         (lambda (inp x)
           (let ([len (input-len inp)] [pos (input-pos inp)])
             (if (< (+ pos step -1) len)
                 (let ([y (ref (binary-input-data inp) pos endian)])
                   (if (= x y)
                       (begin (advance! inp step) #t)
                       #f))
                 #f))))]))
  (define-syntax gen-fpeek!
    (syntax-rules ()
      [(_ name ref step endian)
       (define name
         (lambda (inp x)
           (let ([len (input-len inp)] [pos (input-pos inp)])
             (if (< (+ pos step -1) len)
                 (let ([y (ref (binary-input-data inp) pos endian)])
                   (if (fl=? x y)
                       (begin (advance! inp step) #t)
                       #f))
                 #f))))]))
  (gen-peek! peek-u16le! bytevector-u16-ref 2 (endianness little))
  (gen-peek! peek-u32le! bytevector-u32-ref 4 (endianness little))
  (gen-peek/generic! peek-u64le! bytevector-u64-ref 8 (endianness little))
  (gen-peek! peek-s16le! bytevector-s16-ref 2 (endianness little))
  (gen-peek! peek-s32le! bytevector-s32-ref 4 (endianness little))
  (gen-peek/generic! peek-s64le! bytevector-s64-ref 8 (endianness little))
  (gen-fpeek! peek-f32le! bytevector-ieee-single-ref 4 (endianness little))
  (gen-fpeek! peek-f64le! bytevector-ieee-double-ref 8 (endianness little))

  (gen-peek! peek-u16be! bytevector-u16-ref 2 (endianness big))
  (gen-peek! peek-u32be! bytevector-u32-ref 4 (endianness big))
  (gen-peek/generic! peek-u64be! bytevector-u64-ref 8 (endianness big))
  (gen-peek! peek-s16be! bytevector-s16-ref 2 (endianness big))
  (gen-peek! peek-s32be! bytevector-s32-ref 4 (endianness big))
  (gen-peek/generic! peek-s64be! bytevector-s64-ref 8 (endianness big))
  (gen-fpeek! peek-f32be! bytevector-ieee-single-ref 4 (endianness big))
  (gen-fpeek! peek-f64be! bytevector-ieee-double-ref 8 (endianness big))

;;;; peek for arbirary value
  (define-syntax gen-peek8
    (syntax-rules ()
      [(_ name ref step)
       (define name
         (lambda (inp)
           (let ([len (input-len inp)] [pos (input-pos inp)])
             (if (< (+ pos step -1) len)
                 (let ([y (ref (binary-input-data inp) pos)])
                   (advance! inp step)
                   y)
                 #f))))]))
  (gen-peek8 peek-u8 bytevector-u8-ref 1)
  (gen-peek8 peek-s8 bytevector-s8-ref 1)

  (define-syntax gen-peek
    (syntax-rules ()
      [(_ name ref step endian)
       (define name
         (lambda (inp)
           (let ([len (input-len inp)] [pos (input-pos inp)])
             (if (< (+ pos step -1) len)
                 (let ([y (ref (binary-input-data inp) pos endian)])
                   (advance! inp step)
                   y)
                 #f))))]))
  (gen-peek peek-u16le bytevector-u16-ref 2 (endianness little))
  (gen-peek peek-u32le bytevector-u32-ref 4 (endianness little))
  (gen-peek peek-u64le bytevector-u64-ref 8 (endianness little))
  (gen-peek peek-s16le bytevector-s16-ref 2 (endianness little))
  (gen-peek peek-s32le bytevector-s32-ref 4 (endianness little))
  (gen-peek peek-s64le bytevector-s64-ref 8 (endianness little))
  (gen-peek peek-f32le bytevector-ieee-single-ref 4 (endianness little))
  (gen-peek peek-f64le bytevector-ieee-double-ref 8 (endianness little))

  (gen-peek peek-u16be bytevector-u16-ref 2 (endianness big))
  (gen-peek peek-u32be bytevector-u32-ref 4 (endianness big))
  (gen-peek peek-u64be bytevector-u64-ref 8 (endianness big))
  (gen-peek peek-s16be bytevector-s16-ref 2 (endianness big))
  (gen-peek peek-s32be bytevector-s32-ref 4 (endianness big))
  (gen-peek peek-s64be bytevector-s64-ref 8 (endianness big))
  (gen-peek peek-f32be bytevector-ieee-single-ref 4 (endianness big))
  (gen-peek peek-f64be bytevector-ieee-double-ref 8 (endianness big))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   common primitives
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  #|doc
  `<result>` takes an arbirary value `v`, and returns a parser that when invoked,
  will always succeed with the parse value `v.
  |#
  (define (<result> v)
    (lambda (inp state lvl)
      (values #t v inp)))


  #|doc
  `<fail>` is a parser that always fails when invoked.
  |#
  (define-who <fail>
    (lambda (inp state lvl)
      (values #f #f (parser-failure-custom inp (format "~a: failed" who)))))


  #|doc
  `<fail-with>` takes a message value `msg` (usually a string), and returns a parser
  that always fails with the given message value when invoked.
  |#
  (define-who (<fail-with> msg)
    (lambda (inp state lvl)
      (values #f #f (parser-failure-custom inp (format "~a: ~a" who msg)))))


  #|doc
  `<eof>` is a parser that tries to match the end of the input, be it textual or binary.
  |#
  (define <eof>
    (lambda (inp state lvl)
      (let ([len (input-len inp)] [pos (input-pos inp)])
        (if (= len pos)
            (values #t #t inp)
            (values #f #f (parser-failure-expected-at inp '(eof) (current-found inp)))))))


  #|doc
  `p` must be a parser; `f` must be of type (Any -> Bool).

  `<satisfy>` uses `p` to parse the input, and when succesful,
  applies `f` to the parsed value.
  If `f` returns #t, the parse succeeds and the value is returned;
  otherwise the parse fails.
  |#
  (define <satisfy>
    (case-lambda
      [(p f)
       (<satisfy> p f "failed predicate")]
      [(p f msg)
       (lambda (inp state lvl)
         (let-values ([(stt val inp1) (parser-call p inp state lvl)])
           (if stt
               (if (f val)
                   (values #t val inp1)
                   (values #f #f (parser-failure-at inp 'predicate msg '() val)))
               (values #f #f inp1))))]))


  #|doc
  Return the current parse position in the input.
  |#
  (define-who <pos>
    (lambda (inp state lvl)
      (values #t (input-pos inp) inp)))


  #|proc:<pos-at>
  The `<pos-at>` procedure takes natural number `n` and parser `p`, and returns a parser.
  The parser temporarily sets the input position to `n`, runs `p`, and restores the
  original input position when `p` succeeds. The parser fails when `p` fails.
  It is an error when `n` is greater than or equal to the input length at parse time.
  |#
  (define-who (<pos-at> n p)
    (pcheck ([natural? n] [parser? p])
            (lambda (inp state lvl)
              ;; TODO error report
              (if (> n (input-len inp))
                  (values #f #f
                          (parser-failure-custom
                           inp
                           (format "invalid position ~a, should be between ~a and ~a"
                                   n 0 (input-len inp))))
                  (let ([new-inp (save-input inp)])
                (set-input-position! new-inp n)
                (let-values ([(stt val inp1) (parser-call p new-inp state (fx1+ lvl))])
                  (if stt
                      (values #t val inp)
                      (values #f #f (ensure-parser-failure inp1 new-inp)))))))))


  #|doc
  `msg` should be a printable value; `who`, if present, should also be a
  printable value that can be use to identify the message generator.

  `<msg-t>` receives a printable value `msg` and optionally a printable value `who`
  and returns a parser that when invoked, will print the current parse information
  containing `who`, `msg`, and the current input position, and succeeds unconditionally.

  This combinator facilitates debugging and can be used in `<~>`.
  However, it must be noted that when this combinator appears last in a `<~>`
  that is supposed to fail, and the `<~>` is further wrapped by a `<many>`,
  then there may be a dead loop, since the parser created by `<msg-t>` always succeeds.
  |#
  (define-who <msg-t>
    (case-lambda
      [(msg) (<msg-t> who msg)]
      [(who msg)
       (lambda (inp state lvl)
         (let ([msg (format "~a: ~a (~a/~a)" who msg (input-pos inp) (input-len inp))])
           (println msg)
           (values #t msg inp)))]))


  #|doc
  `msg` should be a printable value. `who`, if present, should also be a
  printable value that can be use to identify the message generator.

  `<msg-f>` receives a printable value `msg` and optionally a printable value `who`
  and returns a parser that when invoked, will print the current parse information
  containing `who`, `msg`, and the current input position, and fails unconditionally.
  |#
  (define-who <msg-f>
    (case-lambda
      [(msg) (<msg-f> who msg)]
      [(who msg)
       (lambda (inp state lvl)
         (let ([msg (format "~a: ~a (~a/~a)" who msg (input-pos inp) (input-len inp))])
           (values #f msg (parser-failure-custom inp msg))))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   textual primitives
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define ascii-digit?
    (lambda (c) (char<=? #\0 c #\9)))

  (define ascii-bindigit?
    (lambda (c) (or (char=? c #\0) (char=? c #\1))))

  (define ascii-octdigit?
    (lambda (c) (char<=? #\0 c #\7)))

  (define ascii-hexdigit?
    (lambda (c)
      (or (ascii-digit? c)
          (char<=? #\a c #\f)
          (char<=? #\A c #\F))))

  (define-who char->num
    (lambda (c)
      (if (ascii-digit? c)
          (fx- (char->integer c) 48)
          (errorf who "not a digit: ~a" c))))

  #|doc
  Parse and return the current character unconditionally.
  `<item>` fails only if EOF is reached.
  |#
  (define <item>
    (lambda (inp state lvl)
      (if (end-of-input? inp)
          (values #f #f (parser-failure-eof inp '(item) #f))
          (values #t (get-next! inp) inp))))

  #|doc
  `f` must be a function of the type (Char -> Bool);
  `msg`, if given, must be a string.

  `<satisfy-char>` takes a character predicate and an optional error message,
  and returns a parser that succeeds when the current character satisfies `f`.
  If the returned parser succeeds, the current character is returned;
  otherwise, the given error message may be used to report the error.
  |#
  (define <satisfy-char>
    (case-lambda
      [(f)     (<satisfy> <item> f "failed predicate")]
      [(f msg) (<satisfy> <item> f msg)]))

  #|proc:<char>
  The `<char>` procedure takes character `c` and returns a textual parser.
  The parser matches exactly `c` at the current input position and returns `c`.
  It fails when the current character is not `c`.
  |#
  (define-who (<char> c)
    (pcheck ([char? c])
            (lambda (inp state lvl)
              (if (peek-char! inp c)
                  (values #t c inp)
                  (values #f #f
                          (parser-failure-expected-at inp (list c) (current-found inp)))))))

  #|proc:<string>
  The `<string>` procedure takes string `str` and returns a textual parser.
  The parser matches exactly `str` at the current input position and returns a fresh
  string containing the matched characters.
  |#
  (define-who (<string> str)
    (pcheck ([string? str])
            (lambda (inp state lvl)
              (if (peek-string! inp str)
                  (values #t (string-copy str) inp)
                  (let-values ([(pos found) (string-mismatch inp str)])
                    (values #f #f
                            (failure-at-position inp pos (list str) found)))))))

  #|doc
  Parse and return an arbirary letter character.
  |#
  (define <letter>
    (<satisfy-char> char-alphabetic? "not a letter"))

  #|doc
  Parse and return an arbirary upper-case letter character.
  |#
  (define <upper>
    (<satisfy-char> char-upper-case? "not uppercase latter"))

  #|doc
  Parse and return an arbirary lower-case letter character.
  |#
  (define <lower>
    (<satisfy-char> char-lower-case? "not lowercase latter"))

  #|doc
  Parse and return an arbirary whitespace character.
  |#
  (define <whitespace>
    (<satisfy-char> char-whitespace? "not a whitespace"))

  #|doc
  Parse a deciaml digit and return the corresponding character.
  |#
  (define <digit>
    (<satisfy-char> ascii-digit? "not a digit"))

  #|doc
  Parse a binary digit and return the corresponding character.
  |#
  (define <bindigit>
    (<satisfy-char> ascii-bindigit? "not a binary digit"))

  #|doc
  Parse a hex digit and return the corresponding character.
  |#
  (define <hexdigit>
    (<satisfy-char> ascii-hexdigit? "not a hexadecimal digit"))

  #|doc
  Parse a lower hex digit and return the corresponding character.
  |#
  (define <lower-hexdigit>
    (<satisfy-char> (lambda (x) (or (ascii-digit? x)
                                    (char<=? #\a x #\f)))))

  #|doc
  Parse an upper hex digit and return the corresponding character.
  |#
  (define <upper-hexdigit>
    (<satisfy-char> (lambda (x) (or (ascii-digit? x)
                                    (char<=? #\A x #\F)))))

  #|doc
  Parse a octal digit and return the corresponding character.
  |#
  (define <octdigit>
    (<satisfy-char> ascii-octdigit? "not an octal digit"))

  #|doc
  `<one-of>` takes a string and returns a parser that succeeds when the current
  character that is in the string `str`.
  When succesful, the current character is returned.
  |#
  (define (<one-of> str)
    (<satisfy-char> (lambda (c) (string-contains? str c))
                    (format "not one of \"~a\"" str)))

  #|doc
  `<none-of>` takes a string and returns a parser that succeeds when the current
  character is not contained in the string `str`.
  When succesful, the current character is returned.
  |#
  (define (<none-of> str)
    (<satisfy-char> (lambda (c) (not (string-contains? str c)))
                    (format "should not be one of \"~a\"" str)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   binary primitives
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define enough-bytes?
    (lambda (inp n)
      (<= (+ (input-pos inp) n) (input-len inp))))

  (define binary-short?
    (lambda (inp n)
      (not (enough-bytes? inp n))))


  (define-syntax gen-bin-prim
    (syntax-rules ()
      [(_ name peek expected step)
       (define-who name
         (lambda (inp state lvl)
           (let ([v (peek inp)])
             (if v
                 (values #t v inp)
                 (values #f #f
                         (if (binary-short? inp step)
                             (parser-failure-eof inp (list expected) #f)
                             (parser-failure-expected-at inp
                                                      (list expected)
                                                      (current-found inp))))))))]))
  (define-syntax gen-bin-imm-prim
    (syntax-rules ()
      [(_ name peek! valid-x? step)
       (define-who (name x)
         (unless (valid-x? x)
           (errorf who "invalid number: ~a" x))
         (lambda (inp state lvl)
           (let ([v (peek! inp x)])
             (if v
                 (values #t x inp)
                 (values #f #f
                         (if (binary-short? inp step)
                             (parser-failure-eof inp (list x) #f)
                             (parser-failure-expected-at inp
                                                      (list x)
                                                      (current-found inp))))))))]))
  (define int-in-range?
    (lambda (x lo hi)
      (and (integer? x) (exact? x) (<= lo x hi))))
  (define u8?  (lambda (x) (int-in-range? x 0 (sub1 (expt 2 8)))))
  (define u16? (lambda (x) (int-in-range? x 0 (sub1 (expt 2 16)))))
  (define u32? (lambda (x) (int-in-range? x 0 (sub1 (expt 2 32)))))
  (define u64? (lambda (x) (int-in-range? x 0 (sub1 (expt 2 64)))))
  (define s8?  (lambda (x) (int-in-range? x (- (expt 2 7))  (sub1 (expt 2 7)))))
  (define s16? (lambda (x) (int-in-range? x (- (expt 2 15)) (sub1 (expt 2 15)))))
  (define s32? (lambda (x) (int-in-range? x (- (expt 2 31)) (sub1 (expt 2 31)))))
  (define s64? (lambda (x) (int-in-range? x (- (expt 2 63)) (sub1 (expt 2 63)))))
  (define f32? (lambda (x) (and (flonum? x) (not (nan? x)))))
  (define f64? (lambda (x) (and (flonum? x) (not (nan? x)))))
  (define sleb128-len? (lambda (x) (and (fixnum? x) (fx> x 0))))
  (define all-u8? (lambda (x*) (andmap u8? x*)))
  (define all-parsers? (lambda (x*) (andmap parser? x*)))

  ;; TODO maybe merge the two gen macros
  ;; TODO rename these since they also change the inp state
  ;; try match and return an arbirary value
  (gen-bin-prim <u8>  peek-u8 'u8 1)
  (gen-bin-prim <u16> peek-u16le 'u16 2)
  (gen-bin-prim <u32> peek-u32le 'u32 4)
  (gen-bin-prim <u64> peek-u64le 'u64 8)
  (gen-bin-prim <s8>  peek-s8 's8 1)
  (gen-bin-prim <s16> peek-s16le 's16 2)
  (gen-bin-prim <s32> peek-s32le 's32 4)
  (gen-bin-prim <s64> peek-s64le 's64 8)
  (gen-bin-prim <f32> peek-f32le 'f32 4)
  (gen-bin-prim <f64> peek-f64le 'f64 8)

  (gen-bin-prim <u16le> peek-u16le 'u16le 2)
  (gen-bin-prim <u32le> peek-u32le 'u32le 4)
  (gen-bin-prim <u64le> peek-u64le 'u64le 8)
  (gen-bin-prim <s16le> peek-s16le 's16le 2)
  (gen-bin-prim <s32le> peek-s32le 's32le 4)
  (gen-bin-prim <s64le> peek-s64le 's64le 8)
  (gen-bin-prim <f32le> peek-f32le 'f32le 4)
  (gen-bin-prim <f64le> peek-f64le 'f64le 8)

  (gen-bin-prim <u16be> peek-u16be 'u16be 2)
  (gen-bin-prim <u32be> peek-u32be 'u32be 4)
  (gen-bin-prim <u64be> peek-u64be 'u64be 8)
  (gen-bin-prim <s16be> peek-s16be 's16be 2)
  (gen-bin-prim <s32be> peek-s32be 's32be 4)
  (gen-bin-prim <s64be> peek-s64be 's64be 8)
  (gen-bin-prim <f32be> peek-f32be 'f32be 4)
  (gen-bin-prim <f64be> peek-f64be 'f64be 8)

  ;; try match a given immediate value
  (gen-bin-imm-prim <uimm8>  peek-u8!    u8? 1)
  (gen-bin-imm-prim <uimm16> peek-u16le! u16? 2)
  (gen-bin-imm-prim <uimm32> peek-u32le! u32? 4)
  (gen-bin-imm-prim <uimm64> peek-u64le! u64? 8)
  (gen-bin-imm-prim <simm8>  peek-s8!    s8? 1)
  (gen-bin-imm-prim <simm16> peek-s16le! s16? 2)
  (gen-bin-imm-prim <simm32> peek-s32le! s32? 4)
  (gen-bin-imm-prim <simm64> peek-s64le! s64? 8)
  (gen-bin-imm-prim <fimm32> peek-f32le! f32? 4)
  (gen-bin-imm-prim <fimm64> peek-f64le! f64? 8)

  (gen-bin-imm-prim <uimm16le> peek-u16le! u16? 2)
  (gen-bin-imm-prim <uimm32le> peek-u32le! u32? 4)
  (gen-bin-imm-prim <uimm64le> peek-u64le! u64? 8)
  (gen-bin-imm-prim <simm16le> peek-s16le! s16? 2)
  (gen-bin-imm-prim <simm32le> peek-s32le! s32? 4)
  (gen-bin-imm-prim <simm64le> peek-s64le! s64? 8)
  (gen-bin-imm-prim <fimm32le> peek-f32le! f32? 4)
  (gen-bin-imm-prim <fimm64le> peek-f64le! f64? 8)

  (gen-bin-imm-prim <uimm16be> peek-u16be! u16? 2)
  (gen-bin-imm-prim <uimm32be> peek-u32be! u32? 4)
  (gen-bin-imm-prim <uimm64be> peek-u64be! u64? 8)
  (gen-bin-imm-prim <simm16be> peek-s16be! s16? 2)
  (gen-bin-imm-prim <simm32be> peek-s32be! s32? 4)
  (gen-bin-imm-prim <simm64be> peek-s64be! s64? 8)
  (gen-bin-imm-prim <fimm32be> peek-f32be! f32? 4)
  (gen-bin-imm-prim <fimm64be> peek-f64be! f64? 8)


  ;; return a copy of the list of bytes if successful
  (define ($<u8*> who)
    (lambda b*
      (pcheck ([all-u8? b*])
              (lambda (inp state lvl)
                (let loop ([u8* b*])
                  (if (null? u8*)
                      (values #t (list-copy b*) inp)
                      (if (peek-u8! inp (car u8*))
                          (loop (cdr u8*))
                          (let ([failure (parser-failure-expected-at
                                          inp
                                          (list (car u8*))
                                          (current-found inp))])
                            (values #f #f failure)))))))))

  #|proc:<u8*>
  The `<u8*>` procedure takes byte values `b*`, each an integer from 0 to 255, and returns
  a binary parser. The parser matches exactly those bytes in order and returns a fresh
  list containing the matched bytes.
  |#
  (define-who <u8*> ($<u8*> who))

  #|proc:<bytes>
  The `<bytes>` procedure takes byte values `b*`, each an integer from 0 to 255, and
  returns a binary parser. The parser matches exactly those bytes in order and returns a
  fresh list containing the matched bytes.
  |#
  (define-who <bytes> ($<u8*> who))


  #|doc
  `c*` must be a string consisting of only ASCII characters.

  `<ascii>` takes an ASCII string `c*` and returns a parser that when invoked,
  will check whether the input bytes starting from the current position
  are equal to the integer values of the characters in `c*`. If equal, a newly
  allocated copy of `c*` is returned; otherwise the returned parser fails.
  |#
  (define-who (<ascii> c*)
    (pcheck ([string? c*])
            (string-for-each (lambda (c)
                               (unless (char<=? #\nul c #\delete)
                                 (errorf who "not a valid ascii character: ~a" c)))
                             c*)
            (lambda (inp state lvl)
              (let loop ([i 0])
                (if (fx= i (string-length c*))
                    (values #t (string-copy c*) inp)
	                    (let ([b (char->integer (string-ref c* i))])
	                      (if (peek-u8! inp b)
	                          (loop (fx1+ i))
                          (let ([failure (parser-failure-expected-at
                                          inp
                                          (list b)
                                          (current-found inp))])
                            (values #f #f failure)))))))))


  #|proc:<u8vec>
  The `<u8vec>` procedure takes natural number `n` and returns a binary parser.
  The parser consumes the next `n` bytes and returns them in a fresh bytevector.
  It is an error when fewer than `n` bytes remain in the input.
  |#
  (define-who (<u8vec> n)
    (pcheck ([natural? n])
            (lambda (inp state lvl)
              (let ([len (input-len inp)] [pos (input-pos inp)])
                ;; TODO how to report error?
                (if (> (+ pos n) len)
                    (values #f #f
                            (parser-failure-eof
                             inp
                             '(u8vec)
                             "unexpected EOF while reading u8vec"))
                    (let ([bv (make-bytevector n 0)] [data (binary-input-data inp)])
                      (bytevector-copy! data pos bv 0 n)
                      (input-pos-set! inp (+ pos n))
                      (values #t bv inp)))))))


  #|doc
  Parse an unsigned LEB128-encoded number.
  See https://en.wikipedia.org/wiki/LEB128.
  |#
  (define-who <uleb128>
    (lambda (inp state lvl)
      (let loop ([shift 0] [n 0])
        (let ([b (peek-u8 inp)])
          (if b
              (let* ([bits (fxlogand b #x7f)] [cont (fxsrl (fxlogand b #x80) 7)]
                     [n (+ n (ash bits shift))])
                (if (fx= cont 0)
                    (values #t n inp)
                    (loop (fx+ shift 7) n)))
              (values #f #f (parser-failure-eof inp '(uleb128) #f)))))))


  #|proc:<sleb128>
  The `<sleb128>` procedure takes positive fixnum bit length `len`.
  It returns a binary parser that reads a signed LEB128-encoded integer.
  The parser interprets the sign bit using `len` bits and returns the decoded integer.
  |#
  (define-who (<sleb128> len)
    (pcheck ([sleb128-len? len])
            (lambda (inp state lvl)
              (let loop ([shift 0] [n 0])
                (let ([b (peek-u8 inp)])
                  (if b
                      (let* ([bits (fxlogand b #x7f)] [cont (fxsrl (fxlogand b #x80) 7)]
                             [n (+ n (ash bits shift))])
                        (if (fx= cont 0)
                            (let ([res (if (and (fx< shift len) (logbit? 6 b))
                                           (logor n (ash -1 (fx+ 7 shift)))
                                           n)])
                              (values #t res inp))
                            (loop (fx+ shift 7) n)))
                      (values #f #f (parser-failure-eof inp '(sleb128) #f))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   combinators
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define ensure-progress
    (lambda (who inp0 inp1)
      (when (= (input-pos inp0) (input-pos inp1))
        (errorf who "parser succeeded without consuming input"))))


  #|proc:<many>
  The `<many>` procedure takes parser `p` and returns a parser that runs `p` repeatedly.
  The parser `p` must consume input whenever it succeeds; otherwise an error is raised.
  It returns a list of values produced by successful runs of `p`.
  If `p` fails before any successful run, the returned parser succeeds with `'()`.
  |#
  (define-who (<many> p)
    (pcheck ([parser? p])
            (lambda (inp state lvl)
              (let ([lb (make-list-builder)])
                (let loop ([inp inp] [old-inp (save-input inp)])
                  (let-values ([(stt val inp1) (parser-call p inp state (fx1+ lvl))])
                    (if stt
                        (begin (ensure-progress who old-inp inp1)
                               (lb val)
                               (loop inp1 (save-input inp1)))
                        (values #t (lb) old-inp))))))))


  #|proc:<some>
  The `<some>` procedure takes parser `p` and returns a parser that runs `p` one or more
  times. The parser `p` must consume input whenever it succeeds; otherwise an error is
  raised. It returns a list of values produced by successful runs of `p`.
  |#
  (define-who (<some> p)
    (pcheck ([parser? p])
            (lambda (inp state lvl)
              (let ([lb (make-list-builder)])
                ;; 1st
                (let ([old-inp (save-input inp)])
                  (let-values ([(stt val inp1) (parser-call p inp state (fx1+ lvl))])
                    (if stt
                        (begin (ensure-progress who old-inp inp1)
                               (lb val)
                               (let loop ([inp inp1] [old-inp (save-input inp1)])
                                 (let-values ([(stt val inp2)
                                               (parser-call p inp state (fx1+ lvl))])
                                   (if stt
                                       (begin (ensure-progress who old-inp inp2)
                                              (lb val)
                                              (loop inp2 (save-input inp2)))
                                       ;; need to backtrack when the last `p` fails
                                       (values #t (lb) old-inp)))))
                        (values #f #f (ensure-parser-failure inp1 inp)))))))))


  #|doc
  `p` must be a parser.

  `<optional>` takes a parser `p` and returns a parser that when invoked,
  will invoke `p` once.
  The returned parser succeeds no matter `p` succeeds or fails.
  If `p` succeeds, the parser value is returned;
  otherwise, `'()` is returned.

  `<optional>` is like the `?` operator in regular expression.
  |#
  (define-who (<optional> p)
    (lambda (inp state lvl)
      (let ([old-inp (save-input inp)])
        (let-values ([(stt val inp1) (parser-call p inp state (fx1+ lvl))])
          (if stt
              (values #t val inp1)
              (values #t '() old-inp))))))


  #|proc:<rep>
  The `<rep>` procedure takes parser `p` and natural number `n`, and returns a parser.
  The returned parser repeatedly invokes `p` exactly `n` times.
  If all runs of `p` succeed, it returns the `n` parse values in a list.
  If any run of `p` fails, the returned parser fails.
  |#
  (define-who (<rep> p n)
    (pcheck ([parser? p] [natural? n])
            (lambda (inp state lvl)
              (let ([lb (make-list-builder)])
                (let loop ([i 0] [inp1 inp])
                  (if (fx= i n)
                      (values #t (lb) inp1)
                      (let-values ([(stt val inp2) (parser-call p inp1 state (fx1+ lvl))])
                        (if stt
                            (begin (lb val)
                                   (loop (fx1+ i) inp2))
                            (values #f #f (ensure-parser-failure inp2 inp1))))))))))


  #|proc:<skip>
  The `<skip>` procedure takes parser `p` and natural number `n`, and returns a parser.
  The parser runs `p` exactly `n` times, ignores the values, and returns `'()`.
  It fails when any run of `p` fails.
  |#
  (define-who (<skip> p n)
    (pcheck ([parser? p] [natural? n])
            (lambda (inp state lvl)
              (let loop ([i 0] [inp1 inp])
                (if (fx= i n)
                    (values #t '() inp1)
                    (let-values ([(stt val inp2) (parser-call p inp1 state (fx1+ lvl))])
                      (if stt
                          (loop (fx1+ i) inp2)
                          (values #f #f (ensure-parser-failure inp2 inp1)))))))))


  #|proc:</>
  The `</>` procedure takes zero or more parsers and returns a left-biased choice parser.
  The returned parser tries each parser from left to right. If no parser is supplied, the
  returned parser always fails.
  |#
  (define (</> . p*)
    (pcheck ([all-parsers? p*])
            (lambda (inp state lvl)
              (if (null? p*)
                  (values #f #f (parser-failure-custom inp "empty choice"))
                  (let ([old-inp (save-input inp)])
                    (let loop ([p* p*] [failure* '()])
                      (if (null? p*)
                          (let* ([max-offset (fold-left
                                              (lambda (offset failure)
                                                (max offset (parser-failure-offset failure)))
                                              -1
                                              failure*)]
                                 [farthest (filter
                                            (lambda (failure)
                                              (= max-offset (parser-failure-offset failure)))
                                            failure*)])
                            (values #f #f (parser-failure-merge-choice (reverse farthest))))
                          (let-values ([(stt val inp1)
                                        (parser-call (car p*) (save-input old-inp)
                                                     state (fx1+ lvl))])
                            (if stt
                                (values #t val inp1)
                                (loop (cdr p*)
                                      (cons (ensure-parser-failure inp1 old-inp)
                                            failure*)))))))))))


  #|proc:<~>
  The `<~>` procedure takes zero or more parsers `p*` and returns a sequence parser.
  The returned parser runs each parser in order, returns a list of their values when all
  parsers succeed, and fails when any parser fails.
  |#
  (define-who (<~> . p*)
    (pcheck ([all-parsers? p*])
            (lambda (inp state lvl)
              (let ([lb (make-list-builder)])
                (let loop ([inp1 inp] [p* p*])
                  (if (null? p*)
                      (values #t (lb) inp1)
                      (let-values ([(stt val inp2)
                                    (parser-call (car p*) inp1 state (fx1+ lvl))])
                        (if stt
                            (begin (lb val)
                                   (loop inp2 (cdr p*)))
                            (values #f #f (ensure-parser-failure inp2 inp1))))))))))


  #|proc:<~n>
  The `<~n>` procedure takes natural number `n` and zero or more parsers `p*`.
  It returns a sequence parser like `<~>`, except success returns only the value from
  parser index `n`. It is an error when `n` is outside the supplied parser range.
  |#
  (define-who (<~n> n . p*)
    (pcheck ([natural? n] [all-parsers? p*])
            (lambda (inp state lvl)
              (let* ([p* p*] [len (length p*)] [v #f])
                (if (<= 0 n (fx1- len))
                    (let loop ([i 0] [p* p*] [inp inp])
                      (if (null? p*)
                          (values #t v inp)
                          (let-values ([(stt val inp1)
                                        (parser-call (car p*) inp state (fx1+ lvl))])
                            (if stt
                                (begin (when (fx= i n) (set! v val))
                                       (loop (fx1+ i) (cdr p*) inp1))
                                (values #f #f (ensure-parser-failure inp1 inp))))))
                    (errorf who "bad parser index ~a (must be between 0 and ~a)"
                            n (fx1- len)))))))


  #|proc:<map>
  The `<map>` procedure takes function `f` and parser `p`, and returns a parser.
  The function `f` must accept one parse value and return one value. When `p` succeeds,
  the returned parser applies `f` to the parse value and returns the result.
  |#
  (define-who (<map> f p)
    (pcheck ([procedure? f] [parser? p])
            (lambda (inp state lvl)
              (let-values ([(stt val inp1) (parser-call p inp state (fx1+ lvl))])
                (if stt
                    (values #t (f val) inp1)
                    (values #f #f (ensure-parser-failure inp1 inp)))))))


  #|proc:<map-st>
  The `<map-st>` procedure takes function `f` and parser `p`, and returns a parser.
  The function `f` must accept a parse value and the parser state, and return one value.
  When `p` succeeds, the returned parser applies `f` and returns the result.
  |#
  (define-who (<map-st> f p)
    (pcheck ([procedure? f] [parser? p])
            (lambda (inp state lvl)
              (let-values ([(stt val inp1) (parser-call p inp state (fx1+ lvl))])
                (if stt
                    (values #t (f val state) inp1)
                    (values #f #f (ensure-parser-failure inp1 inp)))))))


  #|proc:<bind>
  The `<bind>` procedure takes parser `p` and function `f`, and returns a parser.
  The function `f` must accept one parse value and return a parser. When `p` succeeds,
  the returned parser applies `f` to the parse value, then runs the parser from `f`.
  |#
  (define-who (<bind> p f)
    (pcheck ([parser? p] [procedure? f])
            (lambda (inp state lvl)
              (let-values ([(stt val inp) (parser-call p inp state (fx1+ lvl))])
                (if stt
                    (let ([p1 (f val)])
                      (parser-call p1 inp state (fx1+ lvl)))
                    (values #f #f (ensure-parser-failure inp inp)))))))


  #|proc:<bind-st>
  The `<bind-st>` procedure takes parser `p` and function `f`, and returns a parser.
  The function `f` must accept a parse value and the parser state, and return a parser.
  When `p` succeeds, the returned parser applies `f`, then runs the parser from `f`.
  |#
  (define-who (<bind-st> p f)
    (pcheck ([parser? p] [procedure? f])
            (lambda (inp state lvl)
              (let-values ([(stt val inp) (parser-call p inp state (fx1+ lvl))])
                (if stt
                    (let ([p1 (f val state)])
                      (parser-call p1 inp state (fx1+ lvl)))
                    (values #f #f (ensure-parser-failure inp inp)))))))


  #|proc:<followed-by>
  The `<followed-by>` procedure takes parsers `p0` and `p1`, and returns a parser.
  The returned parser runs `p0`, then checks that `p1` succeeds without consuming `p1`'s
  input. It returns the value from `p0` and fails when either parser fails.
  |#
  (define-who (<followed-by> p0 p1)
    (pcheck ([parser? p0 p1])
            (lambda (inp state lvl)
              (let-values ([(stt1 val1 inp1) (parser-call p0 inp state (fx1+ lvl))])
                (if stt1
                    (let ([old-input (save-input inp1)])
                      (let-values ([(stt2 val2 inp2)
                                    (parser-call p1 inp1 state (fx1+ lvl))])
                        (if stt2
                            (values #t val1 old-input)
                            (values #f #f (ensure-parser-failure inp2 inp1)))))
                    (values #f #f inp1))))))


  #|proc:<not-followed-by>
  The `<not-followed-by>` procedure takes parsers `p0` and `p1`, and returns a parser.
  The returned parser runs `p0`, then checks that `p1` fails without consuming `p1`'s
  input. It returns the value from `p0` and fails when `p0` fails or `p1` succeeds.
  |#
  (define-who (<not-followed-by> p0 p1)
    (pcheck ([parser? p0 p1])
            (lambda (inp state lvl)
              (let-values ([(stt1 val1 inp1) (parser-call p0 inp state (fx1+ lvl))])
                (if stt1
                    (let ([old-input (save-input inp1)])
                      (let-values ([(stt2 val2 inp2)
                                    (parser-call p1 inp1 state (fx1+ lvl))])
                        (if stt2
                            (values #f #f
                                    (parser-failure-custom old-input
                                                           "input was not expected"))
                            (values #t val1 old-input))))
                    (values #f #f inp1))))))


  #|doc
  `p0` and `p1` must be two parsers.

  `~>` takes two parsers and returns a parser that runs them sequentially.
  The returned parser succeeds when both `p0` and `p1` succeed and
  in this case, the parse value of the second parser is returned (as indicated by the arrow).
  The returned parser fails when either one of `p0` and `p1` fails.
  |#
  (define (~> p0 p1) (<~n> 1 p0 p1))


  #|doc
  `p0` and `p1` must be two parsers.

  `~>` takes two parsers and returns a parser that runs them sequentially.
  The returned parser succeeds when both `p0` and `p1` succeed and
  in this case, the parse value of the first parser is returned (as indicated by the arrow).
  The returned parser fails when either one of `p0` and `p1` fails.
  |#
  (define (<~ p0 p1) (<~n> 0 p0 p1))


  #|doc
  Convenient aliases that are the same as `<~>`, but only return the parse value
  of the N'th parser in each `<~N>`.
  |#
  (define <~0> (lambda p* (apply <~n> 0 p*)))
  (define <~1> (lambda p* (apply <~n> 1 p*)))
  (define <~2> (lambda p* (apply <~n> 2 p*)))
  (define <~3> (lambda p* (apply <~n> 3 p*)))
  (define <~4> (lambda p* (apply <~n> 4 p*)))
  (define <~5> (lambda p* (apply <~n> 5 p*)))


  #|doc
  `const` can be any value; `p` must be a parser.

  `<as>` takes a constant `const` and a parser `p`, and returns a parser
  that will always return the given constant when `p` succeeds.
  The returned parser fails when `p` fails.
  |#
  (define (<as> const p)
    (<map> (lambda (val) const) p))


  #|proc:<as-string>
  The `<as-string>` procedure takes parser `p` and returns a parser.
  When `p` succeeds, the returned parser converts `p`'s list of characters to a string
  and returns it.
  |#
  (define (<as-string> p)
    (pcheck ([parser? p])
            (<map> (lambda (val) (apply string val)) p)))


  #|proc:<as-symbol>
  The `<as-symbol>` procedure takes parser `p` and returns a parser.
  When `p` succeeds, the returned parser converts `p`'s list of characters to a symbol
  and returns it.
  |#
  (define (<as-symbol> p)
    (pcheck ([parser? p])
            (<map> (lambda (val) (string->symbol (apply string val))) p)))

  (define (<as-integer> p)
    (<map> (lambda (val)
             (if (null? val)
                 #f
                 (let-values ([(sign d*) (cond [(eq? #\+ (car val)) (values + (cdr val))]
                                               [(eq? #\- (car val)) (values - (cdr val))]
                                               [else                (values + val)])])
                   (sign (fold-left (lambda (s d) (+ (* s 10) (char->num d))) 0 d*)))))
           p))

  ;; TODO neg
  ;; TODO avoid building the list
  (define <nat>
    (<map> (lambda (val)
             (fold-left (lambda (s n) (+ (* 10 s) (char->num n))) 0 val))
           (<some> <digit>)))

  #|proc:<sep-by>
  The `<sep-by>` procedure takes parsers `p0` and `p1`, and returns a parser.
  The returned parser parses zero or more `p0` values separated by `p1` values.
  It returns a list of values from `p0` and ignores values from `p1`.
  |#
  (define (<sep-by> p0 p1)
    (pcheck ([parser? p0 p1])
            (<map> (lambda (val) (if (null? val) val (cons (car val) (cadr val))))
                   (<optional> (<~> p0 (<many> (~> p1 p0)))))))

  #|proc:<sep-by1>
  The `<sep-by1>` procedure takes parsers `p0` and `p1`, and returns a parser.
  The returned parser parses one or more `p0` values separated by `p1` values.
  It returns a list of values from `p0` and ignores values from `p1`.
  |#
  (define (<sep-by1> p0 p1)
    (pcheck ([parser? p0 p1])
            (<map> (lambda (val) (cons (car val) (cadr val)))
                   (<~> p0 (<many> (~> p1 p0))))))


  #|proc:<token>
  The `<token>` procedure takes parser `p` and returns a parser.
  The returned parser runs `p`, consumes any following whitespace, and returns the value
  from `p`. It fails when `p` fails.
  |#
  (define (<token> p)
    (pcheck ([parser? p])
            (<~ p (<many> <whitespace>))))


  #|proc:<fully>
  The `<fully>` procedure takes parser `p` and returns a parser.
  The returned parser consumes leading whitespace, runs `p`, consumes trailing
  whitespace, requires EOF, and returns the value from `p`.
  |#
  (define (<fully> p)
    (pcheck ([parser? p])
            (<~n> 1 (<many> <whitespace>) p (<many> <whitespace>) <eof>)))


  ;; TODO These can be placed in the front at o=3, but not at o=2.
  (define c->n
    (lambda (c)
      (if (ascii-digit? c)
          (fx- (char->integer c) 48)
          (errorf 'c->n "not an ASCII digit: ~a" c))))
  (define hexc->n
    (lambda (c)
      (cond [(char<=? #\a c #\f) (fx- (char->integer c) 87)]
            [(char<=? #\A c #\F) (fx- (char->integer c) 55)]
            [else (c->n c)])))

  #|doc
  Parse a binary digit and convert it to the corresponding number.
  E.g., #\0 -> 0, #\1 -> 1.
  |#
  (define <digit2> (<map> c->n <bindigit>))

  #|doc
  Parse a octal digit and convert it to the corresponding number.
  E.g., #\0 -> 0, #\1 -> 1, ..., #\7 -> 7.
  |#
  (define <digit8> (<map> c->n <octdigit>))

  #|doc
  Parse a decimal digit and convert it to the corresponding number.
  E.g., #\0 -> 0, #\1 -> 1, ..., #\9 -> 9.
  |#
  (define <digit10> (<map> c->n <digit>))

  #|doc
  Parse a hex digit and convert it to the corresponding number.
  E.g., #\0 -> 0, #\1 -> 1, ..., #\a -> 10, ..., #\f -> 15, #\A -> 10, ..., #\F -> 15.
  |#
  (define <digit16> (<map> hexc->n <hexdigit>))

  #|doc
  Parse an upper hex digit and convert it to the corresponding number.
  E.g., #\0 -> 0, #\1 -> 1, ..., #\A -> 10, ..., #\F -> 15.
  |#
  (define <upper-digit16> (<map> hexc->n <upper-hexdigit>))

  #|doc
  Parse a lower hex digit and convert it to the corresponding number.
  E.g., #\0 -> 0, #\1 -> 1, ..., #\a -> 10, ..., #\f -> 15.
  |#
  (define <lower-digit16> (<map> hexc->n <lower-hexdigit>))



  )
