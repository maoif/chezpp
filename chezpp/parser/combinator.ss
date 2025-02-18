(library (chezpp parser combinator)
  (export parser-error?
          run-textual-parser run-binary-parser
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
    make-parser-error parser-error?)

  (define raise-parser-error
    (lambda (who perr)
      (raise-continuable (condition (make-parser-error) (make-who-condition who) (make-message-condition perr)))))


  (define $run-textual-parser
    (lambda (who p in state)
      (pcheck ([procedure? p] [string? in])
              (let-values ([(stt val inp/err)
                            (p (make-textual-input (string-length in) 0 in 0 0) state 0)])
                (if stt val (raise-parser-error who inp/err))))))


  (define $run-binary-parser
    (lambda (who p in state)
      (pcheck ([procedure? p] [bytevector? in])
              (let-values ([(stt val inp/err)
                            (p (make-binary-input (bytevector-length in) 0 in) state 0)])
                (if stt val (raise-parser-error who inp/err))))))

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
       (pcheck ([procedure? p] [string? in])
               ($run-textual-parser who p in state))]))


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
       (pcheck ([procedure? p] [bytevector? in])
               ($run-binary-parser who p in state))]))


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
       (pcheck ([procedure? p] [file-regular? path])
               (let ([in (read-string path)])
                 ($run-textual-parser who p in state)))]))


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
       (pcheck ([procedure? p] [file-regular? path])
               (let ([in (read-u8vec path)])
                 ($run-binary-parser who p in state)))]))


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



  (define-record-type lazy-parser
    (fields (mutable proc)))

  ;; used internally for defining parser implementations

  (define-syntax parser-call
    (lambda (stx)
      (syntax-case stx ()
        [(k p args ...)
         #'(let ([pp p])
             (if (lazy-parser? pp)
                 ((lazy-parser-proc pp) args ...)
                 (pp args ...)))])))


  (define-syntax declare-lazy-parser
    (lambda (stx)
      (syntax-case stx ()
        [(k p)
         (identifier? #'p)
         #'(define p (make-lazy-parser (lambda args (errorf 'p "parser code not defined"))))])))


  (define-syntax define-lazy-parser
    (lambda (stx)
      (syntax-case stx ()
        [(k p e)
         (identifier? #'p)
         #'(define dummy
             (let ([body e])
               (if (lazy-parser? p)
                   (if (procedure? body)
                       (lazy-parser-proc-set! p body)
                       (errorf 'k "not a parser procedure: ~a" body))
                   (errorf 'k "not a declared lazy parser: ~a" p))
               #t))])))


;;;; input logic

  (define-record-type input
    (fields (immutable len)
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
         (input-pos inp)
         (textual-input-str inp)
         (textual-input-line inp)
         (textual-input-col inp))]
       [(binary-input? inp)
        (make-binary-input
         (input-len inp)
         (input-pos inp)
         (binary-input-data inp))]
       [else (assert-unreachable)])))

  (define-who advance!
    (case-lambda
      [(inp) (advance! inp 1)]
      [(inp n)
       (let ([len (input-len inp)] [pos (input-pos inp)])
         (if (= len pos)
             (errorf who "already at eof")
             (input-pos-set! inp (if (>= (+ pos n) len)
                                     len
                                     (+ pos n)))))]))

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
  (gen-peek! peek-u64le! bytevector-u64-ref 8 (endianness little))
  (gen-peek! peek-s16le! bytevector-s16-ref 2 (endianness little))
  (gen-peek! peek-s32le! bytevector-s32-ref 4 (endianness little))
  (gen-peek! peek-s64le! bytevector-s64-ref 8 (endianness little))
  (gen-fpeek! peek-f32le! bytevector-ieee-single-ref 4 (endianness little))
  (gen-fpeek! peek-f64le! bytevector-ieee-double-ref 8 (endianness little))

  (gen-peek! peek-u16be! bytevector-u16-ref 2 (endianness big))
  (gen-peek! peek-u32be! bytevector-u32-ref 4 (endianness big))
  (gen-peek! peek-u64be! bytevector-u64-ref 8 (endianness big))
  (gen-peek! peek-s16be! bytevector-s16-ref 2 (endianness big))
  (gen-peek! peek-s32be! bytevector-s32-ref 4 (endianness big))
  (gen-peek! peek-s64be! bytevector-s64-ref 8 (endianness big))
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
      (values #f #f (format "~a: failed" who))))


  #|doc
  `<fail-with>` takes a message value `msg` (usually a string), and returns a parser
  that always fails with the given message value when invoked.
  |#
  (define-who (<fail-with> msg)
    (lambda (inp state lvl)
      (values #f #f (format "~a: ~a" who msg))))


  #|doc
  `<eof>` is a parser that tries to match the end of the input, be it textual or binary.
  |#
  (define <eof>
    (lambda (inp state lvl)
      (let ([len (input-len inp)] [pos (input-pos inp)])
        (if (= len pos)
            (values #t #t inp)
            (values #f #f (format "not EOF: ~a/~a" pos len))))))


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
                   (values #f #f  msg))
               (values #f #f inp1))))]))


  #|doc
  Return the current parse position in the input.
  |#
  (define-who <pos>
    (lambda (inp state lvl)
      (values #t (input-pos inp) inp)))


  #|doc
  `n` must be a natural number; `p` must be a parser.

  `<pos-at>` takes a natural number `n` and a parser `p` as input, and returns
  a parser that will temporarily set the current input position to `n` and call
  `p` from there. If `p` succeeds, its parse value is returned and the input position
  is restored to where it was before the returned parser is called.
  The returned parser fails if `n` is greater than or equal to the input length, or
  when `p` fails.
  |#
  (define-who (<pos-at> n p)
    (lambda (inp state lvl)
      ;; TODO error report
      (when (or (>= n (input-len inp)) (< n 0))
        (errorf who "invaid position ~a, should be between ~a and ~a" n 0 (input-len inp)))
      (let ([new-inp (save-input inp)])
        (printf "~a: ~a~n" who n)
        (input-pos-set! new-inp n)
        (let-values ([(stt val inp1) (parser-call p new-inp state (fx1+ lvl))])
          (if stt
              (values #t val inp)
              (values #f #f (format "failed ~a: ~a" who inp1)))))))


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
           (println msg)
           (values #f msg inp)))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   textual primitives
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define-who char->num
    (lambda (c)
      (if (char<=? #\0 c #\9)
          (fx- (char->integer c) 48)
          (errorf who "not a digit: ~a" c))))

  #|doc
  Parse and return the current character unconditionally.
  `<item>` fails only if EOF is reached.
  |#
  (define <item>
    (lambda (inp state lvl)
      (if (end-of-input? inp)
          (values #f #f "reached eof")
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

  #|doc
  `c` must be a character.

  `<char>` takes a character `c` and returns a parser that parses `c` specifically.
  That is, the returned parser succeeds when the current character is `c`,
  and fails otherwise.
  When successful, `c` is returned.
  |#
  (define-who (<char> c)
    (lambda (inp state lvl)
      (if (peek-char! inp c)
          (values #t c inp)
          (values #f #f (format "~a: expected ~a" who c)))))

  #|doc
  `str` must be a string.

  `<string>` takes a string `str` and returns a parser that parses `str` specifically.
  That is, the returned parser succeeds when there is a string equal to `str` in the input.
  In this case, a newly allocated string equal to `str` is returned.
  |#
  (define-who (<string> str)
    (lambda (inp state lvl)
      (if (peek-string! inp str)
          (values #t (string-copy str) inp)
          (values #f #f (format "~a: expected ~a" who str)))))

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
    (<satisfy-char> char-numeric? "not a digit"))

  #|doc
  Parse a binary digit and return the corresponding character.
  |#
  (define <bindigit>
    (<satisfy-char> (lambda (x) (or (char=? x #\0) (char=? x #\1)))))

  #|doc
  Parse a hex digit and return the corresponding character.
  |#
  (define <hexdigit>
    (<satisfy-char> (lambda (x) (or (char-numeric? x)
                                    (char<=? #\a x #\f)
                                    (char<=? #\A x #\F)))))

  #|doc
  Parse a lower hex digit and return the corresponding character.
  |#
  (define <lower-hexdigit>
    (<satisfy-char> (lambda (x) (or (char-numeric? x)
                                    (char<=? #\a x #\f)))))

  #|doc
  Parse an upper hex digit and return the corresponding character.
  |#
  (define <upper-hexdigit>
    (<satisfy-char> (lambda (x) (or (char-numeric? x)
                                    (char<=? #\A x #\F)))))

  #|doc
  Parse a octal digit and return the corresponding character.
  |#
  (define <octdigit>
    (<satisfy-char> (lambda (x) (char<=? #\0 x #\7))))

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


  (define-syntax gen-bin-prim
    (syntax-rules ()
      [(_ name peek)
       (define-who name
         (lambda (inp state lvl)
           (let ([v (peek inp)])
             (if v
                 (values #t v inp)
                 (values #f #f (format "~a: failed to peek" who))))))]))
  (define-syntax gen-bin-imm-prim
    (syntax-rules ()
      [(_ name peek! valid-x?)
       (define-who (name x)
         (unless (valid-x? x)
           (errorf who "invalid number: ~a" x))
         (lambda (inp state lvl)
           (let ([v (peek! inp x)])
             (if v
                 (values #t x inp)
                 (values #f #f (format "~a: expected ~a" who x))))))]))
  ;; TODO rethink the ranges
  (define u8?  (lambda (x) (<= 0 x (sub1 (expt 2 8)))))
  (define u16? (lambda (x) (<= 0 x (sub1 (expt 2 16)))))
  (define u32? (lambda (x) (<= 0 x (sub1 (expt 2 32)))))
  (define u64? (lambda (x) (<= 0 x (sub1 (expt 2 64)))))
  (define s8?  (lambda (x) (<= (- (expt 2 7))  x (sub1 (expt 2 7)))))
  (define s16? (lambda (x) (<= (- (expt 2 15)) x (sub1 (expt 2 15)))))
  (define s32? (lambda (x) (<= (- (expt 2 31)) x (sub1 (expt 2 31)))))
  (define s64? (lambda (x) (<= (- (expt 2 63)) x (sub1 (expt 2 63)))))
  (define f32? (lambda (x) (<= (- (expt 2 31)) x (sub1 (expt 2 31)))))
  (define f64? (lambda (x) (<= (- (expt 2 63)) x (sub1 (expt 2 63)))))

  ;; TODO maybe merge the two gen macros
  ;; TODO rename these since they also change the inp state
  ;; try match and return an arbirary value
  (gen-bin-prim <u8>  peek-u8)
  (gen-bin-prim <u16> peek-u16le)
  (gen-bin-prim <u32> peek-u32le)
  (gen-bin-prim <u64> peek-u64le)
  (gen-bin-prim <s8>  peek-s8)
  (gen-bin-prim <s16> peek-s16le)
  (gen-bin-prim <s32> peek-s32le)
  (gen-bin-prim <s64> peek-s64le)
  (gen-bin-prim <f32> peek-f32le)
  (gen-bin-prim <f64> peek-f64le)

  (gen-bin-prim <u16le> peek-u16le)
  (gen-bin-prim <u32le> peek-u32le)
  (gen-bin-prim <u64le> peek-u64le)
  (gen-bin-prim <s16le> peek-s16le)
  (gen-bin-prim <s32le> peek-s32le)
  (gen-bin-prim <s64le> peek-s64le)
  (gen-bin-prim <f32le> peek-f32le)
  (gen-bin-prim <f64le> peek-f64le)

  (gen-bin-prim <u16be> peek-u16be)
  (gen-bin-prim <u32be> peek-u32be)
  (gen-bin-prim <u64be> peek-u64be)
  (gen-bin-prim <s16be> peek-s16be)
  (gen-bin-prim <s32be> peek-s32be)
  (gen-bin-prim <s64be> peek-s64be)
  (gen-bin-prim <f32be> peek-f32be)
  (gen-bin-prim <f64be> peek-f64be)

  ;; try match a given immediate value
  (gen-bin-imm-prim <uimm8>  peek-u8!    u8?)
  (gen-bin-imm-prim <uimm16> peek-u16le! u16?)
  (gen-bin-imm-prim <uimm32> peek-u32le! u32?)
  (gen-bin-imm-prim <uimm64> peek-u64le! u64?)
  (gen-bin-imm-prim <simm8>  peek-s8!    s8?)
  (gen-bin-imm-prim <simm16> peek-s16le! s16?)
  (gen-bin-imm-prim <simm32> peek-s32le! s32?)
  (gen-bin-imm-prim <simm64> peek-s64le! s64?)
  (gen-bin-imm-prim <fimm32> peek-f32le! f32?)
  (gen-bin-imm-prim <fimm64> peek-f64le! f64?)

  (gen-bin-imm-prim <uimm16le> peek-u16le! u16?)
  (gen-bin-imm-prim <uimm32le> peek-u32le! u32?)
  (gen-bin-imm-prim <uimm64le> peek-u64le! u64?)
  (gen-bin-imm-prim <simm16le> peek-s16le! s16?)
  (gen-bin-imm-prim <simm32le> peek-s32le! s32?)
  (gen-bin-imm-prim <simm64le> peek-s64le! s64?)
  (gen-bin-imm-prim <fimm32le> peek-f32le! f32?)
  (gen-bin-imm-prim <fimm64le> peek-f64le! f64?)

  (gen-bin-imm-prim <uimm16be> peek-u16be! u16?)
  (gen-bin-imm-prim <uimm32be> peek-u32be! u32?)
  (gen-bin-imm-prim <uimm64be> peek-u64be! u64?)
  (gen-bin-imm-prim <simm16be> peek-s16be! s16?)
  (gen-bin-imm-prim <simm32be> peek-s32be! s32?)
  (gen-bin-imm-prim <simm64be> peek-s64be! s64?)
  (gen-bin-imm-prim <fimm32be> peek-f32be! f32?)
  (gen-bin-imm-prim <fimm64be> peek-f64be! f64?)


  ;; return a copy of the list of bytes if successful
  (define ($<u8*> who)
    (lambda b*
      (for-each (lambda (x) (unless (<= 0 x 255)
                              (errorf who "not a valid byte: ~a" x)))
                b*)
      (lambda (inp state lvl)
        (let loop ([u8* b*])
          (if (null? u8*)
              (values #t (list-copy b*) inp)
              (if (peek-u8! inp (car u8*))
                  (loop (cdr u8*))
                  (values #f #f (format "~a: expected ~a" who (car u8*)))))))))

  #|doc
  `<u8*>` takes a single list of byte values (fixnums between 0 and 255 inclusive)
  and returns a binary parser that tries to parse exactly these bytes in sequence
  in the input. If the parser succeeds, a copy of the given list of bytes is returned,
  |#
  (define-who <u8*> ($<u8*> who))

  #|doc
  `<bytes>` is an alias of `<u8*>`.
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
                          (values #f #f (format "~a: expected ~a (~a)" who (string-ref c* i) b)))))))))


  #|doc
  `n` must be a natural number.

  `<u8vec>` takes a natural number `n` as input and returns a parser that when invoked,
  will parse the next `n` bytes and return them in a bytevector.
  It is an error if the remaining bytes in the input are less than `n` bytes.
  |#
  (define-who (<u8vec> n)
    (lambda (inp state lvl)
      (let ([len (input-len inp)] [pos (input-pos inp)])
        ;; TODO how to report error?
        (when (< n 0) (errorf who "invalid count: ~a" n))
        (when (> (+ pos n) len) (errorf who "count is too long: ~a + ~a > ~a" pos n len))
        (let ([bv (make-bytevector n 0)] [data (binary-input-data inp)])
          (bytevector-copy! data pos bv 0 n)
          (input-pos-set! inp (+ pos n))
          (values #t bv inp)))))


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
              (values #f #f (format "~a: failed to read next byte" who)))))))


  #|doc
  Return a parser that parses a signed LEB128-encoded number that is `len` bits long.
  |#
  (define-who (<sleb128> len)
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
              (values #f #f (format "~a: failed to read next byte" who)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   combinators
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  #|doc
  `p` must be a parser.

  `<many>` takes a parser `p` and returns a parser that when invoked,
  will run `p` as many times as possible, until it fails.
  The last failure is ignored and the parser values of previous successful runs are
  collected into a list and returned.
  If `p` fails the first time it is run, `'()` is returned.
  This means the parser returned by `<many>` never fails.

  `<many>` is like the `*` operator in regular expression.
  |#
  (define-who (<many> p)
    (lambda (inp state lvl)
      (let ([lb (make-list-builder)])
        (let loop ([inp inp] [old-inp (save-input inp)])
          (let-values ([(stt val inp1) (parser-call p inp state (fx1+ lvl))])
            (if stt
                (begin (lb val)
                       (loop inp1 (save-input inp1)))
                (values #t (lb) old-inp)))))))


  #|doc
  `p` must be a parser.

  `<some>` is similar to `<many>`, with the difference that `p` must succeed at least once
  in the parser returned by `<some>`.
  Parser values of successful runs of `p` are collected into a list and returned.

  `<some>` is like the `+` operator in regular expression.
  |#
  (define-who (<some> p)
    (lambda (inp state lvl)
      (let ([lb (make-list-builder)])
        ;; 1st
        (let-values ([(stt val inp1) (parser-call p inp state (fx1+ lvl))])
          (if stt
              (begin (lb val)
                     (let loop ([inp inp1] [old-inp (save-input inp1)])
                       (let-values ([(stt val inp2) (parser-call p inp state (fx1+ lvl))])
                         (if stt
                             (begin (lb val)
                                    (loop inp2 (save-input inp2)))
                             ;; need to backtrack when the last `p` fails
                             (values #t (lb) old-inp)))))
              (values #f #f (format "failed ~a: ~a" who inp1)))))))


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


  #|doc
  `p` must be a parser; `n` must be a natural number.

  `<rep>` takes a parser `p` and a natural number `n` as input, and returns
  a parser that repeatedly invokes `p` `n` times.
  If all runs of `p` succeed, the `n` parse values are returned in a list in order.
  If any attempt of `p` fails, the returned parser fails.
  |#
  (define-who (<rep> p n)
    (pcheck ([procedure? p] [natural? n])
            (lambda (inp state lvl)
              (let ([lb (make-list-builder)])
                (let loop ([i 0] [inp1 inp])
                  (if (fx= i n)
                      (values #t (lb) inp1)
                      (let-values ([(stt val inp2) (parser-call p inp1 state (fx1+ lvl))])
                        (if stt
                            (begin (lb val)
                                   (loop (fx1+ i) inp2))
                            (values #f #f (format "failed ~a: ~a" who inp2))))))))))


  #|doc
  `p` must be a parser; `n` must be a natural number.

  `<skip>` takes a parser `p` and a natural number `n` as input, and returns
  a parser that repeatedly invokes `p` `n` times.
  If all runs of `p` succeed, the `n` parse values are ignored and `'()` is returned.
  If any attempt of `p` fails, the returned parser fails.
  |#
  (define-who (<skip> p n)
    (lambda (inp state lvl)
      (let loop ([i 0] [inp1 inp])
        (if (fx= i n)
            (values #t '() inp1)
            (let-values ([(stt val inp2) (parser-call p inp1 state (fx1+ lvl))])
              (if stt
                  (loop (fx1+ i) inp2)
                  (values #f #f (format "failed ~a: ~a" who inp2))))))))


  #|doc
  `p*` must be a (possibly empty) list of parsers.

  `</>` takes a list of parsers as input, and returns a parser that when invoked,
  will try every parser in `p*` one by one, from left to right.
  The returned parser succeeds when one of the parser succeeds and its parse value
  is returned. The returned parser fails when all parsers are tried but none of them
  succeed.

  `</>` is like the `|` operator in regular expression, but implements left-biased choice.
  |#
  (define (</> . p*)
    (lambda (inp state lvl)
      (if (null? p*)
          (values #t '() inp)
          (let ([old-inp (save-input inp)])
            (let loop ([p* p*] [err #f])
              (if (null? p*)
                  (values #f #f (format "failed </>: ~a" err))
                  (let-values ([(stt val inp1) (parser-call (car p*) (save-input old-inp) state (fx1+ lvl))])
                    (if stt
                        (values #t val inp1)
                        (loop (cdr p*) inp1)))))))))


  #|doc
  `p*` must be a (possibly empty) list of parsers.

  `<~>` takes a list of parsers as input, and returns a parser that when invoked,
  will run all parsers in `p*` in sequence.
  The returned parser succeeds when all parsers in `p*` succeed and their parse values
  are returned in a list. The returned parser fails when any one of the parsers fails.
  |#
  (define-who (<~> . p*)
    (lambda (inp state lvl)
      (let ([lb (make-list-builder)])
        (let loop ([inp1 inp] [p* p*])
          (if (null? p*)
              (values #t (lb) inp1)
              (let-values ([(stt val inp2) (parser-call (car p*) inp1 state (fx1+ lvl))])
                (if stt
                    (begin (lb val)
                           (loop inp2 (cdr p*)))
                    (values #f #f (format "failed ~a: ~a" who inp2)))))))))


  #|doc
  `p*` must be a (possibly empty) list of parsers;
  `n` must be a number between 0 and length of `p*` - 1, inclusive.

  `<~n>` takes a natural number `n` and a list of parsers `p*` as input,
  and returns a parser that behaves just like the parser returned by `<~>`,
  except that when the returned parser succeeds, only the parse value of the
  `n`th parser in `p*` is returned.
  |#
  (define-who (<~n> n . p*)
    (lambda (inp state lvl)
      (let* ([p* p*] [len (length p*)] [v #f])
        (if (<= 0 n (fx1- len))
            (let loop ([i 0] [p* p*] [inp inp])
              (if (null? p*)
                  (values #t v inp)
                  (let-values ([(stt val inp1) (parser-call (car p*) inp state (fx1+ lvl))])
                    (if stt
                        (begin (when (fx= i n) (set! v val))
                               (loop (fx1+ i) (cdr p*) inp1))
                        (values #f #f (format "failed ~a: ~a" who inp1))))))
            (errorf who "bad parser index ~a (must be between 0 and ~a)" n (fx1- len))))))


  #|doc
  `p` must be a parser; `f` must be a unary function that returns one value.

  `<map>` takes a semantic function `f` and a parser `p` as input, and returns a parser
  that when invoked, will invoke `p`. If `p` succeeds, `f` is applied to its parse value,
  and `f`'s return value is returned.
  The returned parser fails if `p` fails. Its behavior is undefined if `f` fails in any way.
  |#
  (define-who (<map> f p)
    (lambda (inp state lvl)
      (let-values ([(stt val inp1) (parser-call p inp state (fx1+ lvl))])
        (if stt
            (values #t (f val) inp1)
            (values #f #f (format "failed ~a: ~a" who inp1))))))


  #|doc
  `p` must be a parser; `f` must be a binary function that returns one value.

  `<map-st>` is similar to `<map>`, with the difference that `<map-st>`'s first argument
  must be a binary function that will receive both the parse value of `p` (if it succeeds)
  and the implicit state value that is created when the parse action begins.
  That's why there's "-st" in the procedure's name (meaning "state").
  This PC is useful when implementing stateful parsing.
  |#
  (define-who (<map-st> f p)
    (lambda (inp state lvl)
      (let-values ([(stt val inp1) (parser-call p inp state (fx1+ lvl))])
        (if stt
            (values #t (f val state) inp1)
            (values #f #f (format "failed ~a: ~a" who inp1))))))


  #|doc
  `p` must be a parser; `f` must be a unary function that returns one value,
  and the value must be a parser.

  `<bind>` takes a parser `p` and a unary function `f` as input and returns a
  parser that will first run `p`, obtain its parse value, then apply `f` to the
  parse value to obtain a new parser, and run the new parser.
  In other words, the parse value of `p` is fed to `f` to obtain a new parser to run.
  The parser created by `<bind>` fails when either `p` fails or when the parser returned
  by `f` fails. If `f` fails in any way, the parse behavior is undefined.

  `<bind>` is useful for implementing some forms of context-sensitive parsing
  where the next parse action depends on the current parse value. In fact,
  this is just the monadic bind operation `>>=` as can be found in, e.g., Haskell.
  |#
  (define-who (<bind> p f)
    (lambda (inp state lvl)
      (let-values ([(stt val inp) (parser-call p inp state (fx1+ lvl))])
        (if stt
            ((f val) inp state (fx1+ lvl))
            (values #f #f (format "failed ~a: ~a" who inp))))))


  #|doc
  `p` must be a parser; `f` must be a binary function that returns one value,
  and the value must be a parser.

  `<bind-st>` is to `<bind>` what `<map-st>` is to `<map>`, that is, the second
  argument of `<bind-st>` is also a function that takes two arguments, the first being
  the parse value of `p`, the second being the implicit state value.
  The rest is the same as in `<bind>`.
  |#
  (define-who (<bind-st> p f)
    (lambda (inp state lvl)
      (let-values ([(stt val inp) (parser-call p inp state (fx1+ lvl))])
        (if stt
            ((f val state) inp state (fx1+ lvl))
            (values #f #f (format "failed ~a: ~a" who inp))))))


  #|doc
  `p0` and `p1` must be parsers.

  `<followed-by>` takes two parsers `p0` and `p1` as input and returns a parser that
  runs `p0` and `p1` in sequence and succeeds when both `p0` and `p1` succeeds.
  If the returned parser succeeds, the value of `p0` is returned and the postion is set to where it
  was before `p1` was run.

  `<followed-by>` fails when either `p0` or `p1` fails.
  |#
  (define-who (<followed-by> p0 p1)
    (lambda (inp state lvl)
      (let-values ([(stt1 val1 inp1) (parser-call p0 inp state (fx1+ lvl))])
        (if stt1
            (let ([old-input (save-input inp1)])
              (let-values ([(stt2 val2 inp2) (parser-call p1 inp1 state (fx1+ lvl))])
                (if stt2
                    (values #t val1 old-input)
                    (values #f #f (format "failed ~a: second parser fails" who)))))
            (values #f #f inp1)))))


  #|doc
  `p0` and `p1` must be parsers.

  `<not-followed-by>`  takes two parsers `p0` and `p1` as input and returns a parser that
  runs `p0` and `p1` in sequence and succeeds when `p0` succeeds and `p1` *fails*.
  If the returned parser succeeds, the value of `p0` is returned and the postion is set to where it
  was before `p1` was run.

  `<not-followed-by>` fails when `p0` fails or when `p1` succeeds.
  |#
  (define-who (<not-followed-by> p0 p1)
    (lambda (inp state lvl)
      (let-values ([(stt1 val1 inp1) (parser-call p0 inp state (fx1+ lvl))])
        (if stt1
            (let ([old-input (save-input inp1)])
              (let-values ([(stt2 val2 inp2) (parser-call p1 inp1 state (fx1+ lvl))])
                (if stt2
                    (values #f #f (format "failed ~a: second parser succeeds" who))
                    (values #t val1 old-input))))
            (values #f #f inp1)))))


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


  #|doc
  `p` must be a parser.

  `<as-string>` takes a parser `p`, and returns a parser that when invoked,
  will invoke `p`. When `p` succeeds, its parse value, which must be a list of characters,
  will be converted to a string and returned.
  The returned parser fails either when `p` fails, or when `p` succeeds but its parse value
  cannot be converted to a string.
  |#
  (define (<as-string> p)
    (<map> (lambda (val) (apply string val)) p))


  #|doc
  `p` must be a parser.

  `<as-symbol>` takes a parser `p`, and returns a parser that when invoked,
  will invoke `p`. When `p` succeeds, its parse value, which must be a list of characters,
  will be converted to a symbol and returned.
  The returned parser fails either when `p` fails, or when `p` succeeds but its parse value
  cannot be converted to a symbol.
  |#
  (define (<as-symbol> p)
    (<map> (lambda (val) (string->symbol (apply string val))) p))

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

  ;; TODO reshape the result
  (define (<sep-by> p0 p1)
    (<map> (lambda (val) (if (null? val) val (cons (car val) (cadr val))))
           (<optional> (<~> p0 (<many> (~> p1 p0))))))

  ;; succeed if text parsed by `p0` is separated by text parsed by `p1`,
  ;; `p0` must succeed once
  (define (<sep-by1> p0 p1)
    (<map> (lambda (val) (cons (car val) (cadr val)))
           (<~> p0 (<many> (~> p1 p0)))))


  #|doc
  `p` must be a parser.

  `<token>` takes a parser `p` and returns a parser that when invoked will
  first invoke `p`, and if `p` succeeds, will also remove all whitespace
  characters up to the next non-whitespace character.
  Then the parser value of `p` is returned.
  The returned parser fails if `p` fails.

  This PC adds the tokenizer behavior to the given parser to automatically
  ignore whitespaces between tokens.
  |#
  (define (<token> p)
    (<~ p (<many> <whitespace>)))


  #|doc
  `p` must be a parser.

  `<fully>` takes a parser `p` and returns a parser that will parse the entire
  input use `p`, that is, whitespaces on both ends of the input are removed,
  and when `p` succeeds, it must have reached EOF.
  The returned parser fails when either
  1) `p` fails or,
  2) `p` succeeds but EOF is not reached.
  |#
  (define (<fully> p)
    (<~n> 1 (<many> <whitespace>) p (<many> <whitespace>) <eof>))


  ;; TODO These can be placed in the front at o=3, but not at o=2.
  (define c->n (lambda (c) (fx- (char->integer c) 48)))
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
