(import (chezpp)
        (chezpp parser json5))

(define json5-value
  (lambda (text)
    (json5-document-value (parse-json5 text))))

(mat parse-json5-records

     (string=? "#[json5-member name: \"answer\" value: 42]"
               (format "~s"
                       (vector-ref
                        (json5-object-members
                         (json5-document-value (parse-json5 "{answer: 42}")))
                        0)))

     (json5-null? (json5-value "null"))

     (let ([value (json5-value "{answer: 42}")])
       (and (json5-object? value)
            (= 1 (vector-length (json5-object-members value)))
            (let ([member (vector-ref (json5-object-members value) 0)])
              (and (json5-member? member)
                   (string=? "answer" (json5-member-name member))
                   (= 42 (json5-member-value member))))))

     (let ([value (json5-value "[1, true, 'three']")])
       (and (json5-array? value)
            (equal? '(1 #t "three")
                    (vector->list (json5-array-elements value)))))

     )

(mat parse-json5-lexical

     (= 1 (json5-value "\t\v\f\r\n\xA0;\x2028;\x2029;\xFEFF;1"))

     (= 1 (json5-value "/* before */ 1 // after"))

     ;; error: a block comment must terminate.
     (error? (parse-json5 "/* unterminated"))

     (json5-object?
      (json5-value "{$value: 1, _x: 2, cafe\x301;: 3, null: 4}"))

     (string=? "escaped"
               (json5-member-name
                (vector-ref
                 (json5-object-members (json5-value "{\\u0065scaped: 1}"))
                 0)))

     ;; error: an identifier name cannot start with an ASCII digit.
     (error? (parse-json5 "{1name: true}"))

     ;; error: a literal token cannot be followed by an identifier character.
     (error? (parse-json5 "trueValue"))

     ;; error: Infinity must be a complete numeric token.
     (error? (parse-json5 "Infinityx"))

     )

(mat parse-json5-strings

     (string=? "\x27;\x22;\\\x8;\xC;\n\r\t\xB;\x0;"
               (json5-value "'\\\'\\\"\\\\\\b\\f\\n\\r\\t\\v\\0'"))

     (string=? "AC/DC" (json5-value "'\\A\\C\\/\\D\\C'"))

     (string=? "line continuation"
               (json5-value "'line \\\r\ncontinuation'"))

     (and (string=? "ab" (json5-value "'a\\\nb'"))
          (string=? "ab" (json5-value "'a\\\rb'"))
          (string=? "ab" (json5-value "'a\\\r\nb'"))
          (string=? "ab" (json5-value "'a\\\x2028;b'"))
          (string=? "ab" (json5-value "'a\\\x2029;b'")))

     (string=? "\x1F3BC;" (json5-value "'\\uD83C\\uDFBC'"))

     ;; error: a zero escape cannot be followed by a decimal digit.
     (error? (parse-json5 "'\\01'"))

     ;; error: a high surrogate escape must be followed by a low surrogate escape.
     (error? (parse-json5 "'\\uD83C'"))

     )

(mat parse-json5-numbers

     (and (= 0 (json5-value "0"))
          (= 125.0 (json5-value "125."))
          (= 0.125 (json5-value ".125"))
          (= 0.01 (json5-value "0.1e-1"))
          (= 12500 (json5-value "125e2"))
          (= #xdecaf (json5-value "+0xdecaf"))
          (= #x-123 (json5-value "-0X123"))
          (infinite? (json5-value "-Infinity"))
          (nan? (json5-value "-NaN")))

     ;; error: hexadecimal syntax requires at least one digit.
     (error? (parse-json5 "0x"))

     ;; error: an exponent requires at least one digit.
     (error? (parse-json5 "1e+"))

     )

(mat parse-json5-structures

     (let ([object (json5-value "{}")]
           [array (json5-value "[]")])
       (and (json5-object? object)
            (= 0 (vector-length (json5-object-members object)))
            (json5-array? array)
            (= 0 (vector-length (json5-array-elements array)))))

     (let ([value
            (json5-value
             "{/*a*/key/*b*/:/*c*/[/*d*/1/*e*/,/*f*/2/*g*/,/*h*/]/*i*/,/*j*/}")])
       (and (json5-object? value)
            (= 1 (vector-length (json5-object-members value)))
            (let ([array (json5-member-value
                          (vector-ref (json5-object-members value) 0))])
              (and (json5-array? array)
                   (equal? '(1 2) (vector->list (json5-array-elements array)))))))

     (let ([value (json5-value "{a: 1, a: 2, nested: {items: [true, null,],},}")])
       (let ([members (json5-object-members value)])
         (and (= 3 (vector-length members))
              (string=? "a" (json5-member-name (vector-ref members 0)))
              (= 1 (json5-member-value (vector-ref members 0)))
              (string=? "a" (json5-member-name (vector-ref members 1)))
              (= 2 (json5-member-value (vector-ref members 1)))
              (json5-object? (json5-member-value (vector-ref members 2))))))

     ;; error: arrays permit at most one trailing comma.
     (error? (parse-json5 "[1,,]"))

     ;; error: objects permit at most one trailing comma.
     (error? (parse-json5 "{a: 1,,}"))

     )

(mat parse-json5-files

     (json5-document? (parse-json5-file "data/json5-example.json5"))

     (json5-document? (parse-json5-file "data/uiua-primitives.json"))

     )
