(import (chezpp)
        (chezpp parser combinator)
        (chezpp parser csv)
        (chezpp parser json5)
        (chezpp parser xml)

        (chezpp parser wasm)
        (chezpp parser jclass)
        (chezpp parser elf))

(define runT
  (case-lambda
    [(p in)    (runT p in #f)]
    [(p in st) (run-textual-parser p in st)]))

(define runB
  (case-lambda
    [(p in)    (runB p in #f)]
    [(p in st) (run-binary-parser p in st)]))

(define u8vec bytevector)


(mat common-prims

     (equal? #\a (runT (<result> #\a) "a"))
     (equal? #\a (runT (<result> #\a) ""))

     (equal? #\a (runT <item> "a"))
     (equal? #\a (runT <item> "ab"))
     (error? (runT <item> ""))

     ;; TODO <fail> <pos> <msg> ...

     )


(mat text-prims

     (equal? #\a (runT (<char> #\a) "a"))
     (error? (runT (<char> #\a) "b"))

     (error? (runT <letter> ""))
     (equal? #\a (runT <letter> "a"))

     (error? (runT <upper> "a"))
     (equal? #\A (runT <upper> "A"))

     (equal? #\0 (runT <digit> "0"))
     (error?  (runT <digit> "a"))

     (begin (define 0-9  (iota 10))
            (define 0-7  (iota 8))
            (define 0-15 (iota 16))

            (define 0-9-str (map (lambda (n) (make-string 1 (integer->char n))) (nums 48 58)))
            (define 0-7-str (map (lambda (n) (make-string 1 (integer->char n))) (nums 48 56)))
            (define a-f-str (map (lambda (n) (make-string 1 (integer->char n))) (nums 97 103)))
            (define A-F-str (map (lambda (n) (make-string 1 (integer->char n))) (nums 65 71)))
            #t)

     (andmap (lambda (s)
               (equal? (string-ref s 0) (runT <digit> s)))
             0-9-str)
     (andmap (lambda (s)
               (equal? (string-ref s 0) (runT <bindigit> s)))
             '("0" "1"))
     (andmap (lambda (s)
               (equal? (string-ref s 0) (runT <octdigit> s)))
             0-7-str)
     (andmap (lambda (s)
               (equal? (string-ref s 0) (runT <hexdigit> s)))
             (append 0-9-str a-f-str A-F-str))
     (andmap (lambda (s)
               (equal? (string-ref s 0) (runT <lower-hexdigit> s)))
             (append 0-9-str a-f-str))
     (andmap (lambda (s)
               (equal? (string-ref s 0) (runT <upper-hexdigit> s)))
             (append 0-9-str A-F-str))

     (andmap (lambda (n s)
               (equal? n (runT <digit2> s)))
             '(0 1) '("0" "1"))
     (andmap (lambda (n s)
               (equal? n (runT <digit8> s)))
             0-7 0-7-str)
     (andmap (lambda (n s)
               (equal? n (runT <digit10> s)))
             0-9 0-9-str)
     (andmap (lambda (n s)
               (equal? n (runT <digit16> s)))
             (append 0-15 (nums 10 16)) (append 0-9-str a-f-str A-F-str))
     (andmap (lambda (n s)
               (equal? n (runT <lower-digit16> s)))
             0-15 (append 0-9-str a-f-str))
     (andmap (lambda (n s)
               (equal? n (runT <upper-digit16> s)))
             0-15 (append 0-9-str A-F-str))

     (error? (runT <bindigit> "3"))
     (error? (runT <octdigit> "8"))
     (error? (runT <hexdigit> "g"))
     (error? (runT <upper-hexdigit> "a"))
     (error? (runT <lower-hexdigit> "A"))
     (error? (runT <digit2> "3"))
     (error? (runT <digit8> "8"))
     (error? (runT <digit10> "a"))
     (error? (runT <upper-digit16> "a"))
     (error? (runT <lower-digit16> "A"))


     (equal? #\a (runT (<one-of> "abc") "a"))
     (equal? #\b (runT (<one-of> "abc") "b"))
     ;; error
     (error? (runT (<one-of> "abc") "d"))

     (equal? #\d (runT (<none-of> "abc") "d"))
     ;; error
     (error? (runT (<none-of> "abc") "b"))


     )


(mat bin-prims

     (= #xff (runB (<uimm8> #xff) (u8vec #xff)))
     (= #xfeef (runB (<uimm16> #xfeef) (u8vec #xef #xfe)))

     (= 1 (runB (</> (<uimm8> 1) (<uimm8> 2) (<uimm8> 3)) (u8vec 1)))
     (= 2 (runB (</> (<uimm8> 1) (<uimm8> 2) (<uimm8> 3)) (u8vec 2)))
     (= 3 (runB (</> (<uimm8> 1) (<uimm8> 2) (<uimm8> 3)) (u8vec 3)))
     (error? (runB (</> (<uimm8> 1) (<uimm8> 2) (<uimm8> 3)) (u8vec 4)))

     (equal? '(1 64)
             (runB (</> (<~> (<uimm8> 1) <u8>)
                        (<~> (<uimm8> 2) <u8>)
                        (<~> (<uimm8> 3) <u8>))
                   (u8vec 1 64)))
     ;; error
     (error? (runB (</> (<~> (<uimm8> 1) <u8>)
                     (<~> (<uimm8> 2) <u8>)
                     (<~> (<uimm8> 3) <u8>))
                (u8vec 4 64)))

     (equal? '(1 2 3)
             (runB (<~> (<uimm8> 1) (<uimm8> 2) (<uimm8> 3))
                   (u8vec 1 2 3)))
     ;; error
     (error? (runB (<~> (<uimm8> 1) (<uimm8> 2) (<uimm8> 3))
                (u8vec 1 3)))
     (error? (runB (<~> (<uimm8> 1) (<uimm8> 2) (<uimm8> 3))
                (u8vec 2 2 3)))

     (equal? '((3 33) (1 11) (2 22))
             (runB (<rep> (</> (<~> (<uimm8> 1) <u8>)
                               (<~> (<uimm8> 2) <u8>)
                               (<~> (<uimm8> 3) <u8>))
                          3)
                   (u8vec 3 33 1 11 2 22)))
     (equal? '((3 33) (1 11))
             (runB (<rep> (</> (<~> (<uimm8> 1) <u8>)
                               (<~> (<uimm8> 2) <u8>)
                               (<~> (<uimm8> 3) <u8>))
                          2)
                   (u8vec 3 33 1 11 2 22)))
     ;; error
     (error? (runB (<rep> (</> (<~> (<uimm8> 1) <u8>)
                        (<~> (<uimm8> 2) <u8>)
                        (<~> (<uimm8> 3) <u8>))
                   4)
            (u8vec 3 33 1 11 2 22)))

     (equal? '(6 6 6 6)
             (runB (<bind> (<uimm8> 4)
                           (lambda (n)
                             (<rep> (<uimm8> 6) n)))
                   (u8vec 4 6 6 6 6)))

     ;; m:u8 n:u8 m*{n}
     (equal? '(1)
             (runB (<bind> (<~> <u8> <u8>)
                           (lambda (n*)
                             (<rep> (<uimm8> (car n*)) (cadr n*))))
                   (u8vec 1 1 1)))
     (equal? '(1 1 1)
             (runB (<bind> (<~> <u8> <u8>)
                           (lambda (n*)
                             (<rep> (<uimm8> (car n*)) (cadr n*))))
                   (u8vec 1 3 1 1 1)))
     (equal? '(42 42 42 42 42 42)
             (runB (<bind> (<~> <u8> <u8>)
                           (lambda (n*)
                             (<rep> (<uimm8> (car n*)) (cadr n*))))
                   (u8vec 42 6 42 42 42 42 42 42)))


     (= #xfeca (runB <u16>   (u8vec #xca #xfe)))
     (= #xfeca (runB <u16le> (u8vec #xca #xfe)))
     (= #xcafe (runB <u16be> (u8vec #xca #xfe)))

     (= #xbebafeca (runB <u32>   (u8vec #xca #xfe #xba #xbe)))
     (= #xbebafeca (runB <u32le> (u8vec #xca #xfe #xba #xbe)))
     (= #xcafebabe (runB <u32be> (u8vec #xca #xfe #xba #xbe)))

     (= #xefbeaddebebafeca (runB <u64>   (u8vec #xca #xfe #xba #xbe #xde #xad #xbe #xef)))
     (= #xefbeaddebebafeca (runB <u64le> (u8vec #xca #xfe #xba #xbe #xde #xad #xbe #xef)))
     (= #xcafebabedeadbeef (runB <u64be> (u8vec #xca #xfe #xba #xbe #xde #xad #xbe #xef)))

     ;; TODO signed
     ;; TODO imm

     (begin
       ;; ChezScheme mat.ss
       (define *fuzz* 1e-6)
       (define fl~=
         (lambda (x y)
           (cond
            [(and (fl>= (flabs x) 2.0) (fl>= (flabs y) 2.0))
             (fl~= (fl/ x 2.0) (fl/ y 2.0))]
            [(and (fl< 0.0 (flabs x) 1.0) (fl< 0.0 (flabs y) 1.0))
             (fl~= (fl* x 2.0) (fl* y 2.0))]
            [else (let ([d (flabs (fl- x y))])
                    (or (fl<= d *fuzz*)
                        (begin (printf "fl~~=: ~s~%" d) #f)))])))
       #t)

     ;; https://www.h-schmidt.net/FloatConverter/IEEE754.html
;;;; f32
     (= +0.0   (runB <f32le> (u8vec 0 0 0 0)))
     (= -0.0   (runB <f32le> (u8vec 0 0 0 #x80)))
     (= +inf.0 (runB <f32le> (u8vec 0 0 #x80 #x7f)))
     (= -inf.0 (runB <f32le> (u8vec 0 0 #x80 #xff)))
     (nan?     (runB <f32le> (u8vec 0 0 #xc0 #x7f)))

     (fl~= 3.14  (runB <f32le> (u8vec #xc3 #xf5 #x48 #x40)))
     (fl~= 2.718 (runB <f32le> (u8vec #xb6 #xf3 #x2d #x40)))
     (fl~= 1.618 (runB <f32le> (u8vec #xa0 #x1a #xcf #x3f)))
     (fl~= 1.414 (runB <f32le> (u8vec #xf4 #xfd #xb4 #x3f)))  ;; sqrt(2)
     (fl~= 0.69  (runB <f32le> (u8vec #xd7 #xa3 #x30 #x3f)))  ;; ln(2)
     (fl~= 2.3   (runB <f32le> (u8vec #x33 #x33 #x13 #x40)))  ;; ln(10)
     ;; precise nums
     (= 0.125 (runB <f32le> (u8vec #x00 #x00 #x00 #x3e)))
     (= 0.5   (runB <f32le> (u8vec #x00 #x00 #x00 #x3f)))
     (= 0.75  (runB <f32le> (u8vec #x00 #x00 #x40 #x3f)))
     (= 1.0   (runB <f32le> (u8vec #x00 #x00 #x80 #x3f)))
     (= 2.25  (runB <f32le> (u8vec #x00 #x00 #x10 #x40)))
     (= 3.75  (runB <f32le> (u8vec #x00 #x00 #x70 #x40)))

     (= +0.0   (runB <f32be> (u8vec 0 0 0 0)))
     (= -0.0   (runB <f32be> (u8vec #x80 0 0 0)))
     (= +inf.0 (runB <f32be> (u8vec #x7f #x80 0 0)))
     (= -inf.0 (runB <f32be> (u8vec #xff #x80 0 0)))
     (nan?     (runB <f32be> (u8vec #x7f #xc0 0 0)))

     (fl~= 3.14  (runB <f32be> (u8vec #x40 #x48 #xf5 #xc3)))
     (fl~= 2.718 (runB <f32be> (u8vec #x40 #x2d #xf3 #xb6)))
     (fl~= 1.618 (runB <f32be> (u8vec #x3f #xcf #x1a #xa0)))
     (fl~= 1.414 (runB <f32be> (u8vec #x3f #xb4 #xfd #xf4)))  ;; sqrt(2)
     (fl~= 0.69  (runB <f32be> (u8vec #x3f #x30 #xa3 #xd7)))  ;; ln(2)
     (fl~= 2.3   (runB <f32be> (u8vec #x40 #x13 #x33 #x33)))  ;; ln(10)
     ;; precise nums
     (= 0.125 (runB <f32be> (u8vec #x3e #x00 #x00 #x00)))
     (= 0.5   (runB <f32be> (u8vec #x3f #x00 #x00 #x00)))
     (= 0.75  (runB <f32be> (u8vec #x3f #x40 #x00 #x00)))
     (= 1.0   (runB <f32be> (u8vec #x3f #x80 #x00 #x00)))
     (= 2.25  (runB <f32be> (u8vec #x40 #x10 #x00 #x00)))
     (= 3.75  (runB <f32be> (u8vec #x40 #x70 #x00 #x00)))

;;;; f64
     (= +0.0   (runB <f64le> (u8vec 0 0 0 0 0 0 0 0)))
     (= -0.0   (runB <f64le> (u8vec #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x80)))
     (= +inf.0 (runB <f64le> (u8vec #x00 #x00 #x00 #x00 #x00 #x00 #xF0 #x7F)))
     (= -inf.0 (runB <f64le> (u8vec #x00 #x00 #x00 #x00 #x00 #x00 #xF0 #xFF)))
     (nan?     (runB <f64le> (u8vec #x00 #x00 #x00 #x00 #x00 #x00 #xF8 #x7F)))

     (= 3.14  (runB <f64le> (u8vec #x1F #x85 #xEB #x51 #xB8 #x1E #x09 #x40)))
     (= 2.718 (runB <f64le> (u8vec #x58 #x39 #xB4 #xC8 #x76 #xBE #x05 #x40)))
     (= 1.618 (runB <f64le> (u8vec #x17 #xD9 #xCE #xF7 #x53 #xE3 #xF9 #x3F)))
     (= 1.414 (runB <f64le> (u8vec #x39 #xB4 #xC8 #x76 #xBE #x9F #xF6 #x3F)))  ;; sqrt(2)
     (= 0.69  (runB <f64le> (u8vec #x14 #xAE #x47 #xE1 #x7A #x14 #xE6 #x3F)))  ;; ln(2)
     (= 2.3   (runB <f64le> (u8vec #x66 #x66 #x66 #x66 #x66 #x66 #x02 #x40)))  ;; ln(10)

     (= +0.0   (runB <f64be> (u8vec 0 0 0 0 0 0 0 0)))
     (= -0.0   (runB <f64be> (u8vec #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))
     (= +inf.0 (runB <f64be> (u8vec #x7F #xF0 #x00 #x00 #x00 #x00 #x00 #x00)))
     (= -inf.0 (runB <f64be> (u8vec #xFF #xF0 #x00 #x00 #x00 #x00 #x00 #x00)))
     (nan?     (runB <f64be> (u8vec #x7F #xF8 #x00 #x00 #x00 #x00 #x00 #x00)))

     (= 3.14  (runB <f64be> (u8vec #x40 #x09 #x1E #xB8 #x51 #xEB #x85 #x1F)))
     (= 2.718 (runB <f64be> (u8vec #x40 #x05 #xBE #x76 #xC8 #xB4 #x39 #x58)))
     (= 1.618 (runB <f64be> (u8vec #x3F #xF9 #xE3 #x53 #xF7 #xCE #xD9 #x17)))
     (= 1.414 (runB <f64be> (u8vec #x3F #xF6 #x9F #xBE #x76 #xC8 #xB4 #x39)))  ;; sqrt(2)
     (= 0.69  (runB <f64be> (u8vec #x3F #xE6 #x14 #x7A #xE1 #x47 #xAE #x14)))  ;; ln(2)
     (= 2.3   (runB <f64be> (u8vec #x40 #x02 #x66 #x66 #x66 #x66 #x66 #x66)))  ;; ln(10)

;;;; f32imm
     (bool (runB (<fimm32le> 0.125) (u8vec #x00 #x00 #x00 #x3e)))
     (bool (runB (<fimm32le> 0.5)   (u8vec #x00 #x00 #x00 #x3f)))
     (bool (runB (<fimm32le> 0.75)  (u8vec #x00 #x00 #x40 #x3f)))
     (bool (runB (<fimm32le> 1.0)   (u8vec #x00 #x00 #x80 #x3f)))
     (bool (runB (<fimm32le> 2.25)  (u8vec #x00 #x00 #x10 #x40)))
     (bool (runB (<fimm32le> 3.75)  (u8vec #x00 #x00 #x70 #x40)))

     (bool (runB (<fimm32be> 0.125) (u8vec #x3e #x00 #x00 #x00)))
     (bool (runB (<fimm32be> 0.5)   (u8vec #x3f #x00 #x00 #x00)))
     (bool (runB (<fimm32be> 0.75)  (u8vec #x3f #x40 #x00 #x00)))
     (bool (runB (<fimm32be> 1.0)   (u8vec #x3f #x80 #x00 #x00)))
     (bool (runB (<fimm32be> 2.25)  (u8vec #x40 #x10 #x00 #x00)))
     (bool (runB (<fimm32be> 3.75)  (u8vec #x40 #x70 #x00 #x00)))

;;;; f64imm
     (bool (runB (<fimm64le> 3.14)  (u8vec #x1F #x85 #xEB #x51 #xB8 #x1E #x09 #x40)))
     (bool (runB (<fimm64le> 2.718) (u8vec #x58 #x39 #xB4 #xC8 #x76 #xBE #x05 #x40)))
     (bool (runB (<fimm64le> 1.618) (u8vec #x17 #xD9 #xCE #xF7 #x53 #xE3 #xF9 #x3F)))
     (bool (runB (<fimm64le> 1.414) (u8vec #x39 #xB4 #xC8 #x76 #xBE #x9F #xF6 #x3F)))  ;; sqrt(2)
     (bool (runB (<fimm64le> 0.69)  (u8vec #x14 #xAE #x47 #xE1 #x7A #x14 #xE6 #x3F)))  ;; ln(2)
     (bool (runB (<fimm64le> 2.3)   (u8vec #x66 #x66 #x66 #x66 #x66 #x66 #x02 #x40)))  ;; ln(10)

     (bool (runB (<fimm64be> 3.14)  (u8vec #x40 #x09 #x1E #xB8 #x51 #xEB #x85 #x1F)))
     (bool (runB (<fimm64be> 2.718) (u8vec #x40 #x05 #xBE #x76 #xC8 #xB4 #x39 #x58)))
     (bool (runB (<fimm64be> 1.618) (u8vec #x3F #xF9 #xE3 #x53 #xF7 #xCE #xD9 #x17)))
     (bool (runB (<fimm64be> 1.414) (u8vec #x3F #xF6 #x9F #xBE #x76 #xC8 #xB4 #x39)))  ;; sqrt(2)
     (bool (runB (<fimm64be> 0.69)  (u8vec #x3F #xE6 #x14 #x7A #xE1 #x47 #xAE #x14)))  ;; ln(2)
     (bool (runB (<fimm64be> 2.3)   (u8vec #x40 #x02 #x66 #x66 #x66 #x66 #x66 #x66)))  ;; ln(10)

     ;; LEB128
     (begin (define encode-uleb128
              (lambda (n)
                (let loop ([res '()] [shift 0] [n n])
                  (let ([bits (logand n #x7f)]
                        [val  (ash n -7)])
                    (if (= val 0)
                        (reverse (cons (logbit0 7 bits) res))
                        (loop (cons (logbit1 7 bits) res) (fx+ shift 7) val))))))

            (define encode-sleb128
              (lambda (len n)
                (let loop ([res '()] [shift 0] [n n])
                  (let ([bits (logand n #x7f)]
                        [val  (ash n -7)])
                    (if (or (and (= val 0) (not (logbit? 6 bits)))
                            (and (= val -1) (logbit? 6 bits)))
                        (reverse (cons bits res))
                        (loop (cons (logbit1 7 bits) res)
                              (fx+ shift 7)
                              val))))))

            (define decode-uleb128
              (lambda (b*)
                (let loop ([b* b*] [shift 0] [n 0])
                  (let* ([b (car b*)]
                         [bits (fxlogand b #x7f)]
                         [cont (fxsrl (logand b #x80) 7)]
                         [n (+ n (ash bits shift))])
                    (if (fx= cont 0)
                        n
                        (loop (cdr b*) (fx+ shift 7) n))))))

            (define decode-sleb128
              (lambda (len b*)
                (let loop ([b* b*] [shift 0] [n 0])
                  (let* ([b (car b*)]
                         [bits (fxlogand b #x7f)]
                         [cont (fxsrl (logand b #x80) 7)]
                         [n (+ n (ash bits shift))])
                    (if (fx= cont 0)
                        (if (and (fx< shift len) (logbit? 6 b))
                            (logor n (ash -1 (fx+ 7 shift)))
                            n)
                        (loop (cdr b*) (fx+ shift 7) n))))))
            #t)

     (define-syntax test-leb128
       (syntax-rules ()
         [(_ bits)
          (and (andmap (lambda (n)
                         (= n (decode-uleb128 (encode-uleb128 n))))
                       (random-list (lambda () (random (expt 2 bits))) 20 21))
               ;; signed, +
               (andmap (lambda (n)
                         (= n (decode-sleb128 bits (encode-sleb128 bits n))))
                       (random-list (lambda () (random (expt 2 bits))) 20 21))
               ;; signed, -
               (andmap (lambda (n)
                         (let ([n (- n)])
                           (= n (decode-sleb128 bits (encode-sleb128 bits n)))))
                       (random-list (lambda () (random (expt 2 bits))) 20 21)))]))

     (test-leb128 32)
     (test-leb128 64)
     (test-leb128 128)

     (define-syntax test-<leb128>
       (syntax-rules ()
         [(_ bits)
          ;; (n ...) -> ((lebn ...) ...) -> (lebn ... ...) -> #vu8(...)
          (and (let* ([n* (random-list (lambda () (random (expt 2 bits))) 20 21)]
                      [bv (apply bytevector
                                 (apply append
                                        (map (lambda (n)
                                               (encode-uleb128 n))
                                             n*)))]
                      [res (equal? n*
                                   (runB (<rep> <uleb128> 20) bv))])
                 (println res)
                 res)
               (let* ([n* (random-list (lambda () (random (expt 2 bits))) 20 21)]
                      [bv (apply bytevector
                                 (apply append
                                        (map (lambda (n)
                                               (encode-sleb128 bits n))
                                             n*)))]
                      [res (equal? n*
                                   (runB (<rep> (<sleb128> bits) 20) bv))])
                 (println res)
                 res)
               (let* ([n* (random-list (lambda () (- (random (expt 2 bits)))) 20 21)]
                      [bv (apply bytevector
                                 (apply append
                                        (map (lambda (n)
                                               (encode-sleb128 bits n))
                                             n*)))]
                      [res (equal? n*
                                   (runB (<rep> (<sleb128> bits) 20) bv))])
                 (println res)
                 res))]))

     (test-<leb128> 32)
     (test-<leb128> 64)
     (test-<leb128> 128)

     ;; u8*
     (equal? '() (runB (<u8*>) (u8vec 1 2 3)))
     (equal? '(1 2)
             (runB (<u8*> 1 2) (u8vec 1 2 3)))
     (equal? '(1 2 3)
             (runB (<u8*> 1 2 3) (u8vec 1 2 3)))
     (error? (runB (<u8*> 1 2 3) (u8vec 1 3 3)))

     ;; TODO errors

     )


(mat parser-combinators

     (equal? '()
             (runT (<many> (<char> #\a)) ""))
     (equal? '(#\a #\a #\a)
             (runT (<many> (<char> #\a)) "aaa"))
     (equal? '(#\a #\a #\a)
             (runT (<many> (<char> #\a)) "aaabbc"))

     (equal? '()
             (runT (<many> <item>) ""))
     (equal? '(#\a #\b #\c)
             (runT (<many> <item>) "abc"))

     ;; error
     (error? (runT (<some> (<char> #\a)) ""))

     (equal? '(#\a)
             (runT (<some> (<char> #\a)) "a"))
     (equal? '(#\a #\a #\a)
             (runT (<some> (<char> #\a)) "aaa"))
     (equal? '(#\a #\a #\a)
             (runT (<some> (<char> #\a)) "aaabbc"))

     (error? (runT (<some> <item>) ""))
     (equal? '(#\a #\b #\c)
             (runT (<some> <item>) "abc"))


     (equal? #\A (runT (</> <letter> <digit> <whitespace>) "A"))
     (equal? #\0 (runT (</> <letter> <digit> <whitespace>) "0"))
     (equal? #\space (runT (</> <letter> <digit> <whitespace>) " "))
     ;; error
     (error? (runT (</> <letter> <digit> <whitespace>) "!"))

     (equal? '(#\d #\e #\f #\1 #\2 #\3 #\a #\b #\c)
             (runT (<many> (</> <digit> <letter>)) "def123abc"))
     (equal? '(#\d #\e #\f #\1 #\2 #\3 #\a #\b #\c)
             (runT (<many> (</> <letter> <digit>)) "def123abc"))
     (equal? '((#\d #\e #\f) (#\1 #\2 #\3) (#\a #\b #\c))
             (runT (<many> (</> (<some> <digit>) (<some> <letter>))) "def123abc"))

     (equal? '(#\A #\9 #\space)
             (runT (<~> <letter> <digit> <whitespace>) "A9 "))
     ;; error
     (error? (runT (<~> <letter> <digit> <whitespace>) " A9 "))

     (equal? '(#\space #\A #\9 #\space)
             (runT (<~> <whitespace> <letter> (</> <digit> <upper>) <whitespace>)
                   " A9 "))
     (equal? '(#\space #\A #\U #\space)
             (runT (<~> <whitespace> <letter> (</> <digit> <upper>) <whitespace>)
                   " AU "))
     ;; error
     (error? (runT (<~> <whitespace> <letter> (</> <digit> <upper>) <whitespace>)
                   " Aa "))

     (equal? '((#\a #\b #\c) (#\space #\space))
             (runT (<~> (<some> <letter>) (<many> <whitespace>)) "abc  "))

     (equal? '(#\( #\( #\() (runT (<rep> (<char> #\() 3) "((("))
     (equal? '(#\) #\) #\)) (runT (<rep> (<char> #\)) 3) ")))"))
     (equal? '(#\a #\a #\a) (runT (<rep> (<char> #\a) 3) "aaaa"))
     ;; error
     (error? (runT (<rep> (<char> #\a) 3) "aa"))

     (equal? #\a (runT (<~n> 0 <letter> <digit> <letter>) "a1c"))
     (equal? #\1 (runT (<~n> 1 <letter> <digit> <letter>) "a1c"))
     (equal? #\c (runT (<~n> 2 <letter> <digit> <letter>) "a1c"))
     ;; error
     (error? (runT (<~n> 0 <letter> <digit> <letter>) "a12"))
     (error? (runT (<~n> 2 <letter> <digit> <letter>) "a12"))
     (error? (runT (<~n> -1 <letter> <digit> <letter>) "a1c"))
     (error? (runT (<~n> 3 <letter> <digit> <letter>) "a1c"))

     (equal? #\c (runT (</> (<~n> 2 <letter> <digit> <letter>) (<~n> 2 <letter> <digit> <digit>)) "a1c"))
     (equal? #\2 (runT (</> (<~n> 2 <letter> <digit> <letter>) (<~n> 2 <letter> <digit> <digit>)) "a12"))

     ;; TODO <map>


     (begin (define <word>
              (<~> (<map>
                    (lambda (val) (apply string val))
                    (<some> <letter>))
                   (<many> <whitespace>)))
            (define <words>
              (<~> (<many> <whitespace>) (<many> <word>)))
            #t)

     (equal? '(() (("this" (#\space))
                   ("is" (#\space))
                   ("a" (#\space))
                   ("sentence" ())))
             (runT <words> "this is a sentence"))
     (equal? '(() (("this" (#\space))
                   ("is" (#\space))
                   ("a" (#\space))
                   ("sentence" (#\space #\space))))
             (runT <words> "this is a sentence  "))
     (equal? '((#\space #\space)
               (("this" (#\space))
                ("is" (#\space))
                ("a" (#\space))
                ("sentence" ())))
             (runT <words> "  this is a sentence"))
     (equal? '((#\space #\space)
               (("this" (#\space #\space))
                ("is" (#\space #\space #\space))
                ("a" (#\space #\space #\space))
                ("sentence" (#\space #\space #\space))))
             (runT <words> "  this  is   a   sentence   "))

     (begin (define <anbncn>
              (<bind> (<some> (<char> #\a))
                      (lambda (val)
                        (let ([count (length val)])
                          (<~> (<result> val)
                               (<rep> (<char> #\b) count)
                               (<rep> (<char> #\c) count))))))
            #t)
     (equal? '((#\a) (#\b) (#\c))
             (runT <anbncn> "abc"))
     (equal? '((#\a #\a) (#\b #\b) (#\c #\c))
             (runT <anbncn> "aabbcc"))
     (equal? '((#\a #\a #\a) (#\b #\b #\b) (#\c #\c #\c))
             (runT <anbncn> "aaabbbccc"))
     ;; still ok
     (equal? '((#\a) (#\b) (#\c))
             (runT <anbncn> "abcc"))
     ;; error
     (error? (runT <anbncn> "aabc"))
     (error? (runT <anbncn> "abbc"))

     (begin (define (<onion> c)
              (<bind> (<some> (<char> #\())
                      (lambda (val)
                        (let ([count (length val)])
                          (<~n> 1
                                (<result> val)
                                (<char> c)
                                (<rep> (<char> #\)) count))))))
            #t)

     (equal? #\x1f496 (runT (<onion> #\x1f496) "(((\x1f496;)))"))
     (equal? #\a (runT (<onion> #\a) "(((a)))"))
     ;; still ok
     (equal? #\a (runT (<onion> #\a) "((a)))"))
     ;; error
     (error? (runT (<~> (<onion> #\a) <eof>) "((a)))"))
     (error? (runT (<onion> #\a) "(((a))"))


     (begin (define <lines>
              (<map> (lambda (val)
                       (map (lambda (c*) (apply string c*)) val))
                     (<sep-by1> (<some> <letter>) (<char> #\newline))))
            #t)

     (equal? '("aa" "bb")
             (runT <lines> "aa\nbb"))
     (equal? '("aa" "bb")
             (runT <lines> "aa\nbb\n"))
     (equal? '("aa" "bb" "cc" "dd" "ff")
             (runT <lines> "aa\nbb\ncc\ndd\nff"))
     ;; error: no content
     (error? (runT <lines> ""))
     (error? (runT <lines> "\n"))
     (error? (runT <lines> "\n\n\n"))

     ;; TODO states <map-st> <bind-st>

     ;; TODO [not]-followed-by

     )
