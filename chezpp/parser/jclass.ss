(library (chezpp parser jclass)
  (export parse-java-class-file)
  (import (chezpp chez)
          (chezpp parser combinator)
          (chezpp internal)
          (chezpp list)
          (chezpp io)
          (chezpp vector)
          (chezpp utils))

  (define parser-java-class
    (let ()
      (define-who get-string-in-constant-pool
        (lambda (cp i)
          ;;(printf "~a: ~a~n" who i)
          ;; "The constant_pool table is indexed from 1 to constant_pool_count - 1."
          (let ([item (vector-ref cp (fx1- i))])
            (unless (eq? 'CONSTANT-utf8 (car item))
              (errorf who "not a utf8 constant (~a) at index ~a" item i))
            (cadr item))))
      (define-who <utf8-string>
        (<map-st> (lambda (i st)
                    (let ([cp (hashtable-ref st 'constant-pool #f)])
                      (if cp
                          (get-string-in-constant-pool cp i)
                          (errorf who "failed to read constant pool"))))
                  <u16be>))

      (define <magic> (<uimm32be> #xCAFEBABE))
      (define <major-v>     <u16be>)
      (define <minor-v>     <u16be>)
      (define <acc-flags>   <u16be>)
      (define <this-class>  <u16be>)
      (define <super-class> <u16be>)

      (define <constant-pool-count> <u16be>)
      (define <interfaces-count>    <u16be>)
      (define <fields-count>        <u16be>)
      (define <methods-count>       <u16be>)
      (define <attrs-count>         <u16be>)

      (define CONSTANT-class                7)
      (define CONSTANT-field-ref            9)
      (define CONSTANT-method-ref           10)
      (define CONSTANT-interface-method-ref 11)
      (define CONSTANT-string               8)
      (define CONSTANT-integer              3)
      (define CONSTANT-float                4)
      (define CONSTANT-long                 5)
      (define CONSTANT-double               6)
      (define CONSTANT-name-and-type        12)
      (define CONSTANT-utf8                 1)
      (define CONSTANT-method-handle        15)
      (define CONSTANT-method-type          16)
      (define CONSTANT-dynamic              17)
      (define CONSTANT-invoke-dynamic       18)
      (define CONSTANT-module               19)
      (define CONSTANT-package              20)

      ;; indexed from 1 to length-1
      ;; TODO turn indices into values?
      ;; TODO add <if>, <cond> and <case> ?
      (define-who <cp-info>
        (</> (<~> (<as> 'CONSTANT-class (<uimm8> CONSTANT-class))
                  ;; name-index
                  <u16be>)
             (<~> (<as> 'CONSTANT-field-ref (<uimm8> CONSTANT-field-ref))
                  ;; class-index
                  <u16be>
                  ;; name-and-type-index
                  <u16be>)
             (<~> (<as> 'CONSTANT-method-ref (<uimm8> CONSTANT-method-ref))
                  ;; class-index
                  <u16be>
                  ;; name-and-type-index
                  <u16be>)
             (<~> (<as> 'CONSTANT-interface-method-ref (<uimm8> CONSTANT-interface-method-ref))
                  ;; class-index
                  <u16be>
                  ;; name-and-type-index
                  <u16be>)
             (<~> (<as> 'CONSTANT-string (<uimm8> CONSTANT-string))
                  ;; string-index
                  <u16be>)
             (<~> (<as> 'CONSTANT-integer (<uimm8> CONSTANT-integer))
                  ;; TODO as bytes
                  <u32be>)
             (<~> (<as> 'CONSTANT-float (<uimm8> CONSTANT-float))
                  ;; TODO as bytes
                  <u32be>)
             (<~> (<as> 'CONSTANT-long (<uimm8> CONSTANT-long)) <s64be>)
             (<~> (<as> 'CONSTANT-double (<uimm8> CONSTANT-double)) <f64be>)
             (<~> (<as> 'CONSTANT-name-and-type (<uimm8> CONSTANT-name-and-type))
                  <u16be>
                  <u16be>)
             (<~> (<as> 'CONSTANT-utf8 (<uimm8> CONSTANT-utf8))
                  (<bind> <u16be>
                          (lambda (len)
                            (<map> (lambda (b*)
                                     (let* ([bv (apply bytevector b*)] [len (bytevector-length bv)])
                                       ;;(printf "utf bv: ~a~n" bv)
                                       (let loop ([i 0] [c* '()])
                                         (cond
                                          [(> i len)
                                           (errorf who "failed to read utf8 constant")]
                                          [(fx= i len)
                                           (let ([str (apply string (reverse c*))])
                                             (println str)
                                             str)]
                                          [else
                                           ;; following ASM, handle only the first three cases
                                           (let ([b (bytevector-u8-ref bv i)])
                                             (cond [(= 0 (fxlogand b #x80))
                                                    (loop (fx+ i 1) (cons (integer->char b) c*))]
                                                   [(= #xc0 (fxlogand b #xe0))
                                                    (loop (fx+ i 2)
                                                          (cons (integer->char
                                                                 (+ (fxsll (fxlogand b #x1f) 6)
                                                                    (fxlogand (bytevector-u8-ref bv (fx1+ i)) #x3f)))
                                                                c*))]
                                                   [else
                                                    (loop (fx+ i 3)
                                                          (cons (integer->char
                                                                 (+ (fxsll (fxlogand b #xf) 12)
                                                                    (fxsll (fxlogand (bytevector-u8-ref bv (fx+ i 1)) #x3f) 6)
                                                                    (fxlogand (bytevector-u8-ref bv (fx+ i 2)) #x3f)))
                                                                c*))]))]))))
                                   (<rep> <u8> len)))))
             (<~> (<as> 'CONSTANT-method-handle (<uimm8> CONSTANT-method-handle))
                  ;; ref-kind
                  <u8>
                  ;; ref-index
                  <u16be>)
             (<~> (<as> 'CONSTANT-method-type (<uimm8> CONSTANT-method-type))
                  ;; desc-index
                  <u16be>)
             (<~> (<as> 'CONSTANT-dynamic (<uimm8> CONSTANT-dynamic))
                  ;; bootstrap-method-attr-index
                  <u16be>
                  ;; name-and-type-index
                  <u16be>)
             (<~> (<as> 'CONSTANT-invoke-dynamic (<uimm8> CONSTANT-invoke-dynamic))
                  ;; bootstrap-method-attr-index
                  <u16be>
                  ;; name-and-type-index
                  <u16be>)
             (<~> (<as> 'CONSTANT-module (<uimm8> CONSTANT-module))
                  ;; name-index
                  <u16be>)
             (<~> (<as> 'CONSTANT-package (<uimm8> CONSTANT-package))
                  ;; name-index
                  <u16be>)))

      (define-who <attr>
        (<bind-st> (<~> <utf8-string> <u32be>) ;; attr-name-index attr-length
                   (lambda (name.len st)
                     (case (car name.len)
                       ;; TODO more attrs
                       ["Code"
                        (<map> (lambda (val) (cons 'code val))
                               (<~> <u16be> <u16be> ;; max-stack max-locals
                                    (<bind> <u32be>
                                            (lambda (code-length)
                                              (<rep> <u8> code-length)))
                                    (<bind> <u16be>
                                            (lambda (exception-table-length)
                                              (<rep> (<~> <u16be> <u16be> <u16be> <u16be>) exception-table-length)))
                                    (<bind> <u16be>
                                            (lambda (attr-count)
                                              ;; TODO self-recursion
                                              (<rep> <attr> attr-count)))))]
                       [else (<rep> <u8> (cadr name.len))]))))
      (define <field>
        (<~> <u16be>       ;; acc-flags
             <utf8-string> ;; name-index
             <utf8-string> ;; desc-index
             (<bind> <u16be>
                     (lambda (n)
                       ;;(printf "field attr: ~a~n" n)
                       (<rep> <attr> n)))))
      (define <method>
        (<~> <u16be>       ;; acc-flags
             <utf8-string> ;; name-index
             <utf8-string> ;; desc-index
             (<bind> <u16be>
                     (lambda (n)
                       ;;(printf "method attr: ~a~n" n)
                       (<rep> <attr> n)))))

      #|doc
      `p` must be a parser combinator;
      `n` must be a natural number;
      `f` must be a function with type (Val -> Nat).
      ;; TODO result adding behavior when skipping
      |#
      (define-who (<skipper> p n f)
        (lambda (inp state lvl)
          (let ([lb (make-list-builder)])
            (let loop ([i 0] [inp1 inp])
              (if (fx>= i n)
                  (values #t (lb) inp1)
                  (let-values ([(stt val inp2) (parser-call p inp1 state (fx1+ lvl))])
                    (if stt
                        (let ([skip (f val)])
                          (when (< skip 0) (errorf who "cannot have negative skip count: ~a" skip))
                          (unless (integer? skip) (errorf who "cannot have non-integer skip count: ~a" skip))
                          (for-each (lambda (_) (lb val)) (iota skip))
                          (printf "~a [~a] skip ~a~n" who i skip)
                          (loop (fx+ i skip) inp2))
                        (values #f #f (format "failed <rep>: ~a" inp2)))))))))

      (define <constant-pool>
        (<bind> <u16be>
                (lambda (n)
                  (printf "read cp count: ~a~n" n)
                  (<map-st> (lambda (val st)
                              (printf "constant pool size=~a~n" (length val))
                              (let ([val (list->vector val)])
                                (hashtable-set! st 'constant-pool val)
                                val))
                            ;; "The value of the constant_pool_count item is equal to the number of entries
                            ;; in the constant_pool table plus one."

                            ;; need to dynamically adjust `n` if a long/double entry is parsed,
                            ;; 1. insert extra value into the result list
                            ;; 2. decr the iter count
                            (<skipper> <cp-info> (fx1- n)
                                       (lambda (val)
                                         (println val)
                                         (if (or (eq? 'CONSTANT-long   (car val))
                                                 (eq? 'CONSTANT-double (car val)))
                                             2
                                             1)))
                            ;;(<rep> <cp-info> (fx1- n))
                            ))))
      (define <interfaces>
        (<bind> <u16be>
                (lambda (n)
                  (printf "interfaces: ~a~n" n)
                  (<rep> <u16be> n))))
      (define <fields>
        (<bind> <u16be>
                (lambda (n)
                  (printf "fields: ~a~n" n)
                  (<rep> <field> n))))
      (define <methods>
        (<bind> <u16be>
                (lambda (n)
                  (printf "methods: ~a~n" n)
                  (<rep> <method> n))))
      (define <attrs>
        (<bind> <u16be>
                (lambda (n)
                  (printf "class attrs: ~a~n" n)
                  (<rep> <attr> n))))

      (define <jclass>
        (<~> <magic> <major-v> <minor-v>
             (<msg-t> "jclass 1")
             <constant-pool>
             (<msg-t> "jclass 2")
             <acc-flags>
             (<msg-t> "jclass 3")
             <this-class>
             (<msg-t> "jclass 4")
             <super-class>
             (<msg-t> "jclass 5")
             <interfaces>
             (<msg-t> "jclass 6")
             <fields>
             (<msg-t> "jclass 7")
             <methods>
             (<msg-t> "jclass 8")
             <attrs>
             <eof>))
      <jclass>))

  (define parse-java-class-file
    (lambda (path)
      (pcheck ([string? path])
              (parse-binary-file parser-java-class path
                                 ;; use hashtable to handle constant pool refs
                                 (make-eq-hashtable)))))

  )
