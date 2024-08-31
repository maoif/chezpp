(library (chezpp utils)
  (export pcase pcheck pcheck-pair pcheck-list pcheck-string pcheck-char pcheck-proc
          pcheck-hashtable pcheck-vector pcheck-port pcheck-binary-port pcheck-textual-port
          pcheck-open-port pcheck-open-binary-port pcheck-open-textual-port
          pcheck-input-binary-port  pcheck-input-textual-port
          pcheck-output-binary-port pcheck-output-textual-port
          pcheck-file pcheck-directory pcheck-symlink
          pcheck-fxvector pcheck-flvector pcheck-bytevector pcheck-natural
          unreachable!
          incr! decr! add1! sub1! fx+! fx-! fx*! fx/!
          cons! cdr!
          natural?
          neq? neqv? nequal?
          id bool octal hex bin
          define-who trace-define-who

          random-char random-string random-symbol random-datum random-list random-box
          random-bytevector random-u8vec

          define-flags flag-any-set? flag-all-set? flag-set flag-reset)
  (import (chezscheme)
          (chezpp internal))

  (define id (lambda (x) x))

  #|doc
  Dispatch code based on the type of the argument, with type error report.
  |#
  (define-syntax pcase
    (lambda (stx)
      (syntax-case stx (else)
        [(_ id cl cl* ... [else el el* ...])
         (identifier? #'id)
         (let loop ([cl* #'(cl cl* ...)])
           (if (null? cl*)
               #'(begin el el* ...)
               (syntax-case (car cl*) ()
                 [((pred preds ...) e e* ...)
                  (with-syntax ([(p pds ...) (generate-temporaries #'(pred preds ...))])
                    #`(let ([p pred] [pds preds] ...)
                        (if (or (p id) (pds id) ...)
                            (begin e e* ...)
                            #,(loop (cdr cl*)))))]
                 [(pred e e* ...)
                  (with-syntax ([(p) (generate-temporaries '(p))])
                    #`(let ([p pred])
                        (if (p id)
                            (begin e e* ...)
                            #,(loop (cdr cl*)))))])))]
        [(k id cl cl* ...)
         (identifier? #'id)
         #'(pcase id cl cl* ...
                  [else (errorf #f "failed ~a: no predicates are satisfied for ~a" 'k 'id)])]
        [_ (syntax-error stx "invalid pcase form:")])))


  ;; This should be not used everywhere,
  ;; instead, it should be used in "boundaries"
  ;; where check is really necessary,
  ;; and inside the boudary, types are all good.
  ;; TODO check if cp0 optimizes code with exceptions
  (define-syntax pcheck
    (lambda (stx)
      (define all-ids?
        (lambda (ls*)
          (andmap
           (lambda (ls)
             (andmap identifier? ls))
           ls*)))
      (syntax-case stx ()
        [(k () e e* ...)
         #'(begin e e* ...)]
        [(k ([pred x x* ...] ...) e e* ...)
         (all-ids? #'((x x* ...) ...))
         (let loop ([preds #'(pred ...)] [id** #'((x x* ...) ...)])
           (if (null? preds)
               #`(let () e e* ...)
               (with-syntax ([pred (car preds)] [ids (car id**)]
                             [(p) (generate-temporaries '(p))])
                 #`(let ([p pred])
                     #,(let f ([id* #'ids])
                         (if (null? id*)
                             (loop (cdr preds) (cdr id**))
                             #`(if (p #,(car id*))
                                   #,(f (cdr id*))
                                   (errorf 'k "failed ~a ~a: ~a, value: ~a" 'k 'pred '#,(car id*) #,(car id*)))))))))]
        [_ (syntax-error stx "invalid pcheck form:")])))

  (define-syntax gen-pcheck
    (lambda (stx)
      (syntax-case stx ()
        [(k name pred)
         (with-syntax ([pcheck-name ($construct-name #'k "pcheck-" #'name)])
           #`(define-syntax pcheck-name
               (lambda (s)
                 (syntax-case s ()
                   [(_ (id id* (... ...)) e e* (... ...))
                    #'(pcheck ([pred id id* (... ...)])
                              e e* (... ...))]))))])))

  (define open-port? (lambda (p) (and (port? p) (not (port-closed? p)))))
  (define open-textual-port? (lambda (p) (and (textual-port? p) (not (port-closed? p)))))
  (define open-binary-port?  (lambda (p) (and (binary-port? p) (not (port-closed? p)))))

  ;; just by default test for openness
  (define input-binary-port?   (lambda (p) (and (not (port-closed? p)) (input-port? p) (binary-port? p))))
  (define input-textual-port?  (lambda (p) (and (not (port-closed? p)) (input-port? p) (textual-port? p))))
  (define output-binary-port?  (lambda (p) (and (not (port-closed? p)) (output-port? p) (binary-port? p))))
  (define output-textual-port? (lambda (p) (and (not (port-closed? p)) (output-port? p) (textual-port? p))))

  (define natural? (lambda (n) (and (integer? n) (>= n 0))))

  (gen-pcheck proc procedure?)
  (gen-pcheck pair pair?)
  (gen-pcheck list list?)
  (gen-pcheck number number?)
  (gen-pcheck fixnum fixnum?)
  (gen-pcheck flonum flonum?)
  (gen-pcheck natural natural?)
  (gen-pcheck vector vector?)
  (gen-pcheck fxvector fxvector?)
  (gen-pcheck flvector flvector?)
  (gen-pcheck bytevector bytevector?)
  (gen-pcheck symbol symbol?)
  (gen-pcheck char char?)
  (gen-pcheck string string?)
  (gen-pcheck hashtable hashtable?)

  (gen-pcheck port port?)
  (gen-pcheck input-port input-port?)
  (gen-pcheck output-port output-port?)
  (gen-pcheck binary-port binary-port?)
  (gen-pcheck textual-port textual-port?)
  (gen-pcheck open-port open-port?)
  (gen-pcheck open-binary-port open-binary-port?)
  (gen-pcheck open-textual-port open-textual-port?)

  (gen-pcheck input-binary-port   input-binary-port?)
  (gen-pcheck input-textual-port  input-textual-port?)
  (gen-pcheck output-binary-port  output-binary-port?)
  (gen-pcheck output-textual-port output-textual-port?)

  (gen-pcheck file file-regular?)
  (gen-pcheck directory file-directory?)
  (gen-pcheck symlink file-symbolic-link?)


  (define unreachable!
    (case-lambda
      [() (raise-continuable (condition (make-error)
                                        (make-message-condition "Should not reach here!")))]
      [(who) (raise-continuable (condition (make-error)
                                           (make-who-condition who)
                                           (make-message-condition "Should not reach here!")))]))


  (define-syntax incr!
    (lambda (stx)
      (syntax-case stx ()
        [(_ n) (identifier? #'n)
         #'(set! n (+ n 1))])))

  (define-syntax decr!
    (lambda (stx)
      (syntax-case stx ()
        [(_ n) (identifier? #'n)
         #'(set! n (- n 1))])))

  (define-syntax add1!
    (syntax-rules ()
      [(_ n) (incr! n)]))

  (define-syntax sub1!
    (syntax-rules ()
      [(_ n) (decr! n)]))

  (define-syntax fx+!
    (lambda (stx)
      (syntax-case stx ()
        [(_ n x ...) (identifier? #'n)
         #'(set! n (fx+ n x ...))])))

  (define-syntax fx-!
    (lambda (stx)
      (syntax-case stx ()
        [(_ n x ...) (identifier? #'n)
         #'(set! n (fx- n x ...))])))

  (define-syntax fx*!
    (lambda (stx)
      (syntax-case stx ()
        [(_ n x ...) (identifier? #'n)
         #'(set! n (fx* n x ...))])))

  (define-syntax fx/!
    (lambda (stx)
      (syntax-case stx ()
        [(_ n x ...) (identifier? #'n)
         #'(set! n (fx/ n x ...))])))

  (define-syntax cons!
    (lambda (stx)
      (syntax-case stx ()
        [(_ a p) (identifier? #'p)
         #'(set! p (cons a p))])))

  (define-syntax cdr!
    (lambda (stx)
      (syntax-case stx ()
        [(_ v) (identifier? #'v)
         #'(set! v (cdr v))])))


  (define-syntax neq?
    (syntax-rules ()
      [(_ e* ...) (not (eq? e* ...))]))
  (define-syntax neqv?
    (syntax-rules ()
      [(_ e* ...) (not (eqv? e* ...))]))
  (define-syntax nequal?
    (syntax-rules ()
      [(_ e* ...) (not (equal? e* ...))]))


  (define bool
    (lambda (x) (if x #t #f)))


  (define octal
    (case-lambda
      [(n) (octal n #t)]
      [(n readable?) (string-append (if readable? "#o" "0o") (number->string n 8))]))
  (define hex
    (case-lambda
      [(n) (hex n #t)]
      [(n readable?) (string-append (if readable? "#x" "0x") (number->string n 16))]))
  (define bin
    (case-lambda
      [(n) (bin n #t)]
      [(n readable?) (string-append (if readable? "#b" "0b") (number->string n 2))]))


  ;; same in list.ss, to avoid cyclic dependency
  (define make-list-builder
    (lambda args
      (let ([res args])
        (let ([current-cell (if (null? res)
                                (cons #f '())
                                (let loop ([res res])
                                  (if (null? (cdr res))
                                      res
                                      (loop (cdr res)))))]
              [next-cell (cons #f '())])
          (define add-item!
            (lambda (item)
              (if (null? res)
                  (begin (set-car! current-cell item)
                         (set! res current-cell))
                  (begin
                    (set-car! next-cell item)
                    (set-cdr! current-cell next-cell)
                    (set! current-cell next-cell)
                    (set! next-cell (cons #f '()))))))
          (rec lb
            (case-lambda
              [() res]
              [(x) (add-item! x)]
              [x* (for-each lb x*)]))))))


  ;; from ChezScheme cmacros.ss
  (define-syntax define-who
    (lambda (x)
      (syntax-case x ()
        [(k (id . args) b1 b2 ...)
         #'(k id (lambda args b1 b2 ...))]
        [(k #(prefix id) e)
         (and (identifier? #'prefix) (identifier? #'id))
         (with-implicit (k who)
           (with-syntax ([ext-id ($construct-name #'id #'prefix #'id)])
             #'(define ext-id (let ([who 'id]) (rec id e)))))]
        [(k id e)
         (identifier? #'id)
         (with-implicit (k who)
           #'(define id (let ([who 'id]) e)))])))

  (define-syntax trace-define-who
    (lambda (x)
      (syntax-case x ()
        [(k (id . args) b1 b2 ...)
         #'(k id (lambda args b1 b2 ...))]
        [(k id e)
         (identifier? #'id)
         (with-implicit (k who)
           #'(trace-define id (let ([who 'id]) e)))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   random data generators
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define $rand (lambda (lb ub) (fx+ lb (random (fx- ub lb)))))
  (define $range-error (lambda (who x y) (errorf who "invalid range: ~a ~a" x y)))

  #|doc
  Generate a random character.
  By default, the character is between ASCII whitespace to ~.
  Pass `lb` (lower bound) and/or `ub` (upper bound) to specify a custom character range.
  |#
  (define-who random-char
    (case-lambda
      [()   (random-char #\space #\~)]
      [(ub) (random-char #\nul ub)]
      [(lb ub)
       (pcheck-char (lb ub)
                    (when (char>=? lb ub) ($range-error who lb ub))
                    (let ([n ($rand (char->integer lb)
                                    (char->integer ub))])
                      (integer->char n)))]))

  #|doc
  Generate a random string.
  By default, the string length is between 0 ~ 10;
  the characters are generated using the default `random-char`.
  Pass `lb` (lower bound) and/or `ub` (upper bound) to specify a custom string length.
  Pass `r` to specify a custom character generator procedure.
  |#
  (define-who random-string
    (case-lambda
      [()   (random-string (lambda () (random-char)) 0 10)]
      [(ub) (random-string (lambda () (random-char)) 0 ub)]
      [(lb ub) (random-string (lambda () (random-char)) lb ub)]
      ;; r: random char generator
      [(r lb ub)
       (pcheck-natural
        (lb ub)
        (when (>= lb ub) ($range-error who lb ub))
        (let* ([len ($rand lb ub)]
               [s (make-string len)])
          (let loop ([i 0])
            (if (fx= i len)
                s
                (begin (string-set! s i (r))
                       (loop (add1 i)))))))]))

  (define $random-symbol (lambda (rs) (string->symbol (rs))))

  #|doc
  Generate a random symbol.
  By default, the symbol name length is between 1 ~ 10;
  the symbol string is generated using the default `random-string`.
  Pass `lb` (lower bound) and/or `ub` (upper bound) to specify a custom symbol name length.
  Pass `r` to specify a custom string generator procedure.
  |#
  (define-who random-symbol
    (case-lambda
      [()   ($random-symbol (lambda () (random-string 1 10)))]
      [(ub) ($random-symbol (lambda () (random-string 1 ub)))]
      [(lb ub) ($random-symbol (lambda () (random-string lb ub)))]
      ;; r: random string generator
      [(r lb ub)
       (pcheck-natural
        (lb ub)
        (when (>= lb ub) ($range-error who lb ub))
        ($random-symbol (lambda () (random-string r lb ub))))]))

  #|doc
  Generate a random datum, i.e., a value of either a fixnum, flonum, char, string, or symbol.
  The range of fixnum is from 0 to the most-positive fixnum.
  The range of flonum is from 0.0 to 999999.999.
  Other types of values are generated using the respective default generators.
  |#
  (define-who random-datum
    (lambda ()
      (case (random 5)
        [0 (random (most-positive-fixnum))]
        [1 (random 999999.999)]
        [2 (random-char)]
        [3 (random-string)]
        [4 (random-symbol)])))

  #|doc
  Generate a random list.
  By default, the list length is between 0 ~ 10;
  the list items are generated using `random-datum`.
  Pass `lb` (lower bound) and/or `ub` (upper bound) to specify a custom list length.
  Pass `r` to specify a list item generator procedure.
  |#
  (define-who random-list
    (case-lambda
      [()
       (random-list (lambda () (random-datum)) 0 10)]
      [(ub)
       (random-list (lambda () (random-datum)) 0 ub)]
      [(lb ub)
       (random-list (lambda () (random-datum)) lb ub)]
      [(r lb ub)
       (pcheck-natural
        (lb ub)
        (when (>= lb ub) ($range-error who lb ub))
        (let ([lb (make-list-builder)] [len ($rand lb ub)])
          (let loop ([i 0])
            (if (fx= i len)
                (lb)
                (begin (lb (r))
                       (loop (add1 i)))))))]))

  #|doc
  Generate a random box.
  By default, the boxed value is generated using `random-datum`.
  Pass `r` to specify a custom boxed value generator procedure.
  |#
  (define-who random-box
    (case-lambda
      [()  (box (random-datum))]
      [(r) (box (r))]))

  (define $random-bytevector
    (case-lambda
      [(who)    ($random-bytevector who 0 10)]
      [(who lb) ($random-bytevector who lb 10)]
      [(who lb ub)
       (pcheck-natural
        (lb ub)
        (when (>= lb ub) ($range-error who lb ub))
        (let* ([len ($rand lb ub)] [u8vec (make-bytevector len 0)])
          (let loop ([i 0])
            (if (fx= i len)
                u8vec
                (begin (bytevector-u8-set! u8vec i (random 256))
                       (loop (add1 i)))))))]))

  (define-who random-bytevector (lambda args (apply $random-bytevector who args)))
  (define-who random-u8vec (lambda args (apply $random-bytevector who args)))

  ;; TODO move random-vector here


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   bit mask/flags
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; taken from ChezScheme cmacros.ss
  (define-syntax define-flags
    (lambda (exp)
      (define mask-environment
        (lambda (flags masks)
          (let f ([flags flags] [masks masks] [e '()])
            (if (null? flags)
                e
                (let ([mask (flag->mask (car masks) e)])
                  (f (cdr flags) (cdr masks)
                     (cons `(,(car flags) . ,mask) e)))))))
      (syntax-case exp ()
        [(_k name (flag mask) ...)
         (with-syntax ([env (datum->syntax #'_k
                                           (mask-environment
                                            (datum (flag ...))
                                            (datum (mask ...))))]
                       [->symbols ($construct-name #'_k #'name "->symbols")])
           #'(begin
               (define-syntax name
                 (lambda (x)
                   (syntax-case x ()
                     ((_k flags (... ...))
                      (datum->syntax #'_k
                                     (flag->mask `(or ,@(datum (flags (... ...)))) 'env))))))
               (define ->symbols
                 (lambda (flags)
                   (let loop ([e 'env] [res '()])
                     (if (null? e)
                         res
                         (if (flag-any-set? (cdar e) flags)
                             (loop (cdr e) (cons (caar e) res))
                             (loop (cdr e) res))))))))])))

  (define-syntax flag-any-set?
    (syntax-rules ()
      ((_ mask x)
       (not (fx= (fxlogand mask x) 0)))))

  (define-syntax flag-all-set?
    (syntax-rules ()
      ((_ mask x)
       (let ((m mask)) (fx= (fxlogand m x) m)))))

  (define-syntax flag-set
    (syntax-rules ()
      ((_ mask x)
       (fxlogor mask x))))

  (define-syntax flag-reset
    (syntax-rules ()
      ((_ mask x)
       (fxlogand (fxlognot mask) x))))

  )
