(library (chezpp utils)
  (export pcase pcheck pcheck-pair pcheck-list pcheck-string pcheck-char pcheck-proc
          pcheck-hashtable pcheck-vector pcheck-port pcheck-binary-port pcheck-textual-port
          pcheck-open-port pcheck-open-binary-port pcheck-open-textual-port
          pcheck-file pcheck-directory pcheck-symlink
          pcheck-fxvector pcheck-flvector pcheck-natural
          incr! decr! add1! sub1! fx+! fx-! fx*! fx/!
          cons! cdr!
          natural?
          neq? neqv? nequal?
          id)
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
               #`(begin e e* ...)
               (with-syntax ([pred (car preds)] [ids (car id**)]
                             [(p) (generate-temporaries '(p))])
                 #`(let ([p pred])
                     #,(let f ([id* #'ids])
                         (if (null? id*)
                             (loop (cdr preds) (cdr id**))
                             #`(if (p #,(car id*))
                                   #,(f (cdr id*))
                                   (errorf 'k "failed ~a ~a: ~a" 'k 'pred '#,(car id*)))))))))]
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
  (gen-pcheck symbol symbol?)
  (gen-pcheck char char?)
  (gen-pcheck string string?)
  (gen-pcheck hashtable hashtable?)
  (gen-pcheck port port?)
  (gen-pcheck binary-port binary-port?)
  (gen-pcheck textual-port textual-port?)
  (gen-pcheck open-port open-port?)
  (gen-pcheck open-binary-port open-binary-port?)
  (gen-pcheck open-textual-port open-textual-port?)
  (gen-pcheck file file-regular?)
  (gen-pcheck directory file-directory?)
  (gen-pcheck symlink file-symbolic-link?)


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

  )
