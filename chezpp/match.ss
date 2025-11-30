#!chezscheme

#|

Adapted from Andy Keep's implementation:
https://github.com/akeep/scheme-to-llvm/blob/main/src/main/scheme/match.sls

|#


(library (chezpp match)
  (export match ;; mlambda mcase-lambda mlet mletrec mlet* mletrec*
          $match-record $match-datatype match-record match-datatype match-box match-string
          mlambda mlambda+ mlet mlet*
          )
  (import (chezscheme)
          (chezpp list)
          (chezpp vector)
          (chezpp irregex)
          (chezpp internal))

  (define subvector->list
    (lambda (vec start)
      (let ([len (vector-length vec)]
            [ls (list (vector-ref vec start))])
        ;; no need to do error tests
        (let loop ([next ls]
                   [i (add1 start)])
          (if (fx= i len)
              ls
              (let ([item (list (vector-ref vec i))])
                (set-cdr! next item)
                (loop item (add1 i))))))))

  ;; match interface
  (define-syntax match
    (lambda (stx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define literal?
        (lambda (lit)
          (let ([p (syntax->datum lit)])
            (or (number? p)
                (boolean? p)
                (string? p)
                (char? p)))))
      (define back-ref-id?
        (lambda (id)
          (and (identifier? id)
               (char=? #\? (string-ref (symbol->string (syntax->datum id)) 0)))))
      (define regex-prefix?
        (lambda (p)
          (let ([p (syntax->datum p)])
            (or (eq? p '$r) (eq? p '$regex)))))
      (define box-prefix?
        (lambda (p)
          (let ([p (syntax->datum p)])
            (or (eq? p '$b) (eq? p '$box)))))
      (define record-prefix?
        (lambda (p)
          (let ([p (syntax->datum p)])
            (or (eq? p '$rec) (eq? p '$record)))))
      (define datatype-prefix?
        (lambda (p)
          (let ([p (syntax->datum p)])
            (or (eq? p '$data) (eq? p '$datatype)))))
      ;; see ChezScheme syntax.ss: define-record-type
      (define get-type-descriptor (lambda (rho dt) (cadr (rho dt))))
      ;; get all field names of the record type, include those from the parents
      (define get-all-fields
        (lambda (rho dt)
          (let ([rtd (get-type-descriptor rho dt)])
            (let loop ([pdesc (record-type-parent rtd)]
                       [fld** (list (record-type-field-names rtd))])
              (if pdesc
                  (loop (record-type-parent pdesc) (cons (record-type-field-names pdesc) fld**))
                  ;; returns a list of vectors: '(#(f0 f1 ...) ...)
                  (reverse fld**))))))
      (define get-fields
        (lambda (rho dt)
          (let ([rtd (get-type-descriptor rho dt)])
            ;; returns vector of names: '#(f0 f1 ...)
            (record-type-field-names rtd))))
      ;; return `t` if it's a record type, otherwise #f
      (define valid-record-type?
        (lambda (rho t)
          (and (identifier? t)
               (let ([r '#{r6rs-record vc7pishgmrh09qm-a}]
                     [info (rho t)])
                 (and info (eq? r (car (rho t))) t)))))
      (define variant-of?
        (lambda (rho dt variant)
          (and (valid-record-type? rho dt)
               (valid-record-type? rho variant)
               (let ([pdesc (record-type-parent (get-type-descriptor rho variant))])
                 (if pdesc
                     (eq? pdesc (get-type-descriptor rho dt))
                     (errorf 'match "invalid variant ~a of datatype ~a" variant dt))))))
      (define by-name-record-pats?
        (lambda (rho dt pats)
          (let* ([pats (syntax->datum pats)]
                 [fld* (get-fields rho dt)])
            (and (apply = 2 (map (lambda (p) (or (and
                                                  ;; in pats like (,x _ ,y), _ is not a list
                                                  (list? p) (length p))
                                                 -1))
                                 pats))
                 (<= (length pats) (vector-length fld*))
                 ;; TODO check names are unique
                 ;; check if each pat has right field names
                 (andmap (lambda (pat) (vmemq (car pat) fld*)) pats)))))
      (define by-name-datatype-pats?
        (lambda (rho variant pats)
          (by-name-record-pats? rho variant pats)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define handle-box
        (lambda (rho expr-id pat body fk)
          (syntax-case pat (unquote)
            [(,bref)
             (back-ref-id? #'bref)
             #'not-impl]
            [(pat)
             #`(if (box? #,expr-id)
                   #,(with-syntax ([(bval) (generate-temporaries '(bval))])
                       #`(let ([bval (unbox #,expr-id)])
                           #,(process-pattern rho #'bval #'pat body fk)))
                   (#,fk))]
            [(,bref pat)
             (identifier? #'bref)
             #'not-impl]
            [_ (syntax-error pat "match: invalid box pattern format:")])))
      (define handle-record
        (lambda (rho expr-id pat body fk)
          ;; return the name of the record type (dt or $record-dt)
          (define check-record-type
            (lambda (rho dt)
              (let ([res (valid-record-type? rho dt)])
                (or res (let ([dt1 ($construct-name dt "$record-" dt)])
                          (or (valid-record-type? rho dt1)
                              (errorf 'match "invalid record type: ~a" (syntax->datum dt))))))))
          (syntax-case pat ()
            [(dt . rest)
             (let ([dt (check-record-type rho #'dt)])
               (if dt
                   (syntax-case #'rest (unquote)
                     [(,bref)
                      (back-ref-id? #'bref)
                      #'not-impl]
                     [(pat pats ...)
                      (by-name-record-pats? rho dt #'(pat pats ...))
                      ;; use procedural interfaces:
                      ;; (record-predicate rtd) (record-accessor rtd i)
                      ;; their use can be optimized if `rtd` is given at compile time
                      (with-syntax ([(type?) (generate-temporaries '(type?))]
                                    [rtd (datum->syntax dt (get-type-descriptor rho dt))]
                                    [(fld* ...) (generate-temporaries (enumerate #'(pat pats ...)))]
                                    [(i ...) (datum->syntax dt (map
                                                                (lambda (n) (vmemq (car n) (get-fields rho dt)))
                                                                (syntax->datum #'(pat pats ...))))]
                                    ;; get the patterns per se: (field <pat>) -> <pat>
                                    [((_ npats) ...) #'(pat pats ...)])
                        ;; need to quote `rtd`
                        #`(let ([type? (record-predicate 'rtd)])
                            (if (type? #,expr-id)
                                (let ([fld* ((record-accessor 'rtd i) #,expr-id)] ...)
                                  #,(let loop ([fields #'(fld* ...)] [npats #'(npats ...)])
                                      (if (null? fields)
                                          body
                                          (process-pattern rho (car fields) (car npats)
                                                           (loop (cdr fields) (cdr npats))
                                                           fk))))
                                (#,fk))))]
                     [(pat pats ...)
                      (fx= (length #'(pat pats ...)) (vector-length (get-fields rho dt)))
                      (with-syntax ([(type?) (generate-temporaries '(type?))]
                                    [rtd (datum->syntax dt (get-type-descriptor rho dt))]
                                    [(fld* ...) (generate-temporaries (enumerate #'(pat pats ...)))]
                                    [(i ...) (datum->syntax dt (enumerate #'(pat pats ...)))])
                        #`(let ([type? (record-predicate 'rtd)])
                            (if (type? #,expr-id)
                                (let ([fld* ((record-accessor 'rtd i) #,expr-id)] ...)
                                  #,(let loop ([fields #'(fld* ...)] [npats #'(pat pats ...)])
                                      (if (null? fields)
                                          body
                                          (process-pattern rho (car fields) (car npats)
                                                           (loop (cdr fields) (cdr npats))
                                                           fk))))
                                (#,fk))))]
                     [_ (syntax-error pat "match: invalid record pattern:")])
                   (syntax-error pat "match: invalid record type in:")))]
            [_ (syntax-error pat "match: invalid record pattern format:")])))
      (define handle-datatype
        (lambda (rho expr-id pat body fk)
          (syntax-case pat ()
            ;; singletons
            [(dt singleton) (identifier? #'singleton)
             #`(if (eq? #,expr-id singleton)
                   #,body
                   (#,fk))]
            [(dt variant . rest)
             ;; name of dt is dt,
             ;; name of variant is $datatype-dt-varaint
             (let ([variant ($construct-name #'dt "$datatype-" #'dt "-" #'variant)])
               (if (variant-of? rho #'dt variant)
                   (syntax-case #'rest (unquote)
                     [(,bref)
                      (back-ref-id? #'bref)
                      #'not-impl]
                     [(pat pats ...)
                      (by-name-datatype-pats? rho variant #'(pat pats ...))
                      (with-syntax ([(type?) (generate-temporaries '(type?))]
                                    [rtd (datum->syntax variant (get-type-descriptor rho variant))]
                                    [(fld* ...) (generate-temporaries (enumerate #'(pat pats ...)))]
                                    [(i ...) (datum->syntax variant (map
                                                                     (lambda (n) (vmemq (car n) (get-fields rho variant)))
                                                                     (syntax->datum #'(pat pats ...))))]
                                    [((_ npats) ...) #'(pat pats ...)])
                        #`(let ([type? (record-predicate 'rtd)])
                            (if (type? #,expr-id)
                                (let ([fld* ((record-accessor 'rtd i) #,expr-id)] ...)
                                  #,(let loop ([fields #'(fld* ...)] [npats #'(npats ...)])
                                      (if (null? fields)
                                          body
                                          (process-pattern rho (car fields) (car npats)
                                                           (loop (cdr fields) (cdr npats))
                                                           fk))))
                                (#,fk))))]
                     [(pat pats ...)
                      (fx= (length #'(pat pats ...)) (vector-length (get-fields rho variant)))
                      (with-syntax ([(type?) (generate-temporaries '(type?))]
                                    [rtd (datum->syntax variant (get-type-descriptor rho variant))]
                                    [(fld* ...) (generate-temporaries (enumerate #'(pat pats ...)))]
                                    [(i ...) (datum->syntax variant (enumerate #'(pat pats ...)))])
                        #`(let ([type? (record-predicate 'rtd)])
                            (if (type? #,expr-id)
                                (let ([fld* ((record-accessor 'rtd i) #,expr-id)] ...)
                                  #,(let loop ([fields #'(fld* ...)] [npats #'(pat pats ...)])
                                      (if (null? fields)
                                          body
                                          (process-pattern rho (car fields) (car npats)
                                                           (loop (cdr fields) (cdr npats))
                                                           fk))))
                                (#,fk))))]
                     [_ (syntax-error pat "match: invalid datatype pattern:")])
                   (syntax-error pat "match: invalid datatype/variant in:")))]
            [_ (syntax-error pat "match: invalid datatype pattern format:")])))
      (define handle-regex
        (lambda (expr-id pat body fk)
          (define handle-rest
            (lambda (expr-id regex rest body fk)
              (syntax-case rest ()
                [()
                 (with-syntax ([(m) (generate-temporaries '(m))])
                   #`(let ([m (irregex-search #,regex #,expr-id)])
                       (if m
                           #,body
                           (#,fk))))]
                ;; TODO check for duplicate numbers and names?
                ;; by number or by name
                [((n* v*) ...)
                 (and (andmap (lambda (n) (or (and (integer? n) (>= n 0))
                                              (symbol? n)))
                              (syntax->datum #'(n* ...)))
                      (andmap (lambda (v)
                                (syntax-case v ()
                                  [,v (identifier? #'v) #t]
                                  [_ (syntax-error pat "only one pattern variable is allowed in regex pattern: ")]))
                              #'(v* ...)))
                 (with-syntax ([(m) (generate-temporaries '(m))])
                   #`(let ([m (irregex-search #,regex #,expr-id)])
                       (if m
                           #,(let loop ([n* #'(n* ...)]
                                        [v* (map (lambda (v) (syntax-case v () [,var #'var])) #'(v* ...))])
                               (if (null? n*)
                                   body
                                   #`(if (irregex-match-valid-index? m '#,(car n*))
                                         (let ([#,(car v*) (irregex-match-substring m '#,(car n*))])
                                           #,(loop (cdr n*) (cdr v*)))
                                         (errorf 'match "invalid index number or name for regex: ~a" #,(car n*)))))
                           (#,fk))))]
                [_ (syntax-error pat "match: invalid regex pattern:")])))
          (syntax-case pat ()
            ;; TODO reg is a var ref
            [(reg . rest)
             (let ([reg (syntax->datum #'reg)])
               (or (string? reg) (list reg)))
             ;; how to directly embed the compiled regex obj?
             (with-syntax ([(regex) (generate-temporaries '(regex))])
               #`(let ([regex (irregex reg)])
                   #,(handle-rest expr-id #'regex #'rest body fk)))]
            [(reg . rest)
             ;; `reg` could be arbitrary expression, check whether it evals to string/reg
             (todo)]
            [_ (syntax-error pat "match: invalid regex pattern format:")])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   main logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define process-pattern
        (lambda (rho expr-id pat body fk)
          (define extract-pat-vars
            (lambda (pat)
              (let loop ([pat pat] [patvars '()])
                (syntax-case pat (unquote unquote-splicing quasisyntax)
                  [,var  (identifier? #'var) (cons #'var patvars)]
                  [,@var (identifier? #'var) (cons #'var patvars)]
                  [,@[,vars ...]
                   (andmap identifier? #'(vars ...))
                   (append #'(vars ...) patvars)]
                  [,@[proc -> ,vars ...]
                   (and (andmap identifier? #'(vars ...)) (identifier? #'proc) (eq? '-> (datum ->)))
                   (append #'(vars ...) patvars)]
                  [(#`nref pat) (identifier? #'nref) (cons #'nref (loop #'pat patvars))]
                  [(pat0 . pat1) (loop #'pat0 (loop #'pat1 patvars))]
                  [#(pat0 pat1 ...) (loop #'pat0 (loop #'(pat1 ...) patvars))]
                  [_ patvars]))))
          ;;(printf "process-pattern ~s ~s ~s ~s~n" expr-id pat body fk)
          (with-syntax ([expr-id expr-id])
            (syntax-case pat (unquote quasiquote unquote-splicing quasisyntax)
              [,var
               (identifier? #'var)
               #`(let ([var expr-id]) #,body)]
              [`var
               (identifier? #'var)
               #`(if (equal? var expr-id) #,body (#,fk))]
              [()
               #`(if (null? expr-id) #,body (#,fk))]
              [,@var
               (identifier? #'var)
               #`(let ([var (match-loop expr-id)])
                   #,body)]
              [,@[]
               #`(begin (match-loop expr-id)
                        #,body)]
              [,@[,vars ...]
               (andmap identifier? #'(vars ...))
               (if (fx= 1 (length #'(vars ...)))
                   #`(let ([#,(car #'(vars ...)) (match-loop expr-id)])
                       #,body)
                   #`(let-values ([(vars ...) (match-loop expr-id)])
                       #,body))]
              [,@[proc ->]
               (and (identifier? #'proc) (eq? '-> (datum ->)))
               #`(begin (proc expr-id)
                        #,body)]
              [,@[proc -> ,vars ...]
               (and (andmap identifier? #'(vars ...)) (identifier? #'proc) (eq? '-> (datum ->)))
               (if (fx= 1 (length #'(vars ...)))
                   #`(let ([#,(car #'(vars ...)) (proc expr-id)])
                       #,body)
                   #`(let-values ([(vars ...) (proc expr-id)])
                       #,body))]
              [(#`nref pat)
               (identifier? #'nref)
               #`(let ([nref expr-id])
                   #,(process-pattern rho #'expr-id #'pat body fk))]
              [,(?prefix special-pats ...)
               (box-prefix? #'?prefix)
               (handle-box rho #'expr-id #'(special-pats ...) body fk)]
              [,(?prefix special-pats ...)
               (record-prefix? #'?prefix)
               (handle-record rho #'expr-id #'(special-pats ...) body fk)]
              [,(?prefix special-pats ...)
               (datatype-prefix? #'?prefix)
               (handle-datatype rho #'expr-id #'(special-pats ...) body fk)]
              [,(?prefix special-pats ...)
               (regex-prefix? #'?prefix)
               (handle-regex #'expr-id #'(special-pats ...) body fk)]
              ;; ... in the end
              [(pat dots)
               (eq? (datum dots) '...)
               #`(if (pair? expr-id)
                     ;; Get all pat vars in `pat`,
                     ;; iterate through the input list, for each item,
                     ;; destructure it and accumulate matched components in `bindings ...`.
                     #,(with-syntax ([(patvars ...) (extract-pat-vars #'pat)])
                         (with-syntax ([(lbs ...)      (generate-temporaries #'(patvars ...))]
                                       [(iter first rest) (generate-temporaries '(iter first rest))])
                           #`(let ([lbs (make-list-builder)] ...)
                               (let iter ([ls expr-id])
                                 (cond
                                  [(null? ls)
                                   (let ([patvars (lbs)] ...)
                                     #,body)]
                                  [(pair? ls)
                                   (let ([first (car ls)] [rest (cdr ls)])
                                     #,(process-pattern rho #'first #'pat
                                                        #'(begin (lbs patvars) ...
                                                                 (iter rest))
                                                        fk))]
                                  [else (#,fk)])))))
                     (#,fk))]
              ;; ... in the middle, prefer `pat1`
              [(pat0 dots . pat1)
               (eq? (datum dots) '...)
               (with-syntax ([(patvars ...) (extract-pat-vars #'pat0)])
                 (with-syntax ([(bindings ...) (generate-temporaries #'(patvars ...))]
                               [(iter ls first rest new-fk) (generate-temporaries '(iter ls first rest new-fk))])
                   #`(let iter ([ls expr-id] [bindings '()] ...)
                       (let ([new-fk (lambda ()
                                       ;; give up one item in the list to be matched against `pat0`
                                       (if (pair? ls)
                                           (let ([first (car ls)] [rest (cdr ls)])
                                             #,(process-pattern rho #'first #'pat0
                                                                #'(iter rest (cons patvars bindings) ...)
                                                                fk))
                                           (#,fk)))])
                         ;; Match the input directly with `pat1` first, if it matches,
                         ;; `pat0` or all pats vars inside `pat0` is '().
                         ;; If the input fails to match `pat1`, call `new-fk`,
                         #,(process-pattern rho #'ls #'pat1
                                            #`(let ([patvars (reverse bindings)] ...)
                                                #,body)
                                            #'new-fk)))))]
              ;; encapsulates (pat ...), (pat . pat), (pat ... . pat)
              [(pat0 . pat1)
               (with-syntax ([(first rest) (generate-temporaries '(first rest))])
                 #`(if (pair? expr-id)
                       (let ([first (car expr-id)] [rest (cdr expr-id)])
                         #,(process-pattern rho #'first #'pat0
                                            (process-pattern rho #'rest #'pat1 body fk)
                                            fk))
                       (#,fk)))]
              [#()
               #`(if (and (vector? expr-id) (fx= 0 (vector-length expr-id)))
                     #,body
                     (#,fk))]
              ;; `pats` has at least one pattern
              [#(pats ...)
               #`(if (vector? expr-id)
                     #,(with-syntax ([(vlen) (generate-temporaries '(vlen))])
                         #`(let ([vlen (vector-length expr-id)])
                             (if (fx= 0 vlen)
                                 (#,fk)
                                 #,(let loop ([veci 0] [pats #'(pats ...)])
                                     (with-syntax ([vi (datum->syntax #'expr-id veci)])
                                       (syntax-case pats ()
                                         ;; here we use list pattern for ease of looping
                                         [()
                                          #`(if (fx= vi vlen)
                                                #,body
                                                (#,fk))]
                                         [(pat dots)
                                          (eq? (datum dots) '...)
                                          (with-syntax ([(patvars ...) (extract-pat-vars #'pat)])
                                            (with-syntax ([(bindings ...) (generate-temporaries #'(patvars ...))]
                                                          [(i viter vitem) (generate-temporaries '(i viter vitem))])
                                              #`(let viter ([i vi] [bindings '()] ...)
                                                  (if (fx= i vlen)
                                                      (let ([patvars (reverse bindings)] ...)
                                                        #,body)
                                                      (let ([vitem (vector-ref expr-id i)])
                                                        #,(process-pattern rho #'vitem #'pat
                                                                           #'(viter (add1 i) (cons patvars bindings) ...)
                                                                           fk))))))]
                                         ;; note: here `.` is restored
                                         [(pat0 dots . pat1)
                                          (eq? (datum dots) '...)
                                          (with-syntax ([(patvars ...) (extract-pat-vars #'pat0)])
                                            (with-syntax ([(bindings ...) (generate-temporaries #'(patvars ...))]
                                                          [(ls iter new-fk) (generate-temporaries '(ls iter new-fk))])
                                              ;; Turn the rest of the vector into a list and proceeed as in `(pat0 dots . pat1)`.
                                              #`(let iter ([ls (subvector->list expr-id vi)] [bindings '()] ...)
                                                  (let ([new-fk (lambda ()
                                                                  (if (pair? ls)
                                                                      (let ([first (car ls)] [rest (cdr ls)])
                                                                        #,(process-pattern rho #'first #'pat0
                                                                                           #'(iter rest (cons patvars bindings) ...)
                                                                                           fk))
                                                                      (#,fk)))])
                                                    #,(process-pattern rho #'ls #'pat1
                                                                       #`(let ([patvars (reverse bindings)] ...)
                                                                           #,body)
                                                                       #'new-fk)))))]
                                         [(pat0 pat1 ...)
                                          (with-syntax ([(vitem) (generate-temporaries '(vitem))])
                                            #`(let ([vitem (vector-ref expr-id vi)])
                                                #,(process-pattern rho #'vitem #'pat0
                                                                   (loop (add1 veci) (cdr pats))
                                                                   fk)))]))))))
                     (#,fk))]
              [lit
               (literal? #'lit)
               #`(if (equal? lit expr-id) #,body (#,fk))]
              [under
               (equal? (datum under) '_)
               body]
              [sym
               (identifier? #'sym)
               #`(if (eq? 'sym expr-id) #,body (#,fk))]))))
      (define process-clause
        (lambda (rho expr-id cl fk)
          ;;(printf "process-clause ~s ~s ~s~n" expr-id cl fk)
          (syntax-case cl (guard)
            [(pat (guard guard-e* ...) e0 e* ...)
             (process-pattern rho expr-id #'pat
                              #`(if (and guard-e* ...)
                                    (begin e0 e* ...)
                                    ;; call fail continuation
                                    (#,fk))
                              fk)]
            [(pat e0 e* ...)
             (process-clause rho expr-id #'[pat (guard #t) e0 e* ...] fk)])))
      (define generate-skeleton
        (lambda (rho expr-id cl* fk)
          ;;(printf "generate-skeleton: ~s ~s ~s~n" expr-id cl* fk)
          (let loop ([cl* cl*])
            (if (null? cl*)
                fk
                (with-syntax ([(fk) (generate-temporaries '(fk))])
                  #`(let ([fk (lambda () #,(loop (cdr cl*)))])
                      #,(process-clause rho expr-id (car cl*) #'fk)))))))
      (syntax-case stx (else)
        [(k e cl* ... [else e0 e* ...])
         ;; `match-loop` used for catamorphism
         ;; `rho`: compile-time environment
         (lambda (rho)
           #`(let match-loop ([v e])
               #,(generate-skeleton rho #'v #'(cl* ...) #'(begin e0 e* ...))))]
        [(k e cl0 cl* ...)
         #'(let ([v e])
             (match v cl0 cl* ...
                    [else (errorf 'match "no match found")]))]
        [_ (syntax-error 'match "bad match form")])))

  (define-syntax match-box
    (lambda (stx)
      (define transform-clause
        (lambda (who cl)
          (syntax-case cl ()
            [[pat e e* ...]
             #`[,($box pat)
                e e* ...]]
            [_ (syntax-error cl "invalid clause for " (symbol->string (syntax->datum who)))])))
      (syntax-case stx (else)
        [(k e cl* ... [else body body* ...])
         (with-syntax ([(cls ...) (map (lambda (cl) (transform-clause #'k cl)) #'(cl* ...))])
           #'(match e cls ... [else body body* ...]))]
        [(k e cl cl* ...)
         #'(match-box e cl cl* ...
                      [else (errorf 'k "no match found")])])))

  ;; TODO correct name (macro and dt name) in error report (in adt)
  (define-syntax $match-record
    (lambda (stx)
      (define transform-clause
        (lambda (who dt cl)
          (syntax-case cl ()
            ;; single named match
            [[(field pat) e e* ...]
             (identifier? #'field)
             #`[,($record #,dt (field pat))
                e e* ...]]
            [[(pat pat* ...) e e* ...]
             #`[,($record #,dt pat pat* ...)
                e e* ...]]
            [_ (syntax-error cl "invalid clause for " (symbol->string (syntax->datum who)))])))
      (syntax-case stx (else)
        [(k who dt e cl* ... [else body body* ...])
         (with-syntax ([(cls ...) (map (lambda (cl) (transform-clause #'who #'dt cl)) #'(cl* ...))])
           #'(match e cls ... [else body body* ...]))]
        [(k who dt e cl cl* ...)
         #'($match-record who dt e cl cl* ...
                          [else (errorf 'who "no match found")])]
        [(k who . bla) (syntax-error stx "bad " (symbol->string (datum who)) " form:")])))

  (define-syntax $match-datatype
    (lambda (stx)
      (define transform-clause
        (lambda (who dt cl)
          (syntax-case cl ()
            [[(variant pat pats ...) e e* ...]
             #`[,($datatype #,dt variant pat pats ...)
                e e* ...]]
            ;; singletons
            [[(singleton) e e* ...]
             (identifier? #'singleton)
             #`[,($datatype #,dt singleton)
                e e* ...]]
            [[singleton e e* ...]
             (identifier? #'singleton)
             #`[,($datatype #,dt singleton)
                e e* ...]]
            [_ (syntax-error cl "invalid clause for " (symbol->string (syntax->datum who)))])))
      (syntax-case stx (else)
        [(k who dt e cl* ... [else body body* ...])
         (with-syntax ([(cl* ...) (map (lambda (cl) (transform-clause #'who #'dt cl)) #'(cl* ...))])
           #'(match e cl* ... [else body body* ...]))]
        [(k who dt e cl cl* ...)
         #'($match-datatype who dt e cl cl* ...
                            [else (errorf 'who "no match found")])]
        [(k who . bla) (syntax-error stx "bad " (symbol->string (datum who)) " form:")])))

  (define-syntax match-record
    (syntax-rules ()
      [(k e cl* ...) ($match-record match-record e cl* ...)]))

  (define-syntax match-datatype
    (syntax-rules ()
      [(k e cl* ...) ($match-datatype match-datatype e cl* ...)]))

  (define-syntax match-string
    (lambda (stx)
      (syntax-case stx (else)
        [(k str [(reg* pat** ...) . (e** ...)] ... [else el* ...])
         #'(match str
             [,($regex reg* pat** ...) e** ...] ...
             [else el* ...])]
        [(k str [(reg* pat** ...) . (e** ...)] ...)
         #'(match str
             [,($regex reg* pat** ...) e** ...]
             ...)]
        [_ (syntax-error stx "bad match-string form:")])))

  (define-syntax mlambda
    (lambda (stx)
      (syntax-case stx ()
        [(k cl cl* ...)
         #'(lambda (arg)
             (match arg cl cl* ...
                    [else (errorf 'k "no match found")]))]
        [_ (syntax-error stx "bad form:")])))

  (define-syntax $mlambda+
    (lambda (stx)
      (syntax-case stx ()
        [(k who cl cl* ...)
         #'(lambda arg
             (match arg cl cl* ...
                    [else (errorf 'who "no match found")]))]
        [_ (syntax-error stx "bad form:")])))

  (define-syntax mlambda+
    (syntax-rules ()
      [(k cl cl* ...) ($mlambda+ mlambda+ cl cl* ...)]))

  (define-syntax mlet
    (lambda (stx)
      (syntax-case stx ()
        [(k () e0 e* ...) #'(let () e0 e* ...)]
        [(k ([pat* rhs*] ...) e0 e* ...)
         #'(($mlambda+ mlet
                       [(pat* ...) e0 e* ...])
            rhs* ...)]
        [_ (syntax-error stx "bad form:")])))

  (define-syntax mlet*
    (lambda (stx)
      (syntax-case stx (else)
        [(k () e0 e* ...) #'(let () e0 e* ...)]
        [(k ([pat* rhs*] ...) e0 e* ...)
         (trace-let loop ([pat* #'(pat* ...)] [rhs* #'(rhs* ...)])
           (if (null? pat*)
               #'(begin e0 e* ...)
               #`(match #,(car rhs*)
                   [#,(car pat*) #,(loop (cdr pat*) (cdr rhs*))]
                   ;; TODO when k is not quoted, the error is "bad form" below,
                   ;; which is not quite precise
                   [else (errorf 'k "no match found")])))]
        [_ (syntax-error stx "bad form:")])))


  )
