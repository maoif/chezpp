(library (chezpp adt)
  (export datatype record)
  (import (chezscheme)
          (chezpp match)
          (chezpp string)
          (chezpp list)
          (chezpp internal))

  ;; TODO collaborate with match, type-case, etc.
  (define-syntax datatype
    (lambda (stx)
      (define construct-datatype-name
        (lambda (template-identifier . args)
          (apply $construct-name (append (list template-identifier "$datatype-") args))))
      (trace-define check-datatype-name
        (lambda (n)
          ;; TODO
          (if (identifier? #'n)
              #t
              (syntax-error n "bad datatype name:"))))
      (trace-define check-pred-name
        (lambda (n)
          (let ([p (syntax->datum n)])
            (if (symbol? p)
                (let ([p (symbol->string p)])
                  (if (string-endswith? p "?")
                      #t
                      (syntax-error p "bad datatype predicate name (should end with \"?\"): ")))
                (syntax-error p "bad datatype predicate name (not a symbol): ")))))
      (trace-define check-variant-name
        (lambda (v)
          ;; TODO
          (let ([v (syntax->datum v)])
            (or (symbol? v) (symbol? (car v))))))
      (define check-all-names
        (lambda (dt dt? variants)
          (let* ([dt (syntax->datum dt)]
                 [dt? (syntax->datum dt?)]
                 [variants (syntax->datum variants)]
                 [names `(,dt ,dt? ,@(map (lambda (v)
                                            (if (symbol? v) v (car v)))
                                          variants))])
            (unless (unique? names)
              (syntax-error names "names are not unique in datatype definition:")))))
      (define gen-dtuid-str
        (lambda (dt variants)
          (let* ([dtname (symbol->string (syntax->datum dt))]
                 ;; transform variant defs like `[variant (f0 mutable) (f1 pred mutable) f2]`
                 ;; to `variantf0f1f2`
                 [variant->string (lambda (variant)
                                    (apply string-append
                                           (map (lambda (v)
                                                  (symbol->string (if (symbol? v) v (car v))))
                                                variant)))]
                 [bigname (apply string-append dtname
                                 (map variant->string variants))]
                 [h (string-hash bigname)])
            (string-append bigname "-" (number->string h)))))
      (define gen-uids
        (lambda (dt variants)
          (let* ([variants (syntax->datum variants)]
                 [dtuidstr (gen-dtuid-str dt variants)])
            (datum->syntax
             dt
             (cons (string->symbol dtuidstr)
                   (map
                    (lambda (variant)
                      (string->symbol
                       (string-append dtuidstr "-" (symbol->string (car variant)))))
                    variants))))))
      (define gen-vnames
        (lambda (dt variants)
          (map
           (lambda (vname)
             (construct-datatype-name dt dt "-" vname))
           (map car (syntax->datum variants)))))
      (define gen-mkvariants
        (lambda (variants)
          (map car (syntax->datum variants))))
      (define gen-vnames?
        (lambda (dt variants)
          (map
           (lambda (vname)
             (construct-datatype-name dt dt "-" vname "?"))
           (map car (syntax->datum variants)))))
      (define handle-vfields
        (lambda (dt variants)
          (map
           (lambda (variant)
             (syntax-case variant ()
               ;; TODO check field number
               [(vname vfields ...)
                (let f ([vfields #'(vfields ...)] [field* '()])
                  (if (null? vfields)
                      (reverse field*)
                      (syntax-case (car vfields) (mutable)
                        [fid
                         (identifier? #'fid)
                         (f (cdr vfields)
                            (cons #`(immutable fid #,($construct-name dt dt "-" #'vname "-" #'fid))
                                  field*))]
                        [(fid mutable) (identifier? #'fid)
                         (f (cdr vfields)
                            (cons #`(mutable fid
                                             #,($construct-name dt dt "-" #'vname "-" #'fid)
                                             #,($construct-name dt dt "-" #'vname "-" #'fid "-set!"))
                                  field*))]
                        [(fid pred) (identifier? #'fid)
                         (f (cdr vfields)
                            (cons #`(immutable fid #,($construct-name dt dt "-" #'vname "-" #'fid))
                                  field*))]
                        [(fid pred mutable) (identifier? #'fid)
                         (f (cdr vfields)
                            (cons #`(mutable fid
                                             #,($construct-name dt dt "-" #'vname "-" #'fid)
                                             #,($construct-name dt dt "-" #'vname "-" #'fid "-set!"))
                                  field*))]
                        [_ (syntax-error variant "invalid data variant definition:")])))]))
           variants)))
      (define gen-protocols
        (lambda (variants)
          (map
           (lambda (variant)
             (syntax-case variant ()
               [(vname vfields ...)
                (with-syntax ([(pcon vcon args ...) (generate-temporaries #'(p v vfields ...))])
                  #`(lambda (pcon)
                      (lambda (args ...)
                        (let ([vcon (pcon)])
                          #,(let f ([vfields #'(vfields ...)] [a* #'(args ...)])
                              (if (null? vfields)
                                  #'(vcon args ...)
                                  (let ([field (car vfields)] [arg (car a*)])
                                    (syntax-case field (mutable)
                                      [(fid mutable) (identifier? #'fid)
                                       (f (cdr vfields) (cdr a*))]
                                      [(fid pred) (identifier? #'fid)
                                       #`(if (pred #,arg)
                                             #,(f (cdr vfields) (cdr a*))
                                             (errorf 'datatype "wrong argument type for variant ~a, field ~a: ~a"
                                                     'vname 'fid #,arg))]
                                      [(fid pred mutable) (identifier? #'fid)
                                       #`(if (pred #,arg)
                                             #,(f (cdr vfields) (cdr a*))
                                             (errorf 'datatype "wrong argument type for variant ~a, field ~a: ~a"
                                                     'vname 'fid #,arg))]
                                      [_ (f (cdr vfields) (cdr a*))]))))))))]))
           variants)))

      (syntax-case stx ()
        [(_ dt dt? variant0 variants ...)
         (and (identifier? #'dt)
              (identifier? #'dt?)
              (check-datatype-name #'dt)
              (check-pred-name #'dt?)
              (andmap check-variant-name #'(variant0 variants ...))
              (check-all-names #'dt #'dt? #'(variant0 variants ...)))
         ;; TODO check uniqueness of names
         ;; names starting with $datatype are for internal use
         (with-syntax
             (
              ;; $datatype-mk-dt
              [mkdt (construct-datatype-name #'dt "mk-" #'dt)]
              [(dtuid vuids ...) (gen-uids #'dt #'(variant0 variants ...))]
              [(vnames ...) (gen-vnames #'dt #'(variant0 variants ...))]
              [(mkvariants ...) (datum->syntax #'dt (map car (syntax->datum #'(variant0 variants ...))))]
              ;; $datatype-dt-variant?
              [(variants? ...) (gen-vnames? #'dt #'(variant0 variants ...))]
              ;; ((mutability name getter ?setter) ...) ...
              ;; getter: dt-variant-field
              ;; setter: dt-variant-field-set!
              [((vfields ...) ...) (handle-vfields #'dt #'(variant0 variants ...))]
              [(protocols ...) (gen-protocols #'(variant0 variants ...))]
              [match-dt  ($construct-name #'dt "match-" #'dt)]
              [match-dt! ($construct-name #'dt "match-" #'dt "!")])
           #`(begin
               (define-record-type (dt mkdt dt?)
                 (nongenerative dtuid))
               (define-record-type (vnames mkvariants variants?)
                 (nongenerative vuids)
                 (parent dt)
                 (sealed #t)
                 ;; TODO when vfields is '(), make a singleton
                 (fields vfields ...)
                 ;; need protocol to do type checking
                 (protocol protocols))
               ...

               (define-syntax match-dt
                 (syntax-rules ()
                   [(k e cl* (... ...)) ($match-datatype match-dt dt e cl* (... ...))]))

               ;; TODO match-dt!
               #;
               (define-syntax match-dt!
                 (lambda (stx)
                   (define transform-clause
                     (lambda (cl)
                       (syntax-case cl ()
                         [[(variant . pats) e e* (... ...)]
                          (memv #'variant '(vnames ...))
                          #`[,($datatype dt variant pats)
                             e e* (... ...)]]
                         [_ (if (memv #'variant '(vnames ...))
                                (syntax-error cl "invalid clause for " (symbol->string 'match-dt))
                                (syntax-error 'variant "invalid variant name for "
                                              (symbol->string 'dt) ":"))])))
                   (syntax-case stx (else)
                     [(k e cl* (... ...) [else body body* (... ...)])
                      (begin (printf "~a 1~n" 'match-dt) #t)
                      (with-syntax ([(cl* (... ...)) (map transform-clause #'(cl* (... ...)))])
                        #'(match e cl* (... ...) [else body body* (... ...)]))]
                     [(k e cl cl* (... ...))
                      (begin (printf "~a 2~n" 'match-dt) #t)
                      #'(match-dt e cl cl* (... ...)
                                  [else (errorf 'match-dt "no match found")])]
                     [_ (syntax-error stx "bad " (symbol->string 'match-dt) " form:")])))
               ))]
        [(_ dt variant0 variants ...)
         (check-datatype-name #'dt)
         (with-syntax ([dt? ($construct-name #'dt #'dt "?")])
           #'(datatype dt dt? variant0 variants ...))]
        [_ (syntax-error stx "bad datatype definition:")])))

  ;; TODO parent?
  ;; check if record def has a parent of datatype (just mask datatype names?)
  ;; or check if the name starts with "$datatype"
  (define-syntax record
    (lambda (stx)
      (define gen-uid
        (lambda (dt field*)
          (let* ([t dt]
                 [dt (syntax->datum dt)]
                 [field* (syntax->datum field*)]
                 [bigname (string-append (symbol->string dt)
                                         (apply string-append
                                                (map (lambda (f) (symbol->string (if (symbol? f) f (car f))))
                                                     field*)))]
                 [h (string-hash bigname)])
            (datum->syntax t
                           (string->symbol (string-append bigname "-" (number->string h)))))))
      (define handle-fields
        (lambda (fields)
          (map
           (lambda (field)
             (syntax-case field (mutable)
               [fid
                #'(immutable fid)]
               [(fid mutable) (identifier? #'fid)
                #'(mutable fid)]
               [(fid pred) (identifier? #'fid)
                #'(immutable fid)]
               [(fid pred mutable) (identifier? #'fid)
                #'(mutable fid)]
               [_ (syntax-error field "invalid record field definition:")]))
           fields)))
      (define gen-protocol
        (lambda (dt fields)
          (with-syntax ([(vcon args ...) (generate-temporaries #`(v #,@fields))])
            #`(lambda (vcon)
                (lambda (args ...)
                  #,(let f ([fields fields] [a* #'(args ...)])
                      (if (null? fields)
                          #'(vcon args ...)
                          (let ([field (car fields)] [arg (car a*)])
                            (syntax-case field (mutable)
                              [(fid mutable) (identifier? #'fid)
                               (f (cdr fields) (cdr a*))]
                              [(fid pred) (identifier? #'fid)
                               #`(if (pred #,arg)
                                     #,(f (cdr fields) (cdr a*))
                                     (errorf 'datatype "wrong argument type for record ~a, field ~a: ~a"
                                             'dt 'fid #,arg))]
                              [(fid pred mutable) (identifier? #'fid)
                               #`(if (pred #,arg)
                                     #,(f (cdr fields) (cdr a*))
                                     (errorf 'datatype "wrong argument type for record ~a, field ~a: ~a"
                                             'dt 'fid #,arg))]
                              [_ (f (cdr fields) (cdr a*))])))))))))
      (syntax-case stx ()
        [(k dt pred (fld fld* ...))
         (andmap identifier? #'(dt pred))
         (with-syntax ([dtname ($construct-name #'dt "$record-" #'dt)]
                       [mkdt #'dt]
                       [uid (gen-uid #'dt #'(fld fld* ...))]
                       [(flds ...) (handle-fields #'(fld fld* ...))]
                       [proto (gen-protocol #'dt #'(fld fld* ...))]
                       [match-dt ($construct-name #'dt "match-" #'dt)])
           #`(begin
               (define-record-type (dtname mkdt dt?)
                 (nongenerative uid)
                 (fields flds ...)
                 (protocol proto))

               (define-syntax match-dt
                 (syntax-rules ()
                   [(k e cl* (... ...)) ($match-record match-dt dtname e cl* (... ...))]))))]
        [(k dt (fld fld* ...))
         (identifier? #'dt)
         (with-syntax ([dt? ($construct-name #'dt #'dt "?")])
           #'(record dt dt? (fld fld* ...)))]
        [_ (syntax-error stx "bad record definition:")])))

  )
