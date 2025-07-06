(library (chezpp adt)
  (export datatype record)
  (import (chezpp chez)
          (chezpp match)
          (chezpp string)
          (chezpp list)
          (chezpp internal))


  (define-syntax datatype
    (lambda (stx)
      (define construct-datatype-name
        (lambda (template-identifier . args)
          (apply $construct-name (append (list template-identifier "$datatype-") args))))
      (define get-vname
        (lambda (v) (if (symbol? v) v (car v))))
      (define check-datatype-name
        (lambda (n)
          ;; TODO
          (if (identifier? #'n)
              #t
              (syntax-error n "bad datatype name:"))))
      (define check-pred-name
        (lambda (n)
          (let ([p (syntax->datum n)])
            (if (symbol? p)
                (let ([p (symbol->string p)])
                  (if (string-endswith? p "?")
                      #t
                      (syntax-error p "bad datatype predicate name (should end with \"?\"): ")))
                (syntax-error p "bad datatype predicate name (not a symbol): ")))))
      (define check-variant-name
        (lambda (v)
          ;; TODO
          (let ([v (syntax->datum v)])
            (or (symbol? v) (symbol? (car v))))))
      (define check-all-names
        (lambda (dt dt? variants)
          (let* ([dt (syntax->datum dt)]
                 [dt? (syntax->datum dt?)]
                 [variants (syntax->datum variants)]
                 [names `(,dt ,dt? ,@(map get-vname variants))])
            (unless (unique? names)
              (syntax-error names "names are not unique in datatype definition:")))))
      (define gen-dtuid
        (lambda (dt variants)
          (let* ([dtname (symbol->string (syntax->datum dt))]
                 ;; transform variant defs like `[variant (f0 mutable) (f1 pred mutable) f2]`
                 ;; to `variantf0f1f2`
                 [variant->string (lambda (variant)
                                    (apply string-append
                                           (map (lambda (v)
                                                  (symbol->string (get-vname v)))
                                                variant)))]
                 [bigname (apply string-append dtname
                                 (map variant->string (syntax->datum variants)))]
                 [h (string-hash bigname)])
            (datum->syntax dt (string->symbol (string-append bigname "-" (number->string h)))))))
      (define gen-vuids
        (lambda (dt dtuid variants)
          (let* ([variants (syntax->datum variants)]
                 [dtuidstr (symbol->string (syntax->datum dtuid))])
            (datum->syntax
             dt
             (map (lambda (v)
                    (string->symbol
                     (string-append dtuidstr "-" (symbol->string (get-vname v)))))
                  variants)))))
      (define gen-vnames
        (lambda (dt variants)
          (map
           (lambda (vname)
             (construct-datatype-name dt dt "-" vname))
           (map get-vname (syntax->datum variants)))))
      (define gen-vnames?
        (lambda (dt variants)
          (map
           (lambda (vname)
             (construct-datatype-name dt dt "-" vname "?"))
           (map get-vname (syntax->datum variants)))))
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
                      (syntax-case (car vfields) (mutable immutable)
                        [fid
                         (identifier? #'fid)
                         (f (cdr vfields)
                            (cons #`(immutable fid #,($construct-name dt dt "-" #'vname "-" #'fid))
                                  field*))]
                        [(fid pred) ;; TODO check more about pred
                         (and (identifier? #'fid) (identifier? #'pred)
                              (not (eq? 'mutable (datum fid))) (not (eq? 'immutable (datum fid))))
                         (f (cdr vfields)
                            (cons #`(immutable fid #,($construct-name dt dt "-" #'vname "-" #'fid))
                                  field*))]
                        [(immutable fid pred)
                         (and (identifier? #'fid) (identifier? #'pred))
                         (f (cdr vfields)
                            (cons #`(immutable fid #,($construct-name dt dt "-" #'vname "-" #'fid))
                                  field*))]
                        [(mutable fid pred)
                         (and (identifier? #'fid) (identifier? #'pred))
                         (f (cdr vfields)
                            (cons #`(mutable fid
                                             #,($construct-name dt dt "-" #'vname "-" #'fid)
                                             #,($construct-name dt dt "-" #'vname "-" #'fid "-set!-raw"))
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
                                    (syntax-case field (mutable immutable)
                                      [(fid mutable)
                                       (f (cdr vfields) (cdr a*))]
                                      [(fid pred)
                                       #`(if (pred #,arg)
                                             #,(f (cdr vfields) (cdr a*))
                                             (errorf 'vname "wrong argument type for field ~a: ~a"
                                                     'fid #,arg))]
                                      [(mutable fid pred)
                                       #`(if (pred #,arg)
                                             #,(f (cdr vfields) (cdr a*))
                                             (errorf 'vname "wrong argument type for field ~a: ~a"
                                                     'fid #,arg))]
                                      [(immutable fid pred)
                                       #`(if (pred #,arg)
                                             #,(f (cdr vfields) (cdr a*))
                                             (errorf 'vname "wrong argument type for field ~a: ~a"
                                                     'fid #,arg))]
                                      [_ (f (cdr vfields) (cdr a*))]))))))))]))
           variants)))
      ;; make sure setters also have type checking, if given
      (define gen-setter-wrappers
        (lambda (dt variants* vfields*)
          (let ([wrappers*
                 (map (lambda (variant vfields)
                        (syntax-case variant ()
                          [(vname vflds ...)
                           (fold-left (lambda (wrappers vfld vfield)
                                        (syntax-case vfield (mutable)
                                          [(mutable fid getter raw-setter)
                                           (syntax-case vfld ()
                                             [(mutable fid pred)
                                              (let ([setter ($construct-name dt dt "-" #'vname "-" #'fid "-set!")])
                                                (cons #`(define #,setter
                                                          (lambda (r v)
                                                            (if (pred v)
                                                                (raw-setter r v)
                                                                (errorf '#,setter "wrong argument type for field ~a: ~a"
                                                                        'fid v))))
                                                      wrappers))])]
                                          [_ wrappers]))
                                      '() #'(vflds ...) vfields)]))
                      variants* vfields*)])
            ;; flatten the list
            (apply append wrappers*))))
      (define get-getters
        (lambda (vfields*)
          (fold-left (lambda (res fields)
                       (append (map (lambda (fld)
                                      (syntax-case fld (mutable immutable)
                                        [(immutable _ getter) #'getter]
                                        [(mutable _ getter _) #'getter]))
                                    fields)
                               res))
                     '() vfields*)))
      (define get-setters
        (lambda (dt variants)
          (apply append
                 (map (lambda (variant)
                        (syntax-case variant ()
                          [(vname vflds ...)
                           (fold-left (lambda (res vfld)
                                        (syntax-case vfld (mutable immutable)
                                          ;; checks here omitted
                                          [(mutable fid _) (cons ($construct-name dt dt "-" #'vname "-" #'fid "-set!") res)]
                                          [_ res]))
                                      '() #'(vflds ...))]))
                      variants))))
      (define classify-variants
        (lambda (variants)
          (let loop ([variants variants] [singletons '()] [others '()])
            (if (null? variants)
                (list (reverse singletons) (reverse others))
                (let ([variant (car variants)])
                  (syntax-case variant ()
                    [(vname)
                     (loop (cdr variants) (cons #'vname singletons) others)]
                    [(vname vfields ...)
                     (loop (cdr variants) singletons (cons variant others))]))))))
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
             ([dtuid (gen-dtuid #'dt #'(variant0 variants ...))]
              ;; $datatype-mk-dt
              [mkdt (construct-datatype-name #'dt "mk-" #'dt)]
              [((singletons ...) (mvariants ...)) (classify-variants #'(variant0 variants ...))]
              [match-dt  ($construct-name #'dt "match-" #'dt)]
              [match-dt! ($construct-name #'dt "match-" #'dt "!")])
           (with-syntax ([(suids ...) (gen-vuids #'dt #'dtuid #'(singletons ...))]
                         [(muids ...) (gen-vuids #'dt #'dtuid #'(mvariants ...))]
                         [(mksingletons ...) (generate-temporaries #'(singletons ...))]
                         [((mkvariants . _) ...) #'(mvariants ...)]
                         ;; $datatype-dt-variant
                         [(snames ...) (gen-vnames #'dt #'(singletons ...))]
                         [(mnames ...) (gen-vnames #'dt #'(mvariants ...))]
                         ;; $datatype-dt-variant?
                         [(singletons? ...) (gen-vnames? #'dt #'(singletons ...))]
                         [(mvariants? ...)  (gen-vnames? #'dt #'(mvariants ...))]
                         ;; ((mutability name getter ?setter) ...) ...
                         ;; getter: dt-variant-field
                         ;; setter: dt-variant-field-set!
                         [((vfields ...) ...) (handle-vfields #'dt #'(mvariants ...))]
                         [(protocols ...) (gen-protocols #'(mvariants ...))])
             (with-syntax ([(setter-wrappers ...) (gen-setter-wrappers #'dt #'(mvariants ...) #'((vfields ...) ...))]
                           [(getters ...) (get-getters #'((vfields ...) ...))]
                           [(setters ...) (get-setters #'dt #'(mvariants ...))])
               #`(module (dt dt? mnames ... snames ...
                             mkvariants ... mvariants? ... singletons ... singletons? ...
                             getters ... setters ...
                             match-dt match-dt!)
                   (define-record-type (dt mkdt dt?)
                     (nongenerative dtuid))
                   (define-record-type (mnames mkvariants mvariants?)
                     (nongenerative muids)
                     (parent dt)
                     (sealed #t)
                     (fields vfields ...)
                     ;; need protocol to do type checking
                     (protocol protocols))
                   ...
                   (define-record-type (snames mksingletons singletons?)
                     (nongenerative suids)
                     (parent dt)
                     (sealed #t)
                     (fields))
                   ...
                   (define singletons (mksingletons))
                   ...
                   (begin setter-wrappers ...)
                   (define-syntax match-dt
                     (syntax-rules ()
                       [(k e cl* (... ...)) ($match-datatype match-dt dt e cl* (... ...))]))
                   (define-syntax match-dt!
                     (lambda (stx)
                       (define check
                         (lambda (cl*)
                           (let ([vrts  (map (lambda (cl)
                                               (syntax-case cl ()
                                                 [(pat . e*)
                                                  (syntax-case #'pat ()
                                                    [(variant . rest) #'variant]
                                                    [singleton (identifier? #'singleton) #'singleton]
                                                    [_ (syntax-error cl "invalid clause for " (symbol->string 'match-dt!))])]
                                                 [_ (syntax-error cl "invalid clause for " (symbol->string 'match-dt!))]))
                                             cl*)])
                             (or (andmap (lambda (v) (memq v (syntax->datum vrts))) '(singletons ... mkvariants ...))
                                 ;; TODO list unwritten variants?
                                 (syntax-error cl* "incomplete variants for " (symbol->string 'match-dt!))))))
                       (syntax-case stx (else)
                         [(k e cl* (... ...) [else body body* (... ...)])
                          #'($match-datatype match-dt! dt e cl* (... ...)
                                             [else body body* (... ...)])]
                         [(k e cl cl* (... ...))
                          (check #'(cl cl* (... ...)))
                          #'(match-dt! e cl cl* (... ...)
                                       [else (errorf 'k "no match found")])]
                         [_ (syntax-error stx "bad " (symbol->string 'match-dt!) " form:")])))))))]
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
        (lambda (dt fields)
          (map
           (lambda (field)
             (syntax-case field (mutable immutable)
               [fid
                (identifier? #'fid)
                #`(immutable fid
                             #,($construct-name dt dt "-" #'fid))]
               [(fid pred) ;; TODO check more about pred
                (and (identifier? #'fid) (identifier? #'pred)
                     (not (eq? 'mutable (datum fid))) (not (eq? 'immutable (datum fid))))
                #`(immutable fid
                             #,($construct-name dt dt "-" #'fid))]
               [(immutable fid pred)
                (and (identifier? #'fid) (identifier? #'pred))
                #`(immutable fid
                             #,($construct-name dt dt "-" #'fid))]
               [(mutable fid pred)
                (and (identifier? #'fid) (identifier? #'pred))
                #`(mutable fid
                           #,($construct-name dt dt "-" #'fid)
                           #,($construct-name dt dt "-" #'fid "-set!-raw"))]
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
                            (syntax-case field (mutable immutable)
                              ;; no guards below as it's checked in handle-fields
                              [(fid pred)
                               #`(if (pred #,arg)
                                     #,(f (cdr fields) (cdr a*))
                                     (errorf '#,dt "wrong argument type for field ~a: ~a"
                                             'fid #,arg))]
                              [(mutable fid pred)
                               #`(if (pred #,arg)
                                     #,(f (cdr fields) (cdr a*))
                                     (errorf '#,dt "wrong argument type for field ~a: ~a"
                                             'fid #,arg))]
                              [(immutable fid pred)
                               #`(if (pred #,arg)
                                     #,(f (cdr fields) (cdr a*))
                                     (errorf '#,dt "wrong argument type for field ~a: ~a"
                                             'fid #,arg))]
                              [_ (f (cdr fields) (cdr a*))])))))))))
      ;; make sure setters also have type checking, if given
      (define gen-setter-wrappers
        (lambda (dt fields flds)
          (let loop ([fields fields] [flds flds] [wrappers '()])
            (if (null? fields)
                wrappers
                (let ([w (syntax-case (car fields) (mutable)
                           [(mutable fid getter raw-setter)
                            (let ([setter ($construct-name dt dt "-" #'fid "-set!")])
                              #`(define #,setter
                                  (lambda (r v)
                                    (if (#,(syntax-case (car flds) () [(_ _ pred) #'pred]) v)
                                        (raw-setter r v)
                                        (errorf '#,setter "wrong argument type for field ~a: ~a"
                                                'fid v)))))]
                           [_ #f])])
                  (if w
                      (loop (cdr fields) (cdr flds) (cons w wrappers))
                      (loop (cdr fields) (cdr flds) wrappers)))))))
      (define get-getters
        (lambda (flds)
          (fold-left (lambda (res fld)
                       (syntax-case fld (mutable immutable)
                         [(immutable _ getter) (cons #'getter res)]
                         [(mutable _ getter _) (cons #'getter res)]))
                     '() flds)))
      (define get-setters
        (lambda (dt flds)
          (fold-left (lambda (res fld)
                       (syntax-case fld (mutable immutable)
                         [(mutable fid _ _) (cons ($construct-name dt dt "-" #'fid "-set!") res)]
                         [_ res]))
                     '() flds)))
      (syntax-case stx ()
        [(k dt pred (fld fld* ...))
         (andmap identifier? #'(dt pred))
         (with-syntax ([mkdt #'dt]
                       [dtname     ($construct-name #'dt "$record-" #'dt)]
                       [match-dt   ($construct-name #'dt "match-" #'dt)]
                       [uid        (gen-uid #'dt #'(fld fld* ...))]
                       [(flds ...) (handle-fields #'dt #'(fld fld* ...))]
                       [proto      (gen-protocol #'dt #'(fld fld* ...))])
           (with-syntax ([(setter-wrappers ...) (gen-setter-wrappers #'dt #'(flds ...) #'(fld fld* ...))]
                         [(getters ...) (get-getters #'(flds ...))]
                         [(setters ...) (get-setters #'dt #'(flds ...))])
             #`(module (dtname mkdt pred getters ... setters ... match-dt)
                 (define-record-type (dtname mkdt pred)
                   (nongenerative uid)
                   (fields flds ...)
                   (protocol proto))
                 (begin setter-wrappers ...)
                 (define-syntax match-dt
                   (syntax-rules ()
                     [(k e cl* (... ...)) ($match-record match-dt dtname e cl* (... ...))])))))]
        [(k dt (fld fld* ...))
         (identifier? #'dt)
         (with-syntax ([dt? ($construct-name #'dt #'dt "?")])
           #'(record dt dt? (fld fld* ...)))]
        [_ (syntax-error stx "bad record definition:")])))

  )
