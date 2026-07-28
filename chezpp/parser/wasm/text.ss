(library (chezpp parser wasm text)
  (export parser-wat-module-syntax)
  (import (chezpp chez)
          (chezpp parser combinator)
          (chezpp parser wasm text lexical)
          (chezpp parser wasm text types))

;;;;===----------------------------------------------------------------------===
;;;; Module grammar helpers and instruction placeholders
;;;;===----------------------------------------------------------------------===

  (define list->immutable-vector
    (lambda (value*)
      (vector->immutable-vector (list->vector value*))))

  (define optional-value
    (lambda (value)
      (if (null? value) #f value)))

  (define wat-open
    (<~0> (<char> #\() <wat-trivia>))

  (define wat-close
    (<~0> (<char> #\)) <wat-trivia>))

  (define wat-double-quote (integer->char #x22))
  (define wat-semicolon (integer->char #x3b))

  (define wat-field-head
    (lambda (keyword)
      (<~0> <pos> wat-open (<wat-keyword> keyword))))

  (define wat-parenthesized
    (lambda (keyword parser)
      (<~1> (<~0> wat-open (<wat-keyword> keyword)) parser wat-close)))

  (define reserved-character?
    (lambda (character)
      (and (not (memv character
                      (list #\space #\tab #\newline #\return #\( #\)
                            wat-double-quote)))
           (not (char=? character wat-semicolon)))))

  (define <wat-reserved>
    (<wat-token>
     (<as-string>
      (<some> (<satisfy-char> reserved-character? "not a WebAssembly reserved character")))))

  (define forbidden-placeholder-head?
    (lambda (head)
      (member head '("import" "export" "param" "result" "local" "type"))))

  (declare-lazy-parser <wat-generic-item>)

  (define <wat-generic-parenthesized>
    (<bind> (<~> <pos> wat-open <wat-reserved>)
            (lambda (head)
              (let ([pos (car head)] [mnemonic (caddr head)])
                (if (forbidden-placeholder-head? mnemonic)
                    (<fail-with> "module clause is not an instruction")
                    (<map>
                     (lambda (operand*)
                       (make-wat-instruction-syntax
                        pos (string->symbol mnemonic) '#() '#() #f
                        (list->immutable-vector operand*)))
                     (<~0> (<many-until> <wat-generic-item> wat-close) wat-close)))))))

  (define <wat-positioned-identifier>
    (<map> (lambda (value) (make-wat-index-reference (car value) (cadr value)))
           (<~> <pos> <wat-identifier>)))

  (define <wat-generic-atom>
    (</> <wat-string> <wat-positioned-identifier> <wat-reserved>))

  (define generic-body
    (begin
      (install-lazy-parser!
       <wat-generic-item>
       (</> <wat-generic-parenthesized> <wat-generic-atom>))
      (<~0> (<many-until> <wat-generic-item> wat-close) wat-close)))

  (define inline-import
    (<map> list->immutable-vector
           (wat-parenthesized "import" (<~> <wat-name> <wat-name>))))

  (define inline-export
    (wat-parenthesized "export" <wat-name>))

  (define field-result
    (lambda (pos kind id data import export* abbreviation)
      (make-wat-module-field
       pos kind id data import (list->immutable-vector export*) abbreviation)))

;;;;===----------------------------------------------------------------------===
;;;; Type and import fields
;;;;===----------------------------------------------------------------------===

  (define type-field
    (<map>
     (lambda (value)
       (let* ([pos (car value)] [subtype (cadr value)]
              [recursive-type
               (make-wat-recursive-type-syntax
                pos (vector->immutable-vector (vector subtype)))])
         (field-result pos 'recursive-type #f recursive-type #f '() #f)))
     (<~> <pos> <wat-type-definition>)))

  (define recursive-type-field
    (<map> (lambda (value)
             (field-result (car value) 'recursive-type #f (cadr value) #f '() #f))
           (<~> <pos> <wat-recursive-type>)))

  (define function-import-description
    (<map> (lambda (value)
             (vector 'function (optional-value (cadr value)) (caddr value)))
           (<~0> (<~> (wat-field-head "func")
                      (<optional> <wat-identifier>) <wat-type-use>)
                 wat-close)))

  (define table-import-description
    (<map> (lambda (value)
             (vector 'table (optional-value (cadr value)) (caddr value)))
           (<~0> (<~> (wat-field-head "table")
                      (<optional> <wat-identifier>) <wat-table-type>)
                 wat-close)))

  (define memory-import-description
    (<map> (lambda (value)
             (vector 'memory (optional-value (cadr value)) (caddr value)))
           (<~0> (<~> (wat-field-head "memory")
                      (<optional> <wat-identifier>) <wat-memory-type>)
                 wat-close)))

  (define global-import-description
    (<map> (lambda (value)
             (vector 'global (optional-value (cadr value)) (caddr value)))
           (<~0> (<~> (wat-field-head "global")
                      (<optional> <wat-identifier>) <wat-global-type>)
                 wat-close)))

  (define tag-import-description
    (<map> (lambda (value)
             (vector 'tag (optional-value (cadr value)) (caddr value)))
           (<~0> (<~> (wat-field-head "tag")
                      (<optional> <wat-identifier>) <wat-tag-type>)
                 wat-close)))

  (define import-description
    (</> function-import-description table-import-description
         memory-import-description global-import-description tag-import-description))

  (define import-field
    (<map>
     (lambda (value)
       (let* ([pos (car value)] [module-name (cadr value)]
              [name (caddr value)] [description (cadddr value)]
              [id (vector-ref description 1)])
         (field-result pos 'import id
                       (vector module-name name
                               (vector-ref description 0)
                               (vector-ref description 2))
                       #f '() #f)))
     (<~0> (<~> (wat-field-head "import") <wat-name> <wat-name> import-description)
            wat-close)))

;;;;===----------------------------------------------------------------------===
;;;; Function, table, memory, global, and tag fields
;;;;===----------------------------------------------------------------------===

  (define named-local-clause
    (<map> (lambda (value)
             (list (make-wat-binding (car value) (cadr value) (caddr value))))
           (<~0> (<~> (wat-field-head "local")
                      <wat-identifier> <wat-value-type>)
                 wat-close)))

  (define unnamed-local-clause
    (<map> (lambda (value)
             (let ([pos (car value)])
               (map (lambda (type) (make-wat-binding pos #f type)) (cadr value))))
           (<~0> (<~> (wat-field-head "local") (<some> <wat-value-type>))
                 wat-close)))

  (define local-clause (</> named-local-clause unnamed-local-clause))

  (define function-field
    (<bind>
     (<~> (wat-field-head "func")
          (<optional> <wat-identifier>)
          (<optional> inline-import)
          (<many> inline-export)
          <wat-type-use>
          (<many> local-clause))
     (lambda (value)
       (<bind> generic-body
               (lambda (body)
                 (let ([pos (car value)]
                       [id (optional-value (cadr value))]
                       [import (optional-value (caddr value))]
                       [export* (cadddr value)]
                       [type-use (car (cddddr value))]
                       [local** (cadr (cddddr value))])
                   (if (and import (pair? body))
                       (<fail-with> "an imported function cannot have a body")
                       (<result>
                        (field-result
                         pos 'function id
                         (vector type-use
                                 (list->immutable-vector (apply append local**))
                                 (list->immutable-vector body))
                         import export* #f)))))))))

  (define table-abbreviation
    (<map> (lambda (value)
             (make-wat-inline-abbreviation
              (car value) 'table-element
              (vector (cadr value) (list->immutable-vector (caddr value)))))
           (<~> <pos> <wat-reference-type>
                (wat-parenthesized "elem" (<many> <wat-generic-item>)))))

  (define table-field
    (<bind>
     (<~> (wat-field-head "table")
          (<optional> <wat-identifier>)
          (<optional> inline-import)
          (<many> inline-export))
     (lambda (value)
       (let ([pos (car value)]
             [id (optional-value (cadr value))]
             [import (optional-value (caddr value))]
             [export* (cadddr value)])
         (<bind> (if import <wat-table-type> (</> table-abbreviation <wat-table-type>))
                 (lambda (type-or-abbreviation)
                   (<map>
                    (lambda (unused)
                      (if (wat-inline-abbreviation? type-or-abbreviation)
                          (field-result pos 'table id #f #f export*
                                        type-or-abbreviation)
                          (field-result pos 'table id type-or-abbreviation
                                        import export* #f)))
                    wat-close)))))))

  (define memory-abbreviation
    (<map> (lambda (value)
             (make-wat-inline-abbreviation
              (car value) 'memory-data
              (list->immutable-vector (cadr value))))
           (<~> <pos> (wat-parenthesized "data" (<some> <wat-string>)))))

  (define memory-field
    (<bind>
     (<~> (wat-field-head "memory")
          (<optional> <wat-identifier>)
          (<optional> inline-import)
          (<many> inline-export))
     (lambda (value)
       (let ([pos (car value)]
             [id (optional-value (cadr value))]
             [import (optional-value (caddr value))]
             [export* (cadddr value)])
         (<bind> (if import <wat-memory-type>
                     (</> memory-abbreviation <wat-memory-type>))
                 (lambda (type-or-abbreviation)
                   (<map>
                    (lambda (unused)
                      (if (wat-inline-abbreviation? type-or-abbreviation)
                          (field-result pos 'memory id #f #f export*
                                        type-or-abbreviation)
                          (field-result pos 'memory id type-or-abbreviation
                                        import export* #f)))
                    wat-close)))))))

  (define global-field
    (<bind>
     (<~> (wat-field-head "global")
          (<optional> <wat-identifier>)
          (<optional> inline-import)
          (<many> inline-export)
          <wat-global-type>)
     (lambda (value)
       (<bind> generic-body
               (lambda (body)
                 (let ([pos (car value)]
                       [id (optional-value (cadr value))]
                       [import (optional-value (caddr value))]
                       [export* (cadddr value)]
                       [type (car (cddddr value))])
                   (cond [(and import (pair? body))
                          (<fail-with> "an imported global cannot have an initializer")]
                         [(and (not import) (null? body))
                          (<fail-with> "a defined global requires an initializer")]
                         [else
                          (<result>
                           (field-result
                            pos 'global id
                            (vector type (list->immutable-vector body))
                            import export* #f))])))))))

  (define tag-field
    (<map>
     (lambda (value)
       (field-result (car value) 'tag (optional-value (cadr value))
                     (car (cddddr value)) (optional-value (caddr value))
                     (cadddr value) #f))
     (<~0> (<~> (wat-field-head "tag")
                (<optional> <wat-identifier>)
                (<optional> inline-import)
                (<many> inline-export)
                <wat-tag-type>)
            wat-close)))

;;;;===----------------------------------------------------------------------===
;;;; Export, start, element, data, and custom fields
;;;;===----------------------------------------------------------------------===

  (define export-description
    (apply </>
           (map (lambda (kind)
                  (<map> (lambda (index) (cons (string->symbol kind) index))
                         (wat-parenthesized kind <wat-index-reference>)))
                '("func" "table" "memory" "global" "tag"))))

  (define export-field
    (<map> (lambda (value)
             (field-result (car value) 'export #f
                           (vector (cadr value) (caddr value)) #f '() #f))
           (<~0> (<~> (wat-field-head "export") <wat-name> export-description)
                 wat-close)))

  (define start-field
    (<map> (lambda (value)
             (field-result (car value) 'start #f (cadr value) #f '() #f))
           (<~0> (<~> (wat-field-head "start") <wat-index-reference>)
                 wat-close)))

  (define element-field
    (<map>
     (lambda (value)
       (field-result
        (car value) 'element (optional-value (cadr value)) #f #f '()
        (make-wat-inline-abbreviation
         (car value) 'element
         (list->immutable-vector (caddr value)))))
     (<~0> (<~> (wat-field-head "elem")
                (<optional> <wat-identifier>)
                (<many-until> <wat-generic-item> wat-close))
            wat-close)))

  (define data-field
    (<map>
     (lambda (value)
       (field-result
        (car value) 'data (optional-value (cadr value)) #f #f '()
        (make-wat-inline-abbreviation
         (car value) 'data
         (list->immutable-vector (caddr value)))))
     (<~0> (<~> (wat-field-head "data")
                (<optional> <wat-identifier>)
                (<many-until> <wat-generic-item> wat-close))
            wat-close)))

  (define custom-field
    (<map>
     (lambda (value)
       (field-result
        (car value) 'custom #f
        (vector (cadr value) (list->immutable-vector (caddr value)) #f)
        #f '() #f))
     (<~0> (<~> (wat-field-head "@custom") <wat-name> (<many> <wat-string>))
            wat-close)))

  (define module-field
    (</> custom-field recursive-type-field type-field import-field function-field
         table-field memory-field global-field tag-field export-field start-field
         element-field data-field))

  (define anchor-custom-fields
    (lambda (field*)
      (let loop ([field* field*] [anchor #f] [result '()])
        (if (null? field*)
            (list->immutable-vector (reverse result))
            (let* ([field (car field*)] [kind (wat-module-field-kind field)])
              (if (eq? kind 'custom)
                  (let ([data (wat-module-field-data field)])
                    (loop
                     (cdr field*) anchor
                     (cons
                      (make-wat-module-field
                       (wat-module-field-pos field) kind #f
                       (vector (vector-ref data 0) (vector-ref data 1) anchor)
                       #f '#() #f)
                      result)))
                  (loop (cdr field*) kind (cons field result))))))))

  #|proc:parser-wat-module-syntax
  The `parser-wat-module-syntax` parser reads exactly one Core 3.0 text module and returns an
  internal positioned module syntax record. Trailing WebAssembly script commands are rejected.
  |#
  (define parser-wat-module-syntax
    (<map>
     (lambda (value)
       (make-wat-module (car value) (optional-value (cadr value))
                        (anchor-custom-fields (caddr value))))
     (<~1> <wat-trivia>
            (<~0> (<~> (wat-field-head "module")
                       (<optional> <wat-identifier>)
                       (<many-until> module-field wat-close))
                  wat-close <eof>))))

  )
