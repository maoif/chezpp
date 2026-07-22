(library (chezpp parser wasm types)
  (export
    make-wasm-module wasm-module? wasm-module-types wasm-module-imports
    wasm-module-functions wasm-module-tables wasm-module-memories wasm-module-globals
    wasm-module-tags wasm-module-exports wasm-module-start wasm-module-elements
    wasm-module-data wasm-module-custom-sections
    make-wasm-custom-section wasm-custom-section? wasm-custom-section-name
    wasm-custom-section-bytes wasm-custom-section-after-section
    make-wasm-recursive-type wasm-recursive-type? wasm-recursive-type-subtypes
    make-wasm-subtype wasm-subtype? wasm-subtype-final? wasm-subtype-supertypes
    wasm-subtype-composite-type
    make-wasm-function-type wasm-function-type? wasm-function-type-parameters
    wasm-function-type-results
    make-wasm-struct-type wasm-struct-type? wasm-struct-type-fields
    make-wasm-array-type wasm-array-type? wasm-array-type-field
    make-wasm-field-type wasm-field-type? wasm-field-type-storage-type
    wasm-field-type-mutable?
    make-wasm-reference-type wasm-reference-type? wasm-reference-type-nullable?
    wasm-reference-type-heap-type
    make-wasm-limits wasm-limits? wasm-limits-address-type wasm-limits-minimum
    wasm-limits-maximum
    make-wasm-table-type wasm-table-type? wasm-table-type-reference-type
    wasm-table-type-limits
    make-wasm-memory-type wasm-memory-type? wasm-memory-type-limits
    make-wasm-global-type wasm-global-type? wasm-global-type-value-type
    wasm-global-type-mutable?
    make-wasm-tag-type wasm-tag-type? wasm-tag-type-type-index
    make-wasm-external-type wasm-external-type? wasm-external-type-kind
    wasm-external-type-type
    make-wasm-import wasm-import? wasm-import-module wasm-import-name
    wasm-import-external-type
    make-wasm-function wasm-function? wasm-function-type-index wasm-function-locals
    wasm-function-body
    make-wasm-table wasm-table? wasm-table-type wasm-table-initializer
    make-wasm-memory wasm-memory? wasm-memory-type
    make-wasm-global wasm-global? wasm-global-type wasm-global-initializer
    make-wasm-tag wasm-tag? wasm-tag-type
    make-wasm-export wasm-export? wasm-export-name wasm-export-kind wasm-export-index
    make-wasm-element wasm-element? wasm-element-mode wasm-element-reference-type
    wasm-element-table-index wasm-element-offset wasm-element-initializers
    make-wasm-data wasm-data? wasm-data-mode wasm-data-memory-index wasm-data-offset
    wasm-data-bytes
    make-wasm-instruction wasm-instruction? wasm-instruction-mnemonic
    wasm-instruction-immediates wasm-instruction-body wasm-instruction-alternate
    make-wasm-memory-argument wasm-memory-argument? wasm-memory-argument-alignment
    wasm-memory-argument-offset wasm-memory-argument-memory-index
    make-wasm-block-type wasm-block-type? wasm-block-type-kind wasm-block-type-value
    make-wasm-catch wasm-catch? wasm-catch-kind wasm-catch-tag-index
    wasm-catch-label-index
    make-wasm-float wasm-float? wasm-float-width wasm-float-bits)
  (import (chezpp chez)
          (chezpp utils))

  (define vector-of?
    (lambda (predicate)
      (lambda (value)
        (and (vector? value)
             (let loop ([index 0])
               (or (fx= index (vector-length value))
                   (and (predicate (vector-ref value index))
                        (loop (fx1+ index)))))))))

  (define optional-natural? (lambda (value) (or (not value) (natural? value))))
  (define instruction-vector?
    (lambda (value) ((vector-of? wasm-instruction?) value)))
  (define optional-instruction-vector?
    (lambda (value) (or (not value) (instruction-vector? value))))
  (define address-type? (lambda (value) (memq value '(i32 i64))))
  (define heap-type?
    (lambda (value)
      (or (natural? value)
          (and (symbol? value)
               (memq value '(any eq i31 struct array none func nofunc exn noexn
                                  extern noextern))))))
  (define value-type?
    (lambda (value)
      (or (and (symbol? value) (memq value '(i32 i64 f32 f64 v128)))
          (wasm-reference-type? value))))
  (define storage-type?
    (lambda (value)
      (or (and (symbol? value) (memq value '(i8 i16)))
          (value-type? value))))
  (define composite-type?
    (lambda (value)
      (or (wasm-function-type? value)
          (wasm-struct-type? value)
          (wasm-array-type? value))))
  (define external-kind?
    (lambda (value) (and (symbol? value) (memq value '(function table memory global tag)))))
  (define element-mode?
    (lambda (value) (and (symbol? value) (memq value '(active passive declarative)))))
  (define data-mode? (lambda (value) (and (symbol? value) (memq value '(active passive)))))
  (define block-kind?
    (lambda (value) (and (symbol? value) (memq value '(empty value-type type-index)))))
  (define catch-kind?
    (lambda (value)
      (and (symbol? value) (memq value '(catch catch-ref catch-all catch-all-ref)))))
  (define instruction-alternate?
    (lambda (value)
      ((vector-of? (lambda (item) (or (wasm-instruction? item) (wasm-catch? item))))
       value)))
  (define float-width? (lambda (value) (or (eqv? value 32) (eqv? value 64))))
  (define section-position?
    (lambda (value)
      (or (not value)
          (and (symbol? value)
               (memq value '(header type import function table memory tag global export start
                                    element data-count code data))))))

  (define-syntax define-checked-record-type
    (syntax-rules ()
      [(_ internal-name public-maker public-predicate internal-maker internal-predicate
          ([field public-accessor internal-accessor field-predicate] ...))
       (begin
         (define-record-type (internal-name internal-maker internal-predicate)
           (fields (immutable field internal-accessor) ...))
         (define public-maker
           (lambda (field ...)
             (pcheck ([field-predicate field] ...)
                     (internal-maker field ...))))
         (define public-predicate
           (lambda (object)
             (pcheck () (internal-predicate object))))
         (define public-accessor
           (lambda (record)
             (pcheck ([internal-predicate record])
                     (internal-accessor record)))) ...)]))

  #|proc:make-wasm-module
  Creates a module. Parameters supply `types`, `imports`, `functions`, `tables`, `memories`,
  `globals`, `tags`, `exports`, `start`, `elements`, `data`, and `custom-sections` fields.
  |#
  #|proc:wasm-module?
  Returns whether `object` is a module record. `object` is the value to test.
  |#
  #|proc:wasm-module-types
  Returns the types of `record`. `record` is a module record.
  |#
  #|proc:wasm-module-imports
  Returns the imports of `record`. `record` is a module record.
  |#
  #|proc:wasm-module-functions
  Returns the functions of `record`. `record` is a module record.
  |#
  #|proc:wasm-module-tables
  Returns the tables of `record`. `record` is a module record.
  |#
  #|proc:wasm-module-memories
  Returns the memories of `record`. `record` is a module record.
  |#
  #|proc:wasm-module-globals
  Returns the globals of `record`. `record` is a module record.
  |#
  #|proc:wasm-module-tags
  Returns the tags of `record`. `record` is a module record.
  |#
  #|proc:wasm-module-exports
  Returns the exports of `record`. `record` is a module record.
  |#
  #|proc:wasm-module-start
  Returns the optional start index of `record`. `record` is a module record.
  |#
  #|proc:wasm-module-elements
  Returns the elements of `record`. `record` is a module record.
  |#
  #|proc:wasm-module-data
  Returns the data segments of `record`. `record` is a module record.
  |#
  #|proc:wasm-module-custom-sections
  Returns the custom sections of `record`. `record` is a module record.
  |#
  (define-checked-record-type $wasm-module make-wasm-module wasm-module?
    $make-wasm-module $wasm-module?
    ([types wasm-module-types $wasm-module-types (vector-of? wasm-recursive-type?)]
     [imports wasm-module-imports $wasm-module-imports (vector-of? wasm-import?)]
     [functions wasm-module-functions $wasm-module-functions (vector-of? wasm-function?)]
     [tables wasm-module-tables $wasm-module-tables (vector-of? wasm-table?)]
     [memories wasm-module-memories $wasm-module-memories (vector-of? wasm-memory?)]
     [globals wasm-module-globals $wasm-module-globals (vector-of? wasm-global?)]
     [tags wasm-module-tags $wasm-module-tags (vector-of? wasm-tag?)]
     [exports wasm-module-exports $wasm-module-exports (vector-of? wasm-export?)]
     [start wasm-module-start $wasm-module-start optional-natural?]
     [elements wasm-module-elements $wasm-module-elements (vector-of? wasm-element?)]
     [data wasm-module-data $wasm-module-data (vector-of? wasm-data?)]
     [custom-sections wasm-module-custom-sections $wasm-module-custom-sections
                      (vector-of? wasm-custom-section?)]))

  #|proc:make-wasm-custom-section
  Creates a custom section. `name`, `bytes`, and `after-section` supply the corresponding fields.
  |#
  #|proc:wasm-custom-section?
  Returns whether `object` is a custom section. `object` is the value to test.
  |#
  #|proc:wasm-custom-section-name
  Returns the name of `record`. `record` is a custom section.
  |#
  #|proc:wasm-custom-section-bytes
  Returns the payload bytes of `record`. `record` is a custom section.
  |#
  #|proc:wasm-custom-section-after-section
  Returns the placement of `record`. `record` is a custom section.
  |#
  (define-checked-record-type $wasm-custom-section
    make-wasm-custom-section wasm-custom-section?
    $make-wasm-custom-section $wasm-custom-section?
    ([name wasm-custom-section-name $wasm-custom-section-name string?]
     [bytes wasm-custom-section-bytes $wasm-custom-section-bytes bytevector?]
     [after-section wasm-custom-section-after-section $wasm-custom-section-after-section
                    section-position?]))

  #|proc:make-wasm-recursive-type
  Creates a recursive type. `subtypes` is a vector of subtype records.
  |#
  #|proc:wasm-recursive-type?
  Returns whether `object` is a recursive type. `object` is the value to test.
  |#
  #|proc:wasm-recursive-type-subtypes
  Returns the subtypes of `record`. `record` is a recursive type.
  |#
  (define-checked-record-type $wasm-recursive-type
    make-wasm-recursive-type wasm-recursive-type?
    $make-wasm-recursive-type $wasm-recursive-type?
    ([subtypes wasm-recursive-type-subtypes $wasm-recursive-type-subtypes
               (vector-of? wasm-subtype?)]))

  #|proc:make-wasm-subtype
  Creates a subtype. `final?`, `supertypes`, and `composite-type` supply its fields.
  |#
  #|proc:wasm-subtype?
  Returns whether `object` is a subtype. `object` is the value to test.
  |#
  #|proc:wasm-subtype-final?
  Returns whether `record` is final. `record` is a subtype.
  |#
  #|proc:wasm-subtype-supertypes
  Returns the supertype indexes of `record`. `record` is a subtype.
  |#
  #|proc:wasm-subtype-composite-type
  Returns the composite type of `record`. `record` is a subtype.
  |#
  (define-checked-record-type $wasm-subtype make-wasm-subtype wasm-subtype?
    $make-wasm-subtype $wasm-subtype?
    ([final? wasm-subtype-final? $wasm-subtype-final? boolean?]
     [supertypes wasm-subtype-supertypes $wasm-subtype-supertypes (vector-of? natural?)]
     [composite-type wasm-subtype-composite-type $wasm-subtype-composite-type composite-type?]))

  #|proc:make-wasm-function-type
  Creates a function type. `parameters` and `results` are vectors of value types.
  |#
  #|proc:wasm-function-type?
  Returns whether `object` is a function type. `object` is the value to test.
  |#
  #|proc:wasm-function-type-parameters
  Returns the parameters of `record`. `record` is a function type.
  |#
  #|proc:wasm-function-type-results
  Returns the results of `record`. `record` is a function type.
  |#
  (define-checked-record-type $wasm-function-type
    make-wasm-function-type wasm-function-type?
    $make-wasm-function-type $wasm-function-type?
    ([parameters wasm-function-type-parameters $wasm-function-type-parameters
                 (vector-of? value-type?)]
     [results wasm-function-type-results $wasm-function-type-results
              (vector-of? value-type?)]))

  #|proc:make-wasm-struct-type
  Creates a struct type. `fields` is a vector of field types.
  |#
  #|proc:wasm-struct-type?
  Returns whether `object` is a struct type. `object` is the value to test.
  |#
  #|proc:wasm-struct-type-fields
  Returns the fields of `record`. `record` is a struct type.
  |#
  (define-checked-record-type $wasm-struct-type make-wasm-struct-type wasm-struct-type?
    $make-wasm-struct-type $wasm-struct-type?
    ([fields wasm-struct-type-fields $wasm-struct-type-fields (vector-of? wasm-field-type?)]))

  #|proc:make-wasm-array-type
  Creates an array type. `field` is its element field type.
  |#
  #|proc:wasm-array-type?
  Returns whether `object` is an array type. `object` is the value to test.
  |#
  #|proc:wasm-array-type-field
  Returns the field of `record`. `record` is an array type.
  |#
  (define-checked-record-type $wasm-array-type make-wasm-array-type wasm-array-type?
    $make-wasm-array-type $wasm-array-type?
    ([field wasm-array-type-field $wasm-array-type-field wasm-field-type?]))

  #|proc:make-wasm-field-type
  Creates a field type. `storage-type` is its storage type and `mutable?` is its mutability.
  |#
  #|proc:wasm-field-type?
  Returns whether `object` is a field type. `object` is the value to test.
  |#
  #|proc:wasm-field-type-storage-type
  Returns the storage type of `record`. `record` is a field type.
  |#
  #|proc:wasm-field-type-mutable?
  Returns whether `record` is mutable. `record` is a field type.
  |#
  (define-checked-record-type $wasm-field-type make-wasm-field-type wasm-field-type?
    $make-wasm-field-type $wasm-field-type?
    ([storage-type wasm-field-type-storage-type $wasm-field-type-storage-type storage-type?]
     [mutable? wasm-field-type-mutable? $wasm-field-type-mutable? boolean?]))

  #|proc:make-wasm-reference-type
  Creates a reference type. `nullable?` is its nullability and `heap-type` is its heap type.
  |#
  #|proc:wasm-reference-type?
  Returns whether `object` is a reference type. `object` is the value to test.
  |#
  #|proc:wasm-reference-type-nullable?
  Returns whether `record` is nullable. `record` is a reference type.
  |#
  #|proc:wasm-reference-type-heap-type
  Returns the heap type of `record`. `record` is a reference type.
  |#
  (define-checked-record-type $wasm-reference-type
    make-wasm-reference-type wasm-reference-type?
    $make-wasm-reference-type $wasm-reference-type?
    ([nullable? wasm-reference-type-nullable? $wasm-reference-type-nullable? boolean?]
     [heap-type wasm-reference-type-heap-type $wasm-reference-type-heap-type heap-type?]))

  #|proc:make-wasm-limits
  Creates limits. `address-type` is `i32` or `i64`; `minimum` and optional `maximum` are bounds.
  |#
  #|proc:wasm-limits?
  Returns whether `object` is limits. `object` is the value to test.
  |#
  #|proc:wasm-limits-address-type
  Returns the address type of `record`. `record` is limits.
  |#
  #|proc:wasm-limits-minimum
  Returns the minimum of `record`. `record` is limits.
  |#
  #|proc:wasm-limits-maximum
  Returns the optional maximum of `record`. `record` is limits.
  |#
  (define-checked-record-type $wasm-limits make-wasm-limits wasm-limits?
    $make-wasm-limits $wasm-limits?
    ([address-type wasm-limits-address-type $wasm-limits-address-type address-type?]
     [minimum wasm-limits-minimum $wasm-limits-minimum natural?]
     [maximum wasm-limits-maximum $wasm-limits-maximum optional-natural?]))

  #|proc:make-wasm-table-type
  Creates a table type. `reference-type` is its element type and `limits` supplies its bounds.
  |#
  #|proc:wasm-table-type?
  Returns whether `object` is a table type. `object` is the value to test.
  |#
  #|proc:wasm-table-type-reference-type
  Returns the reference type of `record`. `record` is a table type.
  |#
  #|proc:wasm-table-type-limits
  Returns the limits of `record`. `record` is a table type.
  |#
  (define-checked-record-type $wasm-table-type make-wasm-table-type wasm-table-type?
    $make-wasm-table-type $wasm-table-type?
    ([reference-type wasm-table-type-reference-type $wasm-table-type-reference-type
                     wasm-reference-type?]
     [limits wasm-table-type-limits $wasm-table-type-limits wasm-limits?]))

  #|proc:make-wasm-memory-type
  Creates a memory type. `limits` supplies its bounds and address type.
  |#
  #|proc:wasm-memory-type?
  Returns whether `object` is a memory type. `object` is the value to test.
  |#
  #|proc:wasm-memory-type-limits
  Returns the limits of `record`. `record` is a memory type.
  |#
  (define-checked-record-type $wasm-memory-type make-wasm-memory-type wasm-memory-type?
    $make-wasm-memory-type $wasm-memory-type?
    ([limits wasm-memory-type-limits $wasm-memory-type-limits wasm-limits?]))

  #|proc:make-wasm-global-type
  Creates a global type. `value-type` supplies its type and `mutable?` supplies its mutability.
  |#
  #|proc:wasm-global-type?
  Returns whether `object` is a global type. `object` is the value to test.
  |#
  #|proc:wasm-global-type-value-type
  Returns the value type of `record`. `record` is a global type.
  |#
  #|proc:wasm-global-type-mutable?
  Returns whether `record` is mutable. `record` is a global type.
  |#
  (define-checked-record-type $wasm-global-type make-wasm-global-type wasm-global-type?
    $make-wasm-global-type $wasm-global-type?
    ([value-type wasm-global-type-value-type $wasm-global-type-value-type value-type?]
     [mutable? wasm-global-type-mutable? $wasm-global-type-mutable? boolean?]))

  #|proc:make-wasm-tag-type
  Creates a tag type. `type-index` is its function type index.
  |#
  #|proc:wasm-tag-type?
  Returns whether `object` is a tag type. `object` is the value to test.
  |#
  #|proc:wasm-tag-type-type-index
  Returns the type index of `record`. `record` is a tag type.
  |#
  (define-checked-record-type $wasm-tag-type make-wasm-tag-type wasm-tag-type?
    $make-wasm-tag-type $wasm-tag-type?
    ([type-index wasm-tag-type-type-index $wasm-tag-type-type-index natural?]))

  #|proc:make-wasm-external-type
  Creates an external type. `kind` identifies the namespace and `type` is its entity type.
  |#
  #|proc:wasm-external-type?
  Returns whether `object` is an external type. `object` is the value to test.
  |#
  #|proc:wasm-external-type-kind
  Returns the kind of `record`. `record` is an external type.
  |#
  #|proc:wasm-external-type-type
  Returns the entity type of `record`. `record` is an external type.
  |#
  (define-checked-record-type $wasm-external-type
    make-wasm-external-type wasm-external-type?
    $make-wasm-external-type $wasm-external-type?
    ([kind wasm-external-type-kind $wasm-external-type-kind external-kind?]
     [type wasm-external-type-type $wasm-external-type-type
           (lambda (value)
             (or (natural? value) (wasm-table-type? value) (wasm-memory-type? value)
                 (wasm-global-type? value) (wasm-tag-type? value)))]))

  #|proc:make-wasm-import
  Creates an import. `module`, `name`, and `external-type` supply its corresponding fields.
  |#
  #|proc:wasm-import?
  Returns whether `object` is an import. `object` is the value to test.
  |#
  #|proc:wasm-import-module
  Returns the module name of `record`. `record` is an import.
  |#
  #|proc:wasm-import-name
  Returns the item name of `record`. `record` is an import.
  |#
  #|proc:wasm-import-external-type
  Returns the external type of `record`. `record` is an import.
  |#
  (define-checked-record-type $wasm-import make-wasm-import wasm-import?
    $make-wasm-import $wasm-import?
    ([module wasm-import-module $wasm-import-module string?]
     [name wasm-import-name $wasm-import-name string?]
     [external-type wasm-import-external-type $wasm-import-external-type wasm-external-type?]))

  #|proc:make-wasm-function
  Creates a function. `type-index`, `locals`, and `body` supply its corresponding fields.
  |#
  #|proc:wasm-function?
  Returns whether `object` is a function. `object` is the value to test.
  |#
  #|proc:wasm-function-type-index
  Returns the type index of `record`. `record` is a function.
  |#
  #|proc:wasm-function-locals
  Returns the local types of `record`. `record` is a function.
  |#
  #|proc:wasm-function-body
  Returns the instruction body of `record`. `record` is a function.
  |#
  (define-checked-record-type $wasm-function make-wasm-function wasm-function?
    $make-wasm-function $wasm-function?
    ([type-index wasm-function-type-index $wasm-function-type-index natural?]
     [locals wasm-function-locals $wasm-function-locals (vector-of? value-type?)]
     [body wasm-function-body $wasm-function-body (vector-of? wasm-instruction?)]))

  #|proc:make-wasm-table
  Creates a table. `type` is its table type and `initializer` is its optional expression.
  |#
  #|proc:wasm-table?
  Returns whether `object` is a table. `object` is the value to test.
  |#
  #|proc:wasm-table-type
  Returns the table type of `record`. `record` is a table.
  |#
  #|proc:wasm-table-initializer
  Returns the optional initializer of `record`. `record` is a table.
  |#
  (define-checked-record-type $wasm-table make-wasm-table wasm-table?
    $make-wasm-table $wasm-table?
    ([type wasm-table-type $wasm-table-entity-type wasm-table-type?]
     [initializer wasm-table-initializer $wasm-table-initializer
                  optional-instruction-vector?]))

  #|proc:make-wasm-memory
  Creates a memory. `type` is its memory type.
  |#
  #|proc:wasm-memory?
  Returns whether `object` is a memory. `object` is the value to test.
  |#
  #|proc:wasm-memory-type
  Returns the memory type of `record`. `record` is a memory.
  |#
  (define-checked-record-type $wasm-memory make-wasm-memory wasm-memory?
    $make-wasm-memory $wasm-memory?
    ([type wasm-memory-type $wasm-memory-entity-type wasm-memory-type?]))

  #|proc:make-wasm-global
  Creates a global. `type` is its global type and `initializer` is its instruction expression.
  |#
  #|proc:wasm-global?
  Returns whether `object` is a global. `object` is the value to test.
  |#
  #|proc:wasm-global-type
  Returns the global type of `record`. `record` is a global.
  |#
  #|proc:wasm-global-initializer
  Returns the initializer of `record`. `record` is a global.
  |#
  (define-checked-record-type $wasm-global make-wasm-global wasm-global?
    $make-wasm-global $wasm-global?
    ([type wasm-global-type $wasm-global-entity-type wasm-global-type?]
     [initializer wasm-global-initializer $wasm-global-initializer instruction-vector?]))

  #|proc:make-wasm-tag
  Creates a tag. `type` is its tag type.
  |#
  #|proc:wasm-tag?
  Returns whether `object` is a tag. `object` is the value to test.
  |#
  #|proc:wasm-tag-type
  Returns the tag type of `record`. `record` is a tag.
  |#
  (define-checked-record-type $wasm-tag make-wasm-tag wasm-tag?
    $make-wasm-tag $wasm-tag?
    ([type wasm-tag-type $wasm-tag-entity-type wasm-tag-type?]))

  #|proc:make-wasm-export
  Creates an export. `name` is its name, `kind` its namespace, and `index` its entity index.
  |#
  #|proc:wasm-export?
  Returns whether `object` is an export. `object` is the value to test.
  |#
  #|proc:wasm-export-name
  Returns the name of `record`. `record` is an export.
  |#
  #|proc:wasm-export-kind
  Returns the kind of `record`. `record` is an export.
  |#
  #|proc:wasm-export-index
  Returns the entity index of `record`. `record` is an export.
  |#
  (define-checked-record-type $wasm-export make-wasm-export wasm-export?
    $make-wasm-export $wasm-export?
    ([name wasm-export-name $wasm-export-name string?]
     [kind wasm-export-kind $wasm-export-kind external-kind?]
     [index wasm-export-index $wasm-export-index natural?]))

  #|proc:make-wasm-element
  Creates an element segment. `mode`, `reference-type`, `table-index`, `offset`, and
  `initializers` supply its fields.
  |#
  #|proc:wasm-element?
  Returns whether `object` is an element segment. `object` is the value to test.
  |#
  #|proc:wasm-element-mode
  Returns the mode of `record`. `record` is an element segment.
  |#
  #|proc:wasm-element-reference-type
  Returns the reference type of `record`. `record` is an element segment.
  |#
  #|proc:wasm-element-table-index
  Returns the optional table index of `record`. `record` is an element segment.
  |#
  #|proc:wasm-element-offset
  Returns the optional offset expression of `record`. `record` is an element segment.
  |#
  #|proc:wasm-element-initializers
  Returns the initializer expressions of `record`. `record` is an element segment.
  |#
  (define-checked-record-type $wasm-element make-wasm-element wasm-element?
    $make-wasm-element $wasm-element?
    ([mode wasm-element-mode $wasm-element-mode element-mode?]
     [reference-type wasm-element-reference-type $wasm-element-reference-type
                     wasm-reference-type?]
     [table-index wasm-element-table-index $wasm-element-table-index optional-natural?]
     [offset wasm-element-offset $wasm-element-offset optional-instruction-vector?]
     [initializers wasm-element-initializers $wasm-element-initializers
                   (vector-of? instruction-vector?)]))

  #|proc:make-wasm-data
  Creates a data segment. `mode`, `memory-index`, `offset`, and `bytes` supply its fields.
  |#
  #|proc:wasm-data?
  Returns whether `object` is a data segment. `object` is the value to test.
  |#
  #|proc:wasm-data-mode
  Returns the mode of `record`. `record` is a data segment.
  |#
  #|proc:wasm-data-memory-index
  Returns the optional memory index of `record`. `record` is a data segment.
  |#
  #|proc:wasm-data-offset
  Returns the optional offset expression of `record`. `record` is a data segment.
  |#
  #|proc:wasm-data-bytes
  Returns the bytes of `record`. `record` is a data segment.
  |#
  (define-checked-record-type $wasm-data make-wasm-data wasm-data?
    $make-wasm-data $wasm-data?
    ([mode wasm-data-mode $wasm-data-mode data-mode?]
     [memory-index wasm-data-memory-index $wasm-data-memory-index optional-natural?]
     [offset wasm-data-offset $wasm-data-offset optional-instruction-vector?]
     [bytes wasm-data-bytes $wasm-data-bytes bytevector?]))

  #|proc:make-wasm-instruction
  Creates an instruction. `mnemonic`, `immediates`, `body`, and `alternate` supply its fields.
  |#
  #|proc:wasm-instruction?
  Returns whether `object` is an instruction. `object` is the value to test.
  |#
  #|proc:wasm-instruction-mnemonic
  Returns the mnemonic of `record`. `record` is an instruction.
  |#
  #|proc:wasm-instruction-immediates
  Returns the immediates of `record`. `record` is an instruction.
  |#
  #|proc:wasm-instruction-body
  Returns the nested body of `record`. `record` is an instruction.
  |#
  #|proc:wasm-instruction-alternate
  Returns the alternate body or catch clauses of `record`. `record` is an instruction.
  |#
  (define-checked-record-type $wasm-instruction
    make-wasm-instruction wasm-instruction?
    $make-wasm-instruction $wasm-instruction?
    ([mnemonic wasm-instruction-mnemonic $wasm-instruction-mnemonic symbol?]
     [immediates wasm-instruction-immediates $wasm-instruction-immediates vector?]
     [body wasm-instruction-body $wasm-instruction-body instruction-vector?]
     [alternate wasm-instruction-alternate $wasm-instruction-alternate
                instruction-alternate?]))

  #|proc:make-wasm-memory-argument
  Creates a memory argument. `alignment`, `offset`, and `memory-index` supply its fields.
  |#
  #|proc:wasm-memory-argument?
  Returns whether `object` is a memory argument. `object` is the value to test.
  |#
  #|proc:wasm-memory-argument-alignment
  Returns the alignment exponent of `record`. `record` is a memory argument.
  |#
  #|proc:wasm-memory-argument-offset
  Returns the address offset of `record`. `record` is a memory argument.
  |#
  #|proc:wasm-memory-argument-memory-index
  Returns the memory index of `record`. `record` is a memory argument.
  |#
  (define-checked-record-type $wasm-memory-argument
    make-wasm-memory-argument wasm-memory-argument?
    $make-wasm-memory-argument $wasm-memory-argument?
    ([alignment wasm-memory-argument-alignment $wasm-memory-argument-alignment natural?]
     [offset wasm-memory-argument-offset $wasm-memory-argument-offset natural?]
     [memory-index wasm-memory-argument-memory-index $wasm-memory-argument-memory-index natural?]))

  #|proc:make-wasm-block-type
  Creates a block type. `kind` identifies the form and `value` stores its optional payload.
  |#
  #|proc:wasm-block-type?
  Returns whether `object` is a block type. `object` is the value to test.
  |#
  #|proc:wasm-block-type-kind
  Returns the kind of `record`. `record` is a block type.
  |#
  #|proc:wasm-block-type-value
  Returns the optional payload of `record`. `record` is a block type.
  |#
  (define-checked-record-type $wasm-block-type make-wasm-block-type wasm-block-type?
    $make-wasm-block-type $wasm-block-type?
    ([kind wasm-block-type-kind $wasm-block-type-kind block-kind?]
     [value wasm-block-type-value $wasm-block-type-value
            (lambda (value) (or (not value) (natural? value) (value-type? value)))]))

  #|proc:make-wasm-catch
  Creates a catch clause. `kind`, optional `tag-index`, and `label-index` supply its fields.
  |#
  #|proc:wasm-catch?
  Returns whether `object` is a catch clause. `object` is the value to test.
  |#
  #|proc:wasm-catch-kind
  Returns the kind of `record`. `record` is a catch clause.
  |#
  #|proc:wasm-catch-tag-index
  Returns the optional tag index of `record`. `record` is a catch clause.
  |#
  #|proc:wasm-catch-label-index
  Returns the label index of `record`. `record` is a catch clause.
  |#
  (define-checked-record-type $wasm-catch make-wasm-catch wasm-catch?
    $make-wasm-catch $wasm-catch?
    ([kind wasm-catch-kind $wasm-catch-kind catch-kind?]
     [tag-index wasm-catch-tag-index $wasm-catch-tag-index optional-natural?]
     [label-index wasm-catch-label-index $wasm-catch-label-index natural?]))

  #|proc:make-wasm-float
  Creates a floating constant. `width` is 32 or 64 and `bits` is its unsigned bit pattern.
  |#
  #|proc:wasm-float?
  Returns whether `object` is a floating constant. `object` is the value to test.
  |#
  #|proc:wasm-float-width
  Returns the bit width of `record`. `record` is a floating constant.
  |#
  #|proc:wasm-float-bits
  Returns the unsigned bit pattern of `record`. `record` is a floating constant.
  |#
  (define-checked-record-type $wasm-float make-wasm-float wasm-float?
    $make-wasm-float $wasm-float?
    ([width wasm-float-width $wasm-float-width float-width?]
     [bits wasm-float-bits $wasm-float-bits natural?]))

  )
