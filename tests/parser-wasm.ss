(import (chezpp)
        (chezpp parser wasm))

(mat wasm-records

     (let* ([limits (make-wasm-limits 'i64 2 9)]
            [memory-type (make-wasm-memory-type limits)]
            [memory (make-wasm-memory memory-type)]
            [module (make-wasm-module '#() '#() '#() '#() (vector memory) '#() '#()
                                      '#() #f '#() '#() '#())])
       (and (wasm-module? module)
            (= 1 (vector-length (wasm-module-memories module)))
            (eq? 'i64 (wasm-limits-address-type
                       (wasm-memory-type-limits
                        (wasm-memory-type
                         (vector-ref (wasm-module-memories module) 0)))))
            (= 2 (wasm-limits-minimum limits))
            (= 9 (wasm-limits-maximum limits))))

     (let* ([reference-type (make-wasm-reference-type #t 'func)]
            [field-type (make-wasm-field-type 'i16 #t)]
            [parameters (vector 'i32 reference-type)]
            [results (vector 'i64)]
            [function-type (make-wasm-function-type parameters results)]
            [struct-fields (vector field-type)]
            [struct-type (make-wasm-struct-type struct-fields)]
            [array-type (make-wasm-array-type field-type)]
            [supertypes (vector 3)]
            [subtype (make-wasm-subtype #f supertypes function-type)]
            [subtypes (vector subtype)]
            [recursive-type (make-wasm-recursive-type subtypes)]
            [limits (make-wasm-limits 'i32 2 9)]
            [table-type (make-wasm-table-type reference-type limits)]
            [memory-type (make-wasm-memory-type limits)]
            [global-type (make-wasm-global-type 'i32 #t)]
            [tag-type (make-wasm-tag-type 4)]
            [external-type (make-wasm-external-type 'function 7)]
            [table-external (make-wasm-external-type 'table table-type)]
            [memory-external (make-wasm-external-type 'memory memory-type)]
            [global-external (make-wasm-external-type 'global global-type)]
            [tag-external (make-wasm-external-type 'tag tag-type)]
            [import (make-wasm-import "env" "callback" external-type)]
            [immediates (vector 7)]
            [leaf (make-wasm-instruction 'i32.const immediates '#() '#())]
            [catch (make-wasm-catch 'catch 4 2)]
            [instruction-body (vector leaf)]
            [instruction-alternate (vector leaf)]
            [instruction (make-wasm-instruction
                          'if '#() instruction-body instruction-alternate)]
            [catches (vector catch)]
            [try-table (make-wasm-instruction 'try-table '#() instruction-body catches)]
            [locals (vector 'i64)]
            [function-body (vector instruction try-table)]
            [function (make-wasm-function 0 locals function-body)]
            [initializer (vector leaf)]
            [table (make-wasm-table table-type initializer)]
            [memory (make-wasm-memory memory-type)]
            [global (make-wasm-global global-type initializer)]
            [tag (make-wasm-tag tag-type)]
            [export (make-wasm-export "run" 'function 0)]
            [element-initializers (vector initializer)]
            [active-element
             (make-wasm-element 'active reference-type 1 initializer element-initializers)]
            [passive-element
             (make-wasm-element 'passive reference-type #f #f element-initializers)]
            [declarative-element
             (make-wasm-element 'declarative reference-type #f #f element-initializers)]
            [data-bytes #vu8(1 2 3)]
            [active-data (make-wasm-data 'active 2 initializer data-bytes)]
            [passive-data (make-wasm-data 'passive #f #f data-bytes)]
            [custom-bytes #vu8(9 8)]
            [custom-section (make-wasm-custom-section "meta" custom-bytes 'type)]
            [memory-argument (make-wasm-memory-argument 2 16 1)]
            [block-type (make-wasm-block-type 'value-type reference-type)]
            [empty-block-type (make-wasm-block-type 'empty #f)]
            [indexed-block-type (make-wasm-block-type 'type-index 3)]
            [catch-ref (make-wasm-catch 'catch-ref 5 3)]
            [catch-all (make-wasm-catch 'catch-all #f 4)]
            [catch-all-ref (make-wasm-catch 'catch-all-ref #f 5)]
            [float32 (make-wasm-float 32 #xffffffff)]
            [float (make-wasm-float 64 #xffffffffffffffff)]
            [types (vector recursive-type)]
            [imports (vector import)]
            [functions (vector function)]
            [tables (vector table)]
            [memories (vector memory)]
            [globals (vector global)]
            [tags (vector tag)]
            [exports (vector export)]
            [elements (vector active-element passive-element declarative-element)]
            [data (vector active-data passive-data)]
            [custom-sections (vector custom-section)]
            [module (make-wasm-module types imports functions tables memories globals tags
                                      exports 0 elements data custom-sections)])
       (and (eq? types (wasm-module-types module))
            (eq? imports (wasm-module-imports module))
            (eq? functions (wasm-module-functions module))
            (eq? tables (wasm-module-tables module))
            (eq? memories (wasm-module-memories module))
            (eq? globals (wasm-module-globals module))
            (eq? tags (wasm-module-tags module))
            (eq? exports (wasm-module-exports module))
            (= 0 (wasm-module-start module))
            (eq? elements (wasm-module-elements module))
            (eq? data (wasm-module-data module))
            (eq? custom-sections (wasm-module-custom-sections module))
            (string=? "meta" (wasm-custom-section-name custom-section))
            (equal? custom-bytes (wasm-custom-section-bytes custom-section))
            (eq? 'type (wasm-custom-section-after-section custom-section))
            (eq? subtypes (wasm-recursive-type-subtypes recursive-type))
            (not (wasm-subtype-final? subtype))
            (eq? supertypes (wasm-subtype-supertypes subtype))
            (eq? function-type (wasm-subtype-composite-type subtype))
            (eq? parameters (wasm-function-type-parameters function-type))
            (eq? results (wasm-function-type-results function-type))
            (eq? struct-fields (wasm-struct-type-fields struct-type))
            (eq? field-type (wasm-array-type-field array-type))
            (eq? 'i16 (wasm-field-type-storage-type field-type))
            (wasm-field-type-mutable? field-type)
            (wasm-reference-type-nullable? reference-type)
            (eq? 'func (wasm-reference-type-heap-type reference-type))
            (eq? 'i32 (wasm-limits-address-type limits))
            (= 2 (wasm-limits-minimum limits))
            (= 9 (wasm-limits-maximum limits))
            (eq? reference-type (wasm-table-type-reference-type table-type))
            (eq? limits (wasm-table-type-limits table-type))
            (eq? limits (wasm-memory-type-limits memory-type))
            (eq? 'i32 (wasm-global-type-value-type global-type))
            (wasm-global-type-mutable? global-type)
            (= 4 (wasm-tag-type-type-index tag-type))
            (eq? 'function (wasm-external-type-kind external-type))
            (= 7 (wasm-external-type-type external-type))
            (eq? table-type (wasm-external-type-type table-external))
            (eq? memory-type (wasm-external-type-type memory-external))
            (eq? global-type (wasm-external-type-type global-external))
            (eq? tag-type (wasm-external-type-type tag-external))
            (string=? "env" (wasm-import-module import))
            (string=? "callback" (wasm-import-name import))
            (eq? external-type (wasm-import-external-type import))
            (= 0 (wasm-function-type-index function))
            (eq? locals (wasm-function-locals function))
            (eq? function-body (wasm-function-body function))
            (eq? table-type (wasm-table-type table))
            (eq? initializer (wasm-table-initializer table))
            (eq? memory-type (wasm-memory-type memory))
            (eq? global-type (wasm-global-type global))
            (eq? initializer (wasm-global-initializer global))
            (eq? tag-type (wasm-tag-type tag))
            (string=? "run" (wasm-export-name export))
            (eq? 'function (wasm-export-kind export))
            (= 0 (wasm-export-index export))
            (eq? 'active (wasm-element-mode active-element))
            (eq? reference-type (wasm-element-reference-type active-element))
            (= 1 (wasm-element-table-index active-element))
            (eq? initializer (wasm-element-offset active-element))
            (eq? element-initializers (wasm-element-initializers active-element))
            (eq? 'passive (wasm-element-mode passive-element))
            (not (wasm-element-table-index passive-element))
            (not (wasm-element-offset passive-element))
            (eq? 'declarative (wasm-element-mode declarative-element))
            (not (wasm-element-table-index declarative-element))
            (not (wasm-element-offset declarative-element))
            (eq? 'active (wasm-data-mode active-data))
            (= 2 (wasm-data-memory-index active-data))
            (eq? initializer (wasm-data-offset active-data))
            (equal? data-bytes (wasm-data-bytes active-data))
            (eq? 'passive (wasm-data-mode passive-data))
            (not (wasm-data-memory-index passive-data))
            (not (wasm-data-offset passive-data))
            (equal? data-bytes (wasm-data-bytes passive-data))
            (eq? 'if (wasm-instruction-mnemonic instruction))
            (equal? '#() (wasm-instruction-immediates instruction))
            (eq? instruction-body (wasm-instruction-body instruction))
            (eq? instruction-alternate (wasm-instruction-alternate instruction))
            (eq? catches (wasm-instruction-alternate try-table))
            (= 2 (wasm-memory-argument-alignment memory-argument))
            (= 16 (wasm-memory-argument-offset memory-argument))
            (= 1 (wasm-memory-argument-memory-index memory-argument))
            (eq? 'value-type (wasm-block-type-kind block-type))
            (eq? reference-type (wasm-block-type-value block-type))
            (not (wasm-block-type-value empty-block-type))
            (= 3 (wasm-block-type-value indexed-block-type))
            (eq? 'catch (wasm-catch-kind catch))
            (= 4 (wasm-catch-tag-index catch))
            (= 2 (wasm-catch-label-index catch))
            (= 5 (wasm-catch-tag-index catch-ref))
            (not (wasm-catch-tag-index catch-all))
            (not (wasm-catch-tag-index catch-all-ref))
            (= #xffffffff (wasm-float-bits float32))
            (= 64 (wasm-float-width float))
            (= #xffffffffffffffff (wasm-float-bits float))))

     ;; error: record accessors reject values of the wrong record type.
     (error? (wasm-module-types 'not-a-module))

     ;; error: limits require an i32 or i64 address type.
     (error? (make-wasm-limits 'f32 0 #f))

     ;; error: a memory external type requires a memory type record.
     (error? (make-wasm-external-type 'memory 0))

     ;; error: only a function external type can use a natural type index directly.
     (error? (make-wasm-external-type 'table 0))

     ;; error: a global external type requires a global type record.
     (error? (make-wasm-external-type 'global 0))

     ;; error: a tag external type requires a tag type record.
     (error? (make-wasm-external-type 'tag 0))

     ;; error: an empty block type cannot carry a value.
     (error? (make-wasm-block-type 'empty 'i32))

     ;; error: a value block type requires a WebAssembly value type.
     (error? (make-wasm-block-type 'value-type 0))

     ;; error: an indexed block type requires a natural type index.
     (error? (make-wasm-block-type 'type-index 'i32))

     ;; error: a tagged catch requires a natural tag index.
     (error? (make-wasm-catch 'catch #f 0))

     ;; error: a tagged reference catch requires a natural tag index.
     (error? (make-wasm-catch 'catch-ref #f 0))

     ;; error: a catch-all clause cannot carry a tag index.
     (error? (make-wasm-catch 'catch-all 0 0))

     ;; error: a catch-all reference clause cannot carry a tag index.
     (error? (make-wasm-catch 'catch-all-ref 0 0))

     ;; error: a 32-bit float bit pattern cannot exceed 32 bits.
     (error? (make-wasm-float 32 #x100000000))

     ;; error: a 64-bit float bit pattern cannot exceed 64 bits.
     (error? (make-wasm-float 64 #x10000000000000000))

     ;; error: a floating-point bit pattern cannot be negative.
     (error? (make-wasm-float 32 -1))

     ;; error: an active element segment requires a table index and offset expression.
     (error? (make-wasm-element 'active (make-wasm-reference-type #t 'func)
                                #f #f '#()))

     ;; error: a passive element segment cannot carry active placement fields.
     (error? (make-wasm-element 'passive (make-wasm-reference-type #t 'func)
                                0 '#() '#()))

     ;; error: an active data segment requires a memory index and offset expression.
     (error? (make-wasm-data 'active #f #f #vu8()))

     ;; error: a passive data segment cannot carry active placement fields.
     (error? (make-wasm-data 'passive 0 '#() #vu8()))

     ;; error: an instruction alternate cannot mix instructions and catch clauses.
     (error? (make-wasm-instruction
              'try-table '#() '#()
              (vector (make-wasm-instruction 'nop '#() '#() '#())
                      (make-wasm-catch 'catch 0 0))))

     ;; error: a present limits maximum cannot be less than its minimum.
     (error? (make-wasm-limits 'i32 9 2))

     )
