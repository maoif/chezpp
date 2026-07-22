(import (chezpp)
        (chezpp parser wasm)
        (chezpp parser wasm binary values)
        (chezpp parser wasm binary types))

(define parse-binary
  (lambda (parser bytes)
    (run-binary-parser (<~0> parser <eof>) bytes)))

(mat wasm-binary-values-and-types

     (= #xffffffff
        (run-binary-parser <wasm-u32>
                           (bytevector #xff #xff #xff #xff #x0f)))

     (= -2147483648
        (run-binary-parser <wasm-s32>
                           (bytevector #x80 #x80 #x80 #x80 #x78)))

     ;; A non-minimal encoding is legal when it stays within ceil(N / 7) bytes.
     (= 0 (run-binary-parser <wasm-u32> (bytevector #x80 #x00)))

     ;; error: u32 has nonzero unused bits in its fifth byte.
     (error? (run-binary-parser <wasm-u32>
                                (bytevector #xff #xff #xff #xff #x10)))

     ;; error: u32 cannot occupy six bytes.
     (error? (run-binary-parser <wasm-u32>
                                (bytevector #x80 #x80 #x80 #x80 #x80 #x00)))

     (let ([type (run-binary-parser <wasm-reference-type> (bytevector #x64 #x70))])
       (and (wasm-reference-type? type)
            (not (wasm-reference-type-nullable? type))
            (eq? 'func (wasm-reference-type-heap-type type))))

     (let ([limits (run-binary-parser <wasm-limits>
                                      (bytevector #x05 #x02 #x09))])
       (and (eq? 'i64 (wasm-limits-address-type limits))
            (= 2 (wasm-limits-minimum limits))
            (= 9 (wasm-limits-maximum limits))))

     )

(mat wasm-binary-integer-boundaries

     (= 0 (parse-binary <wasm-u32> #vu8(0)))

     (= #xffffffffffffffff
        (parse-binary <wasm-u64>
                      (bytevector #xff #xff #xff #xff #xff #xff #xff #xff #xff #x01)))

     (= 0 (parse-binary <wasm-u64> #vu8(128 0)))

     (= 2147483647
        (parse-binary <wasm-s32> #vu8(255 255 255 255 7)))

     (= -2147483648
        (parse-binary <wasm-s32> #vu8(128 128 128 128 120)))

     (= 1 (parse-binary <wasm-s32> #vu8(129 0)))

     (= -1 (parse-binary <wasm-s32> #vu8(255 127)))

     (= #xffffffff
        (parse-binary <wasm-s33> #vu8(255 255 255 255 15)))

     (= -4294967296
        (parse-binary <wasm-s33> #vu8(128 128 128 128 112)))

     (= 1 (parse-binary <wasm-s33> #vu8(129 0)))

     (= -1 (parse-binary <wasm-s33> #vu8(255 127)))

     (= #x7fffffffffffffff
        (parse-binary <wasm-s64>
                      #vu8(255 255 255 255 255 255 255 255 255 0)))

     (= -9223372036854775808
        (parse-binary <wasm-s64>
                      #vu8(128 128 128 128 128 128 128 128 128 127)))

     (= 1 (parse-binary <wasm-s64> #vu8(129 0)))

     (= -1 (parse-binary <wasm-s64> #vu8(255 127)))

     ;; error: a u32 input cannot end while the continuation bit is set.
     (error? (parse-binary <wasm-u32> #vu8(128)))

     ;; error: a u64 tenth byte may only carry its one remaining value bit.
     (error? (parse-binary <wasm-u64>
                           #vu8(128 128 128 128 128 128 128 128 128 2)))

     ;; error: a u64 encoding cannot continue beyond its tenth byte.
     (error? (parse-binary <wasm-u64>
                           #vu8(128 128 128 128 128 128 128 128 128 128 0)))

     ;; error: an s32 positive final byte has nonzero unused high bits.
     (error? (parse-binary <wasm-s32> #vu8(128 128 128 128 8)))

     ;; error: an s32 negative final byte has inconsistent unused high bits.
     (error? (parse-binary <wasm-s32> #vu8(255 255 255 255 119)))

     ;; error: an s33 encoding cannot continue beyond its fifth byte.
     (error? (parse-binary <wasm-s33> #vu8(128 128 128 128 128 0)))

     ;; error: an s33 positive final byte has nonzero unused high bits.
     (error? (parse-binary <wasm-s33> #vu8(128 128 128 128 16)))

     ;; error: an s33 negative final byte has inconsistent unused high bits.
     (error? (parse-binary <wasm-s33> #vu8(255 255 255 255 111)))

     ;; error: an s64 input cannot end while the continuation bit is set.
     (error? (parse-binary <wasm-s64> #vu8(128)))

     ;; error: an s64 positive final byte has nonzero unused high bits.
     (error? (parse-binary <wasm-s64>
                           #vu8(128 128 128 128 128 128 128 128 128 2)))

     ;; error: an s64 negative final byte has inconsistent unused high bits.
     (error? (parse-binary <wasm-s64>
                           #vu8(128 128 128 128 128 128 128 128 128 125)))

     ;; error: an s64 encoding cannot continue beyond its tenth byte.
     (error? (parse-binary <wasm-s64>
                           #vu8(128 128 128 128 128 128 128 128 128 128 0)))

     )

(mat wasm-binary-scalars-and-vectors

     (let ([value (parse-binary <wasm-f32> #vu8(1 0 192 127))])
       (and (= 32 (wasm-float-width value))
            (= #x7fc00001 (wasm-float-bits value))))

     (let ([value (parse-binary <wasm-f64> #vu8(1 0 0 0 0 0 248 127))])
       (and (= 64 (wasm-float-width value))
            (= #x7ff8000000000001 (wasm-float-bits value))))

     (equal? #vu8() (parse-binary <wasm-byte-vector> #vu8(0)))

     (equal? #vu8(1 2 255)
             (parse-binary <wasm-byte-vector> #vu8(3 1 2 255)))

     (string=? "wasm" (parse-binary <wasm-name> #vu8(4 119 97 115 109)))

     (string=? (string (integer->char #x4e2d))
               (parse-binary <wasm-name> #vu8(3 228 184 173)))

     (equal? '#() (parse-binary (<wasm-vector> <wasm-u32>) #vu8(0)))

     (equal? '#(1 300)
             (parse-binary (<wasm-vector> <wasm-u32>) #vu8(2 1 172 2)))

     ;; error: the vector parser constructor requires an element parser.
     (error? (<wasm-vector> 'not-a-parser))

     ;; error: a byte vector must contain the declared number of bytes.
     (error? (parse-binary <wasm-byte-vector> #vu8(2 1)))

     ;; error: overlong UTF-8 is not a valid WebAssembly name.
     (error? (parse-binary <wasm-name> #vu8(2 192 175)))

     )

(mat wasm-binary-basic-types

     (equal? '(i32 i64 f32 f64)
             (map (lambda (byte)
                    (parse-binary <wasm-number-type> (bytevector byte)))
                  '(#x7f #x7e #x7d #x7c)))

     (eq? 'v128 (parse-binary <wasm-vector-type> #vu8(123)))

     (equal? '(func extern any eq i31 struct array none nofunc noextern exn noexn)
             (map (lambda (byte)
                    (parse-binary <wasm-heap-type> (bytevector byte)))
                  '(#x70 #x6f #x6e #x6d #x6c #x6b #x6a #x69 #x68 #x67 #x66 #x65)))

     (= 3 (parse-binary <wasm-heap-type> #vu8(3)))

     (let ([type (parse-binary <wasm-reference-type> #vu8(99 105))])
       (and (wasm-reference-type-nullable? type)
            (eq? 'none (wasm-reference-type-heap-type type))))

     (let ([type (parse-binary <wasm-reference-type> #vu8(100 3))])
       (and (not (wasm-reference-type-nullable? type))
            (= 3 (wasm-reference-type-heap-type type))))

     (equal? '(func extern any eq i31 struct array none nofunc noextern exn noexn)
             (map (lambda (byte)
                    (wasm-reference-type-heap-type
                     (parse-binary <wasm-value-type> (bytevector byte))))
                  '(#x70 #x6f #x6e #x6d #x6c #x6b #x6a #x69 #x68 #x67 #x66 #x65)))

     (equal? '(i8 i16 i32 v128)
             (map (lambda (byte)
                    (parse-binary <wasm-storage-type> (bytevector byte)))
                  '(#x78 #x77 #x7f #x7b)))

     ;; error: an unassigned negative s33 value is not a heap type.
     (error? (parse-binary <wasm-heap-type> #vu8(100)))

     ;; error: an unknown leading byte is not a value type.
     (error? (parse-binary <wasm-value-type> #vu8(97)))

     )

(mat wasm-binary-composite-types

     (let ([type (parse-binary <wasm-function-type> #vu8(96 2 127 112 1 126))])
       (and (equal? '#(i64) (wasm-function-type-results type))
            (= 2 (vector-length (wasm-function-type-parameters type)))
            (eq? 'i32 (vector-ref (wasm-function-type-parameters type) 0))
            (wasm-reference-type?
             (vector-ref (wasm-function-type-parameters type) 1))))

     (let* ([type (parse-binary <wasm-struct-type> #vu8(95 2 120 1 126 0))]
            [fields (wasm-struct-type-fields type)])
       (and (= 2 (vector-length fields))
            (eq? 'i8 (wasm-field-type-storage-type (vector-ref fields 0)))
            (wasm-field-type-mutable? (vector-ref fields 0))
            (eq? 'i64 (wasm-field-type-storage-type (vector-ref fields 1)))
            (not (wasm-field-type-mutable? (vector-ref fields 1)))))

     (let* ([type (parse-binary <wasm-array-type> #vu8(94 119 0))]
            [field (wasm-array-type-field type)])
       (and (eq? 'i16 (wasm-field-type-storage-type field))
            (not (wasm-field-type-mutable? field))))

     (let ([type (parse-binary <wasm-subtype> #vu8(79 1 3 96 0 0))])
       (and (wasm-subtype-final? type)
            (equal? '#(3) (wasm-subtype-supertypes type))
            (wasm-function-type? (wasm-subtype-composite-type type))))

     (let ([type (parse-binary <wasm-subtype> #vu8(80 0 94 127 1))])
       (and (not (wasm-subtype-final? type))
            (equal? '#() (wasm-subtype-supertypes type))
            (wasm-array-type? (wasm-subtype-composite-type type))))

     (let ([type (parse-binary <wasm-subtype> #vu8(96 0 0))])
       (and (wasm-subtype-final? type)
            (equal? '#() (wasm-subtype-supertypes type))))

     (let* ([type (parse-binary <wasm-recursive-type>
                                #vu8(78 2 96 0 0 94 120 1))]
            [subtypes (wasm-recursive-type-subtypes type)])
       (and (= 2 (vector-length subtypes))
            (wasm-function-type?
             (wasm-subtype-composite-type (vector-ref subtypes 0)))
            (wasm-array-type?
             (wasm-subtype-composite-type (vector-ref subtypes 1)))))

     (= 1
        (vector-length
         (wasm-recursive-type-subtypes
          (parse-binary <wasm-recursive-type> #vu8(96 0 0)))))

     ;; error: mutability is encoded by exactly zero or one.
     (error? (parse-binary <wasm-field-type> #vu8(127 2)))

     ;; error: an explicit recursive group must contain every declared subtype.
     (error? (parse-binary <wasm-recursive-type> #vu8(78 2 96 0 0)))

     ;; error: a recursive group cannot end in a truncated composite type.
     (error? (parse-binary <wasm-recursive-type> #vu8(78 1 96 0)))

     )

(mat wasm-binary-administrative-types

     (eq? 'empty
          (wasm-block-type-kind (parse-binary <wasm-block-type> #vu8(64))))

     (eq? 'value-type
          (wasm-block-type-kind (parse-binary <wasm-block-type> #vu8(127))))

     (= 3 (wasm-block-type-value (parse-binary <wasm-block-type> #vu8(3))))

     (let ([type (parse-binary <wasm-global-type> #vu8(126 1))])
       (and (eq? 'i64 (wasm-global-type-value-type type))
            (wasm-global-type-mutable? type)))

     (let ([type (parse-binary <wasm-table-type> #vu8(112 1 2 9))])
       (and (eq? 'func
                 (wasm-reference-type-heap-type
                  (wasm-table-type-reference-type type)))
            (= 2 (wasm-limits-minimum (wasm-table-type-limits type)))
            (= 9 (wasm-limits-maximum (wasm-table-type-limits type)))))

     (let ([type (parse-binary <wasm-memory-type> #vu8(4 7))])
       (and (eq? 'i64
                 (wasm-limits-address-type (wasm-memory-type-limits type)))
            (= 7 (wasm-limits-minimum (wasm-memory-type-limits type)))
            (not (wasm-limits-maximum (wasm-memory-type-limits type)))))

     (= 4 (wasm-tag-type-type-index
           (parse-binary <wasm-tag-type> #vu8(0 4))))

     (equal? '(function table memory global tag)
             (map (lambda (bytes)
                    (wasm-external-type-kind
                     (parse-binary <wasm-external-type> bytes)))
                  (list #vu8(0 2) #vu8(1 112 0 1) #vu8(2 0 1)
                        #vu8(3 127 0) #vu8(4 0 2))))

     (let ([function (parse-binary <wasm-external-type> #vu8(0 2))]
           [table (parse-binary <wasm-external-type> #vu8(1 112 0 1))]
           [memory (parse-binary <wasm-external-type> #vu8(2 0 1))]
           [global (parse-binary <wasm-external-type> #vu8(3 127 0))]
           [tag (parse-binary <wasm-external-type> #vu8(4 0 2))])
       (and (= 2 (wasm-external-type-type function))
            (wasm-table-type? (wasm-external-type-type table))
            (wasm-memory-type? (wasm-external-type-type memory))
            (wasm-global-type? (wasm-external-type-type global))
            (wasm-tag-type? (wasm-external-type-type tag))
            (= 2
               (wasm-tag-type-type-index
                (wasm-external-type-type tag)))))

     ;; error: a negative s33 value cannot be a block type index.
     (error? (parse-binary <wasm-block-type> #vu8(100)))

     ;; error: global mutability is encoded by exactly zero or one.
     (error? (parse-binary <wasm-global-type> #vu8(127 2)))

     ;; error: the tag attribute must be zero in Core 3.0.
     (error? (parse-binary <wasm-tag-type> #vu8(1 0)))

     ;; error: limits flags outside 0, 1, 4, and 5 are invalid.
     (error? (parse-binary <wasm-limits> #vu8(2 0)))

     ;; error: a limits maximum cannot be lower than its minimum.
     (error? (parse-binary <wasm-limits> #vu8(1 9 2)))

     ;; error: external type kind five is undefined.
     (error? (parse-binary <wasm-external-type> #vu8(5)))

     )

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
