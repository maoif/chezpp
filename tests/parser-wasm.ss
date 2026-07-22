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

     (let* ([limits (make-wasm-limits 'i32 0 #f)]
            [reference-type (make-wasm-reference-type #t 'func)]
            [table-type (make-wasm-table-type reference-type limits)]
            [memory-type (make-wasm-memory-type limits)]
            [global-type (make-wasm-global-type 'i32 #f)]
            [tag-type (make-wasm-tag-type 0)])
       (and (wasm-external-type? (make-wasm-external-type 'function 0))
            (wasm-external-type? (make-wasm-external-type 'table table-type))
            (wasm-external-type? (make-wasm-external-type 'memory memory-type))
            (wasm-external-type? (make-wasm-external-type 'global global-type))
            (wasm-external-type? (make-wasm-external-type 'tag tag-type))
            (wasm-block-type? (make-wasm-block-type 'empty #f))
            (wasm-block-type? (make-wasm-block-type 'value-type 'i32))
            (wasm-block-type? (make-wasm-block-type 'type-index 0))
            (wasm-catch? (make-wasm-catch 'catch 0 0))
            (wasm-catch? (make-wasm-catch 'catch-ref 0 0))
            (wasm-catch? (make-wasm-catch 'catch-all #f 0))
            (wasm-catch? (make-wasm-catch 'catch-all-ref #f 0))
            (= #xffffffff (wasm-float-bits (make-wasm-float 32 #xffffffff)))
            (= #xffffffffffffffff
               (wasm-float-bits (make-wasm-float 64 #xffffffffffffffff)))))

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

     )
