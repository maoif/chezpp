(library (chezpp parser wasm binary instructions)
  (export <wasm-memory-argument> <wasm-instruction> <wasm-expression>)
  (import (chezpp chez)
          (chezpp parser combinator)
          (chezpp parser wasm binary types)
          (chezpp parser wasm binary values)
          (chezpp parser wasm opcodes)
          (chezpp parser wasm types))

  (define empty-vector '#())

  (define list->immutable-vector
    (lambda (value*)
      (let* ([length (length value*)]
             [values (make-vector length)])
        (let loop ([index 0] [value* value*])
          (unless (null? value*)
            (vector-set! values index (car value*))
            (loop (fx1+ index) (cdr value*))))
        (vector->immutable-vector values))))

  (define make-immutable-vector
    (lambda value*
      (list->immutable-vector value*)))

  (define one-immediate
    (lambda (parser)
      (<map> make-immutable-vector parser)))

  (define pair-immediates
    (lambda (first-parser second-parser)
      (<map> list->immutable-vector (<~> first-parser second-parser))))

  (define reversed-pair-immediates
    (lambda (first-parser second-parser)
      (<map> (lambda (values)
               (make-immutable-vector (cadr values) (car values)))
             (<~> first-parser second-parser))))

  (define instruction-record
    (lambda (descriptor immediates)
      (make-wasm-instruction
       (wasm-opcode-mnemonic descriptor) immediates empty-vector empty-vector)))

  (define opcode-descriptor-parser
    (<bind>
     <u8>
     (lambda (opcode)
       (if (memv opcode '(#xfb #xfc #xfd))
           (<bind> <wasm-u32>
                   (lambda (subopcode)
                     (let ([descriptor (wasm-opcode-by-binary opcode subopcode)])
                       (if descriptor
                           (<result> descriptor)
                           (<fail-with> "unknown WebAssembly prefixed opcode")))))
           (let ([descriptor (wasm-opcode-by-binary #f opcode)])
             (if descriptor
                 (<result> descriptor)
                 (<fail-with> "unknown WebAssembly opcode")))))))

  #|proc:<wasm-memory-argument>
  The `<wasm-memory-argument>` parser reads a Core 3.0 alignment, memory index, and offset.
  |#
  (define <wasm-memory-argument>
    (<bind>
     <wasm-u32>
     (lambda (flags)
       (cond [(< flags 64)
              (<map> (lambda (offset)
                       (make-wasm-memory-argument flags offset 0))
                     <wasm-u64>)]
             [(< flags 128)
              (<map> (lambda (values)
                       (make-wasm-memory-argument
                        (- flags 64) (cadr values) (car values)))
                     (<~> <wasm-u32> <wasm-u64>))]
             [else
              (<fail-with> "invalid WebAssembly memory argument flags")]))))

  (define catch-parser
    (<bind>
     <u8>
     (lambda (kind)
       (case kind
         [(0)
          (<map> (lambda (values)
                   (make-wasm-catch 'catch (car values) (cadr values)))
                 (<~> <wasm-u32> <wasm-u32>))]
         [(1)
          (<map> (lambda (values)
                   (make-wasm-catch 'catch-ref (car values) (cadr values)))
                 (<~> <wasm-u32> <wasm-u32>))]
         [(2)
          (<map> (lambda (label-index)
                   (make-wasm-catch 'catch-all #f label-index))
                 <wasm-u32>)]
         [(3)
          (<map> (lambda (label-index)
                   (make-wasm-catch 'catch-all-ref #f label-index))
                 <wasm-u32>)]
         [else (<fail-with> "invalid WebAssembly catch kind")]))))

  (define immediate-parser
    (lambda (shape)
      (case shape
        [(none) (<result> empty-vector)]
        [(block-type) (one-immediate <wasm-block-type>)]
        [(label-index function-index type-index table-index memory-index global-index
                      local-index tag-index data-index element-index)
         (one-immediate <wasm-u32>)]
        [(label-vector)
         (<map> (lambda (values)
                  (make-immutable-vector
                   (vector->immutable-vector (car values)) (cadr values)))
                (<~> (<wasm-vector> <wasm-u32>) <wasm-u32>))]
        [(heap-type) (one-immediate <wasm-heap-type>)]
        [(value-type-vector select-types)
         (<map> (lambda (types)
                  (make-immutable-vector (vector->immutable-vector types)))
                (<wasm-vector> <wasm-value-type>))]
        [(call-indirect) (pair-immediates <wasm-u32> <wasm-u32>)]
        [(memory-argument) (one-immediate <wasm-memory-argument>)]
        [(i32) (one-immediate <wasm-s32>)]
        [(i64) (one-immediate <wasm-s64>)]
        [(f32) (one-immediate <wasm-f32>)]
        [(f64) (one-immediate <wasm-f64>)]
        [(table-pair memory-pair) (pair-immediates <wasm-u32> <wasm-u32>)]
        [(memory-data table-element)
         (reversed-pair-immediates <wasm-u32> <wasm-u32>)]
        [(struct-field array-new-fixed array-copy type-data type-element
                       heap-type-non-null heap-type-nullable br-on-cast
                       vector-bytes shuffle-bytes lane-index memory-argument-lane)
         (<fail-with> (format "unsupported immediate shape: ~a" shape))]
        [(try-table)
         (<fail-with> "try-table immediate must be parsed structurally")]
        [else (<fail-with> (format "unsupported immediate shape: ~a" shape))])))

  #|proc:<wasm-instruction>
  The `<wasm-instruction>` parser reads one scalar or structured Core 3.0 instruction.
  |#
  (declare-lazy-parser <wasm-instruction>)

  (define sequence-before
    (lambda (terminator-parser)
      (<map> list->immutable-vector
             (<many-until> <wasm-instruction> terminator-parser))))

  (define sequence-ending-with-end
    (<~ (sequence-before (<uimm8> #x0b)) (<uimm8> #x0b)))

  (define block-instruction-parser
    (lambda (descriptor)
      (<map>
       (lambda (values)
         (make-wasm-instruction
          (wasm-opcode-mnemonic descriptor)
          (make-immutable-vector (car values))
          (cadr values)
          empty-vector))
       (<~> <wasm-block-type> sequence-ending-with-end))))

  (define if-instruction-parser
    (lambda (descriptor)
      (<bind>
       <wasm-block-type>
       (lambda (block-type)
         (<bind>
          (sequence-before (</> (<uimm8> #x05) (<uimm8> #x0b)))
          (lambda (body)
            (</>
             (<map> (lambda (alternate)
                      (make-wasm-instruction
                       (wasm-opcode-mnemonic descriptor)
                       (make-immutable-vector block-type)
                       body
                       alternate))
                    (<~1> (<uimm8> #x05) sequence-ending-with-end))
             (<as> (make-wasm-instruction
                    (wasm-opcode-mnemonic descriptor)
                    (make-immutable-vector block-type)
                    body
                    empty-vector)
                   (<uimm8> #x0b)))))))))

  (define try-table-instruction-parser
    (lambda (descriptor)
      (<bind>
       <wasm-block-type>
       (lambda (block-type)
         (<bind>
          (<wasm-vector> catch-parser)
          (lambda (catches)
            (<map> (lambda (body)
                     (make-wasm-instruction
                      (wasm-opcode-mnemonic descriptor)
                      (make-immutable-vector block-type)
                      body
                      (vector->immutable-vector catches)))
                   sequence-ending-with-end)))))))

  (define structured-instruction-parser
    (lambda (descriptor)
      (case (wasm-opcode-structured-kind descriptor)
        [(block loop) (block-instruction-parser descriptor)]
        [(if) (if-instruction-parser descriptor)]
        [(try-table) (try-table-instruction-parser descriptor)]
        [else (<fail-with> "unsupported structured WebAssembly instruction")])))

  (define ordinary-instruction-parser
    (lambda (descriptor)
      (let ([mnemonic (wasm-opcode-mnemonic descriptor)])
        (cond [(memq mnemonic '(else end))
               (<fail-with> "unexpected WebAssembly instruction terminator")]
              [(wasm-opcode-structured-kind descriptor)
               (structured-instruction-parser descriptor)]
              [else
               (<map> (lambda (immediates)
                        (instruction-record descriptor immediates))
                      (immediate-parser
                       (wasm-opcode-immediate-shape descriptor)))]))))

  #|proc:<wasm-expression>
  The `<wasm-expression>` parser reads an instruction vector terminated by one end byte.
  |#
  (define <wasm-expression> sequence-ending-with-end)

  (install-lazy-parser!
   <wasm-instruction>
   (<bind> opcode-descriptor-parser ordinary-instruction-parser))

  )
