(library (chezpp parser wasm text instructions)
  (export <wat-instruction> <wat-folded-instruction> <wat-expression>)
  (import (chezpp chez)
          (chezpp parser combinator)
          (chezpp parser wasm opcodes)
          (chezpp parser wasm text lexical)
          (chezpp parser wasm text types))

;;;;===----------------------------------------------------------------------===
;;;; Instruction tokens and immediate parsers
;;;;===----------------------------------------------------------------------===

  (define empty-vector '#())

  (define list->immutable-vector
    (lambda (value*)
      (vector->immutable-vector (list->vector value*))))

  (define make-immutable-vector
    (lambda value*
      (list->immutable-vector value*)))

  (define optional-value
    (lambda (value default)
      (if (null? value) default value)))

  (define instruction-character?
    (lambda (character)
      (or (char<=? #\0 character #\9)
          (char<=? #\a character #\z)
          (char<=? #\A character #\Z)
          (memv character '(#\. #\_ #\-)))))

  (define <wat-instruction-token>
    (<wat-token>
     (<as-string>
      (<some> (<satisfy-char> instruction-character?
                              "not a WebAssembly instruction character")))))

  (define text-mnemonic->symbol
    (lambda (text)
      (string->symbol
       (list->string
        (map (lambda (character)
               (if (char=? character #\_) #\- character))
             (string->list text))))))

  (define descriptor-parser
    (<bind>
     <wat-instruction-token>
     (lambda (text)
       (let ([descriptor (wasm-opcode-by-mnemonic (text-mnemonic->symbol text))])
         (if descriptor
             (<result> descriptor)
             (<fail-with> (format "unknown WebAssembly instruction: ~a" text)))))))

  (define one-immediate
    (lambda (parser)
      (<map> (lambda (value) (make-immutable-vector value)) parser)))

  (define two-immediates
    (lambda (first second)
      (<map> list->immutable-vector (<~> first second))))

  (define index-immediate (one-immediate <wat-index-reference>))

  (define indexed-pair-immediate
    (two-immediates <wat-index-reference> <wat-index-reference>))

  (define wat-open (<~0> (<char> #\() <wat-trivia>))
  (define wat-close (<~0> (<char> #\)) <wat-trivia>))

  (define parenthesized
    (lambda (keyword parser)
      (<~1> (<~0> wat-open (<wat-keyword> keyword)) parser wat-close)))

  (define select-types-immediate
    (<map> (lambda (type*)
             (make-immutable-vector (list->immutable-vector type*)))
           (parenthesized "result" (<some> <wat-value-type>))))

  (define call-indirect-immediate
    (<map>
     (lambda (value)
       (make-immutable-vector (cadr value)
                              (optional-value (car value)
                                              (make-wat-index-reference 0 0))))
     (<~> (<optional> <wat-index-reference>) <wat-type-use>)))

  (define named-attribute
    (lambda (name parser)
      (<~1> (<string> name) parser)))

  (define memory-attribute
    (</> (<map> (lambda (value) (cons 'offset value))
                (named-attribute "offset=" <wat-u64>))
         (<map> (lambda (value) (cons 'align value))
                (named-attribute "align=" <wat-u32>))))

  (define attribute-value
    (lambda (name attribute* default)
      (let ([matching (filter (lambda (attribute) (eq? name (car attribute)))
                              attribute*)])
        (cond [(null? matching) default]
              [(null? (cdr matching)) (cdar matching)]
              [else #f]))))

  (define memory-argument-immediate
    (<bind>
     (<~> (<optional> <wat-index-reference>) (<many> memory-attribute))
     (lambda (value)
       (let* ([attribute* (cadr value)]
              [offset (attribute-value 'offset attribute* 0)]
              [alignment (attribute-value 'align attribute* 0)])
         (if (and offset alignment)
             (<result>
              (make-immutable-vector
               (make-immutable-vector
                alignment offset
                (optional-value (car value) (make-wat-index-reference 0 0)))))
             (<fail-with> "duplicate WebAssembly memory attribute"))))))

  (define instruction-lane-count
    (lambda (mnemonic)
      (cond [(memq mnemonic
                   '(i8x16.extract-lane-s i8x16.extract-lane-u i8x16.replace-lane
                     v128.load8-lane v128.store8-lane))
             16]
            [(memq mnemonic
                   '(i16x8.extract-lane-s i16x8.extract-lane-u i16x8.replace-lane
                     v128.load16-lane v128.store16-lane))
             8]
            [(memq mnemonic
                   '(i32x4.extract-lane i32x4.replace-lane
                     f32x4.extract-lane f32x4.replace-lane
                     v128.load32-lane v128.store32-lane))
             4]
            [(memq mnemonic
                   '(i64x2.extract-lane i64x2.replace-lane
                     f64x2.extract-lane f64x2.replace-lane
                     v128.load64-lane v128.store64-lane))
             2]
            [else #f])))

  (define lane-parser
    (lambda (descriptor)
      (<bind>
       <wat-u32>
       (lambda (lane)
         (let ([count (instruction-lane-count (wasm-opcode-mnemonic descriptor))])
           (if (and count (< lane count))
               (<result> lane)
               (<fail-with> "invalid WebAssembly lane index")))))))

  (define shuffle-immediate
    (<bind>
     (<rep> <wat-u32> 16)
     (lambda (lane*)
       (if (andmap (lambda (lane) (< lane 32)) lane*)
           (<result> (make-immutable-vector (apply bytevector lane*)))
           (<fail-with> "invalid WebAssembly shuffle lane")))))

  (define vector-immediate
    (<map> (lambda (value)
             (make-immutable-vector
              (make-immutable-vector (car value)
                                     (list->immutable-vector (cadr value)))))
           (<~> (</> (<as> 'i8x16 (<wat-keyword> "i8x16"))
                     (<as> 'i16x8 (<wat-keyword> "i16x8"))
                     (<as> 'i32x4 (<wat-keyword> "i32x4"))
                     (<as> 'i64x2 (<wat-keyword> "i64x2"))
                     (<as> 'f32x4 (<wat-keyword> "f32x4"))
                     (<as> 'f64x2 (<wat-keyword> "f64x2")))
                (<some> (</> <wat-i64> <wat-f64>)))))

  (define br-on-cast-immediate
    (<map> list->immutable-vector
           (<~> <wat-index-reference> <wat-reference-type> <wat-reference-type>)))

  (define immediate-parser
    (lambda (descriptor)
      (case (wasm-opcode-immediate-shape descriptor)
        [(none) (<result> empty-vector)]
        [(block-type) (one-immediate <wat-block-type>)]
        [(label-index function-index type-index table-index memory-index global-index
                      local-index tag-index data-index element-index)
         index-immediate]
        [(label-vector)
         (<map> (lambda (index*)
                  (make-immutable-vector
                   (list->immutable-vector (reverse (cdr (reverse index*))))
                   (car (reverse index*))))
                (<some> <wat-index-reference>))]
        [(heap-type heap-type-non-null heap-type-nullable)
         (one-immediate <wat-heap-type>)]
        [(reference-type) (one-immediate <wat-reference-type>)]
        [(value-type-vector select-types) select-types-immediate]
        [(call-indirect) call-indirect-immediate]
        [(memory-argument) memory-argument-immediate]
        [(i32) (one-immediate <wat-i32>)]
        [(i64) (one-immediate <wat-i64>)]
        [(f32) (one-immediate <wat-f32>)]
        [(f64) (one-immediate <wat-f64>)]
        [(table-pair memory-pair struct-field array-new-fixed array-copy
                     type-data type-element)
         indexed-pair-immediate]
        [(memory-data)
         (<map> (lambda (value) (make-immutable-vector (cadr value) (car value)))
                (<~> <wat-index-reference> <wat-index-reference>))]
        [(table-element)
         (<map> (lambda (value) (make-immutable-vector (cadr value) (car value)))
                (<~> <wat-index-reference> <wat-index-reference>))]
        [(br-on-cast) br-on-cast-immediate]
        [(vector-bytes) vector-immediate]
        [(shuffle-bytes) shuffle-immediate]
        [(lane-index) (one-immediate (lane-parser descriptor))]
        [(memory-argument-lane)
         (<map> (lambda (value)
                  (let ([memory (vector-ref (car value) 0)])
                    (make-immutable-vector memory (cadr value))))
                (<~> memory-argument-immediate (lane-parser descriptor)))]
        [(try-table) (<result> empty-vector)]
        [else
         (<fail-with>
          (format "unsupported WebAssembly text immediate shape: ~a"
                  (wasm-opcode-immediate-shape descriptor)))])))

;;;;===----------------------------------------------------------------------===
;;;; Flat and folded instruction grammar
;;;;===----------------------------------------------------------------------===

  (define labels-match?
    (lambda (opening closing)
      (or (not closing) (and opening (string=? opening closing)))))

  (define closing-label
    (lambda (opening)
      (<bind>
       (<optional> <wat-identifier>)
       (lambda (closing*)
         (let ([closing (optional-value closing* #f)])
           (if (labels-match? opening closing)
               (<result> closing)
               (<fail-with> "structured instruction labels do not match")))))))

  (declare-lazy-parser <wat-flat-instruction>)
  (declare-lazy-parser <wat-folded-list>)

  (define flat-sequence-before
    (lambda (terminator)
      (<many-until> <wat-flat-instruction> terminator)))

  (define make-instruction
    (lambda (pos descriptor immediates body alternate operands)
      (make-wat-instruction-syntax
       pos (wasm-opcode-mnemonic descriptor) immediates body alternate operands)))

  (define flat-simple-after
    (lambda (pos descriptor)
      (<map> (lambda (immediates)
               (make-instruction pos descriptor immediates empty-vector #f empty-vector))
             (immediate-parser descriptor))))

  (define flat-block-after
    (lambda (pos descriptor)
      (<bind>
       (<~> (<optional> <wat-identifier>) <wat-block-type>)
       (lambda (header)
         (let ([label (optional-value (car header) #f)] [type (cadr header)])
           (<bind>
            (flat-sequence-before (<wat-keyword> "end"))
            (lambda (body)
              (<map>
               (lambda (unused)
                 (make-instruction
                  pos descriptor (make-immutable-vector type)
                  (list->immutable-vector body) #f empty-vector))
               (<~0> (<wat-keyword> "end") (closing-label label))))))))))

  (define flat-if-after
    (lambda (pos descriptor)
      (<bind>
       (<~> (<optional> <wat-identifier>) <wat-block-type>)
       (lambda (header)
         (let ([label (optional-value (car header) #f)] [type (cadr header)])
           (<bind>
            (flat-sequence-before
             (</> (<wat-keyword> "else") (<wat-keyword> "end")))
            (lambda (body)
              (</>
               (<bind>
                (<~0> (<wat-keyword> "else") (closing-label label))
                (lambda (unused)
                  (<bind>
                   (flat-sequence-before (<wat-keyword> "end"))
                   (lambda (alternate)
                     (<map>
                      (lambda (unused)
                        (make-instruction
                         pos descriptor (make-immutable-vector type)
                         (list->immutable-vector body)
                         (list->immutable-vector alternate) empty-vector))
                      (<~0> (<wat-keyword> "end") (closing-label label)))))))
               (<map>
                (lambda (unused)
                  (make-instruction
                   pos descriptor (make-immutable-vector type)
                   (list->immutable-vector body) empty-vector empty-vector))
                (<~0> (<wat-keyword> "end") (closing-label label)))))))))))

  (define try-catch-kind
    (</> (<as> 'catch-all-ref (<wat-keyword> "catch_all_ref"))
         (<as> 'catch-all (<wat-keyword> "catch_all"))
         (<as> 'catch-ref (<wat-keyword> "catch_ref"))
         (<as> 'catch (<wat-keyword> "catch"))))

  (define try-catch-clause
    (<~1>
     (<followed-by> (<result> #t) (<~0> wat-open try-catch-kind))
     wat-open
     (<bind>
      try-catch-kind
      (lambda (kind)
        (<map> (lambda (value) (make-immutable-vector kind value))
               (if (memq kind '(catch catch-ref))
                   (two-immediates <wat-index-reference> <wat-index-reference>)
                   (one-immediate <wat-index-reference>)))))
     wat-close))

  (define flat-try-after
    (lambda (pos descriptor)
      (<bind>
       (<~> (<optional> <wat-identifier>) <wat-block-type>
            (<many> try-catch-clause))
       (lambda (header)
         (let ([label (optional-value (car header) #f)])
           (<bind>
            (flat-sequence-before (<wat-keyword> "end"))
            (lambda (body)
              (<map>
               (lambda (unused)
                 (make-instruction
                  pos descriptor
                  (make-immutable-vector (cadr header)
                                         (list->immutable-vector (caddr header)))
                  (list->immutable-vector body) #f empty-vector))
               (<~0> (<wat-keyword> "end") (closing-label label))))))))))

  (define flat-after
    (lambda (pos descriptor)
      (case (wasm-opcode-structured-kind descriptor)
        [(block loop) (flat-block-after pos descriptor)]
        [(if) (flat-if-after pos descriptor)]
        [(try-table) (flat-try-after pos descriptor)]
        [else
         (if (memq (wasm-opcode-mnemonic descriptor) '(else end))
             (<fail-with> "reserved structured terminator")
             (flat-simple-after pos descriptor))])))

  (define folded-simple-after
    (lambda (pos descriptor)
      (<bind>
       (immediate-parser descriptor)
       (lambda (immediates)
         (<map>
          (lambda (operand**)
            (let* ([operand* (apply append operand**)]
                   [node (make-instruction
                          pos descriptor immediates empty-vector #f
                          (list->immutable-vector operand*))])
              (append operand* (list node))))
          (<many-until> <wat-folded-list> wat-close))))))

  (define folded-block-after
    (lambda (pos descriptor)
      (<bind>
       (<~> (<optional> <wat-identifier>) <wat-block-type>)
       (lambda (header)
         (<map>
          (lambda (body*)
            (list
             (make-instruction
              pos descriptor (make-immutable-vector (cadr header))
              (list->immutable-vector (apply append body*)) #f empty-vector)))
          (<many-until> <wat-folded-list> wat-close))))))

  (define folded-try-after
    (lambda (pos descriptor)
      (<bind>
       (<~> (<optional> <wat-identifier>) <wat-block-type>
            (<many> try-catch-clause))
       (lambda (header)
         (<map>
          (lambda (body*)
            (list
             (make-instruction
              pos descriptor
              (make-immutable-vector (cadr header)
                                     (list->immutable-vector (caddr header)))
              (list->immutable-vector (apply append body*)) #f empty-vector)))
          (<many-until> <wat-folded-list> wat-close))))))

  (define folded-branch
    (lambda (keyword)
      (parenthesized keyword (<many> <wat-folded-list>))))

  (define folded-if-after
    (lambda (pos descriptor)
      (<bind>
       (<~> (<optional> <wat-identifier>) <wat-block-type>
            (<many> <wat-folded-list>)
            (folded-branch "then")
            (<optional> (folded-branch "else")))
       (lambda (value)
         (<result>
          (let* ([operand* (apply append (caddr value))]
                 [body (apply append (cadddr value))]
                 [alternate-source (car (cddddr value))]
                 [alternate (if (null? alternate-source)
                                '()
                                (apply append alternate-source))]
                 [node
                  (make-instruction
                   pos descriptor (make-immutable-vector (cadr value))
                   (list->immutable-vector body) (list->immutable-vector alternate)
                   (list->immutable-vector operand*))])
            (append operand* (list node))))))))

  (define folded-after
    (lambda (pos descriptor)
      (case (wasm-opcode-structured-kind descriptor)
        [(block loop) (folded-block-after pos descriptor)]
        [(try-table) (folded-try-after pos descriptor)]
        [(if) (folded-if-after pos descriptor)]
        [else (folded-simple-after pos descriptor)])))

  (define flat-parser
    (<bind> (<~> <pos> descriptor-parser)
            (lambda (value) (flat-after (car value) (cadr value)))))

  (define folded-parser
    (<~1>
     wat-open
     (<bind> (<~> <pos> descriptor-parser)
             (lambda (value)
               (<~0> (folded-after (car value) (cadr value)) wat-close)))))

  #|proc:<wat-instruction>
  The `<wat-instruction>` parser reads one flat Core 3.0 WAT instruction and returns positioned
  instruction syntax. Structured instructions include their nested bodies.
  |#
  (define <wat-instruction>
    (begin
      (install-lazy-parser! <wat-flat-instruction> flat-parser)
      (install-lazy-parser! <wat-folded-list> folded-parser)
      flat-parser))

  #|proc:<wat-folded-instruction>
  The `<wat-folded-instruction>` parser reads one folded Core 3.0 WAT instruction and returns its
  operands followed by its containing instruction as a list.
  |#
  (define <wat-folded-instruction> folded-parser)

  #|proc:<wat-expression>
  The `<wat-expression>` parser reads flat and folded Core 3.0 WAT instructions and returns one
  immutable vector in execution order.
  |#
  (define <wat-expression>
    (<map> (lambda (instruction**)
             (list->immutable-vector (apply append instruction**)))
           (<many> (</> (<map> list <wat-instruction>)
                        <wat-folded-instruction>))))

  )
