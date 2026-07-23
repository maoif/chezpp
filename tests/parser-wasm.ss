(import (chezpp)
        (chezpp parser wasm)
        (chezpp parser wasm opcodes)
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

(define expected-core-3-mnemonics
  '#(
   ;; parametric
   unreachable nop drop select
   ;; control
   block loop if else end br br-if br-table return call call-indirect br-on-null br-on-non-null
   br-on-cast br-on-cast-fail
   ;; exception/tail-call; final Core 3.0 has no continuation instructions
   throw throw-ref return-call return-call-indirect call-ref return-call-ref try-table
   ;; variable
   local.get local.set local.tee global.get global.set
   ;; table
   table.get table.set table.init elem.drop table.copy table.grow table.size table.fill
   ;; memory
   i32.load i64.load f32.load f64.load i32.load8-s i32.load8-u i32.load16-s i32.load16-u
   i64.load8-s i64.load8-u i64.load16-s i64.load16-u i64.load32-s i64.load32-u i32.store
   i64.store f32.store f64.store i32.store8 i32.store16 i64.store8 i64.store16 i64.store32
   memory.size memory.grow memory.init data.drop memory.copy memory.fill v128.load
   v128.load8x8-s v128.load8x8-u v128.load16x4-s v128.load16x4-u v128.load32x2-s
   v128.load32x2-u v128.load8-splat v128.load16-splat v128.load32-splat v128.load64-splat
   v128.store v128.load8-lane v128.load16-lane v128.load32-lane v128.load64-lane
   v128.store8-lane v128.store16-lane v128.store32-lane v128.store64-lane v128.load32-zero
   v128.load64-zero
   ;; numeric
   i32.const i64.const f32.const f64.const i32.eqz i32.eq i32.ne i32.lt-s i32.lt-u i32.gt-s
   i32.gt-u i32.le-s i32.le-u i32.ge-s i32.ge-u i64.eqz i64.eq i64.ne i64.lt-s i64.lt-u
   i64.gt-s i64.gt-u i64.le-s i64.le-u i64.ge-s i64.ge-u f32.eq f32.ne f32.lt f32.gt f32.le
   f32.ge f64.eq f64.ne f64.lt f64.gt f64.le f64.ge i32.clz i32.ctz i32.popcnt i32.add i32.sub
   i32.mul i32.div-s i32.div-u i32.rem-s i32.rem-u i32.and i32.or i32.xor i32.shl i32.shr-s
   i32.shr-u i32.rotl i32.rotr i64.clz i64.ctz i64.popcnt i64.add i64.sub i64.mul i64.div-s
   i64.div-u i64.rem-s i64.rem-u i64.and i64.or i64.xor i64.shl i64.shr-s i64.shr-u i64.rotl
   i64.rotr f32.abs f32.neg f32.ceil f32.floor f32.trunc f32.nearest f32.sqrt f32.add f32.sub
   f32.mul f32.div f32.min f32.max f32.copysign f64.abs f64.neg f64.ceil f64.floor f64.trunc
   f64.nearest f64.sqrt f64.add f64.sub f64.mul f64.div f64.min f64.max f64.copysign
   i32.wrap-i64 i32.trunc-f32-s i32.trunc-f32-u i32.trunc-f64-s i32.trunc-f64-u
   i64.extend-i32-s i64.extend-i32-u i64.trunc-f32-s i64.trunc-f32-u i64.trunc-f64-s
   i64.trunc-f64-u f32.convert-i32-s f32.convert-i32-u f32.convert-i64-s f32.convert-i64-u
   f32.demote-f64 f64.convert-i32-s f64.convert-i32-u f64.convert-i64-s f64.convert-i64-u
   f64.promote-f32 i32.reinterpret-f32 i64.reinterpret-f64 f32.reinterpret-i32
   f64.reinterpret-i64 i32.extend8-s i32.extend16-s i64.extend8-s i64.extend16-s i64.extend32-s
   i32.trunc-sat-f32-s i32.trunc-sat-f32-u i32.trunc-sat-f64-s i32.trunc-sat-f64-u
   i64.trunc-sat-f32-s i64.trunc-sat-f32-u i64.trunc-sat-f64-s i64.trunc-sat-f64-u
   ;; reference
   ref.null ref.is-null ref.func ref.eq ref.as-non-null ref.test ref.cast any.convert-extern
   extern.convert-any ref.i31
   ;; aggregate/gc
   struct.new struct.new-default struct.get struct.get-s struct.get-u struct.set array.new
   array.new-default array.new-fixed array.new-data array.new-elem array.get array.get-s
   array.get-u array.set array.len array.fill array.copy array.init-data array.init-elem
   i31.get-s i31.get-u
   ;; vector
   v128.const i8x16.shuffle i8x16.swizzle i8x16.splat i16x8.splat i32x4.splat i64x2.splat
   f32x4.splat f64x2.splat i8x16.extract-lane-s i8x16.extract-lane-u i8x16.replace-lane
   i16x8.extract-lane-s i16x8.extract-lane-u i16x8.replace-lane i32x4.extract-lane
   i32x4.replace-lane i64x2.extract-lane i64x2.replace-lane f32x4.extract-lane
   f32x4.replace-lane f64x2.extract-lane f64x2.replace-lane i8x16.eq i8x16.ne i8x16.lt-s
   i8x16.lt-u i8x16.gt-s i8x16.gt-u i8x16.le-s i8x16.le-u i8x16.ge-s i8x16.ge-u i16x8.eq
   i16x8.ne i16x8.lt-s i16x8.lt-u i16x8.gt-s i16x8.gt-u i16x8.le-s i16x8.le-u i16x8.ge-s
   i16x8.ge-u i32x4.eq i32x4.ne i32x4.lt-s i32x4.lt-u i32x4.gt-s i32x4.gt-u i32x4.le-s
   i32x4.le-u i32x4.ge-s i32x4.ge-u f32x4.eq f32x4.ne f32x4.lt f32x4.gt f32x4.le f32x4.ge
   f64x2.eq f64x2.ne f64x2.lt f64x2.gt f64x2.le f64x2.ge v128.not v128.and v128.andnot v128.or
   v128.xor v128.bitselect v128.any-true f32x4.demote-f64x2-zero f64x2.promote-low-f32x4
   i8x16.abs i8x16.neg i8x16.popcnt i8x16.all-true i8x16.bitmask i8x16.narrow-i16x8-s
   i8x16.narrow-i16x8-u f32x4.ceil f32x4.floor f32x4.trunc f32x4.nearest i8x16.shl i8x16.shr-s
   i8x16.shr-u i8x16.add i8x16.add-sat-s i8x16.add-sat-u i8x16.sub i8x16.sub-sat-s
   i8x16.sub-sat-u f64x2.ceil f64x2.floor i8x16.min-s i8x16.min-u i8x16.max-s i8x16.max-u
   f64x2.trunc i8x16.avgr-u i16x8.extadd-pairwise-i8x16-s i16x8.extadd-pairwise-i8x16-u
   i32x4.extadd-pairwise-i16x8-s i32x4.extadd-pairwise-i16x8-u i16x8.abs i16x8.neg
   i16x8.q15mulr-sat-s i16x8.all-true i16x8.bitmask i16x8.narrow-i32x4-s i16x8.narrow-i32x4-u
   i16x8.extend-low-i8x16-s i16x8.extend-high-i8x16-s i16x8.extend-low-i8x16-u
   i16x8.extend-high-i8x16-u i16x8.shl i16x8.shr-s i16x8.shr-u i16x8.add i16x8.add-sat-s
   i16x8.add-sat-u i16x8.sub i16x8.sub-sat-s i16x8.sub-sat-u f64x2.nearest i16x8.mul
   i16x8.min-s i16x8.min-u i16x8.max-s i16x8.max-u i16x8.avgr-u i16x8.extmul-low-i8x16-s
   i16x8.extmul-high-i8x16-s i16x8.extmul-low-i8x16-u i16x8.extmul-high-i8x16-u i32x4.abs
   i32x4.neg i32x4.all-true i32x4.bitmask i32x4.extend-low-i16x8-s i32x4.extend-high-i16x8-s
   i32x4.extend-low-i16x8-u i32x4.extend-high-i16x8-u i32x4.shl i32x4.shr-s i32x4.shr-u
   i32x4.add i32x4.sub i32x4.mul i32x4.min-s i32x4.min-u i32x4.max-s i32x4.max-u
   i32x4.dot-i16x8-s i32x4.extmul-low-i16x8-s i32x4.extmul-high-i16x8-s
   i32x4.extmul-low-i16x8-u i32x4.extmul-high-i16x8-u i64x2.abs i64x2.neg i64x2.all-true
   i64x2.bitmask i64x2.extend-low-i32x4-s i64x2.extend-high-i32x4-s i64x2.extend-low-i32x4-u
   i64x2.extend-high-i32x4-u i64x2.shl i64x2.shr-s i64x2.shr-u i64x2.add i64x2.sub i64x2.mul
   i64x2.eq i64x2.ne i64x2.lt-s i64x2.gt-s i64x2.le-s i64x2.ge-s i64x2.extmul-low-i32x4-s
   i64x2.extmul-high-i32x4-s i64x2.extmul-low-i32x4-u i64x2.extmul-high-i32x4-u f32x4.abs
   f32x4.neg f32x4.sqrt f32x4.add f32x4.sub f32x4.mul f32x4.div f32x4.min f32x4.max f32x4.pmin
   f32x4.pmax f64x2.abs f64x2.neg f64x2.sqrt f64x2.add f64x2.sub f64x2.mul f64x2.div f64x2.min
   f64x2.max f64x2.pmin f64x2.pmax i32x4.trunc-sat-f32x4-s i32x4.trunc-sat-f32x4-u
   f32x4.convert-i32x4-s f32x4.convert-i32x4-u i32x4.trunc-sat-f64x2-s-zero
   i32x4.trunc-sat-f64x2-u-zero f64x2.convert-low-i32x4-s f64x2.convert-low-i32x4-u
   ;; relaxed-simd
   i8x16.relaxed-swizzle i32x4.relaxed-trunc-f32x4-s i32x4.relaxed-trunc-f32x4-u
   i32x4.relaxed-trunc-f64x2-s-zero i32x4.relaxed-trunc-f64x2-u-zero f32x4.relaxed-madd
   f32x4.relaxed-nmadd f64x2.relaxed-madd f64x2.relaxed-nmadd i8x16.relaxed-laneselect
   i16x8.relaxed-laneselect i32x4.relaxed-laneselect i64x2.relaxed-laneselect f32x4.relaxed-min
   f32x4.relaxed-max f64x2.relaxed-min f64x2.relaxed-max i16x8.relaxed-q15mulr-s
   i16x8.relaxed-dot-i8x16-i7x16-s i32x4.relaxed-dot-i8x16-i7x16-add-s
   ))

(define wasm-opcode-immediate-shapes
  '(none block-type label-index label-vector function-index type-index table-index
    memory-index global-index local-index tag-index field-index data-index element-index
    heap-type reference-type value-type-vector select-types call-indirect br-on-cast
    memory-argument memory-argument-lane lane-index shuffle-bytes vector-bytes i32 i64
    f32 f64 table-pair memory-pair array-new-fixed array-copy struct-field try-table
    resume-table))

(define wasm-opcode-structured-kinds '(#f block loop if try-table))

(define expected-core-3-binary-variants
  (vector (vector #f #x1b 'select 'none #f)
          (vector #xfb 21 'ref.test 'reference-type #f)
          (vector #xfb 23 'ref.cast 'reference-type #f)))

(define unique-values?
  (lambda (values)
    (let ([seen (make-hashtable equal-hash equal?)])
      (andmap (lambda (value)
                (and (not (hashtable-ref seen value #f))
                     (begin (hashtable-set! seen value #t) #t)))
              values))))

(define descriptor-fields=?
  (lambda (descriptor prefix code mnemonic immediate-shape structured-kind)
    (and (wasm-opcode-descriptor? descriptor)
         (equal? prefix (wasm-opcode-prefix descriptor))
         (= code (wasm-opcode-code descriptor))
         (eq? mnemonic (wasm-opcode-mnemonic descriptor))
         (eq? immediate-shape (wasm-opcode-immediate-shape descriptor))
         (eq? structured-kind (wasm-opcode-structured-kind descriptor)))))

(mat wasm-core-3-opcode-table

     (andmap (lambda (mnemonic)
               (wasm-opcode-descriptor? (wasm-opcode-by-mnemonic mnemonic)))
             (vector->list expected-core-3-mnemonics))

     (= (vector-length expected-core-3-mnemonics)
        (vector-length wasm-core-3-opcodes))

     (unique-values? (vector->list expected-core-3-mnemonics))

     (unique-values?
      (map wasm-opcode-mnemonic (vector->list wasm-core-3-opcodes)))

     (andmap (lambda (descriptor)
               (and (memq (wasm-opcode-mnemonic descriptor)
                          (vector->list expected-core-3-mnemonics))
                    #t))
             (vector->list wasm-core-3-opcodes))

     (unique-values?
      (append
       (map (lambda (descriptor)
              (cons (wasm-opcode-prefix descriptor) (wasm-opcode-code descriptor)))
            (vector->list wasm-core-3-opcodes))
       (map (lambda (variant)
              (cons (vector-ref variant 0) (vector-ref variant 1)))
            (vector->list expected-core-3-binary-variants))))

     (= 499 (+ (vector-length wasm-core-3-opcodes)
               (vector-length expected-core-3-binary-variants)))

     (andmap
      (lambda (descriptor)
        (eq? descriptor
             (wasm-opcode-by-binary (wasm-opcode-prefix descriptor)
                                    (wasm-opcode-code descriptor))))
      (vector->list wasm-core-3-opcodes))

     (andmap (lambda (descriptor)
               (and (memq (wasm-opcode-immediate-shape descriptor)
                          wasm-opcode-immediate-shapes)
                    (memq (wasm-opcode-structured-kind descriptor)
                          wasm-opcode-structured-kinds)
                    #t))
             (vector->list wasm-core-3-opcodes))

     (andmap
      (lambda (variant)
        (let ([descriptor
               (wasm-opcode-by-binary (vector-ref variant 0) (vector-ref variant 1))])
          (descriptor-fields=? descriptor
                               (vector-ref variant 0) (vector-ref variant 1)
                               (vector-ref variant 2) (vector-ref variant 3)
                               (vector-ref variant 4))))
      (vector->list expected-core-3-binary-variants))

     (descriptor-fields=? (wasm-opcode-by-mnemonic 'unreachable)
                          #f #x00 'unreachable 'none #f)

     (descriptor-fields=? (wasm-opcode-by-mnemonic 'block)
                          #f #x02 'block 'block-type 'block)

     (descriptor-fields=? (wasm-opcode-by-mnemonic 'call-indirect)
                          #f #x11 'call-indirect 'call-indirect #f)

     (descriptor-fields=? (wasm-opcode-by-mnemonic 'struct.get)
                          #xfb 2 'struct.get 'struct-field #f)

     (descriptor-fields=? (wasm-opcode-by-mnemonic 'memory.init)
                          #xfc 8 'memory.init 'data-index #f)

     (descriptor-fields=? (wasm-opcode-by-mnemonic 'v128.load)
                          #xfd 0 'v128.load 'memory-argument #f)

     ;; error case: an unassigned binary pair has no descriptor.
     (not (wasm-opcode-by-binary #xfd #xffff))

     ;; error case: an unknown textual mnemonic has no descriptor.
     (not (wasm-opcode-by-mnemonic 'not-a-wasm-opcode))

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
