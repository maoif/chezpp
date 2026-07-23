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

;; Source: WebAssembly/spec `wg-3.0` commit `9d36019973201a19f9c9ebb0f10828b2fe2374aa`.
(define expected-core-3-binary-assignments
  (vector
   ;; one-byte instructions
   (vector #f #x0 'unreachable)
   (vector #f #x1 'nop)
   (vector #f #x2 'block)
   (vector #f #x3 'loop)
   (vector #f #x4 'if)
   (vector #f #x5 'else)
   (vector #f #x8 'throw)
   (vector #f #xa 'throw-ref)
   (vector #f #xb 'end)
   (vector #f #xc 'br)
   (vector #f #xd 'br-if)
   (vector #f #xe 'br-table)
   (vector #f #xf 'return)
   (vector #f #x10 'call)
   (vector #f #x11 'call-indirect)
   (vector #f #x12 'return-call)
   (vector #f #x13 'return-call-indirect)
   (vector #f #x14 'call-ref)
   (vector #f #x15 'return-call-ref)
   (vector #f #x1a 'drop)
   (vector #f #x1b 'select)
   (vector #f #x1c 'select)
   (vector #f #x1f 'try-table)
   (vector #f #x20 'local.get)
   (vector #f #x21 'local.set)
   (vector #f #x22 'local.tee)
   (vector #f #x23 'global.get)
   (vector #f #x24 'global.set)
   (vector #f #x25 'table.get)
   (vector #f #x26 'table.set)
   (vector #f #x28 'i32.load)
   (vector #f #x29 'i64.load)
   (vector #f #x2a 'f32.load)
   (vector #f #x2b 'f64.load)
   (vector #f #x2c 'i32.load8-s)
   (vector #f #x2d 'i32.load8-u)
   (vector #f #x2e 'i32.load16-s)
   (vector #f #x2f 'i32.load16-u)
   (vector #f #x30 'i64.load8-s)
   (vector #f #x31 'i64.load8-u)
   (vector #f #x32 'i64.load16-s)
   (vector #f #x33 'i64.load16-u)
   (vector #f #x34 'i64.load32-s)
   (vector #f #x35 'i64.load32-u)
   (vector #f #x36 'i32.store)
   (vector #f #x37 'i64.store)
   (vector #f #x38 'f32.store)
   (vector #f #x39 'f64.store)
   (vector #f #x3a 'i32.store8)
   (vector #f #x3b 'i32.store16)
   (vector #f #x3c 'i64.store8)
   (vector #f #x3d 'i64.store16)
   (vector #f #x3e 'i64.store32)
   (vector #f #x3f 'memory.size)
   (vector #f #x40 'memory.grow)
   (vector #f #x41 'i32.const)
   (vector #f #x42 'i64.const)
   (vector #f #x43 'f32.const)
   (vector #f #x44 'f64.const)
   (vector #f #x45 'i32.eqz)
   (vector #f #x46 'i32.eq)
   (vector #f #x47 'i32.ne)
   (vector #f #x48 'i32.lt-s)
   (vector #f #x49 'i32.lt-u)
   (vector #f #x4a 'i32.gt-s)
   (vector #f #x4b 'i32.gt-u)
   (vector #f #x4c 'i32.le-s)
   (vector #f #x4d 'i32.le-u)
   (vector #f #x4e 'i32.ge-s)
   (vector #f #x4f 'i32.ge-u)
   (vector #f #x50 'i64.eqz)
   (vector #f #x51 'i64.eq)
   (vector #f #x52 'i64.ne)
   (vector #f #x53 'i64.lt-s)
   (vector #f #x54 'i64.lt-u)
   (vector #f #x55 'i64.gt-s)
   (vector #f #x56 'i64.gt-u)
   (vector #f #x57 'i64.le-s)
   (vector #f #x58 'i64.le-u)
   (vector #f #x59 'i64.ge-s)
   (vector #f #x5a 'i64.ge-u)
   (vector #f #x5b 'f32.eq)
   (vector #f #x5c 'f32.ne)
   (vector #f #x5d 'f32.lt)
   (vector #f #x5e 'f32.gt)
   (vector #f #x5f 'f32.le)
   (vector #f #x60 'f32.ge)
   (vector #f #x61 'f64.eq)
   (vector #f #x62 'f64.ne)
   (vector #f #x63 'f64.lt)
   (vector #f #x64 'f64.gt)
   (vector #f #x65 'f64.le)
   (vector #f #x66 'f64.ge)
   (vector #f #x67 'i32.clz)
   (vector #f #x68 'i32.ctz)
   (vector #f #x69 'i32.popcnt)
   (vector #f #x6a 'i32.add)
   (vector #f #x6b 'i32.sub)
   (vector #f #x6c 'i32.mul)
   (vector #f #x6d 'i32.div-s)
   (vector #f #x6e 'i32.div-u)
   (vector #f #x6f 'i32.rem-s)
   (vector #f #x70 'i32.rem-u)
   (vector #f #x71 'i32.and)
   (vector #f #x72 'i32.or)
   (vector #f #x73 'i32.xor)
   (vector #f #x74 'i32.shl)
   (vector #f #x75 'i32.shr-s)
   (vector #f #x76 'i32.shr-u)
   (vector #f #x77 'i32.rotl)
   (vector #f #x78 'i32.rotr)
   (vector #f #x79 'i64.clz)
   (vector #f #x7a 'i64.ctz)
   (vector #f #x7b 'i64.popcnt)
   (vector #f #x7c 'i64.add)
   (vector #f #x7d 'i64.sub)
   (vector #f #x7e 'i64.mul)
   (vector #f #x7f 'i64.div-s)
   (vector #f #x80 'i64.div-u)
   (vector #f #x81 'i64.rem-s)
   (vector #f #x82 'i64.rem-u)
   (vector #f #x83 'i64.and)
   (vector #f #x84 'i64.or)
   (vector #f #x85 'i64.xor)
   (vector #f #x86 'i64.shl)
   (vector #f #x87 'i64.shr-s)
   (vector #f #x88 'i64.shr-u)
   (vector #f #x89 'i64.rotl)
   (vector #f #x8a 'i64.rotr)
   (vector #f #x8b 'f32.abs)
   (vector #f #x8c 'f32.neg)
   (vector #f #x8d 'f32.ceil)
   (vector #f #x8e 'f32.floor)
   (vector #f #x8f 'f32.trunc)
   (vector #f #x90 'f32.nearest)
   (vector #f #x91 'f32.sqrt)
   (vector #f #x92 'f32.add)
   (vector #f #x93 'f32.sub)
   (vector #f #x94 'f32.mul)
   (vector #f #x95 'f32.div)
   (vector #f #x96 'f32.min)
   (vector #f #x97 'f32.max)
   (vector #f #x98 'f32.copysign)
   (vector #f #x99 'f64.abs)
   (vector #f #x9a 'f64.neg)
   (vector #f #x9b 'f64.ceil)
   (vector #f #x9c 'f64.floor)
   (vector #f #x9d 'f64.trunc)
   (vector #f #x9e 'f64.nearest)
   (vector #f #x9f 'f64.sqrt)
   (vector #f #xa0 'f64.add)
   (vector #f #xa1 'f64.sub)
   (vector #f #xa2 'f64.mul)
   (vector #f #xa3 'f64.div)
   (vector #f #xa4 'f64.min)
   (vector #f #xa5 'f64.max)
   (vector #f #xa6 'f64.copysign)
   (vector #f #xa7 'i32.wrap-i64)
   (vector #f #xa8 'i32.trunc-f32-s)
   (vector #f #xa9 'i32.trunc-f32-u)
   (vector #f #xaa 'i32.trunc-f64-s)
   (vector #f #xab 'i32.trunc-f64-u)
   (vector #f #xac 'i64.extend-i32-s)
   (vector #f #xad 'i64.extend-i32-u)
   (vector #f #xae 'i64.trunc-f32-s)
   (vector #f #xaf 'i64.trunc-f32-u)
   (vector #f #xb0 'i64.trunc-f64-s)
   (vector #f #xb1 'i64.trunc-f64-u)
   (vector #f #xb2 'f32.convert-i32-s)
   (vector #f #xb3 'f32.convert-i32-u)
   (vector #f #xb4 'f32.convert-i64-s)
   (vector #f #xb5 'f32.convert-i64-u)
   (vector #f #xb6 'f32.demote-f64)
   (vector #f #xb7 'f64.convert-i32-s)
   (vector #f #xb8 'f64.convert-i32-u)
   (vector #f #xb9 'f64.convert-i64-s)
   (vector #f #xba 'f64.convert-i64-u)
   (vector #f #xbb 'f64.promote-f32)
   (vector #f #xbc 'i32.reinterpret-f32)
   (vector #f #xbd 'i64.reinterpret-f64)
   (vector #f #xbe 'f32.reinterpret-i32)
   (vector #f #xbf 'f64.reinterpret-i64)
   (vector #f #xc0 'i32.extend8-s)
   (vector #f #xc1 'i32.extend16-s)
   (vector #f #xc2 'i64.extend8-s)
   (vector #f #xc3 'i64.extend16-s)
   (vector #f #xc4 'i64.extend32-s)
   (vector #f #xd0 'ref.null)
   (vector #f #xd1 'ref.is-null)
   (vector #f #xd2 'ref.func)
   (vector #f #xd3 'ref.eq)
   (vector #f #xd4 'ref.as-non-null)
   (vector #f #xd5 'br-on-null)
   (vector #f #xd6 'br-on-non-null)
   ;; #xfb aggregate and GC instructions
   (vector #xfb #x0 'struct.new)
   (vector #xfb #x1 'struct.new-default)
   (vector #xfb #x2 'struct.get)
   (vector #xfb #x3 'struct.get-s)
   (vector #xfb #x4 'struct.get-u)
   (vector #xfb #x5 'struct.set)
   (vector #xfb #x6 'array.new)
   (vector #xfb #x7 'array.new-default)
   (vector #xfb #x8 'array.new-fixed)
   (vector #xfb #x9 'array.new-data)
   (vector #xfb #xa 'array.new-elem)
   (vector #xfb #xb 'array.get)
   (vector #xfb #xc 'array.get-s)
   (vector #xfb #xd 'array.get-u)
   (vector #xfb #xe 'array.set)
   (vector #xfb #xf 'array.len)
   (vector #xfb #x10 'array.fill)
   (vector #xfb #x11 'array.copy)
   (vector #xfb #x12 'array.init-data)
   (vector #xfb #x13 'array.init-elem)
   (vector #xfb #x14 'ref.test)
   (vector #xfb #x15 'ref.test)
   (vector #xfb #x16 'ref.cast)
   (vector #xfb #x17 'ref.cast)
   (vector #xfb #x18 'br-on-cast)
   (vector #xfb #x19 'br-on-cast-fail)
   (vector #xfb #x1a 'any.convert-extern)
   (vector #xfb #x1b 'extern.convert-any)
   (vector #xfb #x1c 'ref.i31)
   (vector #xfb #x1d 'i31.get-s)
   (vector #xfb #x1e 'i31.get-u)
   ;; #xfc saturating conversion and bulk instructions
   (vector #xfc #x0 'i32.trunc-sat-f32-s)
   (vector #xfc #x1 'i32.trunc-sat-f32-u)
   (vector #xfc #x2 'i32.trunc-sat-f64-s)
   (vector #xfc #x3 'i32.trunc-sat-f64-u)
   (vector #xfc #x4 'i64.trunc-sat-f32-s)
   (vector #xfc #x5 'i64.trunc-sat-f32-u)
   (vector #xfc #x6 'i64.trunc-sat-f64-s)
   (vector #xfc #x7 'i64.trunc-sat-f64-u)
   (vector #xfc #x8 'memory.init)
   (vector #xfc #x9 'data.drop)
   (vector #xfc #xa 'memory.copy)
   (vector #xfc #xb 'memory.fill)
   (vector #xfc #xc 'table.init)
   (vector #xfc #xd 'elem.drop)
   (vector #xfc #xe 'table.copy)
   (vector #xfc #xf 'table.grow)
   (vector #xfc #x10 'table.size)
   (vector #xfc #x11 'table.fill)
   ;; #xfd vector and relaxed SIMD instructions
   (vector #xfd #x0 'v128.load)
   (vector #xfd #x1 'v128.load8x8-s)
   (vector #xfd #x2 'v128.load8x8-u)
   (vector #xfd #x3 'v128.load16x4-s)
   (vector #xfd #x4 'v128.load16x4-u)
   (vector #xfd #x5 'v128.load32x2-s)
   (vector #xfd #x6 'v128.load32x2-u)
   (vector #xfd #x7 'v128.load8-splat)
   (vector #xfd #x8 'v128.load16-splat)
   (vector #xfd #x9 'v128.load32-splat)
   (vector #xfd #xa 'v128.load64-splat)
   (vector #xfd #xb 'v128.store)
   (vector #xfd #xc 'v128.const)
   (vector #xfd #xd 'i8x16.shuffle)
   (vector #xfd #xe 'i8x16.swizzle)
   (vector #xfd #xf 'i8x16.splat)
   (vector #xfd #x10 'i16x8.splat)
   (vector #xfd #x11 'i32x4.splat)
   (vector #xfd #x12 'i64x2.splat)
   (vector #xfd #x13 'f32x4.splat)
   (vector #xfd #x14 'f64x2.splat)
   (vector #xfd #x15 'i8x16.extract-lane-s)
   (vector #xfd #x16 'i8x16.extract-lane-u)
   (vector #xfd #x17 'i8x16.replace-lane)
   (vector #xfd #x18 'i16x8.extract-lane-s)
   (vector #xfd #x19 'i16x8.extract-lane-u)
   (vector #xfd #x1a 'i16x8.replace-lane)
   (vector #xfd #x1b 'i32x4.extract-lane)
   (vector #xfd #x1c 'i32x4.replace-lane)
   (vector #xfd #x1d 'i64x2.extract-lane)
   (vector #xfd #x1e 'i64x2.replace-lane)
   (vector #xfd #x1f 'f32x4.extract-lane)
   (vector #xfd #x20 'f32x4.replace-lane)
   (vector #xfd #x21 'f64x2.extract-lane)
   (vector #xfd #x22 'f64x2.replace-lane)
   (vector #xfd #x23 'i8x16.eq)
   (vector #xfd #x24 'i8x16.ne)
   (vector #xfd #x25 'i8x16.lt-s)
   (vector #xfd #x26 'i8x16.lt-u)
   (vector #xfd #x27 'i8x16.gt-s)
   (vector #xfd #x28 'i8x16.gt-u)
   (vector #xfd #x29 'i8x16.le-s)
   (vector #xfd #x2a 'i8x16.le-u)
   (vector #xfd #x2b 'i8x16.ge-s)
   (vector #xfd #x2c 'i8x16.ge-u)
   (vector #xfd #x2d 'i16x8.eq)
   (vector #xfd #x2e 'i16x8.ne)
   (vector #xfd #x2f 'i16x8.lt-s)
   (vector #xfd #x30 'i16x8.lt-u)
   (vector #xfd #x31 'i16x8.gt-s)
   (vector #xfd #x32 'i16x8.gt-u)
   (vector #xfd #x33 'i16x8.le-s)
   (vector #xfd #x34 'i16x8.le-u)
   (vector #xfd #x35 'i16x8.ge-s)
   (vector #xfd #x36 'i16x8.ge-u)
   (vector #xfd #x37 'i32x4.eq)
   (vector #xfd #x38 'i32x4.ne)
   (vector #xfd #x39 'i32x4.lt-s)
   (vector #xfd #x3a 'i32x4.lt-u)
   (vector #xfd #x3b 'i32x4.gt-s)
   (vector #xfd #x3c 'i32x4.gt-u)
   (vector #xfd #x3d 'i32x4.le-s)
   (vector #xfd #x3e 'i32x4.le-u)
   (vector #xfd #x3f 'i32x4.ge-s)
   (vector #xfd #x40 'i32x4.ge-u)
   (vector #xfd #x41 'f32x4.eq)
   (vector #xfd #x42 'f32x4.ne)
   (vector #xfd #x43 'f32x4.lt)
   (vector #xfd #x44 'f32x4.gt)
   (vector #xfd #x45 'f32x4.le)
   (vector #xfd #x46 'f32x4.ge)
   (vector #xfd #x47 'f64x2.eq)
   (vector #xfd #x48 'f64x2.ne)
   (vector #xfd #x49 'f64x2.lt)
   (vector #xfd #x4a 'f64x2.gt)
   (vector #xfd #x4b 'f64x2.le)
   (vector #xfd #x4c 'f64x2.ge)
   (vector #xfd #x4d 'v128.not)
   (vector #xfd #x4e 'v128.and)
   (vector #xfd #x4f 'v128.andnot)
   (vector #xfd #x50 'v128.or)
   (vector #xfd #x51 'v128.xor)
   (vector #xfd #x52 'v128.bitselect)
   (vector #xfd #x53 'v128.any-true)
   (vector #xfd #x54 'v128.load8-lane)
   (vector #xfd #x55 'v128.load16-lane)
   (vector #xfd #x56 'v128.load32-lane)
   (vector #xfd #x57 'v128.load64-lane)
   (vector #xfd #x58 'v128.store8-lane)
   (vector #xfd #x59 'v128.store16-lane)
   (vector #xfd #x5a 'v128.store32-lane)
   (vector #xfd #x5b 'v128.store64-lane)
   (vector #xfd #x5c 'v128.load32-zero)
   (vector #xfd #x5d 'v128.load64-zero)
   (vector #xfd #x5e 'f32x4.demote-f64x2-zero)
   (vector #xfd #x5f 'f64x2.promote-low-f32x4)
   (vector #xfd #x60 'i8x16.abs)
   (vector #xfd #x61 'i8x16.neg)
   (vector #xfd #x62 'i8x16.popcnt)
   (vector #xfd #x63 'i8x16.all-true)
   (vector #xfd #x64 'i8x16.bitmask)
   (vector #xfd #x65 'i8x16.narrow-i16x8-s)
   (vector #xfd #x66 'i8x16.narrow-i16x8-u)
   (vector #xfd #x67 'f32x4.ceil)
   (vector #xfd #x68 'f32x4.floor)
   (vector #xfd #x69 'f32x4.trunc)
   (vector #xfd #x6a 'f32x4.nearest)
   (vector #xfd #x6b 'i8x16.shl)
   (vector #xfd #x6c 'i8x16.shr-s)
   (vector #xfd #x6d 'i8x16.shr-u)
   (vector #xfd #x6e 'i8x16.add)
   (vector #xfd #x6f 'i8x16.add-sat-s)
   (vector #xfd #x70 'i8x16.add-sat-u)
   (vector #xfd #x71 'i8x16.sub)
   (vector #xfd #x72 'i8x16.sub-sat-s)
   (vector #xfd #x73 'i8x16.sub-sat-u)
   (vector #xfd #x74 'f64x2.ceil)
   (vector #xfd #x75 'f64x2.floor)
   (vector #xfd #x76 'i8x16.min-s)
   (vector #xfd #x77 'i8x16.min-u)
   (vector #xfd #x78 'i8x16.max-s)
   (vector #xfd #x79 'i8x16.max-u)
   (vector #xfd #x7a 'f64x2.trunc)
   (vector #xfd #x7b 'i8x16.avgr-u)
   (vector #xfd #x7c 'i16x8.extadd-pairwise-i8x16-s)
   (vector #xfd #x7d 'i16x8.extadd-pairwise-i8x16-u)
   (vector #xfd #x7e 'i32x4.extadd-pairwise-i16x8-s)
   (vector #xfd #x7f 'i32x4.extadd-pairwise-i16x8-u)
   (vector #xfd #x80 'i16x8.abs)
   (vector #xfd #x81 'i16x8.neg)
   (vector #xfd #x82 'i16x8.q15mulr-sat-s)
   (vector #xfd #x83 'i16x8.all-true)
   (vector #xfd #x84 'i16x8.bitmask)
   (vector #xfd #x85 'i16x8.narrow-i32x4-s)
   (vector #xfd #x86 'i16x8.narrow-i32x4-u)
   (vector #xfd #x87 'i16x8.extend-low-i8x16-s)
   (vector #xfd #x88 'i16x8.extend-high-i8x16-s)
   (vector #xfd #x89 'i16x8.extend-low-i8x16-u)
   (vector #xfd #x8a 'i16x8.extend-high-i8x16-u)
   (vector #xfd #x8b 'i16x8.shl)
   (vector #xfd #x8c 'i16x8.shr-s)
   (vector #xfd #x8d 'i16x8.shr-u)
   (vector #xfd #x8e 'i16x8.add)
   (vector #xfd #x8f 'i16x8.add-sat-s)
   (vector #xfd #x90 'i16x8.add-sat-u)
   (vector #xfd #x91 'i16x8.sub)
   (vector #xfd #x92 'i16x8.sub-sat-s)
   (vector #xfd #x93 'i16x8.sub-sat-u)
   (vector #xfd #x94 'f64x2.nearest)
   (vector #xfd #x95 'i16x8.mul)
   (vector #xfd #x96 'i16x8.min-s)
   (vector #xfd #x97 'i16x8.min-u)
   (vector #xfd #x98 'i16x8.max-s)
   (vector #xfd #x99 'i16x8.max-u)
   (vector #xfd #x9b 'i16x8.avgr-u)
   (vector #xfd #x9c 'i16x8.extmul-low-i8x16-s)
   (vector #xfd #x9d 'i16x8.extmul-high-i8x16-s)
   (vector #xfd #x9e 'i16x8.extmul-low-i8x16-u)
   (vector #xfd #x9f 'i16x8.extmul-high-i8x16-u)
   (vector #xfd #xa0 'i32x4.abs)
   (vector #xfd #xa1 'i32x4.neg)
   (vector #xfd #xa3 'i32x4.all-true)
   (vector #xfd #xa4 'i32x4.bitmask)
   (vector #xfd #xa7 'i32x4.extend-low-i16x8-s)
   (vector #xfd #xa8 'i32x4.extend-high-i16x8-s)
   (vector #xfd #xa9 'i32x4.extend-low-i16x8-u)
   (vector #xfd #xaa 'i32x4.extend-high-i16x8-u)
   (vector #xfd #xab 'i32x4.shl)
   (vector #xfd #xac 'i32x4.shr-s)
   (vector #xfd #xad 'i32x4.shr-u)
   (vector #xfd #xae 'i32x4.add)
   (vector #xfd #xb1 'i32x4.sub)
   (vector #xfd #xb5 'i32x4.mul)
   (vector #xfd #xb6 'i32x4.min-s)
   (vector #xfd #xb7 'i32x4.min-u)
   (vector #xfd #xb8 'i32x4.max-s)
   (vector #xfd #xb9 'i32x4.max-u)
   (vector #xfd #xba 'i32x4.dot-i16x8-s)
   (vector #xfd #xbc 'i32x4.extmul-low-i16x8-s)
   (vector #xfd #xbd 'i32x4.extmul-high-i16x8-s)
   (vector #xfd #xbe 'i32x4.extmul-low-i16x8-u)
   (vector #xfd #xbf 'i32x4.extmul-high-i16x8-u)
   (vector #xfd #xc0 'i64x2.abs)
   (vector #xfd #xc1 'i64x2.neg)
   (vector #xfd #xc3 'i64x2.all-true)
   (vector #xfd #xc4 'i64x2.bitmask)
   (vector #xfd #xc7 'i64x2.extend-low-i32x4-s)
   (vector #xfd #xc8 'i64x2.extend-high-i32x4-s)
   (vector #xfd #xc9 'i64x2.extend-low-i32x4-u)
   (vector #xfd #xca 'i64x2.extend-high-i32x4-u)
   (vector #xfd #xcb 'i64x2.shl)
   (vector #xfd #xcc 'i64x2.shr-s)
   (vector #xfd #xcd 'i64x2.shr-u)
   (vector #xfd #xce 'i64x2.add)
   (vector #xfd #xd1 'i64x2.sub)
   (vector #xfd #xd5 'i64x2.mul)
   (vector #xfd #xd6 'i64x2.eq)
   (vector #xfd #xd7 'i64x2.ne)
   (vector #xfd #xd8 'i64x2.lt-s)
   (vector #xfd #xd9 'i64x2.gt-s)
   (vector #xfd #xda 'i64x2.le-s)
   (vector #xfd #xdb 'i64x2.ge-s)
   (vector #xfd #xdc 'i64x2.extmul-low-i32x4-s)
   (vector #xfd #xdd 'i64x2.extmul-high-i32x4-s)
   (vector #xfd #xde 'i64x2.extmul-low-i32x4-u)
   (vector #xfd #xdf 'i64x2.extmul-high-i32x4-u)
   (vector #xfd #xe0 'f32x4.abs)
   (vector #xfd #xe1 'f32x4.neg)
   (vector #xfd #xe3 'f32x4.sqrt)
   (vector #xfd #xe4 'f32x4.add)
   (vector #xfd #xe5 'f32x4.sub)
   (vector #xfd #xe6 'f32x4.mul)
   (vector #xfd #xe7 'f32x4.div)
   (vector #xfd #xe8 'f32x4.min)
   (vector #xfd #xe9 'f32x4.max)
   (vector #xfd #xea 'f32x4.pmin)
   (vector #xfd #xeb 'f32x4.pmax)
   (vector #xfd #xec 'f64x2.abs)
   (vector #xfd #xed 'f64x2.neg)
   (vector #xfd #xef 'f64x2.sqrt)
   (vector #xfd #xf0 'f64x2.add)
   (vector #xfd #xf1 'f64x2.sub)
   (vector #xfd #xf2 'f64x2.mul)
   (vector #xfd #xf3 'f64x2.div)
   (vector #xfd #xf4 'f64x2.min)
   (vector #xfd #xf5 'f64x2.max)
   (vector #xfd #xf6 'f64x2.pmin)
   (vector #xfd #xf7 'f64x2.pmax)
   (vector #xfd #xf8 'i32x4.trunc-sat-f32x4-s)
   (vector #xfd #xf9 'i32x4.trunc-sat-f32x4-u)
   (vector #xfd #xfa 'f32x4.convert-i32x4-s)
   (vector #xfd #xfb 'f32x4.convert-i32x4-u)
   (vector #xfd #xfc 'i32x4.trunc-sat-f64x2-s-zero)
   (vector #xfd #xfd 'i32x4.trunc-sat-f64x2-u-zero)
   (vector #xfd #xfe 'f64x2.convert-low-i32x4-s)
   (vector #xfd #xff 'f64x2.convert-low-i32x4-u)
   (vector #xfd #x100 'i8x16.relaxed-swizzle)
   (vector #xfd #x101 'i32x4.relaxed-trunc-f32x4-s)
   (vector #xfd #x102 'i32x4.relaxed-trunc-f32x4-u)
   (vector #xfd #x103 'i32x4.relaxed-trunc-f64x2-s-zero)
   (vector #xfd #x104 'i32x4.relaxed-trunc-f64x2-u-zero)
   (vector #xfd #x105 'f32x4.relaxed-madd)
   (vector #xfd #x106 'f32x4.relaxed-nmadd)
   (vector #xfd #x107 'f64x2.relaxed-madd)
   (vector #xfd #x108 'f64x2.relaxed-nmadd)
   (vector #xfd #x109 'i8x16.relaxed-laneselect)
   (vector #xfd #x10a 'i16x8.relaxed-laneselect)
   (vector #xfd #x10b 'i32x4.relaxed-laneselect)
   (vector #xfd #x10c 'i64x2.relaxed-laneselect)
   (vector #xfd #x10d 'f32x4.relaxed-min)
   (vector #xfd #x10e 'f32x4.relaxed-max)
   (vector #xfd #x10f 'f64x2.relaxed-min)
   (vector #xfd #x110 'f64x2.relaxed-max)
   (vector #xfd #x111 'i16x8.relaxed-q15mulr-s)
   (vector #xfd #x112 'i16x8.relaxed-dot-i8x16-i7x16-s)
   (vector #xfd #x113 'i32x4.relaxed-dot-i8x16-i7x16-add-s)
   ))

;; Shapes follow the final Core 3.0 binary grammar and the normalized order below.
(define expected-core-3-special-opcodes
  (vector
   ;; one-byte special descriptors
   (vector #f #x2 'block 'block-type 'block)
   (vector #f #x3 'loop 'block-type 'loop)
   (vector #f #x4 'if 'block-type 'if)
   (vector #f #x8 'throw 'tag-index #f)
   (vector #f #xc 'br 'label-index #f)
   (vector #f #xd 'br-if 'label-index #f)
   (vector #f #xe 'br-table 'label-vector #f)
   (vector #f #x10 'call 'function-index #f)
   (vector #f #x11 'call-indirect 'call-indirect #f)
   (vector #f #x12 'return-call 'function-index #f)
   (vector #f #x13 'return-call-indirect 'call-indirect #f)
   (vector #f #x14 'call-ref 'type-index #f)
   (vector #f #x15 'return-call-ref 'type-index #f)
   (vector #f #x1c 'select 'select-types #f)
   (vector #f #x1f 'try-table 'try-table 'try-table)
   (vector #f #x20 'local.get 'local-index #f)
   (vector #f #x21 'local.set 'local-index #f)
   (vector #f #x22 'local.tee 'local-index #f)
   (vector #f #x23 'global.get 'global-index #f)
   (vector #f #x24 'global.set 'global-index #f)
   (vector #f #x25 'table.get 'table-index #f)
   (vector #f #x26 'table.set 'table-index #f)
   (vector #f #x28 'i32.load 'memory-argument #f)
   (vector #f #x29 'i64.load 'memory-argument #f)
   (vector #f #x2a 'f32.load 'memory-argument #f)
   (vector #f #x2b 'f64.load 'memory-argument #f)
   (vector #f #x2c 'i32.load8-s 'memory-argument #f)
   (vector #f #x2d 'i32.load8-u 'memory-argument #f)
   (vector #f #x2e 'i32.load16-s 'memory-argument #f)
   (vector #f #x2f 'i32.load16-u 'memory-argument #f)
   (vector #f #x30 'i64.load8-s 'memory-argument #f)
   (vector #f #x31 'i64.load8-u 'memory-argument #f)
   (vector #f #x32 'i64.load16-s 'memory-argument #f)
   (vector #f #x33 'i64.load16-u 'memory-argument #f)
   (vector #f #x34 'i64.load32-s 'memory-argument #f)
   (vector #f #x35 'i64.load32-u 'memory-argument #f)
   (vector #f #x36 'i32.store 'memory-argument #f)
   (vector #f #x37 'i64.store 'memory-argument #f)
   (vector #f #x38 'f32.store 'memory-argument #f)
   (vector #f #x39 'f64.store 'memory-argument #f)
   (vector #f #x3a 'i32.store8 'memory-argument #f)
   (vector #f #x3b 'i32.store16 'memory-argument #f)
   (vector #f #x3c 'i64.store8 'memory-argument #f)
   (vector #f #x3d 'i64.store16 'memory-argument #f)
   (vector #f #x3e 'i64.store32 'memory-argument #f)
   (vector #f #x3f 'memory.size 'memory-index #f)
   (vector #f #x40 'memory.grow 'memory-index #f)
   (vector #f #x41 'i32.const 'i32 #f)
   (vector #f #x42 'i64.const 'i64 #f)
   (vector #f #x43 'f32.const 'f32 #f)
   (vector #f #x44 'f64.const 'f64 #f)
   (vector #f #xd0 'ref.null 'heap-type #f)
   (vector #f #xd2 'ref.func 'function-index #f)
   (vector #f #xd5 'br-on-null 'label-index #f)
   (vector #f #xd6 'br-on-non-null 'label-index #f)
   ;; #xfb aggregate, cast, and branch descriptors
   (vector #xfb #x0 'struct.new 'type-index #f)
   (vector #xfb #x1 'struct.new-default 'type-index #f)
   (vector #xfb #x2 'struct.get 'struct-field #f)
   (vector #xfb #x3 'struct.get-s 'struct-field #f)
   (vector #xfb #x4 'struct.get-u 'struct-field #f)
   (vector #xfb #x5 'struct.set 'struct-field #f)
   (vector #xfb #x6 'array.new 'type-index #f)
   (vector #xfb #x7 'array.new-default 'type-index #f)
   (vector #xfb #x8 'array.new-fixed 'array-new-fixed #f)
   (vector #xfb #x9 'array.new-data 'type-data #f)
   (vector #xfb #xa 'array.new-elem 'type-element #f)
   (vector #xfb #xb 'array.get 'type-index #f)
   (vector #xfb #xc 'array.get-s 'type-index #f)
   (vector #xfb #xd 'array.get-u 'type-index #f)
   (vector #xfb #xe 'array.set 'type-index #f)
   (vector #xfb #x10 'array.fill 'type-index #f)
   (vector #xfb #x11 'array.copy 'array-copy #f)
   (vector #xfb #x12 'array.init-data 'type-data #f)
   (vector #xfb #x13 'array.init-elem 'type-element #f)
   (vector #xfb #x14 'ref.test 'heap-type-non-null #f)
   (vector #xfb #x15 'ref.test 'heap-type-nullable #f)
   (vector #xfb #x16 'ref.cast 'heap-type-non-null #f)
   (vector #xfb #x17 'ref.cast 'heap-type-nullable #f)
   (vector #xfb #x18 'br-on-cast 'br-on-cast #f)
   (vector #xfb #x19 'br-on-cast-fail 'br-on-cast #f)
   ;; #xfc bulk descriptors
   (vector #xfc #x8 'memory.init 'memory-data #f)
   (vector #xfc #x9 'data.drop 'data-index #f)
   (vector #xfc #xa 'memory.copy 'memory-pair #f)
   (vector #xfc #xb 'memory.fill 'memory-index #f)
   (vector #xfc #xc 'table.init 'table-element #f)
   (vector #xfc #xd 'elem.drop 'element-index #f)
   (vector #xfc #xe 'table.copy 'table-pair #f)
   (vector #xfc #xf 'table.grow 'table-index #f)
   (vector #xfc #x10 'table.size 'table-index #f)
   (vector #xfc #x11 'table.fill 'table-index #f)
   ;; #xfd vector descriptors
   (vector #xfd #x0 'v128.load 'memory-argument #f)
   (vector #xfd #x1 'v128.load8x8-s 'memory-argument #f)
   (vector #xfd #x2 'v128.load8x8-u 'memory-argument #f)
   (vector #xfd #x3 'v128.load16x4-s 'memory-argument #f)
   (vector #xfd #x4 'v128.load16x4-u 'memory-argument #f)
   (vector #xfd #x5 'v128.load32x2-s 'memory-argument #f)
   (vector #xfd #x6 'v128.load32x2-u 'memory-argument #f)
   (vector #xfd #x7 'v128.load8-splat 'memory-argument #f)
   (vector #xfd #x8 'v128.load16-splat 'memory-argument #f)
   (vector #xfd #x9 'v128.load32-splat 'memory-argument #f)
   (vector #xfd #xa 'v128.load64-splat 'memory-argument #f)
   (vector #xfd #xb 'v128.store 'memory-argument #f)
   (vector #xfd #xc 'v128.const 'vector-bytes #f)
   (vector #xfd #xd 'i8x16.shuffle 'shuffle-bytes #f)
   (vector #xfd #x15 'i8x16.extract-lane-s 'lane-index #f)
   (vector #xfd #x16 'i8x16.extract-lane-u 'lane-index #f)
   (vector #xfd #x17 'i8x16.replace-lane 'lane-index #f)
   (vector #xfd #x18 'i16x8.extract-lane-s 'lane-index #f)
   (vector #xfd #x19 'i16x8.extract-lane-u 'lane-index #f)
   (vector #xfd #x1a 'i16x8.replace-lane 'lane-index #f)
   (vector #xfd #x1b 'i32x4.extract-lane 'lane-index #f)
   (vector #xfd #x1c 'i32x4.replace-lane 'lane-index #f)
   (vector #xfd #x1d 'i64x2.extract-lane 'lane-index #f)
   (vector #xfd #x1e 'i64x2.replace-lane 'lane-index #f)
   (vector #xfd #x1f 'f32x4.extract-lane 'lane-index #f)
   (vector #xfd #x20 'f32x4.replace-lane 'lane-index #f)
   (vector #xfd #x21 'f64x2.extract-lane 'lane-index #f)
   (vector #xfd #x22 'f64x2.replace-lane 'lane-index #f)
   (vector #xfd #x54 'v128.load8-lane 'memory-argument-lane #f)
   (vector #xfd #x55 'v128.load16-lane 'memory-argument-lane #f)
   (vector #xfd #x56 'v128.load32-lane 'memory-argument-lane #f)
   (vector #xfd #x57 'v128.load64-lane 'memory-argument-lane #f)
   (vector #xfd #x58 'v128.store8-lane 'memory-argument-lane #f)
   (vector #xfd #x59 'v128.store16-lane 'memory-argument-lane #f)
   (vector #xfd #x5a 'v128.store32-lane 'memory-argument-lane #f)
   (vector #xfd #x5b 'v128.store64-lane 'memory-argument-lane #f)
   (vector #xfd #x5c 'v128.load32-zero 'memory-argument #f)
   (vector #xfd #x5d 'v128.load64-zero 'memory-argument #f)
   ))

(define wasm-opcode-immediate-shapes
  '(none block-type label-index label-vector function-index type-index table-index
    memory-index global-index local-index tag-index field-index data-index element-index
    heap-type reference-type value-type-vector select-types call-indirect br-on-cast
    memory-argument memory-argument-lane lane-index shuffle-bytes vector-bytes i32 i64
    f32 f64 table-pair memory-pair array-new-fixed array-copy struct-field try-table
    resume-table type-data type-element memory-data table-element heap-type-non-null
    heap-type-nullable))

(define wasm-opcode-structured-kinds '(#f block loop if try-table))

(define expected-core-3-binary-variants
  (vector (vector #f #x1b 'select 'none #f)
          (vector #xfb 21 'ref.test 'heap-type-nullable #f)
          (vector #xfb 23 'ref.cast 'heap-type-nullable #f)))

(define unique-values?
  (lambda (values)
    (let ([seen (make-hashtable equal-hash equal?)])
      (andmap (lambda (value)
                (and (not (hashtable-ref seen value #f))
                     (begin (hashtable-set! seen value #t) #t)))
              values))))

(define binary-assignment-pair
  (lambda (assignment)
    (cons (vector-ref assignment 0) (vector-ref assignment 1))))

(define actual-core-3-binary-pairs
  (lambda ()
    (append
     (map (lambda (descriptor)
            (cons (wasm-opcode-prefix descriptor) (wasm-opcode-code descriptor)))
          (vector->list wasm-core-3-opcodes))
     (map binary-assignment-pair
          (vector->list expected-core-3-binary-variants)))))

(define descriptor-fields=?
  (lambda (descriptor prefix code mnemonic immediate-shape structured-kind)
    (and (wasm-opcode-descriptor? descriptor)
         (equal? prefix (wasm-opcode-prefix descriptor))
         (= code (wasm-opcode-code descriptor))
         (eq? mnemonic (wasm-opcode-mnemonic descriptor))
         (eq? immediate-shape (wasm-opcode-immediate-shape descriptor))
         (eq? structured-kind (wasm-opcode-structured-kind descriptor)))))

(define special-opcode-fields=?
  (lambda (expected)
    (descriptor-fields=?
     (wasm-opcode-by-binary (vector-ref expected 0) (vector-ref expected 1))
     (vector-ref expected 0) (vector-ref expected 1) (vector-ref expected 2)
     (vector-ref expected 3) (vector-ref expected 4))))

(define descriptor-special?
  (lambda (descriptor)
    (or (not (eq? 'none (wasm-opcode-immediate-shape descriptor)))
        (wasm-opcode-structured-kind descriptor))))

(define descriptor-special-fields
  (lambda (descriptor)
    (vector (wasm-opcode-prefix descriptor) (wasm-opcode-code descriptor)
            (wasm-opcode-mnemonic descriptor) (wasm-opcode-immediate-shape descriptor)
            (wasm-opcode-structured-kind descriptor))))

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

     (= 499 (vector-length expected-core-3-binary-assignments))

     (= 128 (vector-length expected-core-3-special-opcodes))

     (unique-values?
      (map binary-assignment-pair
           (vector->list expected-core-3-binary-assignments)))

     (unique-values?
      (map binary-assignment-pair
           (vector->list expected-core-3-special-opcodes)))

     (let ([official-pairs
            (map binary-assignment-pair
                 (vector->list expected-core-3-binary-assignments))])
       (andmap (lambda (special)
                 (and (member (binary-assignment-pair special) official-pairs) #t))
               (vector->list expected-core-3-special-opcodes)))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 0))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 1))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 2))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 3))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 4))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 5))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 6))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 7))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 8))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 9))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 10))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 11))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 12))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 13))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 14))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 15))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 16))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 17))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 18))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 19))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 20))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 21))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 22))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 23))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 24))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 25))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 26))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 27))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 28))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 29))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 30))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 31))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 32))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 33))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 34))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 35))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 36))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 37))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 38))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 39))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 40))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 41))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 42))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 43))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 44))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 45))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 46))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 47))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 48))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 49))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 50))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 51))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 52))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 53))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 54))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 55))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 56))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 57))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 58))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 59))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 60))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 61))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 62))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 63))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 64))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 65))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 66))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 67))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 68))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 69))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 70))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 71))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 72))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 73))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 74))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 75))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 76))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 77))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 78))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 79))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 80))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 81))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 82))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 83))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 84))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 85))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 86))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 87))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 88))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 89))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 90))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 91))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 92))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 93))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 94))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 95))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 96))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 97))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 98))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 99))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 100))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 101))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 102))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 103))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 104))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 105))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 106))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 107))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 108))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 109))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 110))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 111))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 112))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 113))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 114))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 115))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 116))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 117))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 118))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 119))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 120))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 121))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 122))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 123))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 124))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 125))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 126))

     (special-opcode-fields=? (vector-ref expected-core-3-special-opcodes 127))

     (let ([special-pairs
            (map binary-assignment-pair
                 (vector->list expected-core-3-special-opcodes))])
       (andmap
        (lambda (assignment)
          (if (member (binary-assignment-pair assignment) special-pairs)
              #t
              (let ([descriptor
                     (wasm-opcode-by-binary (vector-ref assignment 0)
                                            (vector-ref assignment 1))])
                (and (eq? 'none (wasm-opcode-immediate-shape descriptor))
                     (not (wasm-opcode-structured-kind descriptor))))))
        (vector->list expected-core-3-binary-assignments)))

     (let ([expected-special (vector->list expected-core-3-special-opcodes)])
       (andmap
        (lambda (assignment)
          (let ([descriptor
                 (wasm-opcode-by-binary (vector-ref assignment 0)
                                        (vector-ref assignment 1))])
            (or (not (descriptor-special? descriptor))
                (and (member (descriptor-special-fields descriptor) expected-special)
                     #t))))
        (vector->list expected-core-3-binary-assignments)))

     (= 128
        (length
         (filter descriptor-special?
                 (map (lambda (assignment)
                        (wasm-opcode-by-binary (vector-ref assignment 0)
                                               (vector-ref assignment 1)))
                      (vector->list expected-core-3-binary-assignments)))))

     (andmap
      (lambda (assignment)
        (let ([descriptor
               (wasm-opcode-by-binary (vector-ref assignment 0)
                                      (vector-ref assignment 1))])
          (and (wasm-opcode-descriptor? descriptor)
               (eq? (vector-ref assignment 2)
                    (wasm-opcode-mnemonic descriptor)))))
      (vector->list expected-core-3-binary-assignments))

     (let ([expected-pairs
            (map binary-assignment-pair
                 (vector->list expected-core-3-binary-assignments))])
       (andmap (lambda (actual-pair)
                 (and (member actual-pair expected-pairs) #t))
               (actual-core-3-binary-pairs)))

     (let ([actual-pairs (actual-core-3-binary-pairs)])
       (andmap (lambda (expected-pair)
                 (and (member expected-pair actual-pairs) #t))
               (map binary-assignment-pair
                    (vector->list expected-core-3-binary-assignments))))

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
                          #xfc 8 'memory.init 'memory-data #f)

     (descriptor-fields=? (wasm-opcode-by-mnemonic 'v128.load)
                          #xfd 0 'v128.load 'memory-argument #f)

     ;; error case: an unassigned binary pair has no descriptor.
     (not (wasm-opcode-by-binary #xfd #xffff))

     ;; error case: the largest u32 subopcode is valid input but unassigned.
     (not (wasm-opcode-by-binary #xfd #xffffffff))

     ;; error case: a subopcode above the u32 range violates the public contract.
     (error? (wasm-opcode-by-binary #xfd #x100000000))

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
