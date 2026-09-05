(module
  (; A nested (; Core 3.0 ;) comment. ;)

  (type $node (struct (field $next (mut funcref))))
  (type $nodes (array (mut i32)))
  (type $unary (func (param i32) (result i32)))
  (type $visitor (func (param (ref null $unary)) (result i32)))
  (type $tag-type (func (param i32)))

  (import "env" "imported" (func $imported (type $unary)))

  (table $functions 2 2 funcref)
  (memory $memory64 i64 1 2)
  (memory $memory32 1 2)
  (global $counter (mut i32) (i32.const 0))
  (global $nan f32 (f32.const nan:0x123))
  (tag $tag (type $tag-type))

  (func $identity (export "identity") (type $unary) (param $value i32) (result i32)
    local.get $value)

  (func $tail (export "tail") (type $unary) (param $value i32) (result i32)
    local.get $value
    return_call $identity)

  (func $aggregate (export "aggregate") (type $unary) (param $value i32) (result i32)
    (memory.copy $memory32 $memory32 (i32.const 0) (i32.const 0) (i32.const 1))
    local.get $value)

  (func $copy-functions
    (table.copy $functions $functions (i32.const 0) (i32.const 0) (i32.const 1)))

  (func $vector (export "vector") (result v128)
    (v128.const i32x4 1 2 3 4))

  (func $folded (export "folded") (result i32)
    (i32.add (i32.const 20) (i32.const 22)))

  (elem (table $functions) (offset (i32.const 0)) func $identity $tail)
  (data (memory $memory32) (offset (i32.const 0)) "core3")
  (@custom "chezpp.core3" (after data) "fixture")
)
