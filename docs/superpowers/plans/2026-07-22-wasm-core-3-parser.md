# WebAssembly Core 3.0 Parser Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development
> (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the experimental WebAssembly parser with complete Core 3.0 binary and single-module
WAT parsers that return one documented, typed canonical representation.

**Architecture:** Build binary and textual grammars entirely from Chezpp parser combinators. Decode
both syntaxes into shared immutable records, normalize WAT identifiers and abbreviations after
grammar recognition, and enforce decoding-time structural constraints without implementing the Core
instruction type-validation algorithm.

**Tech Stack:** ChezScheme, Chezpp parser combinators, WebAssembly Core 3.0 (2026-07-10), the `mat`
test harness, and WABT 1.0.41 for development-time fixture generation and inspection only.

---

## File Map

- Create `chezpp/parser/wasm/types.ss`: checked public canonical records and value predicates.
- Create `chezpp/parser/wasm/opcodes.ss`: internal Core 3.0 mnemonic/opcode/immediate descriptors.
- Create `chezpp/parser/wasm/binary/values.ss`: bounded LEB128, names, bytes, and vector combinators.
- Create `chezpp/parser/wasm/binary/types.ss`: Core 3.0 binary type combinators.
- Create `chezpp/parser/wasm/binary/instructions.ss`: binary instruction and expression combinators.
- Replace `chezpp/parser/wasm/binary.ss`: bounded standard-section and module combinators.
- Create `chezpp/parser/wasm/text/lexical.ss`: WAT trivia, token, string, identifier, and number parsers.
- Create `chezpp/parser/wasm/text/types.ss`: private positioned WAT syntax records and type parsers.
- Create `chezpp/parser/wasm/text/instructions.ss`: flat and folded WAT instruction parsers.
- Create `chezpp/parser/wasm/text.ss`: one-module WAT grammar and intermediate module parser.
- Create `chezpp/parser/wasm/normalize.ss`: namespace resolution and abbreviation expansion.
- Create `chezpp/parser/wasm/validate.ss`: positioned structural issues and shared invariants.
- Replace `chezpp/parser/wasm.ss`: documented public facade and record re-exports.
- Create `tests/parser-wasm.ss`: content-level positive, negative, contract, and fixture tests.
- Modify `tests/parser.ss`: update the aggregate parser smoke test for the canonical API.
- Modify `tests/Makefile`: include `parser-wasm.ss` in `PARSER_TESTS`.
- Create `tests/data/wasm-core3.wat` and `tests/data/wasm-core3.wasm`: equivalent focused fixtures.
- Add `tests/data/example.wasm` and `tests/data/fibonacci.wasm`: selected existing relative fixtures.

### Task 0: Confirm The Baseline And Preserve Existing Worktree State

**Files:**
- Read only: existing worktree files and generated test output

- [ ] **Step 1: Record the pre-existing worktree state**

Run: `git status --short --branch`

Expected: branch `finish-parsers-1`; preserve the pre-existing untracked `chezpp/c/scheme.h`,
`tests/parser-xml.stdout`, and `tests/parser-xml.stderr` files throughout implementation.

- [ ] **Step 2: Run the mandatory clean baseline build**

Run: `make clean && make`

Expected: exit 0 with the current library rebuilt.

- [ ] **Step 3: Run the existing aggregate parser test**

Run: `cd tests && make test-some TEST='parser'`

Expected: exit 0; `parser.stdout` and `parser.stderr` are empty. Any baseline failure must be
recorded and separated from failures introduced by the WASM tasks.

### Task 1: Establish The Canonical Public Record Model

**Files:**
- Create: `chezpp/parser/wasm/types.ss`
- Modify: `chezpp/parser/wasm.ss:1-298`
- Create: `tests/parser-wasm.ss`
- Modify: `tests/Makefile:3-5`

- [ ] **Step 1: Add the dedicated test target and failing record-contract tests**

Add `parser-wasm.ss` to `PARSER_TESTS`, import `(chezpp parser wasm)`, and start the test file with
exact field assertions for the canonical model:

```scheme
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

     ;; error: record accessors reject values of the wrong record type.
     (error? (wasm-module-types 'not-a-module))

     ;; error: limits require an i32 or i64 address type.
     (error? (make-wasm-limits 'f32 0 #f))

     )
```

- [ ] **Step 2: Run the new target and verify that the missing API fails**

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: compilation fails because `make-wasm-module` and the other record APIs are unbound.

- [ ] **Step 3: Implement checked records and predicates in `wasm/types.ss`**

Use the ELF checked-record pattern, with `pcheck` on every exported constructor, predicate, and
accessor. Define and document this complete record matrix:

| Record | Fields |
| --- | --- |
| `wasm-module` | `types imports functions tables memories globals tags exports start elements data custom-sections` |
| `wasm-custom-section` | `name bytes after-section` |
| `wasm-recursive-type` | `subtypes` |
| `wasm-subtype` | `final? supertypes composite-type` |
| `wasm-function-type` | `parameters results` |
| `wasm-struct-type` | `fields` |
| `wasm-array-type` | `field` |
| `wasm-field-type` | `storage-type mutable?` |
| `wasm-reference-type` | `nullable? heap-type` |
| `wasm-limits` | `address-type minimum maximum` |
| `wasm-table-type` | `reference-type limits` |
| `wasm-memory-type` | `limits` |
| `wasm-global-type` | `value-type mutable?` |
| `wasm-tag-type` | `type-index` |
| `wasm-external-type` | `kind type` |
| `wasm-import` | `module name external-type` |
| `wasm-function` | `type-index locals body` |
| `wasm-table` | `type initializer` |
| `wasm-memory` | `type` |
| `wasm-global` | `type initializer` |
| `wasm-tag` | `type` |
| `wasm-export` | `name kind index` |
| `wasm-element` | `mode reference-type table-index offset initializers` |
| `wasm-data` | `mode memory-index offset bytes` |
| `wasm-instruction` | `mnemonic immediates body alternate` |
| `wasm-memory-argument` | `alignment offset memory-index` |
| `wasm-block-type` | `kind value` |
| `wasm-catch` | `kind tag-index label-index` |
| `wasm-float` | `width bits` |

The macro body must follow this shape, and each invocation must be preceded by `#|proc:...|#`
documentation for its maker, predicate, and accessors:

```scheme
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
```

Represent numeric/vector value types as symbols `i32`, `i64`, `f32`, `f64`, and `v128`; represent
reference value types with `wasm-reference-type`. Permit heap types to be a type index or one of
`any`, `eq`, `i31`, `struct`, `array`, `none`, `func`, `nofunc`, `exn`, `noexn`, `extern`, and
`noextern`. Store float constants as `wasm-float` bit patterns so NaN payloads survive binary/WAT
normalization.

- [ ] **Step 4: Turn `wasm.ss` into a facade without changing the parser entry point yet**

Import `(chezpp parser wasm types)`, re-export every public maker/predicate/accessor, and retain the
existing `parse-wasm-binary-module` implementation temporarily. Remove no parser behavior in this
commit.

- [ ] **Step 5: Run focused tests and verify the record layer passes**

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: exit 0; `parser-wasm.stdout` and `parser-wasm.stderr` are empty.

- [ ] **Step 6: Commit the record model**

```bash
git add chezpp/parser/wasm/types.ss chezpp/parser/wasm.ss tests/parser-wasm.ss tests/Makefile
git commit -m "wasm: define Core 3.0 parser data structures"
```

### Task 2: Implement Core Binary Values And Types

**Files:**
- Create: `chezpp/parser/wasm/binary/values.ss`
- Create: `chezpp/parser/wasm/binary/types.ss`
- Modify: `tests/parser-wasm.ss`

- [ ] **Step 1: Add failing binary value and type tests**

Import both internal libraries and test exact values for legal non-minimal LEB encodings, width
boundaries, type shorthands, recursive types, and memory64 limits:

```scheme
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
```

- [ ] **Step 2: Run tests and verify the binary combinators are missing**

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: compilation fails with unbound `<wasm-u32>` and `<wasm-reference-type>` identifiers.

- [ ] **Step 3: Implement bounded LEB128 and vector/name combinators**

In `binary/values.ss`, export documented `<wasm-u32>`, `<wasm-u64>`, `<wasm-s32>`, `<wasm-s33>`,
`<wasm-s64>`, `<wasm-f32>`, `<wasm-f64>`, `<wasm-byte-vector>`, `<wasm-name>`, and
`<wasm-vector>`. Consume bytes only through `<u8>`, `<bind>`, `<map>`, `<rep>`, and `<fail-with>`.
The unsigned decoder must use this termination rule:

```scheme
(define (wasm-unsigned-parser width)
  (let ([maximum-bytes (quotient (+ width 6) 7)])
    (letrec ([next
              (lambda (index shift value)
                (<bind>
                 <u8>
                 (lambda (byte)
                   (let* ([payload (logand byte #x7f)]
                          [remaining (- width shift)]
                          [limit (expt 2 (min 7 remaining))]
                          [continued? (not (zero? (logand byte #x80)))])
                     (cond [(>= payload limit)
                            (<fail-with> "integer has nonzero unused bits")]
                           [(and continued? (= index (fx1- maximum-bytes)))
                            (<fail-with> "integer encoding is too long")]
                           [continued?
                            (next (fx1+ index) (+ shift 7)
                                  (+ value (ash payload shift)))]
                           [else
                            (<result> (+ value (ash payload shift)))])))))])
      (next 0 0 0))))
```

Implement signed termination with sign extension and require all unused high bits in the final byte
to agree with its sign bit. Decode names by parsing a u32-sized bytevector and converting it with a
raising UTF-8 transcoder inside `<bind>`; return `<fail-with>` on malformed UTF-8.

- [ ] **Step 4: Implement the complete Core 3.0 binary type grammar**

In `binary/types.ss`, define combinators for number, vector, heap, reference, value, storage,
field, composite, subtype, recursive, block, global, table, memory, tag, and external types. Cover
the explicit encodings and shorthand encodings from Core 3.0, including `0x4e` recursive groups,
`0x4f`/`0x50` subtypes, `0x5e` arrays, `0x5f` structs, `0x60` functions, nullable/non-null reference
forms, abstract heap types, concrete signed type indexes, and i32/i64 limits flags.

Use typed mapping functions rather than positional lists at the library boundary:

```scheme
(define <wasm-function-type>
  (<map> (lambda (field*)
           (make-wasm-function-type (car field*) (cadr field*)))
         (<~> (<uimm8> #x60)
              (<wasm-vector> <wasm-value-type>)
              (<wasm-vector> <wasm-value-type>))))

(define <wasm-limits>
  (<bind> <u8>
          (lambda (flags)
            (case flags
              [(#x00) (<map> (lambda (minimum)
                               (make-wasm-limits 'i32 minimum #f))
                             <wasm-u64>)]
              [(#x01) (<map> (lambda (bounds)
                               (make-wasm-limits 'i32 (car bounds) (cadr bounds)))
                             (<~> <wasm-u64> <wasm-u64>))]
              [(#x04) (<map> (lambda (minimum)
                               (make-wasm-limits 'i64 minimum #f))
                             <wasm-u64>)]
              [(#x05) (<map> (lambda (bounds)
                               (make-wasm-limits 'i64 (car bounds) (cadr bounds)))
                             (<~> <wasm-u64> <wasm-u64>))]
              [else (<fail-with> "invalid WebAssembly limits flags")]))))
```

- [ ] **Step 5: Add malformed type cases and rerun focused tests**

Add commented negative cases for bad mutability, tag attributes, heap encodings, recursive group
lengths, invalid limits flags, and malformed UTF-8 names.

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: exit 0 with empty test stdout/stderr.

- [ ] **Step 6: Commit binary values and types**

```bash
git add chezpp/parser/wasm/binary/values.ss chezpp/parser/wasm/binary/types.ss \
  tests/parser-wasm.ss
git commit -m "wasm: parse Core 3.0 binary values and types"
```

### Task 3: Define The Shared Core 3.0 Opcode Table

**Files:**
- Create: `chezpp/parser/wasm/opcodes.ss`
- Modify: `tests/parser-wasm.ss`

- [ ] **Step 1: Add failing descriptor completeness tests**

Create an explicit expected mnemonic vector in the test, grouped by the specification's parametric,
control, variable, table, memory, reference, aggregate, numeric, and vector families. Assert that
every expected mnemonic has exactly one descriptor, every binary `(prefix, code)` pair is unique,
and no table descriptor is absent from the explicit expected vector.

```scheme
(mat wasm-opcode-table

     (andmap (lambda (mnemonic)
               (wasm-opcode-descriptor? (wasm-opcode-by-mnemonic mnemonic)))
             (vector->list expected-core-3-mnemonics))

     (= (vector-length expected-core-3-mnemonics)
        (vector-length wasm-core-3-opcodes))

     (= (vector-length wasm-core-3-opcodes)
        (length
         (delete-duplicates
          (map (lambda (descriptor)
                 (cons (wasm-opcode-prefix descriptor)
                       (wasm-opcode-code descriptor)))
               (vector->list wasm-core-3-opcodes)))))

     )
```

- [ ] **Step 2: Run tests and verify descriptor APIs are missing**

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: compilation fails with unbound opcode descriptor identifiers.

- [ ] **Step 3: Implement the internal descriptor table**

Define an internal immutable descriptor with fields `mnemonic`, `prefix`, `code`, `immediate-shape`,
and `structured-kind`. Use `#f` for one-byte opcodes and numeric prefixes for prefixed encodings.
Use a closed immediate vocabulary:

```scheme
'(none block-type label-index label-vector function-index type-index table-index memory-index
  global-index local-index tag-index field-index data-index element-index heap-type
  reference-type value-type-vector select-types call-indirect br-on-cast
  memory-argument memory-argument-lane lane-index shuffle-bytes vector-bytes
  i32 i64 f32 f64 table-pair memory-pair array-new-fixed array-copy struct-field
  try-table resume-table)
```

Populate one row for every instruction in the Core 3.0 instruction index. Representative rows must
look like this; the final file contains all rows, not numeric ranges:

```scheme
(define wasm-core-3-opcodes
  (vector
   (make-wasm-opcode-descriptor 'unreachable #f #x00 'none #f)
   (make-wasm-opcode-descriptor 'block #f #x02 'block-type 'block)
   (make-wasm-opcode-descriptor 'call-indirect #f #x11 'call-indirect #f)
   (make-wasm-opcode-descriptor 'struct.get #xfb 2 'struct-field #f)
   (make-wasm-opcode-descriptor 'memory.init #xfc 8 'data-index #f)
   (make-wasm-opcode-descriptor 'v128.load #xfd 0 'memory-argument #f)))
```

Build immutable lookup hashtables once at library initialization. `wasm-opcode-by-binary` returns
`#f` for an unknown pair; `wasm-opcode-by-mnemonic` returns `#f` for an unknown mnemonic.

- [ ] **Step 4: Run descriptor tests**

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: exit 0; the explicit mnemonic list and descriptor vector agree exactly.

- [ ] **Step 5: Commit opcode metadata**

```bash
git add chezpp/parser/wasm/opcodes.ss tests/parser-wasm.ss
git commit -m "wasm: define Core 3.0 instruction metadata"
```

### Task 4: Decode Scalar And Structured Binary Instructions

**Files:**
- Create: `chezpp/parser/wasm/binary/instructions.ss`
- Modify: `tests/parser-wasm.ss`

- [ ] **Step 1: Add failing tests for immediate shapes and control trees**

Test no-immediate, index, branch table, call-indirect, memory argument, constants, typed select,
block, loop, if/else, try-table/catches, and expression termination. Assert exact mnemonics,
immediates, bodies, and alternates:

```scheme
(mat wasm-binary-instructions

     (let ([instruction
            (run-binary-parser <wasm-instruction>
                               (bytevector #x41 #x7f))])
       (and (eq? 'i32.const (wasm-instruction-mnemonic instruction))
            (= -1 (vector-ref (wasm-instruction-immediates instruction) 0))))

     (let ([instruction
            (run-binary-parser <wasm-instruction>
                               (bytevector #x04 #x40 #x41 #x01 #x05 #x41 #x02 #x0b))])
       (and (eq? 'if (wasm-instruction-mnemonic instruction))
            (= 1 (vector-length (wasm-instruction-body instruction)))
            (= 1 (vector-length (wasm-instruction-alternate instruction)))))

     ;; error: a memory alignment immediate cannot be truncated.
     (error? (run-binary-parser <wasm-instruction> (bytevector #x28 #x80)))

     )
```

- [ ] **Step 2: Run tests and verify instruction combinators are missing**

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: compilation fails with unbound `<wasm-instruction>`.

- [ ] **Step 3: Implement descriptor-driven immediate parsers**

Map every `immediate-shape` symbol to a parser constructed from `binary/values.ss` and
`binary/types.ss`. Memory arguments must produce `wasm-memory-argument`; f32/f64 immediates must
produce `wasm-float`; vectors and catch tables must be preallocated vectors.

```scheme
(define <wasm-memory-argument>
  (<bind> <wasm-u32>
          (lambda (flags)
            (cond [(< flags 64)
                   (<map> (lambda (offset)
                            (make-wasm-memory-argument flags offset 0))
                          <wasm-u64>)]
                  [(< flags 128)
                   (<map> (lambda (field*)
                            (make-wasm-memory-argument
                             (- flags 64) (cadr field*) (car field*)))
                          (<~> <wasm-u32> <wasm-u64>))]
                  [else
                   (<fail-with> "invalid WebAssembly memory alignment flags")]))))

(define immediate-parser
  (lambda (shape)
    (case shape
      [(none) (<result> '#())]
      [(function-index type-index table-index memory-index global-index local-index tag-index
        field-index data-index element-index label-index)
       (<map> vector <wasm-u32>)]
      [(memory-argument)
       (<map> vector <wasm-memory-argument>)]
      [(i32) (<map> (lambda (value) (vector value)) <wasm-s32>)]
      [(i64) (<map> (lambda (value) (vector value)) <wasm-s64>)]
      [(f32) (<map> (lambda (value) (vector value)) <wasm-f32>)]
      [(f64) (<map> (lambda (value) (vector value)) <wasm-f64>)]
      [else (<fail-with> (format "unsupported immediate shape ~a" shape))])))
```

Use `<bind> <u8>` followed by optional `<bind> <wasm-u32>` for prefixes, descriptor lookup, and the
selected immediate parser. Do not inspect `binary-input-data` directly.

- [ ] **Step 4: Implement recursive structured instructions**

Declare and install lazy parsers for instructions and instruction sequences. Stop a nested sequence
only on its legal terminators, without consuming the terminator in `<many>`. Build `block`, `loop`,
`if`, and Core 3.0 exception-control records with nested vectors and catch clauses. Parse a module
expression as an instruction vector followed by exactly one `0x0b` end byte.

- [ ] **Step 5: Add every non-vector immediate schema and unknown-opcode errors**

Use the descriptor table to exercise every scalar/control/reference/aggregate immediate schema.
Add a commented negative test for each reserved opcode, unknown `0xfb`/`0xfc` sub-opcode, invalid
catch kind, reserved call-indirect byte where required by Core 3.0, and missing structured terminator.

- [ ] **Step 6: Run focused tests and commit**

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: exit 0 with empty stdout/stderr.

```bash
git add chezpp/parser/wasm/binary/instructions.ss tests/parser-wasm.ss
git commit -m "wasm: decode Core 3.0 scalar and control instructions"
```

### Task 5: Decode Core 3.0 Aggregate And Vector Instructions

**Files:**
- Modify: `chezpp/parser/wasm/binary/instructions.ss`
- Modify: `tests/parser-wasm.ss`

- [ ] **Step 1: Add failing aggregate and vector instruction tests**

Cover `0xfb` aggregate/reference instructions and all `0xfd` vector immediate shapes. Include exact
assertions for type/field indexes, fixed array counts, heap types, 16-byte vectors, shuffle masks,
lane indexes, memory arguments, and memory-lane combinations.

```scheme
(let ([instruction
       (run-binary-parser <wasm-instruction>
                          (bytevector #xfd #x0c 0 1 2 3 4 5 6 7
                                      8 9 10 11 12 13 14 15))])
  (and (eq? 'v128.const (wasm-instruction-mnemonic instruction))
       (equal? (bytevector 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
               (vector-ref (wasm-instruction-immediates instruction) 0))))

;; error: i8x16.shuffle lanes must be in the range 0 through 31.
(error? (run-binary-parser <wasm-instruction>
                           (bytevector #xfd #x0d 32 1 2 3 4 5 6 7
                                       8 9 10 11 12 13 14 15)))
```

- [ ] **Step 2: Run the tests and verify missing immediate shapes fail**

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: aggregate/vector cases fail with `unsupported immediate shape`.

- [ ] **Step 3: Implement every remaining immediate shape**

Extend `immediate-parser` with struct/array operands, cast pairs, fixed counts, vector constants,
shuffle masks, lane indexes, and memory-lane operands. Validate lane and shuffle ranges immediately
after their combinators parse the byte values.

- [ ] **Step 4: Exhaustively exercise the opcode descriptor vector**

For every descriptor, provide a minimal legal immediate bytevector in the test's independent
`minimal-binary-immediate` dispatcher, append the descriptor encoding, parse it, and assert the
returned mnemonic. This makes every descriptor execute through the real instruction parser rather
than only testing table presence.

- [ ] **Step 5: Run tests and commit**

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: exit 0 with empty stdout/stderr.

```bash
git add chezpp/parser/wasm/binary/instructions.ss tests/parser-wasm.ss
git commit -m "wasm: decode Core 3.0 aggregate and vector instructions"
```

### Task 6: Parse Every Binary Module Section

**Files:**
- Create: `chezpp/parser/wasm/validate.ss`
- Create: `chezpp/parser/wasm/binary.ss`
- Modify: `chezpp/parser/wasm.ss:1-298`
- Modify: `tests/parser-wasm.ss`
- Modify: `tests/parser.ss:108-120`

- [ ] **Step 1: Add failing typed-section and facade tests**

Use small bytevector builders in the test to encode sections without hand-counting length bytes.
Create modules containing each standard section and assert exact records for recursive types,
imports, functions/locals/bodies, tables, memories, globals, tags, exports, start, elements, data,
data-count, and custom-section placement. Add a facade test for a minimal bytevector.

```scheme
(let ([module (parse-wasm-binary-module
               (bytevector #x00 #x61 #x73 #x6d 1 0 0 0))])
  (and (wasm-module? module)
       (zero? (vector-length (wasm-module-types module)))
       (zero? (vector-length (wasm-module-functions module)))))
```

- [ ] **Step 2: Run tests and verify the facade still rejects bytevectors**

Run: `cd tests && make test-some TEST='parser parser-wasm'`

Expected: the new test fails because the legacy API requires a file path and returns lists.

- [ ] **Step 3: Implement structural issues and section-order helpers**

In `validate.ss`, define a private checked issue record carrying offset and message. Define the Core
3.0 standard section order as type, import, function, table, memory, tag, global, export, start,
element, data-count, code, data. Custom sections have no rank and may occur anywhere. Add helpers
for duplicate standard IDs, function/code count agreement, and data-count agreement.

- [ ] **Step 4: Implement each bounded standard-section parser**

In `binary.ss`, parse `(id, payload-size)` then dispatch to `<bounded>` with the section parser.
Each section parser must consume its vector count and every payload byte. Parse all eight element
segment encodings and all three data segment encodings, normalizing legacy function-index element
forms into `ref.func` initializer expressions.

```scheme
(define section-parser
  (lambda (id size)
    (<bounded>
     size
     (case id
       [(0) <custom-section>]
       [(1) (<wasm-vector> <wasm-recursive-type>)]
       [(2) (<wasm-vector> <wasm-import>)]
       [(3) (<wasm-vector> <wasm-type-index>)]
       [(4) (<wasm-vector> <wasm-table>)]
       [(5) (<wasm-vector> <wasm-memory>)]
       [(6) (<wasm-vector> <wasm-global>)]
       [(7) (<wasm-vector> <wasm-export>)]
       [(8) <wasm-start>]
       [(9) (<wasm-vector> <wasm-element>)]
       [(10) (<wasm-vector> <wasm-code>)]
       [(11) (<wasm-vector> <wasm-data>)]
       [(12) <wasm-data-count>]
       [(13) (<wasm-vector> <wasm-tag>)]
       [else (<fail-with> "unknown standard WebAssembly section")]))))
```

- [ ] **Step 5: Assemble section results into a canonical module**

Join function type indexes with code bodies after checking equal counts. Preserve imports separately
and definitions in their corresponding vectors. Retain each custom section's name, remaining raw
bytes, and preceding standard-section symbol. Require magic, version 1, all sections, and `<eof>`.

- [ ] **Step 6: Replace the legacy facade**

Export documented `parse-wasm-binary-module` and `parse-wasm-binary-module-file`. Preserve source
compatibility by allowing `parse-wasm-binary-module` to accept either a bytevector or a regular-file
path; route strings through the explicit file procedure. Both procedures use `pcheck` and
`run-binary-parser`/`parse-binary-file`.

Update `tests/parser.ss` to assert `wasm-module?` and use a relative or dynamically created file
without checking for the old list result. Remove all debug `printf`/`println` calls with the legacy
parser.

- [ ] **Step 7: Add positioned malformed-section tests**

Cover bad magic/version, unknown and duplicate standard sections, all out-of-order pairs, section
under/over-consumption, invalid UTF-8, mismatched function/code counts, mismatched data counts,
invalid element/data flags, invalid tag attributes, truncated bodies, and trailing bytes.

- [ ] **Step 8: Run tests and commit the complete binary parser**

Run: `cd tests && make test-some TEST='parser parser-wasm'`

Expected: exit 0; all generated stdout/stderr files are empty.

```bash
git add chezpp/parser/wasm/validate.ss chezpp/parser/wasm/binary.ss \
  chezpp/parser/wasm.ss tests/parser-wasm.ss tests/parser.ss
git commit -m "wasm: parse Core 3.0 binary modules"
```

### Task 7: Parse WAT Lexical Forms And Numeric Values

**Files:**
- Create: `chezpp/parser/wasm/text/lexical.ss`
- Modify: `tests/parser-wasm.ss`

- [ ] **Step 1: Add failing lexical tests**

Test specified whitespace, line comments, multiply nested block comments, identifiers, keywords,
quoted byte strings, UTF-8 names, simple and hexadecimal escapes, Unicode scalar escapes, decimal
and hexadecimal integers, decimal and hexadecimal floats, infinities, canonical `nan` literals,
and explicit `nan:0x...` payloads. Assert float bit patterns with `wasm-float-bits`.

```scheme
(mat wasm-text-lexical

     (equal? "$type-0" (run-textual-parser <wat-identifier> "$type-0"))

     (equal? (bytevector #x41 #x0a #xff)
             (run-textual-parser <wat-string> "\"A\\n\\ff\""))

     (= #x7fc00000
        (wasm-float-bits
         (run-textual-parser <wat-f32> "nan")))

     ;; error: block comments must close at their original nesting depth.
     (error? (run-textual-parser (<fully> <wat-trivia>) "(; outer (; inner ;)"))

     )
```

- [ ] **Step 2: Run tests and verify lexical parsers are missing**

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: compilation fails with unbound WAT lexical parser identifiers.

- [ ] **Step 3: Implement combinator-only trivia and tokenization**

Use lazy parsers for nested `(; ;)` comments, ordinary combinators for `;;` comments and lexical
characters, and `<token>`-style wrappers that consume following trivia. Do not scan the input string
with manual indexes. Require keyword boundaries so `(modulex)` never parses as `(module)`.

- [ ] **Step 4: Implement strings, names, and numeric conversion**

Parse string contents into a preallocated bytevector and reject illegal control characters,
malformed hex escapes, invalid Unicode scalar values, and invalid UTF-8 when a string is consumed as
a name. Parse numeric token spelling with combinators, then use pure conversion helpers to produce
modular i32/i64 integers and exact f32/f64 bit-pattern records, including NaN payload rules.

- [ ] **Step 5: Add all lexical negative cases and run tests**

Add comments for invalid UTF-8 names, lone underscore separators, missing hexadecimal exponent,
overflowing NaN payloads, malformed Unicode escapes, illegal identifier characters, and trailing
token characters.

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: exit 0 with empty stdout/stderr.

- [ ] **Step 6: Commit the WAT lexer**

```bash
git add chezpp/parser/wasm/text/lexical.ss tests/parser-wasm.ss
git commit -m "wasm: parse Core 3.0 WAT lexical forms"
```

### Task 8: Parse WAT Types And Module Fields

**Files:**
- Create: `chezpp/parser/wasm/text/types.ss`
- Create: `chezpp/parser/wasm/text.ss`
- Modify: `tests/parser-wasm.ss`

- [ ] **Step 1: Add failing WAT type and field tests**

Parse an internal module containing named recursive/sub types, function/struct/array types,
imports, functions, tables, memories, globals, tags, exports, start, element segments, data segments,
and custom annotations. Assert retained identifiers and source offsets in private syntax records.

```scheme
(define core-fields-wat
  "(module $m
      (type $pair (struct (field $left (mut i32)) (field $right i64)))
      (type $sig (func (param i32) (result i32)))
      (import \"env\" \"f\" (func $f (type $sig)))
      (memory $mem i64 1 4)
      (global $g (mut i32) (i32.const 0))
      (tag $tag (type $sig))
      (export \"memory\" (memory $mem)))")

(let ([module (run-textual-parser parser-wat-module-syntax core-fields-wat)])
  (and (wat-module? module)
       (= 7 (vector-length (wat-module-fields module)))
       (string=? "$m" (wat-module-id module))))
```

- [ ] **Step 2: Run tests and verify WAT grammar parsers are missing**

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: compilation fails with unbound WAT syntax APIs.

- [ ] **Step 3: Define private positioned syntax records**

Use private records for module, field, index reference, type use, instruction syntax, and inline
abbreviations. Every record that can cause a normalization error stores the `<pos>` offset captured
before its first token. Keep source identifiers and numeric indexes distinct until normalization.

- [ ] **Step 4: Implement Core 3.0 WAT type combinators**

Cover value/reference/heap/storage/field/composite/sub/recursive types, shorthand reference names,
type uses, block types, limits, global/table/memory/tag types, named parameters, named fields, and
all optional IDs. Return private syntax only where unresolved IDs remain; construct canonical type
records directly where no resolution is needed.

- [ ] **Step 5: Implement module-field combinators and one-module framing**

Parse every explicit field plus inline import/export clauses and all table/memory/element/data
abbreviations. The outer parser must accept one `(module ...)`, consume trailing trivia, and require
`<eof>`. Accept module IDs but omit them from the canonical result after normalization. Parse custom
annotations into pending custom-section syntax with their placement anchor.

- [ ] **Step 6: Add grammar failures and run tests**

Cover `(modulex)`, missing delimiters, duplicate singleton clauses inside one field, wrong field
keyword, malformed limits, illegal inline import placement, unterminated annotations, and trailing
WAST commands.

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: exit 0 with empty stdout/stderr.

- [ ] **Step 7: Commit WAT types and fields**

```bash
git add chezpp/parser/wasm/text/types.ss chezpp/parser/wasm/text.ss tests/parser-wasm.ss
git commit -m "wasm: parse Core 3.0 WAT types and module fields"
```

### Task 9: Parse Flat And Folded WAT Instructions

**Files:**
- Create: `chezpp/parser/wasm/text/instructions.ss`
- Modify: `chezpp/parser/wasm/text.ss`
- Modify: `tests/parser-wasm.ss`

- [ ] **Step 1: Add failing instruction syntax tests**

For every descriptor mnemonic, provide an independent minimal text immediate and assert that both
flat and folded forms parse to syntax nodes with the same mnemonic and immediate shape. Add detailed
trees for labels, `if` with `(then)`/`(else)`, blocks, loops, try tables, aggregate operations,
memory attributes, SIMD lanes/shuffles, and multi-memory indexes.

```scheme
(let ([flat (run-textual-parser <wat-expression>
                                "i32.const 1 i32.const 2 i32.add")]
      [folded (run-textual-parser <wat-expression>
                                  "(i32.add (i32.const 1) (i32.const 2))")])
  (and (= 3 (vector-length flat))
       (= 3 (vector-length folded))
       (eq? 'i32.add
            (wat-instruction-mnemonic (vector-ref folded 2)))))
```

- [ ] **Step 2: Run tests and verify instruction syntax is missing**

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: compilation fails with unbound `<wat-expression>`.

- [ ] **Step 3: Implement descriptor-driven flat instruction parsing**

Parse a mnemonic token, find its descriptor, and choose a text immediate parser corresponding to
the same closed immediate-shape vocabulary used by binary parsing. Preserve symbolic indexes in
private index-reference records. Parse `offset=`, `align=`, lane, shuffle, type, table, memory, data,
and element immediates exactly as prescribed by Core 3.0.

- [ ] **Step 4: Implement labels and structured instruction bodies**

Use lazy parser installation for recursive block/loop/if/try-table bodies. Preserve opening and
optional closing labels long enough to reject mismatches at the closing label offset. Convert
`then`, `else`, and catch clauses into private structured syntax records.

- [ ] **Step 5: Implement folded instruction expansion order**

Folded operands must be emitted before their containing instruction, while structured folded forms
retain their nested bodies. Use a list builder and one final `list->vector`; do not repeatedly append
vectors.

- [ ] **Step 6: Add exhaustive mnemonic and error coverage**

Exercise every descriptor through the actual text instruction parser. Add commented errors for
unknown mnemonics, missing immediates, bad labels, illegal lane/shuffle values, duplicate memory
attributes, malformed folded operands, and reserved text forms.

- [ ] **Step 7: Run tests and commit**

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: exit 0 with empty stdout/stderr.

```bash
git add chezpp/parser/wasm/text/instructions.ss chezpp/parser/wasm/text.ss \
  tests/parser-wasm.ss
git commit -m "wasm: parse Core 3.0 WAT instructions"
```

### Task 10: Normalize WAT Into The Canonical Module

**Files:**
- Create: `chezpp/parser/wasm/normalize.ss`
- Modify: `chezpp/parser/wasm/validate.ss`
- Modify: `chezpp/parser/wasm/text.ss`
- Modify: `chezpp/parser/wasm.ss`
- Modify: `tests/parser-wasm.ss`

- [ ] **Step 1: Add failing normalization and public API tests**

Construct WAT using forward references in every index namespace, implicit type definitions, named
locals/labels/fields, inline imports/exports, table shorthand, element/data shorthand, and folded
instructions. Assert exact canonical indexes, generated types, module-vector order, and expanded
instruction sequences.

```scheme
(let* ([module
        (parse-wasm-text-module
         "(module
            (func $id (export \"id\") (param $x i32) (result i32)
              local.get $x)
            (start $id))")]
       [function (vector-ref (wasm-module-functions module) 0)]
       [export (vector-ref (wasm-module-exports module) 0)])
  (and (= 0 (wasm-function-type-index function))
       (eq? 'local.get
            (wasm-instruction-mnemonic
             (vector-ref (wasm-function-body function) 0)))
       (= 0 (vector-ref
             (wasm-instruction-immediates
              (vector-ref (wasm-function-body function) 0))
             0))
       (eq? 'function (wasm-export-kind export))
       (= 0 (wasm-export-index export))))
```

- [ ] **Step 2: Run tests and verify the public text API is missing**

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: compilation fails with unbound `parse-wasm-text-module`.

- [ ] **Step 3: Build all Core index namespaces in declaration order**

Use separate eq hashtables for types, functions, tables, memories, globals, tags, elements, data,
locals, labels, and fields. Insert imported definitions before ordinary definitions in each external
namespace. Reject duplicate textual IDs in the same namespace while allowing absent IDs.

- [ ] **Step 4: Resolve references and expand abbreviations**

Resolve numeric indexes unchanged and symbolic indexes through the correct namespace. Expand inline
exports, inline imports, omitted type uses, named locals, folded instruction operands, legacy
element function lists, table initialization, and inline data. When an inline function signature
has no matching explicit type, append one deterministic final recursive function type and reuse it
for later identical signatures.

- [ ] **Step 5: Report normalization failures through parser errors**

Return `wasm-issue` values containing the source offset and message. In `text.ss`, run normalization
inside `<bind>`; on failure, return `(<pos-at> offset (<fail-with> message))` so public APIs retain
the source line/column. Cover unresolved names, duplicate IDs, mismatched closing labels, conflicting
type-use signatures, and impossible abbreviation combinations.

- [ ] **Step 6: Implement documented public text APIs**

Add `parse-wasm-text-module` for a source string and `parse-wasm-text-module-file` for a regular-file
path. Use `pcheck`, `run-textual-parser`, and `parse-textual-file`. Re-export all four binary/text
entry points through `(chezpp parser)` and `(chezpp)` via the existing facade imports.

- [ ] **Step 7: Run normalization tests and commit**

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: exit 0 with empty stdout/stderr.

```bash
git add chezpp/parser/wasm/normalize.ss chezpp/parser/wasm/validate.ss \
  chezpp/parser/wasm/text.ss chezpp/parser/wasm.ss tests/parser-wasm.ss
git commit -m "wasm: normalize Core 3.0 WAT modules"
```

### Task 11: Add Relative Fixtures And Cross-Syntax Integration Coverage

**Files:**
- Create: `tests/data/wasm-core3.wat`
- Create: `tests/data/wasm-core3.wasm`
- Add: `tests/data/example.wasm`
- Add: `tests/data/fibonacci.wasm`
- Modify: `tests/parser-wasm.ss`

- [ ] **Step 1: Add a focused Core 3.0 WAT fixture**

Create one valid module that combines recursive struct/array/function types, typed references, a tag,
memory64, multiple memories, tail calls, aggregate instructions, SIMD, imports/exports, globals,
elements, data, folded expressions, nested comments, and a custom annotation. Keep functions small
and independently valid so WABT can compile the fixture.

- [ ] **Step 2: Generate and inspect its binary counterpart**

Run:

```bash
wat2wasm --enable-exceptions --enable-function-references --enable-tail-call --enable-gc \
  --enable-memory64 --enable-multi-memory --enable-annotations \
  tests/data/wasm-core3.wat -o tests/data/wasm-core3.wasm
wasm-validate --enable-all tests/data/wasm-core3.wasm
wasm-objdump -x tests/data/wasm-core3.wasm
```

Expected: generation and validation exit 0; the dump lists the intended types, tags, memories,
exports, elements, code, data, and custom sections.

- [ ] **Step 3: Bring selected existing fixtures into the worktree**

From the worktree root, copy the two existing inputs and then refer to them only by paths relative to
`tests/`:

```bash
cp ../../tests/data/example.wasm tests/data/example.wasm
cp ../../tests/data/fibonacci.wasm tests/data/fibonacci.wasm
```

Do not copy `hello.wasm`, `rav1e.wasm`, or `rg.wasm`; their 1.8-49 MB sizes add repository weight
without distinct Core 3.0 grammar coverage.

- [ ] **Step 4: Add exact fixture assertions**

For `data/example.wasm`, assert one `(i32, i32) -> i32` type, one function of type 0, export `add`
at function index 0, and body mnemonics `local.get`, `local.get`, `i32.add`. For
`data/fibonacci.wasm`, assert 11 types, 57 defined functions, one table with limits 18..18, one
memory with minimum 17, three globals, four named exports, one 17-entry active element segment, one
916-byte active data segment, and retained custom section names including `name`.

For `wasm-core3.wat` and `.wasm`, compare every canonical module vector through accessors, including
float bit patterns, instruction trees, segment modes, and custom-section names. Ignore only binary
custom placement when the WAT annotation has no equivalent placement spelling.

- [ ] **Step 5: Run fixture tests and check output files**

Run: `cd tests && make test-some TEST='parser-wasm'`

Expected: exit 0; `parser-wasm.stdout` and `parser-wasm.stderr` both have size zero.

- [ ] **Step 6: Commit fixtures and integration coverage**

```bash
git add tests/data/example.wasm tests/data/fibonacci.wasm tests/data/wasm-core3.wat \
  tests/data/wasm-core3.wasm tests/parser-wasm.ss
git commit -m "wasm: add Core 3.0 parser fixtures and integration tests"
```

### Task 12: Harden Errors, Contracts, And Resource Behavior

**Files:**
- Modify: `chezpp/parser/wasm/types.ss`
- Modify: `chezpp/parser/wasm/binary/values.ss`
- Modify: `chezpp/parser/wasm/binary/types.ss`
- Modify: `chezpp/parser/wasm/binary/instructions.ss`
- Modify: `chezpp/parser/wasm/binary.ss`
- Modify: `chezpp/parser/wasm/text/lexical.ss`
- Modify: `chezpp/parser/wasm/text/types.ss`
- Modify: `chezpp/parser/wasm/text/instructions.ss`
- Modify: `chezpp/parser/wasm/text.ss`
- Modify: `chezpp/parser/wasm/normalize.ss`
- Modify: `chezpp/parser/wasm/validate.ss`
- Modify: `chezpp/parser/wasm.ss`
- Modify: `tests/parser-wasm.ss`

- [ ] **Step 1: Audit public documentation and type checks**

Ensure every exported maker, predicate, accessor, helper procedure, and parser entry point has a
`#|proc:name|#` block immediately above it, meaningful parameter names, lines no longer than 100
characters, and `pcheck`. Document signatures for exported higher-order parser factories, such as
`(Parser -> Parser)` for vector parser arguments. Group the large facade export list by module,
type, entity, instruction, and parser API.

- [ ] **Step 2: Add public contract tests**

For every public constructor and parser entry point, add at least one wrong-type call. Verify binary
input accepts bytevectors and compatible paths, the explicit file APIs reject missing/non-regular
paths, and text input/file APIs do not confuse source text with a path.

- [ ] **Step 3: Add positioned parser-error assertions**

Capture representative binary and text failures and assert `parser-error-source`, offset,
line/column, expected/found where available, and `parser-error->string`. Include failures inside a
bounded section, nested folded instruction, nested comment, and post-parse identifier resolution.

- [ ] **Step 4: Add allocation and port regressions**

Verify vector-producing parsers allocate once via list builders or known counts. Confirm all file
entry points delegate to `parse-binary-file`/`parse-textual-file`, which close their ports. Remove
all parser debug output and any direct bytevector readers that bypass combinators.

- [ ] **Step 5: Run focused and aggregate parser tests**

Run:

```bash
cd tests
make test-some TEST='parser parser-wasm'
make test-some TEST='parser-csv parser-json5 parser-xml parser-toml parser-elf parser-wasm'
```

Expected: both commands exit 0; every generated `.stdout` and `.stderr` file is empty.

- [ ] **Step 6: Commit hardening changes**

```bash
git add chezpp/parser/wasm.ss chezpp/parser/wasm tests/parser-wasm.ss
git commit -m "wasm: harden parser errors and public contracts"
```

### Task 13: Build And Final Verification

**Files:**
- Modify only files required to fix failures caused by Tasks 1-12.

- [ ] **Step 1: Check every changed Scheme file for balanced parentheses**

Run:

```bash
python3 ../../check_parentheses.py \
  chezpp/parser/wasm.ss chezpp/parser/wasm/types.ss chezpp/parser/wasm/opcodes.ss \
  chezpp/parser/wasm/binary/values.ss chezpp/parser/wasm/binary/types.ss \
  chezpp/parser/wasm/binary/instructions.ss chezpp/parser/wasm/binary.ss \
  chezpp/parser/wasm/text/lexical.ss chezpp/parser/wasm/text/types.ss \
  chezpp/parser/wasm/text/instructions.ss chezpp/parser/wasm/text.ss \
  chezpp/parser/wasm/normalize.ss chezpp/parser/wasm/validate.ss \
  tests/parser-wasm.ss tests/parser.ss
```

Expected: every file reaches EOF without a read error.

- [ ] **Step 2: Run the mandatory clean build**

Run: `make clean && make`

Expected: exit 0 with `chezpp.lib` and dependent libraries rebuilt successfully.

- [ ] **Step 3: Run focused and complete parser tests**

Run:

```bash
cd tests
make test-some TEST='parser parser-wasm'
make test-some TEST='parser-csv parser-json5 parser-xml parser-toml parser-elf parser-wasm'
```

Expected: exit 0; all test `.stdout` and `.stderr` files are empty.

- [ ] **Step 4: Validate committed binary fixtures independently**

Run:

```bash
wasm-validate --enable-all tests/data/wasm-core3.wasm
wasm-validate tests/data/example.wasm
wasm-validate tests/data/fibonacci.wasm
```

Expected: all three commands exit 0 without diagnostics.

- [ ] **Step 5: Inspect the final diff and commit history**

Run:

```bash
git diff --check HEAD~12..HEAD
git status --short
git log --oneline -13
```

Expected: no whitespace errors; only the pre-existing untracked files remain; the twelve WASM
implementation commits follow the subsystem commit-message format.

- [ ] **Step 6: Commit only if verification required a correction**

If a verification-only correction was necessary, commit it separately:

```bash
git add chezpp/parser/wasm.ss chezpp/parser/wasm tests/parser-wasm.ss tests/parser.ss \
  tests/Makefile tests/data/example.wasm tests/data/fibonacci.wasm \
  tests/data/wasm-core3.wat tests/data/wasm-core3.wasm
git commit -m "wasm: fix final Core 3.0 parser verification issues"
```

Do not create an empty verification commit.
