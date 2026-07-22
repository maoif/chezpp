# WebAssembly Core 3.0 Parser Design

## Goal

Finish the WebAssembly binary parser and add a parser for exactly one textual WAT module. Both
parsers target WebAssembly Core 3.0 and return the same canonical, typed module representation.
Parsing covers the full Core 3.0 syntax but does not execute modules or perform full semantic type
validation.

## Public Model

The public API exposes checked immutable records for a canonical WebAssembly module and its
contents. A module contains vectors of recursive types, functions, tables, memories, globals,
tags, element segments, data segments, imports, exports, an optional start index, and custom
sections.

Supporting records represent recursive groups, subtypes, function/struct/array types, fields,
storage types, heap and reference types, table/memory/global/tag types, limits, module entities,
imports and exports, segments, and instructions. Instructions carry a canonical mnemonic and typed
immediates. Structured instructions additionally retain their nested bodies, alternatives, or
catch clauses. Compound operands such as memory arguments, block types, and catch clauses have
their own records.

WAT identifiers and abbreviations are not part of the returned model. Text normalization resolves
identifiers to indexes and expands inline imports and exports, type uses, folded instructions, and
abbreviated segments. Binary standard sections are lowered into the same module contents. Custom
sections retain their name, raw payload, and placement because their payload formats are outside
the Core specification.

## Library Structure

- `chezpp/parser/wasm.ss` provides the documented, type-checked public API.
- `chezpp/parser/wasm/types.ss` defines documented checked public records.
- `chezpp/parser/wasm/opcodes.ss` defines the shared Core 3.0 instruction descriptors.
- `chezpp/parser/wasm/binary.ss` parses binary modules and standard sections.
- `chezpp/parser/wasm/binary/instructions.ss` parses binary opcodes and immediates.
- `chezpp/parser/wasm/text/lexical.ss` parses WAT lexical forms.
- `chezpp/parser/wasm/text.ss` parses WAT types, fields, and instructions.
- `chezpp/parser/wasm/normalize.ss` resolves WAT names and expands abbreviations.
- `chezpp/parser/wasm/validate.ss` performs shared decoding-time structural checks.

## Binary Parsing

All binary recognition is built from Chezpp parser combinators. The module parser consumes the
magic and version followed by length-delimited sections. Each payload is parsed through
`<bounded>`, preserving absolute error offsets and rejecting truncated or under-consumed payloads.

Local signed and unsigned LEB128 combinators enforce the widths and unused-bit rules required by
Core 3.0. Standard sections produce typed module contents directly. The parser checks standard
section ordering and uniqueness, function/code count agreement, data-count consistency, reserved
encodings, UTF-8 names, and complete input consumption.

The instruction parser consumes an opcode or prefixed sub-opcode, dispatches through the shared
descriptor table, and applies combinators for the descriptor's immediate shape. Recursive
combinators decode structured control bodies. Unknown opcodes, malformed immediates, invalid flags,
reserved bytes, alignments, and lane indexes fail with positioned parser errors.

## Text Parsing

The WAT parser is combinator-based from characters upward. Lexical combinators cover the specified
whitespace characters, line comments, nested block comments, annotations, identifiers, strings and
escapes, and Core 3.0 integer and floating-point literals. Grammar combinators parse types, module
fields, flat instructions, folded instructions, and exactly one outer `(module ...)` form.

Parsing first produces an internal syntax representation that retains identifiers and abbreviated
forms. A post-parse normalization pass builds namespace tables, resolves symbolic references,
expands abbreviations, and creates the canonical public records. The normalization pass traverses
parsed values but never scans raw source text. WAST commands such as `assert_trap`, `invoke`, and
`register` are outside this parser and are rejected as trailing input.

## Validation Boundary

The parsers enforce constraints required to decode an unambiguous representation: binary lengths
and section layout, valid encodings, WAT grammar and namespace resolution, and consistency rules
embedded in binary or text decoding.

The parsers do not implement the Core validation algorithm. Operand-stack typing, instruction
result typing, constant-expression validity, general index validity, export uniqueness, and start
function signatures remain the responsibility of a future validator.

## Errors

Failures use the existing `parser-error` condition and retain source paths plus exact binary byte
offsets or textual line and column positions. Public entry points use `pcheck`. Each negative test
has a comment naming the malformed or unsupported case.

## Testing

`tests/parser-wasm.ss` verifies public contracts and exact parsed contents. Binary coverage includes
LEB128 boundaries, floating-point bit patterns, UTF-8 names, limits, every standard section, every
Core 3.0 opcode mapping, every immediate shape, and structured instructions. Text coverage includes
all lexical families, module fields, index forms, abbreviations, flat and folded instructions, and
normalization errors.

Integration tests compare binary and WAT versions of the same module after canonicalization. They
also inspect relative fixtures such as `data/example.wasm`, `data/fibonacci.wasm`, and a focused
Core 3.0 WAT/binary pair. WABT may generate and inspect committed fixtures during development, but
the normal test suite does not depend on WABT.

Final verification runs `make clean && make` in the worktree, focused parser tests from `tests/`,
the complete parser suite, and a parenthesis check for every changed Scheme source. Successful test
runs produce no stdout or stderr.
