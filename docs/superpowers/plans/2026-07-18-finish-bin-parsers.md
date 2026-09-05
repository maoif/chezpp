# Binary Parser Completion Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Finish ELF, Java class, and WebAssembly binary parsers and add a content-producing WAT parser.

**Architecture:** Use typed immutable records per format and parser-combinator entry points with bounded binary sub-parsers. Keep binary and textual WebAssembly parsing on one canonical record model, and run structural validation before returning a public value.

**Tech Stack:** ChezScheme bytevectors, Chezpp parser combinators, JVMS SE 26, ELF specification, WebAssembly Core specification, and the existing `mat` test harness.

---

### Task 1: Establish Parser Facades And Shared Fixture Paths

**Files:**
- Modify: `chezpp/parser.ss`
- Modify: `chezpp/parser/elf.ss`
- Modify: `chezpp/parser/jclass.ss`
- Modify: `chezpp/parser/wasm.ss`
- Test: `tests/parser.ss`

- [ ] Restore the three parser-library imports in `chezpp/parser.ss` and keep the experimental parser-combinator import behavior unchanged.
- [ ] Define documented public entry points for bytevectors and files, preserving the existing one-argument names with bytevector/path dispatch and adding explicit `parse-elf-file`, `parse-java-class-file`, `parse-wasm-binary-module-file`, and `parse-wasm-text-module-file` procedures.
- [ ] Add type checks for every public entry point and route all parsing through `run-binary-parser`, `run-textual-parser`, `parse-binary-file`, or `parse-textual-file`.
- [ ] Extend `tests/parser.ss` with a minimal bytevector/path smoke clause for each facade and an error clause for non-bytevector, non-file arguments.

### Task 2: Implement Typed ELF32/ELF64 Parsing

**Files:**
- Create: `chezpp/parser/elf/types.ss`
- Create: `chezpp/parser/elf/binary.ss`
- Create: `chezpp/parser/elf/validate.ss`
- Modify: `chezpp/parser/elf.ss`
- Test: `tests/parser-elf.ss`

- [ ] Define documented immutable records for ELF files, identification, headers, program headers, sections, symbols, relocations, dynamic entries, notes, and typed section payloads.
- [ ] Build combinator parsers for identification, class-selected integer widths, endian-selected values, ELF32/ELF64 headers, and the distinct program-header layouts.
- [ ] Parse program and section tables through `<pos-at>`, `<rep>`, and `<bounded>`; validate table offsets, entry sizes, extended counts, `PN_XNUM`, `e_shnum == 0`, and `SHN_XINDEX`.
- [ ] Resolve section names from a validated string table and decode `SHT_STRTAB`, symbol tables, `REL`/`RELA`, `RELR`, `HASH`, `GROUP`, `SYMTAB_SHNDX`, init/fini arrays, dynamic tables, and notes. Preserve raw bytes for unknown section types.
- [ ] Reject truncated ranges, invalid string offsets, invalid linked-section types, malformed note alignment, `PT_LOAD` size violations, and unsupported identification values with parser errors.
- [ ] Add synthetic ELF32/ELF64 little/big-endian fixtures, typed-section fixtures, malformed inputs, and a relative smoke test against `../libchezpp.so`.

### Task 3: Implement Java Class Structure And Constant Pool

**Files:**
- Create: `chezpp/parser/jclass/types.ss`
- Create: `chezpp/parser/jclass/reader.ss`
- Create: `chezpp/parser/jclass/binary.ss`
- Create: `chezpp/parser/jclass/validate.ss`
- Modify: `chezpp/parser/jclass.ss`
- Test: `tests/parser-jclass.ss`

- [ ] Define documented records for `jclass-file`, fields, methods, interfaces, raw attributes, and every JVMS constant-pool tag. Preserve index-zero and long/double reserved slots in the pool vector.
- [ ] Implement parser-combinator readers for big-endian integers, bounded byte arrays, modified UTF-8, class headers, constant-pool slots, interfaces, fields, methods, and class attributes.
- [ ] Decode modified UTF-8 null encodings and surrogate pairs; reject raw NUL, overlong sequences, bad continuations, four-byte encodings, and unpaired surrogates.
- [ ] Parse field and method descriptors into validated private structures and attach resolved names, descriptors, and parsed descriptors to public records.
- [ ] Validate class-file versions, constant-pool references, access flags, initializer names, descriptor slot limits, and contextual placement rules.
- [ ] Add exact record assertions for every constant-pool tag, malformed pool cases, and relative fixture tests for `data/Pair.class` and `data/Trace.class`.

### Task 4: Implement Standard Java Attributes And Bytecode

**Files:**
- Create: `chezpp/parser/jclass/attributes.ss`
- Create: `chezpp/parser/jclass/bytecode.ss`
- Modify: `chezpp/parser/jclass/types.ss`
- Modify: `chezpp/parser/jclass/binary.ss`
- Modify: `chezpp/parser/jclass/validate.ss`
- Test: `tests/parser-jclass.ss`

- [ ] Add typed records and bounded parsers for fixed and variable attributes, annotations, parameter/type annotations, module and record attributes, nest/permitted attributes, bootstrap methods, code, exception tables, and stack-map tables.
- [ ] Decode all JVM instruction operand forms, including `wide`, switch alignment, invoke reserved operands, multianewarray dimensions, and branch offsets; retain instruction offsets and sizes.
- [ ] Validate attribute lengths, duplicate/placement constraints, annotation target shapes, stack-map frame ranges, constant references, and bytecode operand kinds.
- [ ] Extend tests with positive layouts for every standard attribute family and negative cases with comments identifying the violated JVMS rule. Assert absolute error offsets where applicable.

### Task 5: Implement WebAssembly Binary Records And Sections

**Files:**
- Create: `chezpp/parser/wasm/types.ss`
- Create: `chezpp/parser/wasm/instructions.ss`
- Create: `chezpp/parser/wasm/binary.ss`
- Create: `chezpp/parser/wasm/validate.ss`
- Modify: `chezpp/parser/wasm.ss`
- Test: `tests/parser-wasm.ss`

- [ ] Define documented canonical records for modules, standard/custom sections, types, imports, functions, limits, globals, exports, elements, data segments, and instructions.
- [ ] Implement width-bounded unsigned/signed LEB128 combinators, UTF-8 names, numeric/reference/value types, limits, expressions, and instruction immediates.
- [ ] Parse every core standard section inside `<bounded>` payload parsers, preserving unknown custom-section bytes and validating standard-section ordering, uniqueness, function/code counts, and data-count consistency.
- [ ] Decode structured control instructions and all core opcode families represented by the selected WebAssembly Core release; reject unknown opcodes, invalid prefixes, reserved bytes, bad lanes, and truncated immediates.
- [ ] Add exact field assertions for `data/example.wasm` and `data/fibonacci.wasm`, plus malformed magic/version, LEB, section, count, and instruction cases.

### Task 6: Implement Textual WAT Parsing

**Files:**
- Create: `chezpp/parser/wasm/text.ss`
- Modify: `chezpp/parser/wasm/types.ss`
- Modify: `chezpp/parser/wasm.ss`
- Create or modify: `tests/data/example.wat`
- Test: `tests/parser-wasm.ss`

- [ ] Build textual combinators for whitespace, line comments, nested block comments, identifiers, strings and escapes, decimal/hexadecimal numerals, and parenthesized forms.
- [ ] Parse a single `(module ...)` into the same canonical records as binary parsing, including type, import, function, memory, global, export, start, element, and data fields plus folded instruction syntax.
- [ ] Resolve local symbolic identifiers within the module while retaining explicit numeric indices, and normalize abbreviations into canonical records.
- [ ] Reject `(modulex ...)`, unterminated comments/strings, malformed escapes, invalid field forms, unbalanced parentheses, and trailing WAST commands.
- [ ] Assert parsed names, counts, function bodies, imports, exports, and comments using `data/example.wat` and inline WAT fixtures.

### Task 7: Build And Verify

**Files:**
- Modify only files required by preceding tasks.

- [ ] Run `make clean && make` from `.worktrees/finish-bin-parsers`.
- [ ] Run `cd tests && make test-some TEST='parser-elf parser-jclass parser-wasm'`.
- [ ] Run the complete parser target to catch import/export regressions and confirm generated stdout/stderr are empty except explicitly expected errors.
- [ ] Run the Scheme parenthesis checker over every new or modified `.ss` file.
- [ ] Commit each format and integration task separately with subsystem-prefixed messages.
