# ADT Keyword Options Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development
> (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add keyword-based predicate and mutability options to `record` and `datatype` fields.

**Architecture:** Extend the field grammars in `chezpp/adt.ss` and normalize both keyword and
legacy forms into the existing internal field representation before generating record types,
constructor checks, getters, or setters. The keyword form uses `:predicate pred` and at most one
of `:mutable` or `:immutable`; omission of both mutability options means immutable.

**Tech Stack:** ChezScheme `syntax-case` macros, `mat` tests, `make -C tests test-some`, and the
project build via `make clean && make`.

**Spec:** User-approved syntax in the task conversation: `[field :predicate pred :mutable]`,
`:mutable`/`:immutable` conflict, and no mutability option defaults to immutable.

## Global Constraints

- Keep old positional field forms working as compatibility syntax.
- Accept keyword options in either order after the field name.
- Reject duplicate `:predicate`, duplicate mutability options, `:mutable` plus `:immutable`,
  missing option values, and unknown options at macro expansion time.
- Keep predicate validation consistent with existing identifier-based predicate forms.
- Public macro documentation must be directly above each implementation and each line must be
  at most 100 characters.
- Add a comment before every negative test describing the invalid syntax being tested.
- Separate individual `mat` test cases with newlines and preserve balanced Scheme parentheses.
- Run `make clean && make` from the repository root of the implementation worktree.

---

### Task 1: Add keyword options to `record`

**Files:**
- Modify: `chezpp/adt.ss`
- Test: `tests/record.ss`

**Interfaces:**
- Produces these accepted new record field forms:
  - `[name :predicate string?]` for an immutable field with a constructor predicate.
  - `[age :mutable :predicate natural?]` for a mutable field with a predicate.
  - `[sex :immutable :predicate sex?]` for an explicitly immutable field with a predicate.
  - `[note :mutable]` for a mutable field without a predicate.
  - `id` and `[id :predicate symbol?]` are immutable by default.
- Preserves legacy forms `name`, `(name pred)`, `(mutable name pred)`, and
  `(immutable name pred)`.
- Constructor validation and generated setter validation continue to use the selected predicate.

- [x] **Step 1: Add failing `record` tests for keyword field forms.** Extend
  `mat record-with-predicate` and `mat record-with-mutability` with a record using
  `[name :predicate string?]`, `[age :predicate natural? :mutable]`,
  `[sex :immutable :predicate sex?]`, and `[tag :mutable]`. Assert valid construction, rejection
  of invalid constructor values, successful mutation of `age` and `tag`, and that `name` and
  `sex` have no generated setter. Add a separate no-option field and verify its constructor and
  lack of setter to establish the immutable default.

- [x] **Step 2: Run the record tests and verify the new syntax fails before implementation.**

Run: `make -C tests test-some TEST='record'`

Expected: FAIL during macro expansion because keyword field forms are not yet recognized.

- [x] **Step 3: Normalize record field options before generating record metadata.** In the
  `record` transformer, add a parser for keyword tails following a field identifier. Normalize
  each field to the existing internal shape `(mutability field getter [raw-setter])` and retain
  its optional predicate for constructor and setter generation. Parse options in either order;
  initialize mutability to immutable and predicate to absent. Keep legacy patterns as a distinct
  compatibility path rather than reinterpreting their positional predicate as an option.

- [x] **Step 4: Validate malformed record options during expansion.** Reject a repeated
  `:predicate`, repeated `:mutable` or `:immutable`, a combination of `:mutable` and
  `:immutable`, a `:predicate` without a following identifier, and every unrecognized keyword.
  Use `syntax-error` on the offending field form. Add one negative `mat` case per error with a
  preceding comment naming the invalid option combination.

- [x] **Step 5: Run record tests and check the implementation diff.**

Run: `make -C tests test-some TEST='record'`

Expected: PASS with no diagnostics; existing legacy record cases and new keyword cases both pass.

### Task 2: Add keyword options to `datatype` variant fields

**Files:**
- Modify: `chezpp/adt.ss`
- Test: `tests/datatype.ss`

**Interfaces:**
- Produces the same keyword field grammar and immutable default for fields inside datatype
  variants, for example `[Var (var :predicate symbol? :mutable)]`.
- Preserves legacy variant fields `field`, `(field pred)`, `(mutable field pred)`, and
  `(immutable field pred)`.
- Generated constructor checks, variant getters, mutable setters, and setter predicate checks
  retain their existing names and behavior.

- [x] **Step 1: Add failing datatype tests for keyword field forms.** Extend the datatype
  mutability test with keyword-form mutable and immutable fields, a field with no mutability
  option, and a mutable field without a predicate. Verify constructor and setter predicate
  failures, successful updates, and absence of setters on default/explicit immutable fields.

- [x] **Step 2: Run datatype tests and verify the new syntax fails before implementation.**

Run: `make -C tests test-some TEST='datatype'`

Expected: FAIL during expansion of a keyword-form variant field.

- [x] **Step 3: Normalize datatype variant fields using the same option rules as `record`.**
  Extend `handle-vfields` to translate keyword fields into its established `immutable`/`mutable`
  internal shapes and predicate representation. Ensure `gen-protocols`,
  `gen-setter-wrappers`, `get-getters`, and `get-setters` consume normalized fields without
  special-casing source syntax. Keep legacy field forms unchanged.

- [x] **Step 4: Validate malformed datatype field options.** Apply the same duplicate, conflict,
  missing-value, and unknown-option checks as for `record`. Add negative tests with a comment
  for each invalid form, then test both the default immutable behavior and explicit
  `:immutable` behavior.

- [x] **Step 5: Document both public macros next to their implementations.** Add `#|macro:record`
  and `#|macro:datatype` Markdown blocks above their definitions. Describe the accepted field
  option grammar, legacy compatibility, immutable default, and generated constructor/accessor/
  setter behavior. Keep every documentation line at or below 100 characters.

- [x] **Step 6: Run the focused ADT tests and verify parentheses.**

Run: `make -C tests test-some TEST='record datatype'`

Expected: PASS with empty stdout/stderr except output explicitly permitted by the test runner;
all Scheme files modified by the task have balanced parentheses.

### Task 3: Verify the full project build

**Files:**
- No additional files; validate the changes from Tasks 1 and 2.

- [x] **Step 1: Build from the implementation worktree root.**

Run: `make clean && make`

Expected: successful clean build with no Scheme syntax or macro-expansion errors.

- [x] **Step 2: Re-run focused tests after the clean build.**

Run: `make -C tests test-some TEST='record datatype'`

Expected: all ADT tests pass, including legacy compatibility, keyword parsing, predicates,
mutability defaults, and negative syntax diagnostics.

- [x] **Step 3: Review the final diff.** Confirm only `chezpp/adt.ss`, `tests/record.ss`,
  `tests/datatype.ss`, and this plan contain task changes; verify no generated build artifacts
  are included and all new docs meet the 100-character line limit.
