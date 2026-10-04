# Globbing Library Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement the lazy Linux filesystem iterator in `(chezpp file)` and the `(chezpp glob)` brace/tilde expander, compiler, matcher, filesystem expander, and lazy glob iterator described in the glob design.

**Architecture:** Extend `(chezpp file)` with Linux `readdir` wrappers and lazy stack traversal, then make `(chezpp glob)` use those APIs for unsorted lazy matching while retaining eager list expansion. Reuse `(chezpp path)`, `(chezpp regex)`, and `(chezpp iter)`.

**Tech Stack:** ChezScheme libraries, Linux `opendir`/`readdir`/`closedir`, Chez foreign procedures, `(chezpp regex)` (`sre->regex`, `regex-matches?`), `pcheck`, path objects, `file-type`, `file-stat`, and `(chezpp iter)`.

**Spec:** `docs/superpowers/specs/2026-10-01-glob-design.md`

## Global Constraints

- Use only `lambda` and `case-lambda` procedure definitions; no keyword arguments.
- Add Markdown API documentation immediately above every exported definition.
- Apply `pcheck` to every exported procedure and macro.
- Use meaningful parameter names and close all ports/handles through existing Chez APIs.
- Keep source ASCII, balance every Scheme file, and use `make-list-builder` for collected results.
- Create feature worktrees under `.worktrees` with a date-prefixed name if isolation is used.
- Reuse existing `FT_*` symbols and add `FT_unknown` for `DT_UNKNOWN`.
- `chezpp_fs_read_directory` and the Scheme `fs-read-directory` wrapper return
  `(entry-name . entry-type)` or `#f` at EOF.
- `fs->iter` and `glob->iter` are unsorted and must not collect the complete tree.

### Task 1: Lazy filesystem stream and traversal in `(chezpp file)`

**Files:**
- Modify: `chezpp/file.ss`
- Modify: `chezpp/c/file.c`
- Test: `tests/fs.ss`

**Interfaces:**
- Produce `fs-open-directory`, `fs-read-directory`, `fs-close-directory`,
  `fs-directory?`, `fs-directory-closed?`, and `fs->iter`.
- Reuse `FT_*` symbols from `(chezpp file)` and add/export `FT_unknown`.

- [x] **Step 1: Add tests in `tests/file.ss`** for pair/EOF reads, idempotent close,
  file-type mapping including `FT_unknown`, lazy traversal, symlink policy, and
  top-down/bottom-up order.
- [x] **Step 2: Add C FFI functions** wrapping Linux `opendir`, one-entry
  `readdir`, and `closedir`; return an `uptr` directory handle and a Scheme
  pair `(entry-name . d-type)` without a public entry record allocation.
- [x] **Step 3: Add `(chezpp file)` wrappers** with `pcheck`, pair-returning
  `fs-read-directory`, and explicit unsupported-platform errors.
- [x] **Step 4: Implement `fs->iter`** with a stack of open directory records,
  lazy one-entry reads, `follow-link?`, `top-down?`, and finalization cleanup.
- [x] **Step 5: Run `cd tests && make test-some TEST=file` without failures.
- [x] **Step 6: Commit** with `fs: add lazy local directory iteration`.

### Task 2: Parser and lexical matcher

**Files:**
- Create: `chezpp/glob.ss`
- Test: `tests/glob.ss`

**Interfaces:**
- Produce `make-glob`, `glob?`, `glob-match?` and internal expansion/parsed-token records.
- Consume `path-parse`, `path-components`, `path-flavor`, `(chezpp regex)`, and `pcheck`.

- [x] **Step 1: Add matching tests** for literals, `*`, `?`, classes,
  escapes, `**`, braces, numeric ranges, leading tilde, hidden names,
  Unix/Windows separators, and malformed syntax.
- [x] **Step 2: Run `cd tests && make test-some TEST=glob`** and confirm the
  missing-library/import failure.
- [x] **Step 3: Implement brace and tilde preprocessing.** Parse nested braces,
  comma alternatives, ascending/descending numeric ranges with optional steps,
  reject empty or malformed forms, and expand only a leading current-user `~`
  through `path-expand-user`; reject `~user`.
- [x] **Step 4: Implement the `(chezpp regex)` component compiler.** Translate
  literal runs to literal S-expression strings, `*` to repetition, `?` to a
  single-character atom, and validated bracket classes to S-expression
  character sets; compile each ordinary component with `sre->regex` and match
  it with `regex-matches?`.
  Keep path splitting, `**`, and recursive matching outside the regex engine.
- [x] **Step 5: Implement component matching.** Match complete paths;
  ensure ordinary regex components receive one component at a time and `**` can
  consume zero or more components.
- [x] **Step 6: Run lexical tests** and confirm the syntax cases pass,
  including literal regex metacharacters and newline-containing names.
- [x] **Step 7: Commit** with `glob: add regex-backed lexical matching`.

### Task 3: Filesystem expansion

**Files:**
- Modify: `chezpp/glob.ss`
- Modify: `tests/glob.ss`

**Interfaces:**
- Consume compiled glob objects and the Task 1 regex-backed matcher.
- Produce `glob`, `glob*`, stable sorted filesystem results, and unmatched modes.

- [x] **Step 1: Add filesystem expansion tests** for rooted/relative patterns,
  brace alternatives/ranges, tilde roots, pruning, directory inclusion,
  ordering, literal fallback, and missing branches.
- [x] **Step 2: Implement root/prefix decomposition** preserving Unix roots,
  Windows drives, and UNC paths; start traversal at the longest non-magic prefix.
- [x] **Step 3: Implement eager traversal** using `directory-list`,
  path constructors, file predicates, and `follow-link?`; include directories
  only when requested and track visited directory identities for followed links.
- [x] **Step 4: Implement result deduplication across brace alternatives and
  `'empty`/`'literal` behavior.**
- [x] **Step 5: Run `cd tests && make test-some TEST=glob`** and verify no output
  appears on success.
- [x] **Step 6: Commit** with `glob: expand patterns against filesystem`.

### Task 4: Lazy iterator and integration

**Files:**
- Modify: `chezpp/glob.ss`
- Modify: `tests/glob.ss`
- Modify: `chezpp.ss`
- Modify: `tests/Makefile`

**Interfaces:**
- Consume the expansion traversal and produce `glob->iter` compatible with
  `iter-next!`, `iter-reset!`, and `iter-finalize!`.

- [x] **Step 1: Add failing iterator tests** for order, exhaustion, reset, and
  finalization, including a directory that is removed after iterator creation.
- [x] **Step 2: Fix `glob->iter` root selection and validate it on the test
  working directory; it must use the longest literal prefix and avoid traversing
  unrelated generated directories.
- [x] **Step 3: Add `(chezpp glob)` to `chezpp.ss`** and register `glob.ss` in
  `tests/Makefile`.
- [x] **Step 4: Run focused tests** with `cd tests && make test-some TEST='glob path file'`.
- [x] **Step 5: Run full project verification** with `make clean && make` from
  the project root, then rerun the focused tests.
- [x] **Step 6: Run `tools/check-scheme-balance.ss`** against the new source and
  inspect public API documentation checks.
- [x] **Step 7: Commit** with `glob: add lazy expansion and library integration`.
