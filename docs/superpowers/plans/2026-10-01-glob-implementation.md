# Globbing Library Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement the `(chezpp glob)` brace/tilde expander, compiler, matcher, filesystem expander, and lazy iterator described in the glob design.

**Architecture:** Add one focused `chezpp/glob.ss` library with brace/tilde preprocessing, parser, `(chezpp regex)`-backed lexical matching, and pruned filesystem expansion. Reuse `(chezpp path)`, Chez directory/stat primitives, and `(chezpp iter)`; expose it through `chezpp.ss` and validate it with a dedicated `mat` test file.

**Tech Stack:** ChezScheme libraries, `(chezpp regex)` (`sre->regex`, `regex-matches?`), `pcheck`, path objects, `path-expand-user`, `directory-list`, `file-stat`, and `(chezpp iter)`.

**Spec:** `docs/superpowers/specs/2026-10-01-glob-design.md`

## Global Constraints

- Use only `lambda` and `case-lambda` procedure definitions; no keyword arguments.
- Add Markdown API documentation immediately above every exported definition.
- Apply `pcheck` to every exported procedure and macro.
- Use meaningful parameter names and close all ports/handles through existing Chez APIs.
- Keep source ASCII, balance every Scheme file, and use `make-list-builder` for collected results.
- Create feature worktrees under `.worktrees` with a date-prefixed name if isolation is used.

### Task 1: Parser and lexical matcher

**Files:**
- Create: `chezpp/glob.ss`
- Test: `tests/glob.ss`

**Interfaces:**
- Produce `make-glob`, `glob?`, `glob-match?` and internal expansion/parsed-token records.
- Consume `path-parse`, `path-components`, `path-flavor`, `(chezpp regex)`, and `pcheck`.

- [ ] **Step 1: Write failing matching tests** for literals, `*`, `?`, classes,
  escapes, `**`, braces, numeric ranges, leading tilde, hidden names,
  Unix/Windows separators, and malformed syntax.
- [ ] **Step 2: Run `cd tests && make test-some TEST=glob`** and confirm the
  missing-library/import failure.
- [ ] **Step 3: Implement brace and tilde preprocessing.** Parse nested braces,
  comma alternatives, ascending/descending numeric ranges with optional steps,
  reject empty or malformed forms, and expand only a leading current-user `~`
  through `path-expand-user`; reject `~user`.
- [ ] **Step 4: Implement the `(chezpp regex)` component compiler.** Translate
  literal runs to literal S-expression strings, `*` to repetition, `?` to a
  single-character atom, and validated bracket classes to S-expression
  character sets; compile each ordinary component with `sre->regex` and match
  it with `regex-matches?`.
  Keep path splitting, `**`, and recursive matching outside the regex engine.
- [ ] **Step 5: Implement memoized component matching.** Match complete paths;
  ensure ordinary regex components receive one component at a time and `**` can
  consume zero or more components.
- [ ] **Step 6: Run the focused tests** and confirm all lexical cases pass,
  including literal regex metacharacters and newline-containing names.
- [ ] **Step 7: Commit** with `glob: add regex-backed lexical matching`.

### Task 2: Filesystem expansion

**Files:**
- Modify: `chezpp/glob.ss`
- Modify: `tests/glob.ss`

**Interfaces:**
- Consume compiled glob objects and the Task 1 regex-backed matcher.
- Produce `glob`, `glob*`, stable sorted filesystem results, and unmatched modes.

- [ ] **Step 1: Add failing temporary-tree tests** for rooted/relative patterns,
  brace alternatives/ranges, tilde roots, pruning, directory inclusion,
  ordering, literal fallback, and missing branches.
- [ ] **Step 2: Implement root/prefix decomposition** preserving Unix roots,
  Windows drives, and UNC paths; start traversal at the longest non-magic prefix.
- [ ] **Step 3: Implement sorted pruned traversal** using `directory-list`,
  path constructors, file predicates, and `follow-link?`; include directories
  only when requested and track visited directory identities for followed links.
- [ ] **Step 4: Implement result deduplication across brace alternatives and
  `'empty`/`'literal` behavior.**
- [ ] **Step 5: Run `cd tests && make test-some TEST=glob`** and verify no output
  appears on success.
- [ ] **Step 6: Commit** with `glob: expand patterns against filesystem`.

### Task 3: Lazy iterator and integration

**Files:**
- Modify: `chezpp/glob.ss`
- Modify: `tests/glob.ss`
- Modify: `chezpp.ss`
- Modify: `tests/Makefile`

**Interfaces:**
- Consume the expansion traversal and produce `glob->iter` compatible with
  `iter-next!`, `iter-reset!`, and `iter-finalize!`.

- [ ] **Step 1: Add failing iterator tests** for order, exhaustion, reset, and
  finalization, including a directory that is removed after iterator creation.
- [ ] **Step 2: Implement `glob->iter`** with `make-iter`, lazy directory listing,
  stable ordering, and cleanup on exhaustion/finalization.
- [ ] **Step 3: Add `(chezpp glob)` to `chezpp.ss`** and register `glob.ss` in
  `tests/Makefile`.
- [ ] **Step 4: Run focused tests** with `cd tests && make test-some TEST='glob path file'`.
- [ ] **Step 5: Run full project verification** with `make clean && make` from
  the project root, then rerun the focused tests.
- [ ] **Step 6: Run `tools/check-scheme-balance.ss`** against the new source and
  inspect public API documentation checks.
- [ ] **Step 7: Commit** with `glob: add lazy expansion and library integration`.
