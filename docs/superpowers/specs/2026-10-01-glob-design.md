# Chezpp Globbing Library Design

## Goal

Add a `(chezpp glob)` library for matching path strings against Unix-style glob
patterns and expanding those patterns against the filesystem. The library will
reuse `(chezpp path)` for lexical path parsing/rendering, `(chezpp regex)` for
compiled component matching, and `(chezpp file)` for filesystem conventions,
then be re-exported by `(chezpp)`.

## Scope and syntax

The first release supports:

- `*`: zero or more characters within one path component.
- `?`: exactly one character within one path component.
- Bracket classes: `[abc]`, ranges such as `[a-z]`, and leading `!` or `^` for
  negation. A `]` immediately after the opening bracket is a literal member.
- `**`: recursive matching across zero or more path components. It only has
  recursive meaning when it is a complete component; embedded `a**b` behaves
  like two ordinary `*` tokens in one component.
- `\\` escapes the next pattern character on Unix and Windows flavors. A
  trailing escape is a malformed pattern.
- Braces expand alternatives and comma-separated numeric ranges before glob
  matching: `{js,ts}` becomes two patterns and `{1..3}` becomes `1`, `2`, and
  `3`. Ranges may use an optional step, such as `{0..6..2}`; descending ranges
  such as `{3..1}` are supported. Braces nest, and escaped braces are literal.
- A leading `~` component expands to the current user's home directory. `~user`
  is rejected as unsupported rather than guessed from the environment. Tilde
  expansion happens after brace expansion and before path parsing; an escaped
  or non-leading tilde remains literal.

There is no extglob, regex syntax, pattern negation, or Bash option emulation in
this version. A slash (or either slash on
Windows) always separates components and is never matched by `*`, `?`, or a
bracket class. A leading dot is ordinary text; hidden entries are included when
the pattern permits them.

Patterns are parsed using an explicit path flavor. The default flavor is `'unix`;
the Windows flavor uses the separator and case rules of `(chezpp path)`. Matching
is lexical and does not normalize `.` or `..` components, resolve symlinks, or
consult the filesystem.

Each non-recursive component is translated to an S-expression regex and compiled
once with `(chezpp regex)`'s `sre->regex`. Literal runs become literal SRE
strings, `*` and `?` become regex repetition and single-character atoms, and
parsed bracket classes become regex character sets. `regex-matches?` then
performs the complete-component match. Regex is not used for path splitting,
brace parsing, tilde expansion, `**` recursion, or filesystem traversal.

## Public API

```scheme
(make-glob pattern)
```

Expand braces and a leading current-user `~`, compile the resulting Unix path
pattern, and return an opaque glob object. If braces produce multiple patterns,
the object represents their ordered union.
Malformed escapes or bracket classes raise an error. The object is immutable and
safe to reuse across calls.

```scheme
(make-glob flavor pattern)
```

Expand braces and a leading current-user `~`, then compile using `flavor`, which
must be `'unix` or `'windows`.

```scheme
(glob? x)
```

Return `#t` when `x` is a compiled glob object.

```scheme
(glob-match? glob path)
```

Return `#t` when compiled `glob` matches the complete path string `path`.
`path` must use the glob's flavor; no filesystem access occurs.

```scheme
(glob-match? flavor pattern path)
```

Convenience form that compiles `pattern` with `flavor` and matches `path`.

```scheme
(glob pattern)
(glob flavor pattern)
```

Expand braces and a leading current-user `~`, then expand the resulting patterns
against the filesystem and return matching paths as strings.
The search is rooted at the pattern's non-magic prefix (or the current
directory for a pattern with no such prefix). Results are complete paths in the
same spelling style as the input root, are duplicate-free, and are sorted by
component order using the flavor's comparison rules. An unmatched pattern
returns `()`.

```scheme
(glob* pattern follow-link? include-directories? unmatched)
(glob* flavor pattern follow-link? include-directories? unmatched)
```

Expanded form with explicit traversal and unmatched behavior. Brace alternatives
are traversed in declaration order, then duplicate paths are removed before the
final sort. `follow-link?`
controls whether symlinked directories are descended into, matching
`walk-files`. `include-directories?` controls whether matching directories are
returned; files and symlinks are always eligible. `unmatched` is either `'empty`
or `'literal`; `'literal` returns a one-element list containing the original
pattern when no path matches.

```scheme
(glob->iter pattern)
(glob->iter flavor pattern)
```

Return a `(chezpp iter)` iterator that lazily enumerates the same matches as
`glob`, in the same order. Directory handles are opened only during iteration
and are closed when exhausted or finalized. Filesystem errors other than a
missing/inaccessible candidate raise the underlying Chez condition.

## Architecture

`glob.ss` contains three layers. The parser turns a pattern into immutable
component tokens and validates syntax after a separate expansion phase. The
expander handles nested brace alternatives, numeric ranges, and leading tilde
before parsing. The matcher compiles each ordinary component to a reusable
`(chezpp regex)` object and evaluates those objects against path components;
`**` uses memoized component positions to avoid exponential backtracking. The expander walks only branches
whose next component can match, using `directory-list` and the existing path
constructors. It does not call `walk-files`, because pruning non-matching
branches is essential for glob performance; it follows the same symlink policy.

The expander preserves the input root (relative, absolute, drive-relative,
drive-absolute, or UNC) and renders results through `path-render`. Directory
entries are sorted before traversal, making list and iterator output stable.

## Errors and edge cases

- Non-string patterns and paths, invalid flavors, invalid booleans, and invalid
  unmatched modes are rejected through `pcheck` on every public procedure.
- Unbalanced braces, empty alternatives, malformed numeric ranges, and `~user`
  raise a who/message error. A lone `~` expands to the current user's home.
- Unterminated classes, empty classes, invalid ranges, trailing escapes, and any
  failure while compiling the generated S-expression regex raise a who/message
  error during compilation.
- An inaccessible directory is treated as an expansion error, not as an empty
  match; a missing branch simply contributes no result.
- A pattern naming an existing file without magic returns that file when it is
  eligible under `include-directories?`.
- Symlink loops are avoided when `follow-link?` is true by tracking visited
  directory identities from `file-stat`; the default does not follow links.

## Testing

Add `tests/glob.ss` and register it in `tests/Makefile`. Tests cover literal,
wildcard, class, escape, recursive, hidden, Unix, and Windows-flavor matching;
malformed patterns; stable ordering; unmatched modes; directory inclusion;
symlink policy; and iterator exhaustion/finalization. Tests create temporary
trees with `dynamic-wind` cleanup and use `mat` cases, with comments preceding
negative cases.

Update `chezpp.ss` to import `(chezpp glob)`, and run the project-required
`make clean && make`, followed by `cd tests && make test-some TEST='glob path file'`.
