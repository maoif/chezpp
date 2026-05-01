# CLI Stable Core Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Complete `(chezpp cli)` as a stable library-first command-line parser with procedural and macro APIs, deterministic parse semantics, help output, option relations, and passing tests.

**Architecture:** Keep the implementation in `chezpp/cli.ss` because the library already exists and is imported by `chezpp.ss`. Add small internal helpers for normalization, command validation, parse state, value parsing, help formatting, and macro expansion, but keep the public surface compatible with the existing exports plus the missing stable-core exports called out below. Use tests in `tests/cli.ss` as executable behavior documentation and keep successful tests silent.

**Tech Stack:** ChezScheme libraries, Chezpp helpers from `(chezpp utils)`, `(chezpp list)`, `(chezpp string)`, `(chezpp vector)`, and the existing `mat` test harness.

---

## Stable-Core Decisions

- Boolean options are pure flags. `-v` and `--verbose` set the option value to `#t`. `-v=x` and `--verbose=x` are errors unless the option is explicitly configured with a nonzero `value-number` and non-bool parser.
- Named option values are accepted only through `=`. `-o=file` and `--output=file` are accepted. `-o file` and `--output file` are rejected.
- `run-cli-command!` is library-first. Parse errors signal Scheme errors. Built-in help options print to `current-output-port`, return `(void)`, and never call command exec procedures.
- Out of scope: env vars, config files, response files, shell completion, edit-distance suggestions, style/color palettes, prefix options like `-lfoo`, consume-after options, and short option grouping.

## File Map

- Modify: `chezpp/cli.ss`
  - Public record constructors, accessors, setters, and macros.
  - Internal validation, parse state, parse loop, relation checking, and help rendering.
  - Public documentation blocks above exported APIs.
- Modify: `tests/cli.ss`
  - Replace WIP/debug tests with stable-core tests for procedural API, parsers, relations, help, subcommands, and macros.
  - Capture help output with string ports so the test run remains silent.
- No change expected: `chezpp.ss`
  - `(chezpp cli)` is already imported/exported.
- Optional cleanup only if needed: `chezpp/cmdline.ss`
  - Leave empty or delete only with explicit approval. It is unrelated to stable core.

## Public API Target

Keep existing exported names and add the missing names needed by the stable-core design:

```scheme
cli-option-group
option-name
make-option-group option-group? option-group-add! option-group-options
```

Do not add a separate `parse-result` public record. Exec callbacks continue to receive a lookup procedure of type `(symbol -> value)`, as specified in `docs/ai/notes_cli_command.md`.

---

### Task 1: Replace CLI Tests With Stable-Core Behavior Tests

**Files:**
- Modify: `tests/cli.ss`

- [ ] **Step 1: Write test helpers at the top of `tests/cli.ss`**

Add helpers after `(import (chezpp))`:

```scheme
(define capture-output
  (lambda (thunk)
    (let ([v #f])
      (let ([out (call-with-string-output-port
                   (lambda (port)
                     (parameterize ([current-output-port port])
                       (set! v (thunk)))))])
        (cons v out)))))

(define output-contains?
  (lambda (str needle)
    (if (string-search str needle) #t #f)))

(define silent
  (lambda (thunk)
    (let ([v #f])
      (let ([out (call-with-string-output-port
                   (lambda (port)
                     (parameterize ([current-output-port port])
                       (set! v (thunk)))))])
        (and (string=? "" out) v)))))
```

- [ ] **Step 2: Replace the current WIP procedural tests with focused parser tests**

Use `silent` around normal parse cases. Cover:

```scheme
(mat cli-procedural-p
  (begin
    (define cmd (make-command 'prog))
    (define opt-verbose (make-option 'verbose))
    (option-short-set! opt-verbose "v")
    (option-value-number-set! opt-verbose 0)
    (define opt-out (make-option 'out))
    (option-short-set! opt-out "o")
    (option-default-set! opt-out "a.out")
    (option-value-parser-set! opt-out parser-string)
    (define opt-input (make-option 'input))
    (option-positional?-set! opt-input #t)
    (command-options-add! cmd opt-verbose opt-out opt-input)
    (command-exec-set! cmd
      (lambda (ref)
        (list (ref 'verbose) (ref 'out) (ref 'input))))
    #t)

  (equal? (silent (lambda () (run-cli-command! cmd '("main.ss"))))
          '(#f "a.out" "main.ss"))
  (equal? (silent (lambda () (run-cli-command! cmd '("-v" "--out=prog" "main.ss"))))
          '(#t "prog" "main.ss"))
  (error? (run-cli-command! cmd '("--out" "prog" "main.ss")))
  (error? (run-cli-command! cmd '("--verbose=true" "main.ss")))
  (error? (run-cli-command! cmd '())))
```

- [ ] **Step 3: Add value-number and sink positional tests**

Cover repeated list values, comma splitting, defaults, required options, `--`, and sink behavior:

```scheme
(mat cli-values-p
  (begin
    (define cmd (make-command 'prog))
    (define opt-include (make-option 'include))
    (option-short-set! opt-include "I")
    (option-value-parser-set! opt-include parser-string)
    (option-number-set! opt-include '*)
    (option-value-number-set! opt-include '+)
    (define opt-mode (make-option 'mode))
    (option-value-parser-set! opt-mode parser-string)
    (option-number-set! opt-mode '?)
    (option-default-set! opt-mode "debug")
    (define opt-file (make-option 'file))
    (option-positional?-set! opt-file #t)
    (define opt-args (make-option 'args))
    (option-positional?-set! opt-args #t)
    (option-sink?-set! opt-args #t)
    (command-options-add! cmd opt-include opt-mode opt-file opt-args)
    (command-exec-set! cmd
      (lambda (ref)
        (list (ref 'include) (ref 'mode) (ref 'file) (ref 'args))))
    #t)

  (equal? (silent (lambda () (run-cli-command! cmd '("-I=a,b" "--include=c" "main.ss" "--" "-x" "y"))))
          '(("a" "b" "c") "debug" "main.ss" ("-x" "y")))
  (equal? (silent (lambda () (run-cli-command! cmd '("--mode=release" "main.ss" "x"))))
          '(#f "release" "main.ss" ("x")))
  (error? (run-cli-command! cmd '("--mode" "release" "main.ss")))
  (error? (run-cli-command! cmd '("-I=" "main.ss"))))
```

- [ ] **Step 4: Add built-in parser tests**

Cover natural, integer, fixnum, flonum, string, bool flag, and enum:

```scheme
(mat cli-parsers-p
  (begin
    (define color-parser (parser-enum '[red :value 1] '[green :value 2] '[blue :value 3]))
    (define cmd (make-command 'prog))
    (define opt-n (make-option 'n))
    (option-value-parser-set! opt-n parser-natural)
    (define opt-i (make-option 'i))
    (option-value-parser-set! opt-i parser-integer)
    (define opt-fx (make-option 'fx))
    (option-value-parser-set! opt-fx parser-fixnum)
    (define opt-fl (make-option 'fl))
    (option-value-parser-set! opt-fl parser-flonum)
    (define opt-color (make-option 'color))
    (option-value-parser-set! opt-color color-parser)
    (command-options-add! cmd opt-n opt-i opt-fx opt-fl opt-color)
    (command-exec-set! cmd
      (lambda (ref)
        (list (ref 'n) (ref 'i) (ref 'fx) (ref 'fl) (ref 'color))))
    #t)

  (equal? (silent (lambda () (run-cli-command! cmd '("--n=7" "--i=-2" "--fx=3" "--fl=1.5" "--color=green"))))
          '(7 -2 3 1.5 2))
  (error? (run-cli-command! cmd '("--n=-1" "--i=-2" "--fx=3" "--fl=1.5" "--color=green")))
  (error? (run-cli-command! cmd '("--n=7" "--i=x" "--fx=3" "--fl=1.5" "--color=green")))
  (error? (run-cli-command! cmd '("--n=7" "--i=-2" "--fx=3.2" "--fl=1.5" "--color=green")))
  (error? (run-cli-command! cmd '("--n=7" "--i=-2" "--fx=3" "--fl=x" "--color=green")))
  (error? (run-cli-command! cmd '("--n=7" "--i=-2" "--fx=3" "--fl=1.5" "--color=yellow"))))
```

- [ ] **Step 5: Add option relation tests**

Cover `requires`, `conflicts`, and `overrides`:

```scheme
(mat cli-relations-p
  (begin
    (define cmd (make-command 'prog))
    (define opt-a (make-option 'a))
    (define opt-b (make-option 'b))
    (define opt-c (make-option 'c))
    (option-value-number-set! opt-a 0)
    (option-value-number-set! opt-b 0)
    (option-value-number-set! opt-c 0)
    (option-requires-set! opt-a '(b))
    (option-conflicts-set! opt-b '(c))
    (option-overrides-set! opt-c '(a b))
    (command-options-add! cmd opt-a opt-b opt-c)
    (command-exec-set! cmd (lambda (ref) (list (ref 'a) (ref 'b) (ref 'c))))
    #t)

  (equal? (silent (lambda () (run-cli-command! cmd '("-a" "-b"))))
          '(#t #t #f))
  (equal? (silent (lambda () (run-cli-command! cmd '("-a" "-b" "-c"))))
          '(#f #f #t))
  (error? (run-cli-command! cmd '("-a")))
  (error? (run-cli-command! cmd '("-b" "-c"))))
```

This test fixes a precise rule for overlap: an option listed in `overrides` removes the overridden option from the parse state before conflict checks. Because `c` overrides both `a` and `b`, `-a -b -c` returns `(#f #f #t)`. Because `b` and `c` conflict when `b` has not been overridden, `-b -c` raises an error.

- [ ] **Step 6: Add subcommand exec path tests**

Cover ancestor option visibility and exec order:

```scheme
(mat cli-subcommands-p
  (begin
    (define seen '())
    (define root (make-command 'prog))
    (define opt-config (make-option 'config))
    (option-value-parser-set! opt-config parser-string)
    (option-default-set! opt-config "default.cfg")
    (command-options-add! root opt-config)
    (command-exec-set! root
      (lambda (ref)
        (set! seen (cons (list 'root (ref 'config)) seen))
        'root-result))

    (define build (make-command 'build))
    (define opt-release (make-option 'release))
    (option-value-number-set! opt-release 0)
    (command-options-add! build opt-release)
    (command-exec-set! build
      (lambda (ref)
        (set! seen (cons (list 'build (ref 'config) (ref 'release)) seen))
        (reverse seen)))

    (command-subcommands-add! root build)
    #t)

  (equal? (silent (lambda () (run-cli-command! root '("--config=app.cfg" "build" "--release"))))
          '((root "app.cfg") (build "app.cfg" #t)))
  (error? (run-cli-command! root '("unknown")))
  (error? (run-cli-command! root '("--release" "build"))))
```

- [ ] **Step 7: Add help tests with captured output**

Cover built-in help, help-all hidden options, help-commands recursion, and no exec:

```scheme
(mat cli-help-p
  (begin
    (define ran? #f)
    (define root (make-command 'prog))
    (command-overview-set! root "Program overview")
    (command-version-set! root "1.0.0")
    (command-author-set! root "Maoif")
    (define opt-hidden (make-option 'secret))
    (option-hidden?-set! opt-hidden #t)
    (define sub (make-command 'run))
    (command-help-set! sub "Run the program")
    (command-options-add! root opt-hidden)
    (command-subcommands-add! root sub)
    (command-exec-set! root (lambda (ref) (set! ran? #t)))
    #t)

  (let ([ans (capture-output (lambda () (run-cli-command! root '("--help"))))])
    (and (not ran?)
         (output-contains? (cdr ans) "Usage: prog")
         (output-contains? (cdr ans) "COMMANDS")
         (not (output-contains? (cdr ans) "--secret"))))
  (let ([ans (capture-output (lambda () (run-cli-command! root '("--help-all"))))])
    (output-contains? (cdr ans) "--secret"))
  (let ([ans (capture-output (lambda () (run-cli-command! root '("--help-commands"))))])
    (and (output-contains? (cdr ans) "prog")
         (output-contains? (cdr ans) "run"))))
```

- [ ] **Step 8: Add macro API smoke tests**

Cover `cli-option`, `cli-option-group`, `cli-enum`, and nested `cli-command`:

```scheme
(mat cli-macro-p
  (begin
    (define color-parser
      (cli-enum [red :value 1] [green :value 2]))
    (define common-options
      (cli-option-group
       [verbose :short #\v]
       [color :value-parser color-parser :default 1]))
    (define cmd
      (cli-command prog
        :overview "Macro program"
        :options common-options
        :subcommands
        [run
         :exec (lambda (ref) (list (ref 'verbose) (ref 'color)))
         :options
         [input :positional]]))
    #t)

  (equal? (silent (lambda () (run-cli-command! cmd '("-v" "--color=green" "run" "file"))))
          '(#t 2)))
```

- [ ] **Step 9: Run the CLI test and verify it fails before implementation**

Run:

```bash
cd tests && make test-some TEST='cli'
```

Expected: FAIL while `chezpp/cli.ss` still has unfinished stubs and old behavior.

---

### Task 2: Normalize Records, Defaults, Setters, and Public Documentation

**Files:**
- Modify: `chezpp/cli.ss`

- [ ] **Step 1: Update the export list**

Add these exports near the related names:

```scheme
          cli-option-group
          option-name

          make-option-group option-group? option-group-add! option-group-options
```

- [ ] **Step 2: Add public documentation blocks**

Add `#|proc:...|#` or `#|macro:...|#` immediately above each exported public implementation. The first batch must cover:

```scheme
#|proc:make-command
The `make-command` procedure creates a command object named by `name`.
The `name` argument must be a symbol whose string form does not start with `-`.
|#

#|proc:make-option
The `make-option` procedure creates an option object named by `name`.
The option initially has an inferred short name, an inferred long name, default
boolean flag parsing, and no explicit default value.
|#

#|proc:run-cli-command!
The `run-cli-command!` procedure parses command-line arguments for `cmd`.
Parse errors raise Scheme errors. Built-in help options print help and return
without running command exec procedures.
|#
```

Repeat the same concise style for setters, accessors that are manually defined, macros, parsers, and option-group procedures. Record-generated accessors can be documented at the record constructor section if moving all accessors is impractical, but all hand-written exported definitions need direct docs.

- [ ] **Step 3: Fix `make-command`**

Replace the zero-argument protocol branch with an error:

```scheme
[() (errorf 'make-command "expected command name")]
```

Keep command names stored as strings because existing code compares command names with argv strings.

- [ ] **Step 4: Fix `make-option` defaults**

Use these defaults:

```scheme
name             ; symbol
""               ; help
(string first-char) ; short
name-str         ; long
""               ; category
#f               ; default means no explicit default
'*               ; number
0                ; value-number for pure bool flag
#f               ; value-name
parser-bool      ; value-parser
#\,              ; value-seperator
#f               ; callback
'()              ; alias
'()              ; conflicts
'()              ; overrides
'()              ; requires
#f #f #f #f #f #f
```

Use numeric `0`, not symbol `'0`, because `*value-numbers*` contains numeric `0`.

- [ ] **Step 5: Implement parser-aware defaulting in `option-value-parser-set!`**

When a caller changes from `parser-bool` to another parser and `value-number` is still `0`, set it to `1`:

```scheme
(define option-value-parser-set!
  (lambda (opt v)
    (pcheck ([option? opt] [procedure? v])
      ($option-value-parser-set! opt v)
      (when (and (eq? (option-value-number opt) 0)
                 (not (eq? v parser-bool)))
        ($option-value-number-set! opt 1)))))
```

This makes `(option-value-parser-set! opt parser-string)` accept `--opt=value` without requiring every caller to set `value-number`.

- [ ] **Step 6: Implement `option-name-set!`**

Use the new symbol name and refresh inferred short/long only when the current forms still match the old inferred forms:

```scheme
(define option-name-set!
  (lambda (opt v)
    (pcheck ([option? opt] [symbol? v])
      (let* ([old-name (option-name opt)]
             [old-str (symbol->string old-name)]
             [new-str (symbol->string v)]
             [old-short (and (fx> (string-length old-str) 0)
                             (string (string-ref old-str 0)))])
        (when (string-startswith? new-str #\-)
          (errorf 'option-name-set! "option name cannot start with -: ~a" v))
        ($option-name-set! opt v)
        (when (equal? (option-short opt) old-short)
          ($option-short-set! opt (string (string-ref new-str 0))))
        (when (equal? (option-long opt) old-str)
          ($option-long-set! opt new-str))))))
```

- [ ] **Step 7: Tighten short and long setters**

Rules:

- `#f` disables that form.
- `#t` is invalid.
- A char argument is accepted by macro expansion but converted before calling the setter.
- Short strings must have length 1 and must not be `"-"` or `"="`.
- Long strings must not be empty, must not start with `"-"`, and must not contain `"="`.

Implementation sketch:

```scheme
(define-who option-short-set!
  (lambda (opt v)
    (pcheck ([option? opt] [(p/or string? boolean?) v])
      (when (eq? v #t)
        (errorf who "short option form must be a string or #f"))
      (when (string? v)
        (unless (fx= (string-length v) 1)
          (errorf who "option's short form must have length 1: ~s" v))
        (when (or (string=? v "-") (string=? v "="))
          (errorf who "invalid short option form: ~s" v)))
      ($option-short-set! opt v))))
```

Use the same shape for `option-long-set!`.

- [ ] **Step 8: Implement relation setters**

Accept symbols, options, or lists containing symbols/options. Store relation names as symbols:

```scheme
(define normalize-option-refs
  (lambda (who v)
    (let ([xs (if (list? v) v (list v))])
      (map (lambda (x)
             (cond [(symbol? x) x]
                   [(option? x) (option-name x)]
                   [else (errorf who "expected option, symbol, or list: ~s" v)]))
           xs))))
```

Then:

```scheme
(define option-conflicts-set!
  (lambda (opt v)
    (pcheck ([option? opt])
      ($option-conflicts-set! opt (normalize-option-refs 'option-conflicts-set! v)))))
```

Repeat for alias, overrides, and requires. For alias, store symbols in `option-alias`.

- [ ] **Step 9: Implement option-group procedures**

Use a simple list-builder backed record:

```scheme
(define-record-type option-group
  (fields (mutable options option-group-options $option-group-options-set!))
  (protocol (lambda (n)
              (case-lambda
                [() (n (make-list-builder))]
                [opts
                 (let ([g (n (make-list-builder))])
                   (apply option-group-add! g opts)
                   g)]))))

(define-who make-option-group
  (lambda opts
    (let ([g ((record-constructor (record-type-descriptor option-group)))])
      (apply option-group-add! g opts)
      g)))

(define-who option-group-add!
  (lambda (grp . opts)
    (pcheck ([option-group? grp])
      (unless (andmap option? opts)
        (errorf who "expected option objects: ~s" opts))
      (for-each (option-group-options grp) opts))))
```

If the exact record-constructor form is awkward, use the existing `(record option-group (options))` macro pattern and a separate constructor wrapper. Keep the public behavior exactly as above.

- [ ] **Step 10: Validate record/setter changes**

Run:

```bash
make clean && make
cd tests && make test-some TEST='cli'
```

Expected: build succeeds; CLI tests still fail because parsing/help/macros are not complete.

---

### Task 3: Implement Built-In Value Parsers

**Files:**
- Modify: `chezpp/cli.ss`
- Modify: `tests/cli.ss`

- [ ] **Step 1: Implement `parser-bool`**

For pure flags, it only accepts missing value:

```scheme
(define-who parser-bool
  (lambda (cmd opt x)
    (if x
        (errorf who "option --~a does not take a boolean value; use it as a flag"
                (or (option-long opt) (symbol->string (option-name opt))))
        #t)))
```

- [ ] **Step 2: Implement numeric parsers**

```scheme
(define-who parser-natural
  (lambda (cmd opt x)
    (let ([v (string->number x)])
      (if (and (integer? v) (not (negative? v)))
          v
          (errorf who "failed to parse ~s as natural" x)))))

(define-who parser-integer
  (lambda (cmd opt x)
    (let ([v (string->number x)])
      (if (integer? v)
          v
          (errorf who "failed to parse ~s as integer" x)))))

(define-who parser-fixnum
  (lambda (cmd opt x)
    (let ([v (string->number x)])
      (if (fixnum? v)
          v
          (errorf who "failed to parse ~s as fixnum" x)))))

(define-who parser-flonum
  (lambda (cmd opt x)
    (let ([v (string->number x)])
      (if (real? v)
          (exact->inexact v)
          (errorf who "failed to parse ~s as flonum" x)))))
```

- [ ] **Step 3: Keep `parser-string` as identity but validate input**

```scheme
(define-who parser-string
  (lambda (cmd opt x)
    (if (string? x)
        x
        (errorf who "expected string value for option ~a" (option-name opt)))))
```

- [ ] **Step 4: Implement procedural `parser-enum`**

Accept specs like `'(red green [blue :value 3])` and `'( [red :value 1 :help "Red"] ... )`.

```scheme
(define parser-enum
  (lambda enum-specs
    (let ([table (make-hashtable string-hash string=?)])
      (for-each
       (lambda (spec)
         (let-values ([(name value)
                       (cond
                        [(symbol? spec) (values spec spec)]
                        [(and (list? spec) (pair? spec))
                         (let lp ([xs (cdr spec)] [value (car spec)])
                           (cond [(null? xs) (values (car spec) value)]
                                 [(eq? (car xs) ':value) (lp (cddr xs) (cadr xs))]
                                 [(eq? (car xs) ':help) (lp (cddr xs) value)]
                                 [else (errorf 'parser-enum "unknown enum config: ~s" xs)]))]
                        [else (errorf 'parser-enum "invalid enum spec: ~s" spec)])])
           (hashtable-set! table (symbol->string name) value)))
       enum-specs)
      (lambda (cmd opt x)
        (hashtable-ref table x
          (lambda ()
            (errorf 'parser-enum "invalid enum value ~s for option ~a" x (option-name opt))))))))
```

- [ ] **Step 5: Add parser tests if Task 1 did not already add them**

Use the `cli-parsers-p` test from Task 1.

- [ ] **Step 6: Validate parser task**

Run:

```bash
make clean && make
cd tests && make test-some TEST='cli'
```

Expected: parser-specific failures disappear; parse-loop/help/macro failures may remain.

---

### Task 4: Add Internal Validation and Parse State Helpers

**Files:**
- Modify: `chezpp/cli.ss`

- [ ] **Step 1: Add an internal parsed-option record**

Place in an internals section before `run-cli-command!`:

```scheme
(define-record-type parsed-option
  (fields option command mutable?))
```

If `mutable?` is not needed, omit it. The parse state can use two tables:

- `values`: option symbol -> parsed value
- `seen`: option symbol -> option object

- [ ] **Step 2: Add helper predicates for occurrence specs**

```scheme
(define option-required?
  (lambda (opt)
    (memq (option-number opt) '(1 +))))

(define option-repeatable?
  (lambda (opt)
    (memq (option-number opt) '(* +))))

(define option-singleton?
  (lambda (opt)
    (memq (option-number opt) '(1 ?))))
```

- [ ] **Step 3: Add `option-default-value`**

Return default values without mutating option objects:

```scheme
(define option-default-value
  (lambda (opt)
    (cond [(option-default opt) (option-default opt)]
          [(eq? (option-value-parser opt) parser-bool) #f]
          [(or (eq? (option-value-number opt) '+)
               (eq? (option-value-number opt) '*)
               (option-sink? opt))
           #f]
          [else #f])))
```

This preserves current tests where missing list-valued optional options read as `#f`, not `'()`.

- [ ] **Step 4: Add command-local option table builder**

Function contract:

```scheme
;; Returns short table, long table, name table, and positional option list.
(define build-command-option-tables
  (lambda (cmd)
    ...))
```

Validation rules:

- No duplicate option names in one command.
- No duplicate short forms in one command.
- No duplicate long forms in one command.
- Aliases also occupy names and long/short forms only if the alias names are explicitly supported as lookup aliases. For stable core, aliases are lookup aliases only; they do not create command-line spellings.
- `no-short?` disables short form even when `option-short` is set.
- `no-long?` disables long form even when `option-long` is set.
- Positional options must not have short or long forms.
- A command cannot have positionals and subcommands at the same level.
- Only the last positional option may be a sink.
- At most one sink positional per command.

- [ ] **Step 5: Add command-path validation**

Before parsing, validate the full command tree for structural issues that do not depend on argv:

```scheme
(define validate-command-tree!
  (lambda (cmd)
    ...))
```

Rules:

- No command has itself as a descendant.
- Sibling subcommands have distinct `command-name`.
- `multi-personality?` is allowed only on the top command.
- `flatten-help?`, `trace?`, and `quit?` are accepted fields but only `trace?` affects stable core if implemented.

- [ ] **Step 6: Add selected path name uniqueness check**

After the parse path is known and before exec:

```scheme
(define validate-command-path-option-names!
  (lambda (cmds)
    ...))
```

Rule: option names and aliases must be unique across the selected command path, so the exec lookup procedure is unambiguous.

- [ ] **Step 7: Validate helper task**

Run:

```bash
make clean && make
cd tests && make test-some TEST='cli'
```

Expected: build succeeds; tests still fail until the parse loop uses these helpers.

---

### Task 5: Rework `run-cli-command!` Parse Loop

**Files:**
- Modify: `chezpp/cli.ss`

- [ ] **Step 1: Remove unconditional debug output**

Remove or guard all unconditional `println` calls inside `run-cli-command!`, `print-help-and-quit`, and `print-usage`. Normal successful parsing must produce no stdout/stderr.

If trace support is kept for stable core, gate it:

```scheme
(define trace-enabled? (command-trace? top-command))
(define trace-print
  (lambda args
    (when trace-enabled?
      (apply println args))))
```

- [ ] **Step 2: Replace `print-help-and-quit` parse-error use with `errorf`**

For parse errors:

```scheme
(errorf 'run-cli-command! "unknown option: ~a" arg)
```

Do not print help automatically and do not call `exit`.

- [ ] **Step 3: Implement value splitting**

Function:

```scheme
(define parse-option-value
  (lambda (cmd opt raw-value)
    ...))
```

Rules:

- `value-number 0`: raw value must be `#f`; result is `(parser cmd opt #f)`, normally `#t`.
- `value-number 1`: raw value must be a nonempty string; result is parsed single value.
- `value-number '?`: raw value may be `#f` or a string. If `#f`, use default. If string, parse one value.
- `value-number '+`: raw value must be nonempty, split by separator, no empty elements, return list.
- `value-number '*`: raw value may be `#f` or empty. If missing/empty, return `'()`. Otherwise split and parse list.

- [ ] **Step 4: Implement state update semantics**

Function:

```scheme
(define parse-state-set!
  (lambda (state cmd opt value)
    ...))
```

Rules:

- `number 1` and `?`: error on second occurrence.
- `number *`: repeated scalar option uses last value, unless `value-number` is `+` or `*`, in which case append lists.
- `number +`: must appear at least once; repeated scalar option uses last value, repeated list option appends.
- Run callback after a value is successfully stored:

```scheme
(when (option-callback opt)
  ((option-callback opt) cmd opt value))
```

- [ ] **Step 5: Implement named option parsing**

Parsing rules:

- `--` switches remaining argv to positional-only mode.
- `--name=value` looks in the current command long table.
- `--name` is accepted only for value-number `0` flag options.
- `-x=value` looks in the current command short table.
- `-x` is accepted only for value-number `0` flag options.
- `-xyz` is not short grouping in stable core. It is a single short name and therefore normally errors because short names must be one char.
- Named options belong only to the current command level.

- [ ] **Step 6: Implement positional parsing**

Rules:

- Positionals are consumed only in leaf commands.
- Fixed positionals consume one argv item unless their `value-number` is `+` or `*`, in which case the one argv item is split by separator.
- Sink positional must be last and collects every remaining argv item as a list of parsed string values. For sink positionals, do not split each argv item by separator unless the option explicitly has `value-number +` or `*`.
- `--` is not included in positional values.
- Unknown extra args without a sink raise an error.

- [ ] **Step 7: Implement subcommand transition**

Rules:

- At a non-leaf command, non-option argv is matched against subcommands.
- Before descending, finalize current command options: fill defaults, check required options, run relation checks for current command.
- If no subcommand is provided:
  - If `command-maybe-no-subcommand?` is true, finalize and run exec for that command.
  - If the command has `exec`, stable core allows exec without a subcommand.
  - Otherwise raise an error listing available subcommands.

- [ ] **Step 8: Implement finalization**

Function:

```scheme
(define finalize-command!
  (lambda (state cmd)
    ...))
```

Rules:

- Fill missing optional options with `option-default-value`.
- Error on missing required named options (`number 1` or `+`).
- Error on missing required positionals.
- Apply overrides before conflicts/requires:
  - If option `x` is present and `(option-overrides x)` contains `y`, remove `y` from state.
  - If `x` overrides a required option `y`, `y` is not required after override.
- Check conflicts after overrides.
- Check requires after overrides.

- [ ] **Step 9: Implement exec order and return value**

Maintain `cmds-seen` and `exec-results`.

Rules:

- Each command on the selected path with `command-exec` runs in path order after its command is finalized.
- The lookup procedure can read values from finalized ancestor commands plus the current command.
- The return value of `run-cli-command!` is the last exec result.
- If no exec ran and parsing succeeds, return `(void)`.

- [ ] **Step 10: Implement built-in help early exit**

At each command level, before normal option lookup:

- `-h` and `--help`: print current command help excluding hidden options and return `(void)`.
- `--help-all`: include hidden options.
- `--help-commands`: recursively print available commands from current command.
- If the current command explicitly defines an option using the same spelling, parse it as a normal option instead of built-in help.

- [ ] **Step 11: Validate parse loop task**

Run:

```bash
make clean && make
cd tests && make test-some TEST='cli'
```

Expected: procedural parse tests, parser tests, relation tests, subcommand tests, and help tests pass. Macro tests may still fail.

---

### Task 6: Finish Help Rendering

**Files:**
- Modify: `chezpp/cli.ss`
- Modify: `tests/cli.ss`

- [ ] **Step 1: Replace `print-help-and-quit` with parse-error-free helpers**

Keep a reusable `print-usage`, but delete the `(todo 'quit-or-what?)` path. Parse errors should not call this helper.

- [ ] **Step 2: Implement `print-help`**

Output shape:

```text
Program overview
Version: 1.0.0
Author: Maoif

Usage: prog [OPTIONS] COMMAND

OPTIONS
    -v, --verbose    Enable verbose output

COMMANDS
    run              Run the program
```

Do not assert exact spacing in tests except for stable substrings.

- [ ] **Step 3: Implement hidden filtering**

`--help` excludes `(option-hidden? opt)`. `--help-all` includes hidden options.

- [ ] **Step 4: Implement categories**

Group options and commands by category string. Empty category prints as `OPTIONS` or `COMMANDS`. Nonempty category prints the category string.

- [ ] **Step 5: Implement `--help-commands`**

Print command paths recursively:

```text
prog run
prog image ls
prog image build
```

Include each command help text when present.

- [ ] **Step 6: Validate help task**

Run:

```bash
make clean && make
cd tests && make test-some TEST='cli'
```

Expected: help tests pass and normal successful tests remain silent.

---

### Task 7: Implement Macro API

**Files:**
- Modify: `chezpp/cli.ss`
- Modify: `tests/cli.ss`

- [ ] **Step 1: Implement parser symbol resolution for macros**

Macro keyword values map as:

```scheme
bool    -> parser-bool
natural -> parser-natural
integer -> parser-integer
fixnum  -> parser-fixnum
flonum  -> parser-flonum
string  -> parser-string
```

If a value is not one of those identifiers, use it as an expression.

- [ ] **Step 2: Implement `cli-option` macro**

Accepted forms:

```scheme
(cli-option name :help "..." :short #\x :long "xxx" :default "v" ...)
[name :help "..." ...] ; only inside `cli-command` and `cli-option-group`
```

Expansion shape:

```scheme
(let ([opt (make-option 'name)])
  (option-help-set! opt "...")
  (option-short-set! opt "x")
  opt)
```

Keyword actions:

```scheme
:help               option-help-set!
:default            option-default-set!
:number             option-number-set!
:value-number       option-value-number-set!
:value-name         option-value-name-set!
:value-parser       option-value-parser-set!
:value-seperator    option-value-seperator-set!
:positional         option-positional?-set! #t
:sink               option-sink?-set! #t
:short              option-short-set! ; char converted to one-char string
:long               option-long-set!
:no-short           option-short-set! #f and option-no-short?-set! #t
:no-long            option-long-set! #f and option-no-long?-set! #t
:callback           option-callback-set!
:alias              option-alias-set!
:hidden             option-hidden?-set! #t
:category           option-category-set!
:no-grouping        option-no-grouping?-set! #t
:conflicts          option-conflicts-set!
:overrides          option-overrides-set!
:requires           option-requires-set!
```

Reject unknown keywords with a syntax error.

- [ ] **Step 3: Implement `cli-option-group` macro**

Expansion shape:

```scheme
(let ([grp (make-option-group)])
  (option-group-add! grp <expanded-option> ...)
  grp)
```

Allow both inline option specs and option/group identifiers. If an identifier evaluates to an option group inside `cli-command`, expand by adding all options from the group.

- [ ] **Step 4: Implement `cli-enum` macro**

Expansion:

```scheme
(parser-enum '[red :value 1] '[green :value 2])
```

or equivalent quoted data passed to procedural `parser-enum`.

- [ ] **Step 5: Implement `cli-command` macro**

Accepted top-level form:

```scheme
(cli-command prog
  :overview "..."
  :version "..."
  :author "..."
  :exec main
  :options
  [verbose :short #\v]
  common-options
  :subcommands
  [run
   :help "Run"
   :exec run-main
   :options
   [input :positional]])
```

Expansion shape:

```scheme
(let ([cmd (make-command 'prog)])
  (command-overview-set! cmd "...")
  (command-options-add! cmd <expanded-option> ...)
  (command-subcommands-add! cmd <expanded-command> ...)
  cmd)
```

Command keyword actions:

```scheme
:overview                 command-overview-set!
:version                  command-version-set!
:author                   command-author-set!
:copyright                command-copyright-set!
:about                    command-about-set!
:style                    command-style-set!
:category                 command-category-set!
:exec                     command-exec-set!
:help                     command-help-set!
:trace                    command-trace?-set! #t
:quit                     command-quit?-set! #t
:flatten-help             command-flatten-help?-set! #t
:multi-personality        command-multi-personality?-set! #t
:maybe-no-subcommand      command-maybe-no-subcommand?-set! #t
```

Do not implement `:exec-print-help` or `:exec-print-version` in stable core. Reject them with a syntax error that says they are out of stable-core scope.

- [ ] **Step 6: Validate macro task**

Run:

```bash
make clean && make
cd tests && make test-some TEST='cli'
```

Expected: all CLI tests pass silently.

---

### Task 8: Final Verification, Balance Check, and Cleanup

**Files:**
- Modify: `chezpp/cli.ss`
- Modify: `tests/cli.ss`

- [ ] **Step 1: Search for unfinished stubs in CLI files**

Run:

```bash
rg -n 'todo|TODO|quit-or-what|trace-define|println "' chezpp/cli.ss tests/cli.ss docs/ai/notes_cli_command.md
```

Expected:

- No `todo` or `quit-or-what` in `chezpp/cli.ss`.
- No unconditional debug `println` in normal parse paths.
- Unfinished markers may remain in `docs/ai/notes_cli_command.md` because it is a rough design note, but stable-core exclusions must be clear in this plan and in any added comments.

- [ ] **Step 2: Check Scheme parenthesis balance by compiling**

Run:

```bash
make clean && make
```

Expected: exit status 0.

- [ ] **Step 3: Run focused CLI tests**

Run:

```bash
cd tests && make test-some TEST='cli'
```

Expected: exit status 0 and no stdout/stderr except Makefile progress lines.

- [ ] **Step 4: Run full test suite if CLI tests pass**

Run:

```bash
cd tests && make test-some TEST='cli rich list string vector'
```

Expected: exit status 0.

Then run the full suite:

```bash
cd tests && make test
```

Expected: exit status 0.

- [ ] **Step 5: Commit in stable chunks**

Use commit messages like:

```bash
git add tests/cli.ss chezpp/cli.ss
git commit -m "cli: add stable core parser tests"
```

```bash
git add chezpp/cli.ss
git commit -m "cli: implement stable core parser"
```

```bash
git add chezpp/cli.ss tests/cli.ss
git commit -m "cli: add command macro API"
```

If the whole change is completed in one commit, use:

```bash
git add chezpp/cli.ss tests/cli.ss
git commit -m "cli: complete stable core command parser"
```

## Self-Review Notes

- The plan covers the approved stable core: pure boolean flags, `=`-only named values, non-exiting parse errors/help, procedural API, built-in parsers, positionals, sink args, nested subcommands, exec path, help, relations, and macro API.
- The plan explicitly excludes env/config/response/completion/style/prefix/consume-after/short-grouping features.
- The main implementation risk is macro parsing complexity. Keep the macro expansion small by expanding directly to procedural construction and rejecting unknown/out-of-scope keywords with syntax errors.
- The second implementation risk is relation semantics. Apply overrides before conflicts/requires and encode this in tests before changing code.
