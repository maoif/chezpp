# Benchmark Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [x]`) syntax for tracking.

**Goal:** Build `(chezpp benchmark)` as an in-process benchmark definition, execution, reporting, baseline, and comparison library.

**Current status:** Implemented. `(chezpp benchmark)` is imported by `chezpp.ss`, covered by `tests/benchmark.ss`, and demonstrated by `examples/benchmark.ss`. The original v1 core is complete, and several v2 items are also implemented: aligned text output, a customizable Rich table reporter, CSV/JSON reporters, Scheme-data baselines, comparison helpers, confidence intervals, measured pause/resume subtraction, baseline error round-tripping, and JSON safety fixes.

**Architecture:** The library lives in `chezpp/benchmark.ss` as a single public library split by internal comment sections for records, option parsing, expansion, runner, summaries, reporters, baselines, comparisons, and macros. The runner uses Chez `statistics`/`sstats-difference` and `black-box`, returns structured records, and keeps cost-center-specific instrumentation deferred. Text and Rich reporters share internal result formatting helpers, with Rich output rendered through `(chezpp rich)`.

**Tech Stack:** ChezScheme libraries, Chez records, Chez `statistics`, `black-box`, Chezpp `pcheck`, existing `mat` test harness, `make clean && make`, and `tests/make test-some`.

---

## Roadmap

### Implemented Core

- Public library `(chezpp benchmark)` imported from `chezpp.ss`.
- Opaque records and public accessors for benchmark descriptors, fixtures, suites, states, samples, results, configs, and reporters.
- Procedural registration APIs with a parameterized default registry.
- Macro APIs for `define-benchmark`, `define-benchmark-fixture`, `define-fixture-benchmark`, `define-benchmark-suite`, `define-benchmark-template`, `instantiate-benchmark-template`, and `benchmark-options`.
- Argument expansion for `:args`, `:arg-range`, `:dense-arg-range`, and `:arg-product`.
- Sample-scoped setup/teardown, fixture setup/teardown, benchmark suite execution, and cleanup on body errors.
- Warmup, measured samples, simple adaptive iteration growth to satisfy `min-time`, and capped iterations.
- Summary alist containing CPU, real, and allocated-byte per-iteration statistics: `median`, `mean`, `min`, `max`, `stddev`, and `confidence-interval`.
- Aligned default text reporter, datum reporter, CSV reporter, JSON reporter, and customizable Rich table reporter.
- `benchmark-do-not-optimize` using Chez `black-box` and weak `benchmark-clobber-memory`.
- `benchmark-pause-timing` and `benchmark-resume-timing` subtract paused `statistics` deltas from measured samples.
- Scheme-data baseline save/load with version validation, error condition preservation, and compatibility with legacy v1 `(error? . #t)` baselines.
- Result comparison helpers for percent differences, absolute thresholds, and noise thresholds.
- JSON writer safety for non-JSON numeric values and all string control characters.
- Console example under `examples/benchmark.ss` covering multiple inputs, templates, fixtures, suites, reporters, and comparisons.

### Remaining Roadmap

- Optional JSON baseline file format if a stable repo-level JSON parser/writer becomes available.
- Explicit cost-center sample fields once instrumentation expectations and tests are stable.
- Deterministic fake measurement injection for deeper pause/resume tests.

---

## File Structure

- Create `tests/benchmark.ss`: deterministic tests for descriptors, option parsing, expansion, lifecycle, runner output shape, reporters, and error capture.
- Modify `tests/Makefile`: add `benchmark.ss` to `SRCS_TEST`.
- Modify `chezpp/benchmark.ss`: replace the empty shell with the full public implementation.
- Modify `chezpp.ss`: import/export `(chezpp benchmark)` from the aggregate Chezpp library.
- Create `examples/benchmark.ss`: runnable console example for multiple inputs, templates, fixtures, suites, reporters, and comparisons.
- Keep implementation in one file for v1. Split into `(chezpp benchmark core)`, `(chezpp benchmark runner)`, `(chezpp benchmark reporter)`, and `(chezpp benchmark syntax)` only if the file becomes hard to maintain after v1.

---

### Task 1: Failing Tests And Test Harness Entry

**Files:**
- Create: `tests/benchmark.ss`
- Modify: `tests/Makefile`

- [x] **Step 1: Add benchmark to test build list**

Modify `tests/Makefile` so `SRCS_TEST` includes `benchmark.ss` near the other public library tests:

```make
SRCS_TEST := record.ss datatype.ss match.ss for.ss control.ss os.ss iter.ss transducer.ss file.ss path.ss navigator.ss \
             list.ss string.ss vector.ss array.ss dlist.ss stack.ss queue.ss heap.ss \
             hashset.ss treemap.ss treeset.ss \
             bittree.ss bitvec.ss dset.ss \
             concurrency.ss \
             cli.ss rich.ss logging.ss benchmark.ss hash.ss crypto.ss uuid.ss $(NET_TESTS)
```

- [x] **Step 2: Write failing tests**

Create `tests/benchmark.ss`:

```scheme
(import (chezpp))

(define (benchmark-test-config)
  (benchmark-config-with (default-benchmark-config)
    '((warmup . 0)
      (samples . 2)
      (min-time . 0)
      (max-iterations . 1)
      (reporter . #f)
      (output . #f))))

(benchmark-clear-registry! (current-benchmark-registry))

(define-benchmark bench-basic
  (:args [n 1] [n 2]
   :warmup 0
   :samples 2
   :min-time 0
   :max-iterations 1
   :unit nanosecond)
  (lambda (state n)
    (benchmark-state-counter-add! state 'calls 1)
    (benchmark-do-not-optimize n)))

(define-benchmark-fixture bench-fixture
  (:setup (lambda (state)
            (vector (benchmark-state-arg state 'n)))
   :teardown (lambda (state value)
               (vector-set! value 0 'closed))))

(define-fixture-benchmark bench-with-fixture bench-fixture
  (:args [n 3]
   :warmup 0
   :samples 1
   :min-time 0
   :max-iterations 1)
  (lambda (state n value)
    (benchmark-do-not-optimize (vector-ref value 0))))

(define-benchmark-template bench-template (kind make-seq ref)
  (:args [n 4]
   :warmup 0
   :samples 1
   :min-time 0
   :max-iterations 1)
  (lambda (state n)
    (let ([seq (make-seq n 9)])
      (benchmark-do-not-optimize (ref seq 0)))))

(instantiate-benchmark-template bench-template
  ([vector make-vector vector-ref]))

(define-benchmark bench-error
  (:args [n 1]
   :warmup 0
   :samples 1
   :min-time 0
   :max-iterations 1
   :reporter #f)
  (lambda (state n)
    (error 'bench-error "expected benchmark body error")))

(define-benchmark bench-product
  (:arg-product [n 1 2]
                [m 10 20]
   :warmup 0
   :samples 1
   :min-time 0
   :max-iterations 1)
  (lambda (state n m)
    (benchmark-do-not-optimize (+ n m))))

(define-benchmark-suite bench-suite
  (:suite-setup (lambda (state) 'suite-open)
   :suite-teardown (lambda (state value) (void)))
  bench-basic
  bench-with-fixture)

(mat benchmark-records
     (benchmark? (make-benchmark 'manual (lambda (state) (void)) '()))
     (benchmark-config? (default-benchmark-config))
     (benchmark-reporter? (benchmark-text-reporter))
     (benchmark-fixture? bench-fixture)
     (benchmark-suite? bench-suite)
     (let ([state (benchmark-state 'state-test #f '((n . 7)) '() 0 1 0)])
       (and (benchmark-state? state)
            (eq? (benchmark-state-name state) 'state-test)
            (= (benchmark-state-arg state 'n) 7)
            (not (benchmark-state-arg state 'missing #f))))
     ;; unknown argument names are rejected
     (error? (benchmark-state-arg (benchmark-state 'state-test #f '() '() 0 1 0) 'missing))
     ;; public constructors validate boundary argument types
     (error? (make-benchmark "bad-name" (lambda (state) (void)) '())))

(mat benchmark-registry-and-expansion
     (let ([names (map benchmark-name
                       (benchmark-registry-benchmarks (current-benchmark-registry)))])
       (and (memq 'bench-basic names)
            (memq 'bench-with-fixture names)
            (memq 'bench-template/vector names)))
     (= (length (benchmark-expand bench-basic (benchmark-test-config))) 2)
     (= (length (benchmark-expand bench-product (benchmark-test-config))) 4)
     (equal? (map benchmark-state-args
                  (map car (benchmark-expand bench-product (benchmark-test-config))))
             '(((n . 1) (m . 10))
               ((n . 1) (m . 20))
               ((n . 2) (m . 10))
               ((n . 2) (m . 20))))
     (let* ([selected (benchmark-select (current-benchmark-registry) "bench-basic")]
            [names (map benchmark-name selected)])
       (equal? names '(bench-basic))))

(mat benchmark-runner
     (let ([results (benchmark-run (list bench-basic) (benchmark-test-config))])
       (and (= (length results) 2)
            (andmap benchmark-result? results)
            (andmap (lambda (result)
                      (and (= (length (benchmark-result-samples result)) 2)
                           (not (benchmark-result-error result))
                           (assq 'cpu-ns (benchmark-result-summary result))
                           (assq 'real-ns (benchmark-result-summary result))
                           (assq 'bytes (benchmark-result-summary result))
                           (>= (cdr (assq 'calls (benchmark-result-counters result))) 1)))
                    results)))
     (let ([results (benchmark-run (list bench-with-fixture) (benchmark-test-config))])
       (and (= (length results) 1)
            (not (benchmark-result-error (car results)))))
     (let ([results (benchmark-run (list bench-error) (benchmark-test-config))])
       (and (= (length results) 1)
            (benchmark-result-error (car results)))))

(mat benchmark-reporters
     (let ([out (open-output-string)])
       (benchmark-report (benchmark-run (list bench-basic) (benchmark-test-config))
                         (benchmark-text-reporter)
                         out)
       (let ([text (get-output-string out)])
         (and (string? text)
              (> (string-length text) 0))))
     (let ([out (open-output-string)])
       (benchmark-report (benchmark-run (list bench-basic) (benchmark-test-config))
                         (benchmark-datum-reporter)
                         out)
       (let ([text (get-output-string out)])
         (and (string? text)
              (> (string-length text) 0)))))
```

- [x] **Step 3: Run tests to verify they fail**

Run:

```bash
cd tests && make test-some TEST='benchmark'
```

Historical red-step result: compilation failed because `(chezpp benchmark)` did not export the benchmark APIs yet. Current state: benchmark tests compile and run.

---

### Task 2: Core Records, Configs, Registry, And Options

**Files:**
- Modify: `chezpp/benchmark.ss`

- [x] **Step 1: Implement public exports, records, accessors, constructors, registry, and option parsing**

Replace `chezpp/benchmark.ss` with a full library exporting the v1 API. Implement records with stable public accessors and internal `$` constructors. Use public documentation comments immediately above exported procedures and macros. Use `pcheck` on public procedures.

Core record fields:

```scheme
(define-record-type ($benchmark $mk-benchmark benchmark?)
  (fields name body options args arg-names setup teardown fixture suite template-args))

(define-record-type ($benchmark-fixture $mk-benchmark-fixture benchmark-fixture?)
  (fields name setup teardown))

(define-record-type ($benchmark-suite $mk-benchmark-suite benchmark-suite?)
  (fields name benchmarks setup teardown))

(define-record-type ($benchmark-state $mk-benchmark-state benchmark-state?)
  (fields name suite args template-args iteration iterations sample
          (mutable user-data) (mutable counters) (mutable timing-paused?)))

(define-record-type ($benchmark-sample $mk-benchmark-sample benchmark-sample?)
  (fields iterations sstats cost-center counters))

(define-record-type ($benchmark-result $mk-benchmark-result benchmark-result?)
  (fields name args template-args samples summary counters error))

(define-record-type ($benchmark-config $mk-benchmark-config benchmark-config?)
  (fields warmup samples min-time max-iterations output filter reporter cost-center? stop-on-error?))

(define-record-type ($benchmark-reporter $mk-benchmark-reporter benchmark-reporter?)
  (fields start result finish))
```

Default config values:

```scheme
warmup: 1
samples: 10
min-time: 0.01
max-iterations: 1000000
output: (current-output-port)
filter: #f
reporter: (benchmark-text-reporter)
cost-center?: #f
stop-on-error?: #f
```

Option alists accept these symbols: `args`, `arg-range`, `dense-arg-range`, `arg-product`, `setup`, `teardown`, `warmup`, `samples`, `min-time`, `max-iterations`, `unit`, `throughput`, `complexity`, `reporter`, `output`, `filter`, `cost-center?`, `stop-on-error?`, `suite-setup`, `suite-teardown`, `template-args`.

- [x] **Step 2: Run tests**

Run:

```bash
cd tests && make test-some TEST='benchmark'
```

Historical red-step result: constructor and predicate errors were gone, while macro and runner coverage still failed. Current state: this task is complete.

---

### Task 3: Macros And Argument Expansion

**Files:**
- Modify: `chezpp/benchmark.ss`

- [x] **Step 1: Implement option macros**

Implement `benchmark-options` as a syntax transformer that accepts option forms beginning with keyword symbols and emits an alist. Parse these v1 forms:

```scheme
(:args [n 1] [n 2])
(:arg-range [n 1 8 2])
(:dense-arg-range [n 1 3 1])
(:arg-product [n 1 2] [m 10 20])
(:setup expr)
(:teardown expr)
(:warmup expr)
(:samples expr)
(:min-time expr)
(:max-iterations expr)
(:reporter expr)
(:output expr)
(:filter expr)
(:unit expr)
(:throughput expr)
(:complexity expr)
```

Unknown keyword symbols must raise a syntax error.

- [x] **Step 2: Implement registration macros**

Implement:

```scheme
(define-benchmark name options body)
(define-benchmark-fixture name options)
(define-fixture-benchmark name fixture options body)
(define-benchmark-suite name options benchmark ...)
(define-benchmark-template name (param ...) options body)
(instantiate-benchmark-template template ([value ...] ...))
```

`define-benchmark` and fixture/template instantiation must define a variable and register the resulting benchmark in `current-benchmark-registry`.

- [x] **Step 3: Implement `benchmark-expand`**

`benchmark-expand` returns a list of pairs. The car is the constructed state for the concrete run and the cdr is the benchmark descriptor variant to run. Expansion must preserve source order and produce:

```scheme
'(((n . 1) (m . 10))
  ((n . 1) (m . 20))
  ((n . 2) (m . 10))
  ((n . 2) (m . 20)))
```

for `:arg-product [n 1 2] [m 10 20]`.

- [x] **Step 4: Run tests**

Run:

```bash
cd tests && make test-some TEST='benchmark'
```

Historical red-step result: expansion and registry tests passed, while runner/reporter coverage still failed until Task 4. Current state: this task is complete.

---

### Task 4: Runner, Lifecycle, Summaries, And Reporters

**Files:**
- Modify: `chezpp/benchmark.ss`

- [x] **Step 1: Implement measurement helpers**

Use Chez `statistics`, `sstats-difference`, and exact nanosecond conversion. Provide sanitizers that turn negative time/count values into zero. Store the sanitized `sstats` in each sample.

- [x] **Step 2: Implement lifecycle-safe sample execution**

For each sample:

1. Build sample setup value outside timed work when `setup` or fixture setup exists.
2. Run body `iterations` times inside timed work.
3. Run teardown if setup completed.
4. Capture body/setup errors into the result record and continue to the next benchmark unless `stop-on-error?` is true.

Use `dynamic-wind` or guarded cleanup to ensure teardown runs after body errors.

- [x] **Step 3: Implement warmup and measured samples**

Run `warmup` samples before measured samples. For measured samples, start with one iteration and double until the measured real time reaches `min-time` or `max-iterations` is reached. For tests, `(min-time . 0)` and `(max-iterations . 1)` keep execution deterministic.

- [x] **Step 4: Implement summaries and counters**

`benchmark-summarize` computes `cpu-ns`, `real-ns`, and `bytes` entries. Each entry is an alist:

```scheme
(cpu-ns . ((median . n) (mean . n) (min . n) (max . n) (stddev . n)))
```

Merge sample counters by summing numeric values with the same key.

- [x] **Step 5: Implement reporters**

`benchmark-text-reporter` writes a plain header and one row per result. `benchmark-datum-reporter` writes one Scheme datum per result. `benchmark-report` accepts `(results reporter output-port)`.

- [x] **Step 6: Run tests**

Run:

```bash
cd tests && make test-some TEST='benchmark'
```

Expected: benchmark tests pass with empty stdout/stderr except the test harness command labels.

---

### Task 5: Aggregate Import, Build, Parentheses, And Final Verification

**Files:**
- Modify: `chezpp.ss`
- Modify: `chezpp/benchmark.ss`
- Modify: `tests/benchmark.ss`

- [x] **Step 1: Add aggregate import**

Add `(chezpp benchmark)` to the `chezpp.ss` export import list near `logging`.

- [x] **Step 2: Check Scheme parentheses**

Run:

```bash
python3 check_parentheses.py chezpp/benchmark.ss tests/benchmark.ss
```

Expected: balanced parentheses reported for both files, or no error output depending on script behavior.

- [x] **Step 3: Build project**

Run from repo root:

```bash
make clean && make
```

Expected: exit 0.

- [x] **Step 4: Run benchmark tests**

Run:

```bash
cd tests && make test-some TEST='benchmark'
```

Expected: exit 0 and no `Bug` or `Error` output.

- [x] **Step 5: Review diff**

Run:

```bash
git diff -- docs/superpowers/specs/2026-06-10-benchmark-design.md docs/superpowers/plans/2026-06-13-benchmark.md chezpp/benchmark.ss chezpp.ss tests/Makefile tests/benchmark.ss
```

Expected: roadmap v1/v2 is explicit in the plan, benchmark APIs are documented and exported, implementation matches v1 scope, and unrelated files are untouched.

---

### Current Status Update: Follow-Up Work Completed

**Files:**
- Modified: `chezpp/benchmark.ss`
- Modified: `tests/benchmark.ss`
- Created: `examples/benchmark.ss`

- [x] **Step 1: Harden JSON output**

`benchmark-json-reporter` now writes valid JSON for all strings containing control characters U+0000 through U+001F. Common escapes are emitted for quote, backslash, backspace, form feed, newline, return, and tab; other control characters use `\u00XX`. Non-JSON numeric spellings such as infinities and NaN are serialized as JSON strings.

- [x] **Step 2: Preserve baseline errors**

Baseline files still use version `1`. New files serialize benchmark errors as structured condition data under `error`, while old v1 baselines containing `(error? . #t)` remain compatible and reload as error results.

- [x] **Step 3: Validate baseline versions from file input**

`benchmark-load-baseline` validates that the baseline `version` field is numeric before comparing it with `=`, so malformed files report `unsupported benchmark baseline version` instead of a Chez primitive numeric error.

- [x] **Step 4: Add regression tests**

`tests/benchmark.ss` covers CSV/JSON reporters, JSON number/string safety, baseline save/load, legacy `error?` compatibility, malformed baseline versions, comparisons, confidence intervals, and pause/resume behavior.

- [x] **Step 5: Add runnable examples**

`examples/benchmark.ss` demonstrates explicit input rows, product inputs, fixtures, benchmark templates, suite execution, text/CSV/JSON reporters, and baseline comparison output printed to the console.

- [x] **Step 6: Verify current state**

Fresh verification used during the latest updates:

```bash
python3 check_parentheses.py chezpp/benchmark.ss tests/benchmark.ss
python3 check_parentheses.py examples/benchmark.ss
make clean && make
cd tests && make test-some TEST='benchmark'
./chez++ -q --script examples/benchmark.ss
```

---

### Current Status Update: Rich And Text Reporter Follow-Up

**Files:**
- Modified and committed: `chezpp/benchmark.ss`
- Modified and committed: `tests/benchmark.ss`
- Modified locally by request, not committed: `examples/benchmark.ss`

- [x] **Step 1: Implement customizable Rich reporter**

`benchmark-rich-reporter` now accepts an optional style alist and renders a completed `(chezpp rich)` table at reporter finish time. Supported style keys are `title`, `caption`, `box`, `show-header?`, `show-lines?`, `padding`, and `columns`; columns can select result fields such as `name`, `args`, `iterations`, `cpu-ns`, `real-ns`, `bytes`, `counters`, and `error`.

- [x] **Step 2: Align text reporter output**

`benchmark-text-reporter` now formats all rows using computed column widths. Text output keeps plain ASCII formatting, with left-aligned text columns and right-aligned numeric metric columns.

- [x] **Step 3: Update runnable example locally**

`examples/benchmark.ss` was updated to demonstrate `benchmark-rich-reporter` with custom columns and to use the rich reporter for suite output. This example change is intentionally left uncommitted per the request.

- [x] **Step 4: Verify reporter updates**

Fresh verification used for the reporter updates:

```bash
python3 check_parentheses.py chezpp/benchmark.ss tests/benchmark.ss
make clean && make
cd tests && make clean && make test-some TEST='benchmark'
python3 check_parentheses.py examples/benchmark.ss
./chez++ -q --script examples/benchmark.ss
```

---

## Self-Review

- Spec coverage: v1 core is covered by Tasks 1-5. Implemented v2 follow-up work is captured in the current status updates. Remaining deferred work is limited to optional JSON baseline format, cost-center instrumentation, and deeper deterministic pause/resume tests.
- Placeholder scan: no `TBD`, `TODO`, or unspecified implementation steps remain in this plan.
- Type consistency: public names in tests match the names exported by `chezpp/benchmark.ss`; config and result accessors match the design document.
