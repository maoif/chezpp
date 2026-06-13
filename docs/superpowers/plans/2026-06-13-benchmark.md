# Benchmark Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build `(chezpp benchmark)` as an in-process benchmark definition, execution, and reporting library matching v1 of the benchmark design, with v2 reporting/comparison work documented as the next roadmap phase.

**Architecture:** v1 lives in `chezpp/benchmark.ss` as a single public library split by internal comment sections for records, option parsing, expansion, runner, summaries, reporters, and macros. The runner uses Chez `statistics`/`sstats-difference` and `black-box`, returns structured records, and keeps CLI, persisted baselines, JSON, CSV, and rich comparison features in v2.

**Tech Stack:** ChezScheme libraries, Chez records, Chez `statistics`, `black-box`, Chezpp `pcheck`, existing `mat` test harness, `make clean && make`, and `tests/make test-some`.

---

## Roadmap

### v1: In-Process Core Implemented By This Plan

- Public library `(chezpp benchmark)` imported from `chezpp.ss`.
- Opaque records and public accessors for benchmark descriptors, fixtures, suites, states, samples, results, configs, and reporters.
- Procedural registration APIs with a parameterized default registry.
- Macro APIs for `define-benchmark`, `define-benchmark-fixture`, `define-fixture-benchmark`, `define-benchmark-suite`, `define-benchmark-template`, `instantiate-benchmark-template`, and `benchmark-options`.
- Argument expansion for `:args`, `:arg-range`, `:dense-arg-range`, and `:arg-product`.
- Sample-scoped setup/teardown, fixture setup/teardown, benchmark suite execution, and cleanup on body errors.
- Warmup, measured samples, simple adaptive iteration growth to satisfy `min-time`, and capped iterations.
- Summary alist containing CPU, real, and allocated-byte per-iteration statistics: `median`, `mean`, `min`, `max`, and `stddev`.
- Default text reporter and datum reporter.
- `benchmark-do-not-optimize` using Chez `black-box` and weak `benchmark-clobber-memory`.
- `benchmark-pause-timing` and `benchmark-resume-timing` exported as state markers for future subtraction, but v1 runner does not subtract paused deltas.
- Cost-center mode is deferred. `benchmark-config-cost-center?` exists and samples return `#f` for cost-center data.

### v2: Reporting And Comparison Roadmap

- CSV and JSON reporters once repo JSON support is stable.
- Persisted baseline files represented as Scheme data first, JSON later.
- Result comparison helpers for percent difference, absolute thresholds, and noise thresholds.
- Optional richer summaries such as confidence intervals after v1 sample behavior is proven.
- Optional Rich reporter when `(chezpp rich)` is available without making `(chezpp benchmark)` depend on terminal features.
- Implement measured pause/resume subtraction with deterministic fake measurement injection.
- Add explicit cost-center sample fields once instrumentation expectations and tests are stable.

---

## File Structure

- Create `tests/benchmark.ss`: deterministic tests for descriptors, option parsing, expansion, lifecycle, runner output shape, reporters, and error capture.
- Modify `tests/Makefile`: add `benchmark.ss` to `SRCS_TEST`.
- Modify `chezpp/benchmark.ss`: replace the empty shell with the full public implementation.
- Modify `chezpp.ss`: import/export `(chezpp benchmark)` from the aggregate Chezpp library.
- Keep implementation in one file for v1. Split into `(chezpp benchmark core)`, `(chezpp benchmark runner)`, `(chezpp benchmark reporter)`, and `(chezpp benchmark syntax)` only if the file becomes hard to maintain after v1.

---

### Task 1: Failing Tests And Test Harness Entry

**Files:**
- Create: `tests/benchmark.ss`
- Modify: `tests/Makefile`

- [ ] **Step 1: Add benchmark to test build list**

Modify `tests/Makefile` so `SRCS_TEST` includes `benchmark.ss` near the other public library tests:

```make
SRCS_TEST := record.ss datatype.ss match.ss for.ss control.ss os.ss iter.ss transducer.ss file.ss path.ss navigator.ss \
             list.ss string.ss vector.ss array.ss dlist.ss stack.ss queue.ss heap.ss \
             hashset.ss treemap.ss treeset.ss \
             bittree.ss bitvec.ss dset.ss \
             concurrency.ss \
             cli.ss rich.ss logging.ss benchmark.ss hash.ss crypto.ss uuid.ss $(NET_TESTS)
```

- [ ] **Step 2: Write failing tests**

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

- [ ] **Step 3: Run tests to verify they fail**

Run:

```bash
cd tests && make test-some TEST='benchmark'
```

Expected: compilation fails because `(chezpp benchmark)` exports no benchmark APIs yet.

---

### Task 2: Core Records, Configs, Registry, And Options

**Files:**
- Modify: `chezpp/benchmark.ss`

- [ ] **Step 1: Implement public exports, records, accessors, constructors, registry, and option parsing**

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

- [ ] **Step 2: Run tests**

Run:

```bash
cd tests && make test-some TEST='benchmark'
```

Expected: tests still fail on missing macros/runner, but core constructor and predicate errors are gone.

---

### Task 3: Macros And Argument Expansion

**Files:**
- Modify: `chezpp/benchmark.ss`

- [ ] **Step 1: Implement option macros**

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

- [ ] **Step 2: Implement registration macros**

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

- [ ] **Step 3: Implement `benchmark-expand`**

`benchmark-expand` returns a list of pairs. The car is the constructed state for the concrete run and the cdr is the benchmark descriptor variant to run. Expansion must preserve source order and produce:

```scheme
'(((n . 1) (m . 10))
  ((n . 1) (m . 20))
  ((n . 2) (m . 10))
  ((n . 2) (m . 20)))
```

for `:arg-product [n 1 2] [m 10 20]`.

- [ ] **Step 4: Run tests**

Run:

```bash
cd tests && make test-some TEST='benchmark'
```

Expected: expansion and registry tests pass; runner/reporter tests still fail until Task 4.

---

### Task 4: Runner, Lifecycle, Summaries, And Reporters

**Files:**
- Modify: `chezpp/benchmark.ss`

- [ ] **Step 1: Implement measurement helpers**

Use Chez `statistics`, `sstats-difference`, and exact nanosecond conversion. Provide sanitizers that turn negative time/count values into zero. Store the sanitized `sstats` in each sample.

- [ ] **Step 2: Implement lifecycle-safe sample execution**

For each sample:

1. Build sample setup value outside timed work when `setup` or fixture setup exists.
2. Run body `iterations` times inside timed work.
3. Run teardown if setup completed.
4. Capture body/setup errors into the result record and continue to the next benchmark unless `stop-on-error?` is true.

Use `dynamic-wind` or guarded cleanup to ensure teardown runs after body errors.

- [ ] **Step 3: Implement warmup and measured samples**

Run `warmup` samples before measured samples. For measured samples, start with one iteration and double until the measured real time reaches `min-time` or `max-iterations` is reached. For tests, `(min-time . 0)` and `(max-iterations . 1)` keep execution deterministic.

- [ ] **Step 4: Implement summaries and counters**

`benchmark-summarize` computes `cpu-ns`, `real-ns`, and `bytes` entries. Each entry is an alist:

```scheme
(cpu-ns . ((median . n) (mean . n) (min . n) (max . n) (stddev . n)))
```

Merge sample counters by summing numeric values with the same key.

- [ ] **Step 5: Implement reporters**

`benchmark-text-reporter` writes a plain header and one row per result. `benchmark-datum-reporter` writes one Scheme datum per result. `benchmark-report` accepts `(results reporter output-port)`.

- [ ] **Step 6: Run tests**

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

- [ ] **Step 1: Add aggregate import**

Add `(chezpp benchmark)` to the `chezpp.ss` export import list near `logging`.

- [ ] **Step 2: Check Scheme parentheses**

Run:

```bash
python3 check_parentheses.py chezpp/benchmark.ss tests/benchmark.ss
```

Expected: balanced parentheses reported for both files, or no error output depending on script behavior.

- [ ] **Step 3: Build project**

Run from repo root:

```bash
make clean && make
```

Expected: exit 0.

- [ ] **Step 4: Run benchmark tests**

Run:

```bash
cd tests && make test-some TEST='benchmark'
```

Expected: exit 0 and no `Bug` or `Error` output.

- [ ] **Step 5: Review diff**

Run:

```bash
git diff -- docs/superpowers/specs/2026-06-10-benchmark-design.md docs/superpowers/plans/2026-06-13-benchmark.md chezpp/benchmark.ss chezpp.ss tests/Makefile tests/benchmark.ss
```

Expected: roadmap v1/v2 is explicit in the plan, benchmark APIs are documented and exported, implementation matches v1 scope, and unrelated files are untouched.

---

## Self-Review

- Spec coverage: v1 core is covered by Tasks 1-5. v2 reporting/comparison is documented in the roadmap and intentionally not implemented in v1.
- Placeholder scan: no `TBD`, `TODO`, or unspecified implementation steps remain in this plan.
- Type consistency: public names in tests match the names exported by `chezpp/benchmark.ss`; config and result accessors match the design document.
