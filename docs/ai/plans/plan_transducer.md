# ChezPP Transducer Library Plan

This document specifies the public API, semantics, implementation strategy, and
integration points for a new transducer library for ChezScheme in ChezPP.

Goals:

- provide allocation-light sequence transformations that compose independently
  of the input and output collection type
- interoperate with `(chezpp iter)`, lists, vectors, strings, bytevectors,
  hashtables, ports, and later streams
- support early termination through a typed `reduced` protocol
- represent transducers, reducers, eductions, and reduced values as opaque
  records rather than untyped procedures
- keep the API procedural and Chez-friendly, without keyword arguments
- avoid exporting names such as `map` and `filter` that would collide with
  standard Scheme imports
- make common pipelines clear while keeping direct builtin loops fast

Non-goals:

- replacing `(chezpp iter)` or the `for` forms
- providing lazy streams as the primary abstraction
- implementing a full query language
- requiring macros for ordinary use


## 1. Proposed Library Layout

Public library:

- `(chezpp transducer)`

Private/internal libraries, if the implementation grows:

- `(chezpp transducer private core)`
- `(chezpp transducer private reducers)`
- `(chezpp transducer private source)`
- `(chezpp transducer private sequence)`

Compatibility entrypoint:

- `chezpp.ss` should import and export `(chezpp transducer)` after the library is
  implemented and tested.


## 2. Core Definitions

### Reducing procedure

A reducing procedure is the low-level step procedure used by the transducer
engine. It receives an accumulator and one input value, and returns the next
accumulator or a `reduced` value to request early termination.

```scheme
(lambda (acc x) ...)
```

The parameter name `rf` means "reducing function" in transducer literature. In
this ChezPP design, public APIs should prefer the parameter name `reducer`
because reducers are typed records. Names such as `rflist` use the `rf`
prefix as a conventional abbreviation for "reducer factory" or "reducing
function builder": `rflist` returns a reducer that accumulates a list.

### Completing reducing procedure

A completing reducing procedure is the low-level complete reducer protocol. It
supports three arities:

```scheme
(case-lambda
  [() init-value]
  [(acc) completed-acc]
  [(acc x) next-acc])
```

The zero-argument arity creates a default initial accumulator. The one-argument
arity finishes an accumulator after input ends or early termination occurs. The
two-argument arity is the reducing procedure.

Public reducer values should be records that contain one completing reducing
procedure, not three independent procedure fields and not bare `case-lambda`
values. This gives the library stable predicates, clearer errors, and room for
specialized reducer metadata while keeping the reducer protocol conceptually
single.

### Transducer

A transducer is a typed record that transforms a reducer into another reducer.
The returned reducer transforms, filters, expands, or short-circuits the input
stream before forwarding values to the wrapped reducer.

```scheme
(define xf
  (tcompose
    (tmap add1)
    (tfilter even?)
    (ttake 10)))
```

Transducers are independent of input and output. `transduce`, `into`,
`sequence`, and `eduction` decide how to run them.


## 3. Type Model

The implementation should use opaque sealed records for relevant public
concepts.

Suggested record types:

- `transducer`: name plus procedure that maps a reducer to a reducer
- `reducer`: name, completing reducing procedure, and optional type metadata
- `reduced`: early-termination wrapper around an accumulator
- `eduction`: transducer plus source, used as a reusable or one-shot
  transducible value
- `source-adapter`: internal source dispatch record for lists, vectors,
  fxvectors, flvectors, strings, bytevectors, hashtables, iterators, ports, and
  custom sources

The public predicates `transducer?`, `reducer?`, `reduced?`, `eduction?`, and
`transducible?` should identify these concepts explicitly. Avoid treating
arbitrary procedures as transducers or reducers at public boundaries. A private
adapter can exist for tests or expert interop, but the ordinary API should stay
typed.


## 4. Design Rules

- Public names for transformation constructors use the `t` prefix:
  `tmap`, `tfilter`, `ttake`, etc.
- Public procedures use `lambda` or `case-lambda`, never keyword arguments.
- Public procedures validate boundary arguments with `pcheck`.
- Public API tables explicitly state whether each API is a procedure or macro.
- The initial v1 API is procedure-only. Macros can be added later for syntax
  sugar, but the core library should not depend on them.
- A transducer should not know whether the source is a list, vector, iterator,
  port, or other collection.
- Early termination must be represented explicitly with a `reduced` record.
- Reducers must not unwrap `reduced` accidentally; only the transduction driver
  should finish unwrapping at the boundary.
- Builtin sources should use direct loops by default instead of first converting
  to iterators.
- Source dispatch should be centralized so the default loop strategy can be
  configured or changed without rewriting terminal operations.
- Iterator-backed and port-backed sources that own resources must be finalized
  by terminal operations.
- `dynamic-wind` has measurable overhead. Use it only for source kinds that need
  guaranteed cleanup, such as ports and owning iterators, not for every direct
  list/vector/string/bytevector loop.
- Phase 1 should favor correctness and simple composability while preserving the
  direct-loop path for builtin data.
- Docs should describe whether the transducer/reducer is stateful or not.
- Must include positive and negative tests for every public API.


## 5. Public API

The APIs below are the target v1 surface for `(chezpp transducer)`.

### Core Predicates And Reduced Values

| Kind | Name | Parameters | Description |
| --- | --- | --- | --- |
| Procedure | `transducer?` | `x` | Returns `#t` when `x` is a transducer record. |
| Procedure | `reducer?` | `x` | Returns `#t` when `x` is a reducer record. |
| Procedure | `reduced` | `x` | Wraps `x` in a `reduced` record to signal early termination to the transduction driver. |
| Procedure | `reduced?` | `x` | Returns `#t` when `x` is a reduced wrapper. |
| Procedure | `unreduced` | `x` | Returns the wrapped value if `x` is reduced; otherwise returns `x`. |
| Procedure | `ensure-reduced` | `x` | Returns `x` if it is already reduced, otherwise returns `(reduced x)`. |
| Procedure | `preserving-reduced` | `reducer` | Returns a reducer that applies `reducer` but preserves an incoming reduced result instead of passing it through again. Useful inside expanding transducers such as `tcat`. |
| Procedure | `completion` | `reducer` | Returns the reducer's completing reducing procedure. This is mainly an expert/debugging API; normal code should pass reducer records to terminal operations. |

### Transducer Construction And Composition

| Kind | Name | Parameters | Description |
| --- | --- | --- | --- |
| Procedure | `make-transducer` | `name proc` | Expert constructor. `name` is a symbol; `proc` receives a reducer and returns a reducer. |
| Procedure | `transducer-name` | `xform` | Returns the symbolic name of a transducer record. |
| Procedure | `tidentity` | none | Returns the identity transducer. It returns the reducer unchanged. |
| Procedure | `tcompose` | `xform ...` | Composes transducers left-to-right in pipeline order. `(tcompose (tmap f) (tfilter p))` means map first, then filter. With no arguments, returns `(tidentity)`. |
| Procedure | `tchain` | `xform-list` | Composes a list of transducers in pipeline order. This is useful when the list is computed dynamically. |

### Mapping And Filtering

| Kind | Name | Parameters | Description |
| --- | --- | --- | --- |
| Procedure | `tmap` | `proc` | Returns a transducer that applies `proc` to each input and emits the result. |
| Procedure | `tmap/i` | `proc` | Like `tmap`, but `proc` receives the zero-based item index before the item: `(proc i x)`. |
| Procedure | `tfilter` | `pred` | Returns a transducer that emits only values for which `(pred x)` is true. |
| Procedure | `tfilter/i` | `pred` | Like `tfilter`, but `pred` receives `(i x)`. |
| Procedure | `tremove` | `pred` | Returns a transducer that emits values for which `(pred x)` is false. |
| Procedure | `tkeep` | `proc` | Applies `proc` to each item and emits the result only when it is not `#f`. |
| Procedure | `tkeep/i` | `proc` | Like `tkeep`, but `proc` receives `(i x)`. |
| Procedure | `treplace` | `alist` | Replaces input values according to `alist` using `equal?`; values not present in `alist` pass through unchanged. |

### Expansion And Flattening

| Kind | Name | Parameters | Description |
| --- | --- | --- | --- |
| Procedure | `tcat` | none | Concatenates each input collection into the output stream. Builtin nested collections should be traversed with the same direct source-dispatch layer used by `transduce`. |
| Procedure | `tmapcat` | `proc` | Equivalent to `(tcompose (tmap proc) (tcat))`. |
| Procedure | `tflatten` | none | Recursively emits leaf values from nested supported collections. This should be kept out of the fast path and implemented in terms of an explicit stack. |

### Taking, Dropping, And Slicing

| Kind | Name | Parameters | Description |
| --- | --- | --- | --- |
| Procedure | `ttake` | `n` | Emits at most `n` values, then terminates with `reduced`. |
| Procedure | `tdrop` | `n` | Skips the first `n` values, then emits the rest. |
| Procedure | `ttake-while` | `pred` | Emits values while `(pred x)` is true, then terminates before emitting the first failing value. |
| Procedure | `tdrop-while` | `pred` | Skips values while `(pred x)` is true, then emits the first failing value and the rest. |
| Procedure | `ttake-nth` | `n` | Emits every `n`th value, starting with index `0`. `n` must be a positive exact integer. |
| Procedure | `tslice` | `start stop` | Emits values with indexes in `[start, stop)`. |
| Procedure | `tslice` | `start stop step` | Emits values with indexes in `[start, stop)` where `(modulo (- i start) step)` is zero. `step` must be positive in v1. |

### Partitioning And Stateful Transducers

| Kind | Name | Parameters | Description |
| --- | --- | --- | --- |
| Procedure | `tpartition` | `n` | Emits vectors of exactly `n` values. Drops the trailing partial partition. |
| Procedure | `tpartition-all` | `n` | Emits vectors of up to `n` values, including the trailing partial partition during completion. |
| Procedure | `tpartition-by` | `proc` | Starts a new partition whenever `(proc x)` changes according to `equal?`. Emits vectors. |
| Procedure | `tdedupe` | none | Removes consecutive duplicate values using `equal?`. |
| Procedure | `tdedupe-by` | `proc` | Removes consecutive values whose computed keys are `equal?`. |
| Procedure | `tdistinct` | none | Emits only the first occurrence of each value using an internal hashtable. |
| Procedure | `tdistinct-by` | `proc` | Emits only the first occurrence of each computed key. |
| Procedure | `tinterpose` | `sep` | Emits `sep` between input values. |

### Side Effects And Debugging

| Kind | Name | Parameters | Description |
| --- | --- | --- | --- |
| Procedure | `ttap` | `proc` | Calls `(proc x)` for each value, then emits the original value. |
| Procedure | `tinspect` | `who proc` | Calls `(proc who x)` for each value, then emits the original value. This is a structured debugging hook rather than printing directly. |

### Terminal Operations

| Kind | Name | Parameters | Description |
| --- | --- | --- | --- |
| Procedure | `transduce` | `xform reducer init source` | Runs `xform` over `source` using reducer record `reducer` and initial accumulator `init`. Returns the completed accumulator. |
| Procedure | `transduce` | `xform reducer source` | Uses the reducer's init procedure to obtain the initial accumulator. |
| Procedure | `transduce1` | `xform reducer source` | Uses the first value emitted after applying `xform` as the initial accumulator, then reduces the remaining emitted values. Signals an error if the transformed stream is empty. |
| Procedure | `into` | `to xform source` | Transduces `source` into a destination described by `to`. Supported `to` values in v1: `'list`, `'reverse-list`, `'vector`, `'string`, `'bytevector`, and an existing mutable vector builder if one is introduced. |
| Procedure | `sequence` | `xform source` | Returns an iterator that lazily yields the transformed values. The iterator must finalize any owned upstream source when it is finalized or exhausted. |
| Procedure | `eduction` | `xform source` | Returns an eduction record. Each traversal creates a fresh traversal over `source` when possible. If `source` is one-shot, the eduction is one-shot too. |
| Procedure | `eduction?` | `x` | Returns `#t` when `x` is an eduction record. |
| Procedure | `tfor-each` | `xform proc source` | Runs `proc` for side effects on each transformed value and returns unspecified values. |

### Type-Specific Terminal Operations

These APIs are specialized entry points for callers that already know the source
type. They avoid generic source dispatch and should loop directly over the
source representation.

| Kind | Name | Parameters | Description |
| --- | --- | --- | --- |
| Procedure | `list-transduce` | `xform reducer init list` | Runs `xform` over a list using direct pair traversal. |
| Procedure | `list-transduce` | `xform reducer list` | Like `list-transduce`, using the reducer's init procedure. |
| Procedure | `vector-transduce` | `xform reducer init vector` | Runs `xform` over a vector using `vector-length` and `vector-ref`. |
| Procedure | `vector-transduce` | `xform reducer vector` | Like `vector-transduce`, using the reducer's init procedure. |
| Procedure | `fxvector-transduce` | `xform reducer init fxvector` | Runs `xform` over an fxvector using `fxvector-length` and `fxvector-ref`. |
| Procedure | `fxvector-transduce` | `xform reducer fxvector` | Like `fxvector-transduce`, using the reducer's init procedure. |
| Procedure | `flvector-transduce` | `xform reducer init flvector` | Runs `xform` over an flvector using `flvector-length` and `flvector-ref`. |
| Procedure | `flvector-transduce` | `xform reducer flvector` | Like `flvector-transduce`, using the reducer's init procedure. |
| Procedure | `bytevector-transduce` | `xform reducer init bytevector` | Runs `xform` over unsigned bytes using `bytevector-length` and `bytevector-u8-ref`. |
| Procedure | `bytevector-transduce` | `xform reducer bytevector` | Like `bytevector-transduce`, using the reducer's init procedure. |
| Procedure | `string-transduce` | `xform reducer init string` | Runs `xform` over characters using `string-length` and `string-ref`. |
| Procedure | `string-transduce` | `xform reducer string` | Like `string-transduce`, using the reducer's init procedure. |
| Procedure | `hashtable-transduce` | `xform reducer init hashtable` | Runs `xform` over hashtable values using Chez hashtable traversal APIs. |
| Procedure | `hashtable-transduce` | `xform reducer hashtable` | Like `hashtable-transduce`, using the reducer's init procedure. |
| Procedure | `iter-transduce` | `xform reducer init iter` | Runs `xform` over an iterator using `iter-next!`. It finalizes only when ownership is explicit. |
| Procedure | `iter-transduce` | `xform reducer iter` | Like `iter-transduce`, using the reducer's init procedure. |
| Procedure | `port-lines-transduce` | `xform reducer init port` | Runs `xform` over lines read from a textual input port. The caller owns `port`; this procedure must not close it. |
| Procedure | `port-lines-transduce` | `xform reducer port` | Like `port-lines-transduce`, using the reducer's init procedure. |
| Procedure | `port-bytes-transduce` | `xform reducer init port` | Runs `xform` over bytes read from a binary input port. The caller owns `port`; this procedure must not close it. |
| Procedure | `port-bytes-transduce` | `xform reducer port` | Like `port-bytes-transduce`, using the reducer's init procedure. |

Type-specific terminal operations do not have source slicing arities. Use
`tslice` in the transducer pipeline when a source range or stride is needed.
For v1, `tslice` arguments `start` and `stop` should be exact non-negative
integers, and `step` should be an exact positive integer. Negative indexing and
negative steps can be added later only if the behavior is made consistent across
all source types.

### Reducer Builders

The `rf` prefix means "reducing function builder" by convention. In this typed
design, each `rf...` procedure returns a reducer record, not a bare procedure.

| Kind | Name | Parameters | Description |
| --- | --- | --- | --- |
| Procedure | `make-reducer` | `name proc` | Expert constructor for reducer records. `proc` is a completing reducing procedure with zero-, one-, and two-argument arities. |
| Procedure | `reducer-name` | `reducer` | Returns the symbolic name of a reducer record. |
| Procedure | `rflist` | none | Returns a reducer that accumulates values into a list in input order. Internally it may build in reverse and reverse during completion. |
| Procedure | `rfreverselist` | none | Returns a reducer that accumulates values into a reversed list. |
| Procedure | `rfvector` | none | Returns a reducer that accumulates values into a vector. Use a list builder internally first unless a vector builder exists. |
| Procedure | `rfstring` | none | Returns a reducer that accumulates characters into a string. Non-character inputs are errors. |
| Procedure | `rfbytevector` | none | Returns a reducer that accumulates exact bytes into a bytevector. |
| Procedure | `rfcount` | none | Returns a reducer that counts transformed values. |
| Procedure | `rfsum` | none | Returns a reducer that sums transformed numeric values with `+`. |
| Procedure | `rffxsum` | none | Returns a reducer that sums fixnums with `fx+`. |
| Procedure | `rfflsum` | none | Returns a reducer that sums flonums with `fl+`. |
| Procedure | `rfproduct` | none | Returns a reducer that multiplies transformed numeric values with `*`. |
| Procedure | `rfmin` | `less?` | Returns a reducer that keeps the minimum value according to `less?`. Errors on completion if no value was seen. |
| Procedure | `rfmax` | `less?` | Returns a reducer that keeps the maximum value according to `less?`. Errors on completion if no value was seen. |
| Procedure | `rfany` | `pred` | Returns a reducer that returns the first true predicate result and terminates early. |
| Procedure | `rfevery` | `pred` | Returns a reducer that returns `#f` and terminates early on the first failing value, otherwise returns `#t`. |

### Source Conversion And Dispatch

| Kind | Name | Parameters | Description |
| --- | --- | --- | --- |
| Procedure | `transducible?` | `x` | Returns `#t` for values accepted by `transduce`: eductions, iterators, lists, vectors, fxvectors, flvectors, strings, bytevectors, hashtables, ports when explicitly supported, and later streams or table rows. |
| Procedure | `source->iter` | `source` | Converts a supported source to an iterator. This is an explicit interop helper, not the default path used by `transduce` for builtin collections. |
| Procedure | `current-transducer-source-mode` | none | Parameter procedure. Returns the current source traversal mode, initially `'direct`. |
| Procedure | `current-transducer-source-mode` | `mode` | Parameter procedure. Sets source traversal mode. Supported values: `'direct` and `'iter`. `'direct` uses specialized builtin loops; `'iter` forces traversal through `source->iter` for debugging or compatibility. |


## 6. Example Usage

Basic `into`:

```scheme
(import (chezpp transducer))

(into 'vector
      (tcompose
        (tmap add1)
        (tfilter even?)
        (ttake 5))
      '(0 1 2 3 4 5 6 7 8 9 10 11))
;; => #(2 4 6 8 10)
```

`transduce` with reducer-provided initialization:

```scheme
(transduce (tmap string-length)
           (rffxsum)
           '("aa" "bbb" "c"))
;; => 6
```

`transduce` with an explicit initial accumulator:

```scheme
(transduce (tfilter odd?)
           (rflist)
           '()
           '#(1 2 3 4 5))
;; => (1 3 5)
```

Counting values after filtering:

```scheme
(transduce (tfilter positive?)
           (rfcount)
           '#(-2 0 4 6 -1))
;; => 2
```

Type-specific terminal operations:

```scheme
(vector-transduce (tcompose (tmap add1) (ttake 3))
                  (rflist)
                  '#(10 20 30 40))
;; => (11 21 31)

(bytevector-transduce (tfilter even?)
                      (rflist)
                      #vu8(1 2 3 4 5))
;; => (2 4)

(string-transduce (tmap char-upcase)
                  (rfstring)
                  "chez")
;; => "CHEZ"
```

Slicing with `tslice`:

```scheme
(vector-transduce (tcompose (tslice 0 3) (tmap add1))
                  (rflist)
                  '#(10 20 30 40 50))
;; => (11 21 31)

(string-transduce (tcompose (tslice 1 5) (tmap char-upcase))
                  (rfstring)
                  "abcdef")
;; => "BCDE"

(bytevector-transduce (tcompose (tslice 0 8 2) (tfilter even?))
                      (rflist)
                      #vu8(0 1 2 3 4 5 6 7))
;; => (0 2 4 6)
```

Early termination with `rfany`:

```scheme
(transduce (tmap abs)
           (rfany (lambda (x) (fx> x 10)))
           '(-1 -3 -12 -4))
;; => #t
```

Partition completion:

```scheme
(into 'list
      (tpartition-all 3)
      '(a b c d e))
;; => (#(a b c) #(d e))
```

Hashtable values are traversed directly by default:

```scheme
(define h (make-eq-hashtable))
(hashtable-set! h 'a 10)
(hashtable-set! h 'b 20)

(transduce (tidentity)
           (rfsum)
           h)
;; => 30
```

Lazy sequence interop:

```scheme
(define iter
  (sequence (tcompose (tdrop 2) (ttake 3))
            '(0 1 2 3 4 5 6)))

(iter-next! iter) ;; => 2
(iter-next! iter) ;; => 3
(iter-next! iter) ;; => 4
```

Forcing iterator traversal for debugging:

```scheme
(parameterize ([current-transducer-source-mode 'iter])
  (transduce (tmap add1)
             (rflist)
             '#(1 2 3)))
;; => (2 3 4)
```


## 7. Implementation Suggestions

### Records

Use opaque sealed record types for all public concepts:

```scheme
(define-record-type ($reduced make-$reduced $reduced?)
  (fields (immutable value))
  (opaque #t)
  (sealed #t))

(define-record-type ($reducer make-$reducer $reducer?)
  (fields (immutable name)
          (immutable proc)
          (immutable metadata))
  (opaque #t)
  (sealed #t))

(define-record-type ($transducer make-$transducer $transducer?)
  (fields (immutable name)
          (immutable reducer-proc)
          (immutable metadata))
  (opaque #t)
  (sealed #t))
```

Export renamed predicates such as `reducer?` and `transducer?`; keep the record
constructors private unless the public expert constructors are enough.

### Reducer protocol

Reducer records should store one completing reducing procedure. They should
expose helper procedures internally:

- `reducer-proc`: return the completing reducing procedure
- `reducer-init`: call the reducer procedure with zero arguments
- `reducer-complete`: call the reducer procedure with one argument
- `reducer-step`: call the reducer procedure with two arguments
- `reducer-with-proc`: derive a reducer with the same name/metadata and a new
  completing reducing procedure

If benchmarks show that repeated `case-lambda` arity dispatch is too expensive
in hot loops, the implementation may cache split init/complete/step procedures
privately. That cache should be an optimization detail, not the public reducer
record model.

Transducer constructors should produce records whose reducer procedure accepts a
reducer record and returns a reducer record. Stateful transducers allocate their
state during that call, not when the transducer constructor is called.

### Driver loop

`transduce` should:

1. Validate `xform`, `reducer`, and `source`.
2. Build the effective reducer by applying `xform` to `reducer`.
3. Obtain the initial accumulator from either the explicit `init` argument or
   the reducer init procedure.
4. Dispatch source traversal through one internal source runner.
5. Iterate until source end or `reduced?`.
6. Complete and unwrap the accumulator.
7. Finalize only sources that own resources.

The generic `transduce` should be a thin dispatcher over the same internal
helpers used by the type-specific terminal operations. For example, after
validation, vector input should reach the same direct vector loop used by
`vector-transduce`.

### Direct source traversal

Builtin source types should use direct loops by default:

- lists: walk pairs directly
- vectors: use `vector-length` and `vector-ref`
- fxvectors: use `fxvector-length` and `fxvector-ref`
- flvectors: use `flvector-length` and `flvector-ref`
- strings: use `string-length` and `string-ref`
- bytevectors: use `bytevector-length` and `bytevector-u8-ref`
- hashtables: use Chez hashtable traversal APIs directly
- eductions: run their stored source and transducer without materializing
- iterators: use `iter-next!`

Do not convert these source types to `(chezpp iter)` first in the default path.
Iterator conversion is useful for interop, but it adds avoidable allocation and
dispatch overhead for builtin collections.

Keep this decision easy to adjust by factoring source traversal through a
single internal procedure:

```scheme
(run-source source step acc mode)
```

`mode` can be the value of `current-transducer-source-mode`. In `'direct` mode,
the runner specializes builtin collections. In `'iter` mode, it calls
`source->iter` and uses the iterator loop. This gives a simple debug and
compatibility switch without duplicating terminal operation code.

Each `*-transduce` procedure should call a concrete direct loop without going
through generic dispatch. This keeps known-type code fast and makes benchmark
results easier to interpret.

Source slicing is expressed with the `tslice` transducer instead of extra
terminal-operation arities. This keeps the direct source runners focused on
walking the full source representation while the transducer pipeline handles
index filtering and early termination consistently for every supported source.

### Resource cleanup

Use `dynamic-wind` only when the source owns a resource that must be closed even
on nonlocal exit. Examples include file-backed iterators and ports opened by the
transducer library.

For ordinary lists, vectors, strings, bytevectors, and hashtables, avoid wrapping
the loop in `dynamic-wind`; the cost is unnecessary and affects the common fast
path.

If the caller provides an already-open port, the ownership rule must be explicit:
either the terminal operation does not close caller-owned ports, or a separate
source wrapper marks the port as owned. Do not close ambiguous resources.

### Composition order

Users expect pipeline order. Internally, record-backed composition should still
behave like:

```scheme
(define tcompose
  (lambda xforms
    (make-transducer
      'tcompose
      (lambda (reducer)
        (fold-right
          (lambda (xform reducer)
            ((transducer-reducer-proc xform) reducer))
          reducer
          xforms)))))
```

Add tests that make the order visible.

### Stateful xforms

Each call to a transducer with a reducer must allocate fresh state. For example,
`ttake`, `tdrop`, `tpartition-all`, and `tdistinct` should allocate counters,
buffers, or hashtables inside the transducer's reducer procedure, not when the
transducer constructor is called.

### Collection builders

Reuse `make-list-builder` where possible. Add a small private vector builder
only if repeated vector output becomes common enough to justify it.

### Iterator interop

`sequence` is the only API that requires careful lazy behavior. Implement it
after eager `transduce` is stable. A simple and correct first implementation can
buffer produced values in a small queue while pulling from the upstream source
until either one value is available, upstream ends, or a reduced value appears.


## 8. Testing Guidance

Tests should cover:

- record predicates for transducers, reducers, reduced values, and eductions
- rejection of bare procedures at public transducer/reducer boundaries
- composition order
- empty sources
- early termination with `ttake`, `rfany`, and `rfevery`
- completion behavior for `tpartition-all`
- direct traversal for list, vector, fxvector, flvector, string, bytevector,
  hashtable, and iterator sources
- type-specific terminal operations: `list-transduce`, `vector-transduce`,
  `fxvector-transduce`, `flvector-transduce`, `bytevector-transduce`,
  `string-transduce`, `hashtable-transduce`, `iter-transduce`,
  `port-lines-transduce`, and `port-bytes-transduce`
- `tslice` over list, vector, fxvector, flvector, bytevector, string, iterator,
  and eduction sources
- rejection of removed source slicing arities on type-specific terminal
  operations
- source mode override with `current-transducer-source-mode`
- cleanup for owned file and port-backed sources
- no cleanup of caller-owned ports unless ownership is explicit
- type errors for public APIs
- state isolation when the same transducer value is reused

Negative tests should include comments describing the error case being tested.


## 9. Phase Plan

Phase 1:

- record types and predicates for reduced values, reducers, transducers, and
  eductions
- `make-transducer`, `make-reducer`, `transducer-name`, `reducer-name`
- `tidentity`, `tcompose`, `tchain`
- `tmap`, `tfilter`, `tremove`, `tkeep`
- `ttake`, `tdrop`, `ttake-while`, `tdrop-while`
- `transduce`, `into`
- `list-transduce`, `vector-transduce`, `bytevector-transduce`,
  `string-transduce`, and `iter-transduce`
- `rflist`, `rfvector`, `rfcount`, `rfsum`, and numeric sum reducers
- direct source loops for list, vector, string, bytevector, and iterator sources

Phase 2:

- partitioning, distinct, dedupe, interpose
- indexed variants
- string and bytevector reducers
- `tfor-each`
- `fxvector-transduce`, `flvector-transduce`, `hashtable-transduce`,
  `port-lines-transduce`, and `port-bytes-transduce`
- configurable direct/iterator source mode

Phase 3:

- `sequence`
- `eduction`
- `source->iter` interop helper
- deeper integration with `(chezpp iter)` pipelines
- performance tuning and specialized loops
