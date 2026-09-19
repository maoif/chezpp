# ChezPP Data-Structure API Review

## Scope

Reviewed `array`, `bittree`, `bitvec`, `dlist`, `dset`, `hashset`, `heap`, `list`,
`queue`, `stack`, `string`, `treeset`, `treemap`, and `vector`.

## Current Status (2026-09-18)

The API review implementation is complete through the current integration commits.
Custom data structures now register iterator and navigator adapters from their owning
libraries, and transducers consume the same iterator registry for custom sources.
Sizes, membership, deletion policy, bit-set bounds/algebra, string traversal/slicing,
and the reviewed public documentation have been normalized. Navigator set clearing now
uses delete callbacks, including for custom set protocols.

The focused reviewed suites, clean build, documentation checker, Scheme balance checks,
and the copied ChezScheme header comparison pass. The aggregate `make -C tests test-all`
run still has an unrelated, timing-dependent `net-websocket-phase4-features` failure:
`websocket pong timed out`. The focused `net-websocket` suite passes, and this branch has
no networking changes.

## Resolved Findings

1. Registered adapters make arrays, dlists, queues, stacks, heaps, sets, maps, bit sets,
   and dsets available to `iter` and `transducer` with documented traversal orders.
2. `queue-pop-all!` validates its callback branch, and the reviewed heap and treemap
   documentation now matches their boolean and empty-map behavior.
3. Sequence `X-contains?` procedures return booleans; indexed lookup is provided by
   `X-index-of` and `X-find-index`.
4. Indexed deletion reports range errors, while absent set/map deletion is idempotent.
5. Reviewed public APIs use checker-compatible `#|proc:name` and `#|macro:name` docs.
6. There is no separate character library requirement; string character operations use
   ChezScheme primitives, with the missing string traversal procedures supplied here.
7. `bitvec-bound` is public and sparse bittree union, intersection, and xor are exposed.

## Resolved Iterator Improvements

The iterator follow-up has resolved the registry and lifecycle issues identified by this
review:

1. **Reject or define zero and negative steps.** `list->iter`, `vector->iter`, string
   iterators, the typed indexed iterators, and `range` accept a zero step. Their next
   operation then repeats forever. Negative steps are marked TODO; current calls can
   silently return an empty iterator or fail through an incidental bounds error. Share
   the slice/index normalization rules used elsewhere, or explicitly reject unsupported
   directions with a checked error.
2. **Guard finalized iterators.** `iter-reset!` rejects finalized iterators, but
   `iter-next!` does not. After `iter->list` finalizes an iterator, another `iter-next!`
   can still invoke its source callback, including a callback for a closed file or port.
   Make the lifecycle contract consistent and test next/reset/finalize transitions.
3. **Finish the hashtable iterator path.** The public `get-iter` dispatch calls the
   unimplemented `hashtable->iter` TODO, while `iter-source->iter` separately converts
   hashtable values to a vector. Implement one path and make both dispatchers agree on
   value traversal and unspecified ordering.
4. **Clarify the source-conversion API.** The plan names the iterator-library helper
   `source->iter`, but `(chezpp iter)` exports `iter-source->iter` because transducer
   already exports `source->iter`. Keep the split only if it is intentional and document
   it; otherwise provide an unambiguous alias so users do not need to know the layering.
5. **Resolve port ownership.** `iter.ss` says callers open and close ports, but the
   textual port iterator finalizer closes the caller-provided port. Document iterator
   ownership explicitly or stop closing externally owned ports; file iterators, which
   open their own ports, should continue to close them during finalization.
6. **Specify adapter snapshot semantics.** Most registered adapters snapshot through
   `X->list` when the iterator is created. This makes reset stable but hides subsequent
   source mutations. Document that behavior, or provide stateful adapters where live
   traversal is part of the data-structure contract.
7. **Define registration replacement policy.** Registrations are prepended, so duplicate
   predicates are allowed and the newest one shadows older entries. Either document this
   deliberately or reject duplicate registrations to avoid load-order surprises.

Indexed sources now use directional slice-compatible bounds and reject zero steps, while lists
remain forward-only. Iterator lifecycle checks are consistent, port ownership is explicit, and
the low-level iterator conversion is distinct from the transducer wrapper. Registered mutable
sources traverse live storage and reset against current contents; active-pass mutation is
unspecified. ChezScheme hashtables and hashsets are the documented exception: each pass snapshots
keys because ChezScheme exposes no lazy cursor, while values are read from the source as needed.
Duplicate predicate procedure objects are rejected atomically, and distinct overlapping
predicates retain newest-first precedence.

## API Summary

| Family | Structures | Main operations |
| --- | --- | --- |
| Immutable sequence | `list` | map/fold/scan, uniqueness, slicing, sorting predicates, set-like list algebra |
| Fixed sequence | `vector` | map/fold/scan, filter/partition, membership, reverse, zip, shuffle, sort, arithmetic |
| Mutable sequence | `array`, `dlist` | ref/set/add/delete, slices/copies, two-ended push/pop, filter/search, map/fold, sort |
| Text | `string` | indexed traversal, search, prefix/suffix, split, trim |
| Adapters | `queue`, `stack` | push/pop/peek, bulk pop, size/empty, clear, contains, copy, list conversion |
| Priority | `heap` | comparator, bounded/unbounded construction, push/pop/peek, priority extraction, copy |
| Unordered set | `hashset` | add/delete, search/filter/partition, map/for-each, set algebra, conversions |
| Ordered set | `treeset` | hashset-like operations plus order queries, ordered folds, and set algebra |
| Bit set | `bitvec`, `bittree` | set/unset/flip, population count, ordered traversal; dense versus sparse storage |
| Key-value | `treemap` | set/ref/delete, keys/values/cells, search/filter, order queries, ordered traversal |
| Equivalence classes | `dset` | union-find: same-set, union, size |

## Recommendations

- Make every `X-contains?` boolean. Add `X-index-of` or `X-find-index` for sequences.
- Standardize constructor/observer names: `make-X`, `X`, `X?`, `X-empty?`,
  `X-size`/`X-length`, and `X->list`.
- Document mutator return values and absent-delete behavior consistently.
- Use `pred` for unary predicates; reserve `=?` for binary equality procedures.
- Add `bitvec-bound` and align bit-set algebra where meaningful.
- Document traversal order for every adapter: queue oldest-first, stack newest-first,
  heap priority-pop order, and tree in-order by default.

## Iter, Transducer, and Navigator Design

Add a source-adapter registry to `(chezpp iter)` with a predicate and iterator-maker:

```scheme
(iter-register-source!
  array?
  (lambda (arr)
    (let ([i 0] [n (array-length arr)])
      (make-iter
       (lambda ()
         (if (fx= i n)
             iter-end
             (let ([x (array-ref arr i)])
               (set! i (fx1+ i))
               x)))
       (lambda () (set! i 0))))))
```

`source->iter` and `transducible?` should consult this registry after built-ins.
An optional direct reducer runner can be added later for performance; ordinary custom
sources can initially use `run-iter`.

The navigator registry already supports indexed and keyed containers. Register arrays
and dlists as indexed containers, treemaps as keyed containers, and add a separate set
protocol for treesets, hashsets, bitvecs, and bittrees rather than pretending sets are
maps with dummy values. `dset` should remain outside navigator traversal.

Example transduction after registration:

```scheme
(transduce (tcompose (tfilter even?) (tmap add1))
           (rffxsum)
           (array 1 2 3 4 5 6))
;; => 15
```
