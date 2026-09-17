# ChezPP Data-Structure API Review

## Scope

Reviewed `array`, `bittree`, `bitvec`, `dlist`, `dset`, `hashset`, `heap`, `list`,
`queue`, `stack`, `string`, `treeset`, `treemap`, and `vector`.

## Findings

1. Custom data structures cannot currently participate in `iter` or `transducer`.
   `iter` supports built-in sequences and ports, while `transducer` dispatches only
   those source types. `dlist` still contains `TODO iter API`.
2. `queue-pop-all!` does not validate its `(queue callback)` branch with `pcheck` and
   binds a local named `procedure?`, so invalid arguments produce incidental errors.
3. `array-contains?` and `dlist-contains?` return an index, while hashset, treeset,
   queue, stack, and heap variants return booleans. `heap-contains/p?` is documented
   as returning an index but returns `#t`.
4. Deletion semantics differ: tree deletion errors when absent, hashset deletion is
   idempotent, and indexed deletion errors out of range. The policy should be explicit.
5. Public documentation is inconsistent. Most APIs use `#|doc`, while the repository
   documentation checker expects `#|proc:name`; it reports many exported APIs as
   undocumented.
6. `treemap-max` documentation says both that empty maps raise an error and return
   `#f`.
7. There is no `chezpp/char.ss`; character operations are ChezScheme/string APIs.
8. `bitvec` and `bittree` expose different algebra surfaces. `bitvec` has and/or/xor/not,
   while `bittree` exposes merge only. `bitvec-size` is population count, but its bound
   is not public.

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
;; => 12
```
