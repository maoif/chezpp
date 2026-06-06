# ChezPP Transducer Phase 1 Execution Plan

Source design: `docs/ai/plans/plan_transducer.md`

Goal: implement and verify the Phase 1 `(chezpp transducer)` API with typed
records, early termination, direct builtin source traversal, and integration
through `(chezpp)`.

Scope:

- record-backed reduced values, reducers, transducers, and eductions
- reducer and transducer expert constructors plus predicates and names
- `tidentity`, `tcompose`, `tchain`
- `tmap`, `tfilter`, `tremove`, `tkeep`
- `ttake`, `tdrop`, `ttake-while`, `tdrop-while`
- `transduce`, `into`
- `list-transduce`, `vector-transduce`, `bytevector-transduce`,
  `string-transduce`, `iter-transduce`
- `rflist`, `rfreverselist`, `rfvector`, `rfcount`, `rfsum`, `rffxsum`,
  `rfflsum`
- direct loops for lists, vectors, bytevectors, strings, and iterators

Out of scope for this execution pass:

- Phase 2 stateful transforms such as partitioning, distinct, dedupe,
  interpose, indexed variants, hashtable/fxvector/flvector/port operations,
  sliced arities, and source-mode overrides
- Phase 3 lazy `sequence`, reusable `eduction`, and generic `source->iter`
  interop

Tasks:

1. Add `tests/transducer.ss` to `tests/Makefile` so the test target compiles
   and runs the new test file.
2. Write Phase 1 tests first in `tests/transducer.ss`, including positive
   behavior and commented negative cases for public boundary errors.
3. Run `cd tests && make test-some TEST='transducer'` and confirm the new tests
   fail because the API is not implemented/exported yet.
4. Implement `chezpp/transducer.ss` with documented public APIs, `pcheck`
   boundary validation, opaque sealed records, direct loops, reducer completion,
   and early termination handling.
5. Export `(chezpp transducer)` from `chezpp.ss`.
6. Check Scheme parentheses for modified Scheme files.
7. Run `make clean && make`.
8. Run `cd tests && make test-some TEST='transducer'`.
9. If the focused tests pass, run a small related smoke target such as
   `cd tests && make test-some TEST='iter transducer'`.
