# Fiber Scheduler Correctness Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make CSPP fibers correct under shutdown, blocking, event selection, timers, and
concurrent scheduler contexts, while documenting an IO design based on Go and Concurrent ML.

**Architecture:** Each `run-fibers` invocation owns a run context containing its scheduler vector,
stop state, terminal result/error, parking mutex/condition, and timer wait state. Scheduler
workers publish work and wake parked workers through one condition-variable protocol; fibers
remain continuation-based and never call a blocking OS condition wait themselves. Events use an
explicit synchronization transaction with cancellation cleanup, following Concurrent ML's
atomic poll/register/commit model. IO remains a design-only follow-up: a scheduler-owned
readiness poller with a wakeup descriptor for sockets/pipes, and worker execution for blocking
regular-file operations.

**Tech Stack:** ChezScheme threads, mutexes, condition variables, timer interrupts,
continuations, existing `dlist`/spinlock/abox utilities, existing `cspp net poll` and
`net-operation` APIs, and the `mat` test framework.

**Spec:** User-approved fiber review findings and decisions in the task conversation on 2026-10-14.

## Global Constraints

- Create all worktrees under `.worktrees`; preserve the worktree after merging.
- Use only `lambda` and `case-lambda` in procedure definitions; do not use keyword arguments.
- Use `pcheck` on exported procedures and macros where the API accepts checked values.
- Put the required Markdown API documentation immediately above every changed public implementation.
- Keep every documentation line at or below 100 characters.
- Close every port opened by tests or implementation code.
- Run `make clean && make` from the project root before completion.
- Run selected tests with `make test ...` from `tests/`; successful tests must produce no
  unexpected output.
- Check balanced parentheses in every changed Scheme file.
- Use `set-timer` only for instruction-count preemption; use monotonic deadlines for fiber
  timers and convert each deadline to a `time-duration` before calling `condition-wait`.
- Use a dedicated scheduler park mutex at exactly one ownership level when calling
  `condition-wait`; never acquire it recursively or hold a queue spinlock across the wait.
- Keep one lock order throughout the implementation: run-context park mutex first, then a
  scheduler or event queue spinlock; never acquire them in reverse order.
- Do not implement socket/pipe or regular-file fiber IO in this plan; record the design and
  boundaries only.

### Task 1: Add Reproducible Fiber Regression Tests

**Files:**
- Create: `tests/fiber.ss`
- Modify: `tests/test-files.mk`

**Interfaces:**
- Consumes the current public API from `(cspp concurrency fiber)` through `(cspp)`.
- Produces named `mat` cases that fail against the current implementation and remain as
  regression coverage for scheduler, event, mutex, cvar, timer, and context behavior.

- [ ] **Step 1: Write the failing tests**

  Add bounded tests with one case per behavior. Include comments for every negative case:

  - `fiber-event-never-blocks`: run a child waiting on `event-never`, let a peer signal a
    completion channel, and assert the waiting child has not completed before the test ends.
  - `fiber-empty-select-is-never`: synchronize an empty selection in a child and assert it
    remains blocked rather than returning the `event-never` constructor.
  - `fiber-wrap-does-not-mutate-source`: synchronize both the source and wrapped event and
    assert the source result is unchanged.
  - `fiber-quit-stops-context`: call `fiber-quit` from the init fiber and assert `run-fibers`
    returns without hanging.
  - `fiber-quit-stops-only-child`: call `fiber-quit` from a child, assert the init fiber and
    sibling fibers continue, and assert the run context does not shut down early.
  - `fiber-shutdown-drops-runnable-children`: create a child that yields forever, finish the
    init thunk, and assert the context joins promptly.
  - `fiber-init-error-reaches-caller`: raise from the init thunk and assert the caller catches
    the original condition.
  - `fiber-multiple-values-preserved`: return two values from the init thunk and assert both
    reach the caller.
  - `fiber-nested-run-spawns-and-returns`: call nested `run-fibers`; assert it enqueues the
    nested thunk in the existing context and returns immediately.
  - `fiber-cvar-no-lost-wakeup`: alternate two fibers for enough iterations to reproduce the
    current intermittent cvar hang, with a bounded watchdog.
  - `fiber-mutex-owner-and-unwind`: assert a foreign fiber cannot release a mutex and that an
    exception inside `with-fiber-mutex` does not leave the mutex held.
  - `fiber-select-cleans-losing-registrations`: repeatedly select between an unused channel
    and an active channel, then inspect only public behavior through a completion watchdog;
    the test must not hang or require stale queue growth.
  - `fiber-sleep-event`: assert millisecond and second sleep events delay completion without
    consuming a full OS worker per sleeping fiber.
  - `fiber-with-timer-off-restores-on-error`: catch an exception inside `with-timer-off` and
    assert the caller's preemption timer is restored afterward.
  - `fiber-independent-contexts`: run two `run-fibers` calls concurrently on separate OS
    threads and assert both results are delivered without invalid-memory-reference errors.

- [ ] **Step 2: Register the test file**

  Add `fiber.ss` to `SRCS_TEST` in `tests/test-files.mk` near the existing concurrency tests.

- [ ] **Step 3: Run the focused tests and verify they fail for known reasons**

  Run:

  ```sh
  cd tests
  make test fiber.ss
  ```

  Expected failures include the current `event-never` immediate return, nested `run-fibers`
  `todo`, lost cvar wakeups, swallowed init errors, and shutdown hangs. Stop if the test file
  fails to compile for an unrelated reason; correct the test harness before implementation.

- [ ] **Step 4: Commit the test baseline**

  ```sh
  git add tests/fiber.ss tests/test-files.mk
  git commit -m "fiber: add scheduler regression tests"
  ```

### Task 2: Introduce Per-Run Scheduler Context and Safe Parking

**Files:**
- Modify: `src/cspp/concurrency/fiber.ss:35-353`
- Test: `tests/fiber.ss`

**Interfaces:**
- Consumes existing `scheduler`, `dlist`, `spinlock`, `abox`, `fork-thread`, and `thread-join` APIs.
- Produces a private run-context record containing scheduler vector, stop state, terminal
  result/error, park mutex, park condition, and an atomic publication predicate.
- Preserves `spawn-fiber`, `fiber-yield`, `fiber-quit`, and `run-fibers` public names.

- [ ] **Step 1: Define the run-context state and task publication invariant**

  Add a private record whose fields include the scheduler vector, a stop flag, a terminal
  result container, a terminal condition container, a mutex/condition pair for parked worker
  threads, and a count or predicate for published runnable work. Replace the global `*scheds*`
  lookup with the current thread's context; do not allow a worker from one context to steal
  work from another.

  All task publication paths (`spawn-fiber`, yield requeue, continuation resume, timer wake,
  and network waiter wake) must acquire the context park mutex before publishing work, then
  signal one parked scheduler. A scheduler that is about to park must acquire the same mutex,
  recheck runnable work and stop state, and only then call `condition-wait`. This closes the
  enqueue-versus-sleep lost-wakeup window.

- [ ] **Step 2: Replace the fixed idle sleep with condition-variable parking**

  Change the scheduler loop so it checks its local queue and permitted steal queues, then parks
  on the context condition when no task is available. Use an indefinite `condition-wait` when
  there is no timer deadline. When a deadline exists, compute a `time-duration` with
  `time-difference` from the monotonic deadline to the current monotonic time and pass that
  duration to `condition-wait`; Chez does not accept a `time-monotonic` object as its timeout.
  Treat a timeout return as a reason to inspect the timer queue, not as proof that a task is
  ready. On every wake, recheck the stop predicate and queues. Do not hold a spinlock while
  calling `condition-wait`, and ensure the park mutex is owned exactly once.

- [ ] **Step 3: Make shutdown a first-class scheduler state**

  Make init completion set the context stop state and broadcast the condition. `fiber-quit`
  terminates only the current fiber; if the current fiber is the init fiber, it also requests
  context shutdown. Each worker checks stop state before selecting a new task and after returning
  from a task; it must not run newly published child work after shutdown. Join all worker
  threads, restore thread-local scheduler state, clear the context reference, and return the
  recorded init result.

  Update the public documentation to state that `fiber-quit` terminates the current fiber and
  only stops the entire context when called by its init fiber.

- [ ] **Step 4: Make preemption suppression exception-safe**

  Change `with-timer-off` so the previous instruction-count timer is restored on both normal
  return and an exception. Use an exception cleanup mechanism that does not release or reacquire
  fiber-owned locks when continuations yield; `set-timer` remains unrelated to wall-clock timer
  deadlines.

- [ ] **Step 5: Implement nested `run-fibers` as enqueue-and-return**

  When called from a fiber, wrap the thunk with the normal task exception boundary, publish it
  into the current context, and return immediately with the existing unspecified return value.
  Update the public documentation to state that nested calls do not create a second scheduler
  context and do not wait for the nested thunk.

- [ ] **Step 6: Run scheduler regressions and inspect resource usage**

  Run:

  ```sh
  cd tests
  make test fiber.ss
  ```

  Confirm the shutdown and independent-context cases terminate. Run the one-second sleep probe
  under `strace -f -c -e trace=clock_nanosleep,nanosleep,futex`; the scheduler should no longer
  issue tens of thousands of fixed 10-microsecond sleeps.

- [ ] **Step 7: Commit the scheduler context change**

  ```sh
  git add src/cspp/concurrency/fiber.ss tests/fiber.ss
  git commit -m "fiber: park idle schedulers and isolate run contexts"
  ```

### Task 3: Define Error and Return-Value Propagation

**Files:**
- Modify: `src/cspp/concurrency/fiber.ss:231-353`
- Test: `tests/fiber.ss`

**Interfaces:**
- Consumes the run-context terminal result/error fields from Task 2.
- Produces `run-fibers` behavior that returns all init values and raises the original init or
  child failure after worker threads have joined.

- [ ] **Step 1: Write the failing propagation assertions**

  Extend `fiber-init-error-reaches-caller` to compare `condition-message` and irritants, and
  extend `fiber-multiple-values-preserved` to assert a two-value `call-with-values` result.
  Add a child-error case that verifies the context stops and the caller receives a condition
  rather than parsing scheduler text output.

- [ ] **Step 2: Store conditions without printing from workers**

  Change `wrap-thunk` and init handling to record the first condition in the context and request
  shutdown. Worker threads return through their scheduler continuation without printing. If a
  later condition arrives after shutdown has begun, preserve the first condition and discard the
  later one.

- [ ] **Step 3: Preserve multiple init values**

  Capture init values with `call-with-values` into a list or vector owned by the context, then
  return them with `apply values` after all workers have joined. Ensure the normal single-value
  case remains compatible with existing callers.

- [ ] **Step 4: Verify and commit**

  Run `cd tests && make test fiber.ss`, then:

  ```sh
  git add src/cspp/concurrency/fiber.ss tests/fiber.ss
  git commit -m "fiber: propagate context errors and values"
  ```

### Task 4: Make Event Construction Immutable and Cancellation-Safe

**Files:**
- Modify: `src/cspp/concurrency/fiber.ss:384-545, 728-1000`
- Test: `tests/fiber.ss`

**Interfaces:**
- Consumes the existing `event`, `base-event`, `select-event`, channel queue, and abox state
  representations.
- Produces immutable `event-wrap` and `event-select` values, a truly disabled `event-never`,
  and a synchronization transaction with idempotent cancellation cleanup.

- [ ] **Step 1: Write event semantic tests**

  Run the Task 1 event cases and add an assertion that `event-select` with zero arguments has
  the same blocking behavior as `event-never`. Add nested selection and repeated wrapping cases
  to ensure flattening does not mutate either input event.

- [ ] **Step 2: Correct disabled and empty events**

  Represent `event-never` with a `try-fn` that returns `#f` and a block registration that keeps
  the fiber parked without invoking its resume procedure. Make the empty selection return an
  event value, not the `event-never` constructor procedure.

- [ ] **Step 3: Make wrapping persistent**

  Always allocate a new `select-event` containing newly wrapped base events. Preserve existing
  wrapper composition order and leave the source event graph unchanged.

- [ ] **Step 4: Add transaction cleanup hooks**

  Extend the private event registration protocol so every blocked registration has an idempotent
  cleanup action. A successful registration marks the shared transaction committed, resumes the
  winner once, and runs cleanup for all losing registrations. A canceled fiber marks the
  transaction canceled and runs the same cleanup path. Channel and network wait queues must
  ignore canceled registrations and compact them during the operation that observes them.

- [ ] **Step 5: Verify channel/select stress**

  Run `cd tests && make test fiber.ss net-http-fiber.ss net-lws-stress.ss`. The select cleanup
  case must complete repeatedly without a watchdog timeout or unbounded stale waiter retention.

- [ ] **Step 6: Commit event changes**

  ```sh
  git add src/cspp/concurrency/fiber.ss tests/fiber.ss
  git commit -m "fiber: make event selection cancellation-safe"
  ```

### Task 5: Repair Fiber Mutex and Cvar Atomicity

**Files:**
- Modify: `src/cspp/concurrency/fiber.ss:1004-1165`
- Test: `tests/fiber.ss`

**Interfaces:**
- Consumes the event transaction cleanup protocol from Task 4.
- Produces owner-checked `fiber-mutex` operations, exception-safe `with-fiber-mutex`, and cvar
  wait/signal/broadcast operations with no release-to-publication race.

- [ ] **Step 1: Write the failing ownership, unwind, and cvar tests**

  Ensure the foreign-release test expects an error condition. Make the unwind test catch an
  exception in the protected body, then acquire the same mutex from another fiber. Repeat the
  cvar handoff test at least 10,000 times with a bounded watchdog.

- [ ] **Step 2: Track mutex ownership by fiber task token**

  Give each wrapped fiber task a private identity token and make the current task token
  available to mutex operations. Store the owner token on acquisition, reject release by a
  different token, and transfer ownership directly to a queued waiter when releasing.

  Use the documented Chez mutex rule that `condition-wait` requires exactly one ownership
  level. The fiber mutex and cvar queues must be separate from the scheduler park mutex, and no
  cvar path may call `condition-wait` while recursively owning its park mutex.

- [ ] **Step 3: Make `with-fiber-mutex` exception-safe**

  Do not use ordinary `dynamic-wind` around a fiber mutex: Chez runs its exit and entry thunks
  when a continuation crosses the dynamic extent, and a fiber yield would release and reacquire
  the mutex during normal scheduling. Use an explicit exception cleanup mechanism that releases
  the mutex exactly once on normal return or an exception while preserving ownership across
  yields. Add the required public documentation above `fiber-cvar-wait`, `fiber-cvar-signal`,
  and `fiber-cvar-broadcast` while changing their implementations.

- [ ] **Step 4: Publish cvar waiters before releasing the mutex**

  Under the cvar wait-queue lock, append the waiter registration and then release the associated
  fiber mutex before returning control to the scheduler. `fiber-cvar-signal` and broadcast acquire
  the same wait-queue lock before removing waiters. A wake continuation reacquires the mutex
  before returning to the waiting predicate loop. This mirrors CML's atomic cvar registration.

- [ ] **Step 5: Verify and commit**

  Run `cd tests && make test fiber.ss concurrency.ss`, then:

  ```sh
  git add src/cspp/concurrency/fiber.ss tests/fiber.ss
  git commit -m "fiber: fix mutex ownership and cvar wakeups"
  ```

### Task 6: Implement Scheduler-Owned Timer Events

**Files:**
- Modify: `src/cspp/concurrency/fiber.ss:1168-1214` and the run-context scheduler code from Task 2
- Test: `tests/fiber.ss`

**Interfaces:**
- Consumes the run-context park condition and event cleanup protocol.
- Produces working `milisleep-event`, `sleep-event`, `fiber-milisleep`, and `fiber-sleep`
  procedures with minimum-delay semantics. These four procedures become documented exports;
  `time-event` remains private unless a separate public contract is added.

- [ ] **Step 1: Write timer tests**

  Assert zero-duration sleep completes immediately, positive durations do not complete before
  their deadline within a small tolerance, and many sleeping fibers share scheduler workers.
  Include a cancellation/shutdown case so expired or abandoned timer registrations are removed.

- [ ] **Step 2: Add a monotonic timer wait queue to the run context**

  Store timer registrations ordered by monotonic deadline, with transaction flag, resume
  continuation, and cleanup procedure. When a scheduler parks, use the earliest deadline to
  compute a `time-duration` via `time-difference`; pass that duration to `condition-wait`, never
  the `time-monotonic` deadline itself. Before and after waiting, move all expired registrations
  to runnable tasks under the park mutex. Keep this wall-clock timer queue separate from the
  instruction-count `set-timer` preemption mechanism.

- [ ] **Step 3: Implement the event constructors**

  `try-fn` returns a thunk only after the deadline is reached. `block-fn` registers the timer
  and returns control to the scheduler. Cleanup marks the timer canceled; a late expiry must not
  resume a committed or canceled transaction.

- [ ] **Step 4: Verify and commit**

  Run `cd tests && make test fiber.ss net-http-fiber.ss`, then:

  ```sh
  git add src/cspp/concurrency/fiber.ss tests/fiber.ss
  git commit -m "fiber: add scheduler timer events"
  ```

### Task 7: Record the IO Design Boundary

**Files:**
- Modify: `docs/superpowers/plans/2026-10-14-fiber-scheduler-correctness.md`

**Interfaces:**
- Consumes existing `cspp net poll`, `poll-target`, `net-operation`, and fiber event APIs.
- Produces an explicit implementation boundary for a future IO plan; this task adds no runtime
  IO code and no new public API.

- [ ] **Step 1: Specify the readiness path**

  Document that a future run context should own one readiness poller and a wakeup descriptor.
  Socket and pipe operations should return pending poll targets, register one cancellable event
  waiter per descriptor/mode, and wake fibers when `poll` reports readiness. Enqueue publication
  and poller wakeup must use the run-context park mutex to avoid a readiness-to-sleep race. A
  foreign poll call must be declared `__collect_safe` or explicitly deactivate the OS thread;
  no Scheme object or continuation may be accessed while deactivated. Reactivate before
  constructing Scheme results or publishing a fiber task.

- [ ] **Step 2: Specify the blocking-file path**

  Document that regular files and buffered Chez ports cannot use descriptor readiness as a
  general completion mechanism. Their blocking operations should use a bounded worker pool and
  resume fibers through the same transaction/wakeup protocol. Port ownership and close-on-error
  rules must be defined before implementation.

- [ ] **Step 3: Compare with CML and Go**

  Keep the design rationale in this plan: CML's atomic event registration and cleanup prevent
  lost wakeups; its scheduler-owned timeout and IO queues avoid per-fiber OS waits; Go's runtime
  parks idle workers in a netpoller and wakes only runnable goroutines. Record that the existing
  `net-operation` driver currently advances jobs with four `milisleep 1` workers and should be
  integrated with the future poller rather than expanded in this scheduler fix.

### Task 8: Full Verification and Review Checkpoint

**Files:**
- Test: `tests/fiber.ss`, `tests/net-http-fiber.ss`, `tests/net-lws-stress.ss`,
  `tests/concurrency.ss`
- Build artifacts: generated by the repository build; do not commit generated output unless the
  repository's normal build requires it.

**Interfaces:**
- Consumes all implementation tasks above.
- Produces fresh build and test evidence for review.

- [ ] **Step 1: Check Scheme syntax and parentheses**

  From the worktree root, run this Chez reader check against every changed Scheme file, then
  compile the changed libraries through the normal build:

  ```sh
  .chezscheme-install/bin/scheme --script /dev/stdin \
    src/cspp/concurrency/fiber.ss tests/fiber.ss <<'EOF'
  (import (chezscheme))
  (for-each
   (lambda (path)
     (call-with-input-file path
       (lambda (port)
         (let loop ()
           (unless (eof-object? (read port)) (loop))))))
   (command-line-arguments))
  EOF
  ```

- [ ] **Step 2: Run the required clean build**

  ```sh
  make clean && make
  ```

  Expected result: exit status 0 and no compilation errors.

- [ ] **Step 3: Run focused fiber and network tests**

  ```sh
  cd tests
  make test fiber.ss concurrency.ss net-http-fiber.ss net-lws-stress.ss
  ```

  Expected result: all selected `mat` cases pass and there is no unexpected stdout or stderr.

- [ ] **Step 4: Run the complete test suite**

  ```sh
  make test
  ```

  Record any environment-gated optional-library skips separately from failures. Do not claim
  completion if a non-optional test fails.

- [ ] **Step 5: Review the final diff and commit**

  Confirm public documentation, `pcheck` checks, no unclosed ports, no unbalanced parentheses,
  and no debug output remain. Then commit the implementation using subsystem-prefixed messages
  such as `fiber: ...`.


## Future IO Integration Boundary (Task 7)

This scheduler change does not implement socket, pipe, or regular-file IO. A future IO
implementation should give each run context one readiness poller and a wakeup descriptor.
Nonblocking socket and pipe operations should expose their pending `poll-target` values through
`net-operation-poll-targets`. Register one cancellable event waiter per descriptor and requested
mode; readiness from `(cspp net poll)` makes that waiter eligible to resume and retry its
operation. Readiness is permission to retry, not proof that the operation has completed.

Publishing a waiter, publishing runnable work, and notifying the poller's wakeup descriptor
must follow the run-context park-mutex protocol. Before blocking, the poller must inspect the
published-work and stop predicates under that mutex, and drain or acknowledge wakeup notifications
without losing a concurrent publication. Event commit, cancellation, descriptor close, and context
shutdown must detach the corresponding readiness registrations exactly once.

A blocking foreign poll call must use a `__collect_safe` foreign-procedure declaration or explicitly
deactivate the calling OS thread. Native arguments must remain valid for the entire call. While
deactivated, code must not access Scheme objects or continuations. Reactivate the OS thread before
constructing Scheme readiness results or publishing resumed fiber tasks. The current poll binding
must be audited for these requirements before it becomes the scheduler's indefinite wait.

Regular files and buffered Chez ports require a different completion path: descriptor readiness
cannot generally represent completion of their blocking operations. Use a bounded OS worker pool
for those operations and deliver their results through the same event transaction and scheduler
wakeup protocol. A future plan must first define which worker owns each port, whether concurrent
operations are allowed, and who closes the port after normal completion, cancellation, or error.
Cancellation must not close a port still used by a worker without a defined ownership handoff.

Concurrent ML motivates atomic event polling, registration, commit, and losing-branch cleanup:
that protocol prevents rendezvous and condition-variable lost wakeups. Its scheduler-owned timeout
and IO queues also avoid assigning an OS wait to every blocked fiber. Go's runtime similarly parks
idle workers, integrates descriptor readiness with its netpoller, and schedules goroutines whose
operations can make progress. These are design references, not a requirement to duplicate either
runtime's implementation.

The existing `net-operation` event driver currently advances jobs with at most four OS workers,
each using `milisleep 1` between passes. Keep that compatibility path during this scheduler fix.
A future readiness poller should consume operation poll targets and deadlines and integrate or
replace that polling driver; adding more polling workers is not the intended IO architecture.
