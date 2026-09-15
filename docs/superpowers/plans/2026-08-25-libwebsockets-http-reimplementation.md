# Libwebsockets HTTP/1.x and HTTP/2 Reimplementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Replace Chezpp HTTP/1.x and HTTP/2 with a fresh libwebsockets backend that preserves the high-level `net/http` API, supports blocking and nonblocking requests, and integrates with the future fiber scheduler.

**Architecture:** Chezpp owns a serialized reactor for each libwebsockets context. LWS is driven in external-poll mode through `lws_service_fd()`; native callbacks copy protocol events into bounded queues and never invoke Scheme. Blocking APIs wait on reactor-owned operation state, nonblocking APIs return operations, and fibers suspend on operation events whose completions are posted back to the scheduler.

**Tech Stack:** Chez Scheme, Chezpp `net-operation` and `poll`, optional-library loading, libwebsockets HTTP client/server/H2 roles, POSIX poll and wakeup descriptors, and the existing fiber event system.

## Decisions and invariants

- Preserve the high-level records and procedures exported by `chezpp net http`: request/response records, body sources/sinks, cookies, redirects, auth, proxy policy, pooling, `http-*` blocking helpers, `http-*/nonblocking` operations, and server handler APIs.
- Remove the low-level memory-fed `(chezpp net http2)` API (`http2-open`, `http2-send`, `http2-receive`, `http2-next-event`, and related procedures). HTTP/2 becomes an internal transport selected by `net/http`.
- Do not call `lws_service()` as a hidden HTTP loop. The Chezpp reactor polls descriptors and calls `lws_service_fd()`.
- Every LWS context has one owner. Fibers and scheduler workers submit commands; they never mutate LWS state directly.
- Native LWS callbacks copy data into native-owned queues and return without calling Scheme, user body sources, or user body sinks.
- A nonblocking operation's `advance` never waits. Its pending readiness target is the reactor wakeup descriptor; the reactor services the actual LWS descriptors.
- HTTP/1.x uses one active transaction per pooled connection. HTTP/2 multiplexes logical streams over one physical connection and fans one readiness event out to affected operations.
- Cancellation and timeout are reactor commands. Late callbacks are ignored using operation generations and lifecycle checks.
- The default deployment uses one dedicated reactor thread per LWS context. The operation/fiber interfaces must not depend on that placement, so a scheduler-owned reactor can be added later.

## File map

Create:

- `chezpp/c/net/lws_loader.h`, `chezpp/c/net/lws_loader.c`: optional symbol loading, version checks, and capability bits.
- `chezpp/c/net/lws_http.h`, `chezpp/c/net/lws_http.c`: native context, external-poll registration, callback event queues, request/stream handles, bounded body buffers, and FFI entry points.
- `chezpp/net/lws/ffi.ss`: private FFI bindings and validation.
- `chezpp/net/lws/reactor.ss`: reactor ownership, command queue, wakeup, polling, event publication, and shutdown.
- `chezpp/net/lws/http1.ss`: HTTP/1.x client state, uploads/downloads, redirects, and pooling.
- `chezpp/net/lws/http2.ss`: H2 stream scheduling, flow control, reset, GOAWAY, and stream routing.
- `chezpp/net/lws/server.ss`: logical request queues and HTTP/1.x/H2 server dispatch.
- `chezpp/concurrency/fiber-net.ss`: network-operation events and fiber waiter lifecycle.
- `tests/net-lws-reactor.ss`, `tests/net-lws-server.ss`, `tests/net-http-fiber.ss`: focused tests.

Rewrite or update:

- `chezpp/net/http.ss`: retain public API/data records; delegate all transport work to the new modules.
- `chezpp/net.ss`: stop exporting the low-level HTTP/2 library.
- `chezpp/net/ffi.ss`: remove old memory-session bindings and expose only required private native bindings.
- `chezpp/concurrency/fiber.ss`: add scheduler-safe cross-thread task posting and wakeup.
- `tests/net-http.ss`, `tests/net-loader.ss`, `tests/net-operation.ss`, `tests/Makefile`, and public API checks.

## Task 1: Lock the public API boundary

**Files:** `chezpp/net.ss`, `chezpp/net/http.ss`, `chezpp/net/http2.ss`, `tests/net-loader.ss`, `tests/net-core.ss`, create `tests/net-http-contract.ss`.

- [ ] **Step 1: Add contract tests before implementation.** Assert that `http-open`, `http-send`, `http-send/nonblocking`, `http-download/nonblocking`, `http-listen`, and `http-serve-loop` remain procedures. Evaluate `http2-open` in the `(chezpp net)` environment and assert that lookup fails.
- [ ] **Step 2: Run the contract test.** Run `make clean && make` followed by `make -C tests test-some TEST=net-http-contract`. Record the repository's existing `chezpp parser elf` missing-library failure separately from the expected API red result.
- [ ] **Step 3: Remove low-level exports.** Delete the `(chezpp net http2)` aggregate import/export. Move any internal references to `net/lws/http2.ss`; remove or delete the old `net/http2.ss` memory-session implementation after references are gone.
- [ ] **Step 4: Preserve signatures.** Keep all high-level `net/http` record fields and procedure arities unchanged. Document that `http-response-version` reports `h1` or `h2` while transport selection is internal.
- [ ] **Step 5: Update test and documentation lists.** Remove low-level HTTP/2 MATs, keep H2 behavior tests at the HTTP API level, add the contract test to `tests/Makefile`, and require the public API checker to reject `http2-open`.
- [ ] **Step 6: Verify and commit.** Run `make -C tests test-some TEST=net-http-contract` and `git diff --check`; commit as `net: define libwebsockets HTTP API boundary`.

## Task 2: Add optional libwebsockets loading

**Files:** create `chezpp/c/net/lws_loader.h`, `chezpp/c/net/lws_loader.c`; modify `chezpp/c/optional_library.[ch]`, `chezpp/net/lws/ffi.ss`, `chezpp/net/ffi.ss`, `tests/net-loader.ss`, `tests/optional-library-linkage.sh`.

- [ ] **Step 1: Define capability bits.** Use `CHEZPP_LWS_CAP_HTTP1`, `CHEZPP_LWS_CAP_HTTP2`, `CHEZPP_LWS_CAP_TLS`, `CHEZPP_LWS_CAP_SOCKS5`, and `CHEZPP_LWS_CAP_EXTERNAL_POLL`. Expose `chezpp_lws_ensure_loaded()`, `chezpp_lws_capabilities()`, and `chezpp_lws_error()`.
- [ ] **Step 2: Load required symbols.** Dynamically load context creation/destruction, client connect, `lws_service_fd`, `lws_service_adjust_timeout`, `lws_cancel_service`, `lws_callback_on_writable`, `lws_get_socket_fd`, `lws_http_client_read`, `lws_client_http_body_pending`, header accessors, opaque-user-data accessors, and external-poll callbacks. Load H2 and SOCKS5 capabilities conditionally.
- [ ] **Step 3: Bind a validated status vector.** Add a private Scheme binding returning `#(available? capability-mask version error)`; reject malformed vectors and produce a network error naming the missing capability.
- [ ] **Step 4: Test optional behavior.** Test available HTTP/1/external-poll capability, deterministic unavailable-library errors, and absence of a direct `-lwebsockets` dependency in `libchezpp.so`.
- [ ] **Step 5: Verify and commit.** Run `make clean && make`, `make -C tests test-some TEST=net-loader`, `make -C tests test-optional-library-loader`, and `make -C tests test-optional-library-linkage`; commit as `net: add optional libwebsockets HTTP loader`.

## Task 3: Implement the native nonblocking LWS adapter

**Files:** create `chezpp/c/net/lws_http.[ch]`; modify `chezpp/net/lws/ffi.ss`; test in `tests/net-lws-reactor.ss`.

- [ ] **Step 1: Define ownership records.** Add native `lws_http_context`, `lws_http_connection`, `lws_http_stream`, `lws_http_event`, and `lws_poll_entry` records. The context owns the LWS context, vhosts, wakeup pipe, poll entries, event queue, and live-handle count.
- [ ] **Step 2: Define copied event tags.** Use tags for poll add/change/delete, connected, headers, readable, writable, complete, closed, failed, reset, and GOAWAY. Every event carries context/connection/stream identity, generation, status/error metadata, and copied payload bytes.
- [ ] **Step 3: Implement callbacks.** Translate HTTP client/server bind, established, header, readable, writable, completed, close, error, H2 reset, and GOAWAY callbacks. External-poll callbacks update the native poll-entry list. Callbacks must not call Scheme or user code.
- [ ] **Step 4: Bound queues and flow control.** Limit queued body bytes per stream and per context. Stop draining LWS when the bound is reached; emit a readable event only after Scheme acknowledges consumed bytes. Convert allocation failures into one terminal stream/connection failure.
- [ ] **Step 5: Expose nonblocking FFI operations.** Implement `lws-context-open/close`, `lws-context-wakeup-fd`, `lws-context-poll-snapshot`, `lws-context-service-fd`, `lws-context-next-event`, `lws-context-timeout-ms`, `lws-context-wakeup`, client start/body submit/body drain, server request dequeue/response submit, stream cancel, and body-consumed acknowledgement.
- [ ] **Step 6: Add native fake-event seams.** Inject poll changes, writable events, headers, body chunks, completion, failure, reset, and GOAWAY. Test ordering, queue bounds, generation filtering, and cleanup without a live peer.
- [ ] **Step 7: Verify and commit.** Run `make clean && make` and `make -C tests test-some TEST=net-lws-reactor`; commit as `net: add nonblocking libwebsockets HTTP adapter`.

## Task 4: Build the serialized Chezpp reactor

**Files:** create `chezpp/net/lws/reactor.ss`; modify `chezpp/net/lws/ffi.ss`, `chezpp/net/poll.ss`, `chezpp/net/operation.ss`; test `tests/net-lws-reactor.ss`.

- [ ] **Step 1: Define reactor records.** Store native context, owner thread id, command mutex/condition variable, command queue, waiter table, current poll snapshot, lifecycle, and shutdown condition. Reject owner-only mutation from non-owner callers.
- [ ] **Step 2: Implement command submission.** Support `start`, `submit-body`, `consume-body`, `cancel`, `close-stream`, `register-waiter`, and `unregister-waiter`. Enqueue under a lock and wake the native context immediately.
- [ ] **Step 3: Implement the loop.** Drain commands; service already-ready native events; compute targets and timeout; poll LWS descriptors plus wakeup fd; call `lws-context-service-fd` for each ready descriptor; drain native events; publish operation transitions; repeat until stop.
- [ ] **Step 4: Account for LWS timers.** Call `lws_service_adjust_timeout` when computing the poll timeout. Refresh the snapshot after each service pass because LWS may add/delete/change descriptors.
- [ ] **Step 5: Adapt to `net-operation`.** Add a private constructor whose `advance` observes operation state and whose `cancel` submits a reactor command. Pending targets contain the reactor wakeup fd and requested read/write events.
- [ ] **Step 6: Implement shutdown.** Reject new commands, cancel pending operations, wake the reactor, service close callbacks until handles close or a bounded grace deadline expires, close wakeup descriptors, destroy native context, and make repeated shutdown inert.
- [ ] **Step 7: Test and commit.** Test wakeups, poll add/change/delete, forced LWS timeout service, completion fan-out, cancellation during event processing, reactor failure, and double shutdown. Commit as `net: add serialized libwebsockets reactor`.

## Task 5: Implement HTTP/1.x client behavior

**Files:** create `chezpp/net/lws/http1.ss`; modify `chezpp/net/http.ss` and `chezpp/net/lws/reactor.ss`; test `tests/net-http.ss`.

- [ ] **Step 1: Define request and connection state.** Track method, URI, normalized headers, source/sink, response metadata, connection, lifecycle, deadline, redirect count, generation, and waiters. Track pool origin, LWS handle, idle timestamp, active transaction, and close state.
- [ ] **Step 2: Normalize and start requests.** Apply method/URI/host, proxy, auth, cookie, content length, chunked transfer, TLS/ALPN, and timeout policy before submitting a reactor start command. Set LWS no-follow-redirect behavior so Chezpp owns redirect policy.
- [ ] **Step 3: Stream request bodies.** On writable events, pull bounded chunks from the body source, submit them through LWS, and mark final only on EOF. Source exceptions fail only this request and close/cancel its connection as required.
- [ ] **Step 4: Stream response bodies.** Convert headers to Chezpp response metadata. Drain bounded chunks into memory or a sink; acknowledge consumption to resume LWS receive flow. Sink exceptions fail only the owning request.
- [ ] **Step 5: Complete and pool.** Finalize sinks once, build `http-response`, apply redirect rules while preserving the absolute deadline, and return reusable connections to the idle pool only when protocol state is clean.
- [ ] **Step 6: Route public APIs.** Make `http-send/nonblocking` create the operation; make `http-send` wait on it; delegate get/head/post/put/delete/download/upload to those paths without duplicate transport logic.
- [ ] **Step 7: Test.** Cover cleartext/TLS, fixed and chunked request/response bodies, segmented reads, streaming upload/download, redirects, cookies, auth, proxy, pooling, timeout, cancellation, source/sink failure, EOF, and nonblocking readiness.
- [ ] **Step 8: Verify and commit.** Run `make -C tests test-some TEST=net-http`; commit as `net: implement libwebsockets HTTP/1 client`.

## Task 6: Implement HTTP/2 client multiplexing

**Files:** create `chezpp/net/lws/http2.ss`; modify `chezpp/net/lws/reactor.ss`, `chezpp/net/http.ss`, and TLS ALPN integration if required; test `tests/net-http.ss`.

- [ ] **Step 1: Define transport and stream records.** Transport state includes one LWS connection, origin, queued requests, active stream table, peer capacity, receive-buffer accounting, lifecycle, and GOAWAY state. Stream state includes logical request, LWS identity, headers, sink, byte counters, lifecycle, deadline, and generation.
- [ ] **Step 2: Select H2 explicitly.** Configure ALPN for `h2`; support cleartext prior knowledge; fail if `h2` was required but HTTP/1.1 was negotiated; never silently downgrade.
- [ ] **Step 3: Schedule by peer capacity.** Queue requests FIFO and start streams only when the connection is established, peer capacity allows them, and GOAWAY is not draining. Promote queued requests after completion or cancellation.
- [ ] **Step 4: Route multiplexed events.** Require every native event to carry connection and logical stream identity. Route headers, body, writable, reset, completion, and failure to one stream; fail all streams on connection-level failure while preserving independent cleanup.
- [ ] **Step 5: Implement flow control.** Produce request data only on writable notifications. Drain response chunks into bounded sinks and acknowledge consumed bytes through LWS receive flow. Backpressure one stream without blocking siblings.
- [ ] **Step 6: Implement cancellation and GOAWAY.** Mark cancellation/timeout before submitting reset; discard late callbacks by generation; stop new streams after GOAWAY; allow accepted streams to finish; fail queued work deterministically when the connection closes.
- [ ] **Step 7: Test.** Cover TLS ALPN, cleartext prior knowledge, ten concurrent streams, all wait orders, peer capacity, slow sink isolation, upload flow control, active/queued cancellation, timeout, reset, GOAWAY, final response before close, EOF, and connection reuse.
- [ ] **Step 8: Verify and commit.** Run `make -C tests test-some TEST=net-http`; commit as `net: implement libwebsockets HTTP/2 client`.

## Task 7: Implement HTTP/1.x and HTTP/2 servers

**Files:** create `chezpp/net/lws/server.ss`, `tests/net-lws-server.ss`; modify `chezpp/net/http.ss`, `chezpp/net/lws/ffi.ss`, `tests/net-http.ss`.

- [ ] **Step 1: Define logical request records.** An accepted value represents a logical HTTP request/stream, not a raw socket. Store method, URI, headers, body stream, protocol version, physical connection, logical stream, lifecycle, and response state.
- [ ] **Step 2: Queue callback events.** Queue a request only after headers are complete; deliver body chunks incrementally; queue writable events for response output; apply receive flow control if a handler is not consuming.
- [ ] **Step 3: Dispatch handlers outside locks.** `http-serve` dequeues one logical request, invokes the handler without reactor locks, and submits the response through a reactor command. Pull response bodies only on writable events.
- [ ] **Step 4: Preserve server APIs.** Implement listen, close, accept/nonblocking, handler registration, connection close, request reads, response writes, and serve-loop over the reactor. Closing stops new accepts and drains/cancels existing logical requests deterministically.
- [ ] **Step 5: Test and commit.** Cover HTTP/1 keep-alive, chunking, partial clients, multiple requests per connection, H2 concurrent streams, response backpressure, handler exceptions, close with live streams, and accept cancellation. Commit as `net: implement libwebsockets HTTP server`.

## Task 8: Integrate network operations with fibers

**Files:** create `chezpp/concurrency/fiber-net.ss`, `tests/net-http-fiber.ss`; modify `chezpp/concurrency/fiber.ss`, `chezpp/concurrency.ss`, `chezpp/net/operation.ss`, and `chezpp/net/lws/reactor.ss`.

- [ ] **Step 1: Add scheduler-safe posting.** Implement an internal `fiber-post!` callable from any reactor thread. It selects a live scheduler without reading `current-scheduler` on the posting thread, enqueues under scheduler locks, and signals a scheduler condition/wakeup descriptor.
- [ ] **Step 2: Add `net-operation-event`.** Its try function returns a result thunk only for terminal operations. Its block function registers one reactor waiter. Reactor completion invokes `fiber-post!`, and the posted task claims an atomic waiter flag before calling the event resume function.
- [ ] **Step 3: Make HTTP blocking helpers fiber-aware.** Inside a fiber, wait with `perform-operation (net-operation-event operation)`; outside fibers, wait on the reactor completion condition. Both paths read the same terminal result/condition.
- [ ] **Step 4: Handle cancellation and fiber exit.** Unregister waiters and submit cancellation when a fiber is cancelled or exits. Ignore late completion after the waiter claim is no longer pending; never enqueue a dead continuation.
- [ ] **Step 5: Test.** Run many fibers issuing HTTP/1 and H2 requests, slow/fast completion, cancellation, timeout, reactor-thread completion, idle scheduler wakeup, no Scheme callback on the reactor thread, and shutdown with waiting fibers.
- [ ] **Step 6: Verify and commit.** Run `make -C tests test-some TEST=net-http-fiber net-operation net-http`; commit as `net: integrate LWS operations with fibers`.

## Task 9: Remove obsolete adapters and update examples

**Files:** modify `chezpp/net.ss`, `chezpp/net/ffi.ss`, `chezpp/net/http.ss`, examples, docs, and tests; delete old HTTP/2 adapter files after references are gone.

- [ ] **Step 1: Prove no old imports remain.** Run `rg -n '\\(chezpp net http2\\)|http2-(open|send|receive|next-event)' chezpp tests examples`; remove every result.
- [ ] **Step 2: Remove direct nghttp2 adapter code.** Delete the old Chezpp memory-session implementation and its loader only when no other feature uses it. Keep nghttp2 as an indirect libwebsockets build dependency and do not add a direct link.
- [ ] **Step 3: Update examples.** Make strict HTTP/2 downloads select `h2` through the high-level client policy, verify `http-response-version`, and use ordinary blocking/nonblocking HTTP APIs.
- [ ] **Step 4: Run API/documentation audits.** Run the public API checker and verify every retained export has docs and every removed low-level binding is absent.
- [ ] **Step 5: Commit.** Commit as `net: switch HTTP implementation to libwebsockets`.

## Task 10: Verification gates

- [ ] **Step 1: Clean build.** Run `make clean && make`; resolve the known baseline `chezpp parser elf` dependency before claiming a successful build.
- [ ] **Step 2: Stress focused tests.** Run ten repetitions of `timeout 60s make -C tests test-some TEST='net-http net-http-fiber net-lws-reactor net-lws-server'`.
- [ ] **Step 3: Run the complete network suite.** Include net-operation, net-core, net-http, reactor/server/fiber tests, transfer, TLS, websocket, gRPC, and documentation tests.
- [ ] **Step 4: Run optional-loader/linkage tests.** Run loader, optional-library linkage, optional-hash linkage, and `ldd libchezpp.so`; confirm libwebsockets is optional and no direct nghttp2 dependency remains.
- [ ] **Step 5: Audit lifetimes and ownership.** Verify every native event is freed on success/failure/cancel/close, every waiter unregisters once, every context has one owner, no callback calls Scheme, no operation advance polls, and every deadline remains absolute.
- [ ] **Step 6: Run repository hygiene checks.** Run `git diff --check`, protobuf generation/output comparison, public API docs, and changed-Scheme balance checks.
- [ ] **Step 7: Record evidence.** Update the plan only with concrete command output and keep implementation commits separate from unrelated worktree changes.

## Required interaction contract

The implementation must preserve this sequence:

~~~text
fiber calls http-*-nonblocking
  -> operation command is queued and reactor wakeup is signalled
  -> operation is pending with reactor wakeup poll target
  -> fiber performs net-operation-event and suspends
  -> reactor drains command and starts LWS request/stream
  -> LWS callback copies an event and returns
  -> reactor drains event and updates operation state
  -> reactor calls fiber-post!, never a continuation directly
  -> scheduler runs the resumed fiber
  -> fiber reads result or raises the terminal condition
~~~

A blocking call outside fibers waits on reactor completion. A blocking call inside a fiber uses the same operation but suspends through the network event. Cancellation marks the operation first, queues an LWS reset/close, wakes the reactor, and rejects late callbacks by generation.

## Self-review

- Tasks 1, 5, 6, and 7 cover every retained high-level blocking/nonblocking HTTP API.
- Tasks 1 and 9 remove the low-level memory-fed HTTP/2 API.
- Tasks 2 through 4 cover optional loading, external polling, callback copying, wakeups, timers, and native ownership.
- Tasks 5 and 6 cover HTTP/1 streaming/pooling and HTTP/2 multiplexing/flow control.
- Task 7 covers logical server requests for both protocol versions.
- Task 8 covers scheduler-safe fiber suspension, completion, cancellation, and shutdown.
- Task 10 covers build, stress, linkage, lifecycle, documentation, and hygiene gates.
- No task depends on the existing HTTP parser or existing HTTP/2 memory adapter.
- The baseline build caveat is explicit; no passing-test claim may be made until it is resolved.
