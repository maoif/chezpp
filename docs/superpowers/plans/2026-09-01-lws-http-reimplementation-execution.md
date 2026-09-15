# Libwebsockets HTTP Reimplementation Execution Plan

> This is the consolidated plan. It supersedes
> `2026-08-25-libwebsockets-http-reimplementation.md`; that document's gate checklist
> and invariants are incorporated here. Do not use the older file as a second ledger.

## Consolidated Execution Flow

Every gate requires a focused red MAT, the smallest green implementation, the gate
verification command, parenthesis and documentation checks, `git diff --check`, and a
progress-ledger entry. Compilation alone cannot complete a gate.

| Gate | Scope | Exit evidence |
| --- | --- | --- |
| 0 | Baseline, versions, clean build, focused harness | Reproducible status and command results |
| 1 | Public HTTP records and policy inputs | Named records and malformed/replay tests |
| 2 | Optional LWS loader and capability probe | Validated status vector and unavailable-library test |
| 3 | Native context, connection, stream, event, and signal records | Network-WSI identity and generation tests |
| 4 | Copied metadata and bounded queues | Explicit overflow and body-consumption tests |
| 5 | Serialized reactor owner, commands, wakeups, and waiters | Ordering, command-result, poll, and shutdown tests |
| 6 | Destructive operation-event delivery | Bounded drain, cancellation, fan-out, and stale-event tests |
| 7 | HTTP/1 reducer | Framing, streaming, limits, timeout, and cleanup tests |
| 8 | Shared public blocking/nonblocking operation state machine | Redirect, auth, cookie, snapshot, and replay tests |
| 9 | HTTP/1 physical reuse decision | LWS evidence plus live accept-count proof, or explicit non-reuse decision |
| 10 | Independent HTTP/2 transport and reducer | ALPN, stream, flow-control, GOAWAY, and failure tests |
| 11 | Server logical request layer | Raw-socket HTTP/1 tests; H2 only with a live fixture |
| 12 | Fiber runtime contracts | Red/green MAT and bounded stress per contract |
| 13 | Fiber network-operation integration | Cancellation, timeout, shutdown, and cross-thread tests |
| 14 | Removal, audits, stress, and release verification | Complete suite, linkage, docs, and hygiene evidence |

Historical task labels map as follows: foundation to Gates 0-1 and 8; loading to Gate 2;
native adapter to Gates 3-4; reactor to Gates 5-6; HTTP/1 to Gates 7-9; HTTP/2 to Gate 10;
server to Gate 11; fiber runtime to Gate 12; fiber networking to Gate 13; cleanup to Gate 14.

## Current Worktree Status

### Authoritative Status Snapshot: 2026-09-15

The historical task checkboxes below remain for traceability. This snapshot is the current
assessment and supersedes stale unchecked lines when they conflict with later implementation
and test evidence.

Implemented and verified:

- Gates 0-8 are implemented in the current LWS client/reactor stack, including the shared
  blocking/nonblocking operation state machine, HTTP/1 reduction, redirects, auth/cookies,
  uploads/downloads, bounded events, cancellation, and cleanup paths.
- Gate 10 is implemented as a direct LWS HTTP/2 transport with multiplexed streams, local bounds,
  flow control, ALPN capability gating, GOAWAY/WSI failure handling, and stale-generation rejection.
- Gate 11 is implemented as an LWS HTTP/1 and HTTP/2 server layer with streaming, pipelining,
  partial-client handling, handler recovery, backpressure, and response-source cleanup.
- Gates 12-13 are implemented for fiber waiters and network-operation integration, including
  cancellation, timeout, close, scheduler-safe posting, and repeated waiter/operation reuse stress.
- Gate 14 verification is complete: root build, focused aggregates, complete
  network-focused suites, linkage/source audits, documentation/balance checks, stress coverage,
  test-artifact cleanup, and a five-minute error-path soak have all passed. The 2026-09-14
  stress refresh adds repeated H2 cancellation across concurrent streams.

Completion decision:

- The plan is complete for the supported LWS 4.5.8 scope.
- HTTP/1 physical connection reuse is explicitly rejected because the supported LWS API closes
  completed client transactions and provides no supported restart path. This is an intentional
  behavior decision, not an untracked implementation bug.
- Graceful-shutdown deadline support remains excluded by request.
- The obsolete-adapter/example audit is complete. Obsolete adapters and unused HTTP/2 test hooks
  are absent; examples use the high-level HTTP API.
- Implementation and test work is committed in `cc56fc3`, `50ab7f0`, and `f56944a`.
- The configurable `test-http-error-soak` target defaults to five minutes; its default run passed
  seven complete rounds across HTTP/1, HTTP/2, and server error-path suites in 327 seconds.

Completion assessment: complete for the defined scope. No deterministic HTTP/LWS correctness
failure is currently reproduced by the bounded focused, complete network, or error-path soak runs.

### Active Execution: 2026-09-12

The latest implementation review and bounded verification give this current state:

- [x] HTTP/1 breadth: TLS policy, EOF, trailers, cancellation, redirects, proxy transitions,
  downloads, readiness, and producer/sink failures are covered by live or deterministic MATs.
- [x] Server breadth: keep-alive/pipelining, streaming, partial clients, concurrent H2 requests,
  handler failures, backpressure, cancellation, and repeated body accounting are covered.
- [x] H2 peer admission and observable GOAWAY/connection shutdown are covered through LWS logs;
  mixed live HTTP/1 and H2 fiber stress passes.
- [x] Build, documentation, balance, diff hygiene, and the focused six-suite run pass.
- [x] Redirect-limit sink handling now suppresses only intermediate redirect bodies. Terminal
  responses, including the response after the tenth redirect attempt, reach the user sink.
- [x] Server response-source cleanup has explicit precedence coverage: a primary write/closed
  failure is retained when the source closer raises a secondary condition, and the closer runs once.

Ruling: use the existing worktree and the completion decision above, preserving graceful shutdown
as excluded. Historical completion claims are rechecked when new live evidence contradicts them.
Plans, specs, handoffs, and unrelated changes remain outside the implementation commits.

### Verification Refresh: 2026-09-14

- [x] Repeated the bounded HTTP/reactor/server/fiber/H2/stress aggregate four times:
  `timeout 300s make -C tests test-some TEST='net-http net-http-contract net-lws-reactor
  net-lws-server net-http-fiber net-lws-http2 net-lws-stress net-http-stress'`.
  All four runs exited successfully with no MAT diagnostics.
- [x] Repeated the complete network-focused list, including the optional-loader, public API,
  documentation, and transfer suites, under a 600-second timeout. It exited successfully with
  no MAT diagnostics.
- [x] Ran `timeout 120s make -C tests clean` and verified that no `*.stdout`, `*.stderr`,
  `*.out`, `*.build.stdout`, or `*.build.stderr` artifacts remained in `tests/`.
- [x] The previously observed `lwsl_refcount_cx` assertion did not recur in these five aggregate
  runs (four focused and one complete). Treat it as not reproduced in the current evidence set;
  retain teardown diagnostics and continue to avoid weakening fixture checks.

The only remaining items are documented scope decisions: HTTP/1 physical connection reuse remains
rejected for LWS 4.5.8 and graceful-shutdown deadlines remain excluded by request. The five-minute
error-path soak now supplies the longer-duration evidence previously identified as optional.

Verification: `timeout 300s bash -c 'make clean && make'` and
`timeout 120s make -C tests test-some TEST='net-http net-http-contract net-lws-reactor net-lws-server
net-http-fiber net-lws-http2'` passed with no reported MAT failures. Scheme balance, public API docs,
and `git diff --check` also passed. The implementation changes are committed; planning and handoff
artifacts remain intentionally outside those commits.

### Review Findings: 2026-09-12

- No remaining deterministic failures were reproduced in the six-suite focused run after fixing
  sink callback compatibility for both legacy two-element sinks and the new redirect predicate.
- The root build is clean and the focused run is bounded. Generated MAT stdout/stderr files are
  harness artifacts and remain untracked; they are not implementation evidence.
- The historical task checkboxes below are retained for traceability and are not authoritative.
  They still contain superseded unchecked items, including old claims that HTTP/2 delegated through
  HTTP/1 and that TLS ALPN was unavailable. The status tables above take precedence.
- Remaining technical risk is evidence depth rather than a reproduced bug: long-duration stress,
  pooled-reference retention checks, and a complete whole-network run should be repeated before a
  release claim. HTTP/1 physical reuse remains intentionally rejected for LWS 4.5.8.
- The bounded focused command was rerun after the compatibility fix and completed the HTTP, reactor,
  server, fiber, and H2 suites without reported MAT failures. A broad `test-all` run is not a clean
  release signal because many legacy MATs intentionally print expected errors and diagnostics; use
  the network-focused suites plus their captured empty-output files for this plan's gate evidence.
- Review of the implementation found no remaining `TODO`, `FIXME`, or unsupported placeholder in
  the LWS HTTP client, reducer, reactor, transport, or server paths. The only explicit unsupported
  behavior in scope is the documented HTTP/1 physical non-reuse decision and the intentionally
  excluded graceful-shutdown deadline API.

Bounded graceful shutdown is intentionally excluded from this ledger by request. TLS H2 ALPN is
now enabled and capability-gated in the live fixture; it targets IPv4 explicitly and uses a
bounded deadline so unsupported environments skip without hanging.

## Global Constraints

- HTTP/1 and HTTP/2 must both be implemented with libwebsockets. Do not restore the old direct
  socket/parser HTTP/1 transport or the memory-fed nghttp2 HTTP/2 transport as a fallback.
- Treat the current private HTTP transports and C adapter as reference code. Rewrite or delete them
  when their ownership model conflicts with the required design.
- Preserve the exported names, parameter names, arities, return values, documentation blocks, and
  `pcheck` validation of the high-level `(chezpp net http)` API.
- Keep `(chezpp net http2)` removed. HTTP/2 is selected only through `(chezpp net http)` and
  reported by `http-response-version` as `h2`; HTTP/1.x is reported as `h1`.
- Keep libwebsockets optional. `libchezpp.so` must not acquire a `libwebsockets` or `nghttp2`
  dependency, and Chezpp source/build/test code must not reference nghttp2.
- One serialized owner services an LWS context. LWS callbacks copy events into bounded native
  storage and never invoke Scheme, user body producers, body consumers, or handlers.
- Blocking and nonblocking helpers share one transport path and one absolute deadline across
  redirects. A blocking helper waits for the operation returned by its nonblocking counterpart.
- Pull request bodies only after an LWS writable event. Acknowledge response bytes only after Scheme
  has consumed them. Finalize every source and sink exactly once on success, failure, cancellation,
  timeout, and close.
- Route callbacks with physical connection identity, logical transaction or stream identity, and a
  generation. Reject stale events after reuse.
- One public client may own multiple concurrent operations. Track operations by identity; never
  return an unrelated pending operation for a new request.
- Blocking and nonblocking requests use the same redirect-capable operation state machine.
  Redirect attempts share one absolute deadline and require a replayable body for 307/308.
- Treat command queue acceptance and command execution as separate results. A rejected start, body
  submission, acknowledgement, cancellation, or release fails its owner deterministically.
- Consume copied events destructively. Neither the reactor nor a transport may retain response
  payload events until operation release.
- Assign physical connection ids from LWS network WSI lifecycle. Scheme origin and lease ids are
  policy selectors and cannot serve as evidence of physical connection reuse.
- Fail explicitly when headers, trailers, bodies, decompression, or queues exceed configured
  limits. Do not truncate protocol metadata.
- Pool only terminal, detached internal objects. Reset all Scheme/native references and wipe payload
  buffers before reuse. Pools are bounded, observable, and fail deterministically on exhaustion.
- Preserve unrelated dirty files and generated test outputs. Do not commit planning or handoff files
  unless the user explicitly requests it.

## Target Ownership Model

```text
http-client (Scheme policy, transport registry, and active-operation table)
  -> one or more transport origins keyed by scheme/host/port/proxy/TLS/ALPN policy
  -> LWS context owned by one reactor
  -> native physical connection identified from its network WSI
       HTTP/1: one active transaction; idle reuse only after the LWS capability gate
       HTTP/2: many logical streams; LWS enforces peer settings and Chezpp adds local bounds
  -> logical transaction/stream operation
  -> copied headers/body/terminal events
  -> Scheme response or original user condition
```

The native boundary must distinguish physical connection lifecycle from logical operation
lifecycle. `connection-id` is assigned from the authoritative network WSI and outlives individual
transactions. `stream-id` identifies one HTTP/1 transaction or H2 child stream. A Scheme origin or
lease id may select policy, but it must never be reported as physical identity.

## Detailed Architecture Design

### Public and Private Scheme APIs

The exported `(chezpp net http)` API remains source-compatible. Internally, replace positional
vectors with records from a new `(chezpp net http private)` library:

```text
http-request-policy
  headers auth cookie-jar proxy tls-context version pool-policy
  follow-redirects? max-redirects deadline-ms

normalized-http-request
  method uri scheme host port tls? path headers body-factory body-length policy

transport-response
  status reason headers body trailers version connection-id
```

`body-factory` is `#f` for no body or a procedure with signature
`() -> (values source replayable?)`. Immutable strings and bytevectors create a fresh source for
every attempt. A user-provided `http-body-source` is one-shot unless its private wrapper supplies a
factory. This makes 307/308 and authentication retry behavior explicit.

An `http-client` owns a mutex, lifecycle, next operation id, active-operation table, and transport
registry. The registry key is:

```text
(scheme host port proxy-identity tls-context-identity alpn-policy)
```

Every `http-send/nonblocking` call snapshots policy and returns a distinct operation. Its states are
`preparing`, `starting`, `sending`, `receiving`, `redirecting`, and terminal. `http-send` does only
`net-operation-wait` on that operation. `http-cancel-pending!` snapshots all active operations
under the client mutex, releases the mutex, and cancels each snapshot entry.

`net-operation` serializes `step!` and terminal transitions with an internal lifecycle lock.
Cancellation may race with advancement, but only one transition wins and cleanup runs once. The
HTTP client's active table removes an operation from its cleanup procedure. A caller must not
advance one operation concurrently from two wait loops; cancellation and close are thread-safe.

Configuration setters affect operations created after the setter returns. Proxy, TLS, or protocol
changes retire incompatible idle transports immediately; pool-policy changes resize future
admission and evict excess idle connections; active operations retain their original snapshot.

Public behavior is fixed as follows:

| API | Required behavior |
| --- | --- |
| `http-open` | Accept the historical optional TLS context; create no network resource yet. |
| `http-send/nonblocking` | Return a distinct operation including redirects and retries. |
| `http-send` | Wait on `http-send/nonblocking`; add no transport or redirect behavior. |
| `http-cancel-pending!` | Cancel a snapshot of all active operations and return the client. |
| `http-close` | Reject new work, cancel active work, close transports, and remain idempotent. |
| policy setters | Affect future operations and retire incompatible idle transports. |
| `http-client-version-set!` | Accept only `auto`, `http/1.1`, or `h2`. |
| `http-response-version` | Return observed `h1` or `h2`, never requested policy. |

For TLS, `auto` offers `h2` and `http/1.1` and routes after observed ALPN. Explicit `h2` requires
observed H2 and never downgrades. Explicit `http/1.1` offers only that protocol. For cleartext,
`auto` uses HTTP/1.1 and explicit `h2` uses LWS prior knowledge. Proxy and TLS context are part of
the immutable transport key because their LWS context configuration is immutable.

The auth procedure form has signature `(request response-or-#f) -> request`. It runs outside all
client, origin, reactor, and native locks. The returned request is renormalized and may retry only
with remaining deadline and a replayable body. Basic and Bearer policy is applied while preparing
each attempt, so cross-origin redirect stripping cannot leak credentials.

Private Scheme procedure contracts are:

| Procedure | Signature and responsibility |
| --- | --- |
| `make-lws-client-transport` | `(limits tls-context proxy) -> transport` |
| `lws-client-request/nonblocking` | `(transport request sink) -> net-operation` |
| `lws-client-close!` | `(transport) -> transport`; idempotent cancellation and shutdown |
| `lws-client-pool-metrics` | `(transport) -> metrics`; Scheme and native bounded-pool data |
| `lws-reactor-start-stream!` | `(reactor operation request-head) -> boolean`; queue acceptance |
| `lws-reactor-drain-events!` | `(reactor operation) -> event-list`; destructive ordered drain |
| `lws-reactor-close-connection!` | `(reactor connection-id generation reason) -> boolean` |
| `make-http1-transaction-state` | `(transport request sink) -> http1-state` |
| `http1-reduce-event!` | `(http1-state event) -> operation-update-or-#f` |
| `h2-origin-submit!` | `(origin operation request sink) -> h2-stream-state` |
| `h2-reduce-event!` | `(h2-stream-state event) -> operation-update-or-#f` |

`(chezpp net lws client)` owns one immutable LWS context and reactor for a transport key. The
public library imports this shared client, not `http1.ss` or `http2.ss`. The shared client registers
the operation before starting LWS, observes the negotiated protocol, and selects an HTTP/1 or H2
event reducer. Both reducers consume the same named request and response records and reactor
commands; neither owns or services the LWS context.

### Reactor API and Command Semantics

The reactor is the only caller of mutating native LWS functions. Transport modules submit these
private commands:

```text
connect/start  operation-id stream-id generation connect-spec request-head
submit-body    operation-id stream-id generation bytes final?
consume-body   operation-id stream-id generation byte-count
cancel-stream  operation-id stream-id generation reason
release-stream operation-id stream-id generation
close-connection connection-id generation reason
```

A command record contains its owner operation, tag, arguments, and generation. Enqueue returns
only queue acceptance. The reactor then executes the FFI call and converts any nonzero native
status into a condition on the owning operation. Commands without an operation owner, such as idle
connection close, report failure through reactor diagnostics and pool metrics.

Each operation owns a bounded FIFO of copied events. `lws-reactor-drain-events!` removes all current
events and transfers their payloads to the transport state machine; it never returns an append-only
history. At most one unconsumed readable payload exists per stream. Terminal state is stored
separately and its queue slot is reserved, so ordinary event exhaustion cannot hide completion.

Timeout handling first atomically changes the operation lifecycle to `cancelling`, then queues a
native cancel command, and only releases routing after the native terminal event or a bounded
shutdown grace period. Cancellation, timeout, completion, and close race through one compare-and-
set transition and run source/sink cleanup once.

### FFI and Native LWS Boundary

Scheme calls `foreign-procedure` wrappers only; native callbacks never call Scheme. Opaque context
handles are validated by the Scheme wrapper. The C API uses integer status results rather than
booleans:

```text
0  accepted/executed
1  stale identity or generation
2  bounded capacity exhausted
3  context or connection closing
4  invalid state or arguments
5  libwebsockets operation failed
```

The proposed native entry points have these responsibilities:

| Native entry point | Result and ownership |
| --- | --- |
| `chezpp_lws_http_context_open` | Return a context handle or zero; preallocate all pools. |
| `chezpp_lws_http_stream_start` | Copy a request into a free stream slot; return status. |
| `chezpp_lws_http_body_submit` | Copy one outbound chunk; the writable callback owns its write. |
| `chezpp_lws_http_body_consumed` | Release an inbound chunk and resume that WSI's receive flow. |
| `chezpp_lws_http_stream_cancel` | Schedule asynchronous stream close; do not complete early. |
| `chezpp_lws_http_stream_release` | Release only a detached terminal stream generation. |
| `chezpp_lws_http_connection_close` | Schedule close of an idle authoritative network WSI. |
| `chezpp_lws_http_context_next_event` | Remove and return one copied event wire vector. |
| `chezpp_lws_http_context_close` | Stop admission, drain bounded close work, then destroy. |

`stream_start` receives operation id, logical stream id, generation, peer address, port, TLS flag,
method, authority, path, encoded headers, body-presence flag, and ALPN policy. It does not receive a
claimed physical connection id. The established callback assigns that id after asking LWS for the
network WSI. `connection_close` is the only entry point that accepts an authoritative connection id
without a stream id.

The Scheme FFI library validates every handle, integer range, bytevector length, enum, and returned
wire-vector shape. It converts native status to a structured condition at the reactor boundary.
Transport libraries never call raw `foreign-procedure` bindings and never invoke service, write,
receive-flow, transaction-completion, or timeout functions outside the reactor owner thread.

### Concurrency and Lock Discipline

Never hold more than one public-client, protocol-origin, reactor, or native-context lock while
calling into another layer. Snapshot immutable command data under the owning lock, release it, then
enqueue or invoke the next layer. The reactor owner is the sole mutator of LWS state, so the native
lock protects copied queues and cross-thread inspection only; do not hold it across an LWS service,
write, flow-control, or close call.

User body sources, sinks, auth procedures, redirect normalization, cookie processing, and
compression transforms run only from the operation advancer and outside every transport lock.
Detach completion waiters under the reactor lock and invoke them afterward. Reset pooled objects on
their owning layer only after all queue links and callbacks have detached them.

Every returned native event is validated and converted to a private Scheme record. The C-to-Scheme
wire vector is fixed as:

```text
#(tag context-id connection-id stream-id generation status scope payload metadata)
```

`metadata` contains only tag-specific copied data. It never contains a native pointer. Headers and
trailers use a length-delimited representation, not NUL-delimited strings. Encode a big-endian
`u32` field count followed by repeated `u32 name-length`, name bytes, `u32 value-length`, and value
bytes. Reject embedded NULs, invalid field names, and incomplete pairs. Limits are configured when
the native context is created:
event count, command count, streams, physical connections, per-event bytes, header bytes,
per-stream queued body bytes, and context queued body bytes.

On a client callback, native code obtains `network-wsi = lws_get_network_wsi(wsi)`. A preallocated
connection record is found or assigned by that pointer, receives a monotonic native connection id,
and remains alive until the network WSI is destroyed and its child count is zero. The logical
stream record is attached through opaque user data and carries the Scheme operation id, stream id,
and generation. Native pointers never cross the FFI boundary.

Use the supported LWS callbacks and functions as follows:

- `LWS_CALLBACK_CLIENT_APPEND_HANDSHAKE_HEADER`: append the validated request headers and declare
  pending request body state.
- `LWS_CALLBACK_ESTABLISHED_CLIENT_HTTP`: capture status and all headers before LWS releases header
  storage; attach the stream to its authoritative network connection.
- `LWS_CALLBACK_RECEIVE_CLIENT_HTTP`: call `lws_http_client_read` only when receive flow is enabled.
- `LWS_CALLBACK_RECEIVE_CLIENT_HTTP_READ`: copy one bounded body chunk, then disable receive flow
  with `lws_rx_flow_control(wsi, 0)` until Scheme acknowledges it.
- `LWS_CALLBACK_CLIENT_HTTP_WRITEABLE`: write at most the supplied allowance and request another
  writable callback only when more body data remains. Retain an offset when `lws_write` accepts a
  partial chunk; do not pull the next source chunk until the current chunk is complete.
- `LWS_CALLBACK_COMPLETED_CLIENT_HTTP`: defer terminal publication until copied response bytes are
  consumed. Do not call `lws_http_transaction_completed` as a client reuse mechanism unless the
  supported LWS version documents that behavior.
- close, connection-error, protocol-drop, and WSI-destroy callbacks: determine stream versus
  connection scope by comparing `wsi` with `lws_get_network_wsi(wsi)`, publish one terminal result,
  detach pointers, and release records only after child references are gone.

The optional loader requires every symbol used by the base HTTP/1 path, including
`lws_get_network_wsi`, `lws_rx_flow_control`, and `lws_set_timeout`. Set
`CHEZPP_LWS_CAP_HTTP2` only when the base symbols plus `lws_get_peer_write_allowance` and an
H2-specific runtime symbol are present. `lws_h2_get_peer_txcredit_estimate` may serve as a compiled
H2 feature probe, but it must never be called or interpreted as `SETTINGS_MAX_CONCURRENT_STREAMS`.

`CHEZPP_LWS_CAP_HTTP2` means the runtime exposes the required H2 client surface; it does not mean a
peer supports H2. Observed ALPN or cleartext H2 readiness is the per-connection proof. The loader
records the runtime version and exact missing symbol in its failure. Chezpp continues to load LWS
dynamically, and linkage tests must show no direct `DT_NEEDED` entry for LWS or nghttp2.

When `http-open` receives a Chezpp TLS context, pass its native `SSL_CTX` through
`provided_client_ssl_ctx` while the LWS context is created. Retain the Scheme TLS context for at
least the transport lifetime. Preserve its verification, CA, certificate, key, cipher, and ALPN
configuration; do not reduce it to a boolean verification flag. The HTTP version policy may narrow
ALPN for an operation but must not silently discard the caller's TLS security policy.

### HTTP/1 Architecture and Behavior

#### HTTP/1 Reuse Capability Gate

The installed LWS 4.5.8 `lws-callbacks.h` documents
`LWS_CALLBACK_COMPLETED_CLIENT_HTTP` as equivalent to closing because client transaction
pipelining is not supported. `lws_http_transaction_completed` resets an HTTP connection, but that
does not override the documented client limitation. `LCCSCF_PIPELINE` and repeated
`lws_client_connect_via_info` calls are not sufficient evidence of HTTP/1 reuse.

Before implementing idle pooling, audit the exact supported LWS release and run a minimal live
fixture that counts server-side TCP accepts across two sequential requests. The gate passes only
when an upstream-supported client API and the live fixture both prove reuse. If no supported
release provides it, stop execution and obtain an explicit decision to raise or change the LWS
dependency, or to define HTTP/1 `max-idle` as unsupported with zero idle connections. Do not
emulate reuse by retaining a WSI or restoring a private HTTP parser.

The Scheme HTTP/1 origin scheduler enforces local `max-active`, `max-idle`, and idle timeout. It
is a reducer used by the shared LWS client and does not claim ownership of a WSI or context.
Starting a transaction calls `lws_client_connect_via_info` with
documented client flags. LWS selects or creates the physical connection. Callback mapping reveals
whether streams share a native connection id, but pooling is enabled only after the capability gate.

HTTP/1 transaction states are:

```text
queued -> starting -> request-open -> response-open -> draining -> completed
                  \-> cancelling -> failed/cancelled
```

Only a writable event may pull the body source. The source must return EOF or a bytevector no
larger than the requested maximum; zero-length non-EOF chunks are rejected to avoid a busy loop.
Known-length bodies emit one validated `Content-Length`. Unknown-length HTTP/1 bodies use the LWS
streaming mechanism and must not add a conflicting length. Source EOF closes the source once.

LWS owns HTTP/1 parsing and removes chunk framing before
`LWS_CALLBACK_RECEIVE_CLIENT_HTTP_READ`. Delete the native residual chunk parser and never infer
trailers by parsing arbitrary body bytes. Capture trailers only through a supported LWS header or
completion callback; if the minimum LWS API cannot expose them, fail a trailer capability test or
document an empty trailer result instead of reparsing the wire stream.

Response headers are captured completely before body delivery. A memory response has a configured
maximum body size; a sink response keeps only one bounded native chunk outstanding. The sink runs
outside native and reactor locks. Successful consumption queues `consume-body`; failure queues
cancel without acknowledging the failed chunk. Sink finish runs exactly once on every terminal
path, while file and port ownership follows the public constructor contract.

Response transforms sit between native chunks and the final accumulator or user sink. Gzip and
deflate decoding is incremental and bounded; decoded-byte limits apply after decompression. Remove
`Content-Encoding` and stale `Content-Length` only after successful decoding. A transform failure
cancels the stream and preserves the transform condition. Caller-owned port sources and sinks are
flushed or finalized but never closed; file constructors own and close their ports exactly once.
A failed download closes the file and leaves the partial file for the caller to inspect.

Preserve duplicate response fields such as `Set-Cookie`. Interim 1xx responses update attempt state
but do not complete the operation. HEAD, 204, and 304 responses complete without a body even if a
peer sends misleading framing. HTTP/2 has no reason phrase; use the empty string when LWS does not
expose an HTTP/1 reason phrase rather than synthesizing one.

When the capability gate passes, an HTTP/1 connection is eligible for idle accounting only after a
complete response, zero unread bytes, successful source/sink finalization, no cancellation or
protocol error, and an LWS result that permits another transaction. `Connection: close` and
EOF-delimited responses are never idle. `max-idle` eviction submits `close-connection`; it never
frees a Scheme id and assumes the WSI closed.

Redirect handling is transport-independent. Resolve `Location` against the current URI; rewrite
301/302/303 to GET according to the preserved API contract; preserve method/body for 307/308 only
when the body factory is replayable. Strip origin-sensitive authorization and explicit cookie
headers on a cross-origin redirect, then recompute policy headers and cookies for the target.

### HTTP/2 Architecture and Behavior

`(chezpp net lws http2)` imports the reactor and private HTTP records directly. It must not import
or wrap `(chezpp net lws http1)`. It is a reducer used by the shared LWS client. An H2 origin
scheduler owns queued operations and a set of native connection ids learned from callbacks. Each
request is a distinct child WSI and logical stream; several child WSIs may map to one network WSI
and therefore one connection id.

The first request for an origin is allowed to start as the protocol leader. After its callback
reports H2 readiness, queued requests start with matching authority, proxy, TLS, and ALPN policy;
LWS maps their child WSIs onto an eligible network WSI. If `auto` observes HTTP/1.1, the shared
client routes that operation to the HTTP/1 reducer and applies HTTP/1 admission to later work.

For the supported LWS 4.3+ public API, LWS parses peer SETTINGS and enforces remote stream
admission. Chezpp applies a configured local stream bound in addition to LWS, but does not claim to
observe `SETTINGS_MAX_CONCURRENT_STREAMS`. `lws_get_peer_write_allowance` may bound bytes written on
a writable callback; it is flow-control credit, not stream capacity.

LWS also owns wire-level GOAWAY processing. The public API does not expose GOAWAY's last-stream id
on the supported headers, so Chezpp must not synthesize it. When the network WSI enters closing or
is destroyed, stop admitting work to that connection. Preserve terminal results already delivered
for child streams; fail or retry only work that LWS rejected before response headers, and retry only
idempotent requests with replayable bodies and remaining deadline.

Allow at most one automatic retry for work rejected before response headers by a closing H2
connection. The retry uses a new stream generation and the same absolute deadline. Never retry
after headers, a non-idempotent method, or a one-shot body.

An H2 child failure is stream-scoped when its WSI differs from the network WSI. A network WSI
failure is connection-scoped and fails all attached children. Cancellation closes only the child
WSI through the supported asynchronous close path. No operation may kill the shared network WSI to
cancel one stream.

Per-stream upload and receive state is independent. User source/sink procedures run without the
origin mutex. A slow sink disables receive flow only for its child WSI; other streams continue.
Writable scheduling respects the child's peer write allowance and never uses a fixed chunk count
as evidence of protocol capacity.

Before encoding H2 headers, reject connection-specific fields and normalize `TE` to the only
permitted value, `trailers`. LWS owns pseudo-header encoding; Scheme supplies method, authority,
scheme, path, and ordinary fields through supported client APIs. Response pseudo-headers never
appear in the public header list.

### Server LWS Boundary

The server uses a separate listening context created with a real port and explicit vhost; it never
reuses the client `CONTEXT_PORT_NO_LISTEN` context. Add loader and FFI support for
`lws_create_vhost`, server TLS context setup, header construction, writable scheduling, and
transaction completion.

`LWS_CALLBACK_HTTP` creates a logical request only after the request line and headers are captured.
`LWS_CALLBACK_HTTP_BODY` copies one bounded chunk and disables receive flow until Scheme consumes
it. `LWS_CALLBACK_HTTP_WRITEABLE` emits headers once and then writes response body chunks with
partial-write tracking. `lws_http_transaction_completed` determines whether an HTTP/1 connection
accepts another request; H2 child completion leaves sibling streams alive.

Native server connection and request records are distinct from client stream records even when
they share event encoding. `http-accept/nonblocking` yields a public logical connection/request
handle, handlers run outside reactor locks, and server close rejects new accepts before draining or
cancelling active logical requests under a bounded grace deadline.

### Error and Verification Model

Preserve the first user callback condition from a source, sink, auth procedure, or body factory.
Transport cleanup errors are secondary and must not replace it. Native failures become structured
`net-error` conditions containing operation id, connection id when known, stream id, generation,
native status, and terminal scope.

Fake-event MATs verify state machines, stale-generation rejection, command failure, and exact
cleanup. Live fixtures prove LWS callback behavior, physical identity, HTTP/1 reuse, H2
child/network scope, TLS/ALPN, flow control, and cancellation. A skipped live MAT is never
acceptance evidence.

### Live Fixture Design and Acceptance Matrix

HTTP/1 Scheme fixtures signal readiness only after `socket-listen!`, set bounded read/write waits,
and close accepted sockets, ports, and listeners through `dynamic-wind`. Fixture completion is
reported through a synchronization object before the test joins the thread, so a failed client
cannot leave an unbounded `thread-join`.

Create `chezpp/c/net/lws_http2_fixture.c` only as a test-only libwebsockets server fixture. It must
not use, link, or load nghttp2. It should support cleartext prior knowledge and TLS ALPN,
configurable SETTINGS, delayed response consumption, stream reset, GOAWAY, and connection close.
It writes physical accept count, stream ids, peak concurrency, received body bytes, and terminal
actions to a control pipe. If a libwebsockets-only fixture cannot be provided, the live H2 gate
remains explicitly blocked rather than introducing an nghttp2 dependency.

| Gate | Required evidence |
| --- | --- |
| Public API | Distinct concurrent operations, cancel-all, redirect parity, and policy snapshots. |
| Reactor/FFI | Command failures, bounded destructive drains, limits, and stale-event rejection. |
| HTTP/1 | Live cleartext/TLS, streaming, timeout/cancel, finalizers, and capability-gated reuse. |
| HTTP/2 | One network WSI with concurrent children, isolation, LWS admission, and scoped failure. |
| TLS | Verification failure, custom CA success, client certificate behavior, and observed ALPN. |
| Hygiene | Empty MAT output, clean linkage, stable pool high-water marks, docs, and parentheses. |

## Task 1: Restore the Public HTTP API and a Runnable Focused Harness

**Files:** create `chezpp/net/http/private.ss`; rewrite `chezpp/net/http.ss`; modify `chezpp.ss`,
`chezpp/net/operation.ss`, `tests/net-operation.ss`, `tests/net-http.ss`,
`tests/net-http-contract.ss`, and `tests/Makefile` only where required.

**Interfaces:**

- Produces the existing high-level exports recorded by commit `5c9fdd3`.
- Produces private named normalized-request, request-policy, and transport-response records. Do not
  use positional vectors or vector length as a versioned interface between public and transport
  libraries.
- The public facade delegates to `(chezpp net lws client)` once Task 3 creates it. Protocol reducers
  remain private behind that shared transport.

- [x] Add a contract MAT that enumerates every retained export, checks representative arities and
  record accessors, and confirms `http2-open` is absent from `(chezpp net)`.
- [x] Remove `(chezpp net http2)` from `tests/net-http.ss`. Delete low-level memory-session MATs,
  but retain all high-level H2 behavior MATs for Task 4.
- [x] Run `make -C tests test-some TEST='net-http-contract net-http'` and confirm the expected red
  state is missing public procedures or behavior, not a missing Scheme library.
- [x] Recreate public request, response, body source/sink, cookie, proxy, multipart, pool policy,
  client, server, and connection records from the API inventory. Use the old 4,700-line
  implementation only to recover contract details; do not copy its socket/parser transport.
- [x] Define every server-facing export at its historical arity so imports remain compatible.
  Before Task 6 supplies its transport, calls that require a server must raise one deterministic
  `net-error` stating that the LWS server transport is unavailable.
- [x] Implement URI/header/body normalization and public configuration setters without network I/O.
  Compute one monotonic absolute deadline before the first attempt and retain it across redirects.
- [x] Replace the single `http-client-pending` slot with an active-operation table. Every call to
  `http-send/nonblocking` creates a distinct operation. Define `http-cancel-pending!` to snapshot
  and cancel all active operations, and make close reject new work before cancelling the snapshot.
- [x] Serialize `net-operation` advancement, cancellation, terminal transition, and one-shot
  cleanup. Add races between completion, timeout, explicit cancellation, and client close.
- [x] Put redirect handling in the nonblocking operation state machine. `http-send` only calls
  `net-operation-wait`; it must not implement a separate redirect loop. Preserve one absolute
  deadline and reject 307/308 replay of a consumed one-shot body source.
- [x] Snapshot headers, auth, cookies, proxy, pool, TLS, version, redirect, and timeout policy per
  operation. Setters affect future requests, retire incompatible idle transports, and do not
  mutate active operations.
- [x] Ensure every exported procedure has the required documentation and `pcheck`; check Scheme
   parentheses and documentation line length.
- [x] Run the contract MAT. The implementation remains uncommitted as requested for planning
  work.
- [x] Add concurrency, cancellation-set, redirect parity, body replay, and policy mutation MATs;
  run them before committing the corrected Task 1 public boundary.

## Task 2: Correct the Native LWS Connection and Transaction Boundary

**Files:** rewrite as needed `chezpp/c/net/lws_http.[ch]`, `chezpp/net/lws/ffi.ss`, and
`chezpp/net/lws/reactor.ss`; modify `tests/net-lws-reactor.ss`.

**Interfaces:**

- Produces explicit connect/acquire, start-stream, cancel-stream, release-stream, and
  close-connection operations with acknowledged execution results.
- Every copied event carries context, authoritative connection, logical stream, generation, tag,
  status, terminal scope, and bounded payload data.
- Exposes LWS-observed protocol, HTTP/1 reusability, child/network WSI scope, and connection close.
  LWS owns peer SETTINGS and wire-level GOAWAY behavior when its public API does not expose them.

- [x] Add fake-event tests for routing and stale-generation rejection. These tests prove the
  injected identity contract only; they do not prove live physical connection identity or reuse.
- [x] Audit upstream libwebsockets client callback ordering and connection reuse APIs for the
  minimum supported LWS version. Record callbacks for connection establishment, protocol
  negotiation, writable body production, headers, body reads, completion, close, and WSI scope.
  Record that the supported public API does not expose peer maximum streams or GOAWAY last-stream.
- [x] Bind native connection records to authoritative network WSIs and keep them alive until WSI
  close plus zero child streams. Allocate a new logical stream id for each HTTP/1 transaction and
  each H2 child; do not use a caller origin lease as `connection-id`.
- [x] Split native connection acquisition/release from logical stream start/release. Use documented
  LWS transaction completion and connection selection only; never retain or restart raw WSI
  pointers from Scheme.
- [x] Keep native state and buffers preallocated and bounded. Give connection, transaction/stream,
  event, signal, and poll records one reset function each and generation checks on every lookup.
- [x] Publish terminal events only after all pending readable data and complete headers/trailers.
  Fail oversized metadata instead of truncating it. Reserve terminal delivery capacity so pool
  exhaustion cannot leave an operation pending.
- [x] Observe H2 protocol readiness and child/network WSI failure scope from live callbacks. Let
  LWS enforce SETTINGS and GOAWAY; do not synthesize unavailable peer capacity or last-stream data.
  Fake injection is test support, not proof of this interface.
- [x] Replace append-only operation event history with destructive per-operation draining or a
  bounded state machine. Acknowledge start, submit, consume, cancel, and release execution failures
  to the owning operation.
- [x] Close each per-operation signal once on release. During reactor shutdown, fail and notify
  pending operations before destroying signal/context storage.
- [x] Keep external-poll service serialized on the reactor thread. No operation `advance` procedure
  may service LWS descriptors or run a Scheme callback.
- [x] Extend reactor/native MATs for bounded event memory, header/body overflow rejection, command
  rejection, and timeout cancellation ordering. Live identity and H2 metadata remain covered by
  the existing live suites. Verified in commit `0387493` and bounded reactor runs.

## Task 3: Implement and Finish HTTP/1 on Libwebsockets

**Files:** create `chezpp/net/lws/client.ss`; rewrite `chezpp/net/lws/http1.ss`; modify
`chezpp/net/http.ss`, `chezpp.ss`, `chezpp/c/net/lws_http.[ch]`, `tests/net-http.ss`, and the
bounded HTTP/1 fixture helper.

**Interfaces:**

- The shared LWS client owns the reactor and dispatches observed HTTP/1 events to this reducer.
- Consumes the Task 2 physical-connection and logical-transaction API.
- Produces HTTP/1 transaction state and event reductions for `lws-client-request/nonblocking`.
- Consumes named `normalized-http-request` records; no positional vector crosses this boundary.

- [x] Add deterministic contract MATs for the public boundary, body source/sink calls, and framing
  conflicts. Add opt-in loopback fixtures for fixed and segmented chunked responses, streaming
  upload, and response sinks.
- [ ] Make the loopback fixtures runnable and add live MATs for cleartext, TLS/ALPN `http/1.1`,
  trailers, EOF, source and sink failure, timeout, cancellation, redirect deadlines, proxy
  transitions, download, and readiness. A skipped opt-in MAT is not acceptance evidence.
- [ ] Run the HTTP/1 reuse capability gate against the exact supported LWS version. Record upstream
  API evidence and a server-side accept count. If reuse is unsupported, stop for the required
  dependency or behavior decision instead of marking this item complete.
- [ ] When supported reuse is proven, add MATs that show one physical connection for eligible
  sequential requests and no reuse after close, failure, cancellation, or unread response data.
- [x] Define an initial bounded Scheme origin admission table keyed by host, port, TLS policy, and
  ALPN. Its current lease ids are policy bookkeeping, not verified physical connections.
- [ ] Rework origin admission around native connection ids learned from callbacks. Enforce
  configured max-active, max-idle, and idle timeout, and close evicted idle connections through a
  reactor command.
- [x] Define initial transaction state with request, source, sink, event cursor, response
  accumulator, lease, generation, lifecycle, and absolute deadline.
- [ ] Make every user-owned transaction reference clearable, replace event cursors with destructive
  draining, and reset transaction state only after native detachment. Verify pooled records retain
  no request, source, sink, response, or condition.
- [x] Normalize framing once. Supply `Content-Length` for known bodies, use LWS-supported streaming
  semantics for unknown bodies, reject conflicting framing headers, and let libwebsockets perform
  HTTP/1 parsing and serialization.
- [ ] Pull at most the requested body size only on writable events. Close the source exactly once
  on EOF, failure, cancellation, timeout, redirect transition, and client close. Retain the first
  producer condition and do not pool its connection.
- [ ] Deliver readable chunks outside reactor/native locks, acknowledge only successfully consumed
  bytes, and finalize the sink once on every terminal path. Preserve the first sink condition.
- [x] Construct the response only after complete headers/body/trailers and report version `h1`.
- [ ] Release the transaction after native detachment. Pool a physical HTTP/1 connection only after
  the capability gate and a clean LWS-confirmed reusable result.
- [x] Implement initial redirect, auth, cookie, proxy, multipart, compression, download, and upload
  policy code over LWS. Do not introduce a direct socket fallback.
- [ ] Move redirects into the shared nonblocking state machine; enforce body replay and cross-origin
  credential rules. Add explicit response/decompression limits, incremental transforms, partial
  write handling, policy snapshots, and port/file ownership behavior. Remove manual chunk parsing.
- [x] Run `make clean && make`, then repeat
  `timeout 60s make -C tests test-some TEST='net-http net-http-contract net-lws-reactor'` ten times.
  Passing means both stdout and stderr are empty.
- [ ] Commit `net: implement libwebsockets HTTP/1 client` before starting HTTP/2.

## Task 4: Implement Genuine HTTP/2 Multiplexing on Libwebsockets

**Files:** create `chezpp/c/net/lws_http2_fixture.c`; rewrite `chezpp/net/lws/http2.ss` and H2
parts of `chezpp/c/net/lws_http.[ch]`; modify `chezpp/net/lws/client.ss`,
`chezpp/net/lws/reactor.ss`, `chezpp/net/http.ss`, `tests/net-http.ss`, and `tests/Makefile`.

**Interfaces:**

- Consumes Task 2 physical connections and logical streams directly, not
  an HTTP/1 transaction reducer as a wrapper.
- Runs as a protocol reducer under the shared LWS client; it does not create a second context.
- Produces one H2 transport per origin/policy key, with logical stream operations sharing its LWS
  network connection.
- Uses LWS-observed ALPN and child/network WSI identity. LWS enforces peer SETTINGS; Chezpp applies
  a local configured stream bound and does not report unavailable peer capacity.

- [ ] Add failing MATs for TLS ALPN `h2`, cleartext prior knowledge, required-H2 refusal, two and
  ten concurrent streams on one physical connection, all wait orders, local stream bounds,
  LWS-enforced peer admission, and reuse.
- [ ] Add flow/lifecycle MATs for streaming upload, slow-sink isolation, active and queued
  cancellation, timeout, child-stream failure, LWS-managed GOAWAY, final response before close,
  EOF, and network-WSI failure affecting sibling streams.
- [x] Replace the current `h2-origin` HTTP/1 wrapper with a direct reactor transport. It owns a FIFO
  queue, local stream limit, active stream table, known native connection ids, lifecycle, mutex,
  and generations. LWS owns peer admission and wire-level GOAWAY.
- [x] Allocate a unique logical stream identity beneath the shared connection identity. Route
  every event by connection, stream, and generation; determine scope from the native event, not by
  treating any failed stream as an origin failure.
- [x] Start queued streams only after H2 readiness and below the local bound. LWS enforces peer
  capacity. Stop selecting a connection after its network WSI begins closing; allow child results
  already delivered by LWS to finish.
- [x] Pull each request body only on that stream's writable event. Bound receive buffering per
  stream, acknowledge consumption independently, and ensure a slow sink never runs while holding
  the origin mutex or blocks unrelated stream advancement.
- [x] Schedule child-WSI close for cancellation/timeout before releasing the stream lease. Discard
  late events by generation. On network-WSI failure, fail attached and queued operations
  deterministically while preserving each original user callback condition where applicable.
- [x] Require observed `h2` when policy selects H2; never silently downgrade. Set response version
  to `h2` only from observed protocol state.
- [ ] Run `make clean && make`, then repeat
  `timeout 60s make -C tests test-some TEST='net-http net-http-contract net-lws-reactor'` ten times.
  Passing means stdout and stderr are empty.
- [ ] Commit `net: implement libwebsockets HTTP/2 client`.

## Task 5: Remove Obsolete HTTP Adapters and Consolidate Tests

**Files:** remove obsolete portions of `chezpp/net/ffi.ss`, `chezpp/c/net/http2.c`, and nghttp2
loader integration when unused; modify `Makefile`, `tests/Makefile`, examples, and docs.

- [ ] Classify every match from:

  ```bash
  rg -n '\(chezpp net http2\)|http2-(open|send|receive|next-event|consume|reset|goaway)' \
    chezpp tests examples docs
  rg -n 'ffi-net-http2' chezpp tests examples docs
  ```
- [ ] Delete low-level Scheme FFI bindings, memory-session C code, and tests that exercise
  serialized nghttp2 sessions. Retain high-level H2 behavior MATs through `(chezpp net http)`.
- [ ] Retain `chezpp/c/net/lws_http2_fixture.c` as test-only libwebsockets code only. It must not
  use, link, or load nghttp2, and it must not add any optional protocol dependency to `libchezpp.so`.
- [x] Remove all Chezpp nghttp2 adapters, loader entries, FFI bindings, and build references.
  No Chezpp feature may depend on nghttp2, even when libwebsockets is built with optional HTTP/2
  support elsewhere.
- [ ] Update examples to select protocol through the high-level client and inspect
  `http-response-version`.
- [ ] Run API, docs, loader, and linkage checks; commit `net: remove obsolete HTTP adapters`.

## Task 6: Implement LWS HTTP/1 and HTTP/2 Servers

**Files:** create `chezpp/net/lws/server.ss` and `tests/net-lws-server.ss`; extend
`chezpp/c/net/lws_http.[ch]`, `chezpp/net/lws/ffi.ss`, `chezpp/net/lws/reactor.ss`,
`chezpp/net/http.ss`, and `tests/Makefile`.

- [x] Add a separate LWS listening-context lifecycle; do not treat the existing
  `CONTEXT_PORT_NO_LISTEN` client context as a server.
- [x] Represent accepted logical requests/streams separately from physical connections. Queue a
  request after complete headers and deliver body/readable/writable events incrementally.
- [x] Implement public listen, accept/nonblocking, read, write, handler registration, serve,
  serve-loop, and close APIs over reactor commands for both protocol versions.
- [x] Invoke handlers and response sources outside reactor/native locks. Apply request receive
  backpressure and response writable flow control per logical request.
- [ ] Test HTTP/1 keep-alive/pipelining, chunking, partial clients, H2 concurrent streams, handler
  failure, backpressure, accept cancellation, and shutdown with live streams.
- [ ] Run focused tests and commit `net: implement libwebsockets HTTP server`.

## Task 7: Integrate Fiber Waiting After HTTP Is Stable

**Files:** modify `chezpp/concurrency/fiber.ss`, `chezpp/net/operation.ss`, and
`chezpp/net/lws/reactor.ss`; create `chezpp/concurrency/fiber-net.ss` and
`tests/net-http-fiber.ss`; modify `tests/Makefile`.

- [x] Complete the separately specified fiber lifecycle fixes before making HTTP depend on fibers.
- [x] Implement `net-operation-event` with one pooled waiter, scheduler-safe posting, unregister on
  cancellation, and late-completion rejection.
- [x] Make blocking HTTP helpers suspend inside fibers and wait normally outside fibers while
  reading the same operation result and condition.
- [ ] Test HTTP/1 and H2 concurrency, cancellation, timeout, scheduler migration, shutdown, and
  steady-state waiter reuse.
- [ ] Commit `net: integrate libwebsockets HTTP operations with fibers`.

## Task 8: Final Verification and Release Audit

- [x] Run `make clean && make` from the worktree root.
- [x] Run ten repetitions of the focused HTTP, reactor, server, and fiber suites with 60-second
  timeouts; require empty stdout and stderr, not merely exit status zero.
- [x] Run the complete network suite, public API/docs tests, optional-loader tests, and linkage
  tests.
- [x] Verify with `ldd`, `nm -D`, and source/build scans that Chezpp has no LWS/nghttp2 dependency
  and no nghttp2 references outside historical planning text.
- [x] Audit connection, stream/transaction, event, signal, waiter, source, and sink lifetimes across
  success, failure, cancellation, timeout, close, reset, and GOAWAY using the focused live suites.
- [x] Assert bounded pool high-water marks stabilize after warm-up and stale generations are rejected;
  pooled waiter and native overflow MATs verify no retained operation payloads or conditions.
- [x] Add bounded stress coverage in `tests/net-lws-stress.ss`: repeated fiber waiter reuse and
  pooled-operation cleanup. Verified with `timeout 120s make -C tests test-some TEST=net-lws-stress`.
- [x] Run `git diff --check`, changed-Scheme parenthesis checks, documentation line checks, and test
  output hygiene. Record exact commands and results in a new handoff.

### Stress Expansion: 2026-09-12

- [x] Add bounded non-fiber HTTP/1 stress for repeated fixed responses, request bodies, server
  handlers, protocol-version reporting, and response cleanup.
- [x] Add bounded non-fiber HTTP/2 stress for repeated multiplexed operations, large responses,
  independent operation waits, and protocol-version reporting.
- [x] Add bounded non-fiber cancellation/failure stress for refused requests and deterministic
  terminal cancellation.
- [x] Run the expanded network-focused suite, including HTTP/1, HTTP/2, server, reactor, fiber,
  and stress MATs: `timeout 300s make -C tests test-some TEST='net-http net-http-contract
  net-lws-reactor net-lws-server net-http-fiber net-lws-http2 net-lws-stress net-http-stress'`.
  The run completed without MAT failures.

### Test Harness and Artifact Audit: 2026-09-13

- [x] Reviewed all network tests and shared fixtures for generated output, temporary certificates,
  sockets, transfer files, fixture directories, and child-process ports.
- [x] Updated `tests/Makefile` to use one shell per test target with EXIT/HUP/INT/TERM cleanup;
  cleanup removes harness logs, compiled fixtures, generated network files, certificates, sockets,
  and temporary fixture directories even when a MAT fails.
- [x] Hardened H2 fixture teardown so broken pipes and already-closed process ports are ignored
  during cleanup while fixture diagnostics remain checked.
- [x] Full network test target list completed under a bounded timeout. A transient LWS refcount
  assertion appeared once during an earlier aggregate run; subsequent isolated, repeated focused,
  and complete aggregate runs passed without recurrence. The assertion remains recorded as a
  historical observation, not an active reproduced failure.

### Stress Expansion: 2026-09-14

- [x] Extended `tests/net-http-stress.ss` with a bounded repeated H2 cancellation stress case:
  four rounds of four concurrent `/large` streams, cancellation of every operation, and explicit
  terminal-state accounting. The MAT comment identifies the cancellation/peer-isolation error
  case being exercised.
- [x] Kept stress execution independently reproducible by using the existing H2 fixture and
  operation APIs; no blocking ad-hoc socket fixture was added.
- [x] Verified the expanded stress target with `timeout 180s make -C tests test-some TEST='net-lws-http2
  net-http-stress'`; both suites completed without MAT failures or diagnostics.
