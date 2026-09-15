# HTTP/2 Cooperative Transport Driver Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development
> (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace independently pumped HTTP/2 stream operations with one cooperative transport
scheduler that completes multiplexed requests regardless of wait order.

**Architecture:** A transport-owned FIFO queue and active-stream table hold internal request states.
Every stream operation advances the shared scheduler and then observes its own state. The native
adapter exposes peer concurrent-stream settings so the scheduler submits only eligible queued work.

**Tech Stack:** Chez Scheme, Chez foreign procedures, C11, dynamically loaded nghttp2, MAT tests.

## Current Status (2026-08-23)

The cooperative scheduler and corrective lifecycle work are committed through `4150676`. Peer
settings, FIFO scheduling, wait-order independence, request-local failure, cancellation/reset
ownership, timeout, EOF, GOAWAY, sink cleanup, cleartext/TLS multiplexing, and deterministic
TLS/cancellation regressions are implemented.

| Finding | Status | Evidence or remaining proof |
| --- | --- | --- |
| Final-read EOF discards generated events | Fixed | `net-http2-final-response-before-eof` passes. |
| Response sink failure fails siblings | Fixed | The failing sink operation fails while its sibling completes. |
| Active cancellation mutates nghttp2 outside scheduler | Fixed | Cancellation queues all table, state, reset, and output work for scheduler advancement. |
| TLS read `WANT_WRITE` readiness is lost | Fixed | Forced TLS read regression verifies the operation poll target includes both `read` and `write`. |
| Cancellation during scheduler event processing can complete a queued request | Fixed | Deterministic event hook cancels stream 5 during processing and asserts it does not complete internally. |
| Failed/cancelled sinks are not finalized | Fixed | Cancellation cleanup closes the owned download sink. |
| GOAWAY accepted-stream assertion is weak | Fixed | The accepted stream completes and later queued work fails. |

Fresh evidence: clean build, ten consecutive `net-http` runs, the complete net/protobuf suite,
optional loader/linkage checks, generated-binding comparison, public documentation audit, Scheme
balance, native linkage audit, local transfer verification, and both pinned external downloads pass.

---

## File Structure

- Modify `chezpp/c/nghttp2_loader.c` to require the remote-settings query symbol.
- Modify `chezpp/c/net/http2.c` to expose peer stream capacity and settings events.
- Modify `chezpp/net/ffi.ss` to bind the internal C query.
- Modify `chezpp/net/http2.ss` to wrap and document the internal settings API.
- Modify `chezpp/net/http.ss` to own queued request states and cooperative scheduling.
- Modify `tests/net-loader.ss` to test the adapter query and settings event.
- Modify `tests/net-http.ss` to test queueing, wait order, cancellation, failure, and parity.

### Task 1: Expose Peer Concurrent-Stream Settings

**Files:**
- Modify: `chezpp/c/nghttp2_loader.c`
- Modify: `chezpp/c/net/http2.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/http2.ss`
- Test: `tests/net-loader.ss`

- [ ] **Step 1: Write the failing adapter test**

Add a mat that exchanges initial settings, drains client events, and verifies both the positive peer
limit and the internal settings event tag `6`:

```scheme
(mat net-http2-peer-settings
     (let ([client (http2-open 'client)]
           [server (http2-open 'server)])
       (dynamic-wind
         void
         (lambda ()
           (exchange-http2! client server)
           (exchange-http2! server client)
           (let ([limit (http2-peer-max-concurrent-streams client)])
             (let loop ([event (http2-next-event client)] [saw-settings? #f])
               (if event
                   (loop (http2-next-event client)
                         (or saw-settings? (= 6 (vector-ref event 0))))
                   (and (natural? limit) (positive? limit) saw-settings?)))))
         (lambda ()
           (http2-close client)
           (http2-close server)))))
```

- [ ] **Step 2: Run the test and verify RED**

Run: `make clean && make && make -C tests test-some TEST=net-loader`

Expected: compilation fails because `http2-peer-max-concurrent-streams` is unbound.

- [ ] **Step 3: Load and query the nghttp2 setting**

Add `nghttp2_session_get_remote_settings` to `required_symbols` in
`chezpp/c/nghttp2_loader.c`. In `chezpp/c/net/http2.c`, define
`typedef uint32_t (*remote_settings_fn)(nghttp2_session *, nghttp2_settings_id);`, load the symbol
into `p_session_get_remote_settings`, and expose:

```c
ptr chezpp_net_http2_peer_max_concurrent_streams(uptr handle) {
  h2_session *state = (h2_session *)TO_VOIDP(handle);
  uint32_t value;
  if (state == NULL) return h2_error("invalid HTTP/2 session");
  value = p_session_get_remote_settings(
      state->session, NGHTTP2_SETTINGS_MAX_CONCURRENT_STREAMS);
  return Sunsigned((uptr)value);
}
```

Extend `on_frame` to call `push_event(state, 6, 0, frame->hd.flags, NULL, 0)` for received non-ACK
`NGHTTP2_SETTINGS` frames. Add a comment beside `h2_event` documenting tags `1` header, `2` data,
`3` frame, `4` close, `5` GOAWAY, and `6` settings.

- [ ] **Step 4: Bind and wrap the query**

Export and define `ffi-net-http2-peer-max-concurrent-streams` in `chezpp/net/ffi.ss`. Export and
document this procedure in `chezpp/net/http2.ss`:

```scheme
#|proc:http2-peer-max-concurrent-streams
The `http2-peer-max-concurrent-streams` procedure returns the peer's current HTTP/2 concurrent
stream limit for `session`.
|#
(define-who http2-peer-max-concurrent-streams
  (lambda (session)
    (pcheck ([http2-session? session])
      (ensure-open who session)
      (ensure-result
       who
       (ffi-net-http2-peer-max-concurrent-streams
        (http2-session-handle session))))))
```

- [ ] **Step 5: Verify GREEN and balance**

Run:

```bash
make clean && make
make -C tests test-some TEST=net-loader
python3 ../../check_parentheses.py chezpp/net/ffi.ss chezpp/net/http2.ss tests/net-loader.ss
```

Expected: build succeeds; test stdout and stderr are empty; balance check succeeds.

- [ ] **Step 6: Commit**

```bash
git add chezpp/c/nghttp2_loader.c chezpp/c/net/http2.c chezpp/net/ffi.ss \
  chezpp/net/http2.ss tests/net-loader.ss
git commit -m "net: expose HTTP/2 peer stream capacity"
```

### Task 2: Add Deterministic Scheduler Regressions

**Files:**
- Modify: `tests/net-http.ss`

- [ ] **Step 1: Add the last-stream-first TLS regression**

Extract the current `net-http2-multiplexing` setup into a private
`check-http2-multiplexing-order` test helper. Its `scheme`, `client-context`, and `wait-index*`
parameters select the URI scheme, optional TLS context, and operation wait order. The helper creates
ten operations and verifies each response against the index paired with that operation. Add a TLS
mat that passes wait order `(cons 9 (iota 9))`:

```scheme
(define check-response
  (lambda (operation index)
    (let ([response (net-operation-wait operation)])
      (and (= 200 (http-response-status response))
           (eq? 'h2 (http-response-version response))
           (string=? (format "/stream/~a" index)
                     (utf8->string (http-response-body response)))))))

(for-all (lambda (index)
           (check-response (list-ref operation* index) index))
         wait-index*)
```

- [ ] **Step 2: Add cleartext parity**

Add a separate cleartext mat that calls the helper with an `http://` URI, no TLS context, client
version `h2`, and wait order `(cons 9 (iota 9))`, so failures identify the transport.

- [ ] **Step 3: Verify RED in fresh silent processes**

Run:

```bash
make -C tests net-http.so
for run in 1 2 3 4 5 6 7 8 9 10; do
  timeout 30s make -C tests test-some TEST=net-http || exit $?
done
```

Expected: a run exits 124 before the scheduler implementation. Record the first failing run; if all
ten pass, repeat until the known intermittent stall is observed before writing production code.

- [ ] **Step 4: Commit the failing regressions**

```bash
git add tests/net-http.ss
git commit -m "net: reproduce cooperative HTTP/2 scheduling stall"
```

### Task 3: Introduce Request States And The Cooperative Scheduler

**Files:**
- Modify: `chezpp/net/http.ss`
- Test: `tests/net-http.ss`

- [ ] **Step 1: Add progress-order and queue-cancellation assertions**

Extend the helper to accept an optional cancellation index. Run forward `(iota 10)`, reverse
`(reverse (iota 10))`, and last-first `(cons 9 (iota 9))` wait orders. In a separate mat, cancel
operation 9 before waiting, assert its operation state is `cancelled`, and validate operations 0
through 8. Tests inspect only public operation and response behavior.

- [ ] **Step 2: Extend internal records**

Replace `http2-client-stream` with `http2-client-request-state`, retaining response accumulation and
adding lifecycle, optional stream ID, and deadline fields. Extend `http2-client-transport` with FIFO
queue and advancing state:

```scheme
(define-record-type
    (http2-client-request-state %make-http2-client-request-state
                                http2-client-request-state?)
  (sealed #t)
  (opaque #f)
  (fields (immutable request http2-client-request-state-request)
          (immutable finish http2-client-request-state-finish)
          (immutable sink http2-client-request-state-sink)
          (immutable deadline-ms http2-client-request-state-deadline-ms)
          (mutable lifecycle http2-client-request-state-lifecycle
                   http2-client-request-state-lifecycle-set!)
          (mutable stream-id http2-client-request-state-stream-id
                   http2-client-request-state-stream-id-set!)
          (mutable status http2-client-request-state-status
                   http2-client-request-state-status-set!)
          (mutable headers http2-client-request-state-headers
                   http2-client-request-state-headers-set!)
          (mutable body-parts http2-client-request-state-body-parts
                   http2-client-request-state-body-parts-set!)
          (mutable body-length http2-client-request-state-body-length
                   http2-client-request-state-body-length-set!)
          (mutable response http2-client-request-state-response
                   http2-client-request-state-response-set!)
          (mutable failure http2-client-request-state-failure
                   http2-client-request-state-failure-set!)))
```

Extend `http2-client-transport` with mutable `queue-front`, `queue-back`, and `advancing?` fields.

- [ ] **Step 3: Add FIFO helpers and invariants**

Add private procedures to enqueue, remove, and pop request states. Use a front/back pair or two
lists so enqueue is amortized constant time. Add one private lifecycle transition procedure that
rejects a second terminal transition.

- [ ] **Step 4: Make transfer creation enqueue states and submit eligible work**

Change `http2-transfer/nonblocking` to create a queued state and return a stream operation observing
it. Add `submit-http2-queued!`: compare active-table size with
`http2-peer-max-concurrent-streams`, submit FIFO states until capacity is reached, assign stream
IDs, transition each submitted state to `active`, and insert it in the active table. A submission
error transitions only that state to `failed`.

- [ ] **Step 5: Centralize event processing**

Update header, data, close, settings, and GOAWAY handlers to mutate request states. A close event
deletes the active table entry before completing or failing its state. Event tag `6` causes the next
submission pass to use the newly received peer limit.

- [ ] **Step 6: Implement bounded cooperative advancement**

Add `advance-http2-transport!` with this phase order:

```scheme
(submit-http2-queued! client transport)
(flush-http2-output! transport)
(read-http2-input! client transport)
(drain-http2-events! client transport)
(expire-http2-requests! transport)
(submit-http2-queued! client transport)
(flush-http2-output! transport)
```

Protect the cycle with the transport `advancing?` flag and `dynamic-wind`. Return shared poll
events: include `write` when output is buffered, nghttp2 wants write, or queued work is eligible;
always include `read error hup invalid`.

- [ ] **Step 7: Make stream operations observe request state**

The operation advance callback calls `advance-http2-transport!` and maps `completed` to
`net-operation-completed`, `failed` to the existing `raise-net-error` path, and queued or active to
`net-operation-pending` with the shared socket, readiness events, and request deadline. Its cancel
callback removes queued states or resets active states; cleanup removes only terminal references.

- [ ] **Step 8: Verify GREEN repeatedly**

Run:

```bash
make clean && make
make -C tests test-some TEST=net-loader
for run in 1 2 3 4 5; do
  timeout 30s make -C tests test-some TEST=net-http || exit $?
done
python3 ../../check_parentheses.py chezpp/net/http.ss tests/net-http.ss
```

Expected: build succeeds and five fresh `net-http` runs exit 0 with empty capture files.

- [ ] **Step 9: Commit**

```bash
git add chezpp/net/http.ss tests/net-http.ss
git commit -m "net: drive HTTP/2 requests through one scheduler"
```

### Task 4: Complete Cancellation And Failure Semantics

**Files:**
- Modify: `chezpp/net/http.ss`
- Test: `tests/net-http.ss`

- [ ] **Step 1: Add failing lifecycle mats**

Add separate mats for queued cancellation, active reset, GOAWAY with queued work, and connection
EOF. Each negative case must include a comment describing the error being tested and be separated
by a blank line.

- [ ] **Step 2: Implement queued and active cancellation**

Queued cancellation removes the state from the FIFO. Active cancellation queues error code 8 with
`http2-reset-stream!`, removes the active entry, and leaves sibling states untouched.

- [ ] **Step 3: Implement timeout and connection-wide failure**

Expire each state against its own deadline. On EOF or unrecoverable transport failure, transition
every queued and active request to `failed` exactly once, clear both collections, and close the
transport.

- [ ] **Step 4: Implement GOAWAY handling**

Allow active stream IDs at or below the last accepted ID to finish. Fail queued requests and active
stream IDs above it with the existing GOAWAY error data.

- [ ] **Step 5: Verify focused lifecycle tests**

Run:

```bash
make clean && make
make -C tests test-some TEST='net-operation net-loader net-http'
python3 ../../check_parentheses.py chezpp/net/http.ss tests/net-http.ss
```

Expected: exit 0 and empty stdout/stderr captures.

- [ ] **Step 6: Commit**

```bash
git add chezpp/net/http.ss tests/net-http.ss
git commit -m "net: complete HTTP/2 scheduler lifecycle handling"
```

### Task 5: Run The Release Gate

**Files:**
- Modify only if a gate exposes a defect in this implementation.

- [ ] **Step 1: Verify build and balance**

```bash
make clean && make
python3 ../../check_parentheses.py chezpp/net/ffi.ss chezpp/net/http2.ss \
  chezpp/net/http.ss tests/net-loader.ss tests/net-http.ss
```

- [ ] **Step 2: Stress the original failure**

```bash
for run in 1 2 3 4 5 6 7 8 9 10; do
  timeout 30s make -C tests test-some TEST=net-http || exit $?
done
```

Expected: ten exit-zero runs with empty `tests/net-http.stdout` and `tests/net-http.stderr`.

- [ ] **Step 3: Run the focused net/protobuf suite**

```bash
make -C tests test-some TEST='protobuf protobuf-codegen net-loader net-operation net-transfer \
net-errors net-core net-address net-dns net-ip net-uri net-http net-ftp net-ssh net-sftp net-scp \
net-websocket net-grpc net-docs'
```

Expected: exit 0 with empty stdout and stderr for every test.

- [ ] **Step 4: Run optional and documentation gates**

```bash
make -C tests test-optional-library-loader
make -C tests test-optional-hash-linkage
make -C tests test-optional-library-linkage
./tests/check-generated-bindings.sh
./tests/check-public-api-docs.sh
```

Expected: every command exits 0.

- [ ] **Step 5: Review the final diff**

Run: `git diff --check HEAD~4..HEAD && git status --short`

Expected: no whitespace errors; only preserved user-owned files remain unstaged.
