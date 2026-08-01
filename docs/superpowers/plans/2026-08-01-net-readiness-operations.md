# Net Readiness Operations Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development
> (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace thread-backed pseudo-nonblocking APIs with scheduler-neutral readiness state
machines that manual callers and the future fiber scheduler can drive identically.

**Architecture:** A common Scheme operation record exposes state, poll targets, deadline,
advancement, cancellation, result, and failure. Native shims return explicit would-block direction
and protocol state; FTP uses libcurl multi and gRPC uses one shared completion-queue driver.

**Tech Stack:** ChezScheme records, `poll(2)`, nonblocking sockets, OpenSSL WANT_READ/WANT_WRITE,
libssh SSH_AGAIN, libcurl multi socket callbacks, gRPC completion queues, and `mat` tests.

---

### Task 1: Common Operation And Would-Block Records

**Files:**
- Create: `chezpp/net/operation.ss`
- Create: `chezpp/net/operation/private.ss`
- Modify: `chezpp/net.ss`
- Modify: `chezpp.ss`
- Create: `tests/net-operation.ss`
- Modify: `tests/Makefile`

- [ ] **Step 1: Write lifecycle tests**

Add tests that cover immediate completion, two readiness steps, timeout, cancellation, repeated
cleanup, and invalid result access:

```scheme
(mat net-operation-lifecycle
     (let* ([steps 0]
            [operation
             (make-net-operation
              'test
              (lambda ()
                (set! steps (fx+ steps 1))
                (if (fx= steps 1)
                    (net-operation-pending '() 5000)
                    (net-operation-completed 'done)))
              (lambda () (void)))])
       (and (eq? 'pending (net-operation-state operation))
            (begin (net-operation-step! operation) #t)
            (eq? 'pending (net-operation-state operation))
            (begin (net-operation-step! operation) #t)
            (eq? 'completed (net-operation-state operation))
            (eq? 'done (net-operation-result operation))))

     ;; A cancelled operation cannot be stepped or read as a successful result.
     (let ([operation
            (make-net-operation 'test
                                (lambda () (net-operation-pending '() #f))
                                (lambda () (void)))])
       (net-operation-cancel! operation)
       (and (eq? 'cancelled (net-operation-state operation))
            (error? (net-operation-result operation)))))
```

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-operation'
```

Expected: FAIL because `(chezpp net operation)` does not exist.

- [ ] **Step 3: Implement operation updates**

Define private immutable update records and constructors:

```scheme
(define-record-type (net-operation-update %make-net-operation-update
                                           net-operation-update?)
  (sealed #t)
  (opaque #f)
  (fields (immutable state net-operation-update-state)
          (immutable poll-targets net-operation-update-poll-targets)
          (immutable deadline-ms net-operation-update-deadline-ms)
          (immutable value net-operation-update-value)))

(define net-operation-pending
  (lambda (poll-targets deadline-ms)
    (%make-net-operation-update 'pending poll-targets deadline-ms #f)))

(define net-operation-completed
  (lambda (value)
    (%make-net-operation-update 'completed '() #f value)))

(define net-operation-failed
  (lambda (condition)
    (%make-net-operation-update 'failed '() #f condition)))
```

- [ ] **Step 4: Implement the public operation record**

Use this field contract in `chezpp/net/operation.ss`:

```scheme
(define-record-type (net-operation %make-net-operation net-operation?)
  (sealed #t)
  (opaque #f)
  (fields (immutable kind net-operation-kind)
          (immutable advance net-operation-advance)
          (immutable cancel net-operation-cancel)
          (immutable cleanup net-operation-cleanup)
          (mutable state net-operation-state net-operation-state-set!)
          (mutable poll-targets net-operation-poll-targets
                   net-operation-poll-targets-set!)
          (mutable deadline-ms net-operation-deadline-ms
                   net-operation-deadline-ms-set!)
          (mutable value net-operation-value net-operation-value-set!)
          (mutable cleaned? net-operation-cleaned?
                   net-operation-cleaned?-set!)))
```

Export and document:

```scheme
net-operation?
make-net-operation
net-operation-pending
net-operation-completed
net-operation-failed
net-operation-kind
net-operation-state
net-operation-poll-targets
net-operation-deadline-ms
net-operation-remaining-timeout-ms
net-operation-step!
net-operation-cancel!
net-operation-result
net-operation-condition
net-operation-wait
net-would-block?
make-net-would-block
net-would-block-resource
net-would-block-events
```

The `advance` procedure passed to `make-net-operation` has signature `() -> operation-update`.
The `cancel` and `cleanup` procedures have signature `() -> unspecified`. The three-argument
constructor uses `void` as cleanup; the four-argument form accepts explicit cleanup.
`net-operation-step!` validates updates, catches conditions into `failed`, runs cleanup exactly
once on terminal transition, and returns the same operation. `net-operation-wait` is the only
helper allowed to call blocking `poll`.

- [ ] **Step 5: Run tests and parenthesis check**

```bash
make clean && make
cd tests && make test-some TEST='net-operation'
../chez++ --script ../tools/check-scheme-balance.ss \
  ../chezpp/net/operation.ss ../chezpp/net/operation/private.ss
```

Expected: exit 0 and empty Scheme test stdout/stderr.

- [ ] **Step 6: Commit**

```bash
git add chezpp/net/operation.ss chezpp/net/operation/private.ss chezpp/net.ss chezpp.ss \
  tests/net-operation.ss tests/Makefile
git commit -m "net: add readiness operation contract"
```

### Task 2: Poll And Socket Readiness Primitives

**Files:**
- Modify: `chezpp/c/net/socket.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/poll.ss`
- Modify: `chezpp/net/socket.ss`
- Modify: `tests/net-core.ss`
- Modify: `tests/net-operation.ss`

- [ ] **Step 1: Add explicit would-block and connect tests**

```scheme
(mat net-socket-readiness
     (let ([listener (open-socket 'inet 'stream)])
       (dynamic-wind
         void
         (lambda ()
           (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
           (socket-listen! listener 1)
           (let ([answer (socket-accept/nonblocking listener)])
             (and (net-would-block? answer)
                  (eq? listener (net-would-block-resource answer))
                  (equal? '(read) (net-would-block-events answer)))))
         (lambda () (close-socket listener))))

     (let ([socket (open-socket 'inet 'stream)])
       (dynamic-wind
         void
         (lambda ()
           (let ([operation
                  (socket-connect/nonblocking
                   socket (make-socket-address 'inet "127.0.0.1" 9) 100)])
             (and (net-operation? operation)
                  (memq (net-operation-state operation) '(pending failed)))))
         (lambda () (close-socket socket)))))
```

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-core net-operation'
```

Expected: FAIL because current APIs return `#f` and connect has no operation API.

- [ ] **Step 3: Return readiness direction from C**

Use the established FFI status vector shape:

```c
static ptr would_block_status(const char *event) {
  ptr value = Smake_vector(2, Sfalse);
  Svector_set(value, 0, Sstring_to_symbol("would-block"));
  Svector_set(value, 1, Sstring_to_symbol(event));
  return value;
}
```

Return `read` for accept/recv EAGAIN, `write` for send/connect EINPROGRESS, and verify nonblocking
connect completion with `getsockopt(fd, SOL_SOCKET, SO_ERROR, ...)`.

- [ ] **Step 4: Extend poll resources and deadlines**

`make-poll-target` accepts sockets, integer descriptors, binary ports with an extractable file
descriptor, and net operations. Add `poll-until` with an absolute millisecond deadline. Preserve
`error`, `hup`, and `invalid` even when read/write is also reported.

- [ ] **Step 5: Implement Scheme would-block results and connect operation**

Convert FFI would-block vectors with:

```scheme
(define ffi-result->would-block
  (lambda (resource answer)
    (and (ffi-would-block? answer)
         (make-net-would-block resource
                               (list (ffi-would-block-event answer))))))
```

Add `socket-connect/nonblocking`, whose advance closure attempts connect once, then polls for write
and checks `SO_ERROR`. Cancellation closes the socket. Blocking `socket-connect!` delegates to the
operation and `net-operation-wait`.

- [ ] **Step 6: Build and test**

```bash
make clean && make
cd tests && make test-some TEST='net-core net-operation'
```

Expected: exit 0 and empty test stdout/stderr.

- [ ] **Step 7: Commit**

```bash
git add chezpp/c/net/socket.c chezpp/net/ffi.ss chezpp/net/poll.ss chezpp/net/socket.ss \
  tests/net-core.ss tests/net-operation.ss
git commit -m "net: expose socket readiness explicitly"
```

### Task 3: TLS, SSH, SFTP, And WebSocket Readiness

**Files:**
- Modify: `chezpp/c/net/tls.c`
- Modify: `chezpp/c/net/ssh.c`
- Modify: `chezpp/c/net/websocket.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/tls.ss`
- Modify: `chezpp/net/ssh.ss`
- Modify: `chezpp/net/sftp.ss`
- Modify: `chezpp/net/websocket.ss`
- Modify: `tests/net-core.ss`
- Modify: `tests/net-ssh.ss`
- Modify: `tests/net-ssh-common.ss`
- Modify: `tests/net-sftp.ss`
- Modify: `tests/net-websocket.ss`

- [ ] **Step 1: Add readiness direction tests**

For each protocol start its local fixture, make a read before data is available, and assert a
would-block value names the underlying resource and `read`. Fill output buffers until a write
would block and assert `write`. For TLS also force WANT_WRITE during handshake.

Add `with-test-ssh-channel` to `tests/net-ssh-common.ss`. It starts `start-ssh-test-server`, sets
`HOME`, opens/authenticates one session, opens one channel, executes the supplied command, calls
the supplied `(channel) -> value` procedure, and closes channel/session/server with nested
`dynamic-wind` forms.

```scheme
(mat net-ssh-stderr-readiness
     (with-test-ssh-channel
      "sh -c 'sleep 1; printf err >&2'"
      (lambda (channel)
        (let ([answer (ssh-read-stderr/nonblocking channel 16)])
          (and (net-would-block? answer)
               (memq 'read (net-would-block-events answer)))))))
```

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-core net-ssh net-sftp net-websocket'
```

Expected: FAIL because would-block is `#f` and the SSH stderr procedure is absent.

- [ ] **Step 3: Preserve native readiness reasons**

Map OpenSSL `SSL_ERROR_WANT_READ` and `SSL_ERROR_WANT_WRITE`, libssh `SSH_AGAIN`, SFTP AIO pending
state, and libwebsockets service/writeable state to explicit FFI would-block vectors. Export the
underlying SSH session descriptor and libwebsockets service descriptors as poll targets.

- [ ] **Step 4: Remove sleep polling**

Delete `wait-nonblocking` and every `milisleep` retry loop in `chezpp/net/websocket.ss`. Blocking
wrappers create an operation and call `net-operation-wait`; nonblocking calls perform one attempt.

- [ ] **Step 5: Add SSH stderr one-attempt APIs**

Export and document:

```scheme
ssh-read-stderr
ssh-read-stderr!
ssh-read-stderr/nonblocking
ssh-read-stderr!/nonblocking
```

Pass `is_stderr = 1` through the existing C read functions. Keep stdout APIs at
`is_stderr = 0`. The existing error port delegates to the stderr functions and closes its
duplicated resource on port close.

- [ ] **Step 6: Build and test**

```bash
make clean && make
cd tests && make test-some TEST='net-core net-ssh net-sftp net-websocket'
```

Expected: exit 0 and empty test stdout/stderr.

- [ ] **Step 7: Commit**

```bash
git add chezpp/c/net/tls.c chezpp/c/net/ssh.c chezpp/c/net/websocket.c chezpp/net/ffi.ss \
  chezpp/net/tls.ss chezpp/net/ssh.ss chezpp/net/sftp.ss chezpp/net/websocket.ss \
  tests/net-core.ss tests/net-ssh.ss tests/net-sftp.ss tests/net-websocket.ss
git commit -m "net: expose transport readiness and SSH stderr"
```

### Task 4: libcurl Multi FTP Operations

**Files:**
- Modify: `chezpp/c/net/ftp.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/ftp.ss`
- Modify: `tests/net-ftp.ss`
- Modify: `tests/net-ftp-common.ss`

- [ ] **Step 1: Add a thread-free FTP operation test**

Add `with-test-ftp-session` to `tests/net-ftp-common.ss`. It calls `start-ftp-test-server`, opens
`ftp://127.0.0.1:<port>/`, invokes the supplied `(session) -> value` procedure, and closes the
session/server with nested `dynamic-wind` forms.

```scheme
(mat net-ftp-readiness-operation
     (with-test-ftp-session
      (lambda (session)
        (let ([operation (ftp-list/nonblocking session ".")])
          (and (net-operation? operation)
               (let loop ()
                 (net-operation-step! operation)
                 (case (net-operation-state operation)
                   [(completed) (bytevector? (net-operation-result operation))]
                   [(pending)
                    (poll (net-operation-poll-targets operation)
                          (net-operation-remaining-timeout-ms operation))
                    (loop)]
                   [else #f])))))))
```

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-ftp'
```

Expected: FAIL because the current implementation creates a notifier socket and Scheme thread.

- [ ] **Step 3: Add the native multi transfer handle**

Define a handle that owns one easy handle, the shared session multi handle, tracked sockets,
libcurl's requested timeout, transfer buffers/ports, result code, and cancellation state. Bind and
use:

```c
curl_multi_init
curl_multi_cleanup
curl_multi_add_handle
curl_multi_remove_handle
curl_multi_socket_action
curl_multi_info_read
curl_multi_setopt
curl_easy_pause
```

Use `CURLMOPT_SOCKETFUNCTION` and `CURLMOPT_TIMERFUNCTION` to maintain the exact poll targets and
next timer deadline returned to Scheme.

- [ ] **Step 4: Add FFI start/step/cancel/close operations**

```scheme
ffi-net-ftp-transfer-start
ffi-net-ftp-transfer-step
ffi-net-ftp-transfer-cancel
ffi-net-ftp-transfer-close
```

`step` takes ready `(fd . events)` pairs plus a timer flag and returns one of:
`#(pending targets timeout-ms)`, `#(completed value)`, or `#(error message)`.

- [ ] **Step 5: Replace Scheme worker records**

Delete `ftp-pending-thread`, notifier sockets, `fork-thread`, and `thread-join`. Each
`ftp-*/nonblocking` call constructs one `net-operation`. Blocking counterparts call the same
constructor and `net-operation-wait`.

- [ ] **Step 6: Test cancellation and cleanup**

Add a transfer that pauses after the first chunk, cancel it, and assert the local partial file is
handled according to policy and the session can immediately start another transfer.

- [ ] **Step 7: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-ftp net-operation'
cd ..
git add chezpp/c/net/ftp.c chezpp/net/ffi.ss chezpp/net/ftp.ss tests/net-ftp.ss \
  tests/net-ftp-common.ss
git commit -m "net: drive FTP with libcurl multi readiness"
```

### Task 5: Incremental HTTP Operations And Hashtable Handlers

**Files:**
- Modify: `chezpp/net/http.ss`
- Modify: `tests/net-http.ss`

- [ ] **Step 1: Add incremental request and handler replacement tests**

```scheme
(mat net-http-handler-table
     (let ([server (http-listen "127.0.0.1" 0)]
           [first (lambda (request) (make-http-response 200 "OK" '() "first"))]
           [second (lambda (request) (make-http-response 200 "OK" '() "second"))])
       (dynamic-wind
         void
         (lambda ()
           (http-register-handler! server 'get "/item" first)
           (http-register-handler! server 'get "/item" second)
           (eq? second (http-handler-ref server 'get "/item" #f)))
         (lambda () (http-server-close server)))))
```

Add a slow server test that emits status line, headers, and body in separate readiness cycles and
asserts `http-request/nonblocking` remains pending between each cycle without a worker thread.

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-http'
```

Expected: FAIL because handlers are an alist and composite operations use worker threads.

- [ ] **Step 3: Replace handler storage**

Construct `http-server-handlers` with `(make-hashtable equal-hash equal?)`. Normalize keys to
`(cons (string-upcase method) normalized-path)`. `http-register-handler!` replaces an existing
key and returns the prior handler or `#f`; add documented `http-handler-ref` and
`http-unregister-handler!`.

- [ ] **Step 4: Implement HTTP client states**

Use explicit states:

```scheme
resolve -> connect -> tls-handshake -> write-head -> write-body -> read-status
        -> read-headers -> read-body -> complete
```

Each advance consumes available bytes only, preserves parser buffers on the operation, and returns
the transport's poll targets. Redirects create the next request state within the same operation.
Cancellation closes only resources owned by that operation.

- [ ] **Step 5: Implement server connection operations**

Make accept, request parsing, handler invocation, and response writing independent operations.
`http-serve-loop` becomes a readiness loop with no `threaded?` argument; concurrency is delegated
to the caller or fiber scheduler.

- [ ] **Step 6: Remove worker-thread code**

Delete notifier records, `fork-thread`, and `thread-join` from `chezpp/net/http.ss`. Update all
nonblocking docs to return `net-operation` records and state exact final values.

- [ ] **Step 7: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-http net-operation'
cd ..
git add chezpp/net/http.ss tests/net-http.ss
git commit -m "net: make HTTP readiness-driven and hash handlers"
```

### Task 6: SCP State Machines

**Files:**
- Modify: `chezpp/c/net/ssh.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/scp.ss`
- Modify: `tests/net-scp.ss`

- [ ] **Step 1: Add a multi-step SCP transfer test**

Start the temporary SSH server, upload a file larger than the native chunk size, assert at least
two pending steps occur, then download and compare SHA-256.

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-scp'
```

Expected: FAIL because current nonblocking SCP starts a Scheme worker thread.

- [ ] **Step 3: Split SCP C routines into start and step calls**

Native transfer state records the SCP request phase, open local descriptor, remote metadata,
offset, and direction. Every libssh `SSH_AGAIN` returns the SSH descriptor with the required event.
Never loop over an entire file inside one FFI call.

- [ ] **Step 4: Replace Scheme pending-thread records**

Delete notifier sockets and thread fields. `scp-download/nonblocking`, `scp-upload/nonblocking`,
and recursive copy return net operations. Cancellation closes local descriptors and the SCP handle
once; completed upload/download returns the destination path.

- [ ] **Step 5: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-scp net-ssh net-operation'
cd ..
git add chezpp/c/net/ssh.c chezpp/net/ffi.ss chezpp/net/scp.ss tests/net-scp.ss
git commit -m "net: make SCP transfers readiness-driven"
```

### Task 7: Shared gRPC Completion Driver

**Files:**
- Modify: `chezpp/c/net/grpc.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/grpc.ss`
- Modify: `tests/net-grpc.ss`

- [ ] **Step 1: Add concurrent operation and thread-count tests**

Open one channel, start 100 unary operations, service them locally, and assert all operations share
one readiness descriptor. Record `/proc/self/task` count before and after; the increase must be at
most one for the shared completion driver, not 100.

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-grpc'
```

Expected: FAIL because stream-opening operations currently fork one Scheme thread each.

- [ ] **Step 3: Implement the process-wide completion driver**

Create one driver after gRPC initialization. It owns a completion queue, one `eventfd` on Linux or
nonblocking pipe fallback, a mutex-protected completed-tag queue, and one native pthread. The
thread blocks in `grpc_completion_queue_next`, enqueues completed tags, and signals the descriptor.
It exits during gRPC shutdown after queue shutdown is observed.

- [ ] **Step 4: Expose operation registration and draining**

Add FFI operations that register a tag, return the shared descriptor, drain completed tags without
blocking, cancel a call, and release operation state. A Scheme gRPC operation filters drained tags
for its handle and republishes unrelated tags to the channel's completion map.

- [ ] **Step 5: Remove Scheme threads and notifiers**

Delete `grpc-pending-op-thread`, reader/writer notifier sockets, `fork-thread`, and `thread-join`.
Unary and all streaming open APIs return net operations. Stream send/recv one-attempt APIs return
explicit would-block values.

- [ ] **Step 6: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-grpc net-operation'
cd ..
git add chezpp/c/net/grpc.c chezpp/net/ffi.ss chezpp/net/grpc.ss tests/net-grpc.ss
git commit -m "net: share gRPC completion readiness"
```

### Task 8: Phase 2 Release Gate

**Files:**
- Review all Phase 2 files.

- [ ] **Step 1: Prohibit direct Scheme thread creation in net operations**

```bash
rg -n 'fork-thread|spawn-thread|thread-join|open-pending-notifier' \
  chezpp/net/{ftp,scp,grpc,http}.ss
```

Expected: no matches.

- [ ] **Step 2: Prohibit hidden sleep polling**

```bash
rg -n 'milisleep|sleep.*loop|poll/nonblocking.*loop' chezpp/net
```

Expected: no protocol retry loops; examples and explicit blocking helpers are outside this audit.

- [ ] **Step 3: Build and run the net suite**

```bash
make clean && make
cd tests && make test-some TEST='net-operation net-core net-http net-ftp net-ssh net-sftp net-scp net-websocket net-grpc'
```

Expected: exit 0 and empty test stdout/stderr.

- [ ] **Step 4: Check parentheses**

```bash
../chez++ --script ../tools/check-scheme-balance.ss \
  ../chezpp/net/operation.ss ../chezpp/net/operation/private.ss \
  ../chezpp/net/poll.ss ../chezpp/net/socket.ss ../chezpp/net/tls.ss ../chezpp/net/ssh.ss \
  ../chezpp/net/sftp.ss ../chezpp/net/scp.ss ../chezpp/net/ftp.ss ../chezpp/net/http.ss \
  ../chezpp/net/websocket.ss ../chezpp/net/grpc.ss
```

Expected: every file is balanced. `git status --short` is empty before Phase 3.
