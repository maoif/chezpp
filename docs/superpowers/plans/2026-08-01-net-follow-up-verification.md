# Net Follow-Up And Verification Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development
> (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Complete the remaining address, DNS, error, poll, socket, IP, URI, TLS, private-boundary,
documentation, and end-to-end verification roadmap.

**Architecture:** Finish foundational records and readiness services without changing the common
operation contract. Add optional c-ares and libidn2 adapters, complete TLS policy/state APIs, audit
all public contracts, and finish with local and external transfer gates.

**Tech Stack:** ChezScheme, C/POSIX sockets and poll, c-ares, libidn2, OpenSSL 3, `mat`, temporary
network fixtures, `readelf`, SHA-256, and shell verification drivers.

## Current Status (2026-08-23)

Tasks 1-10 are implemented through HTTP/2 corrective commit `4150676`. Fresh focused and complete
suites, loader/linkage checks, generated-binding comparison, public documentation audit, Scheme
balance, native linkage audit, all ten local transfer variants, and the deterministic HTTP/2
TLS/cancellation regressions pass. The two pinned external downloads also pass with Emacs SHA-256
`414d3a1a21147af257ebd98bdd15976fdcb5ed0563f6de89f76d4a4b5dad9c72` and Arch Linux SHA-256
`e86295dc0bdf9b85a5a9256810c553239689d2ae8e80eeec81b4e2e910d8a6c0`.

---

### Task 1: Structured Net Errors And Private FFI Invariants

**Files:**
- Modify: `chezpp/net/errors.ss`
- Modify: `chezpp/net/private.ss`
- Modify: `chezpp/net/ffi.ss`
- Create: `tests/net-errors.ss`
- Modify: `tests/Makefile`

- [ ] **Step 1: Add structured error tests**

```scheme
(mat net-error-fields
     (let ([cause (condition (make-message-condition "inner"))]
           [error (make-net-error 'socket 'connect "refused"
                                  'connection-refused 111
                                  "127.0.0.1:1" #t cause '())])
       (and (net-error? error)
            (eq? 'socket (net-error-kind error))
            (eq? 'connect (net-error-operation error))
            (eq? 'connection-refused (net-error-status error))
            (= 111 (net-error-errno error))
            (net-error-retryable? error)
            (eq? cause (net-error-cause error))))

     (net-error-matches?
      (make-net-error 'dns 'resolve "timeout" 'timeout #f "example.test" #t #f '())
      '((kind . dns) (operation . resolve) (status . timeout) (retryable? . #t))))
```

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-errors'
```

Expected: FAIL because the current record has only who, kind, message, and data.

- [ ] **Step 3: Expand the public condition record**

Document fields and export accessors for who, kind, operation, message, status, errno, endpoint,
retryable?, cause, and data. Preserve the old `make-net-error` arity as a compatibility wrapper;
the full arity is canonical. `raise-net-error` accepts the same two arities.

- [ ] **Step 4: Add matching helpers**

`net-error-matches?` accepts an alist of exact field expectations. `call-with-net-error` accepts a
thunk of signature `() -> value` and a handler of signature `(net-error) -> value`; it re-raises
non-net conditions.

- [ ] **Step 5: Document private vector shapes**

Place comments beside each FFI decoder specifying vector length, field index, Scheme type,
ownership, and error variants. Validate length/types before access and raise `internal-ffi` errors
on malformed vectors. Do not export raw FFI predicates from `(chezpp net)`.

- [ ] **Step 6: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-errors net-core net-http net-ftp net-ssh net-sftp net-scp net-websocket net-grpc'
cd ..
git add chezpp/net/errors.ss chezpp/net/private.ss chezpp/net/ffi.ss tests/net-errors.ss tests/Makefile
git commit -m "net: add structured transport errors"
```

### Task 2: Scheduler-Friendly DNS And Address Selection

**Files:**
- Create: `chezpp/c/cares_loader.h`
- Create: `chezpp/c/cares_loader.c`
- Create: `chezpp/c/net/dns.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/dns.ss`
- Modify: `chezpp/net/address.ss`
- Modify: `chezpp/net/private.ss`
- Modify: `tests/net-loader.ss`
- Create: `tests/net-dns.ss`
- Create: `tests/net-address.ss`
- Modify: `tests/Makefile`

- [ ] **Step 1: Add resolver option and result tests**

```scheme
(mat net-dns-options
     (let ([options (make-dns-options 'unspecified 'address 2000 #t)])
       (and (dns-options? options)
            (eq? 'unspecified (dns-options-family options))
            (eq? 'address (dns-options-type options))
            (= 2000 (dns-options-timeout-ms options))
            (dns-options-canonical-name? options)))

     (let ([operation (dns-resolve/nonblocking "localhost" default-dns-options)])
       (and (net-operation? operation)
            (dns-result? (net-operation-wait operation)))))
```

Add deterministic fixture responses for A, AAAA, CNAME, NXDOMAIN, timeout, and partial A success
with AAAA failure.

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-dns net-address net-loader'
```

Expected: FAIL because option records and c-ares integration are absent.

- [ ] **Step 3: Add optional c-ares loading**

Load `libcares.so.2`, call `ares_version`, require ABI SONAME 2 and runtime >= 1.18.0, then resolve
library init/cleanup, channel init/destroy, query/getaddrinfo, socket, timeout, process-fd, and free
symbols. Expose c-ares availability and version through `optional-library-info`.

- [ ] **Step 4: Implement DNS operations**

One native query handle owns an ares channel, result accumulator, outstanding query count, sockets,
and deadline. Scheme advance passes ready descriptor events to `ares_process_fd`, then refreshes
poll targets and the `ares_timeout` deadline. Cancellation destroys the channel once.

- [ ] **Step 5: Define structured results**

Expand `dns-result` with query name, canonical name, addresses, aliases, record type, TTLs, status,
and partial errors. Empty successful results return a record with empty addresses; NXDOMAIN and
timeout raise distinct structured errors.

- [ ] **Step 6: Add address selection and service names**

`resolve-addresses` accepts numeric ports or service strings. Add `address-select` with a predicate
`(socket-address) -> boolean`, `address-interleave` for IPv6/IPv4 ordering, and
`connect-addresses/nonblocking` implementing staggered attempts with one deadline. Reverse lookup
gets a timeout option.

- [ ] **Step 7: Test IPv6, Unix-domain, and ordering**

Use local fixtures only. Assert stable multi-result order, IPv6 loopback resolution when available,
Unix-domain round trip, service `http -> 80`, and cleanup after the first successful connection.

- [ ] **Step 8: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-dns net-address net-loader net-operation'
cd ..
git add chezpp/c/cares_loader.h chezpp/c/cares_loader.c chezpp/c/net/dns.c \
  chezpp/net/ffi.ss chezpp/net/dns.ss chezpp/net/address.ss chezpp/net/private.ss \
  tests/net-loader.ss tests/net-dns.ss tests/net-address.ss tests/Makefile
git commit -m "net: add asynchronous DNS and address selection"
```

### Task 3: Poll And Socket Completion

**Files:**
- Modify: `chezpp/c/net/socket.c`
- Modify: `chezpp/c/net/poll.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/poll.ss`
- Modify: `chezpp/net/socket.ss`
- Modify: `tests/net-core.ss`
- Modify: `examples/net/file-transfer/file-transfer-tcp*.ss`

- [ ] **Step 1: Add datagram, dual-stack, option, and half-close tests**

Test UDP send-to/recv-from on IPv4 and IPv6 when available; IPv6-only toggle; reuse-port,
keepalive timing, TCP no-delay, send/receive buffer sizes, broadcast, multicast TTL; write
half-close followed by readable peer response; and read half-close behavior.

- [ ] **Step 2: Add poll propagation tests**

For pipe/socket fixtures assert read plus hangup may be returned together, a closed invalid
descriptor reports `invalid`, connect failure reports `error`, and absolute deadlines do not reset
after spurious readiness.

- [ ] **Step 3: Run and verify failure**

```bash
cd tests && make test-some TEST='net-core net-operation'
```

Expected: FAIL because datagram address operations and several options are absent.

- [ ] **Step 4: Add datagram APIs**

Export blocking and one-attempt nonblocking `socket-send-to`, `socket-send-to/nonblocking`,
`socket-recv-from`, and `socket-recv-from/nonblocking`. Receive returns two values: payload and
socket-address. Into-buffer variants return byte count and address.

- [ ] **Step 5: Complete socket options**

Map each documented symbol explicitly in C and validate Scheme value type/range before FFI. Unknown
options are rejected in Scheme. Dual-stack is controlled by `ipv6-only`; do not infer it from host
platform defaults.

- [ ] **Step 6: Stabilize fixed-format operation records**

Ensure poll targets retain their requested event list and return a separate ready-event list.
Document that error/hup/invalid never suppress read/write bits. Update TCP examples to handle
partial writes/reads and EOF without assuming one call transfers the entire frame.

- [ ] **Step 7: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-core net-operation'
cd ..
git add chezpp/c/net/socket.c chezpp/c/net/poll.c chezpp/net/ffi.ss chezpp/net/poll.ss \
  chezpp/net/socket.ss tests/net-core.ss examples/net/file-transfer/file-transfer-tcp*.ss
git commit -m "net: complete socket and poll readiness"
```

### Task 4: IP Classification And CIDR Algebra

**Files:**
- Modify: `chezpp/net/ip.ss`
- Create: `tests/net-ip.ss`
- Modify: `tests/Makefile`

- [ ] **Step 1: Add mapped, special-range, and CIDR tests**

```scheme
(mat net-ip-special
     (let ([mapped (string->ip-address "::ffff:192.0.2.1")])
       (and (ip-address-mapped-ipv4? mapped)
            (string=? "192.0.2.1"
                      (ip-address->string (ip-address-unmap-ipv4 mapped)))))
     (ip-address-link-local? (string->ip-address "fe80::1"))
     (ip-address-documentation? (string->ip-address "192.0.2.1")))
```

Add table tests for unspecified, broadcast, carrier-grade NAT, benchmarking, documentation,
discard-only, unique-local, link-local, multicast scopes, and reserved ranges.

- [ ] **Step 2: Add property tests**

Generate deterministic IPv4/IPv6 bytevectors. Assert parse/format round trips, every address is in
its host CIDR, split children exactly cover the parent, and merge(split(cidr)) returns the parent.

- [ ] **Step 3: Run and verify failure**

```bash
cd tests && make test-some TEST='net-ip'
```

Expected: FAIL because the new predicates and CIDR operations are absent.

- [ ] **Step 4: Implement APIs with bytevector operations**

Export mapped-address predicates/conversion, link-local/site-local compatibility predicates,
named special-use predicates, `cidr-split`, `cidr-merge`, `cidr-address-count`, and
`cidr-overlaps?`. Use bytevector accessors and fixnum arithmetic for bounded indexes/prefixes.

- [ ] **Step 5: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-ip net-core'
cd ..
git add chezpp/net/ip.ss tests/net-ip.ss tests/Makefile
git commit -m "net: complete IP and CIDR operations"
```

### Task 5: URI Construction, Raw Components, RFC Coverage, And IDNA

**Files:**
- Create: `chezpp/c/idn2_loader.h`
- Create: `chezpp/c/idn2_loader.c`
- Create: `chezpp/c/net/idna.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/uri.ss`
- Modify: `tests/net-loader.ss`
- Create: `tests/net-uri.ss`
- Modify: `tests/Makefile`

- [ ] **Step 1: Add RFC 3986 and raw-component tests**

Use every normal and abnormal relative-resolution example from RFC 3986 section 5.4. Assert raw
path/query/fragment preserve percent-escape spelling while decoded accessors return decoded values.
Assert construction/update round trips do not double-encode `%2F`.

- [ ] **Step 2: Add IDNA tests**

Test `bücher.example -> xn--bcher-kva.example`, reverse conversion, uppercase normalization,
disallowed control characters, bidi failure, and a missing-libidn2 diagnostic.

- [ ] **Step 3: Run and verify failure**

```bash
cd tests && make test-some TEST='net-uri net-loader'
```

Expected: FAIL because raw accessors, update APIs, and IDNA are absent.

- [ ] **Step 4: Add optional libidn2 loading**

Load `libidn2.so.0`, call `idn2_check_version`, require runtime >= 2.3.0, and resolve lookup,
decode, strerror, and free functions. Add the library to capability/linkage tests.

- [ ] **Step 5: Separate raw and decoded URI fields**

Store raw userinfo, host, path, query, and fragment exactly as parsed. Existing accessors return
decoded values for compatibility; add `uri-raw-*` accessors. `uri->string` uses raw components
unless a field was replaced through an update API.

- [ ] **Step 6: Add constructors and updates**

Export `make-uri`, `uri-with-scheme`, `uri-with-authority`, `uri-with-path`, `uri-with-query`, and
`uri-with-fragment`. Each returns a new immutable URI and validates component grammar.

- [ ] **Step 7: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-uri net-loader net-http net-websocket'
cd ..
git add chezpp/c/idn2_loader.h chezpp/c/idn2_loader.c chezpp/c/net/idna.c \
  chezpp/net/ffi.ss chezpp/net/uri.ss tests/net-loader.ss tests/net-uri.ss tests/Makefile
git commit -m "net: complete URI construction and IDNA"
```

### Task 6: TLS Policy, ALPN, OCSP, Sessions, SNI, And Chain Inspection

**Files:**
- Modify: `chezpp/c/openssl_loader.h`
- Modify: `chezpp/c/openssl_loader.c`
- Modify: `chezpp/c/net/tls.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/tls.ss`
- Modify: `tests/net-core.ss`
- Modify: `tests/net-common.ss`

- [ ] **Step 1: Add TLS feature tests**

Test negotiated ALPN, minimum/maximum protocol policy, cipher-list policy, session reuse across two
connections, server SNI choosing two certificates, full peer-chain records, stapled OCSP success,
missing-required-staple failure, and malformed/revoked OCSP failure.

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-core'
```

Expected: FAIL because the public controls/accessors are absent.

- [ ] **Step 3: Extend the OpenSSL table**

Resolve ALPN selected access, SSL session serialization/reuse, servername callback, OCSP status
request/response, chain verification, minimum/maximum protocol setters, TLS 1.2 cipher list, and
TLS 1.3 ciphersuite functions. Mark OCSP and session serialization capabilities separately.

- [ ] **Step 4: Define TLS records**

Document `tls-policy`, `tls-session-ticket`, `tls-certificate`, and `tls-ocsp-result`. Certificate
records contain DER, subject, issuer, serial, validity, digest, public-key summary, and verified
chain position. Private keys are never present.

- [ ] **Step 5: Add context policy and SNI APIs**

Export `tls-context-policy-set!` and `tls-context-sni-selector-set!`. The selector signature is
`(server-name) -> tls-context-or-#f`; invoke it from a safe Scheme-controlled handshake step, not
directly from an arbitrary native callback. Retain selected contexts until handshake completion.

- [ ] **Step 6: Add session and OCSP APIs**

Export negotiated ALPN, session export/import, session reused predicate, peer chain records,
stapled OCSP bytes/result, and required/optional/disabled OCSP policy. Validate OCSP issuer,
signature, status, and validity interval.

- [ ] **Step 7: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-core net-http net-websocket net-grpc'
cd ..
git add chezpp/c/openssl_loader.h chezpp/c/openssl_loader.c chezpp/c/net/tls.c \
  chezpp/net/ffi.ss chezpp/net/tls.ss tests/net-core.ss tests/net-common.ss
git commit -m "net: complete TLS policy and session APIs"
```

### Task 7: Public API And Record Documentation Audit

**Files:**
- Create: `tools/check-public-api-docs.ss`
- Modify: every changed public Scheme library under `chezpp/net/`, `chezpp/protobuf/`, and
  `chezpp/optional-library.ss`
- Create: `tests/net-docs.ss`
- Modify: `tests/Makefile`

- [ ] **Step 1: Write the documentation checker**

Parse Scheme datums and adjacent block comments. For every exported identifier classified as a
procedure/macro/record-generated accessor, require an adjacent matching documentation block. Check
all documentation lines are <= 100 characters and reject the vague return phrases
`returns a value`, `returns the result`, and `returns unspecified` unless unspecified is mandated.

- [ ] **Step 2: Run and capture failures**

```bash
./chez++ --script tools/check-public-api-docs.ss chezpp/net chezpp/protobuf chezpp/optional-library.ss
```

Expected before the audit: nonzero exit listing existing records/accessors and procedures whose
parameter or return documentation is incomplete.

- [ ] **Step 3: Document every public record**

Above each public record definition add `#|record:name|#` text specifying constructor, accessors,
field types/meaning, mutability, ownership, lifetime, and state transitions. Do not expose or
document private FFI handles as user-manipulable values.

- [ ] **Step 4: Clarify every changed procedure return**

State exact return values for success, EOF, would-block, pending operation, close, cancellation,
mutation, lookup miss, and callbacks. Higher-order docs state argument and return signatures.
Every exported procedure uses meaningful parameter names and `pcheck`.

- [ ] **Step 5: Run checker, build, and tests**

```bash
./chez++ --script tools/check-public-api-docs.ss chezpp/net chezpp/protobuf chezpp/optional-library.ss
make clean && make
cd tests && make test-some TEST='net-docs'
```

Expected: checker and tests exit 0 with no output.

- [ ] **Step 6: Commit**

```bash
git add tools/check-public-api-docs.ss tests/net-docs.ss tests/Makefile chezpp/net \
  chezpp/protobuf chezpp/optional-library.ss
git commit -m "net: document public records and return values"
```

### Task 8: Unified Local Transfer Verification

**Files:**
- Create: `examples/net/file-transfer/verify-local-transfers.sh`
- Modify: `examples/net/file-transfer/file-transfer-common.ss`
- Modify protocol examples as required by failures.

- [ ] **Step 1: Create deterministic artifacts**

Generate 1 KiB, 16 MiB, and 128 MiB files from a fixed byte pattern and record SHA-256 with Chezpp
digest APIs. Include empty file and non-ASCII filename cases.

- [ ] **Step 2: Start isolated services**

Start FTP, FTPS, temporary sshd for SFTP/SCP, HTTP, HTTPS, WebSocket, WSS, gRPC, and TLS gRPC on
reserved loopback ports. Store all roots, keys, certificates, logs, and PIDs under one `mktemp -d`
directory. A trap closes services and removes the directory.

- [ ] **Step 3: Upload and download every artifact**

For each protocol and secure variant upload all artifacts, download to a different local path,
compare SHA-256 and size, then repeat one transfer with resume and one with cancellation. Capture
service output and fail if unexpected stdout/stderr is non-empty.

- [ ] **Step 4: Exercise interactive FTP and SFTP**

Feed scripted commands for list/mkdir/upload/download/rename/delete/rmdir to each client. Assert
the final remote directory is empty and every downloaded hash matches.

- [ ] **Step 5: Run the driver**

```bash
./examples/net/file-transfer/verify-local-transfers.sh
```

Expected: all protocol/size/direction rows report `PASS`, service stderr is empty, and exit is 0.

- [ ] **Step 6: Commit**

```bash
git add examples/net/file-transfer
git commit -m "net: verify local transfers across all protocols"
```

### Task 9: External Download Examples

**Files:**
- Modify: `examples/net/http-download.ss`
- Create: `examples/net/file-transfer/verify-external-downloads.ss`
- Create: `examples/net/file-transfer/verify-external-downloads.sh`

- [ ] **Step 1: Define exact artifacts**

The driver contains:

```scheme
(define external-artifacts
  '(("https://ftp.gnu.org/gnu/emacs/windows/emacs-30/emacs-30.2.zip"
     "414d3a1a21147af257ebd98bdd15976fdcb5ed0563f6de89f76d4a4b5dad9c72")
    ("https://mirrors.tuna.tsinghua.edu.cn/archlinux/iso/2026.07.01/archlinux-2026.07.01-x86_64.iso"
     "e86295dc0bdf9b85a5a9256810c553239689d2ae8e80eeec81b4e2e910d8a6c0")))
```

- [ ] **Step 2: Stream, resume, and verify**

Download each URL without buffering the body, interrupt once after at least 8 MiB, resume, and
compute SHA-256. On network/404 failure, try only documented mirrors serving the identical
filename and accept them only when the original expected hash matches.

- [ ] **Step 3: Keep downloads outside the repository**

The shell wrapper creates a temporary output directory or accepts one explicit destination. It
prints URL, destination, byte count, expected hash, actual hash, and elapsed time. A trap removes
temporary partials unless `KEEP_DOWNLOADS=1`.

- [ ] **Step 4: Run both downloads**

```bash
./examples/net/file-transfer/verify-external-downloads.sh
```

Expected: both artifacts report the exact supplied SHA-256 and exit 0.

- [ ] **Step 5: Commit**

```bash
git add examples/net/http-download.ss examples/net/file-transfer/verify-external-downloads.ss \
  examples/net/file-transfer/verify-external-downloads.sh
git commit -m "net: add verified resumable download examples"
```

### Task 10: Final Release Gate

**Files:**
- Review every file changed by the five plans.

- [ ] **Step 1: Build from a clean tree**

```bash
make clean && make
```

Expected: exit 0.

- [ ] **Step 2: Run the complete net and protobuf suites**

```bash
cd tests && make test-some TEST='protobuf protobuf-codegen net-loader net-operation net-transfer net-errors net-core net-address net-dns net-ip net-uri net-http net-ftp net-ssh net-sftp net-scp net-websocket net-grpc net-docs'
```

Expected: exit 0; stdout and stderr are empty except documented `Expect error` output.

- [ ] **Step 3: Run loader and linkage tests**

```bash
cd tests
make test-optional-library-loader
make test-optional-hash-linkage
./optional-library-linkage.sh
cd ..
readelf -d libchezpp.so | rg 'NEEDED'
```

Expected: loader tests pass. Scoped direct dependencies contain only libc and exempt `libuuid`.

- [ ] **Step 4: Confirm no thread-backed net operations remain**

```bash
rg -n 'fork-thread|spawn-thread|thread-join|open-pending-notifier' \
  chezpp/net/{ftp,scp,grpc,http}.ss
```

Expected: no matches.

- [ ] **Step 5: Check every changed Scheme source**

```bash
git diff --name-only master...HEAD -- '*.ss' | \
  xargs ./chez++ --script tools/check-scheme-balance.ss
```

Expected: all files have balanced parentheses.

- [ ] **Step 6: Run local transfer verification**

```bash
./examples/net/file-transfer/verify-local-transfers.sh
```

Expected: upload/download, secure variants, resume, cancellation, and interactive workflows pass.

- [ ] **Step 7: Run external download verification**

```bash
./examples/net/file-transfer/verify-external-downloads.sh
```

Expected: both supplied SHA-256 values match.

- [ ] **Step 8: Run documentation audit and inspect status**

```bash
./chez++ --script tools/check-public-api-docs.ss chezpp/net chezpp/protobuf chezpp/optional-library.ss
git status --short
```

Expected: audit exits 0 with no output and worktree status is empty.
