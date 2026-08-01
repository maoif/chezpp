# Net Roadmap Design

## Purpose

This design completes the Follow-Up Roadmap in
`docs/superpowers/plans/2026-05-13-net-hardening.md` and addresses the additional net hardening,
nonblocking I/O, transfer parity, SSH stderr, HTTP handler, documentation, example, and
verification requirements.

The work is intentionally split into a master plan and five executable phase plans. Each phase
must leave the tree buildable and testable. Later phases build only on public or documented
internal contracts established by earlier phases.

## Starting Point

Implementation starts from committed `master` plus the existing untracked files under
`examples/net/` in the main checkout. Those examples are implementation inputs: migrate them
into the isolated implementation worktree, preserve their useful command-line conventions, and
harden or extend them rather than recreating them without review.

The current isolated baseline does not build because committed `master` lacks
`chezpp/c/scheme.h`. The file exists untracked in the main checkout. This prerequisite must be
resolved explicitly before implementation can establish a clean baseline.

## Scope And Decisions

- All external shared libraries used by Chezpp are loaded lazily and checked at runtime, except
  libc and `libuuid`. `libuuid` remains directly linked by explicit maintainer decision.
- OpenSSL/libcrypto, libcurl, libssh, libwebsockets, gRPC/gpr, BLAKE3, xxHash, and any newly
  introduced native dependencies use the optional loader.
- Importing `(chezpp)` succeeds when optional dependencies are absent or incompatible. The first
  API requiring an unavailable dependency fails with a structured diagnostic.
- Thread-backed `/nonblocking` net operations are replaced by readiness-driven operation state
  machines suitable for manual polling and a future fiber scheduler.
- File upload and download verification covers FTP, FTPS, SFTP, SCP, HTTP, HTTPS, WebSocket,
  secure WebSocket, gRPC, and TLS-enabled gRPC. Raw SSH file transfer is not required.
- SSH remains in scope for interactive shells, independent stdout and stderr, command execution,
  forwarding, keepalives, authentication, environment/subsystem requests, and known hosts.
- HTTP server handler storage uses a hashtable.
- The full Follow-Up Roadmap is included, not merely the original seven hardening tasks.

## Architecture

### Optional Dependency Loader

A shared C loader owns lazy initialization, candidate SONAME selection, required and optional
symbol resolution, version validation, capability reporting, and persistent error diagnostics.
Initialization is thread-safe and idempotent. A failed load remains inspectable and may be
retried only through an explicit test/private reset mechanism.

Each dependency descriptor reports:

- logical library name and attempted SONAMEs;
- availability and initialization state;
- runtime version and required compatibility range;
- resolved optional capabilities;
- loader, symbol, initialization, or version failure details.

Compatibility follows each upstream ABI contract. OpenSSL requires ABI major 3 and the minimum
feature version used by Chezpp. libcurl and libssh require their stable ABI family and a minimum
runtime feature version. libwebsockets and gRPC enforce the supported ABI/SONAME family and a
minimum tested version. BLAKE3 and xxHash retain their existing stricter major/minor checks.

The build removes direct OpenSSL and crypto linkage. Small vendored ABI declarations replace
external headers where practical so runtime-optional libraries do not become mandatory link
dependencies. `libuuid` remains in `LDLIBS` and is excluded from loader tests.

Scheme capability APIs return records containing availability, version, capabilities, and failure
reason. Feature APIs convert loader failures into structured net or crypto errors that identify
the dependency and failed requirement.

### Readiness Operations

Every composite nonblocking operation is represented by a common operation contract with:

- state: `pending`, `completed`, `failed`, or `cancelled`;
- one or more poll targets and their read/write interests;
- an absolute deadline or next timer wake-up;
- a nonblocking `step!` action;
- cancellation and cleanup actions;
- a result or failure condition after completion.

Creating or stepping an operation may perform immediately available work but never waits. When a
step would block, it updates the poll targets and deadline before returning. Manual callers use
`poll` and call `step!` again. A fiber scheduler registers the same targets and timer, suspends the
fiber, and retries the operation after readiness. There are no per-operation Scheme threads,
notifier socket pairs, hidden polling loops, or busy waits.

Simple socket-like operations retain direct one-attempt APIs, but would-block results are explicit
rather than overloaded `#f` values. Composite HTTP, FTP, SCP, and gRPC work uses operation records.
Futures may be offered later as convenience wrappers over the readiness contract; they are not
the nonblocking foundation.

Socket, TLS, SSH, SFTP, SCP, HTTP, and WebSocket operations use their underlying descriptors and
protocol state machines. FTP uses libcurl's multi socket/timer interface instead of
`curl_easy_perform`. gRPC uses one shared completion-queue driver that signals a pollable
descriptor; it does not create one Scheme thread per RPC.

### FTP, SFTP, And SCP

FTP sessions own reusable libcurl multi/easy resources so control and data connections can be
reused. `ftp-file` represents one sequential FTP data transfer, not a random-access remote file.
Opening specifies read or write direction and transfer policy. Closing completes and validates the
transfer. Public read, write, port, blocking, and nonblocking APIs make this distinction explicit.

FTP gains MLSD/MLST parsing, structured directory entries, stat, explicit FTPS modes, progress,
resume/range, overwrite policy, and recursive transfer helpers. Existing active/passive controls
remain FTP-specific.

SFTP gains structured attributes, streaming directory handles, recursive operations, chmod,
chown, utime, symlink/readlink, normalized paths, progress, resume, and overwrite policy. A
client-side logical working directory supports interactive use and is documented as local path
resolution rather than SFTP protocol state.

SCP gains progress, resume/overwrite policy where the protocol permits it, remote stat, symlink
handling, and recursive filters. Limitations imposed by SCP wire semantics are reported rather
than simulated silently.

Interactive FTP and SFTP clients support `ls`, `pwd`, `cd`, `mkdir`, `rmdir`, `rm`, `rename`,
`get`, `put`, and `quit` with explicit connection and authentication arguments.

### HTTP, WebSocket, And gRPC

HTTP request and response bodies accept streaming sources and sinks. Bytevector, string, file,
and in-memory response APIs remain convenience layers. Streaming is the foundation for large
downloads, uploads, compression, multipart data, trailers, and bounded memory use.

HTTP adds compression, cookies, authentication, proxy support, multipart helpers, trailer
exposure, and explicit connection-pool controls. HTTP server handlers are stored in a hashtable
keyed by normalized method and path. Mutation and lookup behavior is documented.

HTTP/2 is implemented through an optionally loaded nghttp2 adapter for client and server paths.
ALPN selects HTTP/2 for TLS sessions, while HTTP/1.1 remains available and is used when HTTP/2 is
not negotiated. nghttp2 participates in loader capability and version reporting.

WebSocket adds TLS server configuration, permessage-deflate, subprotocol negotiation results,
close code/reason access, fragmentation controls, and ping/pong deadline management. File
transfers use bounded binary chunks with metadata and completion frames.

gRPC adds TLS credentials, deadlines, compression, richer status details, normalized metadata,
reflection, and generated bindings. Scheme protobuf support implements wire encoding/decoding. A
development-time `protoc` plugin generates record types, constructors, accessors, codecs, client
stubs, and server registration helpers. `protoc` is not a runtime shared-library dependency.

HTTP/HTTPS, WebSocket/WSS, and gRPC/TLS examples stream file uploads and downloads and validate
the final SHA-256 digest.

### Address, DNS, Errors, Poll, Socket, IP, And URI

An optionally loaded c-ares backend supplies scheduler-friendly DNS resolution with family, type,
timeout, canonical name, and structured partial-result support. Existing libc resolution remains
available as an explicitly blocking path. Address selection adds ordered filtering and connection
attempt helpers for IPv4, IPv6, Unix-domain, service-name, and multi-result cases.

Net errors gain protocol status, OS error number, operation, endpoint, retryability, and cause
fields. Matching helpers support precise tests without string-only assertions.

Poll gains absolute-deadline loops, operation-aware waiting, port/file-descriptor resources, and
complete error, hangup, and invalid-event propagation. Socket gains nonblocking connect, datagram
helpers, dual-stack controls, additional options, consistent partial I/O, and half-close tests.

IP adds IPv4-mapped IPv6 behavior, link-local and site-local helpers, additional special-use
ranges, CIDR merge/split, and parser/formatter property tests.

URI adds construction and update helpers, explicit raw and decoded component choices, expanded
RFC 3986 relative-resolution coverage, and IDNA through an optionally loaded libidn2 backend.

Private FFI vector shapes remain private. Their invariants are documented internally, tested
through public modules, and converted to documented public records before crossing the public API.

### SSH And TLS

SSH stdout and stderr are independent readable streams over the same channel descriptor. Public
stderr read, read-into, and nonblocking operations preserve stream identity. A readiness event may
cause callers to advance either stream until each would block. Input, output, and error ports do
not spawn hidden reader threads.

SSH also adds local and remote forwarding, keepalive configuration, environment requests,
subsystem requests, keyboard-interactive authentication, known-host inspection/update, and
explicit agent/key selection.

TLS adds negotiated ALPN access, OCSP stapling and validation, serializable or opaque resumable
session records as OpenSSL permits, server SNI certificate selection, certificate-chain
inspection, and configurable protocol/cipher policy.

## Public API Documentation

All exported procedures and macros retain adjacent `#|proc:...|#` or `#|macro:...|#`
documentation, meaningful public parameter names, and `pcheck`. Documentation states every
parameter, procedural parameter signature and behavior, return type/value, ownership, blocking
behavior, would-block state, cancellation effect, and failure mode. Lines remain within 100
characters.

Public record definitions receive adjacent `#|record:name|#` documentation describing how values
are constructed, which accessors are public, field meanings, mutability, resource ownership,
lifetime, and valid state transitions. Procedures returning records link their result description
to that contract.

## Error Handling And Cleanup

Missing or incompatible optional libraries fail at feature use, never at `(chezpp)` import.
Partial initialization unwinds all native resources. Every operation has one idempotent cleanup
path shared by completion, failure, cancellation, timeout, and explicit close. Ports, sockets,
channels, sessions, directory streams, transfer handles, completion queues, and temporary files
are closed on all paths.

Cancellation is cooperative and observable. It changes operation state immediately, unregisters
poll targets, requests protocol cancellation, and retains enough diagnostic state to distinguish
cancellation from timeout or transport failure.

## Verification

Unit and `mat` tests cover parsers, records, state transitions, deadlines, cancellation, partial
I/O, and negative paths. Negative test cases include comments describing the expected error and
blank lines between cases. Successful net tests produce no stdout or stderr.

Loader tests build controlled stub libraries for absent dependencies, missing symbols,
incompatible versions, compatible versions, concurrent initialization, and diagnostic text.
`readelf` verifies that `libchezpp.so` directly needs only libc and the exempt `libuuid` among the
libraries in this design.

Local integration fixtures start FTP/FTPS, SSH/SFTP/SCP, HTTP/HTTPS, WebSocket/WSS, and gRPC/TLS
services. Transfers use deterministic files and compare SHA-256 hashes. Tests cover cancellation,
resume, overwrite, and bounded-memory streaming. SSH-family examples accept credentials through
arguments or environment and may use the local `tester` account; committed files do not embed the
password.

Examples include interactive FTP and SFTP clients and upload/download programs for FTP/FTPS,
SFTP, SCP, HTTP/HTTPS, WebSocket/WSS, and gRPC/TLS. A verification driver downloads:

- `https://ftp.gnu.org/gnu/emacs/windows/emacs-30/emacs-30.2.zip`, expected SHA-256
  `414d3a1a21147af257ebd98bdd15976fdcb5ed0563f6de89f76d4a4b5dad9c72`;
- `https://mirrors.tuna.tsinghua.edu.cn/archlinux/iso/2026.07.01/`
  `archlinux-2026.07.01-x86_64.iso`, expected SHA-256
  `e86295dc0bdf9b85a5a9256810c553239689d2ae8e80eeec81b4e2e910d8a6c0`.

Fallback URLs may be documented when the supplied locations are unavailable, but the expected
filename and checksum must identify the same artifact. External large downloads remain examples,
not `mat` tests.

Final release gates are:

1. Resolve the missing `chezpp/c/scheme.h` baseline prerequisite.
2. Run `make clean && make` from the project root.
3. Run focused tests after every phase and the complete net test set at the end.
4. Check every changed Scheme source for balanced parentheses.
5. Run loader linkage and missing/incompatible-library tests.
6. Run local upload/download integration for every required protocol and secure variant.
7. Smoke-test interactive FTP and SFTP command workflows.
8. Run the external download examples and verify the supplied SHA-256 values.
9. Audit exported documentation, record documentation, and return-value descriptions.

## Plan Decomposition

The implementation is specified by one master index and five executable plans:

1. Optional dependency loader and capability reporting.
2. Common readiness operations and protocol adapters.
3. FTP/SFTP/SCP/SSH parity and interactive transfer workflows.
4. HTTP/WebSocket/gRPC features, protobuf generation, and file transfer.
5. Remaining Follow-Up modules, documentation audit, and full-system verification.

Each executable plan uses test-first steps, exact file paths and commands, focused commits, and an
explicit handoff gate to the next phase.
