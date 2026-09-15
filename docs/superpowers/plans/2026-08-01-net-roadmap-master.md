# Net Roadmap Master Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development
> (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Complete the approved net roadmap through five ordered, independently verified phases.

**Architecture:** Establish optional native dependency loading and one readiness-operation
contract before extending individual protocols. Build protocol parity and streaming application
features on those foundations, then finish the remaining net modules and run full transfer,
documentation, and compatibility gates.

**Tech Stack:** ChezScheme 10, Chezpp libraries and `pcheck`, C11/POSIX FFI shims, OpenSSL 3,
libcurl multi, libssh, libwebsockets, gRPC C core, nghttp2, c-ares, libidn2, `mat`, and GNU Make.

## Current Status (2026-08-23)

Phases 0-5 are complete through HTTP/2 corrective commit `4150676`. Response-sink isolation and
cleanup, scheduler-owned cancellation/reset work, final-read EOF handling, TLS read-readiness
preservation, accepted-stream GOAWAY behavior, and deterministic TLS/cancellation regressions are
implemented. The clean build, ten-run `net-http` stress loop, complete net/protobuf suite,
optional loader/linkage gates, generated-binding comparison, documentation audit, Scheme balance,
native linkage audit, ten-variant local transfer verification, and external download gate pass.

The pinned external artifacts match Emacs SHA-256
`414d3a1a21147af257ebd98bdd15976fdcb5ed0563f6de89f76d4a4b5dad9c72` and Arch Linux SHA-256
`e86295dc0bdf9b85a5a9256810c553239689d2ae8e80eeec81b4e2e910d8a6c0`.

---

## Governing Specification

Implement against
`docs/superpowers/specs/2026-08-01-net-roadmap-design.md`. The following decisions are fixed:

- libc and `libuuid` are the only dynamic-loading exceptions; `libuuid` stays in `LDLIBS`.
- Nonblocking APIs expose readiness operations and never create one Scheme thread per operation.
- Required file transfer protocols are FTP/FTPS, SFTP, SCP, HTTP/HTTPS, WebSocket/WSS, and
  gRPC/TLS-enabled gRPC. Raw SSH file transfer is excluded.
- Existing untracked `examples/net/` files in the main checkout are starting inputs.
- HTTP handlers use a hashtable.
- The entire Follow-Up Roadmap from the May 13 plan is included.

## Plan Order

Execute these plans in order. Do not start a later plan until the prior release gate passes.

1. `docs/superpowers/plans/2026-08-01-net-optional-dependencies.md`
2. `docs/superpowers/plans/2026-08-01-net-readiness-operations.md`
3. `docs/superpowers/plans/2026-08-01-net-transfer-parity.md`
4. `docs/superpowers/plans/2026-08-01-net-application-protocols.md`
5. `docs/superpowers/plans/2026-08-01-net-follow-up-verification.md`

## Cross-Phase File Map

### Build And Shared Runtime

- Modify: `Makefile`
- Modify: `chezpp.ss`
- Modify: `chezpp/net.ss`
- Modify: `tests/Makefile`
- Modify: `Makefile` to include the `scheme.h` beside the selected ChezScheme runtime.
- Create: `chezpp/c/optional_library.h`
- Create: `chezpp/c/optional_library.c`
- Create: `chezpp/optional-library.ss`
- Create: `chezpp/net/operation.ss`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/private.ss`

### Native Protocol Shims

- Modify: `chezpp/c/crypto.c`
- Modify: `chezpp/c/digest.c`
- Modify: `chezpp/c/optional_hash.c`
- Modify: `chezpp/c/net/ftp.c`
- Modify: `chezpp/c/net/grpc.c`
- Modify: `chezpp/c/net/poll.c`
- Modify: `chezpp/c/net/socket.c`
- Modify: `chezpp/c/net/ssh.c`
- Modify: `chezpp/c/net/tls.c`
- Modify: `chezpp/c/net/websocket.c`
- Create: `chezpp/c/net/dns.c`
- Create: `chezpp/c/net/http2.c`
- Create: `chezpp/c/net/idna.c`

### Scheme Protocol Libraries

- Modify every library under `chezpp/net/` listed in the Follow-Up Roadmap.
- Create: `chezpp/protobuf.ss`
- Create: `chezpp/protobuf/descriptor.ss`
- Create: `chezpp/protobuf/wire.ss`
- Create: `tools/protoc-gen-chezpp.ss`

### Tests And Examples

- Modify: `tests/net-common.ss`
- Modify all existing `tests/net-*.ss` files.
- Create focused tests named by each phase plan.
- Import and modify the main checkout's untracked `examples/net/` tree.
- Create interactive FTP/SFTP clients and secure transfer verification drivers.

## Phase 0: Establish The Isolated Baseline

This task runs once before Phase 1. It resolves the known baseline failure without absorbing
unrelated changes from the dirty main checkout.

**Files:**
- Create from approved input: `examples/net/**`
- Create: `tools/check-scheme-balance.ss`
- Modify: `Makefile`

- [ ] **Step 1: Confirm worktree and source checkout paths**

Run:

```bash
git rev-parse --show-toplevel
git branch --show-current
git -C /home/maoif/SSD/Projects/chezpp status --short -- chezpp/c/scheme.h examples/net
```

Expected: the current branch is the implementation branch created from `net-hardening-plan`. The
source checkout reports the untracked examples and an obsolete ChezScheme 10.0 header. Do not copy
that header because the selected runtime is 10.5.0-pre-release.1.

- [ ] **Step 2: Import only the approved example inputs**

Run from the implementation worktree:

```bash
mkdir -p examples/net
cp -a /home/maoif/SSD/Projects/chezpp/examples/net/. examples/net/
```

Expected: `git status --short` lists only `examples/net/` before phase-specific edits.

- [ ] **Step 3: Make the Chez header version check explicit**

Replace the `# TODO include Chez header file` build gap with:

```make
SCHEME_EXE := $(realpath $(SCHEME_SCRIPT))
SCHEME_INCLUDE_DIR := $(dir $(SCHEME_EXE))

.PHONY: check-scheme-header
check-scheme-header:
	@test -r "$(SCHEME_INCLUDE_DIR)/scheme.h" || { \
	  echo "scheme.h not found beside $(SCHEME_EXE)" >&2; exit 1; \
	}
	@header_version=$$(sed -n 's/^#define VERSION "\([^"]*\)"/\1/p' \
	  "$(SCHEME_INCLUDE_DIR)/scheme.h"); \
	runtime_version=$$($(SCHEME) --version 2>&1); \
	test -n "$$header_version"; \
	test "$$header_version" = "$$runtime_version" || { \
	  echo "scheme.h version $$header_version does not match ChezScheme $$runtime_version" >&2; \
	  exit 1; \
	}

CFLAGS += -I$(SCHEME_INCLUDE_DIR)

libchezpp.so: check-scheme-header
```

- [ ] **Step 4: Add a Scheme source balance checker**

Create `tools/check-scheme-balance.ss`:

```scheme
(import (chezscheme))

(define check-file
  (lambda (path)
    (call-with-input-file path
      (lambda (input)
        (let loop ()
          (let ([datum (read input)])
            (unless (eof-object? datum)
              (loop))))))))

(for-each check-file (command-line-arguments))
```

Run it against representative library files:

```bash
scheme --script tools/check-scheme-balance.ss chezpp/net/http.ss chezpp/net/grpc.ss
```

Expected: exit 0. The Scheme reader reports unbalanced delimiters and unterminated strings/comments
as read errors.

- [ ] **Step 5: Verify the imported examples contain no obsolete RPC files**

Run:

```bash
rg -n 'define-rpc|rpc-open|file-transfer-rpc|echo-server-rpc' examples/net
```

Expected before cleanup: obsolete custom RPC references are reported. Remove only those files and
references because the custom RPC library was deleted; retain gRPC examples.

- [ ] **Step 6: Build the clean baseline**

Run:

```bash
make clean && make
```

Expected: exit 0. Treat any output ending in an error as a baseline blocker and resolve it before
Phase 1.

- [ ] **Step 7: Run the committed net baseline tests**

Run:

```bash
cd tests && make test-some TEST='net-core net-http net-ftp net-ssh net-sftp net-scp net-websocket net-grpc'
```

Expected: exit 0 and no test stdout/stderr except documented `Expect error` cases.

- [ ] **Step 8: Commit the isolated baseline inputs**

```bash
git add Makefile examples/net tools/check-scheme-balance.ss
git commit -m "net: import transfer examples and locate ChezScheme header"
```

## Cross-Phase API Contract

All phases use the readiness operation defined in Phase 2. Protocol-specific constructors may
return an already completed operation, but they always return a `net-operation` record. The
scheduler-neutral loop is:

```scheme
(let loop ([operation (http-download/nonblocking client uri destination)])
  (net-operation-step! operation)
  (case (net-operation-state operation)
    [(completed) (net-operation-result operation)]
    [(failed) (raise (net-operation-condition operation))]
    [(cancelled) (raise (net-operation-condition operation))]
    [(pending)
     (poll (net-operation-poll-targets operation)
           (net-operation-remaining-timeout-ms operation))
     (loop operation)]))
```

No phase may reintroduce `fork-thread` into `chezpp/net/ftp.ss`, `chezpp/net/scp.ss`,
`chezpp/net/grpc.ss`, or `chezpp/net/http.ss`.

## Commit Policy

Use subsystem-prefixed commits for localized changes. Each phase plan gives exact commit messages.
Do not combine unrelated protocol work into one commit. Never commit generated build output,
downloaded large artifacts, credentials, host keys, or temporary server state.

## Final Completion Gate

The roadmap is complete only when all five phase gates pass and the following command sequence is
successful from the project root:

```bash
make clean && make
cd tests && make test-some TEST='net-core net-operation net-loader net-http net-ftp net-ssh net-sftp net-scp net-websocket net-grpc net-address net-dns net-ip net-uri'
cd ..
./tests/optional-library-linkage.sh
./examples/net/file-transfer/verify-local-transfers.sh
./examples/net/file-transfer/verify-external-downloads.sh
```

Expected: build and test commands exit 0; Scheme tests have empty stdout/stderr; every local
protocol and secure variant uploads and downloads with matching SHA-256; both supplied external
artifacts match their expected SHA-256.
