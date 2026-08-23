# Net Hardening Overall Status Handoff

## Workspace

- Worktree: `/home/maoif/SSD/Projects/chezpp/.worktrees/net-hardening-plan`
- Branch: `net-hardening-plan`
- HEAD: `b12b4ae net: record final hardening verification status`
- Implementation: `4150676 net: add deterministic HTTP/2 lifecycle regressions`
- Status audited: 2026-08-23
- The HTTP/2 corrective implementation and regression coverage are committed.

The HTTP/2 regression work described by this handoff is complete. Preserve unrelated user-owned
changes and captures while staging the implementation and status-ledger commits.

Preserve unrelated user-owned changes and captures. Do not stage, delete, or revert:

- `chezpp/parser.ss`
- `chezpp/c/scheme.h`
- existing handoffs other than this file and unrelated plan edits
- `tests/*.stdout` and `tests/*.stderr`

The five status-ledger edits listed below are uncommitted. They were added only to the current
status sections, but those files already contained user-owned changes, so inspect hunks before
staging:

- `docs/superpowers/plans/2026-08-01-net-roadmap-master.md`
- `docs/superpowers/plans/2026-08-01-net-application-protocols.md`
- `docs/superpowers/plans/2026-08-01-net-follow-up-verification.md`
- `docs/superpowers/plans/2026-08-18-http2-cooperative-transport-driver.md`
- `docs/superpowers/handoffs/2026-08-19-net-overall-status-next-session.md`

This handoff is included in the final status-ledger commit.

## Overall Phase Status

| Phase | Status | Evidence and remaining work |
| --- | --- | --- |
| 0 | Complete | Foundation work is incorporated into the roadmap history. |
| 1 | Complete | Optional loading, capability records, ABI validation, and linkage gates passed. |
| 2 | Complete | Readiness operations, cancellation, resolver lifetime, and the Phase 2 gate passed. |
| 3 | Complete | FTP/FTPS, SFTP, SCP policy and transfer verification passed. |
| 4 | Complete | HTTP/2 lifecycle work and deterministic TLS/cancellation regressions are at `4150676`. |
| 5 | Complete | Local completion gates, final review, and both pinned external downloads passed on 2026-08-23. |

The roadmap and its corrective plan are complete. The authoritative HTTP/2 corrective plan is
`docs/superpowers/plans/2026-08-18-http2-cooperative-transport-driver.md`; the final release gate is
in `docs/superpowers/plans/2026-08-01-net-follow-up-verification.md`.

## Fresh Evidence From 2026-08-23

The following passed after `4150676`:

- `make clean && make`.
- Ten consecutive focused `net-http` runs.
- The complete protobuf/net suite from the final release gate.
- Optional loader, optional hash linkage, and optional library linkage checks.
- Generated protobuf bindings reproduced in a temporary directory and matched `tests/generated`.
- Public API documentation and changed-Scheme balance checks.
- Native linkage audit: direct dependencies were only `libuuid.so.1` and `libc.so.6`.
- Static audit found no thread-backed net operations.
- Ten local transfer variants passed with SHA-256
  `080acf35a507ac9849cfcba47dc2ad83e01b75663a516279c8b9d243b719643e`.
- `git diff --check`.

- External Emacs ZIP SHA-256:
  `414d3a1a21147af257ebd98bdd15976fdcb5ed0563f6de89f76d4a4b5dad9c72`.
- External Arch Linux ISO SHA-256:
  `e86295dc0bdf9b85a5a9256810c553239689d2ae8e80eeec81b4e2e910d8a6c0`.

This evidence was rerun after the regression edits and final implementation commit.

## Final Review Findings

The final HTTP/2 review found no Critical or Important regressions in `4150676`:

1. **Deterministic TLS read-side `WANT_WRITE` coverage is fixed.**
   Production code records `net-would-block-events` in `read-http2-input!` and merges `write` in
   `http2-transport-poll-events`, and the MAT now forces this path and inspects
   `net-operation-poll-targets`. The regression passes in `4150676`.

2. **Cancellation during scheduler activity is fixed.**
   The review originally stated that `cancel-http2-request!` directly deletes active stream state.
   That statement does not match `a117955`: it only appends to the transport cancellation queue;
   `flush-http2-cancellations!` performs table deletion, lifecycle transition, reset queueing, and
   scheduler-owned output work. The deterministic regression cancels an active sibling while
   scheduler event processing is underway and asserts the cancelled state does not complete
   internally. The production guard and regression pass in `4150676`.

3. **The HTTP/2 test server failure path is hardened.**
   `drain-output!` and transport reads now suppress conditions only after explicit shutdown;
   unexpected fixture failures are retained and raised by the server stop procedure.

4. **Sink cleanup proof uses owned-path evidence.**
   `net-http2-download-cancel-closes-sink` verifies that the owned `.part` path is no longer present
   in `/proc/self/fd`; this is indirect finisher evidence but is a documented quality follow-up,
   not a release blocker.

The reviewer ran a forced rebuild, `timeout 120s make -B -C tests test-some TEST=net-http`, and
`git diff --check`; all passed against `4150676`.

## Completed Sequence

1. Added and verified deterministic TLS `WANT_WRITE` and cancellation-during-event-processing
   regressions with private test seams.
2. Hardened fixture failure propagation and replaced aggregate sink-FD counting with owned-path
   evidence.
3. Ran every local completion command, both external downloads, and the final independent review.
4. Updated all five current-status sections to 2026-08-23.

## Completion Commands

```bash
make clean && make
for run in 1 2 3 4 5 6 7 8 9 10; do
  timeout 30s make -C tests test-some TEST=net-http || exit $?
done
make -C tests test-some TEST='protobuf protobuf-codegen net-loader net-operation net-transfer \
net-errors net-core net-address net-dns net-ip net-uri net-http net-ftp net-ssh net-sftp net-scp \
net-websocket net-grpc net-docs'
make -C tests test-optional-library-loader
make -C tests test-optional-hash-linkage
make -C tests test-optional-library-linkage
make protobuf-generate
git diff --exit-code -- tests/generated
./chez++ --script tools/check-public-api-docs.ss \
  chezpp/net chezpp/protobuf chezpp/optional-library.ss
./examples/net/file-transfer/verify-local-transfers.sh
mkdir -p /tmp/chezpp-external-download-state
./examples/net/file-transfer/verify-external-downloads.sh \
  /tmp/chezpp-external-download-state
git diff --check
```

The external verifier has `aria2c` available. No partial Emacs or Arch artifact was found in `/tmp`
on 2026-08-23. Passing hashes must be:

- Emacs 30.2 ZIP:
  `414d3a1a21147af257ebd98bdd15976fdcb5ed0563f6de89f76d4a4b5dad9c72`
- Arch Linux 2026.07.01 ISO:
  `e86295dc0bdf9b85a5a9256810c553239689d2ae8e80eeec81b4e2e910d8a6c0`

The project-required build is always `make clean && make`. The Python parenthesis checker can
falsely flag the existing `#\;` character literal in `http.ss`; use the Chez checker or clean build
for that file and check every other changed Scheme file normally.

## Commit And Staging Boundaries

Before committing, inspect `git status --short`, `git diff --cached`, and `git diff --check`. Stage
explicit paths only. Do not stage the preserved parser/header files, unrelated plans or handoffs,
or test capture files. Implementation is committed through `4150676`; only deliberately selected
status-ledger edits belong in the final documentation commit.

The deterministic TLS regression, focused stress loop, full local gate, both external hashes, final
review, and final status updates are fresh and green.
