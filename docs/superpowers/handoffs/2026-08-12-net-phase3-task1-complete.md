# Net Phase 3 Task 1 Completion Handoff

## Current State

Phase 3 Tasks 1 and 2 in `docs/superpowers/plans/2026-08-01-net-transfer-parity.md` are complete.
The next work is Task 3, FTP metadata, FTPS modes, resume policy, and recursion.

## Completed Contract

`(chezpp net transfer)` exports an immutable `transfer-policy` record. Resume accepts `never`,
`resume`, or an exact nonnegative offset. Overwrite accepts `error`, `replace`, or `skip`. Chunk
size must be positive. The optional progress procedure receives protocol, direction, path,
completed bytes, and total bytes or `#f`.

The aggregate `(chezpp net)` library exports the transfer library, and `tests/net-transfer.ss` is
wired into the standard network test set.

## Verification

The following passed on 2026-08-12:

```bash
make clean && make
cd tests && make test-some TEST='net-transfer net-ftp net-ssh net-sftp net-scp'
scheme --script tools/check-scheme-balance.ss chezpp/net/transfer.ss tests/net-transfer.ss
git diff --check
```

All generated stdout and stderr captures were empty.

## Task 2 Completion

FTP sessions now own a reusable native libcurl multi handle. `ftp-file` exposes sequential read,
write, nonblocking, all-bytes, close, and dynamic-extent APIs. Callback buffers are bounded by the
caller-driven chunk flow, ports stream directly without temporary files, and upload close waits for
the final FTP response. Closing a session cancels and invalidates its active file before releasing
the multi handle.

The fixture records accepted control connections. Sequential upload and download through one
session use exactly one connection.

Task 2 verification added the following successful commands:

```bash
make clean && make
cd tests && timeout 240s make test-some \
  TEST='net-transfer net-ftp net-operation net-core net-http net-ssh net-sftp net-scp \
net-websocket net-grpc'
```

## Next Work

Implement Task 3 from the transfer-parity plan. Build MLSD/MLST parsing and structured entries on
the persistent session/file ownership established by Task 2. Preserve legacy listing compatibility
only through the explicit raw-list API required by the plan.

## Preserved User-Owned Paths

Do not modify, stage, or revert the paths listed in the Phase 2 completion handoff. In particular,
`chezpp/parser.ss` and the older untracked handoff documents remain user-owned.
