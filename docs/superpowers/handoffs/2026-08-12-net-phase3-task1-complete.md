# Net Phase 3 Task 1 Completion Handoff

## Current State

Phase 3 Task 1 in `docs/superpowers/plans/2026-08-01-net-transfer-parity.md` is complete.
The next work is Task 2, persistent FTP file transfers. Do not skip ahead to FTP metadata because
Task 3 depends on the session and transfer ownership introduced by Task 2.

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

## Next Work

Implement Task 2 from the transfer-parity plan. Its native ABI must make the session own one
libcurl multi handle and every active transfer. Preserve the Phase 2 readiness contract and add
the FTP file API and true streaming ports before starting Task 3.

## Preserved User-Owned Paths

Do not modify, stage, or revert the paths listed in the Phase 2 completion handoff. In particular,
`chezpp/parser.ss` and the older untracked handoff documents remain user-owned.
