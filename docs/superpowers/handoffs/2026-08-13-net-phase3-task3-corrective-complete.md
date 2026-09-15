# Net Phase 3 Task 3 Corrective Completion

## Result

FTP Task 3 is complete on branch `net-hardening-plan`.

- The fixture logs control commands and proves `REST` is issued for resumed downloads and uploads.
- Resumed transfers preserve local and remote prefixes and invoke progress callbacks.
- Upload resume uses explicit `REST` plus `STOR`; its owned libcurl quote list is always released.
- Nonzero-offset downloads preserve the existing local prefix instead of truncating it.
- Upload close immediately drives libcurl after signaling EOF, avoiding stale-readiness timeouts.
- Overwrite `error`, `skip`, and `replace`, explicit offsets, and failed-download cleanup are tested.
- Recursive upload/download round trips and local symbolic-link rejection are tested.

## Verification

```bash
make clean && make
cd tests && make test-some TEST='net-ftp net-transfer'
cd ..
scheme --script tools/check-scheme-balance.ss \
  chezpp/net/ftp.ss chezpp/net/ffi.ss tests/net-ftp.ss \
  tests/net-ftp-common.ss tests/net-ftp-server-process.ss
git diff --check
```

The focused test stdout and stderr captures were empty. Phase 3 Task 4, structured SFTP filesystem
APIs, is next.
