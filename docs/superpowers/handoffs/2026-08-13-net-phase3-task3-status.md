# Net Phase 3 Task 3 Status

Task 3 implemented FTP metadata and transfer API parity in the isolated `net-hardening-plan`
worktree.

Completed:

- MLSD parsing with structured immutable directory entries and raw-list compatibility.
- MLST-backed `ftp-stat`, including missing-path behavior.
- `plain`, `explicit`, and `implicit` FTP/FTPS modes.
- Policy-aware sequential transfers with overwrite checks, resume offsets, progress callbacks,
  and native `REST`/`CURLOPT_RESUME_FROM_LARGE` plumbing.
- Recursive download and upload helpers with directory creation and link/unknown-type rejection.
- Test fixture shutdown no longer joins intentionally persistent FTP control handlers.

Verification:

```text
scheme --script tools/check-scheme-balance.ss ...   PASS
make clean && make                                 PASS
make test-some TEST='net-transfer net-ftp'          PASS
```

The focused regression captures were empty. End-to-end resume and recursive fixture coverage is
deferred: the current in-process FTP fixture cannot reliably complete REST transfers before its
control connection timeout. The APIs and native resume plumbing remain ready for a fixture that
supports REST consistently.
