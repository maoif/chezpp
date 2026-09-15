# Net Phase 3 Task 5 Completion

## Result

SCP Task 5 is complete on branch `net-hardening-plan`.

- Added stable `scp-attributes` records and safe remote `scp-stat` using an authenticated SFTP
  stat request, with `#f` for missing paths.
- Added overwrite policy handling for SCP downloads/uploads: `error`, `skip`, and `replace`.
- Added structured `unsupported` errors for resume requests when no verified restart helper exists;
  SCP never silently restarts from zero.
- Added focused metadata, missing-path, overwrite, and unsupported-resume tests.
- Existing recursive SCP transfers and nonblocking behavior remain covered by the suite.

## Verification

```bash
make clean && make
cd tests && make test-some TEST='net-scp net-transfer net-ssh'
cd ..
scheme --script tools/check-scheme-balance.ss \
  chezpp/net/scp.ss chezpp/net/ffi.ss tests/net-scp.ss tests/net-ssh-common.ss
git diff --check
```

The SCP, transfer, and SSH test captures were empty. Phase 3 Task 6, SSH follow-up APIs, is next.
