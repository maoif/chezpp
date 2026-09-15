# Net Phase 3 Task 4 Completion

## Result

SFTP Task 4 is complete on branch `net-hardening-plan`.

- Added stable immutable `sftp-attributes` records with type, size, permissions, uid/gid, and times.
- Added owned `sftp-directory` streams with blocking/nonblocking reads and idempotent closure.
- Added native chmod, chown, utimes, symlink, readlink, and file seek bindings.
- Added normalized client-side path resolution with `sftp-cwd!`, `sftp-pwd`, and `sftp-normalize-path`.
- Applied normalized paths to stat, list, file open, delete, mkdir, rmdir, and rename operations.
- Added recursive SFTP upload/download helpers with metadata preservation and link rejection.
- Corrected SFTP v3 timestamp conversion and the default directory mode (`#o755`).
- Updated SSH/SFTP fixtures and tests for records, directory streams, metadata, paths, recursion,
  and cleanup ordering.

## Verification

```bash
make clean && make
cd tests && make test-some TEST='net-sftp'
cd ..
scheme --script tools/check-scheme-balance.ss \
  chezpp/net/sftp.ss chezpp/net/ffi.ss tests/net-sftp.ss tests/net-ssh-common.ss
git diff --check
```

The SFTP test stdout and stderr captures were empty. Phase 3 Task 5, SCP policies and metadata,
is next.
