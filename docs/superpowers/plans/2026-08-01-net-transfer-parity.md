# Net Transfer Parity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development
> (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give FTP/FTPS, SFTP, and SCP complete, accurately documented file workflows and provide
interactive FTP/SFTP clients plus secure local upload/download verification.

**Architecture:** FTP file handles model sequential libcurl transfers, SFTP exposes real remote
handles and attributes, and SCP reports its protocol limitations explicitly. Shared policy records
align progress, resume, overwrite, recursion, and path behavior without erasing protocol-specific
semantics.

**Tech Stack:** ChezScheme records/custom ports, libcurl multi, libssh SFTP/SCP, temporary sshd and
FTP/FTPS fixtures, SHA-256 digest APIs, and command-line examples.

---

### Task 1: Transfer Policy And Progress Contracts

**Status (2026-08-12): Complete.** The shared immutable policy, validation, aggregate export,
focused tests, clean build, and existing FTP/SSH/SFTP/SCP regression suites pass. The historical
red-step command was not run separately. Task 2 is next.

**Files:**
- Create: `chezpp/net/transfer.ss`
- Modify: `chezpp/net.ss`
- Create: `tests/net-transfer.ss`
- Modify: `tests/Makefile`

- [x] **Step 1: Write transfer-policy tests**

```scheme
(mat net-transfer-policy
     (let ([policy (make-transfer-policy 'resume 'replace 65536 #f)])
       (and (transfer-policy? policy)
            (eq? 'resume (transfer-policy-resume policy))
            (eq? 'replace (transfer-policy-overwrite policy))
            (= 65536 (transfer-policy-chunk-size policy))
            (not (transfer-policy-progress policy))))

     ;; Progress receives protocol, direction, path, completed bytes, and total or #f.
     (let ([seen '()])
       (transfer-report-progress!
        (make-transfer-policy
         'never 'error 4096
         (lambda (protocol direction path completed total)
           (set! seen (list protocol direction path completed total))))
        'ftp 'upload "/remote/a" 4 10)
       (equal? seen '(ftp upload "/remote/a" 4 10))))
```

- [ ] **Step 2: Run and verify failure (historical red step not run separately)**

```bash
cd tests && make test-some TEST='net-transfer'
```

Expected: FAIL because `(chezpp net transfer)` does not exist.

- [x] **Step 3: Implement and document the record**

Export:

```scheme
transfer-policy?
make-transfer-policy
transfer-policy-resume
transfer-policy-overwrite
transfer-policy-chunk-size
transfer-policy-progress
default-transfer-policy
```

Valid resume modes are `never`, `resume`, and an exact non-negative offset. Valid overwrite modes
are `error`, `replace`, and `skip`. The progress procedure signature is
`(protocol direction path completed-bytes total-bytes-or-#f) -> unspecified`. Apply `pcheck` and
document the record's immutable fields above its definition.

- [x] **Step 4: Build and test**

```bash
make clean && make
cd tests && make test-some TEST='net-transfer'
cd ..
git add chezpp/net/transfer.ss chezpp/net.ss tests/net-transfer.ss tests/Makefile
git commit -m "net: add shared transfer policy"
```

### Task 2: FTP Sequential File Handles And Persistent Sessions

**Status (2026-08-12): Complete.** FTP sessions own a reusable libcurl multi handle and one active
sequential file. File and port transfers stream through bounded callbacks, upload close observes
the final server response, session close cancels active ownership, and the fixture proves one
control connection is reused across sequential upload and download.

**Files:**
- Modify: `chezpp/c/net/ftp.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/ftp.ss`
- Modify: `tests/net-ftp.ss`
- Modify: `tests/net-ftp-server-process.ss`

- [x] **Step 1: Add FTP file lifecycle tests**

```scheme
(mat net-ftp-file
     (with-test-ftp-session
      (lambda (session)
        (let ([file (ftp-open-file session "/incoming/data.bin" 'write
                                   default-transfer-policy)])
          (dynamic-wind
            void
            (lambda ()
              (and (ftp-file? file)
                   (= 4 (ftp-write file #vu8(1 2 3 4)))
                   (eq? 'write (ftp-file-direction file))))
            (lambda () (ftp-close-file file))))))

     (with-test-ftp-session
      (lambda (session)
        (call-with-ftp-file
         session "/incoming/data.bin" 'read default-transfer-policy
         (lambda (file)
           (equal? #vu8(1 2 3 4) (ftp-read-all file)))))))

     ;; A closed transfer cannot be read or written.
     (with-test-ftp-session
      (lambda (session)
        (let ([file (ftp-open-file session "/incoming/data.bin" 'read)])
          (ftp-close-file file)
          (error? (ftp-read file 1))))))
```

- [ ] **Step 2: Run and verify failure (historical red step not run separately)**

```bash
cd tests && make test-some TEST='net-ftp'
```

Expected: FAIL because `ftp-file` APIs do not exist.

- [x] **Step 3: Add native session and transfer ownership**

Replace stateless URL calls with an opaque `chezpp_ftp_session` owning one multi handle, reusable
easy handles, credentials, FTPS policy, current logical directory, and active transfer list. Add
opaque `chezpp_ftp_transfer` handles for sequential read/write callbacks. Closing a session cancels
and closes every owned transfer before cleaning the multi handle.

- [x] **Step 4: Add the Scheme record and APIs**

Export and document:

```scheme
ftp-file?
ftp-file-direction
ftp-file-path
ftp-file-closed?
ftp-open-file
ftp-close-file
ftp-read
ftp-read!
ftp-read-all
ftp-write
ftp-write-all
ftp-read/nonblocking
ftp-read!/nonblocking
ftp-write/nonblocking
ftp-write-all/nonblocking
call-with-ftp-file
```

An FTP file is sequential. Reading a write file or writing a read file raises a structured FTP
error. Nonblocking procedures return an explicit would-block value or immediate byte count/data;
opening and final close use readiness operations because they may exchange control replies.

- [x] **Step 5: Make FTP ports stream rather than stage temporary files**

Reimplement `open-ftp-input-port` and `open-ftp-output-port` over the FTP file callbacks. Port close
must close the transfer exactly once and surface final server errors. Remove temporary download
and upload staging from port constructors.

- [x] **Step 6: Verify connection reuse**

Extend the fixture server to count accepted control connections. List, upload, download, and stat
through one session; assert the count remains one unless the server explicitly closes it.

- [x] **Step 7: Build and test**

```bash
make clean && make
cd tests && make test-some TEST='net-transfer net-ftp'
cd ..
git add chezpp/c/net/ftp.c chezpp/net/ffi.ss chezpp/net/ftp.ss tests/net-ftp.ss \
  tests/net-ftp-server-process.ss
git commit -m "net: add persistent FTP file transfers"
```

### Task 3: FTP Metadata, FTPS Modes, Resume, And Recursion

**Files:**
- Modify: `chezpp/c/net/ftp.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/ftp.ss`
- Modify: `tests/net-ftp.ss`
- Modify: `tests/net-ftp-server-process.ss`
- Modify: `tests/net-ftp-common.ss`

- [x] **Step 1: Add MLSD/MLST parsing tests**

```scheme
(mat net-ftp-directory-entry
     (let ([entry (ftp-parse-mlsd-line
                   "type=file;size=12;modify=20260801123456;perm=rw; sample.txt")])
       (and (ftp-directory-entry? entry)
            (string=? "sample.txt" (ftp-directory-entry-name entry))
            (eq? 'file (ftp-directory-entry-type entry))
            (= 12 (ftp-directory-entry-size entry))
            (string=? "20260801123456" (ftp-directory-entry-modify entry))
            (equal? '(read write) (ftp-directory-entry-permissions entry)))))
```

Add negative cases for missing space delimiter, invalid size, duplicate fact, and unknown fact.
Each negative case has an error-case comment and blank line.

- [x] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-ftp'
```

Expected: FAIL because structured FTP entries and MLST are absent.

- [x] **Step 3: Implement structured metadata**

Export `ftp-directory-entry?` and accessors for name, type, size, modify, unique, permissions,
owner, group, and raw facts. `ftp-list` returns a list of entries by default; add
`ftp-list/raw` for the old bytevector listing. `ftp-stat` issues MLST and returns one entry or `#f`
for a missing path.

- [x] **Step 4: Add explicit FTPS modes**

Use symbols `plain`, `explicit`, and `implicit`. URI defaults are `ftp -> plain` and
`ftps -> implicit` for compatibility; `ftp-open` accepts an explicit mode override. Map explicit
to `CURLOPT_USE_SSL = CURLUSESSL_ALL` on port 21 and implicit to an `ftps://` URL, normally port
990. Verification remains enabled unless explicitly changed through the existing API.

- [ ] **Step 5: Apply transfer policies**

Use `CURLOPT_RESUME_FROM_LARGE` for upload/download resume, `CURLOPT_RANGE` for bounded reads,
`CURLOPT_XFERINFOFUNCTION` for progress, and `CURLOPT_NOPROGRESS = 0`. Enforce overwrite policy
before starting and preserve partial files only for `resume` mode.

- [x] **Step 6: Add recursive helpers**

Export `ftp-download-directory` and `ftp-upload-directory`. Traverse structured entries, reject
cycles/links that escape the requested root, precreate directories, and report progress per file.

- [x] **Step 7: Build, test, and commit**

Implementation note: policy-aware resume, overwrite, progress, and recursive APIs are present,
including native REST/`CURLOPT_RESUME_FROM_LARGE` plumbing. End-to-end resume and recursive fixture
coverage remains deferred because the current in-process FTP fixture cannot reliably complete a
REST upload/download before its control connection timeout.

```bash
make clean && make
cd tests && make test-some TEST='net-ftp net-transfer'
cd ..
git add chezpp/c/net/ftp.c chezpp/net/ffi.ss chezpp/net/ftp.ss tests/net-ftp.ss \
  tests/net-ftp-server-process.ss tests/net-ftp-common.ss
git commit -m "net: add FTP metadata and resumable transfers"
```

### Task 4: SFTP Attributes, Directory Streams, Paths, And Metadata Mutation

**Files:**
- Modify: `chezpp/c/net/ssh.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/sftp.ss`
- Modify: `tests/net-sftp.ss`
- Modify: `tests/net-ssh-common.ss`

- [ ] **Step 1: Add attribute and directory stream tests**

Add `with-test-sftp-session` to `tests/net-ssh-common.ss`. It starts the existing SSH fixture,
sets `HOME`, authenticates with the generated public key, opens SFTP, invokes the supplied
`(session) -> value` procedure, and closes SFTP/SSH/server with nested `dynamic-wind` forms.

```scheme
(mat net-sftp-attributes
     (with-test-sftp-session
      (lambda (session)
        (let ([attributes (sftp-stat session ".")])
          (and (sftp-attributes? attributes)
               (symbol? (sftp-attributes-type attributes))
               (exact-nonnegative-integer? (sftp-attributes-permissions attributes))
               (or (not (sftp-attributes-size attributes))
                   (exact-nonnegative-integer? (sftp-attributes-size attributes)))))))

     (with-test-sftp-session
      (lambda (session)
        (call-with-sftp-directory
         session "."
         (lambda (directory)
           (let loop ([count 0])
             (let ([entry (sftp-read-directory/nonblocking directory)])
               (cond
                [(net-would-block? entry) (loop count)]
                [(eof-object? entry) (> count 0)]
                [else (loop (+ count 1))]))))))))
```

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-sftp'
```

Expected: FAIL because stat/list return raw FFI vectors and no directory handle is public.

- [ ] **Step 3: Add native metadata and directory handles**

Copy libssh attributes into a stable FFI vector before freeing them. Add opendir, readdir,
closedir, setstat/chmod/chown/utimes, symlink, and readlink bindings. An open directory owns its
SFTP session reference and closes idempotently.

- [ ] **Step 4: Define public records and APIs**

Export documented records `sftp-attributes` and `sftp-directory`, plus:

```scheme
sftp-open-directory
sftp-read-directory
sftp-read-directory/nonblocking
sftp-close-directory
call-with-sftp-directory
sftp-chmod!
sftp-chown!
sftp-utime!
sftp-symlink!
sftp-readlink
sftp-normalize-path
sftp-cwd!
sftp-pwd
```

Document that `sftp-cwd!` changes only session-side path resolution. Absolute paths bypass it.

- [ ] **Step 5: Add policy-aware recursive and resumable transfers**

Extend `sftp-download` and `sftp-upload` with a transfer-policy arity. Use remote/local stat to
choose offsets, seek the SFTP file, invoke progress after each chunk, and implement recursive
helpers that preserve permissions and times when requested.

- [ ] **Step 6: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-sftp net-transfer net-ssh'
cd ..
git add chezpp/c/net/ssh.c chezpp/net/ffi.ss chezpp/net/sftp.ss tests/net-sftp.ss \
  tests/net-ssh-common.ss
git commit -m "net: add structured SFTP filesystem APIs"
```

### Task 5: SCP Policies, Metadata, Symlinks, And Filters

**Files:**
- Modify: `chezpp/c/net/ssh.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/scp.ss`
- Modify: `tests/net-scp.ss`

- [ ] **Step 1: Add overwrite, metadata, and filter tests**

Create a remote tree with two files and one symlink. Assert `error`, `skip`, and `replace`
overwrite modes; assert remote stat fields; and copy recursively with:

```scheme
(lambda (relative-path attributes)
  (not (string-suffix? ".tmp" relative-path)))
```

The recursive filter signature is `(relative-path scp-attributes) -> boolean`.

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-scp'
```

Expected: FAIL because SCP metadata, policies, and filters are absent.

- [ ] **Step 3: Add SCP attributes and stat**

Export `scp-attributes?` with path, type, size, permissions, and modification time accessors.
Implement `scp-stat` through an authenticated SSH command using a fixed argument protocol, not
shell-concatenated user input. Decode one NUL-delimited response record.

- [ ] **Step 4: Apply transfer policy accurately**

SCP cannot resume an arbitrary interrupted protocol transfer portably. Accept `never`; implement
`resume` only by starting a new transfer at a verified offset through the server-side helper; raise
`unsupported` when the server helper is unavailable. Never silently restart from zero.

- [ ] **Step 5: Handle symlinks and recursive filters**

Expose policies `preserve`, `follow`, and `reject`. Default to `reject`. Apply the documented filter
before creating each destination. Prevent followed links from escaping the selected source root.

- [ ] **Step 6: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-scp net-transfer net-ssh'
cd ..
git add chezpp/c/net/ssh.c chezpp/net/ffi.ss chezpp/net/scp.ss tests/net-scp.ss
git commit -m "net: add SCP policy and metadata support"
```

### Task 6: SSH Follow-Up APIs

**Files:**
- Modify: `chezpp/c/net/ssh.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/ssh.ss`
- Modify: `tests/net-ssh.ss`
- Modify: `tests/net-ssh-common.ss`
- Modify: `examples/net/ssh-open-repl.ss`
- Modify: `examples/net/ssh-run-cmd.ss`

- [ ] **Step 1: Add SSH request and authentication tests**

Test environment variables and subsystem requests against the temporary sshd, keepalive option
round trips, explicit private key selection, keyboard-interactive password prompts, known-host
enumeration/update/removal, and local forwarding to a loopback echo server.

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-ssh'
```

Expected: FAIL because the APIs are absent.

- [ ] **Step 3: Add channel request APIs**

Resolve and bind libssh request-env and request-subsystem symbols. Export:

```scheme
ssh-request-environment!
ssh-request-subsystem!
ssh-set-keepalive!
ssh-send-keepalive!
```

Environment names and values are strings; subsystem is a non-empty string. Return the channel or
session on success and raise a structured SSH error otherwise.

- [ ] **Step 4: Add authentication selection**

Export explicit-key authentication taking public/private key paths and passphrase-or-`#f`.
Keyboard-interactive accepts a responder procedure with signature
`(name instruction prompts echo-flags) -> list-of-strings`. Agent authentication accepts an
optional identity constraint instead of trying every agent key.

- [ ] **Step 5: Add known-host management**

Export a `ssh-known-host` record and list/check/add/remove/update procedures. Operations accept an
explicit known-hosts path or use the session path; they never change trust policy silently.

- [ ] **Step 6: Add forwarding**

Implement direct-tcpip local forwarding as readiness operations. Implement remote forwarding with
request, accept, and cancel APIs. Each forwarding listener/channel exposes its descriptor and
closes idempotently.

- [ ] **Step 7: Update examples to multiplex stdout and stderr**

Replace stdout/stderr reader threads in `ssh-open-repl.ss` and `ssh-run-cmd.ss` with one `poll` loop
over channel stream operations. Drain both streams after exit readiness so trailing stderr is not
lost.

- [ ] **Step 8: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-ssh net-sftp net-scp'
cd ..
git add chezpp/c/net/ssh.c chezpp/net/ffi.ss chezpp/net/ssh.ss tests/net-ssh.ss \
  tests/net-ssh-common.ss examples/net/ssh-open-repl.ss examples/net/ssh-run-cmd.ss
git commit -m "net: add SSH forwarding and authentication controls"
```

### Task 7: Interactive FTP And SFTP Clients

**Files:**
- Create: `examples/net/ftp-client.ss`
- Create: `examples/net/sftp-client.ss`
- Create: `examples/net/interactive-transfer-common.ss`
- Modify: `examples/net/file-transfer/file-transfer-ftp.ss`
- Modify: `examples/net/file-transfer/file-transfer-sftp.ss`
- Create: `examples/net/file-transfer/file-transfer-ftps.ss`
- Create: `examples/net/file-transfer/file-transfer-scp.ss`
- Create: `examples/net/file-transfer/verify-ftp-sftp-scp.sh`

- [ ] **Step 1: Implement a shared command parser**

Parse quoted paths without invoking a shell. Return `(command . arguments)` and reject wrong
arities before protocol calls. Supported commands are `ls`, `pwd`, `cd`, `mkdir`, `rmdir`, `rm`,
`rename`, `get`, `put`, and `quit`.

- [ ] **Step 2: Implement the FTP client**

Accept endpoint, username, password source, FTPS mode, and verification options. Drive every
operation through the readiness loop and print structured entries in stable columns. Always close
the current file and session through `dynamic-wind`.

- [ ] **Step 3: Implement the SFTP client**

Accept host, port, username, authentication kind, and authentication argument. Use client-side
working-directory resolution and the same visible commands as FTP.

- [ ] **Step 4: Harden transfer examples**

Make each FTP/FTPS/SFTP/SCP example support both `upload` and `download`, accept source and
destination paths, stream in bounded chunks, print final SHA-256, and exit nonzero on mismatch.
Credentials come from arguments or environment; do not commit `123345`.

- [ ] **Step 5: Add local verification script**

The script starts temporary FTP/FTPS and sshd fixtures, creates a deterministic 16 MiB file,
uploads and downloads through every variant, compares SHA-256, then stops fixtures and removes
temporary state in a trap.

- [ ] **Step 6: Run the examples**

```bash
./examples/net/file-transfer/verify-ftp-sftp-scp.sh
```

Expected: FTP, FTPS, SFTP, and SCP each report matching upload and download hashes.

- [ ] **Step 7: Commit**

```bash
git add examples/net/ftp-client.ss examples/net/sftp-client.ss \
  examples/net/interactive-transfer-common.ss examples/net/file-transfer
git commit -m "net: add interactive and secure transfer examples"
```

### Task 8: Phase 3 Release Gate

**Files:**
- Review all Phase 3 files.

- [ ] **Step 1: Build and run transfer tests**

```bash
make clean && make
cd tests && make test-some TEST='net-transfer net-ftp net-ssh net-sftp net-scp'
```

Expected: exit 0 and empty test stdout/stderr.

- [ ] **Step 2: Run local transfer verification**

```bash
./examples/net/file-transfer/verify-ftp-sftp-scp.sh
```

Expected: all protocol hashes match.

- [ ] **Step 3: Smoke-test interactive clients**

Pipe a deterministic command transcript containing `pwd`, `ls`, `mkdir`, `put`, `get`, `rename`,
`rm`, `rmdir`, and `quit` into both clients. Compare downloaded content and assert exit 0.

- [ ] **Step 4: Audit docs and parentheses**

```bash
rg -n '^  #\|(record|proc):' chezpp/net/{transfer,ftp,sftp,scp,ssh}.ss
./chez++ --script tools/check-scheme-balance.ss chezpp/net/{transfer,ftp,sftp,scp,ssh}.ss
git status --short
```

Expected: every new record/procedure is documented, files are balanced, and status is empty.
