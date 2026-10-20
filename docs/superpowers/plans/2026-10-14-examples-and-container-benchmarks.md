# ChezPP Examples and Container Benchmarks Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add runnable, discoverable ChezPP examples and detailed generic-versus-specialized container benchmarks, and correct existing network examples.

**Architecture:** Add focused executable scripts under `examples/<library>/` and `benchmarks/`, with a shared example convention and a short examples index. Existing public library APIs remain the implementation surface; task 3 repairs SSH stream handling, HTTP protocol coverage, script invocation, and examples that fail verification. Benchmarks use `(chezpp benchmark)`, comparable workloads, and a common detailed text/CSV/JSON reporting convention.

**Tech Stack:** ChezScheme, `chez++ --script=FILE --`, `(chezpp cli)`, `(chezpp benchmark)`, existing ChezPP libraries, project Makefiles and Scheme test suite.

**Spec:** User request in the current session; no separate design document was requested.

**Current status (2026-10-17):** Tasks 1–8 are implemented. Fresh verification includes clean
release/debug builds, a no-OpenSSL/no-websockets/no-libssh build, the full test suite, parser/data/
container/crypto/benchmark integration, HTTP/1 and cleartext H2/HTTPS-H2 loopback requests,
isolated SSH key/agent/password fixtures, SCP file/tree transfers, both forwarding directions,
and the ten-protocol local transfer matrix. Follow-up implementation and verification are recorded
in commits after `38af622`. HTTP/2/HTTPS remain capability-gated when
their optional libraries or an H2-capable curl are unavailable. Review notes are recorded in
`.superpowers/sdd/2026-10-14-examples-and-container-benchmarks/final-review.md`.

## Global Constraints

- Create worktrees under `.worktrees`, use a date-prefixed name, and preserve the worktree after merging.
- Every new Scheme script accepts `--help` and `-h` through `(chezpp cli)` and runs with `chez++ --script=<script.ss> --`.
- New examples use a uniform structure: imports, reusable helpers, CLI declaration, documented target API calls, then the command entry point.
- Add a short comment immediately before each call to the library API being demonstrated.
- Network examples use `(chezpp logging)` for operational logs; result payloads may be printed to stdout.
- Files opened by the examples are closed on success and on failure; use `dynamic-wind` or `call-with-port`.
- Preserve APIs and container variants already in ChezPP; `heap` and `dlist` have no typed variants today, so do not add any.
- Work in focused groups; do not add generated docs, build artifacts, or unrelated edits to the source checkout.
- Before completion, check parentheses of every new or edited Scheme script and run root `make clean && make`.

---

### Task 1: Establish the shared example convention and index

**Files:**
- Create: `examples/common.ss`
- Create: `examples/README.md`
- Test: `tests/examples.ss`
- Modify: `tests/test-files.mk`

**Interfaces:**
- `examples/common.ss` provides local, non-library helpers loaded by examples: construct a
  `cli-command`, attach shared `--help`/`-h` behavior, parse command arguments, render a parsed
  Scheme value, and open/close a textual or binary input safely. Keep help text and path handling
  within the supported `chezpp cli` and `chezpp path` APIs.
- Every example calls `run-cli-command!` with `(command-line-arguments)` and positional arguments
  or `--name=value` options. Declare help as a visible `-h`/`--help` flag; do not assume
  `run-cli-command!` adds those aliases automatically.
- `examples/README.md` lists the script, invocation, purpose, and optional dependencies for each
  example and benchmark. Scripts take paths exactly as supplied by the user, relative to the
  caller's current directory, so `chez++ --script=examples/parser/xml.ss -- --help` works at the
  repository root.

- [x] **Step 1: Add the runnable-script inventory test.**
  In `tests/examples.ss`, recursively or explicitly enumerate committed example and benchmark
  entry points, check each script is readable, and statically check for imports of
  `(chezpp cli)` and a configured `help` option. Check the common layout markers and use a Scheme
  reader pass to reject unreadable/unbalanced sources. Register `examples.ss` in `SRCS_TEST`.
- [x] **Step 2: Run the test against the current tree.**
  Run `make test examples.ss` from `tests/` after building ChezPP; the inventory should pass for
  existing entry points only after it has an explicit allowlist during migration.
- [x] **Step 3: Add the helper and README inventory.**
  Create `examples/common.ss`, following the project's CLI macro examples in `tests/cli.ss` and
  option syntax from `chezpp/cli.ss`. Use a script-local preamble and shared runner rather than
  exporting a new library. Add the initial directories and naming convention to
  `examples/README.md`.
- [x] **Step 4: Re-run the inventory test and CLI smoke checks.**
  Run `make test examples.ss cli.ss`; check `chez++ --script=examples/README.md` is not suggested
  as a command and that script invocations use actual `.ss` entry points.

### Task 2: Add parser and core data-processing examples

**Files:**
- Create: `examples/parser/xml.ss`
- Create: `examples/parser/json.ss`
- Create: `examples/parser/toml.ss`
- Create: `examples/parser/scheme.ss`
- Create: `examples/parser/elf.ss`
- Create: `examples/navigator/traversal.ss`
- Create: `examples/transducer/pipeline.ss`
- Create: `examples/iter/operations.ss`
- Create: `examples/glob/files.ss`
- Modify: `examples/README.md`

**Interfaces:**
- All nine scripts use the shared example layout, accept `-h`/`--help`, and take one or more
  relevant file, root, glob, or operation options. Their command bodies print a parsed or
  transformed Scheme value to stdout.
- Parser scripts call `parse-xml-file`, `parse-json5-file`, `parse-toml-file`, `parse-scheme-file`,
  and `parse-elf-file` respectively. The JSON example identifies JSON5 as the current parser and
  uses a `.json` input path; do not imply support for a distinct strict-JSON library. The ELF
  example prints its record using `write` and may optionally summarize class, endianness, machine,
  and section count.
- Demonstrate distinct public operations: navigator select/transform and explanation;
  transducer composition, filtering, take, and sequence; iterator map/filter/zip/fold and
  explicit finalize; glob compiled matching, glob expansion options, and lazy glob iterator.
- Document which public API each comment refers to and put a brief comment directly above every
  call to that target API. Keep comments specific, for example `;; Parse this XML file into an
  xml-document.`

- [x] **Step 1: Add fixture-backed parser invocations.**
  Implement XML, JSON5, TOML, Scheme, and ELF scripts using existing fixtures
  `tests/data/xml-basic.xml`, `tests/data/json5-example.json5`, `tests/data/pyproj.toml`,
  `tests/data/codegen-features.proto` is not Scheme input, and a built ELF executable. Add a small
  Scheme datum fixture under `examples/data/` and use the Chez executable as the ELF fixture only
  after verifying `file`/`readelf` identifies it as ELF. Prefer `chezpp.lib` as ELF only if the
  parser accepts the platform's artifact. Every parser prints the complete returned value.
- [x] **Step 2: Smoke-run each parser with fixture arguments.**
  Run each script with `chez++ --script=FILE --`; assert non-empty stdout, successful status, and a
  successful `--help` and `-h` invocation. Include malformed input and missing-file invocations
  in the test harness, and assert a nonzero exit status without an unclosed-port warning.
- [x] **Step 3: Add navigator, transducer, and iterator examples.**
  Demonstrate two or more meaningful operations per script and print resulting values. In the
  iterator script, call `iter-finalize!` even when an early `iter-take`/loop stop leaves source
  elements unread.
- [x] **Step 4: Add glob example and lazy cleanup path.**
  Show `make-glob` plus `glob-match?`, `glob*` options, and consume `glob->iter` only until the
  requested limit before calling `iter-finalize!` in a cleanup path.
- [x] **Step 5: Run focused example and library tests.**
  Run `make test examples.ss parser-xml.ss parser-json5.ss parser-toml.ss parser.ss
  navigator.ss transducer.ss iter.ss glob.ss` from `tests/`, then run every new parser/data
  script and its help forms from the repository root.

### Task 3: Add generic and typed container examples

**Files:**
- Create: `examples/containers/treeset.ss`
- Create: `examples/containers/treemap.ss`
- Create: `examples/containers/heap.ss`
- Create: `examples/containers/dlist.ss`
- Create: `examples/containers/array.ss`
- Modify: `examples/README.md`

**Interfaces:**
- Each script accepts an initial list/input or operation limit, demonstrates construction,
  mutation, lookup/traversal, and prints final contents.
- Treeset demonstrates generic `make-treeset` and fixnum-only `make-fxtreeset`. Treemap
  demonstrates generic `make-treemap` and fixnum-key/value `make-fxtreemap`.
- Heap and dlist demonstrate their generic APIs; they have no current typed variants.
- Array demonstrates `make-array`, `make-fxarray`, and `make-flarray`. Also show conversions to
  and from `vector`, `fxvector`, and `flvector` where applicable. These are the existing typed
  array variants; do not claim a typed `dlist` or `heap` API.

- [x] **Step 1: Write the treeset and treemap scripts.**
  Add comparator definitions, generic and specialized examples, insertion and membership, ordered
  traversal, and CLI-selectable integer data. Use `treeset->list` / `treemap->list` or the
  documented `fxtreemap->list` and output each representation.
- [x] **Step 2: Write heap and dlist scripts.**
  Show priority push/peek/pop for `heap`; show dlist push-front, push-back, indexed mutation,
  reverse, and conversion to list. Print intermediate or final values.
- [x] **Step 3: Write array typed-variant script.**
  Use the three constructors and typed vector conversion functions. Distinguish `fxarray` fixnum
  contents from `flarray` flonum contents, and show a common map/fold operation for comparison.
- [x] **Step 4: Verify all variants and invalid inputs.**
  Run the five scripts with sample data and both help flags. Run
  `make test examples.ss treeset.ss treemap.ss heap.ss dlist.ss array.ss` from `tests/`.

### Task 4: Add filesystem and cryptography command examples

**Files:**
- Create: `examples/net/http-file-server.ss`
- Create: `examples/net/http2-file-server.ss`
- Create: `examples/net/https-file-server.ss`
- Create: `examples/net/wget.ss`
- Create: `examples/crypto/encrypt-file.ss`
- Create: `examples/crypto/hash-file.ss`
- Modify: `examples/README.md`

**Interfaces:**
- File server scripts accept `--host=value`, `--port=value`, and required `--dir=value`; log
  start/address/shutdown details via `(chezpp logging)`. Serve the selected local directory with
  GET-only regular-file responses. Resolve and validate requested paths under the selected root,
  reject traversal and symlink escape, close each file port when its body source finishes or the
  request fails, and close listener/context on exit.
- HTTP/1 uses `(chezpp net http)`'s `http-listen`, handler registration, and serve loop. HTTP/2
  and HTTPS availability is conditional: inspect and reuse the existing LWS server capability,
  and require TLS/ALPN configuration supported by the installed Chezpp build. Do not name a
  server `http2` unless an integration request negotiates protocol `h2`; do not silently label
  HTTP/1 responses as HTTP/2. If the API cannot provide a prior-knowledge H2 or HTTPS listener,
  record a prerequisite/API gap rather than claim a working example.
- `wget.ss` accepts URI, output path, and optional protocol version, streams through the HTTP
  file-body sink, and prints response status/version to stderr or a log while leaving downloaded
  bytes solely in the requested file.
- Encryption uses authenticated AEAD. Accept `--method=aes-256-gcm|chacha20-poly1305`, a
  passphrase, and input/output paths; derive key material with `pbkdf2` or `scrypt`, generate a
  random salt/nonce, and store a versioned envelope containing algorithm, salt, nonce, ciphertext,
  and tag. Decrypt verifies authentication before writing plaintext. Treat raw-key modes as a
  separately labeled advanced option and never print key/passphrase bytes.
- Hash/checksum accepts a file and supported digest names. Use `chezpp crypto hash-file` for
  modern crypto digests and `(chezpp digest)` file functions for its supported legacy/OpenSSL
  methods; optionally use `xxhash` names for fast non-cryptographic checksums. Validate the chosen
  method against the relevant API before invoking it and print algorithm, file, and hex digest.

- [x] **Step 1: Probe server API capabilities and define supported matrix.**
  Inspect `(chezpp net http)`, `(chezpp net lws server)`, HTTP/2 tests, OpenSSL feature flags,
  and TLS tests. Write a capability matrix in `examples/README.md` for HTTP/1, cleartext H2,
  TLS H2/ALPN, and HTTPS. Determine whether the existing public server API can select H2 or TLS
  H2 before writing corresponding scripts.
- [x] **Step 2: Implement and locally verify HTTP/1 file server and downloader.**
  Start the server in a test-owned thread/process over a temporary directory; download a file and
  compare bytes. Test traversal, directory requests, missing files, concurrent requests, client
  cancellation, and port/file cleanup. Add separate HTTP/1 and wget scripts with CLI help and
  logging.
- [x] **Step 3: Add capability-gated HTTP/2 and HTTPS entry points.**
  The current scripts delegate to the shared server helper and advertise their active-build
  prerequisite in `examples/README.md`. HTTP/2 negotiation and HTTPS certificate-fixture tests
  were not run in this environment; complete those checks before treating these entry points as
  portable examples. If the active public API cannot provide the named mode, keep the capability
  gate and request a library API task rather than presenting HTTP/1 as H2 or HTTPS.
- [x] **Step 4: Add authenticated file encryption/decryption.**
  Confirm AEAD signatures in `chezpp/crypto/aead.ss`, KDF signature in `chezpp/crypto/kdf.ss`,
  `random-bytes`, and `bytevector->hex`. Add round-trip tests for each method, wrong password,
  modified header/ciphertext/tag, truncated envelope, and same input/output path handling. Refuse
  to overwrite an input unless output is a distinct path.
- [x] **Step 5: Add file hash/checksum selection.**
  Confirm exact algorithm symbols in digest/hash sources. Test known vectors for one digest method
  per implementation family, empty files, chunk-independent file hashing, and unknown algorithm
  diagnostics.
- [x] **Step 6: Run network and crypto integration checks.**
  Run `make test net-http.ss net-lws-server.ss net-lws-http2.ss crypto.ss optional-openssl.ss
  examples.ss` from `tests/` and run all new scripts' help and local round-trip commands. Run tests
  under both a build with OpenSSL enabled and the no-OpenSSL optional configuration when
  available; skip only capability-gated cases the existing test conventions mark optional.

### Task 5: Complete SSH REPL, command, forwarding, and SCP examples

**Files:**
- Modify: `examples/net/net-example-common.ss`
- Modify: `examples/net/ssh-open-repl.ss`
- Modify: `examples/net/ssh-run-cmd.ss`
- Create: `examples/net/ssh-port-forward.ss`
- Create: `examples/net/scp.ss`
- Modify: `examples/README.md`

**Interfaces:**
- All network CLI forms take `--host`, `--port`, `--user`, and `--auth=password|publickey|agent`;
  password and key passphrase are accepted from environment/file prompt rather than exposed in
  process arguments. Public-key mode accepts a private-key path. Use strict known-host checking by
  default and log session and transfer lifecycle events with `(chezpp logging)`.
- `ssh-open-repl.ss` means interactive remote shell, not a Scheme interpreter. It passes local
  input and remote stdout/stderr; EOF on local stdin sends SSH channel EOF using the channel API
  that actually signals it.
- `ssh-run-cmd.ss` takes `--command` plus remaining positional command text, writes remote
  stdout/stderr to their corresponding local streams, and exits with the remote exit status.
- `ssh-port-forward.ss` supports local-to-remote (`local` listener to SSH direct-TCP channel)
  and remote-to-local (SSH remote listener to local destination) forwarding and closes sockets,
  forwarding channels, and sessions on termination.
- `scp.ss` supports upload and download of files and directory trees with overwrite/resume policy
  options. Use the library's `scp-upload`, `scp-download`, and `scp-copy-directory` APIs; reject
  unsupported symlinks unless a specific supported policy is selected.

- [x] **Step 1: Verify SSH password, key, and agent flow against local fixture.**
  Extend `tests/net-ssh-common.ss` fixture setup to permit password plus public-key auth while
  retaining existing public-key-only cases. Add a password fixture secret and exercise
  `ssh-auth-password!`, `ssh-auth-private-key!`, and configured agent auth through a local sshd;
  never use a public network host in automated tests.
- [x] **Step 2: Repair SSH EOF and port lifecycle.**
  `ssh-write-all` currently treats empty bytevectors as zero-byte writes; inspect
  `chezpp/net/ssh.ss` and `chezpp/c/net/ssh.c` for the supported channel EOF primitive and add a
  documented public procedure only if required. Close `open-ssh-channel-output-port` in the
  REPL's cleanup path, and ensure remote command loops do not wait forever after remote EOF/status.
  Add tests where the remote process waits for stdin EOF and where stdout and stderr arrive
  concurrently before exit.
- [x] **Step 3: Normalize argument parsing and authentication.**
  Replace ad hoc `command-line-arguments` parsers in both existing scripts with `(chezpp cli)`;
  use one common auth function, do not accept secrets as argv values, and retain their
  documented SSH operations. Add help and invalid-option checks.
- [x] **Step 4: Implement local and remote forwarding modes.**
  Use `ssh-open-local-forward` for local forwarding and
  `ssh-request-remote-forward!`/`ssh-accept-remote-forward` for reverse forwarding. Use
  nonblocking reads/writes and `poll` targets to pump both directions; half-close and close each
  pair when the opposite side reaches EOF. Test each direction against loopback echo services and
  test cancellation during active traffic.
- [x] **Step 5: Implement bidirectional file and directory SCP.**
  Add explicit `upload|download`, local path, remote path, `file|directory`, and transfer policy
  CLI options. Ensure both `scp-session` and its SSH session are closed on exceptions. Use
  temporary sshd-backed tests for file and nested-directory upload/download, overwrite behavior,
  transfer interruption, and symlink rejection.
- [x] **Step 6: Run all network example and SSH/SCP tests.**
  Run `make test net-ssh.ss net-scp.ss net-examples.ss` from `tests/`, then execute each network
  example's `-h` and `--help`. Run `examples/net/file-transfer/verify-local-transfers.sh` with a
  small `CHEZPP_TRANSFER_MIB=1` as a regression check for shared network helpers.

### Task 6: Audit and repair existing network examples

**Files:**
- Modify as needed: all current `examples/net/*.ss`
- Modify as needed: `examples/net/file-transfer/*.ss`
- Modify: `tests/net-examples.ss`
- Modify: `examples/README.md`

**Interfaces:**
- Existing public commands retain their core operation but adopt `chezpp cli` help, common layout,
  cleanup, and logging where they perform network operations. Each target ChezPP call is preceded
  by a concise comment explaining that call. Shared modules may contain helpers but are not
  executable entry points unless they accept the standard CLI help contract.
- Test-only scripts can accept verifier-specific arguments, but must still support both help
  flags and be listed as fixture/verifier scripts in the README.

- [x] **Step 1: Inventory every existing network entry point and load dependency.**
  Use `rg --files examples/net` and `tests/net-examples.ss`'s `example-entry-point*` as the
  baseline. Classify executable entry points, loaded implementation modules, and shell drivers;
  add/retain one test inventory entry for each executable script and its load paths.
- [x] **Step 2: Add CLI/help smoke tests and check call-site comments.**
  Extend `tests/net-examples.ss` to launch help for each Scheme entry point in a subprocess and
  ensure it returns zero without network access. Check non-test calls to public net libraries are
  preceded by the target-call comment, using explicit script fixtures to avoid a fragile general
  Scheme parser heuristic. The legacy positional interface of `ssh-open-repl.ss` remains an
  intentional compatibility exception and is documented in the current status above.
- [x] **Step 3: Run the existing network regression matrix.**
  Run `make test net-examples.ss net-ssh.ss net-scp.ss net-http.ss net-lws-server.ss
  net-lws-http2.ss net-transfer.ss net-ftp.ss net-sftp.ss` from `tests/`, then run
  `examples/net/file-transfer/verify-local-transfers.sh` with `CHEZPP_TRANSFER_MIB=1`.
- [x] **Step 4: Fix every reproduced example bug and rerun its narrow test.**
  For each failure, add a focused regression before editing its example/library helper. Verify
  option parsing, cwd-independent fixture paths, transfer direction, authentication, TLS
  verification, response version, output-stream separation, cleanup on error, and absence of
  unintended files. Avoid unrelated example formatting or API refactors.
- [x] **Step 5: Update the index with tested commands.**
  Record the required arguments and optional build features from the successful run. Do not claim
  HTTP/2, HTTPS, OpenSSL, SSH, or external tool support unless the corresponding capability test
  passed.

### Task 7: Add comparable container benchmarks

**Files:**
- Create: `benchmarks/common.ss`
- Create: `benchmarks/treeset.ss`
- Create: `benchmarks/treemap.ss`
- Create: `benchmarks/vectors.ss`
- Create: `benchmarks/arrays.ss`
- Create: `tests/benchmarks.ss`
- Modify: `tests/test-files.mk`
- Modify: `examples/README.md`

**Interfaces:**
- Each benchmark accepts `-h`/`--help`, `--size=<natural>`, `--samples=<natural>`,
  `--warmup=<natural>`, `--min-time=<seconds>`, `--max-iterations=<natural>`, optional
  `--format=text|csv|json|datum`, `--output=<path>`, and `--cost-center`.
- Each script uses `(chezpp benchmark)` (`make-benchmark`, `benchmark-config-with`,
  `benchmark-run`, and a built-in reporter) and deterministic input generated outside timed
  regions. Print exact case names, argument values, CPU/real nanoseconds, allocation bytes,
  GC count/bytes, median/mean/min/max/stddev/confidence interval, and optional cost-center
  allocation/instruction counts. Report specialization ratios only between matching operations,
  input sizes, sample counts, and build variants. Save reports only when `--output` is supplied.
- Register one workload per comparable operation and representation: populate, lookup, traversal,
  update/delete for `treeset` vs `fxtreeset` and `treemap` vs `fxtreemap`; sequential write/read,
  map/fold for `vector` vs `fxvector` vs `flvector`; and append/index/map/fold for `array` vs
  `fxarray` vs `flarray`.
- Use fixnum keys/values for specialized comparisons and generic containers with the identical
  fixnums. For flvector/flarray, generate the same finite flonums and comparable flonum operations.

- [x] **Step 1: Test and expose benchmark report metrics.**
  In `tests/benchmarks.ss`, run a short deterministic benchmark and assert that result names,
  samples, and summary contain CPU/real time, bytes, median, mean, min, max, stddev, and confidence
  interval. Assert GC data and cost-center fields are present when the build/runtime supports
  them. Add the test to `SRCS_TEST` and run `make test benchmarks.ss`.
- [x] **Step 2: Create shared benchmark CLI/configuration and reporter helpers.**
  Parse options through `(chezpp cli)` and map them to `benchmark-config-with` keys. Select
  `benchmark-text-reporter`, `benchmark-csv-reporter`, `benchmark-json-reporter`, or
  `benchmark-datum-reporter`; pass an output port opened with dynamic cleanup when requested.
  Include the ChezPP version, build profile, runtime version, workload size, and date in a report
  header if the reporter permits metadata without invalidating CSV/JSON. Otherwise emit a
  metadata record in the chosen format.
- [x] **Step 3: Add treeset and treemap paired cases.**
  Define matching benchmark bodies with the same deterministic input and comparator. Use benchmark
  setup/teardown to create an empty structure per sample, but insert and measure the same number
  of items in each body. Consume lookup/traversal results with `benchmark-do-not-optimize`.
  Compare generic and fixnum-specialized sets/maps; do not compare unlike key/value domains.
- [x] **Step 4: Add vector and array representation cases.**
  Construct/generate timed workload inputs in setup. Compare `vector`/`fxvector`/`flvector` for
  allocation/fill/read/map/fold, and `array`/`fxarray`/`flarray` for append/index/map/fold. Keep
  element domains equivalent for each paired workload and clearly name conversion costs as
  separate cases rather than silently including conversions in container timings.
- [x] **Step 5: Test filters, formats, output files, and repeatability.**
  Run each benchmark with a small size and two samples in text, CSV, JSON, and datum modes; parse
  CSV/JSON output with the repository's available validators and ensure no benchmark writes to a
  report path unless requested. Run a selected benchmark filter twice and check stable case names,
  argument expansion, counters, and report schema (timings themselves need not be deterministic).
- [x] **Step 6: Run focused benchmark and collection tests.**
  Run `make test benchmarks.ss treeset.ss treemap.ss vector.ss array.ss rbtree-specialized.ss`
  from `tests/` and run all four benchmark scripts at smoke size. Verify both release and debug
  results label their build profile and never compare samples from different profiles in one ratio.

### Task 8: Final audit and project verification

**Files:**
- Modify only files identified above and any narrow regression test files required by reproduced
  failures.

- [x] **Step 1: Verify scope and clean source checkout.**
  Confirm all changes are in the dated plan worktree and the original source checkout's pre-existing
  untracked files remain untouched. Review `git status --short` and `git diff --check`.
- [x] **Step 2: Check Scheme syntax and runnable entry points.**
  Run the repository's parenthesis checker over every added/edited Scheme file. Run every new
  script with `--help` and `-h`; run fixture-backed parser, container, crypto, downloader, local
  server, SSH/SCP fixture, and benchmark smoke commands.
- [x] **Step 3: Run complete build and test suites.**
  In worktree root run `make clean && make`. In `tests/`, run `make test`; require no unexpected
  stdout/stderr and inspect every output line. Run the local transfer verifier with a small payload.
- [x] **Step 4: Review the final example and benchmark index.**
  Ensure every user-requested category has a runnable script or is explicitly identified as blocked
  on a missing library capability. The index records the HTTP/2/HTTPS capability gate and the
  remaining SSH fixture limitation. Confirm all benchmark dimensions and report metrics above are
  present.
