# Net Roadmap Session Handoff

## Objective

Implement the approved Chezpp net roadmap in an isolated worktree. The scope includes optional
native dependency loading, scheduler-ready nonblocking I/O, FTP/SFTP/SCP parity, SSH stderr,
HTTP/WebSocket/gRPC expansion, all items in the May 13 Follow-Up Roadmap, documentation, examples,
and end-to-end file transfer verification.

## Workspace

- Worktree: `/home/maoif/SSD/Projects/chezpp/.worktrees/net-hardening-plan`
- Branch: `net-hardening-plan`
- Base commit: `4451414`
- Design commit: `2e0abab net: document follow-up roadmap design`
- Plan commit: `0d0872f net: plan follow-up roadmap implementation`

Planning is complete. Implementation has not started.

## Current Working Tree

At handoff creation, `git status --short` reports:

```text
 M chezpp/parser.ss
?? chezpp/c/scheme.h
```

These changes appeared after the planning commits and were not created as part of this net work.
Preserve them. Inspect and work around them; do not revert, overwrite, stage, or commit them unless
the maintainer explicitly assigns them to this task.

The untracked `chezpp/c/scheme.h` must not be assumed correct. The copy previously observed in the
main checkout identifies itself as ChezScheme 10.0.0, while the selected runtime is
10.5.0-pre-release.1. Phase 0 deliberately changes the Makefile to locate `scheme.h` beside the
selected runtime instead of tracking a stale copy.

## Approved Design

Read the governing design first:

`docs/superpowers/specs/2026-08-01-net-roadmap-design.md`

Fixed decisions:

- Load and version-check external libraries lazily, except libc and `libuuid`.
- Keep `libuuid` directly linked.
- Missing optional libraries must not prevent `(chezpp)` import.
- Replace per-operation Scheme threads with readiness-driven operation records.
- A fiber scheduler must be able to poll operation descriptors/timers and resume the operation.
- Use libcurl multi for FTP.
- Use one shared gRPC completion driver, not one thread per RPC.
- Use a hashtable for HTTP handlers.
- Add sequential FTP file handles without claiming random-access semantics.
- Expose SSH stdout and stderr as independent readable streams.
- Raw SSH file transfer is excluded.
- Required file transfer protocols are FTP/FTPS, SFTP, SCP, HTTP/HTTPS, WebSocket/WSS, and
  gRPC/TLS-enabled gRPC.
- Use the main checkout's untracked `examples/net/` files as implementation inputs.
- Do not embed the local `tester` account password in committed files.

## Implementation Plans

Start with the master plan:

`docs/superpowers/plans/2026-08-01-net-roadmap-master.md`

Execute the phase plans in this order and pass each release gate before continuing:

1. `docs/superpowers/plans/2026-08-01-net-optional-dependencies.md`
2. `docs/superpowers/plans/2026-08-01-net-readiness-operations.md`
3. `docs/superpowers/plans/2026-08-01-net-transfer-parity.md`
4. `docs/superpowers/plans/2026-08-01-net-application-protocols.md`
5. `docs/superpowers/plans/2026-08-01-net-follow-up-verification.md`

The six plan documents contain 41 tasks and 262 checkable steps. All 17 Follow-Up modules and both
required external download hashes are covered.

## Immediate Next Actions

1. Enter this worktree and confirm the branch and dirty files.
2. Read the design, master plan, and optional-dependencies plan completely.
3. Determine the ownership of `chezpp/parser.ss` and `chezpp/c/scheme.h`; preserve them meanwhile.
4. Execute Phase 0 from the master plan without copying the stale 10.0 `scheme.h`.
5. Import only the approved `examples/net/` inputs from the main checkout.
6. Run `make clean && make` and the committed net baseline tests.
7. If the baseline passes, begin Task 1 of the optional-dependencies plan using TDD.

Use `superpowers:subagent-driven-development` or `superpowers:executing-plans`. Project instructions
permit only one subagent at a time.

## Baseline Evidence

The first isolated build was run before the unexpected untracked header appeared:

```bash
make clean && make
```

It failed because `chezpp/c/common.h` could not include `scheme.h`. The plans do not claim a passing
baseline. Phase 0 resolves header discovery and requires a fresh clean build before implementation.

Runtime/header discovery was checked successfully:

```text
runtime: /home/maoif/Sources/Languages/Schemes/ChezScheme/install/lib/
         csv10.5.0-pre-release.1/ta6le/scheme
header:  /home/maoif/Sources/Languages/Schemes/ChezScheme/install/lib/
         csv10.5.0-pre-release.1/ta6le/scheme.h
version: 10.5.0-pre-release.1
```

Planning verification passed:

- six plan files exist and are nonempty;
- required plan headers are present;
- no plan placeholder patterns remain;
- all 17 Follow-Up modules are represented;
- both supplied SHA-256 values are present;
- `git diff --check` passed before the plan commit.

## Final Verification Requirements

The final phase must demonstrate upload and download with matching SHA-256 over every required
protocol and secure variant. FTP and SFTP must also pass scripted interactive list, mkdir, upload,
download, rename, delete, and rmdir workflows.

External example downloads:

- Emacs 30.2 ZIP:
  `414d3a1a21147af257ebd98bdd15976fdcb5ed0563f6de89f76d4a4b5dad9c72`
- Arch Linux 2026.07.01 ISO:
  `e86295dc0bdf9b85a5a9256810c553239689d2ae8e80eeec81b4e2e910d8a6c0`

Large downloads belong in examples and final verification, not `mat` tests. If an original URL is
unavailable, use a mirror only for the identical artifact and accept it only when the expected hash
matches.

## Project Rules To Retain

- Run `make clean && make` from the project root after each completed implementation unit.
- Successful tests must have empty stdout and stderr, except documented `Expect error` cases.
- Public procedures use meaningful parameters, `pcheck`, and adjacent `#|proc:...|#` docs.
- Public record documentation states construction, fields, ownership, lifetime, and transitions.
- Documentation lines remain at most 100 characters.
- Check every changed Scheme source for balanced parentheses.
- Close every opened port on all paths.
- Preserve unrelated working-tree changes.
