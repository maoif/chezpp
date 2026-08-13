# Net Phase 3 Task 6 Partial Handoff

## Completed

- Added `ssh-request-environment!` with validated string name/value parameters.
- Added `ssh-request-subsystem!` with non-empty subsystem validation.
- Bound both requests through dynamically loaded libssh symbols.
- Enabled `AcceptEnv CHEZPP_TEST_ENV` in the temporary sshd fixture and added deterministic
  request tests, including SFTP subsystem activation on a fresh channel.

## Verification

```bash
make clean && make
cd tests && make test-some TEST='net-ssh'
cd ..
scheme --script tools/check-scheme-balance.ss chezpp/net/ssh.ss tests/net-ssh.ss
```

The SSH test captures were empty. The installed libssh headers expose `ssh_send_keepalive` only in
the server API, so no client keepalive binding was added. Known-host enumeration/update/remove,
explicit-key and keyboard-interactive authentication, and readiness-driven forwarding remain for
the next session.
