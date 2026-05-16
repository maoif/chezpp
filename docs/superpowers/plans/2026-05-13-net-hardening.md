# Net Hardening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Harden the `(chezpp net)` security defaults, add practical HTTP chunked support for reading and writing, and clarify public API semantics without expanding into the full network-feature backlog.

**Architecture:** Keep the existing Scheme/C split: transport policy and public API shape live in `chezpp/net/*.ss`, while OpenSSL, libssh, and libcurl integration stays in `chezpp/c/net/*.c` behind `chezpp/net/ffi.ss`. Preserve existing APIs where possible, make secure behavior the default for new sessions, and keep explicit opt-out APIs for tests and local development.

**Tech Stack:** ChezScheme libraries, Chezpp `(chezpp utils)` `pcheck`, OpenSSL, libssh, libcurl, existing `mat` tests under `tests/`, and project build via `make clean && make`.

## Current Status

Status as of May 16, 2026: implemented on branch `net-lib-v1`.

- TLS/HTTPS verification is secure by default. Client contexts load default CA paths, HTTPS no longer disables verification by default, and TLS connects configure hostname/IP verification. Explicit verification bypass remains available through TLS context APIs for tests and private deployments.
- SSH host-key checking is strict by default through `ssh-open`. `ssh-open-with-policy` provides explicit `strict`, `accept-new`, and `insecure` policies.
- FTPS verification is enabled by default for FTPS URLs, with public `ftp-verify-peer?`, `ftp-verify-host?`, and `ftp-set-tls-verification!` controls.
- HTTP supports chunked request and response body reading, opt-in chunked response writing, and `http-serve-loop` for multi-connection serving.
- High-level HTTP/FTP nonblocking docs now describe their thread-backed implementation and cancellation semantics.
- The custom Chezpp RPC layer was removed from tracked public net support. gRPC remains available through `(chezpp net grpc)` for RPC-style APIs.
- Verified on this branch with `make clean && make` and `cd tests && make test-some TEST='net-core net-http net-ftp net-ssh net-sftp net-scp net-websocket net-grpc'`; both commands exited 0 in an unrestricted environment.

---

## Scope

This plan applies the seven "Most important improvements" from the review:

1. TLS/HTTPS verification defaults and hostname verification.
2. SSH known-host verification.
3. Public FTPS verification controls and secure FTPS defaults.
4. HTTP chunked transfer decoding and automatic chunked response writing.
5. Clear documentation for thread-backed high-level nonblocking APIs.
6. Clearer HTTP server lifecycle semantics plus a multi-client helper.
7. Removal of the older custom RPC layer while keeping gRPC support.

The broad "What is left out" list is treated as roadmap material. It is not part of this implementation unless the maintainer expands the scope.

## Maintainer Decisions

- **SSH default host-key policy:** `ssh-open` becomes strict by default: host key must already match `known_hosts`. `ssh-open-with-policy` provides explicit `accept-new` and `insecure` bypass policies.
- **HTTP chunked encoding scope:** This plan includes chunked response decoding, chunked request decoding, and automatic chunked response writing when the response headers request `Transfer-Encoding: chunked`.
- **TLS default CA loading:** Client TLS contexts load system default verify paths automatically and expose a public `tls-context-load-default-ca!` procedure.

## File Map

- Modify: `chezpp/c/net/tls.c`
  - Default client verification.
  - Default CA loading helper.
  - Hostname verification in `chezpp_net_tls_connect`.
- Modify: `chezpp/net/ffi.ss`
  - Add FFI binding for default CA loading.
  - Change SSH open binding if the C signature gains a host-key policy argument.
- Modify: `chezpp/net/tls.ss`
  - Export and document `tls-context-load-default-ca!`.
  - Keep `tls-context-set-verify!` as the explicit opt-out.
- Modify: `chezpp/net/http.ss`
  - Stop disabling HTTPS verification in the default client path.
  - Add transfer-encoding helpers and chunked request/response body readers.
  - Add chunked response writer support.
  - Add `http-serve-loop`.
  - Clarify nonblocking and server lifecycle docs.
- Modify: `chezpp/c/net/ssh.c`
  - Load libssh known-host APIs.
  - Verify server host keys after `ssh_connect`.
  - Support strict, accept-new, and insecure policies.
- Modify: `chezpp/net/ssh.ss`
  - Add public policy API while preserving existing `ssh-open` arities.
  - Document host-key behavior.
- Modify: `chezpp/net/ftp.ss`
  - Default FTPS sessions to verify peer and host.
  - Add documented public getters/setter for TLS verification.
- Delete: `chezpp/net/rpc.ss`
  - Remove the older custom RPC public library.
- Delete: `chezpp/net/private/rpc.ss`
  - Remove private helpers used only by the older custom RPC layer.
- Delete: `tests/net-rpc.ss`
  - Remove tests for the deleted custom RPC layer.
- Modify: `chezpp/net.ss`
  - Stop re-exporting `(chezpp net rpc)`.
- Modify: `tests/Makefile`
  - Remove `net-rpc.ss` from the net test set.
- Modify: `tests/net-common.ss`
  - Extend test TLS helpers to support verified HTTPS using the local self-signed certificate as a CA.
  - Write SSH `known_hosts` entries for the temporary sshd test server.
- Modify: `tests/net-http.ss`
  - Add HTTPS verification tests.
  - Add chunked response and chunked request tests.
  - Add `http-serve-loop` tests.
- Modify: `tests/net-ssh.ss`
  - Add known-host success and mismatch tests.
- Modify: `tests/net-ftp.ss`
  - Add FTPS verification default and setter tests.

---

### Task 1: TLS And HTTPS Verification Defaults

**Files:**
- Modify: `chezpp/c/net/tls.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/tls.ss`
- Modify: `chezpp/net/http.ss`
- Modify: `tests/net-common.ss`
- Modify: `tests/net-http.ss`

- [x] **Step 1: Add failing HTTPS verification tests**

Add tests in `tests/net-http.ss`:

```scheme
(mat net-https-verification
     ;; Verified HTTPS succeeds when the local test certificate is trusted and
     ;; the URI host matches the certificate name.
     (let ([server-ctx (make-test-http-server-context)]
           [client-ctx (make-test-http-verified-client-context)])
       (let-values ([(server port th)
                     (start-http-dispatch-server
                      (lambda (server)
                        (http-register-handler!
                         server
                         'get
                         "/secure"
                         (lambda (req)
                           (make-http-response 200 "OK" '() "verified"))))
                      server-ctx)])
         (dynamic-wind
           void
           (lambda ()
             (let ([client (http-open client-ctx)])
               (dynamic-wind
                 void
                 (lambda ()
                   (let ([resp (http-get client
                                         (format "https://localhost:~a/secure" port))])
                     (and (= (http-response-status resp) 200)
                          (equal? (utf8->string (http-response-body resp)) "verified"))))
                 (lambda () (http-close client)))))
           (lambda ()
             (close-tls-context client-ctx)
             (close-tls-context server-ctx)
             (thread-join th))))))
     ;; Hostname mismatch is rejected when verification is enabled.
     (let ([server-ctx (make-test-http-server-context)]
           [client-ctx (make-test-http-verified-client-context)])
       (let-values ([(server port th)
                     (start-http-dispatch-server
                      (lambda (server)
                        (http-register-handler!
                         server
                         'get
                         "/secure"
                         (lambda (req)
                           (make-http-response 200 "OK" '() "unexpected"))))
                      server-ctx)])
         (dynamic-wind
           void
           (lambda ()
             (let ([client (http-open client-ctx)])
               (dynamic-wind
                 void
                 (lambda ()
                   (http-error-message-contains?
                    "TLS"
                    (lambda ()
                      (http-get client
                                (format "https://127.0.0.1:~a/secure" port)))))
                 (lambda () (http-close client)))))
           (lambda ()
             (close-tls-context client-ctx)
             (close-tls-context server-ctx)
             (thread-join th))))))
```

Add helper in `tests/net-common.ss`:

```scheme
(define make-test-http-verified-client-context
  (lambda ()
    (write-test-cert-files)
    (let ([ctx (make-tls-context 'client)])
      (tls-context-load-ca-file! ctx "/tmp/chezpp-net-test-cert.pem")
      (tls-context-set-verify! ctx #t)
      ctx)))
```

- [x] **Step 2: Run the focused test and verify it fails for the right reason**

Run:

```bash
cd tests && make test-some TEST='net'
```

Expected before implementation: the new HTTPS verification tests fail because hostname verification is absent or because default HTTPS behavior does not verify peers.

- [x] **Step 3: Add OpenSSL default CA FFI**

In `chezpp/c/net/tls.c`, add:

```c
ptr chezpp_net_tls_context_load_default_ca(uptr handle) {
  chezpp_tls_context *ctx = ctx_from_handle(handle);
  if (ctx == NULL || ctx->ctx == NULL) return make_error_status_message("invalid TLS context");
  if (SSL_CTX_set_default_verify_paths(ctx->ctx) != 1)
    return openssl_error_status("failed to load default TLS verify paths");
  return Strue;
}
```

In `chezpp/net/ffi.ss`, export and bind:

```scheme
ffi-net-tls-context-load-default-ca
```

```scheme
(define ffi-net-tls-context-load-default-ca
  (foreign-procedure "chezpp_net_tls_context_load_default_ca" (uptr) scheme-object))
```

- [x] **Step 4: Make client TLS contexts verify by default**

In `chezpp/c/net/tls.c`, change `chezpp_net_tls_context_create` so:

```c
if (mode == 0) {
  SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
  SSL_CTX_set_default_verify_paths(ctx);
} else {
  SSL_CTX_set_verify(ctx, SSL_VERIFY_NONE, NULL);
}
```

Keep `tls-context-set-verify!` as the explicit control for tests and private deployments.

- [x] **Step 5: Add hostname verification during connect**

In `chezpp_net_tls_connect`, after SNI setup and before `SSL_connect`, add host verification for non-empty `server_name`:

```c
if (server_name != NULL && server_name[0] != '\0') {
  if (SSL_set1_host(ssl, server_name) != 1) {
    restore_socket_flags(fd, saved_flags, changed);
    SSL_free(ssl);
    return openssl_error_status("failed to configure TLS hostname verification");
  }
}
```

If the project must support an OpenSSL variant without `SSL_set1_host`, stop and ask for the minimum supported OpenSSL/LibreSSL version before adding compatibility branches.

- [x] **Step 6: Add public Scheme wrapper and docs**

In `chezpp/net/tls.ss`, export and document:

```scheme
#|proc:tls-context-load-default-ca!
The `tls-context-load-default-ca!` procedure loads the platform default trusted
certificate locations into a TLS context.
|#
(define-who tls-context-load-default-ca!
  (lambda (ctx)
    (pcheck ([tls-context? ctx])
            (ensure-context-open who ctx)
            (ensure-success who
                            (ffi-net-tls-context-load-default-ca
                             (tls-context-handle ctx))))))
```

- [x] **Step 7: Stop disabling HTTPS verification by default**

In `chezpp/net/http.ss`, replace the fallback HTTPS context creation:

```scheme
(let ([ctx (make-tls-context 'client)])
  (tls-context-set-verify! ctx #f)
  ctx)
```

with:

```scheme
(make-tls-context 'client)
```

Tests that need self-signed local HTTPS must pass an explicit client TLS context with `tls-context-set-verify! #f` or with `tls-context-load-ca-file!`.

- [x] **Step 8: Run verification**

Run:

```bash
make clean && make
```

Run:

```bash
cd tests && make test-some TEST='net'
```

Expected in an unrestricted environment: build succeeds and net tests are silent. In a sandbox that forbids socket creation, record the exact `Operation not permitted` failure and verify at least the build.

- [x] **Step 9: Commit**

```bash
git add chezpp/c/net/tls.c chezpp/net/ffi.ss chezpp/net/tls.ss chezpp/net/http.ss tests/net-common.ss tests/net-http.ss
git commit -m "net: enable TLS verification by default"
```

---

### Task 2: SSH Known-Host Verification

**Files:**
- Modify: `chezpp/c/net/ssh.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/ssh.ss`
- Modify: `tests/net-ssh-common.ss`
- Modify: `tests/net-ssh.ss`

- [x] **Step 1: Add tests for strict known-host behavior**

Update `start-ssh-test-server` in `tests/net-ssh-common.ss` to write a known-hosts entry after creating the host key:

```scheme
(let ([known-hosts (string-append ssh-dir "/known_hosts")]
      [host-pub (call-with-string-output-port
                  (lambda (p)
                    (parameterize ([current-output-port p])
                      (run-command!
                       'start-ssh-test-server
                       (format "ssh-keygen -y -f ~a" host-key))))))])
  (write-bytevector-file
   known-hosts
   (string->utf8
    (format "[127.0.0.1]:~a ~a" port host-pub))))
```

If `run-command!` cannot capture stdout because it currently uses `system`, add a small helper that opens a process input port and closes it after reading.

Add a negative test in `tests/net-ssh.ss`:

```scheme
(mat net-ssh-known-hosts
     ;; Strict host-key verification rejects a server when HOME has no known_hosts.
     (let-values ([(remote-root home port user stop-server) (start-ssh-test-server)])
       (let ([empty-home (format "/tmp/chezpp-net-ssh-empty-home-~a" port)])
         (dynamic-wind
           (lambda () (mkdirs (string-append empty-home "/.ssh")))
           (lambda ()
             (with-env
              "HOME"
              empty-home
              (lambda ()
                (ssh-error-message-contains?
                 "host key"
                 (lambda ()
                   (ssh-open "127.0.0.1" port user 2000))))))
           (lambda ()
             (stop-server)
             (when (file-exists? empty-home)
               (file-removetree empty-home #f)))))))
```

- [x] **Step 2: Run the focused test and verify it fails before implementation**

Run:

```bash
cd tests && make test-some TEST='net'
```

Expected before implementation: strict host-key test fails because `ssh-open` accepts the unknown server.

- [x] **Step 3: Add libssh known-host function pointers**

In `chezpp/c/net/ssh.c`, add dynamic function pointer typedefs and symbols for:

```c
typedef enum ssh_known_hosts_e (*ssh_session_is_known_server_fn)(ssh_session);
typedef int (*ssh_session_update_known_hosts_fn)(ssh_session);
```

Load:

```c
ssh_session_is_known_server
ssh_session_update_known_hosts
```

If the installed libssh only exposes the older `ssh_is_server_known` API, stop and ask whether to support older libssh or raise the minimum version.

- [x] **Step 4: Add C policy handling**

Change `chezpp_net_ssh_open` to accept an `int hostkey_policy`:

```c
/* 0 strict, 1 accept-new, 2 insecure */
ptr chezpp_net_ssh_open(const char *host, int port, const char *user,
                        int timeout_ms, int hostkey_policy)
```

After `p_ssh_connect(session)` succeeds, verify:

```c
if (hostkey_policy != 2) {
  enum ssh_known_hosts_e state = p_ssh_session_is_known_server(session);
  switch (state) {
  case SSH_KNOWN_HOSTS_OK:
    break;
  case SSH_KNOWN_HOSTS_NOT_FOUND:
  case SSH_KNOWN_HOSTS_UNKNOWN:
    if (hostkey_policy == 1 && p_ssh_session_update_known_hosts(session) == SSH_OK) {
      break;
    }
    p_ssh_disconnect(session);
    p_ssh_free(session);
    return make_error_status_message("SSH host key is not trusted");
  case SSH_KNOWN_HOSTS_CHANGED:
  case SSH_KNOWN_HOSTS_OTHER:
    p_ssh_disconnect(session);
    p_ssh_free(session);
    return make_error_status_message("SSH host key mismatch");
  case SSH_KNOWN_HOSTS_ERROR:
  default:
    {
      ptr status = ssh_error_status(session, "failed to verify SSH host key");
      p_ssh_disconnect(session);
      p_ssh_free(session);
      return status;
    }
  }
}
```

- [x] **Step 5: Add Scheme policy API**

In `chezpp/net/ssh.ss`, keep existing `ssh-open` arities and make them pass strict policy. Add:

```scheme
#|proc:ssh-open-with-policy
The `ssh-open-with-policy` procedure opens an SSH session using an explicit host
key policy. The policy must be one of `strict`, `accept-new`, or `insecure`.
|#
(define-who ssh-open-with-policy
  (lambda (host port user timeout-ms policy)
    (pcheck ([string? host] [fixnum? port])
            (check-port who port)
            (ensure-user-maybe who user)
            (check-timeout-ms who timeout-ms)
            (let ([policy-int (case policy
                                [(strict) 0]
                                [(accept-new) 1]
                                [(insecure) 2]
                                [else (errorf who "invalid SSH host-key policy ~s" policy)])])
              (let ([ans (ensure-success who 'ssh
                                         (ffi-net-ssh-open host
                                                           port
                                                           (or user "")
                                                           timeout-ms
                                                           policy-int))])
                (%make-ssh-session ans host port user #f))))))
```

Export `ssh-open-with-policy`. Update `ssh-open` to call `ssh-open-with-policy` with `'strict`.

- [x] **Step 6: Update FFI binding**

In `chezpp/net/ffi.ss`, change:

```scheme
(foreign-procedure "chezpp_net_ssh_open" (string int string int) scheme-object)
```

to:

```scheme
(foreign-procedure "chezpp_net_ssh_open" (string int string int int) scheme-object)
```

- [x] **Step 7: Run verification**

Run:

```bash
make clean && make
```

Run:

```bash
cd tests && make test-some TEST='net'
```

- [x] **Step 8: Commit**

```bash
git add chezpp/c/net/ssh.c chezpp/net/ffi.ss chezpp/net/ssh.ss tests/net-ssh-common.ss tests/net-ssh.ss
git commit -m "net: verify SSH host keys"
```

---

### Task 3: FTPS Verification Defaults And Public Controls

**Files:**
- Modify: `chezpp/net/ftp.ss`
- Modify: `tests/net-ftp.ss`

- [x] **Step 1: Add failing FTPS verification API tests**

Add to `tests/net-ftp.ss`:

```scheme
(mat net-ftp-tls-verification-api
     (let ([plain (ftp-open "ftp://127.0.0.1:21/")]
           [secure (ftp-open "ftps://127.0.0.1:21/")])
       (dynamic-wind
         void
         (lambda ()
           (and
            (not (ftp-verify-peer? plain))
            (not (ftp-verify-host? plain))
            (ftp-verify-peer? secure)
            (ftp-verify-host? secure)
            (eq? (ftp-set-tls-verification! secure #f #f) secure)
            (not (ftp-verify-peer? secure))
            (not (ftp-verify-host? secure))
            (eq? (ftp-set-tls-verification! secure #t #t) secure)
            (ftp-verify-peer? secure)
            (ftp-verify-host? secure)))
         (lambda ()
           (ftp-close plain)
           (ftp-close secure)))))
```

- [x] **Step 2: Run the focused test and verify it fails before implementation**

Run:

```bash
cd tests && make test-some TEST='net'
```

Expected before implementation: `ftp-verify-peer?`, `ftp-verify-host?`, and `ftp-set-tls-verification!` are undefined.

- [x] **Step 3: Export and document verification accessors**

In `chezpp/net/ftp.ss`, export:

```scheme
ftp-verify-peer?
ftp-verify-host?
ftp-set-tls-verification!
```

Add public wrappers with `pcheck`:

```scheme
#|proc:ftp-verify-peer?
The `ftp-verify-peer?` procedure returns whether an FTPS session verifies the
server certificate chain.
|#
(define-who ftp-verify-peer?
  (lambda (session)
    (pcheck ([ftp-session? session])
            (ftp-session-verify-peer? session))))

#|proc:ftp-verify-host?
The `ftp-verify-host?` procedure returns whether an FTPS session verifies the
server certificate hostname.
|#
(define-who ftp-verify-host?
  (lambda (session)
    (pcheck ([ftp-session? session])
            (ftp-session-verify-host? session))))

#|proc:ftp-set-tls-verification!
The `ftp-set-tls-verification!` procedure sets FTPS certificate-chain and
hostname verification flags on a session.
|#
(define-who ftp-set-tls-verification!
  (lambda (session verify-peer? verify-host?)
    (pcheck ([ftp-session? session] [boolean? verify-peer? verify-host?])
            (ensure-session-open who session)
            (ftp-session-verify-peer?-set! session verify-peer?)
            (ftp-session-verify-host?-set! session verify-host?)
            session)))
```

- [x] **Step 4: Default FTPS sessions to verify**

In `ftp-open`, when constructing `%make-ftp-session`, derive verification from URI scheme:

```scheme
(let ([secure? (string=? (uri-scheme u) "ftps")])
  (%make-ftp-session u
                     user
                     pass
                     normalized-path
                     #t
                     timeout-ms
                     secure?
                     secure?
                     #f
                     #f))
```

Keep plain FTP verification flags false because libcurl only uses them when `use_tls` is true.

- [x] **Step 5: Run verification**

Run:

```bash
make clean && make
```

Run:

```bash
cd tests && make test-some TEST='net'
```

- [x] **Step 6: Commit**

```bash
git add chezpp/net/ftp.ss tests/net-ftp.ss
git commit -m "net: expose FTPS verification controls"
```

---

### Task 4: HTTP Chunked Transfer Decoding And Writing

**Files:**
- Modify: `chezpp/net/http.ss`
- Modify: `tests/net-http.ss`

- [x] **Step 1: Add failing chunked response test**

Add a raw one-response server helper in `tests/net-http.ss`:

```scheme
(define start-raw-http-response-server
  (lambda (response-bv)
    (let ([listener (open-socket 'inet 'stream)])
      (socket-set-option! listener 'reuse-address #t)
      (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
      (socket-listen! listener 4)
      (let ([port (socket-address-port (socket-local-address listener))])
        (values port
                (fork-thread
                 (lambda ()
                   (let-values ([(client peer) (socket-accept listener)])
                     (let ([op (open-socket-output-port client)])
                       (put-bytevector op response-bv)
                       (flush-output-port op)
                       (close-port op))
                     (close-socket client)
                     (close-socket listener)))))))))
```

Add test:

```scheme
(mat net-http-chunked-response
     (let-values ([(port th)
                   (start-raw-http-response-server
                    (string->utf8
                     "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n5\r\nhello\r\n6;ext=1\r\n world\r\n0\r\nX-Trailer: done\r\n\r\n"))])
       (let ([resp (http-get (format "http://127.0.0.1:~a/chunked" port))])
         (thread-join th)
         (and (= (http-response-status resp) 200)
              (equal? (utf8->string (http-response-body resp)) "hello world")))))
```

- [x] **Step 2: Add failing chunked request test**

Add test:

```scheme
(mat net-http-chunked-request
     (let-values ([(server port th)
                   (start-http-connection-server
                    (lambda (conn)
                      (let ([req (http-read-request conn)])
                        (http-write-response
                         conn
                         (make-http-response
                          200
                          "OK"
                          `(("X-Body" . ,(utf8->string (http-request-body req))))
                          "ok")))))])
       (let ([sock (open-socket 'inet 'stream)])
         (dynamic-wind
           (lambda ()
             (socket-connect! sock (make-socket-address 'inet "127.0.0.1" port)))
           (lambda ()
             (let ([ip (open-socket-input-port sock)]
                   [op (open-socket-output-port sock)])
               (put-bytevector
                op
                (string->utf8
                 "POST /upload HTTP/1.1\r\nHost: 127.0.0.1\r\nTransfer-Encoding: chunked\r\nConnection: close\r\n\r\n4\r\nwiki\r\n5\r\npedia\r\n0\r\n\r\n"))
               (flush-output-port op)
               (let ([line (read-crlf-line ip)])
                 (and (string-contains? line "200")
                      (begin
                        (thread-join th)
                        #t)))))
           (lambda ()
             (guard (c [else #f])
               (close-socket sock)))))))
```

- [x] **Step 3: Add failing chunked response writing test**

Add to `tests/net-http.ss`:

```scheme
(mat net-http-chunked-response-writing
     (let-values ([(server port th)
                   (start-http-dispatch-server
                    (lambda (server)
                      (http-register-handler!
                       server
                       'get
                       "/chunked"
                       (lambda (req)
                         (make-http-response
                          200
                          "OK"
                          '(("Transfer-Encoding" . "chunked"))
                          "chunked response")))))])
       (let ([sock (open-socket 'inet 'stream)])
         (dynamic-wind
           (lambda ()
             (socket-connect! sock (make-socket-address 'inet "127.0.0.1" port)))
           (lambda ()
             (let ([ip (open-socket-input-port sock)]
                   [op (open-socket-output-port sock)])
               (put-bytevector
                op
                (string->utf8
                 "GET /chunked HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"))
               (flush-output-port op)
               (let ([raw (utf8->string (read-port->bytevector ip))])
                 (and (string-contains? raw "Transfer-Encoding: chunked")
                      (string-contains? raw "\r\n10\r\nchunked response\r\n0\r\n\r\n")
                      (begin
                        (thread-join th)
                        #t)))))
           (lambda ()
             (guard (c [else #f])
               (close-socket sock)))))))
```

- [x] **Step 4: Run tests and confirm failure**

Run:

```bash
cd tests && make test-some TEST='net'
```

Expected before implementation: chunked response body is not decoded, chunked request body is missing, and responses with `Transfer-Encoding: chunked` are not chunk framed.

- [x] **Step 5: Add transfer-encoding helpers**

In `chezpp/net/http.ss`, add helpers near `response-body-length`:

```scheme
(define header-token-member?
  (lambda (headers name token)
    (let ([value (http-header-ref headers name #f)])
      (and value
           (let ([target (string-downcase token)])
             (let loop ([rest (string-split value #\,)])
               (and (not (null? rest))
                    (or (string=? (string-trim (string-downcase (car rest))) target)
                        (loop (cdr rest))))))))))

(define chunked-transfer?
  (lambda (headers)
    (header-token-member? headers "Transfer-Encoding" "chunked")))

(define parse-chunk-size
  (lambda (who line)
    (let* ([semi (string-search line #\;)]
           [size-text (string-trim (if semi
                                       (substring line 0 semi)
                                       line))]
           [n (string->number size-text 16)])
      (unless (and n (exact? n) (integer? n) (>= n 0))
        (raise-net-error who 'http "invalid HTTP chunk size" line))
      n)))
```

If `string-trim` is not available from `(chezpp string)`, add a local helper in `http.ss` using existing string utilities.

- [x] **Step 6: Add chunked body reader**

In `chezpp/net/http.ss`, add:

```scheme
(define read-http-body/chunked
  (lambda (who ip)
    (let loop ([parts '()] [total 0])
      (let ([line (read-http-line ip)])
        (when (eof-object? line)
          (raise-net-error who 'http "unexpected EOF while reading HTTP chunk size"))
        (let ([size (parse-chunk-size who line)])
          (if (= size 0)
              (begin
                (let trailer-loop ()
                  (let ([trailer (read-http-line ip)])
                    (when (eof-object? trailer)
                      (raise-net-error who 'http "unexpected EOF while reading HTTP trailers"))
                    (unless (string=? trailer "")
                      (trailer-loop))))
                (let ([out (make-bytevector total 0)])
                  (let fill ([rest (reverse parts)] [i 0])
                    (if (null? rest)
                        out
                        (let* ([part (car rest)]
                               [n (bytevector-length part)])
                          (bytevector-copy! part 0 out i n)
                          (fill (cdr rest) (+ i n)))))))
              (let ([chunk (read-http-body/exact who ip size)]
                    [crlf (read-http-line ip)])
                (unless (string=? crlf "")
                  (raise-net-error who 'http "invalid HTTP chunk terminator" crlf))
                (loop (cons chunk parts) (+ total size)))))))))
```

- [x] **Step 7: Add chunked response writer**

In `chezpp/net/http.ss`, add:

```scheme
(define write-http-body/chunked
  (lambda (op body)
    (let ([len (bytevector-length body)])
      (unless (fx= len 0)
        (put-bytevector op
                        (string->utf8
                         (format "~x\r\n" len)))
        (put-bytevector op body)
        (put-bytevector op (string->utf8 "\r\n")))
      (put-bytevector op (string->utf8 "0\r\n\r\n")))))
```

If `format` does not support `~x` in this ChezScheme environment, add a small local hexadecimal formatter using `number->string` with radix 16:

```scheme
(number->string len 16)
```

- [x] **Step 8: Use chunked reader for responses and requests**

In `read-http-response*`, prefer chunked over content length:

```scheme
(let* ([headers (read-http-headers who ip)]
       [content-length (response-body-length headers)]
       [body (cond
              [(chunked-transfer? headers)
               (read-http-body/chunked who ip)]
              [content-length
               (read-http-body/exact who ip content-length)]
              [else
               (read-http-body/to-eof ip)])])
  ...)
```

In `http-read-request`, use the same precedence, but keep no body as `#f`:

```scheme
(let* ([headers (read-http-headers who (http-connection-input-port conn))]
       [content-length (response-body-length headers)]
       [body (cond
              [(chunked-transfer? headers)
               (read-http-body/chunked who (http-connection-input-port conn))]
              [content-length
               (read-http-body/exact who
                                     (http-connection-input-port conn)
                                     content-length)]
              [else #f])])
  ...)
```

- [x] **Step 9: Use chunked writer for responses**

In `ensure-response-headers`, do not add `Content-Length` when the response uses chunked transfer encoding:

```scheme
(cond
 [(chunked-transfer? headers) headers]
 [(http-header-ref headers "Content-Length" #f) headers]
 [else
  (http-header-set headers "Content-Length"
                   (number->string (bytevector-length body)))])
```

In `write-response-port`, write the body through the chunk encoder when `Transfer-Encoding: chunked` is present:

```scheme
(if (chunked-transfer? headers)
    (write-http-body/chunked op body)
    (unless (fx= 0 (bytevector-length body))
      (put-bytevector op body)))
```

Automatic chunked writing is opt-in by response headers. Existing `make-http-response` calls continue to get a `Content-Length` response unless the caller sets `Transfer-Encoding: chunked`.

- [x] **Step 10: Keep connection reuse correct**

In `reusable-response?`, treat chunked responses as complete reusable bodies:

```scheme
(or (string=? method "HEAD")
    (= status 204)
    (= status 304)
    (http-header-ref (http-response-headers response) "Content-Length" #f)
    (chunked-transfer? (http-response-headers response))
    (and (bytevector? body)
         (fx= 0 (bytevector-length body))))
```

- [x] **Step 11: Run verification**

Run:

```bash
make clean && make
```

Run:

```bash
cd tests && make test-some TEST='net'
```

- [x] **Step 12: Commit**

```bash
git add chezpp/net/http.ss tests/net-http.ss
git commit -m "net: decode HTTP chunked bodies"
```

---

### Task 5: HTTP Server Lifecycle Helper

**Files:**
- Modify: `chezpp/net/http.ss`
- Modify: `tests/net-http.ss`

- [x] **Step 1: Add failing `http-serve-loop` test**

Add to `tests/net-http.ss`:

```scheme
(mat net-http-serve-loop
     (let* ([port (reserve-loopback-port)]
            [server (http-listen "127.0.0.1" port)]
            [count 0])
       (http-register-handler!
        server
        'get
        "/"
        (lambda (req)
          (set! count (+ count 1))
          (make-http-response 200 "OK" '() "ok")))
       (let ([th (fork-thread (lambda () (http-serve-loop server #t)))])
         (dynamic-wind
           void
           (lambda ()
             (let ([r1 (http-get (format "http://127.0.0.1:~a/" port))]
                   [r2 (http-get (format "http://127.0.0.1:~a/" port))])
               (and (= (http-response-status r1) 200)
                    (= (http-response-status r2) 200)
                    (= count 2))))
           (lambda ()
             (http-server-close server)
             (thread-join th))))))
```

- [x] **Step 2: Export and document `http-serve-loop`**

In `chezpp/net/http.ss`, add `http-serve-loop` to the export list and document:

```scheme
#|proc:http-serve-loop
The `http-serve-loop` procedure repeatedly accepts and serves HTTP connections
until the server is closed. If `threaded?` is true, each accepted connection is
served in a new Scheme thread; otherwise connections are served serially.
|#
(define-who http-serve-loop
  (case-lambda
    [(server) (http-serve-loop server #t)]
    [(server threaded?)
     (pcheck ([http-server? server] [boolean? threaded?])
             (let loop ()
               (unless (http-server-closed? server)
                 (guard (c [else
                            (unless (http-server-closed? server)
                              (raise c))])
                   (if threaded?
                       (let ([conn (http-accept server)])
                         (fork-thread
                          (lambda ()
                            (serve-http-connection who server conn))))
                       (http-serve server)))
                 (loop)))
             server)]))
```

Refactor the connection-handling body currently inside `http-serve` into an internal helper:

```scheme
(define serve-http-connection
  (lambda (who server conn)
    (dynamic-wind
      void
      (lambda ()
        (let loop ()
          ...existing request dispatch body...))
      (lambda ()
        (http-connection-close conn)))))
```

Then make `http-serve` accept one connection and call `serve-http-connection`.

- [x] **Step 3: Run verification**

Run:

```bash
make clean && make
```

Run:

```bash
cd tests && make test-some TEST='net'
```

- [x] **Step 4: Commit**

```bash
git add chezpp/net/http.ss tests/net-http.ss
git commit -m "net: add HTTP serve loop helper"
```

---

### Task 6: Public Documentation, RPC Removal, And Nonblocking Semantics

**Files:**
- Modify: `chezpp/net/http.ss`
- Modify: `chezpp/net/ftp.ss`
- Modify: `chezpp/net/ssh.ss`
- Modify: `chezpp/net/tls.ss`
- Modify: `chezpp/net.ss`
- Modify: `tests/Makefile`
- Delete: `chezpp/net/rpc.ss`
- Delete: `chezpp/net/private/rpc.ss`
- Delete: `tests/net-rpc.ss`

- [x] **Step 1: Remove custom RPC support**

The custom Chezpp RPC layer was removed instead of documenting the exported RPC macros. gRPC remains in the net library and is the supported RPC-style API surface.

Completed changes:

- Deleted `chezpp/net/rpc.ss`.
- Deleted `chezpp/net/private/rpc.ss`.
- Deleted `tests/net-rpc.ss`.
- Removed `(chezpp net rpc)` from `chezpp/net.ss`.
- Removed `net-rpc.ss` from `tests/Makefile`.

- [x] **Step 2: Clarify HTTP nonblocking docs**

Update docs above these exports in `chezpp/net/http.ss`:

```scheme
http-send/nonblocking
http-request/nonblocking
http-download/nonblocking
http-upload/nonblocking
```

Each doc block must state that the high-level operation is progressed by a Scheme worker thread and a notifier socket; cancelling closes the client-side connection and marks the pending operation cancelled, but an underlying blocking C or OS operation may finish later.

- [x] **Step 3: Clarify FTP nonblocking docs**

Update docs above:

```scheme
ftp-list/nonblocking
ftp-download/nonblocking
ftp-upload/nonblocking
ftp-cancel-pending!
```

Each doc block must state that libcurl work is performed by a Scheme worker thread and cancellation marks the pending operation cancelled; libcurl cleanup happens when the worker returns.

- [x] **Step 4: Drop obsolete RPC documentation work**

The RPC nonblocking docs and internal RPC helper comments are no longer needed because the custom RPC files were removed.

- [x] **Step 5: Run a documentation block audit**

Run:

```bash
rg -n "define-syntax define-rpc|#\\|macro:define-rpc|#\\|proc:http-send/nonblocking|#\\|proc:ftp-list/nonblocking|unary-pending-key|chezpp net rpc" chezpp/net tests/Makefile
```

Expected: no RPC library references remain; touched nonblocking APIs have updated docs.

- [x] **Step 6: Build**

Run:

```bash
make clean && make
```

- [x] **Step 7: Commit**

```bash
git add chezpp/net/http.ss chezpp/net/ftp.ss chezpp/net/ssh.ss chezpp/net/tls.ss chezpp/net.ss tests/Makefile
git add -u chezpp/net/rpc.ss chezpp/net/private/rpc.ss tests/net-rpc.ss
git commit -m "net: remove custom RPC support"
```

---

### Task 7: Final Verification And Compatibility Review

**Files:**
- Review all modified files from prior tasks.

- [x] **Step 1: Check modified Scheme files for balanced parentheses by compiling**

Run:

```bash
make clean && make
```

Expected: build succeeds. ChezScheme read/compile errors are treated as parenthesis or syntax failures and must be fixed before continuing.

- [x] **Step 2: Run net tests**

Run:

```bash
cd tests && make test-some TEST='net'
```

Expected in an unrestricted environment: stdout and stderr are empty. If the sandbox blocks sockets with `Operation not permitted`, record that exact limitation and run the build plus any non-socket checks available.

- [x] **Step 3: Inspect exported API docs**

Run:

```bash
rg -n "ssh-open-with-policy|ftp-set-tls-verification!|ftp-verify-peer\\?|ftp-verify-host\\?|tls-context-load-default-ca!|http-serve-loop|define-rpc-message|define-rpc-service" chezpp/net
```

Expected: each new public API is exported, documented with `#|proc:...|#` or `#|macro:...|#`, implemented with `pcheck` where applicable, and tested.

- [x] **Step 4: Inspect insecure opt-outs**

Run:

```bash
rg -n "set-verify! ctx #f|insecure|SSL_VERIFY_NONE|VERIFYHOST, 0|VERIFYPEER, 0|hostkey_policy" chezpp/net chezpp/c/net tests
```

Expected: insecure TLS/SSH/FTPS behavior is reachable only through explicit APIs or test helpers. Default HTTPS, FTPS, and SSH paths should be secure.

- [x] **Step 5: Final commit if needed**

If final verification requires cleanup changes, commit them:

```bash
git add chezpp/c/net/tls.c chezpp/c/net/ssh.c chezpp/net/ffi.ss chezpp/net/tls.ss chezpp/net/http.ss chezpp/net/ssh.ss chezpp/net/ftp.ss chezpp/net/rpc.ss tests/net-common.ss tests/net-http.ss tests/net-ssh-common.ss tests/net-ssh.ss tests/net-ftp.ss
git commit -m "net: finish transport hardening"
```

If every task already produced a commit and no cleanup is needed, skip this commit.

## Follow-Up Roadmap Outside This Plan

- HTTP streaming bodies, compression, cookies, auth, proxy support, HTTP/2, and HTTP/3.
- FTP persistent control-connection semantics, MLSD parsing, progress callbacks, and resume.
- SSH port forwarding, keepalive, environment/subsystem helpers, and richer known-host management.
- TLS ALPN result access, OCSP, session resumption, and server SNI certificate selection.
- gRPC TLS credentials, protobuf/codegen integration, reflection, and compression.
- WebSocket server TLS configuration and permessage-deflate.
