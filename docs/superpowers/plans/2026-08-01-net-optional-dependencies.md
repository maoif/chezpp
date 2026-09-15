# Net Optional Dependencies Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development
> (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make every external native dependency except libc and `libuuid` lazy, version-checked,
capability-reporting, and non-fatal at Chezpp import time.

**Architecture:** A shared C loader owns synchronization, SONAME probing, symbol resolution,
version validation, and diagnostics. Protocol-specific function tables remain in their owning C
modules, while a Scheme record API exposes availability without forcing a library load failure.

**Tech Stack:** C11 atomics/POSIX mutexes, `dlopen`/`dlsym`, ChezScheme FFI, OpenSSL 3, libcurl,
libssh, libwebsockets, gRPC/gpr, BLAKE3, xxHash, shell linkage tests, and `readelf`.

---

### Task 1: Shared Optional-Library Loader

**Files:**
- Create: `chezpp/c/optional_library.h`
- Create: `chezpp/c/optional_library.c`
- Create: `tests/optional-library-loader.c`
- Create: `tests/optional-library-loader.sh`
- Modify: `Makefile`
- Modify: `tests/Makefile`

- [ ] **Step 1: Write the failing loader test**

Create a shell test that builds a good fixture and a fixture missing `fixture_value`, then invokes
a small C harness against each isolated `LD_LIBRARY_PATH`:

```c
/* tests/optional-library-loader.c */
#include "../chezpp/c/optional_library.h"
#include <stdio.h>

int main(int argc, char **argv) {
  const char *names[] = {argv[1], NULL};
  chezpp_optional_library library =
      CHEZPP_OPTIONAL_LIBRARY_INIT("fixture", names);
  void *value = NULL;
  if (!chezpp_optional_library_open(&library)) {
    puts(chezpp_optional_library_error(&library));
    return 2;
  }
  if (!chezpp_optional_library_symbol(&library, "fixture_value", &value)) {
    puts(chezpp_optional_library_error(&library));
    return 3;
  }
  return value == NULL;
}
```

The shell assertions are:

```bash
./tests/optional-library-loader /missing/libchezpp-fixture.so >missing.out
test $? -eq 2
grep -F 'fixture: unable to load' missing.out

LD_LIBRARY_PATH="$good_dir" ./tests/optional-library-loader libchezpp-fixture.so

LD_LIBRARY_PATH="$bad_dir" ./tests/optional-library-loader libchezpp-fixture.so >symbol.out
test $? -eq 3
grep -F 'fixture: missing symbol fixture_value' symbol.out
```

- [ ] **Step 2: Run the test and verify failure**

Run:

```bash
./tests/optional-library-loader.sh
```

Expected: compilation fails because `chezpp/c/optional_library.h` does not exist.

- [ ] **Step 3: Define the loader contract**

Create `chezpp/c/optional_library.h` with this public C-internal API:

```c
#ifndef CHEZPP_OPTIONAL_LIBRARY_H
#define CHEZPP_OPTIONAL_LIBRARY_H

#include <pthread.h>
#include <stddef.h>

typedef struct {
  const char *name;
  const char *const *sonames;
  void *handle;
  pthread_mutex_t mutex;
  int state; /* 0 uninitialized, 1 ready, -1 failed */
  char loaded_name[128];
  char version[128];
  char error[512];
} chezpp_optional_library;

#define CHEZPP_OPTIONAL_LIBRARY_INIT(label, candidates) \
  {label, candidates, NULL, PTHREAD_MUTEX_INITIALIZER, 0, "", "", ""}

int chezpp_optional_library_open(chezpp_optional_library *library);
int chezpp_optional_library_symbol(chezpp_optional_library *library,
                                   const char *name, void **target);
void chezpp_optional_library_fail(chezpp_optional_library *library,
                                  const char *format, ...);
void chezpp_optional_library_set_version(chezpp_optional_library *library,
                                         const char *version);
const char *chezpp_optional_library_error(chezpp_optional_library *library);
void chezpp_optional_library_reset_for_test(chezpp_optional_library *library);

#endif
```

Implement `open` with `RTLD_NOW | RTLD_LOCAL`, preserve every attempted SONAME in the final error,
clear `dlerror()` before `dlsym`, serialize state changes with the descriptor mutex, and close a
partially initialized handle before setting state to failed.

- [ ] **Step 4: Run the loader test**

Run:

```bash
./tests/optional-library-loader.sh
```

Expected: exit 0 and no output.

- [ ] **Step 5: Register the loader tests**

Add to `tests/Makefile`:

```make
.PHONY: test-optional-library-loader
test-optional-library-loader:
	@./optional-library-loader.sh
```

- [ ] **Step 6: Commit**

```bash
git add Makefile chezpp/c/optional_library.h chezpp/c/optional_library.c \
  tests/Makefile tests/optional-library-loader.c tests/optional-library-loader.sh
git commit -m "build: add shared optional library loader"
```

### Task 2: Migrate BLAKE3 And xxHash To The Shared Loader

**Files:**
- Modify: `chezpp/c/optional_hash.c`
- Modify: `tests/optional-hash-linkage.sh`
- Test: `tests/optional-hash-libs.ss`

- [ ] **Step 1: Extend hash loader failure tests**

Add cases asserting that concurrent calls return the same version error and that a missing symbol
names the symbol:

```scheme
(mat optional-hash-load-diagnostics
     (let ([messages
            (run-threads 8
              (lambda () (xxhash-load-error))
              (lambda () (xxhash-load-error)))])
       (and (andmap string? messages)
            (andmap (lambda (message) (string=? message (car messages))) messages))))
```

Use the linkage script's fixture library so this test does not depend on host installation state.

- [ ] **Step 2: Run and verify the test fails**

Run:

```bash
cd tests && make test-some TEST='optional-hash-libs'
```

Expected: FAIL because the existing loader has no shared synchronized descriptor.

- [ ] **Step 3: Replace private loader state**

Replace the local `optional_library` implementation in `chezpp/c/optional_hash.c` with:

```c
#include "optional_library.h"

static const char *const xxhash_names[] = {
    "libxxhash.so.0", "libxxhash.so", NULL};
static chezpp_optional_library xxhash_library =
    CHEZPP_OPTIONAL_LIBRARY_INIT("xxhash", xxhash_names);

static const char *const blake3_names[] = {
    "libblake3.so.1", "libblake3.so", NULL};
static chezpp_optional_library blake3_library =
    CHEZPP_OPTIONAL_LIBRARY_INIT("blake3", blake3_names);
```

Keep the existing exact major/minor policies and ABI tables. Route errors through
`chezpp_optional_library_fail` and record successful runtime versions.

- [ ] **Step 4: Run focused tests**

```bash
make clean && make
cd tests && make test-some TEST='hash optional-hash-libs'
./optional-hash-linkage.sh
```

Expected: exit 0; Scheme test stdout/stderr are empty.

- [ ] **Step 5: Commit**

```bash
git add chezpp/c/optional_hash.c tests/optional-hash-libs.ss tests/optional-hash-linkage.sh
git commit -m "hash: use shared optional library loader"
```

### Task 3: Dynamically Load OpenSSL For Crypto, Digest, And TLS

**Files:**
- Create: `chezpp/c/openssl_loader.h`
- Create: `chezpp/c/openssl_loader.c`
- Modify: `chezpp/c/crypto.c`
- Modify: `chezpp/c/digest.c`
- Modify: `chezpp/c/net/tls.c`
- Modify: `Makefile`
- Create: `tests/optional-openssl.sh`
- Modify: `tests/crypto.ss`
- Modify: `tests/net-core.ss`

- [ ] **Step 1: Add missing and incompatible OpenSSL tests**

Add a shell fixture that compiles `libcrypto.so.3` with an incompatible runtime version:

```c
/* Generated by tests/optional-openssl.sh in its temporary directory. */
unsigned long OpenSSL_version_num(void) { return 0x20000000UL; }
const char *OpenSSL_version(int kind) { (void)kind; return "OpenSSL 2.0 fixture"; }
```

The script assertions are:

```bash
printf '%s\n' '(import (chezpp)) (display "ok")' | \
  env LD_LIBRARY_PATH="$fixture_dir" ../chez++ -q >import.out 2>import.err
test "$(cat import.out)" = ok
test ! -s import.err

printf '%s\n' \
  '(import (chezpp))' \
  '(guard (c [else (display-condition c)]) (random-bytes 8))' | \
  env LD_LIBRARY_PATH="$fixture_dir" ../chez++ -q >use.out 2>use.err
grep -F 'OpenSSL runtime ABI major 2; requires major 3' use.out
```

- [ ] **Step 2: Run the tests and verify current direct linkage fails the premise**

Run:

```bash
readelf -d libchezpp.so | rg 'libssl|libcrypto'
cd tests && ./optional-openssl.sh
```

Expected: `readelf` reports direct OpenSSL dependencies and the fixture test cannot isolate them.

- [ ] **Step 3: Add the shared OpenSSL handles and symbol API**

Define one version-checked handle pair shared by crypto, digest, and TLS:

```c
int chezpp_openssl_require(void);
int chezpp_openssl_crypto_symbol(const char *name, void **target);
int chezpp_openssl_ssl_symbol(const char *name, void **target);
const chezpp_optional_library *chezpp_openssl_library(void);
```

The initialization sequence loads `libcrypto.so.3` and `libssl.so.3`, resolves version functions
first, requires `(OpenSSL_version_num() >> 28) == 3`, resolves all required symbols, initializes
crypto/SSL, and publishes the table only after complete success.

- [ ] **Step 4: Convert all direct OpenSSL calls**

Include `openssl_loader.h` in the three owning modules. Declare typed local pointers with the
installed OpenSSL headers and resolve each pointer during that module's initialization:

```c
#define OSSL_DECL(name) static __typeof__(&name) p_##name = NULL
#define OSSL_LOAD(name) \
  chezpp_openssl_crypto_symbol(#name, (void **)&p_##name)

OSSL_DECL(EVP_MD_CTX_new);
OSSL_DECL(EVP_MD_CTX_free);

if (!chezpp_openssl_require())
  return make_error_status_message(
      chezpp_optional_library_error(chezpp_openssl_library()));
if (!OSSL_LOAD(EVP_MD_CTX_new) || !OSSL_LOAD(EVP_MD_CTX_free))
  return make_error_status_message(
      chezpp_optional_library_error(chezpp_openssl_library()));
ctx = p_EVP_MD_CTX_new();
```

Apply this mechanical declaration/load/call conversion to every OpenSSL identifier reported by
`nm -D --undefined-only libchezpp.so` before removing linkage. Do not leave direct OpenSSL
undefined symbols. Remove `-lssl -lcrypto` from `LDLIBS`.

- [ ] **Step 5: Run the undefined-symbol audit**

Run:

```bash
make clean && make
nm -D --undefined-only libchezpp.so | rg ' (SSL_|OPENSSL_|EVP_|X509_|RAND_|PEM_)'
```

Expected: `rg` exits 1 with no matches.

- [ ] **Step 6: Run crypto and TLS tests**

```bash
cd tests && make test-some TEST='crypto hash optional-openssl net-core'
```

Expected: exit 0 and empty test stdout/stderr.

- [ ] **Step 7: Commit**

```bash
git add Makefile chezpp/c/openssl_loader.h chezpp/c/openssl_loader.c chezpp/c/crypto.c \
  chezpp/c/digest.c chezpp/c/net/tls.c tests/optional-openssl.sh tests/crypto.ss \
  tests/net-core.ss
git commit -m "crypto: load OpenSSL dynamically"
```

### Task 4: Unify libcurl, libssh, libwebsockets, And gRPC Loading

**Files:**
- Modify: `chezpp/c/net/ftp.c`
- Modify: `chezpp/c/net/ssh.c`
- Modify: `chezpp/c/net/websocket.c`
- Modify: `chezpp/c/net/grpc.c`
- Create: `tests/net-loader.ss`
- Create: `tests/optional-library-linkage.sh`
- Modify: `tests/Makefile`

- [ ] **Step 1: Write capability/version fixture tests**

For each library build three fixture variants: missing required symbol, version below minimum, and
compatible version. Assert the public probe record fields:

```scheme
(define check-unavailable
  (lambda (name expected)
    (let ([info (optional-library-info name)])
      (and (not (optional-library-available? info))
           (string-contains? (optional-library-error info) expected)))))

(mat net-loader-errors
     (check-unavailable 'curl "curl_version_info")
     (check-unavailable 'ssh "requires libssh")
     (check-unavailable 'websockets "runtime ABI")
     (check-unavailable 'grpc "grpc_version_string"))
```

- [ ] **Step 2: Run the test and verify it fails**

```bash
cd tests && make test-some TEST='net-loader'
```

Expected: FAIL because `optional-library-info` is not defined.

- [ ] **Step 3: Replace module-local loaders**

Use these version sources and policies:

```text
curl:       curl_version_info(CURLVERSION_NOW), ABI SONAME 4, >= 8.0.0
libssh:     ssh_version(SSH_VERSION_INT(0, 10, 0)), ABI SONAME 4
websockets: lws_get_library_version(), ABI SONAME 21, >= 4.3.0
grpc:       grpc_version_string(), matching supported gRPC ABI major, >= 54.0.0
gpr:        loaded as the matching companion of the accepted gRPC runtime
```

Resolve version functions before all other symbols. Retain optional capabilities as bit flags,
including libssh AIO, WebSocket compression/TLS, and gRPC TLS/compression functions.

- [ ] **Step 4: Make feature failure messages precise**

Every `ensure_*_loaded` failure returns the descriptor error instead of a generic message:

```c
if (!ensure_curl_loaded())
  return make_error_status_message(
      chezpp_optional_library_error(&curl_library));
```

Apply the same pattern to SSH, WebSocket, gRPC, and gpr.

- [ ] **Step 5: Run protocol smoke tests under installed libraries**

```bash
make clean && make
cd tests && make test-some TEST='net-ftp net-ssh net-sftp net-scp net-websocket net-grpc'
```

Expected: exit 0 and empty test stdout/stderr.

- [ ] **Step 6: Commit**

```bash
git add chezpp/c/net/ftp.c chezpp/c/net/ssh.c chezpp/c/net/websocket.c \
  chezpp/c/net/grpc.c tests/net-loader.ss tests/optional-library-linkage.sh tests/Makefile
git commit -m "net: validate optional protocol libraries"
```

### Task 5: Public Capability Records

**Files:**
- Create: `chezpp/optional-library.ss`
- Modify: `chezpp.ss`
- Modify: `chezpp/net/ffi.ss`
- Modify: `tests/net-loader.ss`

- [ ] **Step 1: Define and test the public record contract**

Add tests for all exported accessors:

```scheme
(mat optional-library-record
     (let ([info (optional-library-info 'openssl)])
       (and (optional-library-info? info)
            (eq? (optional-library-name info) 'openssl)
            (boolean? (optional-library-available? info))
            (or (not (optional-library-version info))
                (string? (optional-library-version info)))
            (list? (optional-library-capabilities info))
            (or (not (optional-library-error info))
                (string? (optional-library-error info))))))
```

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-loader'
```

Expected: FAIL because the library and record do not exist.

- [ ] **Step 3: Implement the Scheme API**

Export:

```scheme
optional-library-info?
optional-library-info
optional-library-name
optional-library-available?
optional-library-version
optional-library-capabilities
optional-library-error
```

Define the record adjacent to `#|record:optional-library-info|#` documentation. Implement
`optional-library-info` with `pcheck`, accept only the supported symbols, and decode a fixed FFI
vector `(name available? version capabilities error)`.

- [ ] **Step 4: Register the library**

Import and export `(chezpp optional-library)` from `chezpp.ss`. Add its source automatically through
the existing `SRCS_CHEZPP` Makefile discovery.

- [ ] **Step 5: Build and test**

```bash
make clean && make
cd tests && make test-some TEST='net-loader optional-hash-libs crypto net-core'
```

Expected: exit 0 and empty test stdout/stderr.

- [ ] **Step 6: Commit**

```bash
git add chezpp/optional-library.ss chezpp.ss chezpp/net/ffi.ss tests/net-loader.ss
git commit -m "net: expose optional library capabilities"
```

### Task 6: Phase 1 Release Gate

**Files:**
- Review all Phase 1 files.

- [ ] **Step 1: Run the project build**

```bash
make clean && make
```

Expected: exit 0.

- [ ] **Step 2: Verify direct dependencies**

```bash
readelf -d libchezpp.so | rg 'NEEDED'
```

Expected: among scoped libraries, only `libc.so` and `libuuid.so` appear. No OpenSSL, curl,
libssh, libwebsockets, gRPC, gpr, BLAKE3, or xxHash dependency appears.

- [ ] **Step 3: Run loader and affected library tests**

```bash
cd tests
make test-optional-library-loader
make test-optional-hash-linkage
./optional-library-linkage.sh
make test-some TEST='optional-hash-libs net-loader crypto hash net-core net-ftp net-ssh net-sftp net-scp net-websocket net-grpc'
```

Expected: every command exits 0; Scheme test stdout/stderr are empty.

- [ ] **Step 4: Check changed Scheme files**

```bash
../chez++ --script ../tools/check-scheme-balance.ss \
  ../chezpp/optional-library.ss ../chezpp/net/ffi.ss
```

Expected: balanced parentheses for both files.

- [ ] **Step 5: Record the phase gate**

```bash
git status --short
```

Expected: empty. Phase 2 may now begin.
