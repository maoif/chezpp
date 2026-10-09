# chezpp

Chezpp is a collection of ChezScheme libraries and a Make-based build that produces the
`chez++` launcher. The public umbrella library is `(chezpp)`; individual libraries can be
imported when a smaller dependency set is useful.

## ✨ Features

- **Language tools:** algebraic data types and records, pattern matching, comprehensions,
  `for` loops, iterators, transducers, data navigator, control helpers, and list, string, and vector utilities.
- **Collections:** arrays, bit vectors, bit trees, doubly linked lists, sets, heaps, queues,
  stacks, and ordered maps and sets.
- **Parsing and formats:** parser combinators, CSV, JSON5, TOML, XML, Scheme, ELF, WebAssembly,
  and Protocol Buffers.
- **Files and system APIs:** file and port helpers, paths, globbing, processes,
  signals, filesystem and user information, UUIDs, hashes, digests, and optional-library probes.
- **Networking:** sockets, IP and DNS helpers, URIs, TLS, HTTP/HTTPS, FTP/FTPS, SSH, SFTP, SCP,
  WebSocket, and gRPC APIs.
- **Cryptography:** random data, hashes, encodings, MACs, KDFs, AEAD, ciphers, keys, signatures,
  key agreement, passwords, envelopes, and certificates.
- **Application support:** command-line parsing, logging, terminal rendering, benchmarking, and
  test helpers.
- **Concurrency:** threads, thread pools, atomic boxes, spinlocks, futures, and fiber libraries.
  Fibers and fiber-aware network operations are available as `(chezpp concurrency fiber)` and
  `(chezpp concurrency fiber-net)`.

Some networking, cryptography, hashing, compression, and UUID features use optional native
libraries selected at build time. `(chezpp optional-library)` reports which features were enabled.

## 🔧 Build

Build requirements:

- GNU Make
- GCC or Clang
- Git with submodule support

Build the library and launcher:

```sh
git clone --depth=1 https://github.com/maoif/chezpp.git
cd chezpp
make clean && make
```

The repository pins a release ChezScheme in `vendor/ChezScheme`. Ordinary `make` builds and
installs that bundled compiler under `.chezscheme-build` and `.chezscheme-install` before building
Chezpp. If the submodule contents are absent, Make initializes it with a filtered shallow checkout.
The bundled compiler and its matching `scheme.h` are mandatory; the build does not accept a
`SCHEME` override or use an ambient ChezScheme installation. A `scheme` symlink beside `chez++`
points at the generated compiler.

Use a different C compiler when needed:

```sh
make CC=clang
```

The build checks that the bundled ChezScheme executable and development header have the same
version. `make clean` removes Chezpp outputs while preserving the submodule and generated
ChezScheme toolchain. `make clean-all` performs that cleanup and removes `.chezscheme-build`,
`.chezscheme-install`, and the generated root `scheme` symlink.

### Optional native libraries

Optional development packages are not required to build or import Chezpp. Each dependency has
a `WITH_<NAME>` switch: `auto` (the default) enables it when its headers and library pass a
compile/link probe, `1` requires it and reports a build error if unavailable, and `0` disables
it without probing or linking it.

| Library / feature | Switch | Compiler flags | Linker flags |
| --- | --- | --- | --- |
| c-ares / DNS | `WITH_CARES` | `CARES_CFLAGS` | `CARES_LIBS` |
| cURL / FTP | `WITH_CURL` | `CURL_CFLAGS` | `CURL_LIBS` |
| gRPC | `WITH_GRPC` | `GRPC_CFLAGS` | `GRPC_LIBS` |
| libidn2 / IDNA | `WITH_IDN2` | `IDN2_CFLAGS` | `IDN2_LIBS` |
| libssh / SSH, SFTP, SCP | `WITH_LIBSSH` | `LIBSSH_CFLAGS` | `LIBSSH_LIBS` |
| libwebsockets / HTTP, WebSocket | `WITH_WEBSOCKETS` | `WEBSOCKETS_CFLAGS` | `WEBSOCKETS_LIBS` |
| zlib / compression | `WITH_ZLIB` | `ZLIB_CFLAGS` | `ZLIB_LIBS` |
| OpenSSL / TLS, crypto, digests | `WITH_OPENSSL` | `OPENSSL_CFLAGS` | `OPENSSL_LIBS` |
| libuuid / UUID | `WITH_UUID` | `UUID_CFLAGS` | `UUID_LIBS` |
| xxHash | `WITH_XXHASH` | `XXHASH_CFLAGS` | `XXHASH_LIBS` |
| BLAKE3 | `WITH_BLAKE3` | `BLAKE3_CFLAGS` | `BLAKE3_LIBS` |

Detection uses `pkg-config`; set `PKG_CONFIG` to choose its executable. A supplied compiler or
linker variable overrides that side of the package flags, and the missing side comes from
`pkg-config`. Without usable package metadata, supply both variables; an explicitly empty
value counts as supplied. `auto` disables dependencies it cannot detect.

```sh
# Detect available dependencies automatically.
make clean && make WITH_CURL=auto

# Build with FTP support disabled.
make clean && make WITH_CURL=0

# Require cURL using its installed pkg-config metadata.
make clean && make WITH_CURL=1

# Require cURL using manual flags, including when pkg-config is unavailable.
make clean && make WITH_CURL=1 \
  CURL_CFLAGS='-I/opt/curl/include' CURL_LIBS='-L/opt/curl/lib -lcurl'
```

Enabled dependencies use ordinary dynamic linking and must also be present at runtime.
Disabled features remain importable; calling an API that needs one raises a Scheme error
identifying build-time disablement. Feature choices and dependency flags are recorded in the
build signature, so changing them triggers a clean rebuild. `make print-build-options` shows
the requested and resolved choices.

## ▶️ Use

Start a Chezpp REPL:

```sh
make run
```

The generated `chez++` launcher also runs scripts:

```sh
./chez++ --script path/to/program.ss
```

For example:

```scheme
(import (chezpp))

(displayln
  (into 'list
        (tmap string-upcase)
        '("chez" "scheme")))
```

This prints:

```text
(CHEZ SCHEME)
```

## 📦 Install

Install the compiled library and launcher under an absolute prefix:

```sh
make install PREFIX=/absolute/path/to/install
```

The launcher is installed at `<prefix>/bin/chez++`.

## 🧪 Test

Build first, then run all test suites:

```sh
make clean && make
make -C tests test-all
```

Run selected suites by their test names:

```sh
make -C tests test-some TEST='array vector cli rich'
make -C tests test-some TEST='net-http net-lws-http2 net-lws-server'
```

Tests retain the native feature settings from the root build. Clauses that require disabled
libraries report `Skipped mat ...` individually; unrelated clauses and disabled-API error
checks continue to run. Run the build and direct-link regression fixtures with
`make -C tests test-optional-library-build`.
