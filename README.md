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

Some networking, cryptography, hashing, and compression features use native libraries loaded at
runtime. `(chezpp optional-library)` reports which optional libraries are available.

## 🔧 Build

Build requirements:

- ChezScheme and its matching development headers
- GNU Make
- GCC or Clang
- `libuuid` development headers and library

Build the library and launcher:

```sh
git clone --depth=1 https://github.com/maoif/chezpp.git
cd chezpp
make clean && make
```

Use a different compiler or ChezScheme executable when needed:

```sh
make CC=clang
make SCHEME=/path/to/scheme
```

The build checks that the ChezScheme executable and development headers have the same version.

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

