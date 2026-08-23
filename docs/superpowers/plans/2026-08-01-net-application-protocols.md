# Net Application Protocols Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development
> (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add streaming HTTP/1.1 and HTTP/2 features, complete WebSocket controls, provide protobuf
code generation, harden gRPC, and verify bounded-memory file transfer over every application
protocol and secure variant.

**Architecture:** Streaming sources and sinks are shared transfer primitives. HTTP/2 uses an
optional nghttp2 adapter, WebSocket uses bounded fragments, and gRPC generated bindings encode
protobuf messages while all transports expose the Phase 2 readiness contract.

**Tech Stack:** ChezScheme custom ports and records, nghttp2, zlib-compatible compression,
libwebsockets, gRPC C core, `protoc` plugin protocol, OpenSSL TLS, and SHA-256 examples.

## Current Status (2026-08-23)

Phase 4 protocol implementation is complete through HTTP/2 corrective commit `4150676`:
protobuf/codegen, HTTP streaming and HTTP/2 integration, WebSocket TLS/compression, gRPC
TLS/policy/reflection, generated `FileChunk` transfers, and deterministic HTTP/2 TLS/cancellation
regressions are implemented. The clean build, focused suites, ten-run HTTP/2 stress loop,
complete net/protobuf suite, generated-binding comparison, documentation and linkage audits, and
ten-variant application transfer verification pass. The pinned external download gate also passes
with the documented Emacs and Arch SHA-256 values.

---

### Task 1: Protobuf Wire Codec

**Files:**
- Create: `chezpp/protobuf/wire.ss`
- Create: `chezpp/protobuf.ss`
- Modify: `chezpp.ss`
- Create: `tests/protobuf.ss`
- Modify: `tests/Makefile`

- [ ] **Step 1: Add known-wire-vector tests**

```scheme
(mat protobuf-wire
     (equal? #vu8(150 1) (protobuf-encode-varint 150))
     (= 150 (protobuf-decode-varint #vu8(150 1)))
     (equal? #vu8(10 3 97 98 99)
             (protobuf-encode-field 1 'string "abc"))
     (equal? #vu8(8 1 18 3 102 111 111)
             (protobuf-encode-message
              '((1 bool #t) (2 string "foo")))))
```

Add negative tests for truncated varints, a tenth continuation byte, field number zero, invalid
wire type, truncated fixed-width fields, and length beyond remaining input. Comment each case.

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='protobuf'
```

Expected: FAIL because `(chezpp protobuf)` does not exist.

- [ ] **Step 3: Implement primitive codecs**

Export documented procedures for unsigned/signed/zigzag varints, fixed32/fixed64, float/double,
bool, enum, bytes, string, embedded messages, tags, unknown-field skipping, and field iteration.
Use Chez bytevector native accessors for fixed-width values and explicit little endianness where
required by protobuf. Reject field numbers above 536870911.

- [ ] **Step 4: Implement bounded writers and readers**

Precompute encoded sizes and allocate one output bytevector. A decoder record owns the source,
current index, limit, recursion depth, and unknown fields. Default recursion limit is 100 and is
configurable through an explicit constructor arity.

- [ ] **Step 5: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='protobuf'
cd ..
git add chezpp/protobuf.ss chezpp/protobuf/wire.ss chezpp.ss tests/protobuf.ss tests/Makefile
git commit -m "protobuf: add wire codec"
```

### Task 2: Descriptor Decoder And protoc Plugin

**Files:**
- Create: `chezpp/protobuf/descriptor.ss`
- Create: `tools/protoc-gen-chezpp.ss`
- Create: `tools/protoc-gen-chezpp`
- Create: `tests/data/file-transfer.proto`
- Create (generated): `tests/generated/file-transfer.pb.ss`
- Create: `tests/protobuf-codegen.ss`
- Modify: `Makefile`

- [ ] **Step 1: Add a representative schema**

```proto
syntax = "proto3";
package chezpp.examples.transfer;

message FileChunk {
  string name = 1;
  uint64 offset = 2;
  bytes data = 3;
  bytes sha256 = 4;
  bool done = 5;
}

message TransferResult {
  uint64 size = 1;
  bytes sha256 = 2;
}

service FileTransfer {
  rpc Upload(stream FileChunk) returns (TransferResult);
  rpc Download(FileChunk) returns (stream FileChunk);
}
```

- [ ] **Step 2: Add generation and round-trip tests**

Run `protoc` with `--chezpp_out`, compile the generated library, construct `FileChunk`, encode it,
decode it, and assert equality of every field. Assert generated method names are:

```text
/chezpp.examples.transfer.FileTransfer/Upload
/chezpp.examples.transfer.FileTransfer/Download
```

- [ ] **Step 3: Run and verify failure**

```bash
cd tests && make test-some TEST='protobuf-codegen'
```

Expected: FAIL because the plugin does not exist.

- [ ] **Step 4: Decode protoc descriptors**

Implement the subset of `google.protobuf.compiler.CodeGeneratorRequest`,
`CodeGeneratorResponse`, `FileDescriptorProto`, `DescriptorProto`, `FieldDescriptorProto`,
`EnumDescriptorProto`, `ServiceDescriptorProto`, and `MethodDescriptorProto` required for proto2
and proto3 generation. Preserve uninterpreted options so unsupported options can be diagnosed.

- [ ] **Step 5: Generate Scheme records and codecs**

For every message generate a sealed non-opaque record, constructor, predicate, accessors,
`<message>-encoded-size`, `<message>-encode`, and `bytevector-><message>`. Preserve unknown fields
for decode/re-encode. Repeated fields use vectors; maps use hashtables; oneofs use a tagged pair;
absent optional scalar fields use an explicit presence bit.

- [ ] **Step 6: Generate gRPC helpers**

Generate client procedures for each RPC shape and one server registration procedure per service.
Generated clients encode request records and decode response records. Generated server wrappers
decode requests before invoking the user handler and encode returned records.

- [ ] **Step 7: Install the plugin launcher**

Make `tools/protoc-gen-chezpp` execute the repository's `chez++` with the Scheme plugin. Add a
`protobuf-generate` Make target that regenerates test bindings reproducibly.

- [ ] **Step 8: Build, test, and commit**

```bash
make clean && make
make protobuf-generate
cd tests && make test-some TEST='protobuf protobuf-codegen'
cd ..
git add chezpp/protobuf/descriptor.ss tools/protoc-gen-chezpp.ss tools/protoc-gen-chezpp \
  tests/data/file-transfer.proto tests/protobuf-codegen.ss Makefile
git commit -m "protobuf: generate records and gRPC bindings"
```

### Task 3: HTTP Streaming Bodies

**Files:**
- Modify: `chezpp/net/http.ss`
- Modify: `tests/net-http.ss`
- Modify: `tests/net-common.ss`

- [ ] **Step 1: Add bounded-memory streaming tests**

Generate a deterministic 64 MiB body without storing it twice. Stream it through local PUT and
GET endpoints in 64 KiB chunks, record the maximum buffered bytes, and assert it remains below
512 KiB. Verify the SHA-256 at both ends.

```scheme
(mat net-http-stream-body
     (let ([source (make-http-body-source
                    (lambda (maximum-bytes) (eof-object))
                    #f)]
           [sink (make-http-body-sink
                  (lambda (bytevector start stop) (void)))])
       (and (http-body-source? source)
            (not (http-body-source-length source))
            (http-body-sink? sink))))
```

The producer signature is `(maximum-bytes) -> bytevector-or-eof`. The consumer signature is
`(bytevector start stop) -> unspecified`.

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-http'
```

Expected: FAIL because HTTP bodies are materialized bytevectors.

- [ ] **Step 3: Define body source and sink records**

Export constructors, predicates, length accessor, source read, and sink write/finish procedures.
Add file and port adapters. Document ownership: HTTP never closes caller-owned ports; file helper
ports created by HTTP are closed on terminal operation state.

- [ ] **Step 4: Stream requests and responses**

Known-length bodies send `Content-Length`. Unknown-length HTTP/1.1 bodies use chunked encoding.
Readers feed sinks as chunks arrive and expose parsed trailers after the terminating chunk.
Convenience calls collect into memory only when the caller requests an in-memory response.

- [ ] **Step 5: Update download and upload APIs**

`http-download` streams directly to a replace-or-resume temporary file and atomically renames on
success. `http-upload` streams from the source file. Nonblocking versions return net operations;
blocking versions wait on those operations.

- [ ] **Step 6: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-http net-operation'
cd ..
git add chezpp/net/http.ss tests/net-http.ss tests/net-common.ss
git commit -m "net: stream HTTP request and response bodies"
```

### Task 4: HTTP Compression, Cookies, Auth, Proxy, Multipart, And Pool Controls

**Files:**
- Create: `chezpp/c/zlib_loader.h`
- Create: `chezpp/c/zlib_loader.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/http.ss`
- Modify: `tests/net-loader.ss`
- Modify: `tests/net-http.ss`

- [ ] **Step 1: Add feature tests**

Add local tests for gzip/deflate request and response streaming, secure/domain/path cookie rules,
Basic and Bearer authentication, HTTP proxy absolute-form requests, HTTPS CONNECT tunneling,
multipart fields/files, response trailers, idle/active pool limits, and connection eviction.

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-http net-loader'
```

Expected: FAIL because these APIs do not exist.

- [ ] **Step 3: Add optional zlib loading**

Load `libz.so.1`, call `zlibVersion`, require ABI major 1 and runtime >= 1.2.11, then resolve the
streaming deflate/inflate functions. Add zlib to `optional-library-info` and the linkage fixture.

- [ ] **Step 4: Add HTTP records**

Define and document `http-cookie`, `http-cookie-jar`, `http-proxy`, `http-multipart-part`, and
`http-pool-policy`. Cookie jar mutation is synchronized because clients may be shared. Sensitive
authentication values are never included in record writers or error messages.

- [ ] **Step 5: Add public configuration APIs**

```scheme
http-client-cookie-jar-set!
http-client-auth-set!
http-client-proxy-set!
http-client-pool-policy-set!
make-http-multipart-body
http-response-trailers
```

Support `basic`, `bearer`, and a caller procedure with signature
`(request response-or-#f) -> request`. Redirects remove origin credentials when authority changes.

- [ ] **Step 6: Implement streaming feature composition**

Apply request transforms in this order: auth/cookies, multipart source, compression, framing.
Apply response transforms in reverse: framing, decompression, sink. Enforce decompressed-size and
compression-ratio limits to prevent resource exhaustion.

- [ ] **Step 7: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-http net-loader'
cd ..
git add chezpp/c/zlib_loader.h chezpp/c/zlib_loader.c chezpp/net/ffi.ss chezpp/net/http.ss \
  tests/net-loader.ss tests/net-http.ss
git commit -m "net: add HTTP client and body features"
```

### Task 5: HTTP/2 Client And Server Adapter

**Files:**
- Create: `chezpp/c/nghttp2_loader.h`
- Create: `chezpp/c/nghttp2_loader.c`
- Create: `chezpp/c/net/http2.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/http.ss`
- Modify: `tests/net-loader.ss`
- Modify: `tests/net-http.ss`
- Modify: `tests/net-common.ss`

- [ ] **Step 1: Add ALPN and multiplexing tests**

Start a local TLS server advertising `h2,http/1.1`. Assert negotiated `h2`, issue ten concurrent
stream operations over one connection, interleave response chunks, and verify each body. Add a
fallback test where the server selects HTTP/1.1.

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-http net-loader'
```

Expected: FAIL because nghttp2 is absent.

- [ ] **Step 3: Add optional nghttp2 loading**

Load `libnghttp2.so.14`, call `nghttp2_version(0)`, require the SONAME 14 ABI and runtime >=
1.52.0, then resolve session, callback, submit, send, receive, and flow-control symbols used by the
adapter. Expose version/capabilities through `optional-library-info`.

- [ ] **Step 4: Implement the C adapter**

Own one nghttp2 session per HTTP connection and one stream record per request. Feed bytes from the
existing TLS/socket operations into `nghttp2_session_mem_recv`; drain output with
`nghttp2_session_mem_send`. Copy header name/value bytes during callbacks and queue body chunks to
the Scheme sink. Never call Scheme from a native callback.

- [ ] **Step 5: Integrate HTTP version selection**

Add client policy symbols `auto`, `http/1.1`, and `h2`. TLS `auto` offers ALPN `h2,http/1.1`;
cleartext HTTP/2 uses explicit prior knowledge only. Server contexts dispatch by negotiated ALPN.
Expose `http-response-version`.

- [ ] **Step 6: Enforce flow control and cancellation**

Consume windows only after the Scheme sink accepts bytes. Cancelling one operation sends
RST_STREAM and leaves other streams usable. GOAWAY prevents new streams and lets eligible existing
streams complete.

- [ ] **Step 7: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-http net-loader net-core'
cd ..
git add chezpp/c/nghttp2_loader.h chezpp/c/nghttp2_loader.c chezpp/c/net/http2.c \
  chezpp/net/ffi.ss chezpp/net/http.ss tests/net-loader.ss tests/net-http.ss tests/net-common.ss
git commit -m "net: add HTTP/2 client and server support"
```

### Task 6: WebSocket TLS, Compression, Negotiation, Fragmentation, And Liveness

**Files:**
- Modify: `chezpp/c/net/websocket.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/websocket.ss`
- Modify: `tests/net-websocket.ss`
- Modify: `tests/net-common.ss`

- [ ] **Step 1: Add feature tests**

Test WSS with the local certificate, negotiation from two offered subprotocols, negotiated
permessage-deflate, a message fragmented across three frames, close code 1000 with reason, pong
deadline success, and timeout closure when pong is absent.

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-websocket'
```

Expected: FAIL because server TLS and negotiated state accessors are absent.

- [ ] **Step 3: Extend server and connection options**

Define `websocket-options` with TLS context-or-`#f`, offered subprotocols, compression enabled,
fragment size, ping interval, and pong timeout. Preserve current arities as defaults and add one
options arity.

- [ ] **Step 4: Expose negotiated and close state**

Export:

```scheme
websocket-negotiated-subprotocol
websocket-compression
websocket-close-code
websocket-close-reason
websocket-send-fragment/nonblocking
websocket-finish-message/nonblocking
websocket-ping-operation
```

Copy callback data before returning to libwebsockets. Validate close codes and UTF-8 reasons.

- [ ] **Step 5: Add permessage-deflate and TLS setup**

Configure libwebsockets extensions only when the runtime capability probe reports support. Server
TLS loads cert/key from the supplied Chezpp TLS context configuration. Missing compression support
raises `unsupported` only when requested.

- [ ] **Step 6: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-websocket net-loader net-core'
cd ..
git add chezpp/c/net/websocket.c chezpp/net/ffi.ss chezpp/net/websocket.ss \
  tests/net-websocket.ss tests/net-common.ss
git commit -m "net: complete WebSocket negotiation and liveness"
```

### Task 7: gRPC TLS, Compression, Deadlines, Status, Metadata, And Reflection

**Files:**
- Modify: `chezpp/c/net/grpc.c`
- Modify: `chezpp/net/ffi.ss`
- Modify: `chezpp/net/grpc.ss`
- Create: `chezpp/net/grpc/reflection.ss`
- Modify: `chezpp/net.ss`
- Modify: `tests/net-grpc.ss`
- Modify: `tests/protobuf-codegen.ss`

- [ ] **Step 1: Add gRPC behavior tests**

Test trusted TLS success, hostname mismatch, client certificate authentication, gzip compression,
deadline exceeded, cancellation, binary and ASCII metadata casing, status detail bytes, and
reflection listing the generated FileTransfer service.

- [ ] **Step 2: Run and verify failure**

```bash
cd tests && make test-some TEST='net-grpc protobuf-codegen'
```

Expected: FAIL because secure credentials and reflection are absent.

- [ ] **Step 3: Resolve optional gRPC capabilities**

Add TLS credential, call credential, compression, deadline, status-details, and channel/server
argument symbols to the gRPC function table. Report each optional feature through the gRPC
capability record and raise `unsupported` if requested against an older accepted runtime.

- [ ] **Step 4: Define credential and option records**

Document `grpc-channel-credentials`, `grpc-server-credentials`, `grpc-call-options`, and expanded
`grpc-status`. Credentials copy certificate/key bytes into native owned memory and clear private
key copies during close.

- [ ] **Step 5: Validate metadata**

Normalize ASCII keys to lowercase. Reject uppercase, non-ASCII, reserved `grpc-` keys from callers,
and bytevector values on keys without `-bin`. Preserve duplicate metadata entries and order.

- [ ] **Step 6: Apply deadlines and compression**

Use absolute deadlines derived once at operation creation. Propagate cancellation to
`grpc_call_cancel`. Permit `identity`, `gzip`, and runtime-reported algorithms; record the selected
algorithm in response metadata.

- [ ] **Step 7: Implement reflection**

Register the standard v1 server reflection bidi method. Answer list-services, file-by-filename,
file-containing-symbol, and file-containing-extension requests from generated descriptor bytes.
Deduplicate transitive descriptor dependencies.

- [ ] **Step 8: Build, test, and commit**

```bash
make clean && make
cd tests && make test-some TEST='net-grpc protobuf protobuf-codegen net-loader'
cd ..
git add chezpp/c/net/grpc.c chezpp/net/ffi.ss chezpp/net/grpc.ss \
  chezpp/net/grpc/reflection.ss chezpp/net.ss tests/net-grpc.ss tests/protobuf-codegen.ss
git commit -m "net: add secure generated gRPC services"
```

### Task 8: Application-Protocol File Transfer Examples

**Files:**
- Modify: `examples/net/file-transfer/file-transfer-http*.ss`
- Modify: `examples/net/file-transfer/file-transfer-websocket*.ss`
- Modify: `examples/net/file-transfer/file-transfer-grpc*.ss`
- Create: `examples/net/file-transfer/file-transfer-https.ss`
- Create: `examples/net/file-transfer/file-transfer-wss.ss`
- Create: `examples/net/file-transfer/file-transfer-grpc-tls.ss`
- Create: `examples/net/file-transfer/verify-application-transfers.sh`

- [ ] **Step 1: Replace full-buffer framing**

Use the generated `FileChunk` message for gRPC and the same logical fields for HTTP/WebSocket:
name, offset, bounded data, final SHA-256, and done flag. Default chunks are 64 KiB. Reject offsets
that are not exactly the next expected byte.

- [ ] **Step 2: Support upload and download in every example**

HTTP uses PUT and GET streaming. WebSocket uses metadata/chunk/completion messages in both
directions. gRPC uses client-streaming Upload and server-streaming Download. Secure variants use
the local test CA and verify hostname.

- [ ] **Step 3: Add deterministic verification**

Create a 64 MiB source, record SHA-256, run upload and download for HTTP, HTTPS, WebSocket, WSS,
gRPC, and TLS gRPC, and compare each destination. Record peak RSS before/after and fail if growth
exceeds 16 MiB per transfer.

- [ ] **Step 4: Run verification**

```bash
./examples/net/file-transfer/verify-application-transfers.sh
```

Expected: every variant reports matching hashes and bounded memory.

- [ ] **Step 5: Commit**

```bash
git add examples/net/file-transfer
git commit -m "net: add streaming application transfer examples"
```

### Task 9: Phase 4 Release Gate

**Files:**
- Review all Phase 4 files.

- [ ] **Step 1: Regenerate bindings and detect drift**

```bash
make protobuf-generate
git diff --exit-code -- tests/generated
```

Expected: generated files are reproducible with no diff.

- [ ] **Step 2: Build and run focused tests**

```bash
make clean && make
cd tests && make test-some TEST='protobuf protobuf-codegen net-http net-websocket net-grpc net-loader net-operation'
```

Expected: exit 0 and empty test stdout/stderr.

- [ ] **Step 3: Run application transfer verification**

```bash
./examples/net/file-transfer/verify-application-transfers.sh
```

Expected: all six protocol variants upload and download with matching hashes.

- [ ] **Step 4: Audit direct linkage, docs, and parentheses**

```bash
readelf -d libchezpp.so | rg 'libnghttp2|libz|libssl|libcrypto|libwebsockets|libgrpc'
./chez++ --script tools/check-scheme-balance.ss \
  chezpp/protobuf.ss chezpp/protobuf/*.ss chezpp/net/http.ss \
  chezpp/net/websocket.ss chezpp/net/grpc.ss chezpp/net/grpc/reflection.ss
git status --short
```

Expected: `readelf` has no matches, Scheme files are balanced, and status is empty.
