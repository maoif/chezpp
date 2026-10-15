# ChezPP Examples and Benchmarks

Run commands from the repository root with `chez++ --script`. Every command accepts `-h` and
`--help`. File arguments are paths relative to the current directory.

## Parsers

| Script | Invocation | Demonstrates |
|---|---|---|
| `examples/parser/xml.ss` | `--file=tests/data/xml-basic.xml` | XML document parser |
| `examples/parser/json.ss` | `--file=tests/data/json5-example.json5` | JSON5 parser |
| `examples/parser/toml.ss` | `--file=tests/data/pyproj.toml` | TOML parser |
| `examples/parser/scheme.ss` | `--file=examples/data/sample.scm` | Scheme datum parser |
| `examples/parser/elf.ss` | `--file=.chezscheme-install/bin/scheme` | ELF parser; input must be an ELF file |

## Data Processing

| Script | Invocation | Demonstrates |
|---|---|---|
| `examples/navigator/traversal.ss` | `--value='((name . Ada) (scores . #(2 4 6)))'` | select and transform |
| `examples/transducer/pipeline.ss` | `--limit=4` | composed map/filter/take pipeline |
| `examples/iter/operations.ss` | `--limit=4` | iterator map, filter, zip, fold, cleanup |
| `examples/glob/files.ss` | `--pattern='examples/**/*.ss' --limit=10` | matching, expansion, lazy iteration |

## Containers

| Script | Invocation | Demonstrates |
|---|---|---|
| `examples/containers/treeset.ss` | `--values=3,1,2,2` | generic and fixnum treesets |
| `examples/containers/treemap.ss` | `--values=3,1,2` | generic and fixnum treemaps |
| `examples/containers/heap.ss` | `--values=3,1,2` | heap priority operations |
| `examples/containers/dlist.ss` | `--values=3,1,2` | mutable doubly-linked list |
| `examples/containers/array.ss` | `--size=8` | generic, fixnum, and flonum arrays |

Heap and dlist currently provide generic APIs only. Typed variants are shown only for collections
that currently expose them.

## Networking

| Script | Invocation | Demonstrates |
|---|---|---|
| `examples/net/http-file-server.ss` | `--dir=examples/data --port=8080` | local HTTP/1 file server |
| `examples/net/http2-file-server.ss` | `--dir=examples/data --port=8443` | H2 server, requires H2 server support |
| `examples/net/https-file-server.ss` | `--dir=examples/data --port=8443 --cert=cert.pem --key=key.pem` | TLS file server |
| `examples/net/wget.ss` | `--uri=http://127.0.0.1:8080/sample.scm --output=sample.scm` | HTTP downloader |
| `examples/net/ssh-open-repl.ss` | `HOST PORT USER agent` | interactive remote shell |
| `examples/net/ssh-run-cmd.ss` | `--host=HOST --port=22 --user=USER --auth=agent --command='uname -a'` | remote command and streams |
| `examples/net/ssh-port-forward.ss` | `--host=HOST --user=USER --auth=agent --direction=local --target-host=127.0.0.1 --target-port=22` | SSH TCP forwarding |
| `examples/net/scp.ss` | `--host=HOST --user=USER --auth=agent --direction=upload --kind=file --local=FILE --remote=PATH` | SCP files and directories |

SSH uses strict host-key verification. Passwords and private-key passphrases are read from
environment variables, not command arguments. HTTPS and HTTP/2 rows are included only when the
active Chezpp build and public listener APIs support the named mode; see their `--help` output.

## Cryptography

| Script | Invocation | Demonstrates |
|---|---|---|
| `examples/crypto/encrypt-file.ss` | `--mode=encrypt --input=input --output=input.enc --method=aes-256-gcm` | authenticated file encryption/decryption |
| `examples/crypto/hash-file.ss` | `--file=input --method=sha256` | cryptographic hashes and checksums |

## Benchmarks

| Script | Invocation | Compares |
|---|---|---|
| `benchmarks/treeset.ss` | `--size=10000 --samples=10` | treeset and fxtreeset |
| `benchmarks/treemap.ss` | `--size=10000 --samples=10` | treemap and fxtreemap |
| `benchmarks/vectors.ss` | `--size=100000 --samples=10` | vector, fxvector, and flvector |
| `benchmarks/arrays.ss` | `--size=100000 --samples=10` | array, fxarray, and flarray |

Benchmarks accept text, CSV, JSON, or datum output where supported and report timing and allocation
summaries. Optional runtime cost-center metrics vary by build.
