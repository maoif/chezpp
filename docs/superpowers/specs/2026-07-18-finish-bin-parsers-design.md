# Binary Parser Completion Design

**Goal:** Finish the ELF, Java class, and WebAssembly parsers, and add a real WAT parser, with
typed public data structures, parser-combinator entry points, strict validation, and content-level
tests.

## Architecture

Each format is split into a public facade, typed records, parser implementation, and validation
helpers. Binary parsing is expressed through Chezpp parser combinators, using bounded sub-parsers
for format-length-delimited structures. Format-specific semantic validation runs immediately after
the relevant structure is decoded so failures retain useful offsets.

ELF supports 32/64-bit files and both byte orders. Java class parsing covers the class-file format,
standard attributes, annotations, stack maps, descriptors, and bytecode operands. WebAssembly
binary and textual parsers produce the same canonical module records; the text parser handles WAT
syntax but deliberately does not parse WAST command scripts.

## Public API

The existing one-argument parser names remain available. They accept either a bytevector or a file
path for compatibility; explicit `...-file` variants are also provided. All exported procedures
and record accessors have documentation and public type checks.

## Testing

Tests use relative paths from `tests/`, such as `data/Pair.class` and `data/example.wasm`. Synthetic
bytevectors cover boundary and malformed cases. Fixture tests assert decoded field values, not only
record predicates. The final verification is `make clean && make` followed by the three parser test
targets, with empty stdout and stderr on success.
