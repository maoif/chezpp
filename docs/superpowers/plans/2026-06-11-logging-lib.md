# Logging Library Status

**Original goal:** Build the `(chezpp logging)` library described by
`docs/superpowers/specs/2026-06-10-logging-design.md`.

**Current status:** Implemented and integrated. The library exists under
`chezpp/logging/`, is re-exported by `(chezpp logging)`, and is included in the
aggregate `(chezpp)` library. The focused logging tests are registered in
`tests/Makefile` as `logging.ss`.

**Architecture:** Logging is implemented as focused ChezScheme libraries with a
public facade. The synchronous path passes direct log fields from loggers to
sinks without exposing public log-record objects.

**Tech stack:** ChezScheme libraries, Chezpp `pcheck`, Chez mutexes/threads,
existing `(chezpp rich)` console APIs, the existing `tests/mat.sls` harness, and
project build through `make clean && make`.

---

## Implemented File Structure

- `chezpp/logging/private/common.ss`: shared predicates, message conversion,
  newline normalization, current timestamp, and current Chez thread id lookup.
- `chezpp/logging/level.ss`: level predicates, conversions, ordering, and
  string parsing.
- `chezpp/logging/formatter.ss`: formatter records, pattern formatter, rich
  formatter, rich palettes, JSON-line formatter, default formatter, and direct
  field formatting.
- `chezpp/logging/sink.ss`: sink record, sink configuration APIs, port/file/
  rotating-file/procedure/null/tee/rich-console sink constructors, write, flush,
  close, locking, filtering, and close-state behavior.
- `chezpp/logging/logger.ss`: logger record, configuration APIs, filtering,
  level checks, sink dispatch, current logger parameter, `with-logger`, error
  policies, flush, close, and sink-list snapshotting.
- `chezpp/logging/macros.ss`: `logger` constructor macro, sink constructor
  macros, current-logger logging macros, and explicit-logger logging macros.
- `chezpp/logging.ss`: facade that re-exports level, formatter, sink, logger,
  and macro APIs.
- `tests/logging.ss`: regression tests for levels, formatters, sinks, loggers,
  macros, files, rotation, rich console output, error policy, and concurrency.

## Current Feature Set

### Levels

- Supported levels are `trace`, `debug`, `info`, `warn`, `error`, `critical`,
  and `off`.
- Message levels exclude `off`.
- APIs:
  - `log-level?`
  - `log-message-level?`
  - `log-level->integer`
  - `integer->log-level`
  - `log-level<?`
  - `log-level>=?`
  - `log-level->string`
  - `string->log-level`
- `string->log-level` is case-insensitive and accepts `warning` as an alias for
  `warn`.

### Formatters

- `log-formatter?` identifies formatter records.
- `make-log-pattern-formatter` supports these tokens:
  - `%T`: timestamp
  - `%L`: full lower-case level name
  - `%l`: short one-letter level name
  - `%n`: logger name
  - `%t`: thread id
  - `%s`: source metadata
  - `%m`: formatted message
  - `%%`: literal percent sign
- `make-log-json-line-formatter` emits one JSON object string per message with
  logger, level, timestamp, thread, source, and message fields.
- `make-log-rich-formatter` formats messages as rich text using the default
  palette or a caller-supplied palette.
- `make-log-rich-palette`, `log-rich-palette?`, and `log-rich-palette-ref`
  support level-specific rich styles.
- `log-default-formatter` returns the default plain text formatter using
  `[%L] %n %m`.
- `log-formatter-format` formats direct logger/sink fields.

### Sinks

- Common sink APIs:
  - `log-sink?`
  - `log-sink-name`
  - `log-sink-level`
  - `log-sink-level-set!`
  - `log-sink-filter`
  - `log-sink-filter-set!`
  - `log-sink-formatter`
  - `log-sink-formatter-set!`
  - `log-sink-write!`
  - `log-sink-flush!`
  - `log-sink-close!`
  - `log-sink-closed?`
- Sink types:
  - `make-log-port-sink`: writes formatted text to a caller-owned output port;
    closing flushes but does not close the port.
  - `make-log-rich-console-sink`: writes styled output through `(chezpp rich)`.
  - `make-log-file-sink`: opens a file for append, owns the port, and closes it.
  - `make-log-rotating-file-sink`: rotates when the next line would exceed
    `max-bytes`, keeping up to `backup-count` backups.
  - `make-log-null-sink`: discards messages.
  - `make-log-procedure-sink`: calls a procedure with direct log fields.
  - `make-log-tee-sink`: forwards writes, flushes, and closes to child sinks.
- Sink-level thresholds and filters are applied by direct writes.
- Each sink has its own mutex protecting configuration, writes, flush, close,
  close-state checks, and rotation.
- Sink locking uses Chez's built-in `with-mutex` through a small local
  `with-sink-mutex` macro.
- Closing a sink is idempotent. Direct writes to a closed sink raise an error.

### Loggers

- `make-logger` creates a synchronous logger with level `info`, no sinks, no
  filter, and the `stderr` error policy.
- Configuration and query APIs:
  - `logger?`
  - `logger-name`
  - `logger-level`
  - `logger-level-set!`
  - `logger-sinks`
  - `logger-sinks-set!`
  - `logger-add-sink!`
  - `logger-remove-sink!`
  - `logger-filter`
  - `logger-filter-set!`
  - `logger-error-policy`
  - `logger-error-policy-set!`
  - `logger-enabled?`
- Logging APIs:
  - `logger-log`
  - `logger-log/source`
  - `logger-logf`
  - `logger-logf/source`
  - `logger-dispatch!`
- Resource APIs:
  - `logger-flush!`
  - `logger-close!`
- Dynamic logger APIs:
  - `current-logger`
  - `with-logger`
- Logger filters receive logger name, level, timestamp, thread, source, kind,
  payload, and args.
- Error policies are `raise`, `stderr`, and `ignore`.
- Logger dispatch snapshots logger state under lock, then performs sink I/O
  after releasing the logger lock.
- Logger locking uses Chez's built-in `with-mutex` through a small local
  `with-logger-mutex` macro.
- Closing a logger is idempotent and closes the sinks that were attached at the
  time of the first close.

### Macros

- Constructor macros:
  - `logger`
  - `log-port-sink`
  - `log-rich-console-sink`
  - `log-file-sink`
  - `log-rotating-file-sink`
- Current-logger message macros:
  - `log-trace`
  - `log-debug`
  - `log-info`
  - `log-warn`
  - `log-error`
  - `log-critical`
- Current-logger formatted-message macros:
  - `log-tracef`
  - `log-debugf`
  - `log-infof`
  - `log-warnf`
  - `log-errorf`
  - `log-criticalf`
- Explicit-logger message macros:
  - `logger-trace`
  - `logger-debug`
  - `logger-info`
  - `logger-warn`
  - `logger-error`
  - `logger-critical`
- Explicit-logger formatted-message macros:
  - `logger-tracef`
  - `logger-debugf`
  - `logger-infof`
  - `logger-warnf`
  - `logger-errorf`
  - `logger-criticalf`
- Logging macros check `logger-enabled?` before evaluating the message or format
  arguments, so disabled log calls avoid constructing the message payload.

## Tested Coverage

`tests/logging.ss` currently covers:

- Level predicates, conversion, parsing, ordering, and invalid input errors.
- Pattern formatter tokens, JSON-line escaping, values formatting, and invalid
  formatter inputs.
- Thread id formatting through `%t` using Chez's `get-thread-id`.
- Null, port, procedure, tee, file, rotating-file, and rich-console sinks.
- Sink thresholds, filters, flush, close state, and closed-sink write errors.
- Logger defaults, sink mutation, level filtering, logger filters, formatted
  messages, source metadata, current logger, and `with-logger`.
- Constructor and logging macros, including disabled-call laziness.
- File append and rotation behavior, including zero backups and UTF-8 byte
  length accounting.
- Rich console plain degradation, standard ANSI color output, and custom
  palettes.
- Sink/filter exception handling for `stderr`, `ignore`, and `raise` policies.
- Concurrent writes through a shared sink and concurrent logger sink-list
  mutation while another thread logs.

## Known Deferrals and Gaps

- No asynchronous logger or background worker queue is implemented.
- No public log-record object is exposed.
- Timestamps use Chez `current-time` display output; there is no configurable
  timestamp renderer yet.
- JSON-line formatting escapes quotes, backslashes, newline, return, and tab, but
  does not implement a complete JSON string escaping pass for every control
  character.
- File rotation is size based and local-file oriented; there is no time-based
  rotation, compression, retention policy beyond count, or cross-process
  coordination.
- Procedure, filter, and formatter signatures are direct-field based:
  `logger-name level timestamp thread source kind payload args`.
- Macro constructor syntax uses colon identifiers such as `:name`, `:level`,
  and `:sinks`; it is intentionally not Chez keyword-argument procedure syntax.

## Verification Commands

Focused logging test:

```bash
cd tests && make test-some TEST='logging'
```

Project build:

```bash
make clean && make
```

Parenthesis check for logging-related Scheme files:

```bash
python3 check_parentheses.py \
  chezpp/logging/private/common.ss \
  chezpp/logging/level.ss \
  chezpp/logging/formatter.ss \
  chezpp/logging/sink.ss \
  chezpp/logging/logger.ss \
  chezpp/logging/macros.ss \
  chezpp/logging.ss \
  tests/logging.ss \
  chezpp.ss
```

## Current Assessment

The synchronous v1 logging library is complete enough for normal use through
`(chezpp logging)` or aggregate `(chezpp)`. The most important remaining design
decisions are whether to add async logging, whether to expose structured log
records, and how much timestamp metadata customization should become part of the
public API.
