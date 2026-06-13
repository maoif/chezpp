# Logging Library Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the `(chezpp logging)` library described by `docs/superpowers/specs/2026-06-10-logging-design.md`.

**Architecture:** Implement logging as focused ChezScheme libraries under `chezpp/logging/`, with `(chezpp logging)` as the public facade. The synchronous path passes direct log fields from loggers to sinks without exposing public log records.

**Tech Stack:** ChezScheme libraries, Chezpp `pcheck`, Chez threads/mutexes/condition variables, existing `(chezpp rich)` console APIs, existing `tests/mat.sls` test harness, project build through `make clean && make`.

---

## File Structure

- Create `chezpp/logging/private/common.ss`: shared predicates, line ending helper, display-to-string helper, mutex helpers, and closed-resource diagnostics.
- Create `chezpp/logging/level.ss`: level predicates, ordering, conversion, and comparisons.
- Create `chezpp/logging/formatter.ss`: formatter records, default pattern formatter, rich formatter adapter, JSON-line formatter, and direct field formatting.
- Create `chezpp/logging/sink.ss`: sink record, sink configuration APIs, port/file/rotating/procedure/null/tee/rich sink constructors, sink write/flush/close behavior.
- Create `chezpp/logging/logger.ss`: logger record, configuration APIs, dispatch, filtering, current logger parameter, `with-logger`, error policies, flush/close.
- Create `chezpp/logging/macros.ss`: `logger` constructor macro, sink constructor macros, and level-specific logging macros.
- Create `chezpp/logging.ss`: facade that re-exports the stable public APIs from implementation libraries.
- Modify `chezpp.ss`: import and export `(chezpp logging)` from aggregate `(chezpp)`.
- Modify `tests/Makefile`: add `logging.ss` to `SRCS_TEST`.
- Create `tests/logging.ss`: focused tests for levels, formatters, sinks, loggers, macros, file/rotation, rich console output, error policy, and concurrency.

## Task 1: Level API

**Files:**
- Create: `chezpp/logging/private/common.ss`
- Create: `chezpp/logging/level.ss`
- Create: `chezpp/logging.ss`
- Modify: `chezpp.ss`
- Modify: `tests/Makefile`
- Create: `tests/logging.ss`

- [ ] **Step 1: Write failing level tests**

Add the initial test file:

```scheme
(import (chezpp))

(mat logging-levels

     (and (log-level? 'trace)
          (log-level? 'debug)
          (log-level? 'info)
          (log-level? 'warn)
          (log-level? 'error)
          (log-level? 'critical)
          (log-level? 'off))

     (and (log-message-level? 'trace)
          (not (log-message-level? 'off))
          (not (log-message-level? 'bad)))

     (and (= 0 (log-level->integer 'trace))
          (= 1 (log-level->integer 'debug))
          (= 2 (log-level->integer 'info))
          (= 3 (log-level->integer 'warn))
          (= 4 (log-level->integer 'error))
          (= 5 (log-level->integer 'critical))
          (= 6 (log-level->integer 'off)))

     (and (eq? 'trace (integer->log-level 0))
          (eq? 'debug (integer->log-level 1))
          (eq? 'info (integer->log-level 2))
          (eq? 'warn (integer->log-level 3))
          (eq? 'error (integer->log-level 4))
          (eq? 'critical (integer->log-level 5))
          (eq? 'off (integer->log-level 6)))

     (and (log-level<? 'debug 'info)
          (not (log-level<? 'error 'warn))
          (log-level>=? 'error 'warn)
          (log-level>=? 'info 'info))

     (and (string=? "trace" (log-level->string 'trace))
          (string=? "debug" (log-level->string 'debug))
          (string=? "info" (log-level->string 'info))
          (string=? "warn" (log-level->string 'warn))
          (string=? "error" (log-level->string 'error))
          (string=? "critical" (log-level->string 'critical))
          (string=? "off" (log-level->string 'off)))

     (and (eq? 'trace (string->log-level "trace"))
          (eq? 'debug (string->log-level "DEBUG"))
          (eq? 'info (string->log-level "Info"))
          (eq? 'warn (string->log-level "warning"))
          (eq? 'error (string->log-level "error"))
          (eq? 'critical (string->log-level "CRITICAL"))
          (eq? 'off (string->log-level "off")))

     ;; Error case: invalid levels and level ordinals must be rejected.
     (error? (log-level->integer 'bad))
     (error? (integer->log-level 7))
     (error? (string->log-level "verbose"))
     )
```

Add `logging.ss` to `SRCS_TEST` in `tests/Makefile`. Add `(chezpp logging)` to the export list in `chezpp.ss`.

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
cd tests && make test-some TEST='logging'
```

Expected: FAIL during compile or load because `(chezpp logging)` and level procedures are not defined.

- [ ] **Step 3: Implement minimal level library**

Create `chezpp/logging/private/common.ss`:

```scheme
#!chezscheme
(library (chezpp logging private common)
  (export log-symbol-or-string?
          log-false-or-procedure?
          log-false-or-level?
          log-error-policy?
          log-display->string
          log-message->string
          log-ensure-newline
          log-current-timestamp
          log-current-thread-id)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp logging level))

  (define log-symbol-or-string?
    (lambda (x) (or (symbol? x) (string? x))))

  (define log-false-or-procedure?
    (lambda (x) (or (not x) (procedure? x))))

  (define log-false-or-level?
    (lambda (x) (or (not x) (log-level? x))))

  (define log-error-policy?
    (lambda (x) (memq x '(raise stderr ignore))))

  (define log-display->string
    (lambda (x)
      (call-with-string-output-port
        (lambda (port) (display x port)))))

  (define log-message->string
    (lambda (kind payload args)
      (case kind
        [(message) (log-display->string payload)]
        [(format) (apply format payload args)]
        [(values)
         (let ([port (open-output-string)])
           (display payload port)
           (for-each (lambda (arg)
                       (display #\space port)
                       (display arg port))
                     args)
           (get-output-string port))]
        [else (errorf 'log-message->string "unknown log message kind: ~a" kind)])))

  (define log-ensure-newline
    (lambda (text)
      (let ([n (string-length text)])
        (if (or (= n 0) (not (char=? (string-ref text (- n 1)) #\newline)))
            (string-append text "\n")
            text))))

  (define log-current-timestamp
    (lambda () (current-time)))

  (define log-current-thread-id
    (lambda () #f))

  )
```

Keep `(chezpp logging level)` independent from `(chezpp logging private common)` so the shared helper module can import level predicates without creating an import cycle.

Create `chezpp/logging/level.ss`:

```scheme
#!chezscheme
(library (chezpp logging level)
  (export log-level?
          log-message-level?
          log-level->integer
          integer->log-level
          log-level<?
          log-level>=?
          log-level->string
          string->log-level)
  (import (chezpp chez)
          (chezpp utils))

  (define $levels '#(trace debug info warn error critical off))

  (define $string-downcase
    (lambda (s)
      (list->string (map char-downcase (string->list s)))))

  #|proc:log-level?
  The `log-level?` procedure returns whether `x` is one of the supported log
  level symbols: `trace`, `debug`, `info`, `warn`, `error`, `critical`, or
  `off`.
  |#
  (define log-level?
    (lambda (x)
      (and (memq x '(trace debug info warn error critical off)) #t)))

  #|proc:log-message-level?
  The `log-message-level?` procedure returns whether `x` is a log level that
  may be emitted as a message. The `off` level is excluded because it is only a
  threshold.
  |#
  (define log-message-level?
    (lambda (x)
      (and (memq x '(trace debug info warn error critical)) #t)))

  #|proc:log-level->integer
  The `log-level->integer` procedure returns the ordinal severity for `level`.
  Lower integers are less severe.
  |#
  (define log-level->integer
    (lambda (level)
      (pcheck ([log-level? level])
              (case level
                [(trace) 0]
                [(debug) 1]
                [(info) 2]
                [(warn) 3]
                [(error) 4]
                [(critical) 5]
                [(off) 6]))))

  #|proc:integer->log-level
  The `integer->log-level` procedure returns the log level symbol for ordinal
  `n`.
  |#
  (define integer->log-level
    (lambda (n)
      (pcheck ([(lambda (x) (and (fixnum? x) (fx<= 0 x 6))) n])
              (vector-ref $levels n))))

  #|proc:log-level<?
  The `log-level<?` procedure returns whether level `a` is less severe than
  level `b`.
  |#
  (define log-level<?
    (lambda (a b)
      (pcheck ([log-level? a b])
              (< (log-level->integer a) (log-level->integer b)))))

  #|proc:log-level>=?
  The `log-level>=?` procedure returns whether level `a` is at least as severe
  as level `b`.
  |#
  (define log-level>=?
    (lambda (a b)
      (pcheck ([log-level? a b])
              (>= (log-level->integer a) (log-level->integer b)))))

  #|proc:log-level->string
  The `log-level->string` procedure returns the lower-case name of `level`.
  |#
  (define log-level->string
    (lambda (level)
      (pcheck ([log-level? level])
              (symbol->string level))))

  #|proc:string->log-level
  The `string->log-level` procedure parses `string` as a log level name.
  `warning` is accepted as an alias for `warn`.
  |#
  (define string->log-level
    (lambda (string)
      (pcheck ([string? string])
              (case (string->symbol ($string-downcase string))
                [(trace) 'trace]
                [(debug) 'debug]
                [(info) 'info]
                [(warn warning) 'warn]
                [(error) 'error]
                [(critical) 'critical]
                [(off) 'off]
                [else (errorf 'string->log-level "invalid log level: ~a" string)]))))

  )
```

Create `chezpp/logging.ss`:

```scheme
#!chezscheme
(library (chezpp logging)
  (export (import (chezpp logging level)))
  (import (chezpp chez)
          (chezpp logging level)))
```

- [ ] **Step 4: Run test to verify it passes**

Run:

```bash
cd tests && make test-some TEST='logging'
```

Expected: no stdout and no stderr from the test run, except make's own target progress lines if the local harness prints them.

- [ ] **Step 5: Check parentheses**

Run:

```bash
python3 check_parentheses.py chezpp/logging/private/common.ss chezpp/logging/level.ss chezpp/logging.ss tests/logging.ss
```

Expected: exit 0.

## Task 2: Formatters

**Files:**
- Modify: `chezpp/logging/private/common.ss`
- Create: `chezpp/logging/formatter.ss`
- Modify: `chezpp/logging.ss`
- Modify: `tests/logging.ss`

- [ ] **Step 1: Write failing formatter tests**

Append:

```scheme
(mat logging-formatters

     (log-formatter? (log-default-formatter))

     (let* ([fmt (make-log-pattern-formatter "[%L] %n %m")]
            [text (log-formatter-format fmt 'app 'info 'time #f #f 'message "started" '())])
       (string=? "[info] app started" text))

     (let* ([fmt (make-log-pattern-formatter "%l %T %t %s %% %m")]
            [text (log-formatter-format fmt "api" 'critical "T0" 'th "src" 'format "~a=~a" '(status 500))])
       (string=? "C T0 th src % status=500" text))

     (let* ([fmt (make-log-pattern-formatter "%m")]
            [text (log-formatter-format fmt 'app 'debug #f #f #f 'values 'a '(b 3))])
       (string=? "a b 3" text))

     (let* ([fmt (make-log-json-line-formatter)]
            [text (log-formatter-format fmt 'app 'warn "T" #f #f 'message "quote \" slash \\" '())])
       (and (string? text)
            (let ([n (string-length text)])
              (and (> n 20)
                   (char=? (string-ref text 0) #\{)
                   (char=? (string-ref text (- n 1)) #\})
                   (not (not (string-contains text "\"level\":\"warn\"")))
                   (not (not (string-contains text "\\\"")))))))

     ;; Error case: formatter constructors and formatter calls reject bad inputs.
     (error? (make-log-pattern-formatter 'bad))
     (error? (log-formatter-format 'bad 'app 'info #f #f #f 'message "x" '()))
     )
```

If Chez lacks `string-contains`, add a private helper in the test:

```scheme
(define logging-string-contains?
  (lambda (s needle)
    (let ([n (string-length s)] [m (string-length needle)])
      (let loop ([i 0])
        (cond [(> (+ i m) n) #f]
              [(string=? needle (substring s i (+ i m))) #t]
              [else (loop (+ i 1))])))))
```

and use `logging-string-contains?`.

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
cd tests && make test-some TEST='logging'
```

Expected: FAIL because formatter APIs are undefined.

- [ ] **Step 3: Implement formatter library**

Implement `chezpp/logging/formatter.ss` with:

```scheme
#!chezscheme
(library (chezpp logging formatter)
  (export log-formatter?
          make-log-pattern-formatter
          make-log-rich-formatter
          make-log-json-line-formatter
          log-default-formatter
          log-formatter-format)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp logging level)
          (chezpp logging private common))

  (define-record-type $log-formatter
    (fields (immutable kind $log-formatter-kind)
            (immutable data $log-formatter-data)
            (immutable proc $log-formatter-proc)))

  #|proc:log-formatter?
  The `log-formatter?` procedure returns whether `x` is a log formatter.
  |#
  (define log-formatter? $log-formatter?)

  (define $short-level
    (lambda (level)
      (case level
        [(trace) "T"]
        [(debug) "D"]
        [(info) "I"]
        [(warn) "W"]
        [(error) "E"]
        [(critical) "C"]
        [(off) "O"])))

  (define $token-string
    (lambda (token logger-name level timestamp thread source message)
      (case token
        [(#\T) (if timestamp (log-display->string timestamp) "")]
        [(#\L) (log-level->string level)]
        [(#\l) ($short-level level)]
        [(#\n) (log-display->string logger-name)]
        [(#\t) (if thread (log-display->string thread) "")]
        [(#\s) (if source (log-display->string source) "")]
        [(#\m) message]
        [(#\%) "%"]
        [else (string #\% token)])))

  (define $pattern-format
    (lambda (pattern logger-name level timestamp thread source kind payload args)
      (let ([message (log-message->string kind payload args)]
            [out (open-output-string)]
            [n (string-length pattern)])
        (let loop ([i 0])
          (cond [(= i n) (get-output-string out)]
                [(and (char=? (string-ref pattern i) #\%) (< (+ i 1) n))
                 (display ($token-string (string-ref pattern (+ i 1))
                                         logger-name level timestamp thread source message)
                          out)
                 (loop (+ i 2))]
                [else
                 (write-char (string-ref pattern i) out)
                 (loop (+ i 1))])))))

  (define $json-escape
    (lambda (s)
      (let ([out (open-output-string)])
        (let loop ([i 0])
          (if (= i (string-length s))
              (get-output-string out)
              (let ([ch (string-ref s i)])
                (case ch
                  [(#\") (display "\\\"" out)]
                  [(#\\) (display "\\\\" out)]
                  [(#\newline) (display "\\n" out)]
                  [(#\return) (display "\\r" out)]
                  [(#\tab) (display "\\t" out)]
                  [else (write-char ch out)])
                (loop (+ i 1))))))))

  (define $json-field
    (lambda (out name value first?)
      (unless first? (display "," out))
      (fprintf out "\"~a\":\"~a\"" name ($json-escape (log-display->string value)))))

  (define $json-line-format
    (lambda (logger-name level timestamp thread source kind payload args)
      (let ([out (open-output-string)]
            [message (log-message->string kind payload args)])
        (display "{" out)
        ($json-field out "logger" logger-name #t)
        ($json-field out "level" (log-level->string level) #f)
        ($json-field out "timestamp" (if timestamp timestamp "") #f)
        ($json-field out "thread" (if thread thread "") #f)
        ($json-field out "source" (if source source "") #f)
        ($json-field out "message" message #f)
        (display "}" out)
        (get-output-string out))))

  #|proc:make-log-pattern-formatter
  The `make-log-pattern-formatter` procedure creates a formatter using
  `pattern`. Supported tokens are `%T`, `%L`, `%l`, `%n`, `%t`, `%s`, `%m`, and
  `%%`.
  |#
  (define make-log-pattern-formatter
    (lambda (pattern)
      (pcheck ([string? pattern])
              (make-$log-formatter 'pattern pattern
                                   (lambda (logger-name level timestamp thread source kind payload args)
                                     ($pattern-format pattern logger-name level timestamp thread source kind payload args))))))

  #|proc:make-log-rich-formatter
  The `make-log-rich-formatter` procedure creates the default rich formatter.
  It currently produces the same textual content as the default plain formatter;
  the rich console sink applies level styling when rendering it.
  |#
  (define make-log-rich-formatter
    (lambda ()
      (make-$log-formatter 'rich #f
                           (lambda (logger-name level timestamp thread source kind payload args)
                             ($pattern-format "[%L] %n %m" logger-name level timestamp thread source kind payload args)))))

  #|proc:make-log-json-line-formatter
  The `make-log-json-line-formatter` procedure creates a formatter that returns
  one JSON object string for each log message.
  |#
  (define make-log-json-line-formatter
    (lambda ()
      (make-$log-formatter 'json-line #f $json-line-format)))

  (define $default-formatter (make-log-pattern-formatter "[%L] %n %m"))

  #|proc:log-default-formatter
  The `log-default-formatter` procedure returns the default plain-text
  formatter.
  |#
  (define log-default-formatter
    (lambda () $default-formatter))

  #|proc:log-formatter-format
  The `log-formatter-format` procedure formats direct log data using
  `formatter`. The `logger-name`, `level`, `timestamp`, `thread`, `source`,
  `kind`, `payload`, and `args` parameters are the direct fields supplied by a
  logger.
  |#
  (define log-formatter-format
    (lambda (formatter logger-name level timestamp thread source kind payload args)
      (pcheck ([log-formatter? formatter]
               [log-message-level? level]
               [list? args])
              (($log-formatter-proc formatter) logger-name level timestamp thread source kind payload args))))

  )
```

Update `chezpp/logging.ss` to re-export `(chezpp logging formatter)`.

- [ ] **Step 4: Run test to verify it passes**

Run:

```bash
cd tests && make test-some TEST='logging'
```

Expected: logging tests pass with no error output.

- [ ] **Step 5: Check parentheses**

Run:

```bash
python3 check_parentheses.py chezpp/logging/formatter.ss tests/logging.ss
```

Expected: exit 0.

## Task 3: Synchronous Sinks

**Files:**
- Modify: `chezpp/logging/private/common.ss`
- Create: `chezpp/logging/sink.ss`
- Modify: `chezpp/logging.ss`
- Modify: `tests/logging.ss`

- [ ] **Step 1: Write failing sink tests**

Append tests covering:

```scheme
(define logging-capture
  (lambda (proc)
    (let ([port (open-output-string)])
      (proc port)
      (get-output-string port))))

(mat logging-sinks

     (let ([sink (make-log-null-sink 'null)])
       (and (log-sink? sink)
            (eq? 'null (log-sink-name sink))
            (not (log-sink-level sink))
            (not (log-sink-closed? sink))))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)])
       (log-sink-write! sink 'app 'info "T" #f #f 'message "hello" '())
       (log-sink-flush! sink)
       (string=? "[info] app hello\n" (get-output-string out)))

     (let ([seen '()])
       (let ([sink (make-log-procedure-sink
                    'proc
                    (lambda (logger-name level timestamp thread source kind payload args)
                      (set! seen (list logger-name level timestamp thread source kind payload args))))])
         (log-sink-write! sink 'app 'warn 'ts 'th 'src 'format "~a" '(x))
         (equal? seen '(app warn ts th src format "~a" (x)))))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)])
       (log-sink-level-set! sink 'error)
       (log-sink-write! sink 'app 'warn #f #f #f 'message "drop" '())
       (log-sink-write! sink 'app 'error #f #f #f 'message "keep" '())
       (string=? "[error] app keep\n" (get-output-string out)))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)])
       (log-sink-filter-set! sink
                             (lambda (logger-name level timestamp thread source kind payload args)
                               (eq? level 'critical)))
       (log-sink-write! sink 'app 'error #f #f #f 'message "drop" '())
       (log-sink-write! sink 'app 'critical #f #f #f 'message "keep" '())
       (string=? "[critical] app keep\n" (get-output-string out)))

     (let* ([out1 (open-output-string)]
            [out2 (open-output-string)]
            [sink1 (make-log-port-sink 'one out1)]
            [sink2 (make-log-port-sink 'two out2)]
            [tee (make-log-tee-sink 'tee (list sink1 sink2))])
       (log-sink-write! tee 'app 'info #f #f #f 'message "both" '())
       (and (string=? "[info] app both\n" (get-output-string out1))
            (string=? "[info] app both\n" (get-output-string out2))))

     ;; Error case: closed sinks reject writes.
     (let ([sink (make-log-null-sink 'closed)])
       (log-sink-close! sink)
       (and (log-sink-closed? sink)
            (error? (log-sink-write! sink 'app 'info #f #f #f 'message "x" '()))))
     )
```

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
cd tests && make test-some TEST='logging'
```

Expected: FAIL because sink APIs are undefined.

- [ ] **Step 3: Implement sink library**

Implement `chezpp/logging/sink.ss` with:

- `$log-sink` record fields: name, level, filter, formatter, writer, flusher, closer, close-owned?, closed?, lock.
- Public accessor and setter APIs documented with `#|proc:name ... |#`.
- `make-log-port-sink`: writes `(log-ensure-newline (log-formatter-format ...))` to a caller-owned textual output port and flushes.
- `make-log-file-sink`: opens with `(open-file-output-port path (file-options no-fail no-create append) (buffer-mode block) (current-transcoder))`, owns the port, flushes/closes it.
- `make-log-rotating-file-sink`: before writing, checks current file size plus line size and rotates `path.N` backups under the sink lock. Keep this deterministic and simple: delete oldest if present, rename descending backups, rename active file to `.1`, open a fresh append port.
- `make-log-null-sink`: discards writes.
- `make-log-procedure-sink`: calls the supplied procedure with direct fields.
- `make-log-tee-sink`: forwards writes/flush/close to child sinks.
- `make-log-rich-console-sink`: uses `(rich-fprintln console renderable)` or `(rich-fprint console text)` from `(chezpp rich)` with rich style APIs, falling back to text through the rich console policy. Do not emit handwritten ANSI escapes.
- `log-sink-write!`: pcheck the public fields, skip if closed, skip below sink level, apply filter, lock while calling the sink writer, unlock even on exceptions using `dynamic-wind`.
- `log-sink-close!`: idempotently mark closed, flush, then run closer for owned resources.

Use direct log fields only; do not introduce a public event record.

- [ ] **Step 4: Run test to verify it passes**

Run:

```bash
cd tests && make test-some TEST='logging'
```

Expected: logging tests pass.

- [ ] **Step 5: Check parentheses**

Run:

```bash
python3 check_parentheses.py chezpp/logging/sink.ss tests/logging.ss
```

Expected: exit 0.

## Task 4: Logger Dispatch and Current Logger

**Files:**
- Create: `chezpp/logging/logger.ss`
- Modify: `chezpp/logging.ss`
- Modify: `tests/logging.ss`

- [ ] **Step 1: Write failing logger tests**

Append tests covering:

```scheme
(mat logging-loggers

     (let ([log (make-logger 'app)])
       (and (logger? log)
            (eq? 'app (logger-name log))
            (eq? 'info (logger-level log))
            (null? (logger-sinks log))
            (eq? 'stderr (logger-error-policy log))))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)]
            [log (make-logger 'app)])
       (logger-add-sink! log sink)
       (logger-log log 'info "hello")
       (string=? "[info] app hello\n" (get-output-string out)))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)]
            [log (make-logger 'app)])
       (logger-add-sink! log sink)
       (logger-level-set! log 'error)
       (logger-log log 'warn "drop")
       (logger-log log 'error "keep")
       (string=? "[error] app keep\n" (get-output-string out)))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)]
            [log (make-logger 'app)])
       (logger-add-sink! log sink)
       (logger-filter-set! log
                           (lambda (logger-name level timestamp thread source kind payload args)
                             (eq? level 'critical)))
       (logger-log log 'error "drop")
       (logger-log log 'critical "keep")
       (string=? "[critical] app keep\n" (get-output-string out)))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)]
            [log (make-logger 'app)])
       (logger-add-sink! log sink)
       (logger-logf log 'info "~a=~a" 'status 200)
       (string=? "[info] app status=200\n" (get-output-string out)))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)]
            [log (make-logger 'app)])
       (logger-add-sink! log sink)
       (logger-log/source log 'info 'src "hello")
       (logger-logf/source log 'warn 'src "~a" "warn")
       (string=? "[info] app hello\n[warn] app warn\n" (get-output-string out)))

     (let ([old (current-logger)]
           [log (make-logger 'scoped)])
       (with-logger log
         (and (eq? log (current-logger))
              (begin (current-logger old) (eq? old (current-logger))))))

     ;; Error case: logger setters reject invalid arguments.
     (let ([log (make-logger 'app)])
       (and (error? (logger-level-set! log 'bad))
            (error? (logger-sinks-set! log (list 'bad)))
            (error? (logger-error-policy-set! log 'bad))))
     )
```

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
cd tests && make test-some TEST='logging'
```

Expected: FAIL because logger APIs are undefined.

- [ ] **Step 3: Implement logger library**

Implement `chezpp/logging/logger.ss` with:

- `$logger` record fields: name, level, sinks, filter, error-policy, closed?, lock.
- `make-logger` creates a synchronous logger with `level = 'info`, no sinks, no filter, `error-policy = 'stderr`, and a mutex.
- All public accessors/setters documented and protected by `pcheck`.
- `logger-sinks` returns a fresh list copy.
- `logger-sinks-set!` validates every item with `log-sink?`.
- `logger-add-sink!` appends a sink.
- `logger-remove-sink!` removes sinks by `eq?`.
- `logger-enabled?` returns true only for message levels at or above logger level and when logger level is not `off`.
- Dispatch computes timestamp and thread once, checks logger filter, snapshots sinks while holding the logger lock, then releases the lock before `log-sink-write!`.
- Implement `logger-log`, `logger-log/source`, `logger-logf`, and `logger-logf/source` with direct fields and rest args. `logger-logf` may allocate the natural rest list; formatting remains sink-side.
- Error policy handling wraps sink dispatch. `'raise` re-raises, `'stderr` writes a short diagnostic to `(current-error-port)`, `'ignore` suppresses.
- `logger-flush!` flushes all sinks.
- `logger-close!` is idempotent: flushes and closes each sink once.
- `current-logger` is a thread parameter initialized to a default logger.
- `with-logger` parameterizes `current-logger`.

- [ ] **Step 4: Run test to verify it passes**

Run:

```bash
cd tests && make test-some TEST='logging'
```

Expected: logging tests pass.

- [ ] **Step 5: Check parentheses**

Run:

```bash
python3 check_parentheses.py chezpp/logging/logger.ss tests/logging.ss
```

Expected: exit 0.

## Task 5: Constructor and Logging Macros

**Files:**
- Create: `chezpp/logging/macros.ss`
- Modify: `chezpp/logging.ss`
- Modify: `tests/logging.ss`

- [ ] **Step 1: Write failing macro tests**

Append tests covering:

```scheme
(mat logging-macros

     (let* ([out (open-output-string)]
            [sink (log-port-sink :name 'port :port out :level 'debug)]
            [log (logger :name 'app :level 'debug :sinks sink)])
       (with-logger log
         (log-debug "debug")
         (log-infof "~a" "info"))
       (string=? "[debug] app debug\n[info] app info\n" (get-output-string out)))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)]
            [log (make-logger 'app)])
       (logger-add-sink! log sink)
       (logger-level-set! log 'error)
       (logger-warn log (error 'disabled "message expression evaluated"))
       (logger-error log "enabled")
       (string=? "[error] app enabled\n" (get-output-string out)))

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'port out)]
            [log (make-logger 'app)])
       (logger-add-sink! log sink)
       (logger-level-set! log 'critical)
       (logger-errorf log "~a" (error 'disabled "format argument evaluated"))
       (logger-criticalf log "~a" "enabled")
       (string=? "[critical] app enabled\n" (get-output-string out)))

     ;; Error case: constructor macros reject unknown fields at expansion time.
     (error? (eval '(logger :bogus 1)))
     )
```

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
cd tests && make test-some TEST='logging'
```

Expected: FAIL because macros are undefined.

- [ ] **Step 3: Implement macro library**

Implement `chezpp/logging/macros.ss` with:

- `logger` macro parsing `:name`, `:level`, `:sinks`, `:filter`, and `:error-policy`. `:sinks` accepts one or more expressions until the next keyword symbol.
- `log-port-sink`, `log-rich-console-sink`, `log-file-sink`, and `log-rotating-file-sink` macros. They construct the sink and apply configured `:level`, `:filter`, and `:formatter` using setters.
- Current logger macros: `log-trace`, `log-debug`, `log-info`, `log-warn`, `log-error`, `log-critical`, and formatted variants.
- Explicit logger macros: `logger-trace`, `logger-debug`, `logger-info`, `logger-warn`, `logger-error`, `logger-critical`, and formatted variants.
- Every logging macro must expand to code shaped like:

```scheme
(let ([tmp logger-expr])
  (when (logger-enabled? tmp 'level)
    (logger-log tmp 'level message-expr)))
```

For formatted macros, put `format-string` and `arg ...` inside the `when` body so disabled calls do not evaluate them.

- [ ] **Step 4: Run test to verify it passes**

Run:

```bash
cd tests && make test-some TEST='logging'
```

Expected: logging tests pass.

- [ ] **Step 5: Check parentheses**

Run:

```bash
python3 check_parentheses.py chezpp/logging/macros.ss tests/logging.ss
```

Expected: exit 0.

## Task 6: File, Rotation, Rich, Error Policy, and Close Semantics

**Files:**
- Modify: `chezpp/logging/sink.ss`
- Modify: `chezpp/logging/logger.ss`
- Modify: `tests/logging.ss`

- [ ] **Step 1: Write failing integration tests**

Append tests covering:

```scheme
(define logging-read-file
  (lambda (path)
    (call-with-input-file path
      (lambda (port)
        (let loop ([chars '()])
          (let ([ch (read-char port)])
            (if (eof-object? ch)
                (list->string (reverse chars))
                (loop (cons ch chars)))))))))

(define logging-delete-if-exists
  (lambda (path)
    (when (file-exists? path) (delete-file path))))

(mat logging-files-and-errors

     (let ([path "logging-file-test.out"])
       (logging-delete-if-exists path)
       (let ([sink (make-log-file-sink 'file path)])
         (log-sink-write! sink 'app 'info #f #f #f 'message "file" '())
         (log-sink-close! sink)
         (let ([text (logging-read-file path)])
           (logging-delete-if-exists path)
           (string=? "[info] app file\n" text))))

     (let ([path "logging-rotate-test.out"])
       (logging-delete-if-exists path)
       (logging-delete-if-exists "logging-rotate-test.out.1")
       (logging-delete-if-exists "logging-rotate-test.out.2")
       (let ([sink (make-log-rotating-file-sink 'rot path 35 2)])
         (log-sink-write! sink 'app 'info #f #f #f 'message "first-long-line" '())
         (log-sink-write! sink 'app 'info #f #f #f 'message "second-long-line" '())
         (log-sink-close! sink)
         (let ([ok (and (file-exists? path)
                        (file-exists? "logging-rotate-test.out.1"))])
           (logging-delete-if-exists path)
           (logging-delete-if-exists "logging-rotate-test.out.1")
           (logging-delete-if-exists "logging-rotate-test.out.2")
           ok)))

     (let* ([out (open-output-string)]
            [console (rich-console :output-port out :force-terminal? #f :color-system 'none)]
            [sink (make-log-rich-console-sink 'rich console)])
       (log-sink-write! sink 'app 'warn #f #f #f 'message "plain" '())
       (string=? "[warn] app plain\n" (get-output-string out)))

     (let* ([err (open-output-string)]
            [bad-sink (make-log-procedure-sink
                       'bad
                       (lambda (logger-name level timestamp thread source kind payload args)
                         (error 'sink "boom")))]
            [log (make-logger 'app)])
       (logger-add-sink! log bad-sink)
       (parameterize ([current-error-port err])
         (logger-log log 'info "x")
         (not (not (logging-string-contains? (get-output-string err) "logging sink error")))))

     (let* ([bad-sink (make-log-procedure-sink
                       'bad
                       (lambda (logger-name level timestamp thread source kind payload args)
                         (error 'sink "boom")))]
            [log (make-logger 'app)])
       (logger-add-sink! log bad-sink)
       (logger-error-policy-set! log 'ignore)
       (logger-log log 'info "x")
       #t)

     ;; Error case: raise policy propagates sink exceptions.
     (let* ([bad-sink (make-log-procedure-sink
                       'bad
                       (lambda (logger-name level timestamp thread source kind payload args)
                         (error 'sink "boom")))]
            [log (make-logger 'app)])
       (logger-add-sink! log bad-sink)
       (logger-error-policy-set! log 'raise)
       (error? (logger-log log 'info "x")))
     )
```

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
cd tests && make test-some TEST='logging'
```

Expected: FAIL for the unimplemented pieces or incorrect close/rotation/error behavior.

- [ ] **Step 3: Complete sink/logger behavior**

Fill missing behavior in `sink.ss` and `logger.ss` so all tests pass. Pay attention to:

- Opened file ports must be closed after usage.
- Existing caller-owned port sinks flush but do not close the port.
- `log-sink-close!` and `logger-close!` must be idempotent.
- Formatter/filter/procedure sink exceptions are handled by logger error policy.
- Sink-level direct writes may raise on closed sinks; logger dispatch catches according to policy.

- [ ] **Step 4: Run test to verify it passes**

Run:

```bash
cd tests && make test-some TEST='logging'
```

Expected: logging tests pass.

- [ ] **Step 5: Check parentheses**

Run:

```bash
python3 check_parentheses.py chezpp/logging/sink.ss chezpp/logging/logger.ss tests/logging.ss
```

Expected: exit 0.

## Task 7: Concurrency and Build Integration

**Files:**
- Modify: `tests/logging.ss`
- Modify: `chezpp.ss`
- Modify: any logging module with fixes found by integration tests

- [ ] **Step 1: Write failing concurrency and aggregate tests**

Append tests covering:

```scheme
(mat logging-concurrency

     (let* ([out (open-output-string)]
            [sink (make-log-port-sink 'shared out)]
            [log1 (make-logger 'one)]
            [log2 (make-logger 'two)])
       (logger-add-sink! log1 sink)
       (logger-add-sink! log2 sink)
       (let ([threads
              (map (lambda (i)
                     (fork-thread
                      (lambda ()
                        (do ([j 0 (+ j 1)])
                            ((= j 20))
                          (if (even? i)
                              (logger-logf log1 'info "~a-~a" i j)
                              (logger-logf log2 'info "~a-~a" i j))))))
                   (iota 8))])
         (for-each thread-join threads)
         (let ([text (get-output-string out)])
           (and (not (not (logging-string-contains? text "[info] one 0-0\n")))
                (not (not (logging-string-contains? text "[info] two 1-0\n")))))))

     (let* ([out (open-output-string)]
            [sink1 (make-log-port-sink 'one out)]
            [sink2 (make-log-null-sink 'two)]
            [log (make-logger 'app)])
       (logger-add-sink! log sink1)
       (let ([writer (fork-thread
                      (lambda ()
                        (do ([i 0 (+ i 1)])
                            ((= i 50))
                          (logger-logf log 'info "~a" i))))]
             [mutator (fork-thread
                       (lambda ()
                         (do ([i 0 (+ i 1)])
                             ((= i 50))
                           (logger-add-sink! log sink2)
                           (logger-remove-sink! log sink2))))])
         (thread-join writer)
         (thread-join mutator)
         #t))
     )
```

- [ ] **Step 2: Run test to verify it fails or exposes races**

Run:

```bash
cd tests && make test-some TEST='logging'
```

Expected: FAIL if locking/snapshot behavior is incomplete, otherwise PASS because earlier implementation already satisfies it. If it passes immediately, keep the test as regression coverage.

- [ ] **Step 3: Fix concurrency and aggregate integration**

Ensure:

- Logger lock protects level, filter, sinks, error policy, and close state.
- Sink lock protects write, flush, close, and rotation.
- Logger dispatch snapshots sinks under lock and performs I/O after releasing the logger lock.
- `chezpp.ss` imports and exports `(chezpp logging)` so `(import (chezpp))` exposes the new library.
- `chezpp/logging.ss` facade exports every stable API from level, formatter, sink, logger, and macros.

- [ ] **Step 4: Run focused tests**

Run:

```bash
cd tests && make test-some TEST='logging'
```

Expected: logging tests pass.

- [ ] **Step 5: Run full project build**

From project root:

```bash
make clean && make
```

Expected: build exits 0.

- [ ] **Step 6: Run logging test through project harness**

Run:

```bash
cd tests && make test-some TEST='logging'
```

Expected: no stdout/stderr test errors.

- [ ] **Step 7: Final parenthesis check**

Run:

```bash
python3 check_parentheses.py chezpp/logging/private/common.ss chezpp/logging/level.ss chezpp/logging/formatter.ss chezpp/logging/sink.ss chezpp/logging/logger.ss chezpp/logging/macros.ss chezpp/logging.ss tests/logging.ss chezpp.ss
```

Expected: exit 0.

## Self-Review

- Spec coverage: plan covers public facade, level APIs, formatters, sinks, logger dispatch, current logger, macros, file/rotating/rich/procedure/null/tee sinks, error policies, close/flush, concurrency, and build integration.
- Scoped deferral: v1 tests for rich console verify plain degradation through `(chezpp rich)`; exact color style assertions can be added later after confirming the existing rich test helpers for ANSI-forced output.
- Placeholder scan: no task contains deferred-work markers, unspecified implementation steps, or missing test commands.
- Type consistency: direct sink fields consistently use `logger-name level timestamp thread source kind payload args`.
