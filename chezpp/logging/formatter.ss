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
