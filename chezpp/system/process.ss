(library (chezpp system process)
  (export process-exit-status? process-exit-status-kind process-exit-status-code process-exit-success?
          process-result? process-result-status process-result-stdout process-result-stderr process-result-pid process-result-command
          run-process run-process/check capture-process capture-process/check shell-command capture-shell-command)
  (import (chezpp chez)
          (chezpp system common)
          (chezpp utils))

;;;;===----------------------------------------------------------------------===
;;;; process result records
;;;;===----------------------------------------------------------------------===

  #|proc:process-exit-status?
The `process-exit-status?` procedure returns `#t` when its argument is a process exit-status record, otherwise `#f`.
The `object` parameter is the object to test.
|#
  #|proc:process-exit-status-kind
The `process-exit-status-kind` procedure returns the status kind stored in `status`.
The `status` parameter is a process exit-status record.
|#
  #|proc:process-exit-status-code
The `process-exit-status-code` procedure returns the numeric exit code stored in `status`.
The `status` parameter is a process exit-status record.
|#
  (define-record-type ($process-exit-status make-process-exit-status process-exit-status?)
    (nongenerative)
    (fields (immutable kind process-exit-status-kind)
            (immutable code process-exit-status-code)))

  #|proc:process-result?
The `process-result?` procedure returns `#t` when its argument is a process result record, otherwise `#f`.
The `object` parameter is the object to test.
|#
  #|proc:process-result-status
The `process-result-status` procedure returns the process exit-status record stored in `result`.
The `result` parameter is a process result record.
|#
  #|proc:process-result-stdout
The `process-result-stdout` procedure returns the captured standard output string stored in `result`, or `#f` when output was not requested.
The `result` parameter is a process result record.
|#
  #|proc:process-result-stderr
The `process-result-stderr` procedure returns the captured standard error string stored in `result`, or `#f` when error output was not requested.
The `result` parameter is a process result record.
|#
  #|proc:process-result-pid
The `process-result-pid` procedure returns the process id stored in `result`, or `#f` when no process id is available.
The `result` parameter is a process result record.
|#
  #|proc:process-result-command
The `process-result-command` procedure returns the shell command string stored in `result`.
The `result` parameter is a process result record.
|#
  (define-record-type ($process-result make-process-result process-result?)
    (nongenerative)
    (fields (immutable status process-result-status)
            (immutable stdout process-result-stdout)
            (immutable stderr process-result-stderr)
            (immutable pid process-result-pid)
            (immutable command process-result-command)))

  #|proc:process-exit-success?
The `process-exit-success?` procedure returns `#t` when `status` represents a successful process exit, otherwise `#f`.
The `status` parameter is a process exit-status record.
|#
  (define process-exit-success?
    (lambda (status)
      (pcheck ([process-exit-status? status])
              (and (eq? 'exit (process-exit-status-kind status))
                   (= 0 (process-exit-status-code status))))))

;;;;===----------------------------------------------------------------------===
;;;; temporary shell bootstrap
;;;;===----------------------------------------------------------------------===

  (define $string-list?
    (lambda (x)
      (and (list? x) (andmap string? x))))

  (define $option-alist?
    (lambda (x)
      (list? x)))

  (define $option-ref
    (lambda (options key default)
      (let ([a (assq key options)])
        (if a (cdr a) default))))

  (define $close-port/quiet
    (lambda (p)
      (guard (c [else #f])
        (close-port p))))

  (define $slurp-text-port
    (lambda (ip)
      (call-with-string-output-port
       (lambda (op)
         (let loop ()
           (let ([c (read-char ip)])
             (unless (eof-object? c)
               (write-char c op)
               (loop))))))))

  (define $shell-quote
    (lambda (s)
      (call-with-string-output-port
       (lambda (op)
         (write-char #\' op)
         (let ([n (string-length s)])
           (let loop ([i 0])
             (when (< i n)
               (let ([c (string-ref s i)])
                 (if (char=? c #\')
                     (display "'\\''" op)
                     (write-char c op)))
               (loop (+ i 1)))))
         (write-char #\' op)))))

  (define $join-shell-words
    (lambda (words)
      (call-with-string-output-port
       (lambda (op)
         (unless (null? words)
           (display (car words) op)
           (for-each (lambda (word)
                       (write-char #\space op)
                       (display word op))
                     (cdr words)))))))

  (define $argv->shell-command
    (lambda (program arguments)
      ($join-shell-words (map $shell-quote (cons program arguments)))))

  (define $capture-status-marker "\n__CHEZPP_PROCESS_STATUS__:")

  (define $string-last-index
    (lambda (s needle)
      (let ([slen (string-length s)]
            [nlen (string-length needle)])
        (let loop ([i 0] [last #f])
          (if (> (+ i nlen) slen)
              last
              (let ([match?
                     (let check ([j 0])
                       (cond [(= j nlen) #t]
                             [(char=? (string-ref s (+ i j)) (string-ref needle j))
                              (check (+ j 1))]
                             [else #f]))])
                (loop (+ i 1) (if match? i last))))))))

  (define $string-index-from
    (lambda (s ch start)
      (let ([n (string-length s)])
        (let loop ([i start])
          (cond [(= i n) #f]
                [(char=? (string-ref s i) ch) i]
                [else (loop (+ i 1))])))))

  (define $capture-command-wrapper
    (lambda (command)
      (string-append "( " command " ); __chezpp_status=$?; printf '\\n__CHEZPP_PROCESS_STATUS__:%s\\n' \"$__chezpp_status\" >&2; exit \"$__chezpp_status\"")))

  (define $split-stderr-status
    (lambda (stderr)
      (let ([pos ($string-last-index stderr $capture-status-marker)])
        (if pos
            (let* ([code-start (+ pos (string-length $capture-status-marker))]
                   [code-end (or ($string-index-from stderr #\newline code-start)
                                 (string-length stderr))]
                   [code (string->number (substring stderr code-start code-end))])
              (values (substring stderr 0 pos)
                      (make-process-exit-status 'exit (if (integer? code) code 1))))
            (values stderr (make-process-exit-status 'exit 1))))))

  (define $raise-exit-error
    (lambda (who command status)
      (raise (make-system-exit-error who
                                     (format "process exited with status ~a" (process-exit-status-code status))
                                     `((command . ,command)
                                       (status-kind . ,(process-exit-status-kind status))
                                       (status-code . ,(process-exit-status-code status)))))))

  (define $check-status
    (lambda (who command status)
      (unless (process-exit-success? status)
        ($raise-exit-error who command status))))

  (define %run-shell-command
    (lambda (who command options check?)
      (pcheck ([string? command] [$option-alist? options] [boolean? check?])
              (let* ([code (system command)]
                     [status (make-process-exit-status 'exit code)])
                (when check?
                  ($check-status who command status))
                status))))

  (define %capture-shell-command
    (lambda (who command options check?)
      (pcheck ([string? command] [$option-alist? options] [boolean? check?])
              (let ([stdout-mode ($option-ref options 'stdout 'capture)]
                    [stderr-mode ($option-ref options 'stderr 'capture)])
                (let-values ([(to-stdin from-stdout from-stderr pid)
                              (open-process-ports ($capture-command-wrapper command)
                                                  (buffer-mode block)
                                                  (native-transcoder))])
                  (guard (c [else
                             ($close-port/quiet to-stdin)
                             ($close-port/quiet from-stdout)
                             ($close-port/quiet from-stderr)
                             (raise c)])
                    ($close-port/quiet to-stdin)
                    (let* ([stdout ($slurp-text-port from-stdout)]
                           [stderr/status ($slurp-text-port from-stderr)])
                      ($close-port/quiet from-stdout)
                      ($close-port/quiet from-stderr)
                      (let-values ([(stderr status) ($split-stderr-status stderr/status)])
                        (when check?
                          ($check-status who command status))
                        (make-process-result status
                                             (if (eq? stdout-mode 'capture) stdout #f)
                                             (if (eq? stderr-mode 'capture) stderr #f)
                                             pid
                                             command)))))))))

  (define %run-process
    (lambda (who program arguments options check?)
      (pcheck ([string? program] [$string-list? arguments] [$option-alist? options] [boolean? check?])
              (%run-shell-command who ($argv->shell-command program arguments) options check?))))

  (define %capture-process
    (lambda (who program arguments options check?)
      (pcheck ([string? program] [$string-list? arguments] [$option-alist? options] [boolean? check?])
              (%capture-shell-command who ($argv->shell-command program arguments) options check?))))

;;;;===----------------------------------------------------------------------===
;;;; process macros
;;;;===----------------------------------------------------------------------===

  (define-syntax $process-form
    (let ()
      (define known-option-keywords
        '(:cwd :env :env-mode :stdin :stdout :stderr :encoding :timeout :success))
      (define literal-option-values
        '(capture inherit null stdout replace))
      (define colon-symbol?
        (lambda (x)
          (and (symbol? x)
               (let ([s (symbol->string x)])
                 (and (< 0 (string-length s))
                      (char=? #\: (string-ref s 0)))))))
      (define known-option-keyword?
        (lambda (x)
          (and (symbol? x) (memq x known-option-keywords))))
      (define option-key
        (lambda (x)
          (let* ([s (symbol->string x)]
                 [n (string-length s)])
            (string->symbol (substring s 1 n)))))
      (define parse-value
        (lambda (stx)
          (let ([datum (syntax->datum stx)])
            (if (and (symbol? datum) (memq datum literal-option-values))
                #`'#,(datum->syntax stx datum)
                stx))))
      (define parse-forms
        (lambda (stx forms)
          (let parse-args ([rest forms] [args '()])
            (if (null? rest)
                (if (null? args)
                    (syntax-error stx "missing process command")
                    (values (reverse args) '()))
                (let ([datum (syntax->datum (car rest))])
                  (cond [(known-option-keyword? datum)
                         (values (reverse args)
                                 (parse-options stx rest '() '()))]
                        [(colon-symbol? datum)
                         (syntax-error (car rest) "unknown process option keyword")]
                        [else
                         (parse-args (cdr rest) (cons (car rest) args))]))))))
      (define parse-options
        (lambda (stx rest seen out)
          (if (null? rest)
              (reverse out)
              (let ([kw-stx (car rest)])
                (let ([kw (syntax->datum kw-stx)])
                  (cond [(not (known-option-keyword? kw))
                         (if (colon-symbol? kw)
                             (syntax-error kw-stx "unknown process option keyword")
                             (syntax-error kw-stx "expected process option keyword"))]
                        [(memq kw seen)
                         (syntax-error kw-stx "duplicate process option keyword")]
                        [(null? (cdr rest))
                         (syntax-error kw-stx "missing process option value")]
                        [else
                         (parse-options stx
                                        (cddr rest)
                                        (cons kw seen)
                                        (cons #`(cons '#,(datum->syntax kw-stx (option-key kw))
                                                      #,(parse-value (cadr rest)))
                                              out))]))))))
      (lambda (stx)
        (syntax-case stx ()
          [(_ kind check? form ...)
           (let ([kind (syntax->datum #'kind)]
                 [check? (syntax->datum #'check?)]
                 [forms (syntax->list #'(form ...))])
             (unless forms
               (syntax-error stx "invalid process form"))
             (let-values ([(command+args option-forms) (parse-forms stx forms)])
               (when (null? command+args)
                 (syntax-error stx "missing process command"))
               (let ([command (car command+args)]
                     [arguments (cdr command+args)])
                 (with-syntax ([command command]
                               [(argument ...) arguments]
                               [(option ...) option-forms]
                               [check check?])
                   (case kind
                     [(run-process)
                      #'(%run-process 'run-process command (list argument ...) (list option ...) check)]
                     [(capture-process)
                      #'(%capture-process 'capture-process command (list argument ...) (list option ...) check)]
                     [(shell-command)
                      (unless (null? arguments)
                        (syntax-error stx "shell-command accepts one command expression"))
                      #'(%run-shell-command 'shell-command command (list option ...) check)]
                     [(capture-shell-command)
                      (unless (null? arguments)
                        (syntax-error stx "capture-shell-command accepts one command expression"))
                      #'(%capture-shell-command 'capture-shell-command command (list option ...) check)]
                     [else
                      (syntax-error #'kind "unknown process macro kind")])))))]))))

  #|macro:run-process
The `run-process` macro runs `program` with zero or more command argument expressions and returns a process exit-status record.
The `program` form and command argument forms are unquoted expressions before any option keyword.
Supported option keywords are `:cwd`, `:env`, `:env-mode`, `:stdin`, `:stdout`, `:stderr`, `:encoding`, `:timeout`, and `:success`.
The option values `capture`, `inherit`, `null`, `stdout`, and `replace` may be written as bare identifiers and are treated as symbols.
|#
  (define-syntax run-process
    (syntax-rules ()
      [(_ form ...) ($process-form run-process #f form ...)]))

  #|macro:run-process/check
The `run-process/check` macro is like `run-process`, but raises a system exit error when the process exit status is unsuccessful.
The `program` form and command argument forms are unquoted expressions before any option keyword.
Supported option keywords are `:cwd`, `:env`, `:env-mode`, `:stdin`, `:stdout`, `:stderr`, `:encoding`, `:timeout`, and `:success`.
The option values `capture`, `inherit`, `null`, `stdout`, and `replace` may be written as bare identifiers and are treated as symbols.
|#
  (define-syntax run-process/check
    (syntax-rules ()
      [(_ form ...) ($process-form run-process #t form ...)]))

  #|macro:capture-process
The `capture-process` macro runs `program` with zero or more command argument expressions and returns a process result record.
The `program` form and command argument forms are unquoted expressions before any option keyword.
Supported option keywords are `:cwd`, `:env`, `:env-mode`, `:stdin`, `:stdout`, `:stderr`, `:encoding`, `:timeout`, and `:success`.
The option values `capture`, `inherit`, `null`, `stdout`, and `replace` may be written as bare identifiers and are treated as symbols.
|#
  (define-syntax capture-process
    (syntax-rules ()
      [(_ form ...) ($process-form capture-process #f form ...)]))

  #|macro:capture-process/check
The `capture-process/check` macro is like `capture-process`, but raises a system exit error when the process exit status is unsuccessful.
The `program` form and command argument forms are unquoted expressions before any option keyword.
Supported option keywords are `:cwd`, `:env`, `:env-mode`, `:stdin`, `:stdout`, `:stderr`, `:encoding`, `:timeout`, and `:success`.
The option values `capture`, `inherit`, `null`, `stdout`, and `replace` may be written as bare identifiers and are treated as symbols.
|#
  (define-syntax capture-process/check
    (syntax-rules ()
      [(_ form ...) ($process-form capture-process #t form ...)]))

  #|macro:shell-command
The `shell-command` macro runs the shell command string produced by `command` and returns a process exit-status record.
The `command` form is an unquoted expression followed by optional keyword clauses.
Supported option keywords are `:cwd`, `:env`, `:env-mode`, `:stdin`, `:stdout`, `:stderr`, `:encoding`, `:timeout`, and `:success`.
The option values `capture`, `inherit`, `null`, `stdout`, and `replace` may be written as bare identifiers and are treated as symbols.
|#
  (define-syntax shell-command
    (syntax-rules ()
      [(_ form ...) ($process-form shell-command #f form ...)]))

  #|macro:capture-shell-command
The `capture-shell-command` macro runs the shell command string produced by `command` and returns a process result record.
The `command` form is an unquoted expression followed by optional keyword clauses.
Supported option keywords are `:cwd`, `:env`, `:env-mode`, `:stdin`, `:stdout`, `:stderr`, `:encoding`, `:timeout`, and `:success`.
The option values `capture`, `inherit`, `null`, `stdout`, and `replace` may be written as bare identifiers and are treated as symbols.
|#
  (define-syntax capture-shell-command
    (syntax-rules ()
      [(_ form ...) ($process-form capture-shell-command #f form ...)]))

  )
