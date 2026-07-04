(library (chezpp system process)
  (export
          ;; exit status records
          process-exit-status?
          process-exit-status-kind
          process-exit-status-code
          process-exit-success?

          ;; process records
          process?
          process-pid
          process-command
          process-arguments
          process-stdin
          process-stdout
          process-stderr
          process-status
          process-running?

          ;; expert process controls re-exported for convenience
          spawn-process
          spawn-shell-command
          process-wait
          process-wait/no-hang
          process-wait/timeout
          process-kill
          process-terminate
          process-interrupt
          process-close-ports!
          make-pipe
          pipe-processes
          run-pipeline
          fork
          vfork
          getpid
          gettid
          getppid

          ;; process result records
          process-result?
          process-result-status
          process-result-stdout
          process-result-stderr
          process-result-pid
          process-result-command

          ;; high-level process forms
          run-process
          run-process/check
          capture-process
          capture-process/check
          shell-command
          capture-shell-command
          capture-pipeline)
  (import (chezpp chez)
          (chezpp system common)
          (chezpp system process expert)
          (chezpp utils))

;;;;===----------------------------------------------------------------------===
;;;; process result records
;;;;===----------------------------------------------------------------------===

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

;;;;===----------------------------------------------------------------------===
;;;; process backend
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

  (define $status-accepted?
    (lambda (status success)
      (cond [(not success) #t]
            [(procedure? success) (success status)]
            [(list? success)
             (and (eq? 'exit (process-exit-status-kind status))
                  (memv (process-exit-status-code status) success))]
            [else #f])))

  (define $check-accepted-status
    (lambda (who command status options check?)
      (let ([success ($option-ref options 'success (if check? '(0) #f))])
        (unless ($status-accepted? status success)
          ($raise-exit-error who command status)))))

  (define $wait-status->exit-status
    (lambda (raw)
      (cond [(fx= (fxand raw #x7f) 0)
             (make-process-exit-status 'exit (fxsrl raw 8))]
            [(fx= (fxand raw #x7f) #x7f)
             (make-process-exit-status 'stopped (fxsrl raw 8))]
            [else
             (make-process-exit-status 'signal (fxand raw #x7f))])))

  (define $stdin-payload
    (lambda (options)
      (let ([stdin ($option-ref options 'stdin 'null)])
        (cond [(or (not stdin) (eq? stdin 'null) (eq? stdin 'inherit)) #f]
              [(string? stdin) (string->utf8 stdin)]
              [(bytevector? stdin) stdin]
              [else (errorf 'process "unsupported stdin option: ~a" stdin)]))))

  (define $env-option
    (lambda (options)
      (let ([env ($option-ref options 'env #f)])
        (if env env #f))))

  (define $cwd-option
    (lambda (options)
      (let ([cwd ($option-ref options 'cwd #f)])
        (if cwd cwd ""))))

  (define $timeout-option
    (lambda (options)
      (let ([timeout ($option-ref options 'timeout #f)])
        (if timeout timeout -1))))

  (define $capture-stdout?
    (lambda (options default)
      (eq? 'capture ($option-ref options 'stdout default))))

  (define $capture-stderr?
    (lambda (options default)
      (eq? 'capture ($option-ref options 'stderr default))))

  (define $null-stdout?
    (lambda (options)
      (eq? 'null ($option-ref options 'stdout 'inherit))))

  (define $null-stderr?
    (lambda (options)
      (eq? 'null ($option-ref options 'stderr 'inherit))))

  (define $stderr-to-stdout?
    (lambda (options)
      (eq? 'stdout ($option-ref options 'stderr 'inherit))))

  (define $spawn-capture-ffi
    (foreign-procedure "chezpp_spawn_capture"
                       (ptr ptr string ptr int int int int int int)
                       ptr))

  (define $spawn-pipeline-capture-ffi
    (foreign-procedure "chezpp_spawn_pipeline_capture" (ptr int) ptr))

  (define $capture-vector->result
    (lambda (command result options check? who)
      (let* ([pid (vector-ref result 0)]
             [status ($wait-status->exit-status (vector-ref result 1))]
             [stdout-mode ($option-ref options 'stdout 'capture)]
             [stderr-mode ($option-ref options 'stderr 'capture)])
        ($check-accepted-status who command status options check?)
        (make-process-result status
                             (if (eq? stdout-mode 'capture) (vector-ref result 2) #f)
                             (if (eq? stderr-mode 'capture) (vector-ref result 3) #f)
                             pid
                             command))))

  (define $run-vector->status
    (lambda (command result options check? who)
      (let ([status ($wait-status->exit-status (vector-ref result 1))])
        ($check-accepted-status who command status options check?)
        status)))

  (define $spawn-capture
    (lambda (argv options capture-default)
      (ffi-result-ref
       ($spawn-capture-ffi argv
                           ($env-option options)
                           ($cwd-option options)
                           ($stdin-payload options)
                           (if ($capture-stdout? options capture-default) 1 0)
                           (if ($capture-stderr? options capture-default) 1 0)
                           (if ($null-stdout? options) 1 0)
                           (if ($null-stderr? options) 1 0)
                           (if ($stderr-to-stdout? options) 1 0)
                           ($timeout-option options)))))

  (define %run-process
    (lambda (who program arguments options check?)
      (pcheck ([string? program] [$string-list? arguments] [$option-alist? options] [boolean? check?])
              ($run-vector->status program
                                  ($spawn-capture (cons program arguments) options 'inherit)
                                  options
                                  check?
                                  who))))

  (define %capture-process
    (lambda (who program arguments options check?)
      (pcheck ([string? program] [$string-list? arguments] [$option-alist? options] [boolean? check?])
              ($capture-vector->result program
                                      ($spawn-capture (cons program arguments) options 'capture)
                                      options
                                      check?
                                      who))))

  (define %run-shell-command
    (lambda (who command options check?)
      (pcheck ([string? command] [$option-alist? options] [boolean? check?])
              ($run-vector->status command
                                  ($spawn-capture (list "sh" "-c" command) options 'inherit)
                                  options
                                  check?
                                  who))))

  (define %capture-shell-command
    (lambda (who command options check?)
      (pcheck ([string? command] [$option-alist? options] [boolean? check?])
              ($capture-vector->result command
                                      ($spawn-capture (list "sh" "-c" command) options 'capture)
                                      options
                                      check?
                                      who))))

  #|proc:capture-pipeline
The `capture-pipeline` procedure runs string-list process specs as a pipeline.
The `process-specs` parameter is a list of nonempty string lists.
|#
  (define capture-pipeline
    (lambda (process-specs)
      (pcheck ([list? process-specs])
              (let ([result (ffi-result-ref ($spawn-pipeline-capture-ffi process-specs -1))])
                ($capture-vector->result 'pipeline result '((stdout . capture)) #f
                                        'capture-pipeline)))))

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
      (define parse-command-word
        (lambda (stx)
          (if (identifier? stx)
              #`'#,(datum->syntax stx (symbol->string (syntax->datum stx)))
              stx)))
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
                               [command-expr (parse-command-word command)]
                               [(argument ...) (map parse-command-word arguments)]
                               [(option ...) option-forms]
                               [check check?])
                   (case kind
                     [(run-process)
                      #'(%run-process 'run-process command-expr (list argument ...) (list option ...) check)]
                     [(capture-process)
                      #'(%capture-process 'capture-process command-expr (list argument ...) (list option ...) check)]
                     [(shell-command)
                      (unless (null? arguments)
                        (syntax-error stx "shell-command accepts one command expression"))
                      #'(%run-shell-command 'shell-command command-expr (list option ...) check)]
                     [(capture-shell-command)
                      (unless (null? arguments)
                        (syntax-error stx "capture-shell-command accepts one command expression"))
                      #'(%capture-shell-command 'capture-shell-command command-expr (list option ...) check)]
                     [else
                      (syntax-error #'kind "unknown process macro kind")])))))]))))

  #|macro:run-process
The `run-process` macro runs `program` with arguments and returns an exit status.
The `program` form and argument forms are expressions before any option keyword.
The `:cwd` option accepts a string working directory or `#f`.
The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`.
The `:stdout` option accepts `inherit`, `null`, or `capture`; captured output is ignored.
The `:stderr` option accepts `inherit`, `null`, `capture`, or `stdout`.
The `:encoding` option is parsed for future use and is currently ignored.
The `:timeout` option accepts `#f` or an exact millisecond timeout.
The `:success` option accepts `#f`, a list of exit codes, or a status predicate.
Bare `capture`, `inherit`, `null`, `stdout`, and `replace` are treated as symbols.
|#
  (define-syntax run-process
    (syntax-rules ()
      [(_ form ...) ($process-form run-process #f form ...)]))

  #|macro:run-process/check
The `run-process/check` macro runs `program` and raises on an unacceptable exit.
The `program` form and argument forms are expressions before any option keyword.
The `:cwd` option accepts a string working directory or `#f`.
The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`.
The `:stdout` option accepts `inherit`, `null`, or `capture`; captured output is ignored.
The `:stderr` option accepts `inherit`, `null`, `capture`, or `stdout`.
The `:encoding` option is parsed for future use and is currently ignored.
The `:timeout` option accepts `#f` or an exact millisecond timeout.
The `:success` option accepts `#f`, a list of exit codes, or a status predicate.
Bare `capture`, `inherit`, `null`, `stdout`, and `replace` are treated as symbols.
|#
  (define-syntax run-process/check
    (syntax-rules ()
      [(_ form ...) ($process-form run-process #t form ...)]))

  #|macro:capture-process
The `capture-process` macro runs `program` with arguments and returns a result.
The `program` form and argument forms are expressions before any option keyword.
The `:cwd` option accepts a string working directory or `#f`.
The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`.
The `:stdout` option accepts `capture`, `inherit`, or `null`.
The `:stderr` option accepts `capture`, `inherit`, `null`, or `stdout`.
The `:encoding` option is parsed for future use and is currently ignored.
The `:timeout` option accepts `#f` or an exact millisecond timeout.
The `:success` option accepts `#f`, a list of exit codes, or a status predicate.
Bare `capture`, `inherit`, `null`, `stdout`, and `replace` are treated as symbols.
|#
  (define-syntax capture-process
    (syntax-rules ()
      [(_ form ...) ($process-form capture-process #f form ...)]))

  #|macro:capture-process/check
The `capture-process/check` macro runs `program` and raises on an unacceptable exit.
The `program` form and argument forms are expressions before any option keyword.
The `:cwd` option accepts a string working directory or `#f`.
The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`.
The `:stdout` option accepts `capture`, `inherit`, or `null`.
The `:stderr` option accepts `capture`, `inherit`, `null`, or `stdout`.
The `:encoding` option is parsed for future use and is currently ignored.
The `:timeout` option accepts `#f` or an exact millisecond timeout.
The `:success` option accepts `#f`, a list of exit codes, or a status predicate.
Bare `capture`, `inherit`, `null`, `stdout`, and `replace` are treated as symbols.
|#
  (define-syntax capture-process/check
    (syntax-rules ()
      [(_ form ...) ($process-form capture-process #t form ...)]))

  #|macro:shell-command
The `shell-command` macro runs `command` through the host shell.
The `command` form is an expression followed by optional keyword clauses.
The `:cwd` option accepts a string working directory or `#f`.
The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`.
The `:stdout` option accepts `inherit`, `null`, or `capture`; captured output is ignored.
The `:stderr` option accepts `inherit`, `null`, `capture`, or `stdout`.
The `:encoding` option is parsed for future use and is currently ignored.
The `:timeout` option accepts `#f` or an exact millisecond timeout.
The `:success` option accepts `#f`, a list of exit codes, or a status predicate.
Bare `capture`, `inherit`, `null`, `stdout`, and `replace` are treated as symbols.
|#
  (define-syntax shell-command
    (syntax-rules ()
      [(_ form ...) ($process-form shell-command #f form ...)]))

  #|macro:capture-shell-command
The `capture-shell-command` macro runs `command` through the host shell.
The `command` form is an expression followed by optional keyword clauses.
The `:cwd` option accepts a string working directory or `#f`.
The `:env` option accepts an alist of string pairs or `#f` to inherit the environment.
The `:env-mode` option accepts `replace`; replacement is implied when `:env` is set.
The `:stdin` option accepts a string, bytevector, `null`, `inherit`, or `#f`.
The `:stdout` option accepts `capture`, `inherit`, or `null`.
The `:stderr` option accepts `capture`, `inherit`, `null`, or `stdout`.
The `:encoding` option is parsed for future use and is currently ignored.
The `:timeout` option accepts `#f` or an exact millisecond timeout.
The `:success` option accepts `#f`, a list of exit codes, or a status predicate.
Bare `capture`, `inherit`, `null`, `stdout`, and `replace` are treated as symbols.
|#
  (define-syntax capture-shell-command
    (syntax-rules ()
      [(_ form ...) ($process-form capture-shell-command #f form ...)]))

  )
