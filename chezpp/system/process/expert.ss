(library (chezpp system process expert)
  (export process-exit-status? process-exit-status-kind process-exit-status-code
          process-exit-success? make-process-exit-status
          process? process-pid process-command process-arguments process-stdin
          process-stdout process-stderr process-status process-running?
          spawn-process spawn-shell-command process-wait process-wait/no-hang
          process-wait/timeout process-kill process-terminate process-interrupt
          process-close-ports! make-pipe pipe-processes run-pipeline
          fork vfork getpid gettid getppid)
  (import (chezpp chez)
          (chezpp system common)
          (chezpp system info)
          (chezpp utils))

;;;;===----------------------------------------------------------------------===
;;;; process status records
;;;;===----------------------------------------------------------------------===

  #|proc:process-exit-status?
The `process-exit-status?` procedure returns whether `object` is a status.
The `object` parameter is any Scheme object.
|#
  #|proc:process-exit-status-kind
The `process-exit-status-kind` procedure returns the kind stored in `status`.
The `status` parameter is a process exit-status record.
|#
  #|proc:process-exit-status-code
The `process-exit-status-code` procedure returns the numeric code in `status`.
The `status` parameter is a process exit-status record.
|#
  (define-record-type ($process-exit-status make-process-exit-status
                                             process-exit-status?)
    (nongenerative)
    (fields (immutable kind process-exit-status-kind)
            (immutable code process-exit-status-code)))

  #|proc:process-exit-success?
The `process-exit-success?` procedure returns whether `status` is exit code 0.
The `status` parameter is a process exit-status record.
|#
  (define process-exit-success?
    (lambda (status)
      (pcheck ([process-exit-status? status])
              (and (eq? 'exit (process-exit-status-kind status))
                   (= 0 (process-exit-status-code status))))))

;;;;===----------------------------------------------------------------------===
;;;; process records
;;;;===----------------------------------------------------------------------===

  #|proc:process?
The `process?` procedure returns whether `object` is a Chezpp process object.
The `object` parameter is any Scheme object.
|#
  #|proc:process-pid
The `process-pid` procedure returns the operating-system process id of `process`.
The `process` parameter is a process object.
|#
  #|proc:process-command
The `process-command` procedure returns the program or command for `process`.
The `process` parameter is a process object.
|#
  #|proc:process-arguments
The `process-arguments` procedure returns the argument list stored in `process`.
The `process` parameter is a process object.
|#
  #|proc:process-stdin
The `process-stdin` procedure returns the stdin port for `process`, or `#f`.
The `process` parameter is a process object.
|#
  #|proc:process-stdout
The `process-stdout` procedure returns the stdout port for `process`, or `#f`.
The `process` parameter is a process object.
|#
  #|proc:process-stderr
The `process-stderr` procedure returns the stderr port for `process`, or `#f`.
The `process` parameter is a process object.
|#
  #|proc:process-status
The `process-status` procedure returns the cached exit status for `process`.
The `process` parameter is a process object.
|#
  (define-record-type ($process make-process process?)
    (nongenerative)
    (fields (immutable pid process-pid)
            (immutable command process-command)
            (immutable arguments process-arguments)
            (immutable stdin process-stdin)
            (immutable stdout process-stdout)
            (immutable stderr process-stderr)
            (mutable status process-status set-process-status!)))

  (define $string-list?
    (lambda (object)
      (and (list? object) (andmap string? object))))

  (define $option-alist?
    (lambda (object)
      (list? object)))

  (define $option-ref
    (lambda (options key default)
      (let ([entry (assq key options)])
        (if entry (cdr entry) default))))

  (define $env-option
    (lambda (options)
      (let ([env ($option-ref options 'env #f)])
        (if env env #f))))

  (define $cwd-option
    (lambda (options)
      (let ([cwd ($option-ref options 'cwd #f)])
        (if cwd cwd ""))))

  (define $null-mode?
    (lambda (options key)
      (eq? 'null ($option-ref options key 'inherit))))

  (define $spawn-process-ffi
    (foreign-procedure "chezpp_spawn_process" (ptr ptr string int int int) ptr))

  (define $waitpid-ffi
    (foreign-procedure "chezpp_waitpid" (int int) ptr))

  (define $send-signal-ffi
    (foreign-procedure "chezpp_send_signal" (int int) ptr))

  (define $make-pipe-ffi
    (foreign-procedure "chezpp_make_pipe" () ptr))

  (define $spawn-pipeline-capture-ffi
    (foreign-procedure "chezpp_spawn_pipeline_capture" (ptr int) ptr))

  (define $wait-status->exit-status
    (lambda (raw)
      (cond [(fx= (fxand raw #x7f) 0)
             (make-process-exit-status 'exit (fxsrl raw 8))]
            [(fx= (fxand raw #x7f) #x7f)
             (make-process-exit-status 'stopped (fxsrl raw 8))]
            [else
             (make-process-exit-status 'signal (fxand raw #x7f))])))

  #|proc:spawn-process
The `spawn-process` procedure starts `program` with `arguments`.
The `program` parameter is the executable name or path.
The `arguments` parameter is a list of argument strings.
The `options` parameter is an option alist; v1 supports `cwd`, `env`, and null
standard-stream modes.
|#
  (define-who spawn-process
    (lambda (program arguments options)
      (pcheck ([string? program] [$string-list? arguments] [$option-alist? options])
              (let* ([argv (cons program arguments)]
                     [result (ffi-result-ref
                              ($spawn-process-ffi argv
                                                  ($env-option options)
                                                  ($cwd-option options)
                                                  (if ($null-mode? options 'stdin) 1 0)
                                                  (if ($null-mode? options 'stdout) 1 0)
                                                  (if ($null-mode? options 'stderr) 1 0)))]
                     [pid (vector-ref result 0)])
                (make-process pid program (list-copy arguments) #f #f #f #f)))))

  #|proc:spawn-shell-command
The `spawn-shell-command` procedure starts `command` through the host shell.
The `command` parameter is interpreted by the host shell.
The `options` parameter is an option alist accepted by `spawn-process`.
|#
  (define-who spawn-shell-command
    (lambda (command options)
      (pcheck ([string? command] [$option-alist? options])
              (spawn-process "sh" (list "-c" command) options))))

  #|proc:process-wait/no-hang
The `process-wait/no-hang` procedure checks whether `process` has exited.
The `process` parameter is a process object.
|#
  (define $wait-vector->status
    (lambda (result)
      (if result
          ($wait-status->exit-status (vector-ref result 1))
          #f)))

  (define process-wait/no-hang
    (lambda (process)
      (pcheck ([process? process])
              (let ([cached (process-status process)])
                (if cached
                    cached
                    (let ([status ($wait-vector->status
                                   (ffi-result-ref
                                    ($waitpid-ffi (process-pid process) 1)))])
                      (when status
                        (set-process-status! process status))
                      status))))))

  #|proc:process-wait
The `process-wait` procedure waits for `process` and returns its exit status.
The `process` parameter is a process object.
|#
  (define process-wait
    (lambda (process)
      (pcheck ([process? process])
              (let ([cached (process-status process)])
                (if cached
                    cached
                    (let ([status ($wait-vector->status
                                   (ffi-result-ref
                                    ($waitpid-ffi (process-pid process) 0)))])
                      (set-process-status! process status)
                      status))))))

  #|proc:process-wait/timeout
The `process-wait/timeout` procedure waits up to `milliseconds` for `process`.
The `process` parameter is a process object.
The `milliseconds` parameter is an exact nonnegative timeout in milliseconds.
|#
  (define-who process-wait/timeout
    (lambda (process milliseconds)
      (pcheck ([process? process] [natural? milliseconds])
              (let loop ([elapsed 0])
                (let ([status (process-wait/no-hang process)])
                  (cond [status status]
                        [(fx>= elapsed milliseconds)
                         (raise (make-system-timeout-error
                                 who "process wait timed out"
                                 `((pid . ,(process-pid process))
                                   (timeout . ,milliseconds))))]
                        [else
                         (milisleep 10)
                         (loop (fx+ elapsed 10))]))))))

  #|proc:process-running?
The `process-running?` procedure returns whether `process` is still running.
The `process` parameter is a process object.
|#
  (define process-running?
    (lambda (process)
      (pcheck ([process? process])
              (not (process-wait/no-hang process)))))

  #|proc:process-kill
The `process-kill` procedure sends signal number `signal-number` to `process`.
The `process` parameter is a process object.
The `signal-number` parameter is an exact positive signal number.
|#
  (define-who process-kill
    (lambda (process signal-number)
      (pcheck ([process? process] [integer? signal-number])
              (ffi-result-ref ($send-signal-ffi (process-pid process) signal-number)))))

  #|proc:process-terminate
The `process-terminate` procedure sends `SIGTERM` to `process`.
The `process` parameter is a process object.
|#
  (define process-terminate
    (lambda (process)
      (process-kill process 15)))

  #|proc:process-interrupt
The `process-interrupt` procedure sends `SIGINT` to `process`.
The `process` parameter is a process object.
|#
  (define process-interrupt
    (lambda (process)
      (process-kill process 2)))

  #|proc:process-close-ports!
The `process-close-ports!` procedure closes ports owned by `process`.
The `process` parameter is a process object.
|#
  (define process-close-ports!
    (lambda (process)
      (pcheck ([process? process])
              (for-each (lambda (port)
                          (when (port? port)
                            (close-port port)))
                        (list (process-stdin process)
                              (process-stdout process)
                              (process-stderr process))))))

  #|proc:make-pipe
The `make-pipe` procedure returns input and output ports backed by an OS pipe.
|#
  (define make-pipe
    (lambda ()
      (let ([fds (ffi-result-ref ($make-pipe-ffi))])
        (values (open-fd-input-port (vector-ref fds 0) 'block #f)
                (open-fd-output-port (vector-ref fds 1) 'block #f)))))

  #|proc:pipe-processes
The `pipe-processes` procedure starts process specs as a live pipeline.
The `process-specs` parameter is reserved for future live process pipelines.
The `options` parameter is reserved for future live process pipeline options.
|#
  (define pipe-processes
    (lambda (process-specs options)
      (pcheck ([list? process-specs] [list? options])
              (raise-system-unsupported 'pipe-processes "pipeline process objects are not implemented"))))

  #|proc:run-pipeline
The `run-pipeline` procedure runs string-list process specs as a pipeline.
The `process-specs` parameter is a list of nonempty string lists.
|#
  (define run-pipeline
    (lambda (process-specs)
      (pcheck ([list? process-specs])
              (let ([result (ffi-result-ref ($spawn-pipeline-capture-ffi process-specs -1))])
                (list ($wait-status->exit-status (vector-ref result 1)))))))

;;;;===----------------------------------------------------------------------===
;;;; raw process APIs
;;;;===----------------------------------------------------------------------===

  (define $err-process
    (lambda (who message)
      (raise (make-system-error who #f message '()))))

  #|proc:fork
The `fork` procedure performs a raw operating-system fork.
This expert API is unsafe in a threaded Scheme runtime.
|#
  (define-who fork
    (let ([ffi (foreign-procedure "chezpp_fork" () ptr)])
      (lambda ()
        (let ([result (ffi)])
          (if (string? result) ($err-process who result) result)))))

  #|proc:vfork
The `vfork` procedure performs a raw operating-system vfork.
This expert API is unsafe unless the child immediately execs or exits.
|#
  (define-who vfork
    (let ([ffi (foreign-procedure "chezpp_vfork" () ptr)])
      (lambda ()
        (let ([result (ffi)])
          (if (string? result) ($err-process who result) result)))))

  #|proc:getpid
The `getpid` procedure returns the current process id.
|#
  (define getpid get-process-id)

  #|proc:gettid
The `gettid` procedure returns the current thread id.
|#
  (define gettid get-thread-id)

  #|proc:getppid
The `getppid` procedure returns the parent process id.
|#
  (define getppid (foreign-procedure "chezpp_getppid" () int))

  )
