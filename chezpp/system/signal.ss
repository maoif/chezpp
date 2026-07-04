(library (chezpp system signal)
  (export
          ;; signal records and conversion
          signal?
          signal
          signal-name
          signal-number
          signal->string
          string->signal
          signal-list

          ;; common signal names
          hup
          int
          quit
          kill
          usr1
          usr2
          alrm
          term
          chld
          pipe

          ;; sending and waiting
          send-signal
          send-process-signal
          send-process-group-signal
          signal-mask
          signal-mask-set!
          signal-block!
          signal-unblock!
          wait-signal)
  (import (chezpp chez)
          (chezpp system common)
          (chezpp system process expert)
          (chezpp utils))

;;;;===----------------------------------------------------------------------===
;;;; signal values
;;;;===----------------------------------------------------------------------===

  #|proc:signal?
The `signal?` procedure returns `#t` when its argument is a signal record, otherwise `#f`.
The `object` parameter is the object to test.
|#
  #|proc:signal-name
The `signal-name` procedure returns the canonical symbolic name of `sig`.
The `sig` parameter is a signal record returned by `signal`, `string->signal`, or `signal-list`.
|#
  #|proc:signal-number
The `signal-number` procedure returns the operating-system signal number of `sig`.
The `sig` parameter is a signal record returned by `signal`, `string->signal`, or `signal-list`.
|#
  (define-record-type ($signal make-signal signal?)
    (nongenerative)
    (fields (immutable name signal-name)
            (immutable number signal-number)))

  (define $signal-table
    (list (make-signal 'hup 1)
          (make-signal 'int 2)
          (make-signal 'quit 3)
          (make-signal 'kill 9)
          (make-signal 'usr1 10)
          (make-signal 'usr2 12)
          (make-signal 'alrm 14)
          (make-signal 'term 15)
          (make-signal 'chld 17)
          (make-signal 'pipe 13)))

  (define hup 'hup)
  (define int 'int)
  (define quit 'quit)
  (define kill 'kill)
  (define usr1 'usr1)
  (define usr2 'usr2)
  (define alrm 'alrm)
  (define term 'term)
  (define chld 'chld)
  (define pipe 'pipe)

  (define $signal-input?
    (lambda (x)
      (or (signal? x) (symbol? x) (string? x) (integer? x))))

  (define $positive-integer?
    (lambda (x)
      (and (integer? x) (> x 0))))

  (define $string-prefix?
    (lambda (prefix str)
      (let ([prefix-len (string-length prefix)]
            [str-len (string-length str)])
        (and (fx<= prefix-len str-len)
             (let loop ([i 0])
               (or (fx= i prefix-len)
                   (and (char=? (string-ref prefix i) (string-ref str i))
                        (loop (fx+ i 1)))))))))

  (define $normalize-signal-string
    (lambda (str)
      (let* ([up (string-upcase str)]
             [name (if ($string-prefix? "SIG" up)
                       (substring up 3 (string-length up))
                       up)])
        (string->symbol (string-downcase name)))))

  (define $find-signal
    (lambda (pred)
      (let loop ([signals $signal-table])
        (cond [(null? signals) #f]
              [(pred (car signals)) (car signals)]
              [else (loop (cdr signals))]))))

  (define $symbol->signal
    (lambda (who name)
      (let ([sig ($find-signal (lambda (sig) (eq? name (signal-name sig))))])
        (or sig (errorf who "unknown signal name: ~a" name)))))

  (define $number->signal
    (lambda (who number)
      (let ([sig ($find-signal (lambda (sig) (= number (signal-number sig))))])
        (or sig (errorf who "unknown signal number: ~a" number)))))

  (define $signal-ref
    (lambda (who spec)
      (cond [(signal? spec) spec]
            [(symbol? spec) ($symbol->signal who spec)]
            [(string? spec) ($symbol->signal who ($normalize-signal-string spec))]
            [(integer? spec) ($number->signal who spec)]
            [else (errorf who "invalid signal: ~a" spec)])))

  #|proc:%signal
The `%signal` procedure returns the signal record named or numbered by `spec`.
The `spec` parameter is a signal record, symbol, string such as `"TERM"` or `"SIGTERM"`, or integer signal number.
|#
  (define-who %signal
    (lambda (spec)
      (pcheck ([$signal-input? spec])
              ($signal-ref who spec))))

  #|macro:signal
The `signal` macro returns the signal record named or numbered by `spec`.
The `spec` form may be a bare identifier such as `term`, a quoted symbol such as `'term`, a string such as `"TERM"` or `"SIGTERM"`, an integer signal number, or an existing signal record.
|#
  (define-syntax signal
    (lambda (stx)
      (syntax-case stx ()
        [(_ spec)
         #'(%signal spec)])))

  #|proc:string->signal
The `string->signal` procedure returns the signal record named by `string`.
The `string` parameter is a signal name with or without a leading `"SIG"` prefix, such as `"TERM"` or `"SIGTERM"`.
|#
  (define-who string->signal
    (lambda (string)
      (pcheck ([string? string])
              ($signal-ref who string))))

  #|proc:signal->string
The `signal->string` procedure returns the conventional `"SIG..."` name for `sig`.
The `sig` parameter is a signal record.
|#
  (define signal->string
    (lambda (sig)
      (pcheck ([signal? sig])
              (string-append "SIG" (string-upcase (symbol->string (signal-name sig)))))))

  #|proc:signal-list
The `signal-list` procedure returns the list of known signal records.
|#
  (define signal-list
    (lambda ()
      (list-copy $signal-table)))

;;;;===----------------------------------------------------------------------===
;;;; signal operations
;;;;===----------------------------------------------------------------------===

  (define $send-signal-ffi
    (foreign-procedure "chezpp_send_signal" (int int) ptr))

  #|proc:send-signal
The `send-signal` procedure sends `sig` to process `pid`.
The `pid` parameter is an exact integer process ID.
The `sig` parameter is a signal record, symbol, string, or integer signal number accepted by `signal`.
|#
  (define-who send-signal
    (lambda (pid sig)
      (pcheck ([$positive-integer? pid] [$signal-input? sig])
              (ffi-result-ref ($send-signal-ffi pid (signal-number ($signal-ref who sig)))))))

  #|proc:send-process-signal
The `send-process-signal` procedure sends `sig` to `process`.
The `process` parameter is a Chezpp process object.
The `sig` parameter is a signal record, symbol, string, or integer signal number accepted by `signal`.
|#
  (define-who send-process-signal
    (lambda (process sig)
      (pcheck ([process? process] [$signal-input? sig])
              (ffi-result-ref ($send-signal-ffi (process-pid process)
                                                (signal-number ($signal-ref who sig)))))))

  #|proc:send-process-group-signal
The `send-process-group-signal` procedure sends `sig` to process group `process-group-id`.
The `process-group-id` parameter is an exact integer process group ID.
The `sig` parameter is a signal record, symbol, string, or integer signal number accepted by `signal`.
|#
  (define-who send-process-group-signal
    (lambda (process-group-id sig)
      (pcheck ([$positive-integer? process-group-id] [$signal-input? sig])
              (ffi-result-ref ($send-signal-ffi (- process-group-id) (signal-number ($signal-ref who sig)))))))

  #|proc:signal-mask
The `signal-mask` procedure returns the current thread signal mask.
|#
  (define-who signal-mask
    (lambda ()
      (raise-system-unsupported who "signal mask inspection is unsupported")))

  #|proc:signal-mask-set!
The `signal-mask-set!` procedure sets the current thread signal mask to `signals`.
The `signals` parameter is a list of signal specs accepted by `signal`.
|#
  (define-who signal-mask-set!
    (lambda (signals)
      (pcheck ([list? signals])
              (for-each (lambda (sig) ($signal-ref who sig)) signals)
              (raise-system-unsupported who "signal mask setting is unsupported"))))

  #|proc:signal-block!
The `signal-block!` procedure blocks each signal in `signals` for the current thread.
The `signals` parameter is a list of signal specs accepted by `signal`.
|#
  (define-who signal-block!
    (lambda (signals)
      (pcheck ([list? signals])
              (for-each (lambda (sig) ($signal-ref who sig)) signals)
              (raise-system-unsupported who "signal blocking is unsupported"))))

  #|proc:signal-unblock!
The `signal-unblock!` procedure unblocks each signal in `signals` for the current thread.
The `signals` parameter is a list of signal specs accepted by `signal`.
|#
  (define-who signal-unblock!
    (lambda (signals)
      (pcheck ([list? signals])
              (for-each (lambda (sig) ($signal-ref who sig)) signals)
              (raise-system-unsupported who "signal unblocking is unsupported"))))

  #|proc:wait-signal
The `wait-signal` procedure waits until one of `signals` is delivered and returns that signal.
The `signals` parameter is a list of signal specs accepted by `signal`.
|#
  (define-who wait-signal
    (lambda (signals)
      (pcheck ([list? signals])
              (for-each (lambda (sig) ($signal-ref who sig)) signals)
              (raise-system-unsupported who "waiting for signals is unsupported"))))

  )
