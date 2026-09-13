(library (chezpp system signal operations)
  (export send-signal send-process-signal send-process-group-signal signal-mask
          signal-mask-set! signal-block! signal-unblock! wait-signal)
  (import (chezpp chez) (chezpp utils) (chezpp system common)
          (chezpp system signal values) (chezpp system process expert))

;;;;===----------------------------------------------------------------------===
;;;; signal operations
;;;;===----------------------------------------------------------------------===

  (define $send-signal-ffi
    (foreign-procedure "chezpp_send_signal" (int int) ptr))

  #|proc:send-signal
  The `send-signal` procedure sends `sig` to process `pid`.
  The `pid` parameter is an exact integer process ID.
  The `sig` parameter is a signal record, symbol, string, or integer signal number accepted by
  `signal`.
  |#
  (define-who send-signal
    (lambda (pid sig)
      (pcheck ([$positive-integer? pid] [$signal-input? sig])
              (ffi-result-ref ($send-signal-ffi pid (signal-number ($signal-ref who sig)))))))

  #|proc:send-process-signal
  The `send-process-signal` procedure sends `sig` to `process`.
  The `process` parameter is a Chezpp process object.
  The `sig` parameter is a signal record, symbol, string, or integer signal number accepted by
  `signal`.
  |#
  (define-who send-process-signal
    (lambda (process sig)
      (pcheck ([process? process] [$signal-input? sig])
              (ffi-result-ref ($send-signal-ffi (process-pid process)
                                                (signal-number ($signal-ref who sig)))))))

  #|proc:send-process-group-signal
  The `send-process-group-signal` procedure sends `sig` to process group `process-group-id`.
  The `process-group-id` parameter is an exact integer process group ID.
  The `sig` parameter is a signal record, symbol, string, or integer signal number accepted by
  `signal`.
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
