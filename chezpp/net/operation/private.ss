(library (chezpp net operation private)
  (export net-operation-update?
          net-operation-update-state
          net-operation-update-poll-targets
          net-operation-update-deadline-ms
          net-operation-update-value
          net-operation-pending
          net-operation-completed
          net-operation-failed)
  (import (chezpp chez)
          (chezpp utils))

  (define-record-type (net-operation-update %make-net-operation-update
                                            net-operation-update?)
    (sealed #t)
    (opaque #f)
    (fields (immutable state net-operation-update-state)
            (immutable poll-targets net-operation-update-poll-targets)
            (immutable deadline-ms net-operation-update-deadline-ms)
            (immutable value net-operation-update-value)))

  #|proc:net-operation-pending
The `net-operation-pending` procedure constructs a pending operation update.
The `poll-targets` parameter is a list of poll targets on which the operation is waiting.
The `deadline-ms` parameter is an absolute monotonic deadline in milliseconds, or `#f`.
The return value is an immutable operation update.
|#
  (define net-operation-pending
    (lambda (poll-targets deadline-ms)
      (pcheck ([list? poll-targets]
               [(lambda (value) (or (not value) (natural? value))) deadline-ms])
              (%make-net-operation-update 'pending poll-targets deadline-ms #f))))

  #|proc:net-operation-completed
The `net-operation-completed` procedure constructs a completed operation update.
The `value` parameter is the operation's successful result.
The return value is an immutable operation update containing `value`.
|#
  (define net-operation-completed
    (lambda (value)
      (pcheck ()
              (%make-net-operation-update 'completed '() #f value))))

  #|proc:net-operation-failed
The `net-operation-failed` procedure constructs a failed operation update.
The `failure` parameter is the condition that caused the operation to fail.
The return value is an immutable operation update containing `failure`.
|#
  (define net-operation-failed
    (lambda (failure)
      (pcheck ([condition? failure])
              (%make-net-operation-update 'failed '() #f failure))))
  )
