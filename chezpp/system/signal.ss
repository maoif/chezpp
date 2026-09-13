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
  (import (chezpp system signal values) (chezpp system signal operations)))
