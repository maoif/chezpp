(import (chezpp))

(mat signal-parse

     (signal? (signal term))
     (signal? (signal "TERM"))
     (signal? (signal "SIGTERM"))
     (signal? (signal 15))
     (signal? (string->signal "sigterm"))
     (= (signal-number (signal term))
        (signal-number (signal "TERM")))
     (= (signal-number (signal term))
        (signal-number (signal 15)))
     (eq? 'term (signal-name (string->signal "TERM")))
     (string? (signal->string (signal term))))

(mat signal-list-basic

     (exists (lambda (s) (eq? 'term (signal-name s))) (signal-list))
     (exists (lambda (s) (eq? 'int (signal-name s))) (signal-list))

     (let ([signals (signal-list)])
       (set-cdr! signals '())
       (exists (lambda (s) (eq? 'int (signal-name s))) (signal-list))))

(mat signal-bound-identifiers

     (let ([term (signal int)])
       (eq? 'int (signal-name (signal term)))))

;; Error case: unknown signal names and numbers should be rejected.
(mat signal-parse-errors

     (guard (c [(error? c) #t] [else #f])
       (signal "NOT_A_SIGNAL")
       #f)

     (guard (c [(error? c) #t] [else #f])
       (signal 9999)
       #f))

;; Error case: send-signal should reject process-group/all-process pid values.
(mat signal-send-validation

     (guard (c [(error? c) #t] [else #f])
       (send-signal 0 term)
       #f)

     (guard (c [(error? c) #t] [else #f])
       (send-signal -1 term)
       #f)

     (guard (c [(error? c) #t] [else #f])
       (send-process-group-signal 0 term)
       #f)

     (guard (c [(error? c) #t] [else #f])
       (send-process-group-signal -1 term)
       #f))

;; Error case: signal mask APIs are currently unsupported after validation.
(mat signal-mask-unsupported

     (guard (c [(system-unsupported-error? c) #t] [else #f])
       (signal-mask)
       #f)

     (guard (c [(system-unsupported-error? c) #t] [else #f])
       (signal-mask-set! (list term int))
       #f)

     (guard (c [(system-unsupported-error? c) #t] [else #f])
       (signal-block! (list term))
       #f)

     (guard (c [(system-unsupported-error? c) #t] [else #f])
       (signal-unblock! (list term))
       #f)

     (guard (c [(system-unsupported-error? c) #t] [else #f])
       (wait-signal (list term))
       #f))

(mat signal-send-child

     (let ([p (spawn-process "sh" '("-c" "sleep 5") '((stdout . null) (stderr . null)))])
       (send-process-signal p term)
       (let ([status (process-wait/timeout p 1000)])
         (eq? 'signal (process-exit-status-kind status))))

     (let ([p (spawn-process "sh" '("-c" "sleep 5") '((stdout . null) (stderr . null)))])
       (send-signal (process-pid p) "TERM")
       (let ([status (process-wait/timeout p 1000)])
         (eq? 'signal (process-exit-status-kind status)))))
