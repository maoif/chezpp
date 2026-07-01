(import (chezpp))

(mat signal-parse

     (signal? (signal term))
     (signal? (signal "TERM"))
     (signal? (signal "SIGTERM"))
     (= (signal-number (signal term))
        (signal-number (signal "TERM")))
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

(mat signal-send-child

     (let ([p (spawn-process "sh" '("-c" "sleep 5") '((stdout . null) (stderr . null)))])
       (send-process-signal p term)
       (let ([status (process-wait/timeout p 1000)])
         (eq? 'signal (process-exit-status-kind status)))))
