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
     (exists (lambda (s) (eq? 'int (signal-name s))) (signal-list)))
