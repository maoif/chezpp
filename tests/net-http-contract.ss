(import (chezpp chez))

(define net-environment
  (environment '(chezpp net)))

(mat net-http-public-api-contract
     (andmap
      (lambda (name)
        (procedure? (eval name net-environment)))
      '(http-open
        http-send
        http-send/nonblocking
        http-download/nonblocking
        http-listen
        http-serve-loop)))

(mat net-http2-private-api-contract
     ;; Error case: the low-level HTTP/2 session API is not exported by `(chezpp net)`.
     (guard (condition [else #t])
       (eval 'http2-open net-environment)
       #f))
