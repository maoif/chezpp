(import (chezpp))
(load "examples/net/interactive-transfer-common.ss")
(load "examples/net/ftp-client-common.ss")

(let ([arg* (command-line-arguments)])
  (unless (= (length arg*) 4)
    (errorf 'ftp-client "usage: ftp-client HOST PORT USER PASSWORD"))
  (run-ftp-client (car arg*) (string->number (cadr arg*)) (caddr arg*) (cadddr arg*)
                  (current-input-port) (current-output-port)))
