(import (chezpp))
(load "examples/net/interactive-transfer-common.ss")
(load "examples/net/sftp-client-common.ss")

(let ([arg* (command-line-arguments)])
  (unless (= (length arg*) 4)
    (errorf 'sftp-client "usage: sftp-client HOST PORT USER AGENT-OR-PRIVATE-KEY"))
  (run-sftp-client (car arg*) (string->number (cadr arg*)) (caddr arg*) (cadddr arg*)
                   (current-input-port) (current-output-port)))
