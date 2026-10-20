(import (chezpp))
(load "examples/net/legacy-example-common.ss")
(net-example-help! 'ftp-client "Open an interactive FTP client. Positional arguments: HOST PORT USER PASSWORD.")

(load "examples/net/interactive-transfer-common.ss")
(load "examples/net/ftp-client-common.ss")

(call-with-legacy-example-log
 'ftp-client
 (lambda ()
   (let ([arg* (command-line-arguments)])
     (unless (= (length arg*) 4)
       (errorf 'ftp-client "usage: ftp-client HOST PORT USER PASSWORD"))
     (run-ftp-client (car arg*) (string->number (cadr arg*)) (caddr arg*) (cadddr arg*)
                     (current-input-port) (current-output-port)))))
