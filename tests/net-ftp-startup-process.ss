(import (chezpp))

;; Yield during readiness output so a prematurely started stdin reader takes the console mutex.
(let ([stdout (current-output-port)])
  (call-with-port
   (make-custom-textual-output-port
    "delayed FTP readiness"
    (lambda (text start count)
      (milisleep 100)
      (put-string stdout text start count)
      (flush-output-port stdout)
      count)
    #f #f void)
   (lambda (output)
     (parameterize ([current-output-port output])
       (load "net-ftp-server-process.ss")))))
