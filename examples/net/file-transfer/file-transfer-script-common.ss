(load "examples/file-transfer-common.ss")
(load "examples/file-transfer-tcp.ss")
(load "examples/file-transfer-http.ss")
(load "examples/file-transfer-ftp.ss")
(load "examples/file-transfer-sftp.ss")
(load "examples/file-transfer-websocket.ss")
(load "examples/file-transfer-grpc.ss")

(define normalize-script-arguments
  (lambda (arg*)
    (let loop ([rest (reverse arg*)] [seen? #f] [out '()])
      (if (null? rest)
          out
          (let ([arg (car rest)])
            (if (and (not seen?)
                     (string? arg)
                     (string=? arg "/home/maoif/SSD/Projects/chezpp/chez++.ss"))
                (loop (cdr rest) #t out)
                (loop (cdr rest) seen? (cons arg out))))))))

(define require-single-command-argument
  (lambda (who kind)
    (let ([arg* (normalize-script-arguments (command-line-arguments))])
      (unless (= (length arg*) 1)
        (errorf who "expected exactly one ~a path argument, given ~s" kind arg*))
      (car arg*))))

(define run-file-transfer-client-script
  (lambda (who proc)
    (let ([path (require-single-command-argument who "file")])
      (proc (list path)))))

(define run-file-transfer-server-script
  (lambda (who proc)
    (let ([dir (require-single-command-argument who "directory")])
      (proc dir))))
