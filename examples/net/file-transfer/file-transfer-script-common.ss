(load "examples/net/file-transfer/file-transfer-common.ss")
(load "examples/net/file-transfer/file-transfer-tcp.ss")
(load "examples/net/file-transfer/file-transfer-http.ss")
(load "examples/net/file-transfer/file-transfer-ftp.ss")
(load "examples/net/file-transfer/file-transfer-sftp.ss")
(load "examples/net/file-transfer/file-transfer-websocket.ss")
(load "examples/net/file-transfer/file-transfer-grpc.ss")

(define require-single-command-argument
  (lambda (who kind)
    (let ([arg* (command-line-arguments)])
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
