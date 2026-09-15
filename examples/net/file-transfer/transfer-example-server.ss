(load "examples/net/file-transfer/file-transfer-script-common.ss")

(define lookup-server
  (lambda (name)
    (case (string->symbol name)
      [(tcp) tcp-socket-file-server]
      [(http) http-file-server]
      [(ftp) ftp-file-server]
      [(sftp) sftp-file-server]
      [(websocket) websocket-file-server]
      [(grpc) grpc-file-server]
      [else
       (errorf 'transfer-example-server
               "unknown protocol ~s"
               name)])))

(let ([arg* (command-line-arguments)])
  (unless (= (length arg*) 2)
    (errorf 'transfer-example-server
            "expected protocol and upload directory, given ~s"
            arg*))
  ((lookup-server (car arg*)) (cadr arg*)))
