(load "examples/file-transfer-script-common.ss")

(define lookup-client
  (lambda (name)
    (case (string->symbol name)
      [(tcp) tcp-socket-file-client]
      [(http) http-file-client]
      [(ftp) ftp-file-client]
      [(sftp) sftp-file-client]
      [(websocket) websocket-file-client]
      [(rpc) rpc-file-client]
      [(grpc) grpc-file-client]
      [else
       (errorf 'transfer-example-client
               "unknown protocol ~s"
               name)])))

(let ([arg* (command-line-arguments)])
  (unless (>= (length arg*) 2)
    (errorf 'transfer-example-client
            "expected protocol and at least one file path, given ~s"
            arg*))
  ((lookup-client (car arg*)) (cdr arg*)))
