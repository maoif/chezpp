(import (chezpp) (chezpp net))

(load "examples/net/file-transfer/file-transfer-common.ss")
(load "examples/net/file-transfer/file-transfer-http.ss")

(define secure-context
  (lambda (role)
    (let ([ctx (make-tls-context role)])
      (if (eq? role 'server)
          (begin
            (tls-context-load-cert! ctx (getenv "CHEZPP_TRANSFER_CERT"))
            (tls-context-load-private-key! ctx (getenv "CHEZPP_TRANSFER_KEY")))
          (begin
            (tls-context-load-ca-file! ctx (getenv "CHEZPP_TRANSFER_CERT"))
            (tls-context-set-verify! ctx #t)))
      ctx)))

(let ([arg* (command-line-arguments)])
  (unless (= (length arg*) 1)
    (errorf 'file-transfer-https "expected one directory or file path"))
  (let ([server? (string=? (getenv "CHEZPP_TRANSFER_ROLE") "server")]
        [ctx (secure-context
              (if (string=? (getenv "CHEZPP_TRANSFER_ROLE") "server") 'server 'client))])
    (set! http-file-transfer-port 41008)
    (if server?
        (begin
          (set! http-file-transfer-server-tls-context ctx)
          (http-file-server (car arg*)))
        (begin
          (set! http-file-transfer-client-tls-context ctx)
          (http-file-client (list (car arg*)))))
    (close-tls-context ctx)))
