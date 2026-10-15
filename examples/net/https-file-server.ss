(import (chezpp) (chezpp net http) (chezpp net tls) (chezpp logging))
(load "examples/net/http-file-server-common.ss")

(example-run!
 'https-file-server
 "Serve files from a local directory over TLS."
 '(host port dir cert key)
 (lambda (ref)
   (run-http-file-server! 'https ref))
 (command-line-arguments))
