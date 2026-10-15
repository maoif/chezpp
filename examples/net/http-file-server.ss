(import (chezpp) (chezpp net http) (chezpp logging))
(load "examples/net/http-file-server-common.ss")

(example-run!
 'http-file-server
 "Serve regular files from a local directory over HTTP/1."
 '(host port dir)
 (lambda (ref)
   (run-http-file-server! 'http1 ref))
 (command-line-arguments))
