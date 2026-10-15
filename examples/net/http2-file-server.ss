(import (chezpp) (chezpp net http) (chezpp logging))
(load "examples/net/http-file-server-common.ss")

(example-run!
 'http2-file-server
 "Serve files from a local directory on an H2-capable LWS listener."
 '(host port dir)
 (lambda (ref)
   (run-http-file-server! 'h2 ref))
 (command-line-arguments))
