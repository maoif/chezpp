(import (chezpp) (chezpp net http) (chezpp net tls) (chezpp logging))
(load "examples/net/http-file-server-common.ss")

(example-run!
 'https-file-server
 "Serve files from a local directory over TLS."
 '((host . "Listener address (default: 127.0.0.1).")
   (port . "Listener port, 0 through 65535 (default: 8080; 0 requests a free port).")
   (dir . "Required: local directory to serve; symlink paths are rejected.")
   (cert . "Required: path to the server PEM certificate.")
   (key . "Required: path to the matching PEM private key."))
 (lambda (ref)
   (run-http-file-server! 'https ref))
 (command-line-arguments))
