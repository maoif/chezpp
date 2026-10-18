(import (chezpp) (chezpp net http) (chezpp logging))
(load "examples/common.ss")

(example-run!
 'chezpp-wget
 "Download a URI to a file using HTTP or HTTP/2."
 '((uri . "Required: HTTP or HTTPS URI to download.")
   (output . "Required: destination file path.")
   (version . "HTTP policy: auto, http/1.1, or h2 (default: auto)."))
 (lambda (ref)
   (let* ([uri (example-required 'chezpp-wget ref 'uri)]
          [output (example-required 'chezpp-wget ref 'output)]
          [version (string->symbol (or (ref 'version) "auto"))]
          [client (http-open)]
          [log (make-logger 'chezpp-wget)])
     (logger-add-sink! log (make-log-port-sink 'stderr (current-error-port)))
     (dynamic-wind
       void
       (lambda ()
         (unless (memq version '(auto http/1.1 h2))
           (errorf 'chezpp-wget "version must be auto, http/1.1, or h2"))
         ;; Select the requested HTTP protocol policy on the client.
         (http-client-version-set! client version)
         ;; Stream the response body to output and close its sink on completion.
         (let ([response (http-download client uri output)])
           (with-logger log
             (log-infof "downloaded ~a to ~a with HTTP ~a (status ~a)"
                        uri output (http-response-version response)
                        (http-response-status response)))
           0))
       (lambda ()
         ;; Close both the HTTP client and logger when transfer ends or fails.
         (http-close client)
         (logger-close! log)))))
 (command-line-arguments))
