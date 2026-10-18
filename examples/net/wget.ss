(import (chezpp) (chezpp net http) (chezpp logging))
(load "examples/common.ss")

(define download-file!
  (lambda (client uri output)
    ;; Create a sibling file exclusively so failed requests cannot damage the destination.
    (let* ([temporary (format "~a.chezpp-download-~a-~a"
                              output (get-process-id) (random (most-positive-fixnum)))]
           [port (open-file-output-port temporary (file-options) (buffer-mode block) #f)])
      (dynamic-wind
        void
        (lambda ()
          ;; Stream into an owned port; the sink flushes it without taking ownership.
          (let* ([sink (make-http-port-body-sink port)]
                 ;; Build a GET request and wait for the streaming HTTP operation.
                 [request (make-http-request 'get uri '() #f)]
                 [response (net-operation-wait (http-send/nonblocking client request sink))]
                 [status (http-response-status response)])
            (unless (<= 200 status 299)
              (errorf 'chezpp-wget "download failed: HTTP ~a for ~a" status uri))
            (close-port port)
            ;; Publish only a complete successful download, replacing any existing output.
            (rename-file temporary output)
            response))
        (lambda ()
          (unless (port-closed? port) (close-port port))
          (when (file-exists? temporary) (delete-file temporary)))))))

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
         ;; Preserve the destination until the request has completed with a successful status.
         (let ([response (download-file! client uri output)])
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
