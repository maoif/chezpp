(import (chezpp) (chezpp net http) (chezpp logging))
(load "examples/common.ss")
(load "examples/net/transfer-progress.ss")

(define download-file!
  (lambda (client uri output report)
    ;; Create a sibling file exclusively so failed requests cannot damage the destination.
    (let* ([temporary (format "~a.chezpp-download-~a-~a"
                              output (get-process-id) (random (most-positive-fixnum)))]
           [port (open-file-output-port temporary (file-options) (buffer-mode block) #f)]
           [completed 0]
           [total #f])
      (dynamic-wind
        void
        (lambda ()
          (report output 0 #f)
          ;; Stream each body chunk to disk and report bytes without buffering the whole file.
          (let* ([sink (make-http-body-sink
                        (lambda (bytes start count)
                          (put-bytevector port bytes start count)
                          (set! completed (+ completed count))
                          (report output completed total))
                        (lambda () (flush-output-port port))
                        (lambda (status headers)
                          ;; Read the total from the actual response, after followed redirects.
                          (let ([length (http-header-ref headers "Content-Length" #f)]
                                [encoding (http-header-ref headers "Transfer-Encoding" #f)])
                            (set! total
                                  (and (<= 200 status 299) (not encoding) length
                                       (positive? (string-length length))
                                       (for-all (lambda (char) (char<=? #\0 char #\9))
                                                (string->list length))
                                       (string->number length)))
                            (report output completed total))))]
                 ;; Build a GET request and wait for the streaming HTTP operation.
                 [request (make-http-request 'get uri '() #f)]
                 [response (net-operation-wait (http-send/nonblocking client request sink))]
                 [status (http-response-status response)])
            (unless (<= 200 status 299)
              (errorf 'chezpp-wget "download failed: HTTP ~a for ~a" status uri))
            (close-port port)
            ;; Publish only a complete successful download, replacing any existing output.
            (rename-file temporary output)
            (report output completed completed)
            response))
        (lambda ()
          (unless (port-closed? port) (close-port port))
          (when (file-exists? temporary) (delete-file temporary)))))))

(example-run!
 'chezpp-wget
 "Download a URI to a file using HTTP or HTTP/2."
 '((uri . "Required: HTTP or HTTPS URI to download.")
   (output . "Required: destination file path.")
   (version . "HTTP policy: auto, http/1.1, or h2 (default: auto).")
   (timeout . "Total request timeout in seconds, a positive integer (default: 3600).")
   (progress . "Show a progress bar on stderr: yes or no (default: yes).")
   (user-agent . "User-Agent sent to the server (default: chezpp-wget)."))
 (lambda (ref)
   (let* ([uri (example-required 'chezpp-wget ref 'uri)]
          [output (example-required 'chezpp-wget ref 'output)]
          [version (string->symbol (or (ref 'version) "auto"))]
          [timeout (example-option-natural 'chezpp-wget ref 'timeout 3600)]
          [user-agent (or (ref 'user-agent) "chezpp-wget")]
          [progress? (example-progress-enabled? 'chezpp-wget ref)]
          [client (http-open)]
          [log (make-logger 'chezpp-wget)])
     (logger-add-sink! log (make-log-port-sink 'stderr (current-error-port)))
     (dynamic-wind
       void
       (lambda ()
         (unless (memq version '(auto http/1.1 h2))
           (errorf 'chezpp-wget "version must be auto, http/1.1, or h2"))
         (unless (<= 1 timeout (fxquotient (most-positive-fixnum) 1000))
           (errorf 'chezpp-wget "timeout must be positive and fit in milliseconds as a fixnum"))
         ;; Allow large downloads to finish within the selected total request deadline.
         (http-set-timeout! client (* timeout 1000))
         ;; Identify the downloader; mirrors may reject requests without a User-Agent.
         (http-set-header! client "User-Agent" user-agent)
         ;; Select the requested HTTP protocol policy on the client.
         (http-client-version-set! client version)
         ;; Preserve the destination until the request has completed with a successful status.
         (let ([response (call-with-example-progress
                           progress?
                           (lambda (report) (download-file! client uri output report)))])
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
