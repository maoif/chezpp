(import (chezpp net http) (chezpp net tls) (chezpp net uri)
        (chezpp path) (chezpp logging))
(load "examples/common.ss")

(define server-log
  (lambda ()
    (let ([logger (make-logger 'chezpp-file-server)])
      (logger-add-sink! logger
                        (make-log-port-sink 'stderr (current-error-port)))
      logger)))

(define reject-symlink-path?
  (lambda (path)
    (let loop ([current (path-normalize (path-parse 'unix path))]
               [parts (reverse (path-components
                                (path-normalize (path-parse 'unix path))))])
      (or (file-symbolic-link? (path-render current))
          (and (pair? parts)
               (loop (path-dirname current) (cdr parts)))))))

(define file-server-response
  (lambda (root request)
    (let* ([request-uri (http-request-uri request)]
           [segments (uri-path-segments request-uri)])
      (cond
       [(not (string=? (http-request-method request) "GET"))
        (make-http-response 405 "Method Not Allowed" '() #vu8())]
       [(or (null? segments)
            (exists (lambda (segment)
                      (or (string=? segment "")
                          (string=? segment ".")
                          (string=? segment "..")
                          (string-contains? segment "/")
                          (string-contains? segment "\\")))
                    segments))
        (make-http-response 400 "Bad Request" '() #vu8())]
       [else
        (let ([path (fold-left path-add-component (path-parse 'unix root) segments)])
          (let ([full-path (path-render path)])
            (if (or (not (file-regular? full-path))
                    (reject-symlink-path? full-path))
                (make-http-response 404 "Not Found" '() #vu8())
                (make-http-response
                 200 "OK"
                 '(("content-type" . "application/octet-stream"))
                 (make-http-file-body-source full-path)))))]))))

(define run-http-file-server!
  (lambda (protocol-version ref)
    (let* ([host (or (ref 'host) "127.0.0.1")]
           [port (example-option-natural 'http-file-server ref 'port 8080)]
           [root-arg (example-required 'http-file-server ref 'dir)]
           [root (path-render (path-normalize (path-absolute
                                                (path-parse 'unix (current-directory))
                                                (path-parse 'unix root-arg))))])
      (unless (and (file-directory? root) (not (reject-symlink-path? root)))
        (errorf 'http-file-server "directory is missing or contains a symlink: ~a" root))
      (let* ([log (server-log)]
             [context
              (and (eq? protocol-version 'https)
                   (let ([ctx (make-tls-context 'server)])
                     (tls-context-load-cert! ctx (example-required 'https-file-server ref 'cert))
                     (tls-context-load-private-key!
                      ctx (example-required 'https-file-server ref 'key))
                     ctx))]
             [server (http-listen host port context)])
        (dynamic-wind
          (lambda ()
            (with-logger log
              (log-infof "serving ~a from ~a:~a" root host port)))
          (lambda ()
            (let loop ()
              ;; Accept one HTTP request so this example can resolve paths itself.
              (let ([connection (http-accept server)])
                (dynamic-wind
                  void
                  (lambda ()
                    (let* ([request (http-read-request connection)]
                           [response (file-server-response root request)])
                      ;; Send the static file response or the request error response.
                      (http-write-response connection response)))
                  (lambda () (http-connection-close connection))))
              (loop)))
          (lambda ()
            ;; Close the listening server after interruption or failure.
            (http-server-close server)
            (when context (close-tls-context context))
            (logger-close! log))))
      0)))
