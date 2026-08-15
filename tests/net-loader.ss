(import (chezpp))

(define check-unavailable
  (lambda (name expected)
    (let ([info (optional-library-info name)])
      (and (optional-library-info? info)
           (if (optional-library-available? info)
               (or (not (optional-library-version info))
                   (string? (optional-library-version info)))
               (and (string? (optional-library-error info))
                    (string-contains? (optional-library-error info) expected)))))))

(mat optional-library-record
     (let ([info (optional-library-info 'openssl)])
       (and (optional-library-info? info)
            (eq? (optional-library-name info) 'openssl)
            (boolean? (optional-library-available? info))
            (or (not (optional-library-version info))
                (string? (optional-library-version info)))
            (list? (optional-library-capabilities info))
            (or (not (optional-library-error info))
                (string? (optional-library-error info))))))

(mat net-loader-errors
     (check-unavailable 'curl "curl")
     (check-unavailable 'ssh "ssh")
     (check-unavailable 'websockets "websockets")
     (check-unavailable 'grpc "grpc")
     (check-unavailable 'zlib "zlib")
     (check-unavailable 'nghttp2 "nghttp2"))

(mat net-http2-session-adapter
     ;; The dynamically loaded adapter must exchange the client preface and settings.
     (let ([client (http2-open 'client)]
           [server (http2-open 'server)])
       (dynamic-wind
         void
         (lambda ()
           (let drain-client ([bytes (http2-send client)])
             (if (not bytes)
                 (let drain-server ([settings (http2-send server)])
                   (if (not settings)
                       #t
                       (and (= (http2-receive client settings 0 (bytevector-length settings))
                               (bytevector-length settings))
                            (drain-server (http2-send server)))))
                 (and (= (http2-receive server bytes 0 (bytevector-length bytes))
                         (bytevector-length bytes))
                      (drain-client (http2-send client))))))
         (lambda ()
           (http2-close client)
           (http2-close server)))))
