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
     (check-unavailable 'nghttp2 "nghttp2")
     (check-unavailable 'cares "c-ares")
     (check-unavailable 'idn2 "libidn2"))
