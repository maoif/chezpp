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

(define exchange-http2!
  (lambda (sender receiver)
    (let loop ([bytes (http2-send sender)])
      (when bytes
        (http2-receive receiver bytes 0 (bytevector-length bytes))
        (loop (http2-send sender))))))

(mat net-http2-peer-settings
     (let ([client (http2-open 'client)]
           [server (http2-open 'server)])
       (dynamic-wind
         void
         (lambda ()
           (exchange-http2! client server)
           (exchange-http2! server client)
           (let ([limit (http2-peer-max-concurrent-streams client)])
             (let loop ([event (http2-next-event client)] [saw-settings? #f])
               (if event
                   (loop (http2-next-event client)
                         (or saw-settings? (= 6 (vector-ref event 0))))
                   (and (natural? limit) (positive? limit) saw-settings?)))))
         (lambda ()
           (http2-close client)
           (http2-close server)))))

(define collect-http2-headers
  (lambda (session stream-id)
    (let loop ([headers '()])
      (let ([event (http2-next-event session)])
        (cond
         [(not event) (reverse headers)]
         [(and (= (vector-ref event 0) 1) (= (vector-ref event 1) stream-id))
          (let ([header (vector-ref event 3)])
            (loop (cons (cons (vector-ref header 0) (vector-ref header 1)) headers)))]
         [else (loop headers)])))))

(mat net-http2-request-response-values
     ;; Request pseudo-headers and response status must come from caller values.
     (let ([client (http2-open 'client)]
           [server (http2-open 'server)])
       (dynamic-wind
         void
         (lambda ()
           (exchange-http2! client server)
           (exchange-http2! server client)
           (let ([stream-id
                  (http2-submit-request client "POST" "https" "example.test" "/upload"
                                        '(("x-request-id" . "42")) #vu8())])
             (exchange-http2! client server)
             (let ([request-headers (collect-http2-headers server stream-id)])
               (http2-submit-response server stream-id 201
                                      '(("content-type" . "text/plain")) #vu8())
               (exchange-http2! server client)
               (let ([response-headers (collect-http2-headers client stream-id)])
                 (and (equal? (cdr (assoc ":method" request-headers)) "POST")
                      (equal? (cdr (assoc ":authority" request-headers)) "example.test")
                      (equal? (cdr (assoc ":path" request-headers)) "/upload")
                      (equal? (cdr (assoc ":status" response-headers)) "201"))))))
         (lambda ()
           (http2-close client)
           (http2-close server)))))

(mat net-http2-goaway
     ;; GOAWAY rejects later streams while allowing an eligible submitted stream to finish.
     (let ([client (http2-open 'client)]
           [server (http2-open 'server)])
       (dynamic-wind
         void
         (lambda ()
           (exchange-http2! client server)
           (exchange-http2! server client)
           (let ([stream-id
                  (http2-submit-request
                   client "GET" "https" "example.test" "/before-goaway" '() #vu8())])
             (exchange-http2! client server)
             (http2-goaway! server stream-id 0)
             (http2-submit-response server stream-id 200 '() (string->utf8 "complete"))
             (exchange-http2! server client)
             (let loop ([event (http2-next-event client)]
                        [saw-goaway? #f]
                        [saw-status? #f])
               (if event
                   (loop
                    (http2-next-event client)
                    (or saw-goaway?
                        (and (= 5 (vector-ref event 0))
                             (= stream-id (vector-ref event 1))))
                    (or saw-status?
                        (and (= 1 (vector-ref event 0))
                             (= stream-id (vector-ref event 1))
                             (string=? ":status"
                                       (vector-ref (vector-ref event 3) 0))
                             (string=? "200"
                                       (vector-ref (vector-ref event 3) 1)))))
                   (and saw-goaway?
                        saw-status?
                        ;; Error case: nghttp2 must reject a stream submitted after GOAWAY.
                        (guard (condition
                                [(net-error? condition) #t]
                                [else #f])
                          (http2-submit-request
                           client "GET" "https" "example.test" "/too-late" '() #vu8())
                          #f))))))
         (lambda ()
           (http2-close client)
           (http2-close server)))))
