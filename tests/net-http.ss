(import (chezpp)
        (chezpp net)
        (chezpp net lws http1)
        (chezpp net lws http2)
        (chezpp net lws reactor)
        (chezpp net lws transport)
        (chezpp net http private)
        (chezpp net lws ffi))

(load "net-common.ss")

(define lws-http-available?
  (let ([status (lws-status)])
    (and (vector? status) (vector-ref status 0))))

;; These bounded loopback fixtures run against the supported LWS runtime on this host.
(define run-live-http1-tests? #t)

(define read-http-request-head
  (lambda (port)
    (let ([request-line (read-crlf-line port)])
      (let loop ([headers '()])
        (let ([line (read-crlf-line port)])
          (if (string=? line "")
              (values request-line (reverse headers))
              (let ([colon
                     (let find-colon ([index 0])
                       (cond
                        [(fx= index (string-length line))
                         (errorf 'read-http-request-head "header has no colon: ~s" line)]
                        [(char=? (string-ref line index) #\:) index]
                        [else (find-colon (fx1+ index))]))])
                (loop (cons (cons (substring line 0 colon)
                                  (string-trim (substring line (+ colon 1)
                                                          (string-length line))))
                            headers)))))))))

(define fixture-header-ref
  (lambda (headers name default)
    (let ([entry (find (lambda (entry) (string-ci=? (car entry) name)) headers)])
      (if entry (cdr entry) default))))

(define concatenate-bytevectors
  (lambda (bytevector*)
    (let ([result
           (make-bytevector
            (fold-left (lambda (length bytes) (+ length (bytevector-length bytes)))
                       0 bytevector*) 0)])
      (let loop ([rest bytevector*] [offset 0])
        (unless (null? rest)
          (let ([bytes (car rest)])
            (bytevector-copy! bytes 0 result offset (bytevector-length bytes))
            (loop (cdr rest) (+ offset (bytevector-length bytes))))))
      result)))

(define start-http1-fixture
  (lambda (handler)
    (let ([listener (open-socket 'inet 'stream)])
      (socket-set-option! listener 'reuse-address #t)
      (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
      (socket-listen! listener 4)
      (let ([port (socket-address-port (socket-local-address listener))])
        (values port
                (fork-thread
                 (lambda ()
                   (let-values ([(client peer) (socket-accept listener)])
                     (let ([input (open-socket-input-port client)]
                           [output (open-socket-output-port client)])
                       (dynamic-wind void
                         (lambda () (handler input output))
                         (lambda () (close-port input) (close-port output))))
                     (close-socket client)
                     (close-socket listener)))))))))

(define http-condition-message-contains?
  (lambda (fragment thunk)
    (guard (condition
            [else
             (string-contains?
              (call-with-string-output-port
               (lambda (port) (display-condition condition port)))
              fragment)])
      (thunk)
      #f)))

(define make-h2-test-request
  (case-lambda
    [(path) (make-h2-test-request path #f)]
    [(path deadline)
     (make-normalized-http-request
      "GET" #f 'http "h2.test" 80 #f path '() #f #f
      (make-http-request-policy '() #f #f #f 0 'h2 #f #f 0 deadline))]))

(define wait-for-reactor-commands
  (lambda (reactor)
    (let loop ([remaining 200])
      (cond
       [(zero? (vector-ref (lws-reactor-pool-metrics reactor) 1)) #t]
       [(zero? remaining) #f]
       [else
        (milisleep 1)
        (loop (fx1- remaining))]))))

(define inject-h2-event!
  (lambda (reactor tag connection-id stream-id generation status payload scope)
    (and (lws-reactor-inject-event!
          reactor tag connection-id stream-id generation status payload
          'http2 #f 0 #f scope)
         (wait-for-reactor-commands reactor))))

(define finish-h2-stream!
  (lambda (reactor operation connection-id stream-id generation status body)
    (and (inject-h2-event! reactor 'headers connection-id stream-id generation
                           status #vu8() 'none)
         (or (zero? (bytevector-length body))
             (inject-h2-event! reactor 'readable connection-id stream-id generation
                               0 body 'none))
         (begin
           (net-operation-step! operation)
           #t)
         (inject-h2-event! reactor 'complete connection-id stream-id generation
                           0 #vu8() 'stream)
         (begin
           (net-operation-step! operation)
           (eq? 'completed (net-operation-state operation))))))

(mat net-http-lws-http1-boundary
     (and (procedure? make-lws-http1-client)
          (procedure? lws-http1-client-close!)
          (procedure? lws-http1-request/nonblocking)
          (procedure? lws-http1-client-pool-metrics)))

(mat net-http-stream-body
     (let ([source (make-http-body-source (lambda (maximum-bytes) (eof-object)) #f)]
           [sink (make-http-body-sink (lambda (bytevector start count) (void)))])
       (and (http-body-source? source)
            (not (http-body-source-length source))
            (eof-object? (http-body-source-read source 65536))
            (http-body-sink? sink)
            (begin
              (http-body-sink-write! sink #vu8(1 2 3) 0 3)
              (http-body-sink-finish! sink)
              #t)))
     ;; Error case: a body source read size must be positive.
     (http-condition-message-contains?
      "positive"
      (lambda ()
      (http-body-source-read
         (make-http-body-source (lambda (maximum-bytes) (eof-object)) #f)
         0))))

(mat net-http-framing-validation
     ;; Error case: Content-Length and Transfer-Encoding must not be sent together.
     (http-condition-message-contains?
      "conflicting"
      (lambda ()
        (http-send/nonblocking
         (http-open)
         (make-http-request 'post "http://127.0.0.1/"
                            '(("Content-Length" . "1")
                              ("Transfer-Encoding" . "chunked"))
                            "x")))))

(mat net-http-fixed-response
     (or (not run-live-http1-tests?) (not lws-http-available?)
         (let-values ([(port thread)
                   (start-http1-fixture
                    (lambda (input output)
                      (read-http-request-head input)
                      (put-bytevector output
                       (string->utf8
                        "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nConnection: close\r\n\r\nhello"))
                      (flush-output-port output)))])
         (let ([client (http-open)])
         (dynamic-wind void
           (lambda ()
             (let ([response (http-get client
                                        (format "http://127.0.0.1:~a/fixed" port))])
               (and (= (http-response-status response) 200)
                    (eq? (http-response-version response) 'h1)
                    (equal? (utf8->string (http-response-body response)) "hello"))))
             (lambda () (http-close client) (thread-join thread)))))))

(mat net-http-segmented-chunked-response
     (or (not run-live-http1-tests?) (not lws-http-available?)
         (let-values ([(port thread)
                   (start-http1-fixture
                    (lambda (input output)
                      (read-http-request-head input)
                      (for-each
                       (lambda (part)
                         (put-bytevector output (string->utf8 part))
                         (flush-output-port output)
                         (milisleep 10))
                       '("HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n"
                         "Connection: close\r\n\r\n"
                         "4\r\nWiki\r\n5\r\npedia\r\n0\r\nX-End: yes\r\n\r\n"))))])
         (let ([client (http-open)])
         (dynamic-wind void
           (lambda ()
             (let ([response (http-get client
                                        (format "http://127.0.0.1:~a/chunked" port))])
               (and (= (http-response-status response) 200)
                    (equal? (utf8->string (http-response-body response)) "Wikipedia"))))
             (lambda () (http-close client) (thread-join thread)))))))

(mat net-http-complete-response-metadata
     (let-values ([(port thread)
                   (start-http1-fixture
                    (lambda (input output)
                      (read-http-request-head input)
                      (put-bytevector output
                        (string->utf8
                          (string-append
                            "HTTP/1.1 200 OK\r\nContent-Length: 1\r\nConnection: close\r\n"
                            "ETag: abc\r\nCache-Control: no-store\r\nX-Empty:\r\n"
                            "Set-Cookie: a=1\r\nSet-Cookie: b=2\r\n\r\nx")))
                      (flush-output-port output)))])
       (let ([client (http-open)])
         (dynamic-wind
           void
           (lambda ()
             (let* ([response (http-get client (format "http://127.0.0.1:~a/headers" port))]
                    [headers (http-response-headers response)])
               (and (equal? "abc" (http-header-ref headers "etag" #f))
                    (equal? "no-store" (http-header-ref headers "cache-control" #f))
                    (equal? "" (http-header-ref headers "x-empty" #f))
                    (equal? '("a=1" "b=2")
                            (map cdr (filter (lambda (entry)
                                               (string-ci=? (car entry) "set-cookie"))
                                             headers))))))
           (lambda () (http-close client) (thread-join thread))))))

(mat net-http-streaming-upload
     (or (not run-live-http1-tests?) (not lws-http-available?)
         (let ([received #f])
       (let-values ([(port thread)
                     (start-http1-fixture
                      (lambda (input output)
                        (let-values ([(request-line headers)
                                      (read-http-request-head input)])
                          (let ([length (string->number
                                         (fixture-header-ref headers "Content-Length" "0"))])
                            (set! received (get-bytevector-n input length))))
                        (put-bytevector output
                         (string->utf8
                          "HTTP/1.1 204 No Content\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"))
                        (flush-output-port output)))])
           (let ([client (http-open)] [payload (string->utf8 "upload-body")])
           (dynamic-wind void
             (lambda ()
               (let ([response
                      (http-send client
                       (make-http-request 'put
                        (format "http://127.0.0.1:~a/upload" port)
                        `(("Content-Length" . ,(number->string
                                                (bytevector-length payload))))
                        payload))])
                 (and (= (http-response-status response) 204)
                      (equal? received payload))))
               (lambda () (http-close client) (thread-join thread))))))))

(mat net-http-response-sink
     (or (not run-live-http1-tests?) (not lws-http-available?)
         (let ([chunks '()] [finished 0])
       (let-values ([(port thread)
                     (start-http1-fixture
                      (lambda (input output)
                        (read-http-request-head input)
                        (put-bytevector output
                         (string->utf8
                          "HTTP/1.1 200 OK\r\nContent-Length: 4\r\nConnection: close\r\n\r\ndata"))
                        (flush-output-port output)))])
           (let ([client (http-open)]
               [sink (make-http-body-sink
                      (lambda (bytes start count)
                        (let ([copy (make-bytevector count 0)])
                          (bytevector-copy! bytes start copy 0 count)
                          (set! chunks (cons copy chunks))))
                      (lambda () (set! finished (+ finished 1))))])
           (dynamic-wind void
             (lambda ()
               (let ([response
                      (net-operation-wait
                       (http-send/nonblocking client
                        (make-http-request 'get
                         (format "http://127.0.0.1:~a/sink" port)) sink))])
                 (and (not (http-response-body response))
                      (= finished 1)
                      (equal? (utf8->string
                               (concatenate-bytevectors (reverse chunks))) "data"))))
             (lambda () (http-close client) (thread-join thread))))))))

(mat net-http-http1-explicit-nonreuse
     (or (not run-live-http1-tests?) (not lws-http-available?)
         (let ([listener (open-socket 'inet 'stream)] [accept-count 0])
           (socket-set-option! listener 'reuse-address #t)
           (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
           (socket-listen! listener 4)
           (let* ([port (socket-address-port (socket-local-address listener))]
                  [thread
                   (fork-thread
                    (lambda ()
                      (let loop ([remaining 2])
                        (unless (fxzero? remaining)
                          (let-values ([(socket peer) (socket-accept listener)])
                            (set! accept-count (fx1+ accept-count))
                            (let ([input (open-socket-input-port socket)]
                                  [output (open-socket-output-port socket)])
                              (dynamic-wind
                                void
                                (lambda ()
                                  (read-http-request-head input)
                                  (put-bytevector
                                   output
                                   (string->utf8
                                    "HTTP/1.1 204 No Content\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"))
                                  (flush-output-port output))
                                (lambda () (close-port input) (close-port output))))
                            (close-socket socket))
                          (loop (fx1- remaining))))
                      (close-socket listener)))])
             (let ([client (http-open)]
                   [uri (format "http://127.0.0.1:~a/nonreuse" port)])
               (dynamic-wind
                 void
                 (lambda ()
                   (and (= 204 (http-response-status (http-get client uri)))
                        (= 204 (http-response-status (http-get client uri)))
                        (begin (thread-join thread) #t)
                        (= accept-count 2)))
                 (lambda () (http-close client))))))))

(mat net-http-transport-header-roundtrip
     (let ([headers '(("set-cookie" . "one=1") ("set-cookie" . "two=2") ("x-empty" . ""))])
       (and (equal? headers
                    (lws-transport-decode-headers (lws-transport-encode-headers headers)))
            (null? (lws-transport-decode-headers #vu8())))))

(mat net-http-transport-rejects-incomplete-headers
     ;; Error cases: unterminated name/value, an incomplete suffix, and an empty name.
     (for-all
      (lambda (bytes)
        (guard (condition [(net-error? condition) #t] [else #f])
          (lws-transport-decode-headers bytes)
          #f))
      (list #vu8(120) #vu8(120 0) #vu8(120 0 121)
            #vu8(120 0 121 0 122) #vu8(0 121 0))))

(mat net-http-transport-rejects-ambiguous-headers
     ;; Error cases: empty names and embedded NULs cannot roundtrip through native metadata.
     (for-all
      (lambda (headers)
        (guard (condition [(net-error? condition) #t] [else #f])
          (lws-transport-encode-headers headers)
          #f))
      (list '(("" . "value"))
            (list (cons "x" (string #\nul)))
            (list (cons (string #\nul) "value")))))

(mat net-http-transport-command-rejection
     ;; Error case: an unserviced full command queue must reject upload and consumption.
     (let ([reactor (make-lws-reactor 8 64 1)])
       (dynamic-wind
         void
         (lambda ()
           (and (lws-reactor-client-acquire! reactor 1 1 1)
                (guard (condition [(net-error? condition) #t] [else #f])
                  (lws-transport-submit-body! reactor 1 1 1 #vu8(1) #f)
                  #f)
                (guard (condition [(net-error? condition) #t] [else #f])
                  (lws-transport-submit-body! reactor 1 1 1 #vu8() #t)
                  #f)
                (guard (condition [(net-error? condition) #t] [else #f])
                  (lws-transport-consume-body! reactor 1 1 1 1)
                  #f)))
         (lambda () (lws-reactor-shutdown! reactor)))))

(mat net-http2-rejects-truncated-metadata
     ;; Error case: a missing header terminator must fail, even with completion already queued.
     (let* ([client (make-lws-http2-client 32 65536 32 0 10 #f)]
            [reactor (lws-http2-client-reactor client)]
            [operation (lws-http2-request/nonblocking
                        client (make-h2-test-request "/truncated") #f)])
       (dynamic-wind
         void
         (lambda ()
           (and (wait-for-reactor-commands reactor)
                (inject-h2-event! reactor 'headers 1 1 1 200 #vu8(120 0 121) 'none)
                (inject-h2-event! reactor 'complete 1 1 1 0 #vu8() 'stream)
                (begin
                  (net-operation-step! operation)
                  (and (eq? 'failed (net-operation-state operation))
                       (net-error? (net-operation-condition operation))))))
         (lambda () (lws-http2-client-close! client)))))

(mat net-http2-direct-transport-identity
     (let* ([client (make-lws-http2-client 32 65536 32 0 10 #f)]
            [reactor (lws-http2-client-reactor client)]
            [operation (lws-http2-request/nonblocking
                        client (make-h2-test-request "/identity") #f)])
       (dynamic-wind
         void
         (lambda ()
           (and (wait-for-reactor-commands reactor)
                (inject-h2-event! reactor 'connected 1 1 1 -2000 #vu8() 'none)
                (begin (net-operation-step! operation) #t)
                (finish-h2-stream! reactor operation 1 1 1 200 #vu8(111 107))
                (let ([response (net-operation-result operation)])
                  (and (transport-response? response)
                       (= 1 (transport-response-connection-id response))
                       (eq? 'h2 (transport-response-version response))
                       (equal? #vu8(111 107) (transport-response-body response))))))
         (lambda () (lws-http2-client-close! client)))))

(mat net-http2-preserves-headers-and-trailers
     (let* ([client (make-lws-http2-client 32 65536 32 0 10 #f)]
            [reactor (lws-http2-client-reactor client)]
            [headers '(("set-cookie" . "one=1") ("set-cookie" . "two=2"))]
            [trailers '(("x-checksum" . "verified"))]
            [operation (lws-http2-request/nonblocking
                        client (make-h2-test-request "/metadata") #f)])
       (dynamic-wind
         void
         (lambda ()
           (and (wait-for-reactor-commands reactor)
                (inject-h2-event! reactor 'headers 1 1 1 200
                                  (lws-transport-encode-headers headers) 'none)
                (inject-h2-event! reactor 'headers 1 1 1 -1
                                  (lws-transport-encode-headers trailers) 'none)
                (inject-h2-event! reactor 'complete 1 1 1 0 #vu8() 'stream)
                (begin
                  (net-operation-step! operation)
                  (and (eq? 'completed (net-operation-state operation))
                       (let ([response (net-operation-result operation)])
                         (and (equal? headers (transport-response-headers response))
                              (equal? trailers (transport-response-trailers response))))))))
         (lambda () (lws-http2-client-close! client)))))

(mat net-http2-rejects-truncated-trailers-before-finishing-sink
     ;; Error case: malformed trailers prevent completion and suppress the sink finish callback.
     (let* ([client (make-lws-http2-client 32 65536 32 0 10 #f)]
            [reactor (lws-http2-client-reactor client)]
            [finished 0]
            [operation (lws-http2-request/nonblocking
                        client (make-h2-test-request "/bad-trailers")
                        (vector (lambda (bytes start count) (void))
                                (lambda () (set! finished (fx1+ finished)))))])
       (dynamic-wind
         void
         (lambda ()
           (and (wait-for-reactor-commands reactor)
                (inject-h2-event! reactor 'headers 1 1 1 200 #vu8() 'none)
                (inject-h2-event! reactor 'headers 1 1 1 -1 #vu8(120 0 121) 'none)
                (inject-h2-event! reactor 'complete 1 1 1 0 #vu8() 'stream)
                (begin
                  (net-operation-step! operation)
                  (and (eq? 'failed (net-operation-state operation))
                       (net-error? (net-operation-condition operation))
                       (fxzero? finished)))))
         (lambda () (lws-http2-client-close! client)))))

(mat net-http2-independent-streams-all-wait-orders
     (let* ([client (make-lws-http2-client 64 65536 64 0 10 #f)]
            [reactor (lws-http2-client-reactor client)]
            [first (lws-http2-request/nonblocking client (make-h2-test-request "/one") #f)]
            [second (lws-http2-request/nonblocking client (make-h2-test-request "/two") #f)])
       (dynamic-wind
         void
         (lambda ()
           (and (wait-for-reactor-commands reactor)
                (inject-h2-event! reactor 'connected 1 1 1 -2000 #vu8() 'none)
                (begin (net-operation-step! first) #t)
                (wait-for-reactor-commands reactor)
                (finish-h2-stream! reactor second 1 2 2 202 #vu8(2))
                (finish-h2-stream! reactor first 1 1 1 201 #vu8(1))
                (= 201 (transport-response-status (net-operation-result first)))
                (= 202 (transport-response-status (net-operation-result second)))
                (= (transport-response-connection-id (net-operation-result first))
                   (transport-response-connection-id (net-operation-result second)))))
         (lambda () (lws-http2-client-close! client)))))

(mat net-http2-local-stream-bound-fifo
     (let* ([client (make-lws-http2-client 64 65536 64 0 1 #f)]
            [reactor (lws-http2-client-reactor client)]
            [first (lws-http2-request/nonblocking client (make-h2-test-request "/first") #f)]
            [second (lws-http2-request/nonblocking client (make-h2-test-request "/second") #f)])
       (dynamic-wind
         void
         (lambda ()
           (and (wait-for-reactor-commands reactor)
                (inject-h2-event! reactor 'connected 1 1 1 -2000 #vu8() 'none)
                (begin (net-operation-step! first) #t)
                (eq? 'pending (net-operation-state second))
                (finish-h2-stream! reactor first 1 1 1 200 #vu8())
                (begin (net-operation-step! second) #t)
                (wait-for-reactor-commands reactor)
                (finish-h2-stream! reactor second 1 2 2 200 #vu8())
                (= 1 (transport-response-connection-id
                      (net-operation-result second)))))
         (lambda () (lws-http2-client-close! client)))))

(mat net-http2-queued-deadline
     ;; Error case: a request waiting for H2 admission must time out without cancelling its leader.
     (let* ([client (make-lws-http2-client 32 65536 32 0 1 #f)]
            [first (lws-http2-request/nonblocking client (make-h2-test-request "/leader") #f)]
            [second (lws-http2-request/nonblocking client (make-h2-test-request "/queued" 0) #f)])
       (dynamic-wind
         void
         (lambda ()
           (net-operation-step! second)
           (and (eq? 'failed (net-operation-state second))
                (net-error? (net-operation-condition second))
                (eq? 'timeout (net-error-kind (net-operation-condition second)))
                (eq? 'pending (net-operation-state first))))
         (lambda () (lws-http2-client-close! client)))))

(mat net-http2-required-protocol-refusal
     ;; Error case: an H2 request must fail when LWS observes HTTP/1.
     (let* ([client (make-lws-http2-client 32 65536 32 0 10 #f)]
            [reactor (lws-http2-client-reactor client)]
            [operation (lws-http2-request/nonblocking
                        client (make-h2-test-request "/refuse") #f)])
       (dynamic-wind
         void
         (lambda ()
           (and (wait-for-reactor-commands reactor)
                (lws-reactor-inject-event!
                 reactor 'connected 1 1 1 200 #vu8() 'http1 #f 0 #f 'none)
                (wait-for-reactor-commands reactor)
                (lws-reactor-inject-event!
                 reactor 'complete 1 1 1 0 #vu8() 'http1 #f 0 #f 'stream)
                (wait-for-reactor-commands reactor)
                (begin (net-operation-step! operation) #t)
                (eq? 'failed (net-operation-state operation))))
         (lambda () (lws-http2-client-close! client)))))

(mat net-http2-stream-and-connection-failure-scope
     (let* ([client (make-lws-http2-client 64 65536 64 0 10 #f)]
            [reactor (lws-http2-client-reactor client)]
            [first (lws-http2-request/nonblocking client (make-h2-test-request "/reset") #f)]
            [second (lws-http2-request/nonblocking client (make-h2-test-request "/ok") #f)])
       (dynamic-wind
         void
         (lambda ()
           (and (wait-for-reactor-commands reactor)
                (inject-h2-event! reactor 'connected 1 1 1 -2000 #vu8() 'none)
                (begin (net-operation-step! first) #t)
                (wait-for-reactor-commands reactor)
                (inject-h2-event! reactor 'reset 1 1 1 8 #vu8() 'stream)
                (begin (net-operation-step! first) #t)
                (eq? 'failed (net-operation-state first))
                (finish-h2-stream! reactor second 1 2 2 200 #vu8(2))))
         (lambda () (lws-http2-client-close! client))))
     (let* ([client (make-lws-http2-client 64 65536 64 0 10 #f)]
            [reactor (lws-http2-client-reactor client)]
            [first (lws-http2-request/nonblocking client (make-h2-test-request "/failed") #f)]
            [second (lws-http2-request/nonblocking client (make-h2-test-request "/sibling") #f)])
       (dynamic-wind
         void
         (lambda ()
           (and (wait-for-reactor-commands reactor)
                (inject-h2-event! reactor 'connected 1 1 1 -2000 #vu8() 'none)
                (begin (net-operation-step! first) #t)
                (wait-for-reactor-commands reactor)
                (inject-h2-event! reactor 'failed 1 1 1 111 #vu8() 'connection)
                (begin (net-operation-step! first) #t)
                (begin (net-operation-step! second) #t)
                (eq? 'failed (net-operation-state first))
                (eq? 'failed (net-operation-state second))))
         (lambda () (lws-http2-client-close! client)))))

(mat net-http2-cancellation-and-stale-generation
     (let* ([client (make-lws-http2-client 64 65536 64 0 1 #f)]
            [reactor (lws-http2-client-reactor client)]
            [first (lws-http2-request/nonblocking client (make-h2-test-request "/cancel") #f)]
            [second (lws-http2-request/nonblocking client (make-h2-test-request "/next") #f)])
       (dynamic-wind
         void
         (lambda ()
           (and (wait-for-reactor-commands reactor)
                (inject-h2-event! reactor 'connected 1 1 1 -2000 #vu8() 'none)
                (begin (net-operation-step! first) #t)
                (begin (net-operation-cancel! first) #t)
                (eq? 'cancelled (net-operation-state first))
                (wait-for-reactor-commands reactor)
                (begin (net-operation-step! second) #t)
                (wait-for-reactor-commands reactor)
                (lws-reactor-inject-event!
                 reactor 'complete 1 1 1 0 #vu8(9) 'http2 #f 0 #f 'stream)
                (wait-for-reactor-commands reactor)
                (eq? 'pending (net-operation-state second))
                (finish-h2-stream! reactor second 1 2 2 200 #vu8(2))))
         (lambda () (lws-http2-client-close! client)))))
