(import (chezpp)
        (chezpp net)
        (chezpp net lws http1)
        (chezpp net lws ffi))

(load "net-common.ss")

(define lws-http-available?
  (let ([status (lws-status)])
    (and (vector? status) (vector-ref status 0))))

;; Live HTTP tests stay opt-in until the fixture can run against every supported LWS build.
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
