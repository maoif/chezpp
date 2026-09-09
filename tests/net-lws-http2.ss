(import (chezpp)
        (chezpp net lws http2)
        (chezpp net lws ffi)
        (chezpp net http private))

(load "net-common.ss")

;; TLS fixture coverage is opt-in because LWS builds differ in client trust/SNI behavior.
(define run-live-h2-tls-tests?
  (guard (condition [else #f])
    (let ([status (lws-status)])
      (and (vector-ref status 0)
           (lws-capability? (vector-ref status 1) lws-cap-http2)
           (lws-capability? (vector-ref status 1) lws-cap-tls)))))

(define call-with-h2-fixture
  (lambda (maximum-streams procedure)
    (let-values ([(input output errors pid)
                  (open-process-ports
                   (format "timeout 15s ./lws-http2-fixture 0 ~a" maximum-streams)
                   (buffer-mode block) (native-transcoder))])
      (dynamic-wind
        void
        (lambda ()
          (let ([ready (read output)])
            (unless (and (list? ready) (= 2 (length ready)) (eq? 'ready (car ready)))
              (errorf 'call-with-h2-fixture "fixture did not become ready: ~s" ready))
            (procedure (cadr ready)
                       (lambda (command)
                         (display command input) (newline input)
                         (flush-output-port input)
                         (if (eq? command 'stats) (read output) (void))))))
        (lambda ()
          (guard (ignored [else (void)])
            (display "stop\n" input) (flush-output-port input))
          (close-port input)
          (let* ([remaining (read output)] [diagnostic (get-string-all errors)])
            (close-port output) (close-port errors)
            (unless (and (eof-object? remaining)
                         (or (eof-object? diagnostic) (string=? "" diagnostic)))
              (errorf 'call-with-h2-fixture "unexpected fixture output: ~s ~s"
                      remaining diagnostic))))))))

(define call-with-h2-tls-fixture
  (lambda (maximum-streams procedure)
    (write-bytevector-file "/tmp/chezpp-net-test-cert.pem" tls-test-san-certificate)
    (write-bytevector-file "/tmp/chezpp-net-test-key.pem" tls-test-san-private-key)
    (let-values ([(input output errors pid)
                  (open-process-ports
                   (format "timeout 15s ./lws-http2-fixture 0 ~a /tmp/chezpp-net-test-cert.pem /tmp/chezpp-net-test-key.pem"
                           maximum-streams)
                   (buffer-mode block) (native-transcoder))])
      (dynamic-wind
        void
        (lambda ()
          (let ([ready (read output)])
            (unless (and (list? ready) (= 2 (length ready)) (eq? 'ready (car ready)))
              (errorf 'call-with-h2-tls-fixture "fixture did not become ready: ~s" ready))
            (procedure (cadr ready)
                       (lambda (command)
                         (display command input) (newline input)
                         (flush-output-port input)
                         (if (eq? command 'stats) (read output) (void))))))
        (lambda ()
          (guard (ignored [else (void)])
            (display "stop\n" input) (flush-output-port input))
          (close-port input) (close-port output) (close-port errors))))))

(define await-h2-streams
  (lambda (operations command count)
    (let loop ([remaining 500])
      (for-each
       (lambda (operation)
         (when (eq? 'pending (net-operation-state operation)) (net-operation-step! operation)))
       operations)
      (let ([stats (command 'stats)])
        (cond
         [(and (list? stats) (= (list-ref stats 4) count)) stats]
         [(fxzero? remaining) #f]
       [else (milisleep 1) (loop (fx1- remaining))])))))

(define make-h2-tls-test-request
  (lambda (path port)
    (make-normalized-http-request
     "GET" #f 'https "127.0.0.1" port #t path '() #f #f
     (make-http-request-policy '() #f #f #f 0 'h2 #f #f 0
       (let ([now (current-time 'time-monotonic)])
         (+ 3000 (* 1000 (time-second now))
            (quotient (time-nanosecond now) 1000000)))))))

(mat net-lws-http2-live-prior-knowledge
     (call-with-h2-fixture
      10
      (lambda (port command)
        (let ([client (http-open)])
          (dynamic-wind
            void
            (lambda ()
              (http-client-version-set! client 'h2)
              (http-set-timeout! client 2000)
              (let ([response (http-get client (format "http://127.0.0.1:~a/" port))])
                (and (eq? 'h2 (http-response-version response))
                     (= 200 (http-response-status response))
                     (equal? "xxxxxxxxxx" (utf8->string (http-response-body response)))
                     (= 1 (cadr (command 'stats))))))
            (lambda () (http-close client)))))))

(mat net-lws-http2-live-completion-survives-close
     (call-with-h2-fixture
      10
      (lambda (port command)
        (let ([client (http-open)] [received 0])
          (dynamic-wind
            void
            (lambda ()
              (http-client-version-set! client 'h2)
              (http-set-timeout! client 2000)
              (let ([response
                     (net-operation-wait
                      (http-send/nonblocking
                       client (make-http-request 'get (format "http://127.0.0.1:~a/" port))
                       (make-http-body-sink
                        (lambda (bytes start count)
                          (milisleep 50)
                          (set! received (+ received count))))))])
                (and (= received 10) (= 200 (http-response-status response)))))
            (lambda () (http-close client)))))))

(mat net-lws-http2-live-concurrent-streams
     (call-with-h2-fixture
      2
      (lambda (port command)
        (let ([client (http-open)])
          (dynamic-wind
            void
            (lambda ()
              (http-client-version-set! client 'h2)
              (http-set-timeout! client 3000)
              (let* ([first (http-send/nonblocking
                             client (make-http-request 'get
                                                       (format "http://127.0.0.1:~a/hold" port)) #f)]
                     [second (http-send/nonblocking
                              client (make-http-request 'get
                                                        (format "http://127.0.0.1:~a/hold" port)) #f)])
                (let ([started (await-h2-streams (list first second) command 2)])
                  (command 'release)
                  (let ([responses (map net-operation-wait (list first second))])
                    (and started
                         (= 2 (length responses))
                         (for-all (lambda (response)
                                   (= 200 (http-response-status response)))
                                  responses)
                         (= 1 (cadr started)))))))
            (lambda () (http-close client)))))))

(mat net-lws-http2-live-flow-control
     ;; A large response must cross repeated writable/acknowledgement boundaries intact.
     (call-with-h2-fixture
      4
      (lambda (port command)
        (let ([client (http-open)] [received 0])
          (dynamic-wind
            void
            (lambda ()
              (http-client-version-set! client 'h2)
              (http-set-timeout! client 4000)
              (let ([response
                     (net-operation-wait
                      (http-send/nonblocking
                       client (make-http-request 'get
                         (format "http://127.0.0.1:~a/large" port))
                       (make-http-body-sink
                        (lambda (bytes start count)
                          (set! received (+ received count))))))])
                (and (= 200 (http-response-status response))
                     (= 262144 received))))
            (lambda () (http-close client)))))))

(mat net-lws-http2-live-tls-alpn
     ;; A trusted localhost certificate must negotiate H2 through the LWS ALPN path.
     (if (not run-live-h2-tls-tests?)
         #t
         (call-with-h2-tls-fixture
          4
          (lambda (port command)
            (let ([tls (make-tls-context 'client)] [client #f])
              (dynamic-wind
                void
                (lambda ()
                  (tls-context-load-ca-file! tls "/tmp/chezpp-net-test-cert.pem")
                  (tls-context-set-verify! tls #t)
                  (set! client (make-lws-http2-client 64 65536 64
                                                        (tls-context-native-handle tls)
                                                        4))
                  (let ([response
                         (net-operation-wait
                          (lws-http2-request/nonblocking
                           client (make-h2-tls-test-request "/" port) #f))])
                    (and (= 200 (transport-response-status response))
                         (eq? 'h2 (transport-response-version response)))))
                (lambda ()
                  (when client (lws-http2-client-close! client))
                  (close-tls-context tls))))))))
