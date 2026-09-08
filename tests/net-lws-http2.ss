(import (chezpp))

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
            (procedure
             (cadr ready)
             (lambda (command)
               (display command input)
               (newline input)
               (flush-output-port input)
               (if (eq? command 'stats) (read output) (void))))))
        (lambda ()
          (guard (ignored [else (void)])
            (display "stop\n" input)
            (flush-output-port input))
          (close-port input)
          (let* ([remaining (read output)] [diagnostic (get-string-all errors)])
            (close-port output)
            (close-port errors)
            (unless (and (eof-object? remaining)
                         (or (eof-object? diagnostic) (string=? "" diagnostic)))
              (errorf 'call-with-h2-fixture "unexpected fixture output: ~s ~s"
                      remaining diagnostic))))))))

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
