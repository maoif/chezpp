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
          (guard (ignored [else (void)]) (close-port input))
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
          (guard (ignored [else (void)]) (close-port input))
          (guard (ignored [else (void)]) (close-port output))
          (guard (ignored [else (void)]) (close-port errors)))))))

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
