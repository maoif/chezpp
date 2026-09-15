(import (chezpp))

(load "examples/net/net-example-common.ss")

(define parse-ssh-open-repl-arguments
  (lambda (arg*)
    (unless (>= (length arg*) 4)
      (errorf 'ssh-open-repl
              "expected host port user auth-kind arguments, given ~s"
              arg*))
    (let ([host (car arg*)]
          [port (parse-port-argument 'ssh-open-repl (cadr arg*))]
          [user (caddr arg*)]
          [auth-kind (cadddr arg*)]
          [rest (cddddr arg*)])
      (cond
       [(string=? auth-kind "agent")
        (unless (null? rest)
          (errorf 'ssh-open-repl
                  "agent authentication does not take an extra argument, given ~s"
                  rest))
        (values host port user auth-kind #f)]
       [(or (string=? auth-kind "password")
            (string=? auth-kind "publickey"))
        (unless (= (length rest) 1)
          (errorf 'ssh-open-repl
                  "authentication kind ~s requires exactly one secret/passphrase argument"
                  auth-kind))
        (values host port user auth-kind (car rest))]
       [else
        (errorf 'ssh-open-repl "invalid SSH auth kind ~s" auth-kind)]))))

#|proc:ssh-open-repl
The `ssh-open-repl` procedure opens an authenticated interactive shell on
`host:port` and forwards local terminal input/output to the remote shell.
|#
(define ssh-open-repl
  (lambda (host port user auth-kind auth-arg)
    (pcheck ([string? host user auth-kind] [fixnum? port])
      (let ([session (ssh-open host port user)]
            [stop? (vector #f)])
        (dynamic-wind
          void
          (lambda ()
            (ssh-authenticate! 'ssh-open-repl session user auth-kind auth-arg)
            (call-with-ssh-channel
             session
             (lambda (channel)
               (ssh-request-pty! channel)
               (ssh-shell channel)
               (call-with-example-output-ports
                (lambda (out err)
                  (let ([op (open-ssh-channel-output-port channel)]
                        [stdin-open? #t])
                    (let loop ([stdout-eof? #f] [stderr-eof? #f] [exit-status #f])
                      (let* ([stdout (if stdout-eof? #t (ssh-read/nonblocking channel 65536))]
                             [stderr (if stderr-eof? #t (ssh-read-stderr/nonblocking channel 65536))]
                             [stdout-eof* (or stdout-eof? (eof-object? stdout))]
                             [stderr-eof* (or stderr-eof? (eof-object? stderr))]
                             [status (or exit-status
                                         (guard (c [else #f])
                                           (let ([x (ssh-channel-exit-status channel)])
                                             (and (fixnum? x) (fx>= x 0) x))))]
                             [targets
                              (append
                               (if (net-would-block? stdout)
                                   (list (make-poll-target
                                          (net-would-block-resource stdout)
                                          (net-would-block-events stdout)))
                                   '())
                               (if (net-would-block? stderr)
                                   (list (make-poll-target
                                          (net-would-block-resource stderr)
                                          (net-would-block-events stderr)))
                                   '()))])
                        (when (bytevector? stdout) (put-bytevector out stdout) (flush-output-port out))
                        (when (bytevector? stderr) (put-bytevector err stderr) (flush-output-port err))
                        (when (and stdin-open? (char-ready? (current-input-port)))
                          (let ([ch (get-char (current-input-port))])
                            (if (eof-object? ch)
                                (begin (set! stdin-open? #f) (ssh-write-all channel #vu8()))
                                (begin
                                  (put-bytevector op (string->utf8 (string ch)))
                                  (flush-output-port op)))))
                        (cond
                         [(and stdout-eof* stderr-eof* status) (vector-set! stop? 0 #t) channel]
                         [else
                          (if (null? targets) (milisleep 5) (poll targets 100))
                          (loop stdout-eof* stderr-eof* status)])))))))))
          (lambda ()
            (ssh-close session)))))))

(let-values ([(host port user auth-kind auth-arg)
              (parse-ssh-open-repl-arguments (command-line-arguments))])
  (ssh-open-repl host port user auth-kind auth-arg))
