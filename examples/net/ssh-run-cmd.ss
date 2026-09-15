(import (chezpp))

(load "examples/net/net-example-common.ss")

(define parse-ssh-run-cmd-arguments
  (lambda (arg*)
    (unless (>= (length arg*) 5)
      (errorf 'ssh-run-cmd
              "expected host port user auth-kind and command arguments, given ~s"
              arg*))
    (let ([host (car arg*)]
          [port (parse-port-argument 'ssh-run-cmd (cadr arg*))]
          [user (caddr arg*)]
          [auth-kind (cadddr arg*)]
          [rest (cddddr arg*)])
      (cond
       [(string=? auth-kind "agent")
        (when (null? rest)
          (errorf 'ssh-run-cmd "missing remote command"))
        (values host port user auth-kind #f (join-command-arguments rest))]
       [(or (string=? auth-kind "password")
            (string=? auth-kind "publickey"))
        (when (< (length rest) 2)
          (errorf 'ssh-run-cmd
                  "authentication kind ~s requires a secret/passphrase and command"
                  auth-kind))
        (values host
                port
                user
                auth-kind
                (car rest)
                (join-command-arguments (cdr rest)))]
       [else
        (errorf 'ssh-run-cmd "invalid SSH auth kind ~s" auth-kind)]))))

#|proc:ssh-run-cmd
The `ssh-run-cmd` procedure authenticates to `host:port`, runs `cmd`, and
forwards the remote stdout and stderr streams to the local standard ports.
|#
(define ssh-run-cmd
  (lambda (host port user auth-kind auth-arg cmd)
    (pcheck ([string? host user auth-kind cmd] [fixnum? port])
      (let ([session (ssh-open host port user)])
        (dynamic-wind
          void
          (lambda ()
            (ssh-authenticate! 'ssh-run-cmd session user auth-kind auth-arg)
            (call-with-ssh-channel
             session
             (lambda (channel)
               (ssh-exec channel cmd)
               (call-with-example-output-ports
                (lambda (out err)
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
                      (cond
                       [(and stdout-eof* stderr-eof* status) status]
                       [else
                        (if (null? targets) (milisleep 5) (poll targets 100))
                        (loop stdout-eof* stderr-eof* status)]))))))))
          (lambda ()
            (ssh-close session)))))))

(let-values ([(host port user auth-kind auth-arg cmd)
              (parse-ssh-run-cmd-arguments (command-line-arguments))])
  (exit (ssh-run-cmd host port user auth-kind auth-arg cmd)))
