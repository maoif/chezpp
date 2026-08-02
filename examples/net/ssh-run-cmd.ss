(import (chezpp))

(load "examples/net-example-common.ss")

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
                  (let ([ip (open-ssh-channel-input-port channel)]
                        [ep (open-ssh-channel-error-port channel)])
                    (let ([stdout-thread
                           (fork-thread
                            (lambda ()
                              (pump-binary-input-port! ip out)))]
                          [stderr-thread
                           (fork-thread
                            (lambda ()
                              (pump-binary-input-port! ep err)))])
                      (thread-join stdout-thread)
                      (thread-join stderr-thread)
                      (ssh-channel-exit-status channel))))))))
          (lambda ()
            (ssh-close session)))))))

(let-values ([(host port user auth-kind auth-arg cmd)
              (parse-ssh-run-cmd-arguments (command-line-arguments))])
  (exit (ssh-run-cmd host port user auth-kind auth-arg cmd)))
