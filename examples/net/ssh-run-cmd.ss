(import (chezpp) (chezpp logging))
(load "examples/common.ss")
(load "examples/net/net-example-common.ss")

(define ssh-run-cmd
  (lambda (host port user auth-kind auth-arg command)
    (let ([session (ssh-open host port user)]
          [log (make-logger 'ssh-run-cmd)])
      (logger-add-sink! log (make-log-port-sink 'stderr (current-error-port)))
      (dynamic-wind
        void
        (lambda ()
          (with-logger log
            (log-infof "opening SSH command session to ~a:~a" host port))
          ;; Authenticate the SSH session using the selected credential.
          (ssh-authenticate! 'ssh-run-cmd session user auth-kind auth-arg)
          ;; Execute the remote command and forward both output streams.
          (call-with-ssh-channel
           session
           (lambda (channel)
             (ssh-exec channel command)
             (call-with-example-output-ports
              (lambda (out err)
                (let loop ()
                  (let ([stdout (ssh-read/nonblocking channel 65536)]
                        [stderr (ssh-read-stderr/nonblocking channel 65536)])
                    (when (bytevector? stdout) (put-bytevector out stdout))
                    (when (bytevector? stderr) (put-bytevector err stderr))
                    (flush-output-port out)
                    (flush-output-port err)
                    (unless (and (eof-object? stdout) (eof-object? stderr))
                      (milisleep 5)
                      (loop)))))))))
        (lambda ()
          (logger-close! log)
          (ssh-close session))))))

(example-run!
 'ssh-run-cmd
 "Run a command over SSH."
 '(host port user auth command)
 (lambda (ref)
   (let* ([host (example-required 'ssh-run-cmd ref 'host)]
          [port (parse-port-argument
                 'ssh-run-cmd (example-required 'ssh-run-cmd ref 'port))]
          [user (example-required 'ssh-run-cmd ref 'user)]
          [auth (or (ref 'auth) "agent")]
          [secret (or (getenv "CHEZPP_SSH_PASSWORD")
                      (getenv "CHEZPP_SSH_PASSPHRASE"))]
          [command (example-required 'ssh-run-cmd ref 'command)])
     (ssh-run-cmd host port user auth secret command)))
 (command-line-arguments))
