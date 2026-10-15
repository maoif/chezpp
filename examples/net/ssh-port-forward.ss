(import (chezpp) (chezpp logging))
(load "examples/common.ss")
(load "examples/net/net-example-common.ss")

(define open-forwarding!
  (lambda (session direction listen-host listen-port target-host target-port log)
    (case direction
      ((local)
       ;; Open a direct SSH channel to the target service.
       (let ((forwarding (ssh-open-local-forward session target-host target-port
                                                 listen-host listen-port)))
         (with-logger log
           (log-infof "local forwarding channel opened to ~a:~a"
                      target-host target-port))
         forwarding))
      ((remote)
       ;; Ask the SSH server to listen on the requested address and port.
       (let ((listener (ssh-request-remote-forward! session listen-host listen-port)))
         (with-logger log
           (log-infof "remote forwarding listener opened on ~a:~a"
                      (ssh-forwarding-address listener)
                      (ssh-forwarding-port listener)))
         listener))
      (else
       (errorf 'ssh-port-forward "direction must be local or remote")))))

(example-run!
 'ssh-port-forward
 "Open an SSH forwarding channel in local or remote direction."
 '(host port user auth direction listen-host listen-port target-host target-port)
 (lambda (ref)
   (let* ((host (example-required 'ssh-port-forward ref 'host))
          (port (example-option-natural 'ssh-port-forward ref 'port 22))
          (user (example-required 'ssh-port-forward ref 'user))
          (auth (or (ref 'auth) "agent"))
          (direction (string->symbol
                      (example-required 'ssh-port-forward ref 'direction)))
          (listen-host (or (ref 'listen-host) "127.0.0.1"))
          (listen-port (example-option-natural 'ssh-port-forward ref 'listen-port 0))
          (target-host (or (ref 'target-host) "127.0.0.1"))
          (target-port (example-option-natural 'ssh-port-forward ref 'target-port 22))
          (secret (or (getenv "CHEZPP_SSH_PASSWORD")
                      (getenv "CHEZPP_SSH_PASSPHRASE")))
          (log (make-logger 'ssh-port-forward))
          (session (ssh-open host port user)))
     (logger-add-sink! log (make-log-port-sink 'stderr (current-error-port)))
     (dynamic-wind
       void
       (lambda ()
         ;; Authenticate before opening forwarding resources.
         (ssh-authenticate! 'ssh-port-forward session user auth secret)
         (let ((forwarding
                (open-forwarding! session direction listen-host listen-port
                                  target-host target-port log)))
           ;; Keep the forwarding resource available for a bounded example lifetime.
           (milisleep (example-option-natural 'ssh-port-forward ref 'duration 1000))
           (ssh-close-forwarding forwarding)))
       (lambda ()
         ;; Close session and logger on all exit paths.
         (logger-close! log)
         (ssh-close session)))
     0))
 (command-line-arguments))
