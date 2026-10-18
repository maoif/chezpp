;; Shared SSH CLI, authentication, output draining, and lifecycle helpers.
(load "examples/net/net-example-common.ss")

(define ssh-example-options '((host . "Required: SSH server IP address or hostname; known_hosts verification is strict.")
   (port . "SSH server port, 0 through 65535 (default: 22).")
   (user . "Required: remote SSH username.")
   (auth . "Authentication: agent, publickey, or password (default: agent). Password uses CHEZPP_SSH_PASSWORD; agent uses SSH_AUTH_SOCK.")
   (private-key . "Private key path for publickey auth (default: discover keys in ~/.ssh). Passphrase uses CHEZPP_SSH_PASSPHRASE.")))

(define ssh-example-port
  (lambda (who ref name default)
    (let ([port (example-option-natural who ref name default)])
      (unless (<= 0 port 65535) (errorf who "--~a must be between 0 and 65535" name))
      port)))

(define call-with-example-ssh
  (lambda (who ref proc)
    (let* ([host (example-required who ref 'host)]
           [port (ssh-example-port who ref 'port 22)]
           [user (example-required who ref 'user)]
           [auth (or (ref 'auth) "agent")]
           [log (make-logger who)]
           [old-interrupt (keyboard-interrupt-handler)])
      (unless (member auth '("password" "publickey" "agent"))
        (errorf who "--auth must be password, publickey, or agent"))
      (logger-add-sink! log (make-log-port-sink 'stderr (current-error-port)))
      (dynamic-wind
        (lambda ()
          (keyboard-interrupt-handler (lambda () (errorf who "interrupted"))))
        (lambda ()
          ;; ssh-open uses strict known-host verification against $HOME/.ssh/known_hosts.
          (call-with-ssh-session
           host port user
           (lambda (session)
             (with-logger log (log-infof "opening SSH session to ~a:~a" host port))
             (cond
              [(string=? auth "password")
               (let ([password (getenv "CHEZPP_SSH_PASSWORD")])
                 (unless password (errorf who "set CHEZPP_SSH_PASSWORD for password auth"))
                 ;; Authenticate with an environment-supplied password.
                 (ssh-auth-password! session user password))]
              [(string=? auth "publickey")
               (let ([key (ref 'private-key)] [passphrase (getenv "CHEZPP_SSH_PASSPHRASE")])
                 (if key
                     ;; Load the explicitly selected private key, with optional passphrase.
                     (ssh-auth-private-key! session user #f key passphrase)
                     ;; Discover a private key in the user's SSH directory.
                     (ssh-auth-publickey! session user passphrase)))]
              [else
               ;; Authenticate through the agent selected by SSH_AUTH_SOCK.
               (ssh-auth-agent! session user)])
             (proc session log))))
        (lambda ()
          (keyboard-interrupt-handler old-interrupt)
          (with-logger log (log-info "closing SSH session"))
          (logger-close! log))))))

(define ssh-example-wait
  (lambda (answer*)
    (let ([targets
           (map (lambda (answer)
                  (make-poll-target (net-would-block-resource answer)
                                    (net-would-block-events answer)))
                (filter net-would-block? answer*))])
      (if (null? targets) (milisleep 5) (poll targets 25)))))

(define ssh-example-drain
  (lambda (channel input-step)
    (call-with-example-output-ports
     (lambda (out err)
       (let loop ([out-eof? #f] [err-eof? #f])
         ;; Read stdout and stderr independently so neither stream can block the other.
         (let* ([stdout (and (not out-eof?) (ssh-read/nonblocking channel 65536))]
                [stderr (and (not err-eof?) (ssh-read-stderr/nonblocking channel 65536))]
                [out-eof? (or out-eof? (eof-object? stdout))]
                [err-eof? (or err-eof? (eof-object? stderr))])
           (when (bytevector? stdout) (put-bytevector out stdout) (flush-output-port out))
           (when (bytevector? stderr) (put-bytevector err stderr) (flush-output-port err))
           (input-step)
           (if (and out-eof? err-eof?)
               ;; Exit-status retrieval can wait: drain both streams and finish stdin first.
               (let ([status (ssh-channel-exit-status channel)])
                 (if (>= status 0) status 255))
               (begin (ssh-example-wait (list stdout stderr))
                      (loop out-eof? err-eof?)))))))))
