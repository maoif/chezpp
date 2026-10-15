(import (chezpp) (chezpp net scp) (chezpp logging))
(load "examples/common.ss")

(define scp-example
  (lambda (direction kind host port user auth-kind auth-arg local-path remote-path)
    (let ([session (scp-open host port user auth-kind auth-arg)]
          [log (make-logger 'scp-example)])
      (logger-add-sink! log (make-log-port-sink 'stderr (current-error-port)))
      (dynamic-wind
        void
        (lambda ()
          (with-logger log
            (log-infof "starting SCP ~a of ~a" direction kind))
          (case kind
            [(file)
             (case direction
               [(upload)
                ;; Upload one local file to the selected remote path.
                (scp-upload session local-path remote-path)]
               [(download)
                ;; Download one remote file to the selected local path.
                (scp-download session remote-path local-path)])]
            [(directory)
             (case direction
               [(upload)
                ;; Recursively upload a local directory tree.
                (scp-copy-directory session 'upload local-path remote-path)]
               [(download)
                ;; Recursively download a remote directory tree.
                (scp-copy-directory session 'download remote-path local-path)])])
          (with-logger log (log-infof "SCP ~a completed" direction))
          0)
        (lambda ()
          ;; Close the owned SCP and SSH sessions after transfer or failure.
          (scp-close session)
          (logger-close! log))))))

(example-run!
 'scp-example
 "Copy files or directories in either direction over SSH SCP."
 '(host port user auth direction kind local remote)
 (lambda (ref)
   (let* ([host (example-required 'scp-example ref 'host)]
          [port (example-option-natural 'scp-example ref 'port 22)]
          [user (example-required 'scp-example ref 'user)]
          [auth (or (ref 'auth) "agent")]
          [password (getenv "CHEZPP_SSH_PASSWORD")]
          [passphrase (getenv "CHEZPP_SSH_PASSPHRASE")]
          [auth-arg (case (string->symbol auth)
                      [(password) password]
                      [(publickey) passphrase]
                      [(agent) #f]
                      [else (errorf 'scp-example "auth must be password, publickey, or agent")])]
          [direction (string->symbol (example-required 'scp-example ref 'direction))]
          [kind (string->symbol (example-required 'scp-example ref 'kind))]
          [local (example-required 'scp-example ref 'local)]
          [remote (example-required 'scp-example ref 'remote)])
     (unless (and (fixnum? port) (<= 1 port 65535))
       (errorf 'scp-example "port must be between 1 and 65535"))
     (unless (memq direction '(upload download))
       (errorf 'scp-example "direction must be upload or download"))
     (unless (memq kind '(file directory))
       (errorf 'scp-example "kind must be file or directory"))
     (scp-example direction kind host port user auth auth-arg local remote)))
 (command-line-arguments))
