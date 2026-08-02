(define sftp-home-path
  (lambda ()
    (path-join sftp-file-transfer-state-root "home")))

(define sftp-private-key-path
  (lambda ()
    (path-join (path-join (sftp-home-path) ".ssh") "id_ed25519")))

(define sftp-public-key-path
  (lambda ()
    (string-append (sftp-private-key-path) ".pub")))

(define sftp-host-key-path
  (lambda ()
    (path-join sftp-file-transfer-state-root "ssh_host_ed25519_key")))

(define sftp-authorized-keys-path
  (lambda ()
    (path-join sftp-file-transfer-state-root "authorized_keys")))

(define sftp-pid-path
  (lambda ()
    (path-join sftp-file-transfer-state-root "sshd.pid")))

(define sftp-log-path
  (lambda ()
    (path-join sftp-file-transfer-state-root "sshd.log")))

(define sftp-config-path
  (lambda ()
    (path-join sftp-file-transfer-state-root "sshd_config")))

(define sftp-target-dir-path
  (lambda ()
    (path-join sftp-file-transfer-state-root "target-dir.txt")))

(define current-login-user
  (lambda ()
    (or (getenv "USER")
        (getenv "LOGNAME")
        (errorf 'current-login-user "missing USER/LOGNAME environment variable"))))

(define write-string-file
  (lambda (path text)
    (write-bytevector-file path (string->utf8 text))))

(define read-string-file
  (lambda (path)
    (utf8->string (read-u8vec path))))

(define sftp-start-sshd!
  (lambda (dir)
    (let* ([user (current-login-user)]
           [home (sftp-home-path)]
           [ssh-dir (path-join home ".ssh")]
           [private-key (sftp-private-key-path)]
           [public-key (sftp-public-key-path)]
           [authorized-keys (sftp-authorized-keys-path)]
           [host-key (sftp-host-key-path)]
           [pid-path (sftp-pid-path)]
           [log-path (sftp-log-path)]
           [config-path (sftp-config-path)]
           [target-path (sftp-target-dir-path)])
      (when (file-exists? sftp-file-transfer-state-root)
        (file-removetree sftp-file-transfer-state-root #f))
      (mkdirs ssh-dir)
      (write-string-file target-path dir)
      (run-command!
       'sftp-file-server
       (format "ssh-keygen -q -t ed25519 -N '' -f ~a >/dev/null 2>&1" private-key))
      (run-command!
       'sftp-file-server
       (format "cp ~a ~a >/dev/null 2>&1" public-key authorized-keys))
      (run-command!
       'sftp-file-server
       (format "ssh-keygen -q -t ed25519 -N '' -f ~a >/dev/null 2>&1" host-key))
      (run-command!
       'sftp-file-server
       (format "chmod 700 ~a && chmod 600 ~a ~a ~a >/dev/null 2>&1"
               ssh-dir
               private-key
               authorized-keys
               host-key))
      (write-string-file
       config-path
       (format "Port ~a\nListenAddress 127.0.0.1\nHostKey ~a\nPidFile ~a\nAuthorizedKeysFile ~a\nPasswordAuthentication no\nKbdInteractiveAuthentication no\nChallengeResponseAuthentication no\nPubkeyAuthentication yes\nUsePAM no\nPermitRootLogin no\nStrictModes no\nLogLevel ERROR\nSubsystem sftp internal-sftp -d ~a\nAllowUsers ~a\n"
               sftp-file-transfer-port
               host-key
               pid-path
               authorized-keys
               dir
               user))
      (run-command!
       'sftp-file-server
       (format "/usr/bin/sshd -D -f ~a -E ~a >/dev/null 2>&1 & echo $! > ~a"
               config-path
               log-path
               pid-path))
      (wait-for-ready-server file-transfer-host sftp-file-transfer-port)
      sftp-file-transfer-state-root)))

(define sftp-stop-sshd!
  (lambda ()
    (let ([pid-path (sftp-pid-path)])
      (when (file-exists? pid-path)
        (system (format "kill $(cat ~a) >/dev/null 2>&1" pid-path))
        (milisleep 50))
      (when (file-exists? sftp-file-transfer-state-root)
        (file-removetree sftp-file-transfer-state-root #f)))))

#|proc:sftp-file-server
The `sftp-file-server` procedure starts a localhost `sshd`-backed SFTP service,
waits until the client uploads the end-marker file into `dir`, then stops the
server and removes the marker.
|#
(define sftp-file-server
  (lambda (dir)
    (pcheck ([string? dir])
      (ensure-upload-directory dir)
      (delete-file/ignore (done-marker-path dir))
      (sftp-start-sshd! dir)
      (dynamic-wind
        void
        (lambda ()
          (wait-for-file (done-marker-path dir))
          (delete-file/ignore (done-marker-path dir))
          dir)
        (lambda ()
          (sftp-stop-sshd!))))))

#|proc:sftp-file-client
The `sftp-file-client` procedure uploads each file in `path*` to the localhost
SFTP example server and then uploads the end-marker file.
|#
(define sftp-file-client
  (lambda (path*)
    (validate-file-list 'sftp-file-client path*)
    (let ([target-dir (string-trim-right (read-string-file (sftp-target-dir-path)) #\newline)]
          [user (current-login-user)]
          [done-path (path-join "/tmp" "chezpp-example-sftp-done-marker")])
      (dynamic-wind
        (lambda ()
          (write-bytevector-file done-path #vu8()))
        (lambda ()
          (with-env
           "HOME"
           (sftp-home-path)
           (lambda ()
             (let ([session (ssh-open file-transfer-host sftp-file-transfer-port user)])
               (dynamic-wind
                 void
                 (lambda ()
                   (ssh-auth-publickey! session user)
                   (let ([sftp (sftp-open session)])
                     (dynamic-wind
                       void
                       (lambda ()
                         (for-each
                          (lambda (path)
                            (sftp-upload sftp
                                         path
                                         (path-join target-dir (path-basename path))))
                          path*)
                         (sftp-upload sftp
                                      done-path
                                      (done-marker-path target-dir))
                         path*)
                       (lambda ()
                         (sftp-close sftp)))))
                 (lambda ()
                   (ssh-close session)))))))
        (lambda ()
          (delete-file/ignore done-path))))))
