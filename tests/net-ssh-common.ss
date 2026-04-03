(define with-env
  (lambda (name value proc)
    (let ([old (getenv name)])
      (dynamic-wind
        (lambda () (putenv name value))
        proc
        (lambda () (putenv name (or old "")))))))

(define run-command!
  (lambda (who cmd)
    (unless (= (system cmd) 0)
      (errorf who "command failed: ~a" cmd))))

(define wait-for-ready-server
  (lambda (host port)
    (let loop ([attempt 50])
      (if (fx= attempt 0)
          (errorf 'wait-for-ready-server "server did not start on ~a:~a" host port)
          (guard (c [else
                     (milisleep 50)
                     (loop (fx1- attempt))])
            (let ([sock (open-socket 'inet 'stream)])
              (socket-connect! sock (make-socket-address 'inet host port))
              (close-socket sock)
              #t))))))

(define wait-for-result
  (lambda (thunk)
    (let loop ([attempt 40])
      (let ([x (thunk)])
        (if (eq? x #f)
            (and (fx> attempt 0)
                 (begin
                   (milisleep 50)
                   (loop (fx1- attempt))))
            x)))))

(define complete-sftp-write-all-nonblocking
  (lambda (file bv start stop)
    (let loop ([i start] [attempt 80])
      (cond
       [(fx= i stop)
        (fx- stop start)]
       [(fx= attempt 0)
        #f]
       [else
        (let ([n (sftp-write-all/nonblocking file bv i stop)])
          (cond
           [(eq? n #f)
            (milisleep 25)
            (loop i (fx1- attempt))]
           [(fx= n 0)
            (milisleep 25)
             (loop i (fx1- attempt))]
           [else
            (loop (fx+ i n) attempt)]))]))))

(define ssh-test-server-master-pid-path
  (lambda (remote-root)
    (let* ([suffix "/remote"]
           [n (string-length remote-root)]
           [m (string-length suffix)])
      (unless (and (fx>= n m)
                   (string=? (substring remote-root (fx- n m) n) suffix))
        (errorf 'ssh-test-server-master-pid-path "unexpected remote root ~s" remote-root))
      (string-append (substring remote-root 0 (fx- n m)) "/sshd.pid"))))

(define with-suspended-ssh-session-child
  (lambda (who remote-root proc)
    (let ([pid-path (ssh-test-server-master-pid-path remote-root)]
          [pids-path (string-append remote-root "/.sshd-pids")])
      (dynamic-wind
        (lambda ()
          (when (file-exists? pids-path)
            (delete-file pids-path #f))
          (run-command!
           who
           (format "sh -c 'collect() { for child in $(ps -o pid= --ppid \"$1\"); do child=$(printf \"%s\" \"$child\" | tr -d \"[:space:]\"); test -n \"$child\" || continue; printf \"%s\\n\" \"$child\"; collect \"$child\"; done; }; master=$(cat ~a); stopped=0; attempt=0; while [ \"$attempt\" -lt 4 ] && [ \"$stopped\" -eq 0 ]; do { collect \"$master\"; } > ~a; if [ -s ~a ]; then while IFS= read -r pid; do if kill -0 \"$pid\" >/dev/null 2>&1; then kill -STOP \"$pid\" >/dev/null 2>&1 || true; stopped=1; fi; done < ~a; fi; attempt=$((attempt + 1)); if [ \"$stopped\" -eq 0 ]; then sleep 0.05; fi; done; test \"$stopped\" -eq 1' >/dev/null 2>&1"
                   pid-path
                   pids-path
                   pids-path
                   pids-path))
          (milisleep 50))
        proc
        (lambda ()
          (when (file-exists? pids-path)
            (system (format "sh -c 'while IFS= read -r pid; do kill -CONT \"$pid\"; done < ~a' >/dev/null 2>&1"
                            pids-path))
            (milisleep 50)
            (delete-file pids-path #f)))))))

(define start-ssh-test-server
  (lambda ()
    (let* ([port (reserve-loopback-port)]
           [user (or (getenv "USER") (getenv "LOGNAME")
                     (errorf 'start-ssh-test-server "missing USER/LOGNAME environment variable"))]
           [root (format "/tmp/chezpp-net-ssh-~a" port)]
           [home (string-append root "/home")]
           [ssh-dir (string-append home "/.ssh")]
           [host-key (string-append root "/ssh_host_ed25519_key")]
           [authorized-keys (string-append root "/authorized_keys")]
           [pid-path (string-append root "/sshd.pid")]
           [log-path (string-append root "/sshd.log")]
           [config-path (string-append root "/sshd_config")]
           [remote-root (string-append root "/remote")])
      (when (file-exists? root)
        (file-removetree root #f))
      (mkdirs ssh-dir)
      (mkdirs (string-append remote-root "/nested"))
      (write-bytevector-file (string-append remote-root "/hello.txt") (string->utf8 "hello sftp"))
      (write-bytevector-file (string-append remote-root "/nested/base.txt") (string->utf8 "abcdef"))
      (run-command! 'start-ssh-test-server
                    (format "ssh-keygen -q -t ed25519 -N '' -f ~a >/dev/null 2>&1"
                            (string-append ssh-dir "/id_ed25519")))
      (run-command! 'start-ssh-test-server
                    (format "cp ~a ~a >/dev/null 2>&1"
                            (string-append ssh-dir "/id_ed25519.pub")
                            authorized-keys))
      (run-command! 'start-ssh-test-server
                    (format "ssh-keygen -q -t ed25519 -N '' -f ~a >/dev/null 2>&1"
                            host-key))
      (run-command! 'start-ssh-test-server
                    (format "chmod 700 ~a && chmod 600 ~a ~a ~a >/dev/null 2>&1"
                            ssh-dir
                            (string-append ssh-dir "/id_ed25519")
                            authorized-keys
                            host-key))
      (write-bytevector-file
       config-path
       (string->utf8
        (format "Port ~a\nListenAddress 127.0.0.1\nHostKey ~a\nPidFile ~a\nAuthorizedKeysFile ~a\nPasswordAuthentication no\nKbdInteractiveAuthentication no\nChallengeResponseAuthentication no\nPubkeyAuthentication yes\nUsePAM no\nPermitRootLogin no\nStrictModes no\nLogLevel ERROR\nSubsystem sftp internal-sftp\nAllowUsers ~a\n"
                port
                host-key
                pid-path
                authorized-keys
                user)))
      (run-command! 'start-ssh-test-server
                    (format "/usr/bin/sshd -f ~a -E ~a >/dev/null 2>&1 & echo $! > ~a"
                            config-path
                            log-path
                            pid-path))
      (wait-for-ready-server "127.0.0.1" port)
      (values remote-root
              home
              port
              user
              (lambda ()
                (when (file-exists? pid-path)
                  (system (format "kill $(cat ~a) >/dev/null 2>&1" pid-path))
                  (milisleep 50))
                (when (file-exists? root)
                  (file-removetree root #f)))))))

(define ssh-test-pty
  (lambda (session)
    (call-with-ssh-channel
     session
     (lambda (channel)
       (and (ssh-channel? channel)
            (eq? (ssh-request-pty! channel) channel))))))

(define ssh-test-read-slice
  (lambda (session)
    (let ([channel (ssh-exec session "printf abcde")]
          [buf (make-bytevector 8 0)])
      (dynamic-wind
        void
        (lambda ()
          (let ([n (ssh-read! channel buf 1 6)])
            (and (= n 5)
                 (equal? (slice-bytevector buf 1 6) (string->utf8 "abcde"))
                 (= (ssh-channel-exit-status channel) 0))))
        (lambda () (ssh-close-channel channel))))))

(define ssh-test-session-exec
  (lambda (session)
    (let ([channel (ssh-exec session "printf session-exec")])
      (dynamic-wind
        void
        (lambda ()
          (and (equal? (utf8->string (ssh-read channel 32)) "session-exec")
               (= (ssh-channel-exit-status channel) 0)))
        (lambda () (ssh-close-channel channel))))))

(define ssh-test-blocking-write
  (lambda (session)
    (let ([channel (ssh-exec session "sh -c 'IFS= read -r line; printf \"%s\" \"$line\"'")]
          [payload (string->utf8 "hello\n")])
      (dynamic-wind
        void
        (lambda ()
          (and (= (ssh-write channel payload 0 2) 2)
               (= (ssh-write-all channel payload 2 (bytevector-length payload)) 4)
               (equal? (utf8->string (ssh-read channel 16)) "hello")
               (= (ssh-channel-exit-status channel) 0)))
        (lambda () (ssh-close-channel channel))))))

(define ssh-test-nonblocking-io
  (lambda (session)
    (let ([channel (ssh-exec session "sh -c 'IFS= read -r line; printf \"%s\" \"$line\"'")]
          [payload (string->utf8 "world\n")]
          [buf (make-bytevector 8 0)])
      (dynamic-wind
        void
        (lambda ()
          (let ([n1 #f] [n2 #f] [n3 #f])
            (and (not (ssh-read/nonblocking channel 8))
                 (begin
                   (set! n1 (wait-for-result
                             (lambda ()
                               (ssh-write/nonblocking channel payload 0 2))))
                   #t)
                 (begin
                   (set! n2 (wait-for-result
                             (lambda ()
                               (ssh-write-all/nonblocking channel payload 2 (bytevector-length payload)))))
                   #t)
                 (begin
                   (set! n3 (wait-for-result
                             (lambda ()
                               (ssh-read!/nonblocking channel buf 0 5))))
                   #t)
                 (fixnum? n1)
                 (= n1 2)
                 (fixnum? n2)
                 (= n2 4)
                 (fixnum? n3)
                 (= n3 5)
                 (equal? (slice-bytevector buf 0 5) (string->utf8 "world"))
                 (= (ssh-channel-exit-status channel) 0))))
        (lambda () (ssh-close-channel channel))))))

(define ssh-test-read-timeout
  (lambda (session timeout?)
    (let ([channel-1 (ssh-exec session "sh -c 'sleep 1; printf late'")]
          [channel-2 (ssh-exec session "sh -c 'sleep 1; printf later'")]
          [buf (make-bytevector 8 0)])
      (dynamic-wind
        void
        (lambda ()
          (and (timeout?
                (lambda ()
                  (ssh-read channel-1 4 50)))
               (timeout?
                (lambda ()
                  (ssh-read! channel-2 buf 1 6 50)))))
        (lambda ()
          (ssh-close-channel channel-1)
          (ssh-close-channel channel-2))))))

(define ssh-test-shell
  (lambda (session)
    (let ([channel (ssh-open-channel session)])
      (dynamic-wind
        void
        (lambda ()
          (and (ssh-channel? (ssh-shell channel))
               (= (ssh-write-all channel (string->utf8 "exit\n")) 5)))
        (lambda () (ssh-close-channel channel))))))

(define ssh-test-setup-timeouts
  (lambda (session remote-root timeout?)
    (and
     (timeout?
      (lambda ()
        (with-suspended-ssh-session-child
         'ssh-test-setup-timeouts
         remote-root
         (lambda ()
           (ssh-open-channel session 50)))))
     (timeout?
      (lambda ()
        (with-suspended-ssh-session-child
         'ssh-test-setup-timeouts
         remote-root
         (lambda ()
           (call-with-ssh-channel session 50 ssh-channel?)))))
     (let ([channel (ssh-open-channel session)])
       (dynamic-wind
         void
         (lambda ()
           (timeout?
            (lambda ()
              (with-suspended-ssh-session-child
               'ssh-test-setup-timeouts
               remote-root
               (lambda ()
                 (ssh-request-pty! channel 50))))))
          (lambda () (ssh-close-channel channel))))
     (let ([channel (ssh-open-channel session)])
       (dynamic-wind
         void
         (lambda ()
           (timeout?
            (lambda ()
              (with-suspended-ssh-session-child
               'ssh-test-setup-timeouts
               remote-root
               (lambda ()
                 (ssh-exec channel "printf late" 50))))))
          (lambda () (ssh-close-channel channel))))
     (let ([channel (ssh-open-channel session)])
       (dynamic-wind
         void
         (lambda ()
           (timeout?
            (lambda ()
              (with-suspended-ssh-session-child
               'ssh-test-setup-timeouts
               remote-root
               (lambda ()
                 (ssh-shell channel 50))))))
          (lambda () (ssh-close-channel channel))))
     (timeout?
      (lambda ()
        (with-suspended-ssh-session-child
         'ssh-test-setup-timeouts
         remote-root
         (lambda ()
           (ssh-exec session "printf late" 50)))))
     (timeout?
      (lambda ()
        (with-suspended-ssh-session-child
         'ssh-test-setup-timeouts
         remote-root
         (lambda ()
           (ssh-shell session 50))))))))

(define ssh-test-port-wrappers
  (lambda (session)
    (let ([channel (ssh-exec session "sh -c 'IFS= read -r line; printf \"%s\" \"$line\"; printf err 1>&2'")])
      (dynamic-wind
        void
        (lambda ()
          (call-with-port
           (open-ssh-channel-input-port channel)
           (lambda (ip)
             (call-with-port
              (open-ssh-channel-error-port channel)
              (lambda (ep)
                (call-with-port
                 (open-ssh-channel-output-port channel)
                 (lambda (op)
                   (put-bytevector op (string->utf8 "ports\n"))
                   (flush-output-port op)
                   (and (equal? (utf8->string (get-bytevector-n ip 5)) "ports")
                        (equal? (utf8->string (get-bytevector-n ep 3)) "err")
                        (= (ssh-channel-exit-status channel) 0)))))))))
        (lambda () (ssh-close-channel channel))))))

(define sftp-test-list-and-stat
  (lambda (sftp remote-root)
    (and
     (let ([entries (sftp-list sftp remote-root)])
       (and (member "." entries)
            (member ".." entries)
            (member "hello.txt" entries)
            (member "nested" entries)))
     (let ([info (sftp-stat sftp (string-append remote-root "/hello.txt"))])
       (and (eq? (cdr (assq 'type info)) 'regular)
            (= (cdr (assq 'size info)) 10))))))

(define sftp-test-read-apis
  (lambda (sftp remote-root)
    (and
     (let ([file (sftp-open-file sftp (string-append remote-root "/hello.txt") 'read)])
       (dynamic-wind
         void
         (lambda ()
           (let ([x (utf8->string (sftp-read file 32))])
             (unless (equal? x "hello sftp")
               (errorf 'sftp-test-read-apis "unexpected hello.txt contents: ~s" x))
             #t))
         (lambda () (sftp-close-file file))))
     (let ([file (sftp-open-file sftp (string-append remote-root "/nested/base.txt") 'read)]
           [buf (make-bytevector 10 0)])
       (dynamic-wind
         void
         (lambda ()
           (let* ([n1 (sftp-read! file buf 1 4)]
                  [n2 (wait-for-result
                       (lambda ()
                         (sftp-read!/nonblocking file buf 4 7)))])
             (unless (= n1 3)
               (errorf 'sftp-test-read-apis "unexpected first read count: ~s" n1))
             (unless (= n2 3)
               (errorf 'sftp-test-read-apis "unexpected second read count: ~s" n2))
             (let ([x (slice-bytevector buf 1 7)])
               (unless (equal? x (string->utf8 "abcdef"))
                 (errorf 'sftp-test-read-apis "unexpected slice contents: ~s" x))
               #t)))
         (lambda () (sftp-close-file file)))))))

(define sftp-test-write-apis
  (lambda (sftp remote-root)
    (and
     (let ([file (sftp-open-file sftp (string-append remote-root "/written.txt")
                                 '(write create truncate))]
           [payload (string->utf8 "ABCDEF")])
       (dynamic-wind
         void
         (lambda ()
           (and (= (sftp-write file payload 0 2) 2)
                (= (sftp-write-all file payload 2 (bytevector-length payload)) 4)))
         (lambda () (sftp-close-file file))))
     (let ([file (sftp-open-file sftp (string-append remote-root "/written.txt") 'read)])
       (dynamic-wind
         void
         (lambda ()
           (let ([x (wait-for-result
                     (lambda ()
                       (sftp-read/nonblocking file 16)))])
             (and (bytevector? x)
                  (equal? (utf8->string x) "ABCDEF"))))
         (lambda () (sftp-close-file file))))
     (let ([file (sftp-open-file sftp (string-append remote-root "/nonblocking.txt")
                                 '(write create truncate))]
           [payload (string->utf8 "nb-data")])
       (dynamic-wind
         void
         (lambda ()
           (let ([n1 #f] [n2 #f])
             (and (begin
                    (set! n1 (wait-for-result
                              (lambda ()
                                (sftp-write/nonblocking file payload 0 2))))
                    #t)
                 (begin
                    (set! n2 (complete-sftp-write-all-nonblocking
                              file
                              payload
                              2
                              (bytevector-length payload)))
                    #t)
                 (fixnum? n1)
                 (= n1 2)
                 (fixnum? n2)
                 (= n2 5))))
         (lambda () (sftp-close-file file)))))))

(define sftp-test-port-apis
  (lambda (sftp remote-root)
    (and
     (let ([file (sftp-open-file sftp (string-append remote-root "/port.txt")
                                 '(write create truncate))])
       (dynamic-wind
         void
         (lambda ()
           (call-with-port
            (open-sftp-output-port file)
            (lambda (op)
              (put-bytevector op (string->utf8 "port-data"))
              (flush-output-port op)
              #t)))
         (lambda () (sftp-close-file file))))
     (let ([file (sftp-open-file sftp (string-append remote-root "/port.txt") 'read)])
       (dynamic-wind
         void
         (lambda ()
           (call-with-port
            (open-sftp-input-port file)
            (lambda (ip)
              (equal? (utf8->string (read-port->bytevector ip)) "port-data"))))
         (lambda () (sftp-close-file file)))))))

(define sftp-test-nonblocking-apis
  (lambda (sftp remote-root)
    (let ([file (sftp-open-file sftp (string-append remote-root "/hello.txt") 'read)])
      (dynamic-wind
        void
        (lambda ()
          (let ([x (wait-for-result
                    (lambda ()
                      (sftp-read/nonblocking file 32)))])
            (unless (bytevector? x)
              (errorf 'sftp-test-nonblocking-apis "unexpected nonblocking read result: ~s" x))
            (unless (equal? (utf8->string x) "hello sftp")
              (errorf 'sftp-test-nonblocking-apis "unexpected nonblocking read contents: ~s" x))
            #t))
        (lambda () (sftp-close-file file))))
    (let ([file (sftp-open-file sftp (string-append remote-root "/nested/base.txt") 'read)]
          [buf (make-bytevector 8 0)])
      (dynamic-wind
        void
        (lambda ()
          (let ([n (wait-for-result
                    (lambda ()
                      (sftp-read!/nonblocking file buf 1 7)))])
            (unless (fixnum? n)
              (errorf 'sftp-test-nonblocking-apis "unexpected nonblocking read! count: ~s" n))
            (unless (= n 6)
              (errorf 'sftp-test-nonblocking-apis "wrong nonblocking read! count: ~s" n))
            (let ([x (slice-bytevector buf 1 7)])
              (unless (equal? x (string->utf8 "abcdef"))
                (errorf 'sftp-test-nonblocking-apis "unexpected nonblocking read! contents: ~s" x))
              #t)))
        (lambda () (sftp-close-file file))))
    (let ([file (sftp-open-file sftp (string-append remote-root "/nb-write.txt")
                                '(write create truncate))]
          [payload (string->utf8 "non")])
      (dynamic-wind
        void
        (lambda ()
          (let ([n1 (wait-for-result
                     (lambda ()
                       (sftp-write/nonblocking file payload 0 (bytevector-length payload))))])
            (unless (fixnum? n1)
              (errorf 'sftp-test-nonblocking-apis "unexpected nonblocking write count: ~s" n1))
            (unless (= n1 3)
              (errorf 'sftp-test-nonblocking-apis "wrong nonblocking write count: ~s" n1))
            #t))
        (lambda () (sftp-close-file file))))
    (let ([file (sftp-open-file sftp (string-append remote-root "/nb-write.txt") 'read)])
      (dynamic-wind
        void
        (lambda ()
          (let ([x (sftp-read file 32)])
            (unless (bytevector? x)
              (errorf 'sftp-test-nonblocking-apis "unexpected verify read result: ~s" x))
            (unless (equal? (utf8->string x) "non")
              (errorf 'sftp-test-nonblocking-apis "unexpected verify read contents: ~s" x))
            #t))
        (lambda () (sftp-close-file file))))
    (let ([file (sftp-open-file sftp (string-append remote-root "/nb-write-all.txt")
                                '(write create truncate))]
          [payload (string->utf8 "nonblock")])
      (dynamic-wind
        void
        (lambda ()
          (let ([n (complete-sftp-write-all-nonblocking
                    file
                    payload
                    0
                    (bytevector-length payload))])
            (unless (fixnum? n)
              (errorf 'sftp-test-nonblocking-apis "unexpected nonblocking write-all count: ~s" n))
            (unless (= n (bytevector-length payload))
              (errorf 'sftp-test-nonblocking-apis "wrong nonblocking write-all count: ~s" n))
            #t))
        (lambda () (sftp-close-file file))))
    (let ([file (sftp-open-file sftp (string-append remote-root "/nb-write-all.txt") 'read)])
      (dynamic-wind
        void
        (lambda ()
          (let ([x (sftp-read file 32)])
            (unless (bytevector? x)
              (errorf 'sftp-test-nonblocking-apis "unexpected verify write-all read result: ~s" x))
            (unless (equal? (utf8->string x) "nonblock")
              (errorf 'sftp-test-nonblocking-apis "unexpected verify write-all contents: ~s" x))
            #t))
        (lambda () (sftp-close-file file))))
    #t))

(define sftp-test-transfer-apis
  (lambda (sftp remote-root upload-path download-path)
    (write-bytevector-file upload-path (string->utf8 "upload-bytes"))
    (and
     (equal? (sftp-upload sftp upload-path (string-append remote-root "/uploaded.txt"))
             (string-append remote-root "/uploaded.txt"))
     (equal? (sftp-download sftp (string-append remote-root "/uploaded.txt") download-path)
             download-path)
     (equal? (read-u8vec download-path) (string->utf8 "upload-bytes"))
     (eq? (sftp-mkdir! sftp (string-append remote-root "/tmpdir")) sftp)
     (eq? (sftp-rename! sftp
                        (string-append remote-root "/uploaded.txt")
                        (string-append remote-root "/renamed.txt"))
          sftp)
     (let ([entries (sftp-list sftp remote-root)])
       (and (member "renamed.txt" entries)
            (member "tmpdir" entries)))
     (eq? (sftp-delete! sftp (string-append remote-root "/renamed.txt")) sftp)
     (eq? (sftp-rmdir! sftp (string-append remote-root "/tmpdir")) sftp)
     (let ([entries (sftp-list sftp remote-root)])
       (and (not (member "renamed.txt" entries))
            (not (member "tmpdir" entries)))))))

(define sftp-test-timeouts
  (lambda (sftp remote-root timeout?)
    (let ([read-path-1 (string-append remote-root "/hello.txt")]
          [read-path-2 (string-append remote-root "/nested/base.txt")]
          [write-path (string-append remote-root "/timeout-write.txt")])
      (and
       (let ([file (sftp-open-file sftp read-path-1 'read)])
         (dynamic-wind
           void
           (lambda ()
             (with-suspended-ssh-session-child
              'sftp-test-timeouts
              remote-root
              (lambda ()
                (timeout?
                 (lambda ()
                   (sftp-read file 16 50))))))
           (lambda () (sftp-close-file file))))
       (let ([file (sftp-open-file sftp read-path-2 'read)]
             [buf (make-bytevector 32 0)])
         (dynamic-wind
           void
           (lambda ()
             (with-suspended-ssh-session-child
              'sftp-test-timeouts
              remote-root
              (lambda ()
                (timeout?
                 (lambda ()
                   (sftp-read! file buf 2 18 50))))))
           (lambda () (sftp-close-file file))))
       (let ([file (sftp-open-file sftp write-path '(write create truncate))]
             [payload (make-bytevector (* 1024 1024) 65)])
         (dynamic-wind
           void
           (lambda ()
             (with-suspended-ssh-session-child
              'sftp-test-timeouts
              remote-root
              (lambda ()
                (timeout?
                 (lambda ()
                   (sftp-write-all file payload 0 (bytevector-length payload) 50))))))
           (lambda () (sftp-close-file file))))))))

(define run-net-sftp-test
  (lambda (remote-root home port user)
    (with-env
     "HOME"
     home
     (lambda ()
       (let ([session (ssh-open "127.0.0.1" port user)]
             [upload-path "/tmp/chezpp-net-sftp-upload.bin"]
             [download-path "/tmp/chezpp-net-sftp-download.bin"])
         (dynamic-wind
           void
           (lambda ()
             (and
              (eq? (ssh-auth-publickey! session user) session)
              (call-with-sftp-session session sftp-session?)
              (let ([sftp (sftp-open session)])
                (dynamic-wind
                  void
                  (lambda ()
                    (and
                     (sftp-session? sftp)
                     (sftp-test-list-and-stat sftp remote-root)
                     (sftp-test-read-apis sftp remote-root)
                     (sftp-test-write-apis sftp remote-root)
                     (sftp-test-port-apis sftp remote-root)
                     (sftp-test-transfer-apis
                      sftp
                      remote-root
                      upload-path
                      download-path)))
                  (lambda () (sftp-close sftp))))))
           (lambda () (ssh-close session))))))))

(define run-net-sftp-nonblocking-test
  (lambda (remote-root home port user)
    (with-env
     "HOME"
     home
     (lambda ()
       (let ([session (ssh-open "127.0.0.1" port user)])
         (dynamic-wind
           void
           (lambda ()
             (and
              (eq? (ssh-auth-publickey! session user) session)
              (let ([sftp (sftp-open session)])
                (dynamic-wind
                  void
                  (lambda ()
                    (and (sftp-session? sftp)
                         (sftp-test-nonblocking-apis sftp remote-root)))
                  (lambda () (sftp-close sftp))))))
           (lambda () (ssh-close session))))))))

(define run-net-sftp-timeout-test
  (lambda (remote-root home port user timeout?)
    (with-env
     "HOME"
     home
     (lambda ()
       (let ([session (ssh-open "127.0.0.1" port user)])
         (dynamic-wind
           void
           (lambda ()
             (and
              (eq? (ssh-auth-publickey! session user) session)
              (let ([sftp (sftp-open session)])
                (dynamic-wind
                  void
                  (lambda ()
                    (and (sftp-session? sftp)
                         (sftp-test-timeouts sftp remote-root timeout?)))
                  (lambda () (sftp-close sftp))))))
           (lambda () (ssh-close session))))))))
