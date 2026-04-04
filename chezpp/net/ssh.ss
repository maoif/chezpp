(library (chezpp net ssh)
  (export ssh-session?
          ssh-channel?
          %ssh-session-handle
          ssh-open
          ssh-close
          ssh-auth-password!
          ssh-auth-publickey!
          ssh-auth-agent!
          ssh-open-channel
          ssh-close-channel
          ssh-exec
          ssh-shell
          ssh-read
          ssh-read!
          ssh-write
          ssh-write-all
          ssh-read/nonblocking
          ssh-read!/nonblocking
          ssh-write/nonblocking
          ssh-write-all/nonblocking
          ssh-request-pty!
          ssh-channel-exit-status
          call-with-ssh-session
          call-with-ssh-channel
          open-ssh-channel-input-port
          open-ssh-channel-output-port
          open-ssh-channel-error-port)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp net errors)
          (chezpp net ffi)
          (chezpp net private))

  (define-record-type (ssh-session %make-ssh-session ssh-session?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle ssh-session-handle ssh-session-handle-set!)
            (immutable host ssh-session-host)
            (immutable port ssh-session-port)
            (mutable user ssh-session-user ssh-session-user-set!)
            (mutable closed? ssh-session-closed? ssh-session-closed?-set!)))

  (define-record-type (ssh-channel %make-ssh-channel ssh-channel?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle ssh-channel-handle ssh-channel-handle-set!)
            (immutable session ssh-channel-session)
            (mutable closed? ssh-channel-closed? ssh-channel-closed?-set!)))

  (define ssh-default-timeout-ms 30000)

  (define ensure-success
    (lambda (who kind x)
      (cond
       [(ffi-error? x)
        (raise-net-error who kind (ffi-error-message x) x)]
       [else x])))

  (define ensure-session-open
    (lambda (who session)
      (when (ssh-session-closed? session)
        (raise-net-error who 'ssh "SSH session is closed" session))))

  (define ensure-channel-open
    (lambda (who channel)
      (when (ssh-channel-closed? channel)
        (raise-net-error who 'ssh "SSH channel is closed" channel))
      (ensure-session-open who (ssh-channel-session channel))))

  (define read-result
    (lambda (who x)
      (cond
       [(or (bytevector? x) (eof-object? x)) x]
       [(ffi-would-block? x) #f]
       [else (ensure-success who 'ssh x)])))

  (define read-into-result
    (lambda (who x)
      (cond
       [(or (fixnum? x) (eof-object? x)) x]
       [(ffi-would-block? x) #f]
       [else (ensure-success who 'ssh x)])))

  (define write-result
    (lambda (who x)
      (cond
       [(fixnum? x) x]
       [(ffi-would-block? x) #f]
       [else (ensure-success who 'ssh x)])))

  (define check-slice
    (lambda (who len start stop)
      (unless (and (fixnum? start) (fixnum? stop) (fx<= 0 start stop len))
        (errorf who "invalid slice [~a, ~a) for length ~a" start stop len))))

  (define check-size
    (lambda (who size)
      (when (fx< size 0)
        (errorf who "size must be non-negative, given ~s" size))
      size))

  (define ensure-user-maybe
    (lambda (who user)
      (unless (or (string? user) (eq? user #f))
        (errorf who "expected string or #f, given ~s" user))))

  (define check-timeout-ms
    (lambda (who timeout-ms)
      (unless (fixnum? timeout-ms)
        (errorf who "expected timeout fixnum, given ~s" timeout-ms))
      (when (fx< timeout-ms 0)
        (errorf who "timeout must be non-negative, given ~s" timeout-ms))
      timeout-ms))

  (define current-time-ms
    (lambda ()
      (let ([t (current-time)])
        (+ (* (time-second t) 1000)
           (quotient (time-nanosecond t) 1000000)))))

  (define timeout->deadline-ms
    (lambda (timeout-ms)
      (+ (current-time-ms) timeout-ms)))

  (define remaining-timeout-ms
    (lambda (deadline-ms)
      (let ([remaining (fx- deadline-ms (current-time-ms))])
        (if (fx<= remaining 0) 0 remaining))))

  (define make-binary-input-port
    (lambda (channel stderr?)
      (make-custom-binary-input-port
       (if stderr? "chezpp-ssh-error" "chezpp-ssh-input")
       (lambda (bv start count)
         (ensure-channel-open 'open-ssh-channel-input-port channel)
         (let ([stop (fx+ start count)])
           (let ([n (read-into-result
                     'open-ssh-channel-input-port
                     (ffi-net-ssh-channel-read-into (ssh-channel-handle channel)
                                                    bv
                                                    start
                                                    stop
                                                    (if stderr? 1 0)
                                                    0
                                                    -1))])
             (cond
              [(fixnum? n) n]
              [(eof-object? n) 0]
              [else (errorf 'open-ssh-channel-input-port
                            "unexpected nonblocking result from blocking SSH port read")]))))
       (lambda () #f)
       (lambda (x) #f)
       (lambda () #t))))

  (define make-binary-output-port
    (lambda (channel)
      (make-custom-binary-output-port
       "chezpp-ssh-output"
       (lambda (bv start count)
         (ssh-write-all channel bv start (fx+ start count)))
       (lambda () #f)
       (lambda (x) #f)
       (lambda () #t))))

  (define request-exec!
    (case-lambda
      [(who channel cmd)
       (request-exec! who channel cmd -1)]
      [(who channel cmd timeout-ms)
       (ensure-success who 'ssh
                       (ffi-net-ssh-channel-request-exec (ssh-channel-handle channel)
                                                         cmd
                                                         timeout-ms))
       channel]))

  (define request-shell!
    (case-lambda
      [(who channel)
       (request-shell! who channel -1)]
      [(who channel timeout-ms)
       (ensure-success who 'ssh
                       (ffi-net-ssh-channel-request-shell (ssh-channel-handle channel)
                                                          timeout-ms))
       channel]))

  #|proc:%ssh-session-handle
The `%ssh-session-handle` procedure returns the foreign handle stored inside an SSH session record.
|#
  (define-who %ssh-session-handle
    (lambda (session)
      (pcheck ([ssh-session? session])
              (ensure-session-open who session)
              (ssh-session-handle session))))

  #|proc:ssh-open
The `ssh-open` procedure opens a network SSH session to a remote host.
|#
  (define-who ssh-open
    (case-lambda
      [(host) (ssh-open host 22 #f ssh-default-timeout-ms)]
      [(host port) (ssh-open host port #f ssh-default-timeout-ms)]
      [(host port user-or-timeout)
       (if (fixnum? user-or-timeout)
           (ssh-open host port #f user-or-timeout)
           (ssh-open host port user-or-timeout ssh-default-timeout-ms))]
      [(host port user timeout-ms)
       (pcheck ([string? host] [fixnum? port])
               (check-port who port)
               (ensure-user-maybe who user)
               (unless (fixnum? timeout-ms)
                 (errorf who "expected timeout fixnum, given ~s" timeout-ms))
               (when (fx< timeout-ms 0)
                 (errorf who "timeout must be non-negative, given ~s" timeout-ms))
               (let ([ans (ensure-success who 'ssh
                                          (ffi-net-ssh-open host
                                                            port
                                                            (or user "")
                                                            timeout-ms))])
                 (%make-ssh-session ans host port user #f)))]))

  #|proc:ssh-close
The `ssh-close` procedure closes an SSH session and releases its foreign resources.
|#
  (define-who ssh-close
    (lambda (session)
      (pcheck ([ssh-session? session])
              (unless (ssh-session-closed? session)
                (ensure-success who 'ssh (ffi-net-ssh-close (ssh-session-handle session)))
                (ssh-session-handle-set! session 0)
                (ssh-session-closed?-set! session #t))
              session)))

  #|proc:ssh-auth-password!
The `ssh-auth-password!` procedure authenticates an SSH session with a password.
|#
  (define-who ssh-auth-password!
    (case-lambda
      [(session password)
       (ssh-auth-password! session #f password)]
      [(session user password)
       (pcheck ([ssh-session? session] [string? password])
               (ensure-user-maybe who user)
               (ensure-session-open who session)
               (ensure-success who 'ssh
                               (ffi-net-ssh-auth-password (ssh-session-handle session)
                                                          (or user "")
                                                          password))
               (when user
                 (ssh-session-user-set! session user))
               session)]))

  #|proc:ssh-auth-publickey!
The `ssh-auth-publickey!` procedure authenticates an SSH session using libssh's automatic public-key discovery.
|#
  (define-who ssh-auth-publickey!
    (case-lambda
      [(session)
       (ssh-auth-publickey! session #f #f)]
      [(session user)
       (ssh-auth-publickey! session user #f)]
      [(session user passphrase)
       (pcheck ([ssh-session? session])
               (ensure-user-maybe who user)
               (unless (or (string? passphrase) (eq? passphrase #f))
                 (errorf who "expected string or #f for passphrase"))
               (ensure-session-open who session)
               (ensure-success who 'ssh
                               (ffi-net-ssh-auth-publickey-auto (ssh-session-handle session)
                                                                (or user "")
                                                                (or passphrase "")))
               (when user
                 (ssh-session-user-set! session user))
               session)]))

  #|proc:ssh-auth-agent!
The `ssh-auth-agent!` procedure authenticates an SSH session using the local SSH agent.
|#
  (define-who ssh-auth-agent!
    (case-lambda
      [(session)
       (ssh-auth-agent! session #f)]
      [(session user)
       (pcheck ([ssh-session? session])
               (ensure-user-maybe who user)
               (ensure-session-open who session)
               (ensure-success who 'ssh
                               (ffi-net-ssh-auth-agent (ssh-session-handle session)
                                                       (or user "")))
               (when user
                 (ssh-session-user-set! session user))
               session)]))

  #|proc:ssh-open-channel
The `ssh-open-channel` procedure opens a new SSH session channel.
|#
  (define-who ssh-open-channel
    (case-lambda
      [(session)
       (pcheck ([ssh-session? session])
               (ensure-session-open who session)
               (%make-ssh-channel
                (ensure-success who 'ssh
                                (ffi-net-ssh-channel-open (ssh-session-handle session) -1))
                session
                #f))]
      [(session timeout-ms)
       (pcheck ([ssh-session? session] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (ensure-session-open who session)
               (%make-ssh-channel
                (ensure-success who 'ssh
                                (ffi-net-ssh-channel-open (ssh-session-handle session)
                                                          timeout-ms))
                session
                #f))]))

  #|proc:ssh-close-channel
The `ssh-close-channel` procedure closes an SSH channel and releases its foreign resources.
|#
  (define-who ssh-close-channel
    (lambda (channel)
      (pcheck ([ssh-channel? channel])
              (unless (ssh-channel-closed? channel)
                (when (guard (c [else #f])
                        (not (fx= 0 (%ssh-session-handle (ssh-channel-session channel)))))
                  (ensure-success who 'ssh
                                  (ffi-net-ssh-channel-close (ssh-channel-handle channel))))
                (ssh-channel-handle-set! channel 0)
                (ssh-channel-closed?-set! channel #t))
              channel)))

  #|proc:ssh-exec
The `ssh-exec` procedure requests remote command execution on an SSH channel.
|#
  (define-who ssh-exec
    (case-lambda
      [(channel cmd)
       (ssh-exec channel cmd -1)]
      [(channel cmd timeout-ms)
       (cond
        [(ssh-channel? channel)
         (pcheck ([string? cmd] [fixnum? timeout-ms])
                 (when (fx>= timeout-ms 0)
                   (check-timeout-ms who timeout-ms))
                 (ensure-channel-open who channel)
                 (request-exec! who channel cmd timeout-ms))]
        [(ssh-session? channel)
         (pcheck ([string? cmd] [fixnum? timeout-ms])
                 (let* ([deadline-ms (and (fx>= timeout-ms 0)
                                          (begin
                                            (check-timeout-ms who timeout-ms)
                                            (timeout->deadline-ms timeout-ms)))]
                        [ch (if deadline-ms
                                (ssh-open-channel channel
                                                  (remaining-timeout-ms deadline-ms))
                                (ssh-open-channel channel))])
                   (guard (c [else (ssh-close-channel ch) (raise c)])
                     (request-exec! who
                                    ch
                                    cmd
                                    (if deadline-ms
                                        (remaining-timeout-ms deadline-ms)
                                        -1)))))]
        [else
         (errorf who "expected ssh channel or session, given ~s" channel)])]))

  #|proc:ssh-shell
The `ssh-shell` procedure requests an interactive shell on an SSH channel.
|#
  (define-who ssh-shell
    (case-lambda
      [(target)
       (ssh-shell target -1)]
      [(target timeout-ms)
       (cond
        [(ssh-channel? target)
         (pcheck ([fixnum? timeout-ms])
                 (when (fx>= timeout-ms 0)
                   (check-timeout-ms who timeout-ms))
                 (ensure-channel-open who target)
                 (request-shell! who target timeout-ms))]
        [(ssh-session? target)
         (pcheck ([fixnum? timeout-ms])
                 (let* ([deadline-ms (and (fx>= timeout-ms 0)
                                          (begin
                                            (check-timeout-ms who timeout-ms)
                                            (timeout->deadline-ms timeout-ms)))]
                        [ch (if deadline-ms
                                (ssh-open-channel target
                                                  (remaining-timeout-ms deadline-ms))
                                (ssh-open-channel target))])
                   (guard (c [else (ssh-close-channel ch) (raise c)])
                     (request-shell! who
                                     ch
                                     (if deadline-ms
                                         (remaining-timeout-ms deadline-ms)
                                         -1)))))]
        [else
         (errorf who "expected ssh channel or session, given ~s" target)])]))

  #|proc:ssh-request-pty!
The `ssh-request-pty!` procedure requests a pseudo-terminal on an SSH channel.
|#
  (define-who ssh-request-pty!
    (case-lambda
      [(channel)
       (ssh-request-pty! channel -1)]
      [(channel timeout-ms)
       (pcheck ([ssh-channel? channel] [fixnum? timeout-ms])
               (when (fx>= timeout-ms 0)
                 (check-timeout-ms who timeout-ms))
               (ensure-channel-open who channel)
               (ensure-success who 'ssh
                               (ffi-net-ssh-channel-request-pty (ssh-channel-handle channel)
                                                                timeout-ms))
               channel)]))

  #|proc:ssh-read
The `ssh-read` procedure reads up to `size` bytes from an SSH channel's stdout stream.
|#
  (define-who ssh-read
    (case-lambda
      [(channel size)
       (pcheck ([ssh-channel? channel] [fixnum? size])
               (check-size who size)
               (ensure-channel-open who channel)
               (read-result who
                            (ffi-net-ssh-channel-read (ssh-channel-handle channel)
                                                      size
                                                      0
                                                      0
                                                      -1)))]
      [(channel size timeout-ms)
       (pcheck ([ssh-channel? channel] [fixnum? size])
               (check-size who size)
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (read-result who
                            (ffi-net-ssh-channel-read (ssh-channel-handle channel)
                                                      size
                                                      0
                                                      0
                                                      timeout-ms)))]))

  #|proc:ssh-read/nonblocking
The `ssh-read/nonblocking` procedure attempts to read from an SSH channel without blocking.
|#
  (define-who ssh-read/nonblocking
    (lambda (channel size)
      (pcheck ([ssh-channel? channel] [fixnum? size])
              (check-size who size)
              (ensure-channel-open who channel)
              (read-result who
                           (ffi-net-ssh-channel-read (ssh-channel-handle channel) size 0 1 -1)))))

  #|proc:ssh-read!
The `ssh-read!` procedure reads into a bytevector slice from an SSH channel's stdout stream.
|#
  (define-who ssh-read!
    (case-lambda
      [(channel bv) (ssh-read! channel bv 0 (bytevector-length bv))]
      [(channel bv start) (ssh-read! channel bv start (bytevector-length bv))]
      [(channel bv start stop)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (read-into-result
                who
                (ffi-net-ssh-channel-read-into (ssh-channel-handle channel)
                                               bv
                                               start
                                               stop
                                               0
                                               0
                                               -1)))]
      [(channel bv start stop timeout-ms)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (read-into-result
                who
                (ffi-net-ssh-channel-read-into (ssh-channel-handle channel)
                                               bv
                                               start
                                               stop
                                               0
                                               0
                                               timeout-ms)))]))

  #|proc:ssh-read!/nonblocking
The `ssh-read!/nonblocking` procedure attempts to read into a bytevector slice without blocking.
|#
  (define-who ssh-read!/nonblocking
    (case-lambda
      [(channel bv) (ssh-read!/nonblocking channel bv 0 (bytevector-length bv))]
      [(channel bv start) (ssh-read!/nonblocking channel bv start (bytevector-length bv))]
      [(channel bv start stop)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (read-into-result
                who
                (ffi-net-ssh-channel-read-into (ssh-channel-handle channel)
                                               bv
                                               start
                                               stop
                                               0
                                               1
                                               -1)))]))

  #|proc:ssh-write
The `ssh-write` procedure writes a bytevector slice to an SSH channel's stdin stream.
|#
  (define-who ssh-write
    (case-lambda
      [(channel bv) (ssh-write channel bv 0 (bytevector-length bv))]
      [(channel bv start) (ssh-write channel bv start (bytevector-length bv))]
      [(channel bv start stop)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (write-result
                who
                (ffi-net-ssh-channel-write (ssh-channel-handle channel)
                                           bv
                                           start
                                           stop
                                           0
                                           -1)))]
      [(channel bv start stop timeout-ms)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (write-result
                who
                (ffi-net-ssh-channel-write (ssh-channel-handle channel)
                                           bv
                                           start
                                           stop
                                           0
                                           timeout-ms)))]))

  #|proc:ssh-write/nonblocking
The `ssh-write/nonblocking` procedure attempts to write a bytevector slice without blocking.
|#
  (define-who ssh-write/nonblocking
    (case-lambda
      [(channel bv) (ssh-write/nonblocking channel bv 0 (bytevector-length bv))]
      [(channel bv start) (ssh-write/nonblocking channel bv start (bytevector-length bv))]
      [(channel bv start stop)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (write-result
                who
                (ffi-net-ssh-channel-write (ssh-channel-handle channel)
                                           bv
                                           start
                                           stop
                                           1
                                           -1)))]))

  #|proc:ssh-write-all
The `ssh-write-all` procedure writes an entire bytevector slice to an SSH channel.
|#
  (define-who ssh-write-all
    (case-lambda
      [(channel bv) (ssh-write-all channel bv 0 (bytevector-length bv))]
      [(channel bv start) (ssh-write-all channel bv start (bytevector-length bv))]
      [(channel bv start stop)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (let loop ([i start])
                 (if (fx= i stop)
                     (fx- stop start)
                     (loop (fx+ i (ssh-write channel bv i stop))))))]
      [(channel bv start stop timeout-ms)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (check-timeout-ms who timeout-ms)
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (let ([deadline-ms (and (fixnum? timeout-ms) (fx>= timeout-ms 0)
                                       (timeout->deadline-ms timeout-ms))])
                 (let loop ([i start])
                   (if (fx= i stop)
                       (fx- stop start)
                       (let ([step-timeout (if deadline-ms
                                               (remaining-timeout-ms deadline-ms)
                                               -1)])
                         (loop (fx+ i (ssh-write channel bv i stop step-timeout))))))))]))

  #|proc:ssh-write-all/nonblocking
The `ssh-write-all/nonblocking` procedure writes as much of a bytevector slice as possible without blocking.
|#
  (define-who ssh-write-all/nonblocking
    (case-lambda
      [(channel bv) (ssh-write-all/nonblocking channel bv 0 (bytevector-length bv))]
      [(channel bv start) (ssh-write-all/nonblocking channel bv start (bytevector-length bv))]
      [(channel bv start stop)
       (pcheck ([ssh-channel? channel] [bytevector? bv])
               (ensure-channel-open who channel)
               (check-slice who (bytevector-length bv) start stop)
               (let loop ([i start])
                 (if (fx= i stop)
                     (fx- stop start)
                     (let ([n (ssh-write/nonblocking channel bv i stop)])
                       (cond
                        [(eq? n #f) (and (fx> i start) (fx- i start))]
                        [(fx= n 0) (fx- i start)]
                        [else (loop (fx+ i n))])))))]))

  #|proc:ssh-channel-exit-status
The `ssh-channel-exit-status` procedure returns the remote process exit status for an SSH channel.
|#
  (define-who ssh-channel-exit-status
    (lambda (channel)
      (pcheck ([ssh-channel? channel])
              (ensure-channel-open who channel)
              (ensure-success who 'ssh
                              (ffi-net-ssh-channel-exit-status (ssh-channel-handle channel))))))

  #|proc:call-with-ssh-session
The `call-with-ssh-session` procedure opens an SSH session, applies a procedure, and closes the session afterwards.
|#
  (define-who call-with-ssh-session
    (case-lambda
      [(host proc)
       (call-with-ssh-session host 22 #f ssh-default-timeout-ms proc)]
      [(host port proc)
       (call-with-ssh-session host port #f ssh-default-timeout-ms proc)]
      [(host port user-or-timeout proc)
       (pcheck ([procedure? proc])
               (if (fixnum? user-or-timeout)
                   (call-with-ssh-session host port #f user-or-timeout proc)
                   (call-with-ssh-session host port user-or-timeout ssh-default-timeout-ms proc)))]
      [(host port user timeout-ms proc)
       (pcheck ([procedure? proc])
               (let ([session (ssh-open host port user timeout-ms)])
                 (dynamic-wind
                   void
                   (lambda () (proc session))
                   (lambda () (ssh-close session)))))]))

  #|proc:call-with-ssh-channel
The `call-with-ssh-channel` procedure opens an SSH channel, applies a procedure, and closes the channel afterwards.
|#
  (define-who call-with-ssh-channel
    (case-lambda
      [(session proc)
       (pcheck ([ssh-session? session] [procedure? proc])
               (let ([channel (ssh-open-channel session)])
                 (dynamic-wind
                   void
                   (lambda () (proc channel))
                   (lambda () (ssh-close-channel channel)))))]
      [(session timeout-ms proc)
       (pcheck ([ssh-session? session] [fixnum? timeout-ms] [procedure? proc])
               (check-timeout-ms who timeout-ms)
               (let ([channel (ssh-open-channel session timeout-ms)])
                 (dynamic-wind
                   void
                   (lambda () (proc channel))
                   (lambda () (ssh-close-channel channel)))))]))

  #|proc:open-ssh-channel-input-port
The `open-ssh-channel-input-port` procedure opens a binary input port over an SSH channel's stdout stream.
|#
  (define-who open-ssh-channel-input-port
    (lambda (channel)
      (pcheck ([ssh-channel? channel])
              (ensure-channel-open who channel)
              (make-binary-input-port channel #f))))

  #|proc:open-ssh-channel-output-port
The `open-ssh-channel-output-port` procedure opens a binary output port over an SSH channel's stdin stream.
|#
  (define-who open-ssh-channel-output-port
    (lambda (channel)
      (pcheck ([ssh-channel? channel])
              (ensure-channel-open who channel)
              (make-binary-output-port channel))))

  #|proc:open-ssh-channel-error-port
The `open-ssh-channel-error-port` procedure opens a binary input port over an SSH channel's stderr stream.
|#
  (define-who open-ssh-channel-error-port
    (lambda (channel)
      (pcheck ([ssh-channel? channel])
              (ensure-channel-open who channel)
              (make-binary-input-port channel #t))))
  )
