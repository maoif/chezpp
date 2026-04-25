(library (chezpp net scp)
  (export scp-session?
          scp-open
          scp-close
          scp-cancel-pending!
          scp-download
          scp-upload
          scp-download/nonblocking
          scp-upload/nonblocking
          scp-copy-directory
          scp-copy-directory/nonblocking
          call-with-scp-session)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp file)
          (chezpp net errors)
          (chezpp net ffi)
          (chezpp net poll)
          (chezpp net address)
          (chezpp net socket)
          (chezpp net private)
          (chezpp net ssh))

  (define-record-type (scp-pending-op %make-scp-pending-op scp-pending-op?)
    (sealed #t)
    (opaque #f)
    (fields (immutable kind scp-pending-kind)
            (immutable args scp-pending-args)
            (immutable reader scp-pending-reader)
            (immutable writer scp-pending-writer)
            (immutable thread scp-pending-thread)
            (mutable done? scp-pending-done? scp-pending-done?-set!)
            (mutable result scp-pending-result scp-pending-result-set!)
            (mutable cancelled? scp-pending-cancelled? scp-pending-cancelled?-set!)))

  (define-record-type (scp-session %make-scp-session scp-session?)
    (sealed #t)
    (opaque #f)
    (fields (immutable ssh-session scp-session-ssh-session)
            (immutable timeout-ms scp-session-timeout-ms)
            (immutable owns-ssh? scp-session-owns-ssh?)
            (mutable pending scp-session-pending scp-session-pending-set!)
            (mutable closed? scp-session-closed? scp-session-closed?-set!)))

  (define scp-default-timeout-ms 30000)

  (define ensure-success
    (lambda (who x)
      (cond
       [(ffi-error? x)
        (raise-net-error who 'scp (ffi-error-message x) x)]
       [else x])))

  (define ensure-session-open
    (lambda (who session)
      (when (scp-session-closed? session)
        (raise-net-error who 'scp "SCP session is closed" session))
      (%ssh-session-handle (scp-session-ssh-session session))
      session))

  (define check-timeout-ms
    (lambda (who timeout-ms)
      (unless (fixnum? timeout-ms)
        (errorf who "expected timeout fixnum, given ~s" timeout-ms))
      (when (fx< timeout-ms 0)
        (errorf who "timeout must be non-negative, given ~s" timeout-ms))
      timeout-ms))

  (define ensure-user-maybe
    (lambda (who user)
      (unless (or (string? user) (eq? user #f))
        (errorf who "expected string or #f, given ~s" user))))

  (define normalize-auth-kind
    (lambda (who auth-kind)
      (cond
       [(symbol? auth-kind) auth-kind]
       [(string? auth-kind)
        (let ([sym (string->symbol auth-kind)])
          (case sym
            [(agent password publickey) sym]
            [else (errorf who "invalid scp auth kind ~s" auth-kind)]))]
       [else
        (errorf who "expected auth kind symbol or string, given ~s" auth-kind)])))

  (define authenticate-ssh!
    (lambda (who session user auth-kind auth-arg)
      (case (normalize-auth-kind who auth-kind)
        [(agent)
         (when (not (eq? auth-arg #f))
           (errorf who "agent authentication does not take an extra argument"))
         (ssh-auth-agent! session user)]
        [(password)
         (unless (string? auth-arg)
           (errorf who "password authentication requires a string password"))
         (ssh-auth-password! session user auth-arg)]
        [(publickey)
         (unless (or (string? auth-arg) (eq? auth-arg #f))
           (errorf who "publickey authentication requires a string passphrase or #f"))
         (ssh-auth-publickey! session user auth-arg)])))

  (define ensure-no-pending-mismatch
    (lambda (who session kind args)
      (let ([pending (scp-session-pending session)])
        (when (and pending
                   (or (not (eq? (scp-pending-kind pending) kind))
                       (not (equal? (scp-pending-args pending) args))))
          (raise-net-error who 'scp "another nonblocking SCP operation is pending" pending)))))

  (define open-pending-notifier
    (lambda ()
      (let ([listener (open-socket 'inet 'stream)]
            [client #f]
            [server #f])
        (guard (c [else
                   (when server
                     (guard (x [else #f])
                       (close-socket server)))
                   (when client
                     (guard (x [else #f])
                       (close-socket client)))
                   (guard (x [else #f])
                     (close-socket listener))
                   (raise c)])
          (dynamic-wind
            void
            (lambda ()
              (socket-set-option! listener 'reuse-address #t)
              (socket-bind! listener (make-socket-address 'inet "127.0.0.1" 0))
              (socket-listen! listener 1)
              (let ([addr (socket-local-address listener)])
                (set! client (open-socket 'inet 'stream))
                (socket-connect! client addr)
                (let-values ([(accepted peer) (socket-accept listener)])
                  (set! server accepted)
                  (values server client))))
            (lambda ()
              (guard (c [else #f])
                (close-socket listener))))))))

  (define close-pending-notifier!
    (lambda (pending)
      (guard (c [else #f])
        (close-socket (scp-pending-reader pending)))
      (guard (c [else #f])
        (close-socket (scp-pending-writer pending)))))

  (define start-pending!
    (lambda (session kind args thunk)
      (let-values ([(reader writer) (open-pending-notifier)])
        (letrec ([pending
                  (%make-scp-pending-op
                   kind
                   args
                   reader
                   writer
                   (fork-thread
                    (lambda ()
                      (let ([result
                             (guard (c [else c])
                               (thunk))])
                        (when (scp-pending-cancelled? pending)
                          (set! result #f))
                        (scp-pending-result-set! pending result))
                      (scp-pending-done?-set! pending #t)
                      (guard (c [else #f])
                        (socket-send-all writer #vu8(1)))))
                   #f
                   #f
                   #f)])
          (scp-session-pending-set! session pending)
          pending))))

  (define pending-ready?
    (lambda (pending)
      (or (scp-pending-done? pending)
          (let* ([target (make-poll-target (scp-pending-reader pending)
                                           '(read error hup invalid))]
                 [ready (car (poll/nonblocking (list target)))])
            (memq 'read (poll-target-ready-events ready))))))

  (define finish-pending!
    (lambda (who session pending)
      (scp-session-pending-set! session #f)
      (thread-join (scp-pending-thread pending))
      (close-pending-notifier! pending)
      (let ([result (scp-pending-result pending)])
        (if (condition? result)
            (raise result)
            result))))

  (define cancel-pending!
    (lambda (session pending)
      (scp-pending-cancelled?-set! pending #t)
      (close-pending-notifier! pending)
      (scp-session-pending-set! session #f)
      (thread-join (scp-pending-thread pending))
      session))

  (define scp-transfer/nonblocking
    (lambda (who session kind args thunk)
      (ensure-session-open who session)
      (ensure-no-pending-mismatch who session kind args)
      (let ([pending (or (scp-session-pending session)
                         (start-pending! session kind args thunk))])
        (if (pending-ready? pending)
            (finish-pending! who session pending)
            #f))))

  (define scp-download*
    (lambda (who session remote-path local-path timeout-ms)
      (ensure-session-open who session)
      (ensure-success who
                      (ffi-net-scp-download-file
                       (%ssh-session-handle (scp-session-ssh-session session))
                       remote-path
                       local-path
                       timeout-ms))
      local-path))

  (define scp-upload*
    (lambda (who session local-path remote-path timeout-ms)
      (ensure-session-open who session)
      (unless (file-regular? local-path #t)
        (errorf who "local file expected, given ~a" local-path))
      (ensure-success who
                      (ffi-net-scp-upload-file
                       (%ssh-session-handle (scp-session-ssh-session session))
                       local-path
                       remote-path
                       timeout-ms))
      remote-path))

  (define scp-copy-directory*
    (lambda (who session direction source-path target-path timeout-ms)
      (ensure-session-open who session)
      (case direction
        [(upload)
         (unless (file-directory? source-path #t)
           (errorf who "local directory expected, given ~a" source-path))
         (ensure-success who
                         (ffi-net-scp-upload-directory
                          (%ssh-session-handle (scp-session-ssh-session session))
                          source-path
                          target-path
                          timeout-ms))
         target-path]
        [(download)
         (ensure-success who
                         (ffi-net-scp-download-directory
                          (%ssh-session-handle (scp-session-ssh-session session))
                          source-path
                          target-path
                          timeout-ms))
         target-path]
        [else
         (errorf who "direction must be one of '(upload download), given ~s" direction)])))

  #|proc:scp-open
The `scp-open` procedure wraps an authenticated SSH session, or opens and authenticates one, for subsequent SCP transfers.
|#
  (define-who scp-open
    (case-lambda
      [(session)
       (scp-open session scp-default-timeout-ms)]
      [(session timeout-ms)
       (pcheck ([ssh-session? session] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (%make-scp-session session timeout-ms #f #f #f))]
      [(host port user auth-kind auth-arg)
       (scp-open host port user auth-kind auth-arg scp-default-timeout-ms)]
      [(host port user auth-kind auth-arg timeout-ms)
       (pcheck ([string? host] [fixnum? port timeout-ms])
               (check-port who port)
               (check-timeout-ms who timeout-ms)
               (ensure-user-maybe who user)
               (let ([ssh-session (ssh-open host port user timeout-ms)]
                     [ok? #f])
                 (dynamic-wind
                   void
                   (lambda ()
                     (authenticate-ssh! who ssh-session user auth-kind auth-arg)
                     (set! ok? #t)
                     (%make-scp-session ssh-session timeout-ms #t #f #f))
                   (lambda ()
                     (unless ok?
                       (guard (c [else #f])
                         (ssh-close ssh-session)))))))]))

  #|proc:scp-close
The `scp-close` procedure closes an SCP session and, if it owns the wrapped SSH session, closes that SSH session as well.
|#
  (define-who scp-close
    (lambda (session)
      (pcheck ([scp-session? session])
              (unless (scp-session-closed? session)
                (let ([pending (scp-session-pending session)])
                  (when pending
                    (cancel-pending! session pending)))
                (when (and (scp-session-owns-ssh? session)
                           (guard (c [else #f])
                             (not (fx= 0 (%ssh-session-handle
                                          (scp-session-ssh-session session))))))
                  (ssh-close (scp-session-ssh-session session)))
                (scp-session-closed?-set! session #t))
              session)))

  #|proc:scp-cancel-pending!
The `scp-cancel-pending!` procedure cancels and discards the currently pending non-blocking SCP operation on a session, if any.
|#
  (define-who scp-cancel-pending!
    (lambda (session)
      (pcheck ([scp-session? session])
              (let ([pending (scp-session-pending session)])
                (when pending
                  (cancel-pending! session pending)))
              session)))

  #|proc:scp-download
The `scp-download` procedure downloads a single remote file to the exact local target path.
|#
  (define-who scp-download
    (case-lambda
      [(session remote-path local-path)
       (scp-download session remote-path local-path (scp-session-timeout-ms session))]
      [(session remote-path local-path timeout-ms)
       (pcheck ([scp-session? session] [string? remote-path local-path] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (scp-download* who session remote-path local-path timeout-ms))]))

  #|proc:scp-upload
The `scp-upload` procedure uploads a single local file to the exact remote target path.
|#
  (define-who scp-upload
    (case-lambda
      [(session local-path remote-path)
       (scp-upload session local-path remote-path (scp-session-timeout-ms session))]
      [(session local-path remote-path timeout-ms)
       (pcheck ([scp-session? session] [string? local-path remote-path] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (scp-upload* who session local-path remote-path timeout-ms))]))

  #|proc:scp-download/nonblocking
The `scp-download/nonblocking` procedure progresses a file download without blocking and returns `#f` while the transfer is still pending.
|#
  (define-who scp-download/nonblocking
    (case-lambda
      [(session remote-path local-path)
       (scp-download/nonblocking session remote-path local-path (scp-session-timeout-ms session))]
      [(session remote-path local-path timeout-ms)
       (pcheck ([scp-session? session] [string? remote-path local-path] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (scp-transfer/nonblocking
                who
                session
                'download
                (list remote-path local-path timeout-ms)
                (lambda ()
                  (scp-download* who session remote-path local-path timeout-ms))))]))

  #|proc:scp-upload/nonblocking
The `scp-upload/nonblocking` procedure progresses a file upload without blocking and returns `#f` while the transfer is still pending.
|#
  (define-who scp-upload/nonblocking
    (case-lambda
      [(session local-path remote-path)
       (scp-upload/nonblocking session local-path remote-path (scp-session-timeout-ms session))]
      [(session local-path remote-path timeout-ms)
       (pcheck ([scp-session? session] [string? local-path remote-path] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (scp-transfer/nonblocking
                who
                session
                'upload
                (list local-path remote-path timeout-ms)
                (lambda ()
                  (scp-upload* who session local-path remote-path timeout-ms))))]))

  #|proc:scp-copy-directory
The `scp-copy-directory` procedure recursively copies a directory tree in the specified `direction`, using exact root-path semantics for the local and remote targets.
|#
  (define-who scp-copy-directory
    (case-lambda
      [(session direction source-path target-path)
       (scp-copy-directory session
                           direction
                           source-path
                           target-path
                           (scp-session-timeout-ms session))]
      [(session direction source-path target-path timeout-ms)
       (pcheck ([scp-session? session] [string? source-path target-path] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (scp-copy-directory* who session direction source-path target-path timeout-ms))]))

  #|proc:scp-copy-directory/nonblocking
The `scp-copy-directory/nonblocking` procedure progresses a recursive directory copy without blocking and returns `#f` while the transfer is still pending.
|#
  (define-who scp-copy-directory/nonblocking
    (case-lambda
      [(session direction source-path target-path)
       (scp-copy-directory/nonblocking session
                                       direction
                                       source-path
                                       target-path
                                       (scp-session-timeout-ms session))]
      [(session direction source-path target-path timeout-ms)
       (pcheck ([scp-session? session] [string? source-path target-path] [fixnum? timeout-ms])
               (check-timeout-ms who timeout-ms)
               (scp-transfer/nonblocking
                who
                session
                'copy-directory
                (list direction source-path target-path timeout-ms)
                (lambda ()
                  (scp-copy-directory* who
                                       session
                                       direction
                                       source-path
                                       target-path
                                       timeout-ms))))]))

  #|proc:call-with-scp-session
The `call-with-scp-session` procedure opens an SCP session, applies a procedure, and closes the session afterwards.
|#
  (define-who call-with-scp-session
    (case-lambda
      [(session proc)
       (call-with-scp-session session scp-default-timeout-ms proc)]
      [(session timeout-ms proc)
       (pcheck ([ssh-session? session] [fixnum? timeout-ms] [procedure? proc])
               (check-timeout-ms who timeout-ms)
               (let ([scp-session (scp-open session timeout-ms)])
                 (dynamic-wind
                   void
                   (lambda () (proc scp-session))
                   (lambda () (scp-close scp-session)))))]
      [(host port user auth-kind auth-arg proc)
       (call-with-scp-session host
                              port
                              user
                              auth-kind
                              auth-arg
                              scp-default-timeout-ms
                              proc)]
      [(host port user auth-kind auth-arg timeout-ms proc)
       (pcheck ([string? host] [fixnum? port timeout-ms] [procedure? proc])
               (ensure-user-maybe who user)
               (check-port who port)
               (check-timeout-ms who timeout-ms)
               (let ([scp-session (scp-open host port user auth-kind auth-arg timeout-ms)])
                 (dynamic-wind
                   void
                   (lambda () (proc scp-session))
                   (lambda () (scp-close scp-session)))))]))
  )
