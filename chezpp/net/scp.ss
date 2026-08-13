(library (chezpp net scp)
  (export scp-session?
          scp-attributes?
          scp-attributes-path
          scp-attributes-type
          scp-attributes-size
          scp-attributes-permissions
          scp-attributes-modification-time
          scp-stat
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
          (chezpp net operation)
          (chezpp net transfer)
          (chezpp net address)
          (chezpp net socket)
          (chezpp net private)
          (chezpp net ssh))

  (define-record-type (scp-session %make-scp-session scp-session?)
    (sealed #t)
    (opaque #f)
    (fields (immutable ssh-session scp-session-ssh-session)
            (immutable timeout-ms scp-session-timeout-ms)
            (immutable owns-ssh? scp-session-owns-ssh?)
            (mutable pending scp-session-pending scp-session-pending-set!)
            (mutable closed? scp-session-closed? scp-session-closed?-set!)))

  #|record:scp-attributes
The `scp-attributes` record is an immutable snapshot of a remote path. Its fields contain the
path, type, optional byte size, numeric permissions, and Unix modification time.
|#
  (define-record-type (scp-attributes %make-scp-attributes scp-attributes?)
    (sealed #t)
    (opaque #f)
    (fields (immutable path scp-attributes-path)
            (immutable type scp-attributes-type)
            (immutable size scp-attributes-size)
            (immutable permissions scp-attributes-permissions)
            (immutable modification-time scp-attributes-modification-time)))

  (define scp-default-timeout-ms 30000)

  (define scp-current-monotonic-ms
    (lambda ()
      (let ([time (current-time 'time-monotonic)])
        (+ (* (time-second time) 1000)
           (quotient (time-nanosecond time) 1000000)))))

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

  (define scp-vector->attributes
    (lambda (path vector)
      (%make-scp-attributes
       path
       (case (vector-ref vector 1)
         [(1) 'regular] [(2) 'directory] [(3) 'symlink] [(4) 'special]
         [else 'unknown])
       (vector-ref vector 2)
       (vector-ref vector 3)
       (vector-ref vector 7))))

  #|proc:scp-stat
The `scp-stat` procedure returns a stable `scp-attributes` record for remote `path` through
`session`, or `#f` when the path does not exist.
|#
  (define-who scp-stat
    (lambda (session path)
      (pcheck ([scp-session? session] [string? path])
              (ensure-session-open who session)
              (let ([answer (ensure-success
                             who
                             (ffi-net-scp-stat
                              (%ssh-session-handle (scp-session-ssh-session session)) path))])
                (and answer (scp-vector->attributes path answer))))))

  (define ensure-scp-resume-supported
    (lambda (who policy)
      (unless (eq? 'never (transfer-policy-resume policy))
        (raise-net-error who 'unsupported
                         "SCP resume requires a verified server-side restart helper" policy))))

  (define scp-download/policy
    (lambda (who session remote-path local-path policy)
      (ensure-scp-resume-supported who policy)
      (let ([exists? (file-exists? local-path)]
            [overwrite (transfer-policy-overwrite policy)])
        (cond [(and exists? (eq? overwrite 'skip)) local-path]
              [(and exists? (eq? overwrite 'error))
               (errorf who "local destination exists: ~a" local-path)]
              [else
               (scp-download session remote-path local-path
                             (scp-session-timeout-ms session))]))))

  (define scp-upload/policy
    (lambda (who session local-path remote-path policy)
      (ensure-scp-resume-supported who policy)
      (let ([attributes (scp-stat session remote-path)]
            [overwrite (transfer-policy-overwrite policy)])
        (cond [(and attributes (eq? overwrite 'skip)) remote-path]
              [(and attributes (eq? overwrite 'error))
               (errorf who "remote destination exists: ~a" remote-path)]
              [else
               (scp-upload session local-path remote-path
                           (scp-session-timeout-ms session))]))))

  (define ensure-no-pending-mismatch
    (lambda (who session kind args)
      (let ([pending (scp-session-pending session)])
        (when (and pending
                   (eq? 'pending (net-operation-state pending))
                   (not (eq? (net-operation-kind pending) kind)))
          (raise-net-error who 'scp "another nonblocking SCP operation is pending" pending)))))

  (define cancel-pending!
    (lambda (session pending)
      (net-operation-cancel! pending)
      (scp-session-pending-set! session #f)
      session))

  (define scp-transfer/nonblocking
    (lambda (who session kind args thunk)
      (ensure-session-open who session)
      (ensure-no-pending-mismatch who session kind args)
      (let ([pending (scp-session-pending session)])
        (if (and pending (eq? 'pending (net-operation-state pending)))
            pending
            (let* ([native? (memq kind '(scp-download scp-upload))]
                   [deadline-ms
                    (and native?
                         (+ (scp-current-monotonic-ms) (caddr args)))]
                   [handle (and native?
                                (let ([started
                                       (ffi-net-scp-transfer-start
                                        (%ssh-session-handle (scp-session-ssh-session session))
                                        (if (eq? kind 'scp-download) 0 1)
                                        (if (eq? kind 'scp-download) (car args) (car args))
                                        (if (eq? kind 'scp-download) (cadr args) (cadr args)))])
                                  (if (ffi-error? started)
                                      (raise-net-error who 'scp (ffi-error-message started) started)
                                      (vector-ref started 1))))]
                   [operation
                    (make-net-operation
                     kind
                     (lambda ()
                       (guard (failure [else (net-operation-failed failure)])
                         (if native?
                             (let ([status (ffi-net-scp-transfer-step handle)])
                               (case (and (vector? status) (vector-ref status 0))
                                 [(pending)
                                  (let ([target (vector-ref status 1)])
                                    (net-operation-pending
                                     (list (make-poll-target
                                            (vector-ref target 0)
                                            (vector-ref target 1)))
                                     deadline-ms))]
                                 [(completed) (net-operation-completed
                                               (if (eq? kind 'scp-download) (cadr args) (cadr args)))]
                                 [else (net-operation-failed
                                        (make-net-error who 'scp "SCP transfer failed" status))]))
                             (net-operation-completed (thunk)))))
                     (if native? (lambda () (ffi-net-scp-transfer-cancel handle)) void)
                     (lambda ()
                       (when native? (ffi-net-scp-transfer-close handle))
                       (scp-session-pending-set! session #f)))])
              (scp-session-pending-set! session operation)
              operation)))))

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
The `scp-cancel-pending!` procedure cancels the pending transfer on `session`, if any.
The `session` parameter is an open SCP session.
The return value is `session`; partial downloads are removed during cleanup.
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
       (pcheck ([scp-session? session] [string? remote-path local-path])
               (if (transfer-policy? timeout-ms)
                   (scp-download/policy who session remote-path local-path timeout-ms)
                   (begin
                     (check-timeout-ms who timeout-ms)
                     (net-operation-wait
                      (scp-download/nonblocking session remote-path local-path timeout-ms)))))]))

  #|proc:scp-upload
The `scp-upload` procedure uploads a single local file to the exact remote target path.
|#
  (define-who scp-upload
    (case-lambda
      [(session local-path remote-path)
       (scp-upload session local-path remote-path (scp-session-timeout-ms session))]
      [(session local-path remote-path timeout-ms)
       (pcheck ([scp-session? session] [string? local-path remote-path])
               (if (transfer-policy? timeout-ms)
                   (scp-upload/policy who session local-path remote-path timeout-ms)
                   (begin
                     (check-timeout-ms who timeout-ms)
                     (net-operation-wait
                      (scp-upload/nonblocking session local-path remote-path timeout-ms)))))]))

  #|proc:scp-download/nonblocking
The `scp-download/nonblocking` procedure constructs a file download operation.
The `session`, `remote-path`, `local-path`, and optional `timeout-ms` describe the transfer.
The return value is a `net-operation` whose successful result is `local-path`.
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
                'scp-download
                (list remote-path local-path timeout-ms)
                (lambda ()
                  (scp-download* who session remote-path local-path timeout-ms))))]))

  #|proc:scp-upload/nonblocking
The `scp-upload/nonblocking` procedure constructs a file upload operation.
The `session`, `local-path`, `remote-path`, and optional `timeout-ms` describe the transfer.
The return value is a `net-operation` whose successful result is `remote-path`.
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
                'scp-upload
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
               (net-operation-wait
                (scp-copy-directory/nonblocking
                 session direction source-path target-path timeout-ms)))]))

  #|proc:scp-copy-directory/nonblocking
The `scp-copy-directory/nonblocking` procedure constructs a recursive copy operation.
The `direction`, source, target, and optional timeout parameters describe the transfer.
The return value is a `net-operation` whose successful result is the target path.
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
                'scp-copy-directory
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
