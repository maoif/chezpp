(library (chezpp net ftp)
  (export ftp-session?
          ftp-open
          ftp-close
          ftp-login!
          ftp-quit!
          ftp-list
          ftp-list/nonblocking
          ftp-download
          ftp-download/nonblocking
          ftp-upload
          ftp-upload/nonblocking
          ftp-delete!
          ftp-mkdir!
          ftp-rmdir!
          ftp-rename!
          ftp-cwd!
          ftp-pwd
          ftp-passive-mode!
          ftp-active-mode!
          call-with-ftp-session
          open-ftp-input-port
          open-ftp-output-port)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp string)
          (chezpp net uri)
          (chezpp net errors)
          (chezpp net address)
          (chezpp net socket)
          (chezpp net poll)
          (chezpp net ffi)
          (chezpp net private))

  (define-record-type (ftp-pending-op %make-ftp-pending-op ftp-pending-op?)
    (sealed #t)
    (opaque #f)
    (fields (immutable kind ftp-pending-kind)
            (immutable args ftp-pending-args)
            (immutable reader ftp-pending-reader)
            (immutable writer ftp-pending-writer)
            (immutable thread ftp-pending-thread)
            (mutable done? ftp-pending-done? ftp-pending-done?-set!)
            (mutable result ftp-pending-result ftp-pending-result-set!)))

  (define-record-type (ftp-session %make-ftp-session ftp-session?)
    (sealed #t)
    (opaque #f)
    (fields (immutable uri ftp-session-uri)
            (mutable username ftp-session-username ftp-session-username-set!)
            (mutable password ftp-session-password ftp-session-password-set!)
            (mutable cwd ftp-session-cwd ftp-session-cwd-set!)
            (mutable passive? ftp-session-passive? ftp-session-passive?-set!)
            (mutable timeout-ms ftp-session-timeout-ms ftp-session-timeout-ms-set!)
            (mutable verify-peer? ftp-session-verify-peer? ftp-session-verify-peer?-set!)
            (mutable verify-host? ftp-session-verify-host? ftp-session-verify-host?-set!)
            (mutable pending ftp-session-pending ftp-session-pending-set!)
            (mutable closed? ftp-session-closed? ftp-session-closed?-set!)))

  (define ftp-default-timeout-ms 30000)
  (define ftp-temp-counter 0)

  (define ensure-session-open
    (lambda (who session)
      (when (ftp-session-closed? session)
        (raise-net-error who 'ftp "FTP session is closed" session))))

  (define ensure-no-pending-mismatch
    (lambda (who session kind args)
      (let ([pending (ftp-session-pending session)])
        (when (and pending
                   (or (not (eq? (ftp-pending-kind pending) kind))
                       (not (equal? (ftp-pending-args pending) args))))
          (raise-net-error who 'ftp "another nonblocking FTP operation is pending" pending)))))

  (define normalize-ftp-uri
    (lambda (who value)
      (let ([u (cond
                [(uri? value) value]
                [(string? value)
                 (or (string->uri value)
                     (errorf who "invalid URI string ~s" value))]
                [else
                 (errorf who "expected URI object or string, given ~s" value)])])
        (unless (member (uri-scheme u) '("ftp" "ftps"))
          (errorf who "expected ftp or ftps URI, given ~s" (uri-scheme u)))
        (unless (uri-host u)
          (errorf who "FTP URI requires a host: ~s" (uri->string u)))
        u)))

  (define split-userinfo
    (lambda (userinfo)
      (if (and userinfo (not (string=? userinfo "")))
          (let ([i (string-search userinfo #\:)])
            (if i
                (values (substring userinfo 0 i)
                        (substring userinfo (+ i 1) (string-length userinfo)))
                (values userinfo "")))
          (values "" ""))))

  (define path-join
    (lambda (segments)
      (let loop ([rest segments] [out ""])
        (if (null? rest)
            out
            (loop (cdr rest)
                  (if (string=? out "")
                      (car rest)
                      (string-append out "/" (car rest))))))))

  (define normalize-absolute-path
    (lambda (path)
      (let loop ([rest (string-split path #\/)] [stack '()])
        (if (null? rest)
            (let ([joined (path-join (reverse stack))])
              (if (string=? joined "")
                  "/"
                  (string-append "/" joined)))
            (let ([part (car rest)])
              (cond
               [(or (string=? part "") (string=? part "."))
                (loop (cdr rest) stack)]
               [(string=? part "..")
                (loop (cdr rest) (if (null? stack) '() (cdr stack)))]
               [else
                (loop (cdr rest) (cons part stack))]))))))

  (define resolve-session-path
    (lambda (session path)
      (let ([path (if (or (not path) (string=? path "")) "." path)])
        (normalize-absolute-path
         (if (and (> (string-length path) 0)
                  (char=? (string-ref path 0) #\/))
             path
             (let ([cwd (ftp-session-cwd session)])
               (if (string=? cwd "/")
                   (string-append "/" path)
                   (string-append cwd "/" path))))))))

  (define session-origin
    (lambda (session)
      (let* ([u (ftp-session-uri session)]
             [scheme (uri-scheme u)]
             [host (uri-host u)]
             [port (uri-port u)]
             [default-port (if (string=? scheme "ftps") 990 21)])
        (string-append scheme
                       "://"
                       host
                       (if (and port (not (= port default-port)))
                           (format ":~a" port)
                           "")))))

  (define session-path-url
    (lambda (session path)
      (string-append (session-origin session) (resolve-session-path session path))))

  (define session-directory-url
    (lambda (session path)
      (let ([url (session-path-url session path)])
        (if (char=? (string-ref url (- (string-length url) 1)) #\/)
            url
            (string-append url "/")))))

  (define session-base-url
    (lambda (session)
      (let ([cwd (ftp-session-cwd session)])
        (string-append (session-origin session)
                       (if (string=? cwd "/")
                           "/"
                           (string-append cwd "/"))))))

  (define session-use-tls?
    (lambda (session)
      (string=? (uri-scheme (ftp-session-uri session)) "ftps")))

  (define ensure-success
    (lambda (who x)
      (when (ffi-error? x)
        (raise-net-error who 'ftp (ffi-error-message x) x))
      x))

  (define ftp-list*
    (lambda (who session path)
      (ensure-success
       who
       (ffi-net-ftp-list (session-directory-url session path)
                         (ftp-session-username session)
                         (ftp-session-password session)
                         (if (ftp-session-passive? session) 1 0)
                         (ftp-session-timeout-ms session)
                         (if (session-use-tls? session) 1 0)
                         (if (ftp-session-verify-peer? session) 1 0)
                         (if (ftp-session-verify-host? session) 1 0)))))

  (define ftp-command*
    (lambda (who session cmd)
      (ensure-success
       who
       (ffi-net-ftp-command (session-base-url session)
                            (ftp-session-username session)
                            (ftp-session-password session)
                            (if (ftp-session-passive? session) 1 0)
                            (ftp-session-timeout-ms session)
                            (if (session-use-tls? session) 1 0)
                            (if (ftp-session-verify-peer? session) 1 0)
                            (if (ftp-session-verify-host? session) 1 0)
                            cmd))))

  (define next-temp-path
    (lambda (suffix)
      (set! ftp-temp-counter (+ ftp-temp-counter 1))
      (let loop ()
        (let* ([t (current-time)]
               [path (format "/tmp/chezpp-net-ftp-~a-~a-~a~a"
                             (time-second t)
                             (time-nanosecond t)
                             ftp-temp-counter
                             suffix)])
          (if (file-exists? path)
              (begin
                (set! ftp-temp-counter (+ ftp-temp-counter 1))
                (loop))
              path)))))

  (define cleanup-temp-file!
    (lambda (path)
      (when (and path (file-exists? path))
        (guard (c [else #f])
          (delete-file path)))))

  (define close-pending-notifier!
    (lambda (pending)
      (guard (c [else #f])
        (close-socket (ftp-pending-reader pending)))
      (guard (c [else #f])
        (close-socket (ftp-pending-writer pending)))))

  (define open-pending-notifier
    (lambda ()
      (let ([listener (open-socket 'inet 'stream)]
            [client #f]
            [server #f])
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
              (close-socket listener)))))))

  (define start-pending!
    (lambda (session kind args thunk)
        (let-values ([(reader writer) (open-pending-notifier)])
          (letrec ([pending
                    (%make-ftp-pending-op
                     kind
                     args
                     reader
                     writer
                     (fork-thread
                      (lambda ()
                        (ftp-pending-result-set!
                         pending
                         (guard (c [else c])
                           (thunk)))
                        (ftp-pending-done?-set! pending #t)
                        (guard (c [else #f])
                          (socket-send-all writer #vu8(1)))))
                     #f
                     #f)])
            (ftp-session-pending-set! session pending)
            pending))))

  (define pending-ready?
    (lambda (pending)
      (or (ftp-pending-done? pending)
          (let* ([target (make-poll-target (ftp-pending-reader pending)
                                           '(read error hup invalid))]
                 [ready (car (poll/nonblocking (list target)))])
            (pair? (poll-target-ready-events ready))))))

  (define finish-pending!
    (lambda (who session pending)
      (ftp-session-pending-set! session #f)
      (thread-join (ftp-pending-thread pending))
      (close-pending-notifier! pending)
      (let ([result (ftp-pending-result pending)])
        (if (condition? result)
            (raise result)
            result))))

  (define ftp-transfer/nonblocking
    (lambda (who session kind args thunk)
      (ensure-session-open who session)
      (ensure-no-pending-mismatch who session kind args)
      (let ([pending (or (ftp-session-pending session)
                         (start-pending! session kind args thunk))])
        (if (pending-ready? pending)
            (finish-pending! who session pending)
            #f))))

  (define make-ftp-input-port
    (lambda (session remote-path)
      (let ([local-path (next-temp-path ".download")]
            [ip #f]
            [closed? #f])
        (define close!
          (lambda ()
            (unless closed?
              (set! closed? #t)
              (when ip
                (close-port ip)
                (set! ip #f))
              (cleanup-temp-file! local-path))))
        (dynamic-wind
          void
          (lambda ()
            (ftp-download session remote-path local-path)
            (set! ip (open-file-input-port local-path
                                           (file-options)
                                           (buffer-mode block)
                                           #f))
            (make-custom-binary-input-port
             "chezpp-ftp-input"
             (lambda (bv start count)
               (let ([n (get-bytevector-n! ip bv start count)])
                 (if (eof-object? n) 0 n)))
             (lambda () (port-position ip))
             (lambda (pos) (set-port-position! ip pos))
             (lambda () (close!) #t)))
          (lambda ()
            (when (and (not ip) (file-exists? local-path))
              (cleanup-temp-file! local-path)))))))

  (define make-ftp-output-port
    (lambda (session remote-path)
      (let* ([local-path (next-temp-path ".upload")]
             [op (open-file-output-port local-path
                                        (file-options no-fail replace)
                                        (buffer-mode block)
                                        #f)]
             [closed? #f])
        (define close!
          (lambda ()
            (unless closed?
              (set! closed? #t)
              (when op
                (close-port op)
                (set! op #f))
              (dynamic-wind
                void
                (lambda ()
                  (ftp-upload session local-path remote-path))
                (lambda ()
                  (cleanup-temp-file! local-path))))))
        (make-custom-binary-output-port
         "chezpp-ftp-output"
         (lambda (bv start count)
           (put-bytevector op bv start count)
           count)
         (lambda ()
           (flush-output-port op)
           #t)
         (lambda (pos)
           (set-port-position! op pos))
         (lambda ()
           (close!)
           #t)))))

  #|proc:ftp-open
The `ftp-open` procedure constructs an FTP or FTPS session record from an endpoint URI or host/port tuple.
|#
  (define-who ftp-open
    (case-lambda
      [(endpoint)
       (let ([u (normalize-ftp-uri who endpoint)])
         (let-values ([(user pass) (split-userinfo (uri-userinfo u))])
           (%make-ftp-session u
                              user
                              pass
                              (normalize-absolute-path
                               (if (or (not (uri-path u)) (string=? (uri-path u) ""))
                                   "/"
                                   (uri-path u)))
                              #t
                              ftp-default-timeout-ms
                              #f
                              #f
                              #f
                              #f)))]
      [(host port)
       (ftp-open host port #f)]
      [(host port secure?)
       (pcheck ([string? host] [fixnum? port] [boolean? secure?])
               (ftp-open
                (format "~a://~a:~a/"
                        (if secure? "ftps" "ftp")
                        host
                        port)))]))

  #|proc:ftp-close
The `ftp-close` procedure marks an FTP session as closed.
|#
  (define-who ftp-close
    (lambda (session)
      (pcheck ([ftp-session? session])
              (ftp-session-closed?-set! session #t)
              session)))

  #|proc:ftp-quit!
The `ftp-quit!` procedure closes an FTP session.
|#
  (define-who ftp-quit!
    (lambda (session)
      (ftp-close session)))

  #|proc:ftp-login!
The `ftp-login!` procedure updates the username and password stored on an FTP session.
|#
  (define-who ftp-login!
    (lambda (session username password)
      (pcheck ([ftp-session? session] [string? username password])
              (ensure-session-open who session)
              (ftp-session-username-set! session username)
              (ftp-session-password-set! session password)
              session)))

  #|proc:ftp-passive-mode!
The `ftp-passive-mode!` procedure switches an FTP session into passive mode.
|#
  (define-who ftp-passive-mode!
    (lambda (session)
      (pcheck ([ftp-session? session])
              (ensure-session-open who session)
              (ftp-session-passive?-set! session #t)
              #t)))

  #|proc:ftp-active-mode!
The `ftp-active-mode!` procedure switches an FTP session into active mode.
|#
  (define-who ftp-active-mode!
    (lambda (session)
      (pcheck ([ftp-session? session])
              (ensure-session-open who session)
              (ftp-session-passive?-set! session #f)
              #f)))

  #|proc:ftp-cwd!
The `ftp-cwd!` procedure updates the current working directory stored on an FTP session.
|#
  (define-who ftp-cwd!
    (lambda (session path)
      (pcheck ([ftp-session? session] [string? path])
              (ensure-session-open who session)
              (ftp-session-cwd-set! session (resolve-session-path session path))
              session)))

  #|proc:ftp-pwd
The `ftp-pwd` procedure returns the current working directory stored on an FTP session.
|#
  (define-who ftp-pwd
    (lambda (session)
      (pcheck ([ftp-session? session])
              (ensure-session-open who session)
              (ftp-session-cwd session))))

  #|proc:ftp-list
The `ftp-list` procedure returns the names of entries in a remote directory.
|#
  (define-who ftp-list
    (case-lambda
      [(session)
       (ftp-list session ".")]
      [(session path)
       (pcheck ([ftp-session? session] [string? path])
               (ensure-session-open who session)
               (let ([bv (ftp-list* who session path)])
                 (let loop ([lines (string-split (utf8->string bv) #\newline)] [out '()])
                   (if (null? lines)
                       (reverse out)
                       (let ([line (string-trim-right (car lines) #\return)])
                         (loop (cdr lines)
                               (if (string=? line "")
                                   out
                                   (cons line out))))))))]))

  #|proc:ftp-list/nonblocking
The `ftp-list/nonblocking` procedure progresses a directory listing without blocking and returns `#f` while the listing is still pending.
|#
  (define-who ftp-list/nonblocking
    (case-lambda
      [(session)
       (ftp-list/nonblocking session ".")]
      [(session path)
       (pcheck ([ftp-session? session] [string? path])
               (ftp-transfer/nonblocking who
                                         session
                                         'list
                                         (list path)
                                         (lambda ()
                                           (ftp-list session path))))]))

  #|proc:ftp-download
The `ftp-download` procedure downloads a remote file to a local pathname.
|#
  (define-who ftp-download
    (lambda (session remote-path local-path)
      (pcheck ([ftp-session? session] [string? remote-path local-path])
              (ensure-session-open who session)
              (ensure-success
               who
               (ffi-net-ftp-download (session-path-url session remote-path)
                                     local-path
                                     (ftp-session-username session)
                                     (ftp-session-password session)
                                     (if (ftp-session-passive? session) 1 0)
                                     (ftp-session-timeout-ms session)
                                     (if (session-use-tls? session) 1 0)
                                     (if (ftp-session-verify-peer? session) 1 0)
                                     (if (ftp-session-verify-host? session) 1 0)))
              local-path)))

  #|proc:ftp-download/nonblocking
The `ftp-download/nonblocking` procedure progresses a file download without blocking and returns `#f` while the download is still pending.
|#
  (define-who ftp-download/nonblocking
    (lambda (session remote-path local-path)
      (pcheck ([ftp-session? session] [string? remote-path local-path])
              (ftp-transfer/nonblocking who
                                        session
                                        'download
                                        (list remote-path local-path)
                                        (lambda ()
                                          (ftp-download session remote-path local-path))))))

  #|proc:ftp-upload
The `ftp-upload` procedure uploads a local file to a remote pathname.
|#
  (define-who ftp-upload
    (lambda (session local-path remote-path)
      (pcheck ([ftp-session? session] [string? local-path remote-path])
              (ensure-session-open who session)
              (ensure-success
               who
               (ffi-net-ftp-upload (session-path-url session remote-path)
                                   local-path
                                   (ftp-session-username session)
                                   (ftp-session-password session)
                                   (if (ftp-session-passive? session) 1 0)
                                   (ftp-session-timeout-ms session)
                                   (if (session-use-tls? session) 1 0)
                                   (if (ftp-session-verify-peer? session) 1 0)
                                   (if (ftp-session-verify-host? session) 1 0)))
              remote-path)))

  #|proc:ftp-upload/nonblocking
The `ftp-upload/nonblocking` procedure progresses a file upload without blocking and returns `#f` while the upload is still pending.
|#
  (define-who ftp-upload/nonblocking
    (lambda (session local-path remote-path)
      (pcheck ([ftp-session? session] [string? local-path remote-path])
              (ftp-transfer/nonblocking who
                                        session
                                        'upload
                                        (list local-path remote-path)
                                        (lambda ()
                                          (ftp-upload session local-path remote-path))))))

  #|proc:ftp-delete!
The `ftp-delete!` procedure deletes a remote file.
|#
  (define-who ftp-delete!
    (lambda (session remote-path)
      (pcheck ([ftp-session? session] [string? remote-path])
              (ensure-session-open who session)
              (ftp-command* who session
                            (format "DELE ~a" (resolve-session-path session remote-path)))
              session)))

  #|proc:ftp-mkdir!
The `ftp-mkdir!` procedure creates a remote directory.
|#
  (define-who ftp-mkdir!
    (lambda (session remote-path)
      (pcheck ([ftp-session? session] [string? remote-path])
              (ensure-session-open who session)
              (ftp-command* who session
                            (format "MKD ~a" (resolve-session-path session remote-path)))
              session)))

  #|proc:ftp-rmdir!
The `ftp-rmdir!` procedure removes an empty remote directory.
|#
  (define-who ftp-rmdir!
    (lambda (session remote-path)
      (pcheck ([ftp-session? session] [string? remote-path])
              (ensure-session-open who session)
              (ftp-command* who session
                            (format "RMD ~a" (resolve-session-path session remote-path)))
              session)))

  #|proc:ftp-rename!
The `ftp-rename!` procedure renames or moves a remote file or directory.
|#
  (define-who ftp-rename!
    (lambda (session from-path to-path)
      (pcheck ([ftp-session? session] [string? from-path to-path])
              (ensure-session-open who session)
              (ensure-success
               who
               (ffi-net-ftp-rename (session-base-url session)
                                   (ftp-session-username session)
                                   (ftp-session-password session)
                                   (if (ftp-session-passive? session) 1 0)
                                   (ftp-session-timeout-ms session)
                                   (if (session-use-tls? session) 1 0)
                                   (if (ftp-session-verify-peer? session) 1 0)
                                   (if (ftp-session-verify-host? session) 1 0)
                                   (resolve-session-path session from-path)
                                   (resolve-session-path session to-path)))
              session)))

  #|proc:call-with-ftp-session
The `call-with-ftp-session` procedure opens an FTP session, applies a procedure to it, and closes it afterward.
|#
  (define-who call-with-ftp-session
    (case-lambda
      [(endpoint proc)
       (pcheck ([procedure? proc])
               (let ([session (ftp-open endpoint)])
                 (dynamic-wind
                   void
                   (lambda () (proc session))
                   (lambda () (ftp-close session)))))]
      [(host port proc)
       (call-with-ftp-session host port #f proc)]
      [(host port secure? proc)
       (pcheck ([procedure? proc])
               (let ([session (ftp-open host port secure?)])
                 (dynamic-wind
                   void
                   (lambda () (proc session))
                   (lambda () (ftp-close session)))))]))

  #|proc:open-ftp-input-port
The `open-ftp-input-port` procedure opens a binary input port for a remote FTP file.
|#
  (define-who open-ftp-input-port
    (lambda (session remote-path)
      (pcheck ([ftp-session? session] [string? remote-path])
              (ensure-session-open who session)
              (make-ftp-input-port session remote-path))))

  #|proc:open-ftp-output-port
The `open-ftp-output-port` procedure opens a binary output port that uploads its contents to a remote FTP file when closed.
|#
  (define-who open-ftp-output-port
    (lambda (session remote-path)
      (pcheck ([ftp-session? session] [string? remote-path])
              (ensure-session-open who session)
              (make-ftp-output-port session remote-path))))
  )
