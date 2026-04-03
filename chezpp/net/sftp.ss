(library (chezpp net sftp)
  (export sftp-session?
          sftp-file?
          sftp-open
          sftp-close
          sftp-list
          sftp-stat
          sftp-download
          sftp-upload
          sftp-delete!
          sftp-mkdir!
          sftp-rmdir!
          sftp-rename!
          sftp-open-file
          sftp-close-file
          sftp-read
          sftp-read!
          sftp-write
          sftp-write-all
          sftp-read/nonblocking
          sftp-read!/nonblocking
          sftp-write/nonblocking
          sftp-write-all/nonblocking
          call-with-sftp-session
          open-sftp-input-port
          open-sftp-output-port)
  (import (chezpp chez)
          (chezpp os)
          (chezpp utils)
          (chezpp net errors)
          (chezpp net ffi)
          (chezpp net private)
          (chezpp net ssh))

  (define-record-type (sftp-session %make-sftp-session sftp-session?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle sftp-session-handle sftp-session-handle-set!)
            (immutable ssh-session sftp-session-ssh-session)
            (mutable closed? sftp-session-closed? sftp-session-closed?-set!)))

  (define-record-type (sftp-file %make-sftp-file sftp-file?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle sftp-file-handle sftp-file-handle-set!)
            (immutable session sftp-file-session)
            (mutable closed? sftp-file-closed? sftp-file-closed?-set!)))

  (define sftp-default-mode #o644)
  (define sftp-open-rdonly (ffi-net-open-rdonly))
  (define sftp-open-wronly (ffi-net-open-wronly))
  (define sftp-open-rdwr (ffi-net-open-rdwr))
  (define sftp-open-append (ffi-net-open-append))
  (define sftp-open-creat (ffi-net-open-creat))
  (define sftp-open-trunc (ffi-net-open-trunc))
  (define sftp-open-excl (ffi-net-open-excl))
  (define sftp-open-text (ffi-net-open-text))

  (define ensure-success
    (lambda (who kind x)
      (cond
       [(ffi-error? x)
        (raise-net-error who kind (ffi-error-message x) x)]
       [else x])))

  (define ensure-session-open
    (lambda (who session)
      (when (sftp-session-closed? session)
        (raise-net-error who 'sftp "SFTP session is closed" session))))

  (define ensure-file-open
    (lambda (who file)
      (when (sftp-file-closed? file)
        (raise-net-error who 'sftp "SFTP file is closed" file))
      (ensure-session-open who (sftp-file-session file))))

  (define check-slice
    (lambda (who len start stop)
      (unless (and (fixnum? start) (fixnum? stop) (fx<= 0 start stop len))
        (errorf who "invalid slice [~a, ~a) for length ~a" start stop len))))

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

  (define await-timeout-result
    (lambda (who message timeout-ms thunk)
      (let ([deadline-ms (timeout->deadline-ms timeout-ms)])
        (let loop ()
          (let ([remaining-ms (remaining-timeout-ms deadline-ms)])
            (when (fx= remaining-ms 0)
              (raise-net-error who 'sftp message timeout-ms))
            (let ([x (thunk remaining-ms)])
              (if x
                  x
                  (begin
                    (milisleep 1)
                    (loop)))))))))

  (define read-result
    (lambda (who x)
      (cond
       [(or (bytevector? x) (eof-object? x)) x]
       [(ffi-would-block? x) #f]
       [else (ensure-success who 'sftp x)])))

  (define read-into-result
    (lambda (who x)
      (cond
       [(or (fixnum? x) (eof-object? x)) x]
       [(ffi-would-block? x) #f]
       [else (ensure-success who 'sftp x)])))

  (define write-result
    (lambda (who x)
      (cond
       [(fixnum? x) x]
       [(ffi-would-block? x) #f]
       [else (ensure-success who 'sftp x)])))

  (define open-flags->int
    (lambda (who flags)
      (let ([flag* (cond
                    [(symbol? flags) (list flags)]
                    [(list? flags) flags]
                    [else
                     (errorf who "expected symbol or list of symbols for sftp open flags")])])
        (define access
          (cond
           [(memq 'read/write flag*) sftp-open-rdwr]
           [(and (memq 'read flag*) (memq 'write flag*)) sftp-open-rdwr]
           [(memq 'write flag*) sftp-open-wronly]
           [else sftp-open-rdonly]))
        (let loop ([rest flag*] [out access])
          (if (null? rest)
              out
              (loop
               (cdr rest)
               (case (car rest)
                 [(read write read/write) out]
                 [(append) (fxlogor out sftp-open-append)]
                 [(create) (fxlogor out sftp-open-creat)]
                 [(truncate) (fxlogor out sftp-open-trunc)]
                 [(exclusive) (fxlogor out sftp-open-excl)]
                 [(text) (fxlogor out sftp-open-text)]
                 [else (errorf who "invalid sftp open flag ~s" (car rest))])))))))

  (define stat-vector->alist
    (lambda (v)
      `((name . ,(vector-ref v 0))
        (type . ,(case (vector-ref v 1)
                   [(1) 'regular]
                   [(2) 'directory]
                   [(3) 'symlink]
                   [(4) 'special]
                   [else 'unknown]))
        (size . ,(vector-ref v 2))
        (permissions . ,(vector-ref v 3))
        (uid . ,(vector-ref v 4))
        (gid . ,(vector-ref v 5))
        (atime . ,(vector-ref v 6))
        (mtime . ,(vector-ref v 7)))))

  (define make-binary-input-port
    (lambda (file)
      (make-custom-binary-input-port
       "chezpp-sftp-input"
       (lambda (bv start count)
         (let ([n (sftp-read! file bv start (fx+ start count))])
           (cond
            [(fixnum? n) n]
            [(eof-object? n) 0]
            [else (errorf 'open-sftp-input-port
                          "unexpected nonblocking result from blocking SFTP port read")])))
       (lambda () #f)
       (lambda (x) #f)
       (lambda () #t))))

  (define make-binary-output-port
    (lambda (file)
      (make-custom-binary-output-port
       "chezpp-sftp-output"
       (lambda (bv start count)
         (sftp-write-all file bv start (fx+ start count)))
       (lambda () #f)
       (lambda (x) #f)
       (lambda () #t))))

  (define copy-port-chunks
    (lambda (reader writer)
      (let loop ()
        (let ([chunk (reader)])
          (if (eof-object? chunk)
              #t
              (begin
                (writer chunk)
                (loop)))))))

  #|proc:sftp-open
The `sftp-open` procedure opens an SFTP session on top of an authenticated SSH session.
|#
  (define-who sftp-open
    (lambda (session)
      (pcheck ([ssh-session? session])
              (%make-sftp-session
               (ensure-success who 'sftp
                               (ffi-net-sftp-open (%ssh-session-handle session)))
               session
               #f))))

  #|proc:sftp-close
The `sftp-close` procedure closes an SFTP session.
|#
  (define-who sftp-close
    (lambda (session)
      (pcheck ([sftp-session? session])
              (unless (sftp-session-closed? session)
                (ensure-success who 'sftp (ffi-net-sftp-close (sftp-session-handle session)))
                (sftp-session-handle-set! session 0)
                (sftp-session-closed?-set! session #t))
              session)))

  #|proc:sftp-list
The `sftp-list` procedure returns the names of entries in a remote directory.
|#
  (define-who sftp-list
    (case-lambda
      [(session) (sftp-list session ".")]
      [(session path)
       (pcheck ([sftp-session? session] [string? path])
               (ensure-session-open who session)
               (reverse (ensure-success who 'sftp
                                        (ffi-net-sftp-list (sftp-session-handle session) path))))]))

  #|proc:sftp-stat
The `sftp-stat` procedure returns an alist describing a remote file or directory.
|#
  (define-who sftp-stat
    (lambda (session path)
      (pcheck ([sftp-session? session] [string? path])
              (ensure-session-open who session)
              (stat-vector->alist
               (ensure-success who 'sftp
                               (ffi-net-sftp-stat (sftp-session-handle session) path))))))

  #|proc:sftp-delete!
The `sftp-delete!` procedure deletes a remote file.
|#
  (define-who sftp-delete!
    (lambda (session path)
      (pcheck ([sftp-session? session] [string? path])
              (ensure-session-open who session)
              (ensure-success who 'sftp
                              (ffi-net-sftp-delete (sftp-session-handle session) path))
              session)))

  #|proc:sftp-mkdir!
The `sftp-mkdir!` procedure creates a remote directory.
|#
  (define-who sftp-mkdir!
    (case-lambda
      [(session path)
       (sftp-mkdir! session path sftp-default-mode)]
      [(session path mode)
       (pcheck ([sftp-session? session] [string? path] [fixnum? mode])
               (ensure-session-open who session)
               (ensure-success who 'sftp
                               (ffi-net-sftp-mkdir (sftp-session-handle session) path mode))
               session)]))

  #|proc:sftp-rmdir!
The `sftp-rmdir!` procedure removes an empty remote directory.
|#
  (define-who sftp-rmdir!
    (lambda (session path)
      (pcheck ([sftp-session? session] [string? path])
              (ensure-session-open who session)
              (ensure-success who 'sftp
                              (ffi-net-sftp-rmdir (sftp-session-handle session) path))
              session)))

  #|proc:sftp-rename!
The `sftp-rename!` procedure renames a remote path.
|#
  (define-who sftp-rename!
    (lambda (session from-path to-path)
      (pcheck ([sftp-session? session] [string? from-path to-path])
              (ensure-session-open who session)
              (ensure-success who 'sftp
                              (ffi-net-sftp-rename (sftp-session-handle session)
                                                   from-path
                                                   to-path))
              session)))

  #|proc:sftp-open-file
The `sftp-open-file` procedure opens a remote file handle using one or more access flags.
|#
  (define-who sftp-open-file
    (case-lambda
      [(session path flags)
       (sftp-open-file session path flags sftp-default-mode)]
      [(session path flags mode)
       (pcheck ([sftp-session? session] [string? path] [fixnum? mode])
               (ensure-session-open who session)
               (%make-sftp-file
                (ensure-success who 'sftp
                                (ffi-net-sftp-open-file (sftp-session-handle session)
                                                        path
                                                        (open-flags->int who flags)
                                                        mode))
                session
                #f))]))

  #|proc:sftp-close-file
The `sftp-close-file` procedure closes an SFTP file handle.
|#
  (define-who sftp-close-file
    (lambda (file)
      (pcheck ([sftp-file? file])
              (unless (sftp-file-closed? file)
                (ensure-success who 'sftp (ffi-net-sftp-close-file (sftp-file-handle file)))
                (sftp-file-handle-set! file 0)
                (sftp-file-closed?-set! file #t))
              file)))

  #|proc:sftp-read
The `sftp-read` procedure reads up to `size` bytes from an SFTP file handle.
|#
  (define-who sftp-read
    (case-lambda
      [(file size)
       (pcheck ([sftp-file? file] [fixnum? size])
               (ensure-file-open who file)
               (read-result who
                            (ffi-net-sftp-read (sftp-file-handle file) size 0 -1)))]
      [(file size timeout-ms)
       (pcheck ([sftp-file? file] [fixnum? size])
               (check-timeout-ms who timeout-ms)
               (ensure-file-open who file)
               (await-timeout-result
                who
                "sftp read timed out"
                timeout-ms
                (lambda (remaining-ms)
                  (read-result who
                               (ffi-net-sftp-read (sftp-file-handle file)
                                                  size
                                                  0
                                                  remaining-ms)))))]))

  #|proc:sftp-read/nonblocking
The `sftp-read/nonblocking` procedure attempts to read from an SFTP file handle without blocking.
|#
  (define-who sftp-read/nonblocking
    (lambda (file size)
      (pcheck ([sftp-file? file] [fixnum? size])
              (ensure-file-open who file)
              (read-result who
                           (ffi-net-sftp-read (sftp-file-handle file) size 1 -1)))))

  #|proc:sftp-read!
The `sftp-read!` procedure reads into a bytevector slice from an SFTP file handle.
|#
  (define-who sftp-read!
    (case-lambda
      [(file bv) (sftp-read! file bv 0 (bytevector-length bv))]
      [(file bv start) (sftp-read! file bv start (bytevector-length bv))]
      [(file bv start stop)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
               (read-into-result who
                                 (ffi-net-sftp-read-into (sftp-file-handle file)
                                                         bv
                                                         start
                                                         stop
                                                         0
                                                         -1)))]
      [(file bv start stop timeout-ms)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (check-timeout-ms who timeout-ms)
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
               (await-timeout-result
                who
                "sftp read timed out"
                timeout-ms
                (lambda (remaining-ms)
                  (read-into-result who
                                    (ffi-net-sftp-read-into (sftp-file-handle file)
                                                            bv
                                                            start
                                                            stop
                                                            0
                                                            remaining-ms)))))]))

  #|proc:sftp-read!/nonblocking
The `sftp-read!/nonblocking` procedure attempts to read into a bytevector slice without blocking.
|#
  (define-who sftp-read!/nonblocking
    (case-lambda
      [(file bv) (sftp-read!/nonblocking file bv 0 (bytevector-length bv))]
      [(file bv start) (sftp-read!/nonblocking file bv start (bytevector-length bv))]
      [(file bv start stop)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
               (let ([chunk (sftp-read/nonblocking file (fx- stop start))])
                 (cond
                  [(bytevector? chunk)
                   (let ([n (bytevector-length chunk)])
                     (bytevector-copy! chunk 0 bv start n)
                     n)]
                  [else chunk])))]))

  #|proc:sftp-write
The `sftp-write` procedure writes a bytevector slice to an SFTP file handle.
|#
  (define-who sftp-write
    (case-lambda
      [(file bv) (sftp-write file bv 0 (bytevector-length bv))]
      [(file bv start) (sftp-write file bv start (bytevector-length bv))]
      [(file bv start stop)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
               (write-result who
                             (ffi-net-sftp-write (sftp-file-handle file)
                                                 bv
                                                 start
                                                 stop
                                                 0
                                                 -1)))]
      [(file bv start stop timeout-ms)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (check-timeout-ms who timeout-ms)
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
               (await-timeout-result
                who
                "sftp write timed out"
                timeout-ms
                (lambda (remaining-ms)
                  (write-result who
                                (ffi-net-sftp-write (sftp-file-handle file)
                                                    bv
                                                    start
                                                    stop
                                                    0
                                                    remaining-ms)))))]))

  #|proc:sftp-write/nonblocking
The `sftp-write/nonblocking` procedure attempts to write a bytevector slice without blocking.
|#
  (define-who sftp-write/nonblocking
    (case-lambda
      [(file bv) (sftp-write/nonblocking file bv 0 (bytevector-length bv))]
      [(file bv start) (sftp-write/nonblocking file bv start (bytevector-length bv))]
      [(file bv start stop)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
                (write-result who
                             (ffi-net-sftp-write (sftp-file-handle file)
                                                 bv
                                                 start
                                                 stop
                                                 1
                                                 -1)))]))

  #|proc:sftp-write-all
The `sftp-write-all` procedure writes an entire bytevector slice to an SFTP file handle.
|#
  (define-who sftp-write-all
    (case-lambda
      [(file bv) (sftp-write-all file bv 0 (bytevector-length bv))]
      [(file bv start) (sftp-write-all file bv start (bytevector-length bv))]
      [(file bv start stop)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
               (let loop ([i start])
                 (if (fx= i stop)
                     (fx- stop start)
                     (loop (fx+ i (sftp-write file bv i stop))))))]
      [(file bv start stop timeout-ms)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (check-timeout-ms who timeout-ms)
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
               (let ([deadline-ms (timeout->deadline-ms timeout-ms)])
                 (let loop ([i start])
                   (if (fx= i stop)
                       (fx- stop start)
                       (let ([step-timeout (remaining-timeout-ms deadline-ms)])
                         (loop (fx+ i (sftp-write file bv i stop step-timeout))))))))]))

  #|proc:sftp-write-all/nonblocking
The `sftp-write-all/nonblocking` procedure writes as much of a bytevector slice as possible without blocking.
|#
  (define-who sftp-write-all/nonblocking
    (case-lambda
      [(file bv) (sftp-write-all/nonblocking file bv 0 (bytevector-length bv))]
      [(file bv start) (sftp-write-all/nonblocking file bv start (bytevector-length bv))]
      [(file bv start stop)
       (pcheck ([sftp-file? file] [bytevector? bv])
               (ensure-file-open who file)
               (check-slice who (bytevector-length bv) start stop)
               (let loop ([i start])
                 (if (fx= i stop)
                     (fx- stop start)
                     (let ([n (sftp-write/nonblocking file bv i stop)])
                       (cond
                        [(eq? n #f) (and (fx> i start) (fx- i start))]
                        [(fx= n 0) (fx- i start)]
                        [else (loop (fx+ i n))])))))]))

  #|proc:sftp-download
The `sftp-download` procedure downloads a remote file to a local pathname.
|#
  (define-who sftp-download
    (lambda (session remote-path local-path)
      (pcheck ([sftp-session? session] [string? remote-path local-path])
              (ensure-session-open who session)
              (let ([file (sftp-open-file session remote-path 'read)])
                (dynamic-wind
                  void
                  (lambda ()
                    (call-with-port
                     (open-file-output-port local-path
                                            (file-options no-fail replace)
                                            (buffer-mode block)
                                            #f)
                     (lambda (op)
                       (copy-port-chunks
                        (lambda () (sftp-read file 4096))
                        (lambda (chunk) (put-bytevector op chunk))))))
                  (lambda () (sftp-close-file file))))
              local-path)))

  #|proc:sftp-upload
The `sftp-upload` procedure uploads a local file to a remote pathname.
|#
  (define-who sftp-upload
    (lambda (session local-path remote-path)
      (pcheck ([sftp-session? session] [string? local-path remote-path])
              (ensure-session-open who session)
              (let ([file (sftp-open-file session remote-path '(write create truncate))])
                (dynamic-wind
                  void
                  (lambda ()
                    (call-with-port
                     (open-file-input-port local-path
                                           (file-options)
                                           (buffer-mode block)
                                           #f)
                     (lambda (ip)
                       (copy-port-chunks
                        (lambda () (get-bytevector-n ip 4096))
                        (lambda (chunk) (sftp-write-all file chunk))))))
                  (lambda () (sftp-close-file file))))
              remote-path)))

  #|proc:call-with-sftp-session
The `call-with-sftp-session` procedure opens an SFTP session, applies a procedure, and closes it afterwards.
|#
  (define-who call-with-sftp-session
    (lambda (ssh-session proc)
      (pcheck ([ssh-session? ssh-session] [procedure? proc])
              (let ([session (sftp-open ssh-session)])
                (dynamic-wind
                  void
                  (lambda () (proc session))
                  (lambda () (sftp-close session)))))))

  #|proc:open-sftp-input-port
The `open-sftp-input-port` procedure opens a binary input port over an SFTP file handle.
|#
  (define-who open-sftp-input-port
    (lambda (file)
      (pcheck ([sftp-file? file])
              (ensure-file-open who file)
              (make-binary-input-port file))))

  #|proc:open-sftp-output-port
The `open-sftp-output-port` procedure opens a binary output port over an SFTP file handle.
|#
  (define-who open-sftp-output-port
    (lambda (file)
      (pcheck ([sftp-file? file])
              (ensure-file-open who file)
              (make-binary-output-port file))))
  )
