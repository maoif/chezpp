(library (chezpp net tls)
  (export make-tls-context
          tls-context?
          close-tls-context
          tls-context-load-ca-file!
          tls-context-load-ca-path!
          tls-context-load-cert!
          tls-context-load-private-key!
          tls-context-set-verify!
          tls-context-set-alpn!
          tls-connect
          tls-accept
          tls-session?
          close-tls-session
          tls-read
          tls-read!
          tls-write
          tls-write-all
          tls-read/nonblocking
          tls-read!/nonblocking
          tls-write/nonblocking
          tls-write-all/nonblocking
          tls-flush
          tls-shutdown!
          tls-peer-certificate
          tls-peer-certificate-chain
          tls-protocol-version
          tls-cipher-name
          tls-verified?
          call-with-tls-client
          call-with-tls-server
          open-tls-port
          open-tls-input-port
          open-tls-output-port
          open-tls-text-input-port
          open-tls-text-output-port
          call-with-tls-ports)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp crypto cert)
          (chezpp crypto private)
          (chezpp net errors)
          (chezpp net ffi)
          (chezpp net private)
          (chezpp net socket))

  (define tls-formats '(pem der))

  (define-record-type (tls-context %make-tls-context tls-context?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle tls-context-handle tls-context-handle-set!)
            (immutable mode tls-context-mode)
            (mutable closed? tls-context-closed? tls-context-closed?-set!)))

  (define-record-type (tls-session %make-tls-session tls-session?)
    (sealed #t)
    (opaque #f)
    (fields (mutable handle tls-session-handle tls-session-handle-set!)
            (immutable context tls-session-context)
            (immutable socket tls-session-socket)
            (mutable closed? tls-session-closed? tls-session-closed?-set!)))

  (define check-format
    (lambda (who fmt)
      (unless (memq fmt tls-formats)
        (errorf who "TLS format must be one of ~s" tls-formats))))

  (define format->int
    (lambda (fmt)
      (case fmt
        [(pem) 0]
        [(der) 1]
        [else (unreachable!)])))

  (define mode->int
    (lambda (who mode)
      (case mode
        [(client) 0]
        [(server) 1]
        [else (errorf who "invalid TLS mode ~s" mode)])))

  (define ensure-context-open
    (lambda (who ctx)
      (when (tls-context-closed? ctx)
        (raise-net-error who 'tls "TLS context is closed" ctx))))

  (define ensure-session-open
    (lambda (who session)
      (when (tls-session-closed? session)
        (raise-net-error who 'tls "TLS session is closed" session))))

  (define ensure-success
    (lambda (who value)
      (when (ffi-error? value)
        (raise-net-error who 'tls (ffi-error-message value) value))
      value))

  (define encode-alpn
    (lambda (who proto*)
      (unless (list? proto*)
        (errorf who "ALPN protocols must be a list, given ~s" proto*))
      (let ([chunks
             (map (lambda (proto)
                    (unless (string? proto)
                      (errorf who "ALPN protocol must be a string, given ~s" proto))
                    (let ([bv (string->utf8 proto)])
                      (when (fx> (bytevector-length bv) 255)
                        (errorf who "ALPN protocol is too long: ~s" proto))
                      (let ([out (make-bytevector (fx1+ (bytevector-length bv)) 0)])
                        (bytevector-u8-set! out 0 (bytevector-length bv))
                        (bytevector-copy! bv 0 out 1 (bytevector-length bv))
                        out)))
                  proto*)])
        (apply bytevector-append chunks))))

  (define maybe-derive-peer-certificate
    (lambda (der)
      (and der (load-certificate der 'der))))

  (define maybe-derive-peer-certificate-chain
    (lambda (der*)
      (map (lambda (der) (load-certificate der 'der)) der*)))

  (define read-result
    (lambda (who x)
      (cond
       [(or (bytevector? x) (eof-object? x)) x]
       [(ffi-would-block? x) #f]
       [else (ensure-success who x)])))

  (define read-into-result
    (lambda (who x)
      (cond
       [(or (fixnum? x) (eof-object? x)) x]
       [(ffi-would-block? x) #f]
       [else (ensure-success who x)])))

  (define write-result
    (lambda (who x)
      (cond
       [(fixnum? x) x]
       [(ffi-would-block? x) #f]
       [else (ensure-success who x)])))

  (define make-binary-input-port
    (lambda (session)
      (make-custom-binary-input-port
       "chezpp-tls-input"
       (lambda (bv start count)
         (let ([n (tls-read! session bv start (fx+ start count))])
           (cond
            [(fixnum? n) n]
            [(eof-object? n) 0]
            [else (errorf 'call-with-tls-ports
                          "unexpected nonblocking result from blocking TLS port read")])))
       (lambda () #f)
       (lambda (x) #f)
       (lambda () #t))))

  (define make-binary-output-port
    (lambda (session)
      (make-custom-binary-output-port
       "chezpp-tls-output"
       (lambda (bv start count)
         (tls-write-all session bv start (fx+ start count)))
       (lambda () #f)
       (lambda (x) #f)
       (lambda () #t))))

  #|proc:make-tls-context
The `make-tls-context` procedure constructs a client or server TLS context.
|#
  (define-who make-tls-context
    (lambda (mode)
      (let ([handle (ffi-net-tls-context-create (mode->int who mode))])
        (when (= handle 0)
          (raise-net-error who 'tls "failed to create TLS context"))
        (%make-tls-context handle mode #f))))

  #|proc:close-tls-context
The `close-tls-context` procedure releases foreign resources owned by a TLS context.
|#
  (define-who close-tls-context
    (lambda (ctx)
      (pcheck ([tls-context? ctx])
              (unless (tls-context-closed? ctx)
                (ffi-net-tls-context-free (tls-context-handle ctx))
                (tls-context-handle-set! ctx 0)
                (tls-context-closed?-set! ctx #t))
              ctx)))

  #|proc:tls-context-load-ca-file!
The `tls-context-load-ca-file!` procedure loads trusted CA certificates from a PEM file.
|#
  (define-who tls-context-load-ca-file!
    (lambda (ctx path)
      (pcheck ([tls-context? ctx] [string? path])
              (ensure-context-open who ctx)
              (ensure-success who (ffi-net-tls-context-load-ca-file (tls-context-handle ctx) path)))))

  #|proc:tls-context-load-ca-path!
The `tls-context-load-ca-path!` procedure loads trusted CA certificates from a directory path.
|#
  (define-who tls-context-load-ca-path!
    (lambda (ctx path)
      (pcheck ([tls-context? ctx] [string? path])
              (ensure-context-open who ctx)
              (ensure-success who (ffi-net-tls-context-load-ca-path (tls-context-handle ctx) path)))))

  #|proc:tls-context-load-cert!
The `tls-context-load-cert!` procedure loads a TLS certificate from a pathname string or bytevector data.
|#
  (define-who tls-context-load-cert!
    (case-lambda
      [(ctx source) (tls-context-load-cert! ctx source 'pem)]
      [(ctx source fmt)
       (pcheck ([tls-context? ctx])
               (ensure-context-open who ctx)
               (check-format who fmt)
               (cond
                [(string? source)
                 (ensure-success
                  who
                  (ffi-net-tls-context-load-cert-file (tls-context-handle ctx)
                                                      source
                                                      (format->int fmt)))]
                [(bytevector? source)
                 (ensure-success
                  who
                  (ffi-net-tls-context-load-cert-bytes (tls-context-handle ctx)
                                                       source
                                                       0
                                                       (bytevector-length source)
                                                       (format->int fmt)))]
                [else
                 (errorf who "expected pathname string or bytevector certificate source, given ~s"
                         source)]))]))

  #|proc:tls-context-load-private-key!
The `tls-context-load-private-key!` procedure loads a TLS private key from a pathname string or bytevector data.
|#
  (define-who tls-context-load-private-key!
    (case-lambda
      [(ctx source) (tls-context-load-private-key! ctx source 'pem)]
      [(ctx source fmt)
       (pcheck ([tls-context? ctx])
               (ensure-context-open who ctx)
               (check-format who fmt)
               (cond
                [(string? source)
                 (ensure-success
                  who
                  (ffi-net-tls-context-load-key-file (tls-context-handle ctx)
                                                     source
                                                     (format->int fmt)))]
                [(bytevector? source)
                 (ensure-success
                  who
                  (ffi-net-tls-context-load-key-bytes (tls-context-handle ctx)
                                                      source
                                                      0
                                                      (bytevector-length source)
                                                      (format->int fmt)))]
                [else
                 (errorf who "expected pathname string or bytevector private-key source, given ~s"
                         source)])
               (ensure-success who (ffi-net-tls-context-check-key (tls-context-handle ctx))))]))

  #|proc:tls-context-set-verify!
The `tls-context-set-verify!` procedure enables or disables peer verification on a TLS context.
|#
  (define-who tls-context-set-verify!
    (lambda (ctx verify?)
      (pcheck ([tls-context? ctx] [boolean? verify?])
              (ensure-context-open who ctx)
              (ensure-success who (ffi-net-tls-context-set-verify (tls-context-handle ctx)
                                                                  (if verify? 1 0))))))

  #|proc:tls-context-set-alpn!
The `tls-context-set-alpn!` procedure configures ALPN protocol strings on a TLS context.
|#
  (define-who tls-context-set-alpn!
    (lambda (ctx proto*)
      (pcheck ([tls-context? ctx])
              (ensure-context-open who ctx)
              (let ([wire (encode-alpn who proto*)])
                (ensure-success
                 who
                 (ffi-net-tls-context-set-alpn (tls-context-handle ctx)
                                               wire
                                               0
                                               (bytevector-length wire)))))))

  #|proc:tls-connect
The `tls-connect` procedure performs a client-side TLS handshake over an existing socket.
|#
  (define-who tls-connect
    (case-lambda
      [(ctx sock) (tls-connect ctx sock #f)]
      [(ctx sock server-name)
       (pcheck ([tls-context? ctx] [socket? sock])
               (unless (or (not server-name) (string? server-name))
                 (errorf who "server name must be a string or #f, given ~s" server-name))
               (ensure-context-open who ctx)
               (let ([ans (ffi-net-tls-connect (tls-context-handle ctx)
                                               (socket-fd sock)
                                               (or server-name ""))])
                 (cond
                  [(ffi-error? ans)
                   (raise-net-error who 'tls (ffi-error-message ans) ans)]
                  [(ffi-would-block? ans) #f]
                  [else (%make-tls-session ans ctx sock #f)])))]))

  #|proc:tls-accept
The `tls-accept` procedure performs a server-side TLS handshake over an existing socket.
|#
  (define-who tls-accept
    (lambda (ctx sock)
      (pcheck ([tls-context? ctx] [socket? sock])
              (ensure-context-open who ctx)
              (let ([ans (ffi-net-tls-accept (tls-context-handle ctx) (socket-fd sock))])
                (cond
                 [(ffi-error? ans)
                  (raise-net-error who 'tls (ffi-error-message ans) ans)]
                 [(ffi-would-block? ans) #f]
                 [else (%make-tls-session ans ctx sock #f)])))))

  #|proc:close-tls-session
The `close-tls-session` procedure releases foreign resources owned by a TLS session.
|#
  (define-who close-tls-session
    (lambda (session)
      (pcheck ([tls-session? session])
              (unless (tls-session-closed? session)
                (ensure-success who (ffi-net-tls-close (tls-session-handle session)))
                (tls-session-handle-set! session 0)
                (tls-session-closed?-set! session #t))
              session)))

  #|proc:tls-read
The `tls-read` procedure reads up to `size` bytes from a TLS session.
|#
  (define-who tls-read
    (lambda (session size)
      (pcheck ([tls-session? session] [fixnum? size])
              (ensure-session-open who session)
              (read-result who (ffi-net-tls-read (tls-session-handle session) size 0)))))

  #|proc:tls-read/nonblocking
The `tls-read/nonblocking` procedure attempts a non-blocking TLS read and returns `#f` if progress would block.
|#
  (define-who tls-read/nonblocking
    (lambda (session size)
      (pcheck ([tls-session? session] [fixnum? size])
              (ensure-session-open who session)
              (read-result who (ffi-net-tls-read (tls-session-handle session) size 1)))))

  #|proc:tls-read!
The `tls-read!` procedure reads into a bytevector slice and returns a byte count or EOF object.
|#
  (define-who tls-read!
    (case-lambda
      [(session bv) (tls-read! session bv 0 (bytevector-length bv))]
      [(session bv start) (tls-read! session bv start (bytevector-length bv))]
      [(session bv start stop)
       (pcheck ([tls-session? session] [bytevector? bv])
               (ensure-session-open who session)
               (check-slice who (bytevector-length bv) start stop)
               (read-into-result
                who
                (ffi-net-tls-read-into (tls-session-handle session) bv start stop 0)))]))

  #|proc:tls-read!/nonblocking
The `tls-read!/nonblocking` procedure attempts a non-blocking TLS read into a bytevector slice and returns `#f` if progress would block.
|#
  (define-who tls-read!/nonblocking
    (case-lambda
      [(session bv) (tls-read!/nonblocking session bv 0 (bytevector-length bv))]
      [(session bv start) (tls-read!/nonblocking session bv start (bytevector-length bv))]
      [(session bv start stop)
       (pcheck ([tls-session? session] [bytevector? bv])
               (ensure-session-open who session)
               (check-slice who (bytevector-length bv) start stop)
               (read-into-result
                who
                (ffi-net-tls-read-into (tls-session-handle session) bv start stop 1)))]))

  #|proc:tls-write
The `tls-write` procedure writes a bytevector slice to a TLS session and returns the number of bytes written.
|#
  (define-who tls-write
    (case-lambda
      [(session bv) (tls-write session bv 0 (bytevector-length bv))]
      [(session bv start) (tls-write session bv start (bytevector-length bv))]
      [(session bv start stop)
       (pcheck ([tls-session? session] [bytevector? bv])
               (ensure-session-open who session)
               (check-slice who (bytevector-length bv) start stop)
               (write-result who (ffi-net-tls-write (tls-session-handle session) bv start stop 0)))]))

  #|proc:tls-write/nonblocking
The `tls-write/nonblocking` procedure attempts a non-blocking TLS write and returns `#f` if progress would block.
|#
  (define-who tls-write/nonblocking
    (case-lambda
      [(session bv) (tls-write/nonblocking session bv 0 (bytevector-length bv))]
      [(session bv start) (tls-write/nonblocking session bv start (bytevector-length bv))]
      [(session bv start stop)
       (pcheck ([tls-session? session] [bytevector? bv])
               (ensure-session-open who session)
               (check-slice who (bytevector-length bv) start stop)
               (write-result who (ffi-net-tls-write (tls-session-handle session) bv start stop 1)))]))

  #|proc:tls-write-all
The `tls-write-all` procedure writes an entire bytevector slice to a TLS session before returning.
|#
  (define-who tls-write-all
    (case-lambda
      [(session bv) (tls-write-all session bv 0 (bytevector-length bv))]
      [(session bv start) (tls-write-all session bv start (bytevector-length bv))]
      [(session bv start stop)
       (pcheck ([tls-session? session] [bytevector? bv])
               (ensure-session-open who session)
               (check-slice who (bytevector-length bv) start stop)
               (let loop ([i start])
                 (if (fx= i stop)
                     (fx- stop start)
                     (loop (fx+ i (tls-write session bv i stop))))))]))

  #|proc:tls-write-all/nonblocking
The `tls-write-all/nonblocking` procedure writes as much of a bytevector slice as possible without blocking.
|#
  (define-who tls-write-all/nonblocking
    (case-lambda
      [(session bv) (tls-write-all/nonblocking session bv 0 (bytevector-length bv))]
      [(session bv start) (tls-write-all/nonblocking session bv start (bytevector-length bv))]
      [(session bv start stop)
       (pcheck ([tls-session? session] [bytevector? bv])
               (ensure-session-open who session)
               (check-slice who (bytevector-length bv) start stop)
               (let loop ([i start])
                 (if (fx= i stop)
                     (fx- stop start)
                     (let ([n (tls-write/nonblocking session bv i stop)])
                       (cond
                        [(eq? n #f) (and (fx> i start) (fx- i start))]
                        [(fx= n 0) (fx- i start)]
                        [else (loop (fx+ i n))])))))]))

  #|proc:tls-flush
The `tls-flush` procedure flushes buffered TLS writes and currently acts as a no-op marker.
|#
  (define-who tls-flush
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              #t)))

  #|proc:tls-shutdown!
The `tls-shutdown!` procedure performs an orderly TLS shutdown.
|#
  (define-who tls-shutdown!
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (ensure-success who (ffi-net-tls-shutdown (tls-session-handle session))))))

  #|proc:tls-peer-certificate
The `tls-peer-certificate` procedure returns the peer certificate as a `(chezpp crypto cert)` certificate object, or `#f`.
|#
  (define-who tls-peer-certificate
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (let ([ans (ensure-success who (ffi-net-tls-peer-certificate-der (tls-session-handle session)))])
                (maybe-derive-peer-certificate ans)))))

  #|proc:tls-peer-certificate-chain
The `tls-peer-certificate-chain` procedure returns the presented certificate chain as a list of certificate objects.
|#
  (define-who tls-peer-certificate-chain
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (let ([ans (ensure-success who (ffi-net-tls-peer-certificate-chain-der (tls-session-handle session)))])
                (maybe-derive-peer-certificate-chain ans)))))

  #|proc:tls-protocol-version
The `tls-protocol-version` procedure returns the negotiated TLS protocol version string.
|#
  (define-who tls-protocol-version
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (ensure-success who (ffi-net-tls-protocol-version (tls-session-handle session))))))

  #|proc:tls-cipher-name
The `tls-cipher-name` procedure returns the negotiated cipher-suite name.
|#
  (define-who tls-cipher-name
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (ensure-success who (ffi-net-tls-cipher-name (tls-session-handle session))))))

  #|proc:tls-verified?
The `tls-verified?` procedure returns `#t` when peer verification succeeded.
|#
  (define-who tls-verified?
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (ensure-success who (ffi-net-tls-verified (tls-session-handle session))))))

  #|proc:call-with-tls-client
The `call-with-tls-client` procedure performs a client TLS handshake, passes the session to a procedure, and closes the session afterwards.
|#
  (define-who call-with-tls-client
    (case-lambda
      [(ctx sock proc) (call-with-tls-client ctx sock #f proc)]
      [(ctx sock server-name proc)
       (pcheck ([tls-context? ctx] [socket? sock] [procedure? proc])
               (let ([session (tls-connect ctx sock server-name)])
                 (dynamic-wind
                   void
                   (lambda () (proc session))
                   (lambda () (when session (close-tls-session session))))))]))

  #|proc:call-with-tls-server
The `call-with-tls-server` procedure performs a server TLS handshake, passes the session to a procedure, and closes the session afterwards.
|#
  (define-who call-with-tls-server
    (lambda (ctx sock proc)
      (pcheck ([tls-context? ctx] [socket? sock] [procedure? proc])
              (let ([session (tls-accept ctx sock)])
                (dynamic-wind
                  void
                  (lambda () (proc session))
                  (lambda () (when session (close-tls-session session))))))))

  #|proc:open-tls-port
The `open-tls-port` procedure opens a bidirectional binary port layered over a TLS session.
|#
  (define-who open-tls-port
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (make-custom-binary-input/output-port
               "chezpp-tls-port"
               (lambda (bv start count)
                 (tls-read! session bv start (fx+ start count)))
               (lambda (bv start count)
                 (tls-write-all session bv start (fx+ start count)))
               (lambda () #f)
               (lambda (x) #f)
               (lambda () #t)))))

  #|proc:open-tls-input-port
The `open-tls-input-port` procedure opens a binary input port layered over a TLS session.
|#
  (define-who open-tls-input-port
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (make-binary-input-port session))))

  #|proc:open-tls-output-port
The `open-tls-output-port` procedure opens a binary output port layered over a TLS session.
|#
  (define-who open-tls-output-port
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (make-binary-output-port session))))

  #|proc:open-tls-text-input-port
The `open-tls-text-input-port` procedure opens a text input port layered over a TLS session.
|#
  (define-who open-tls-text-input-port
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (transcoded-port (open-tls-input-port session) (native-transcoder)))))

  #|proc:open-tls-text-output-port
The `open-tls-text-output-port` procedure opens a text output port layered over a TLS session.
|#
  (define-who open-tls-text-output-port
    (lambda (session)
      (pcheck ([tls-session? session])
              (ensure-session-open who session)
              (transcoded-port (open-tls-output-port session) (native-transcoder)))))

  #|proc:call-with-tls-ports
The `call-with-tls-ports` procedure opens binary TLS ports, passes them to a procedure, and closes the wrapper ports afterwards.
|#
  (define-who call-with-tls-ports
    (lambda (session proc)
      (pcheck ([tls-session? session] [procedure? proc])
              (ensure-session-open who session)
              (let ([ip (open-tls-input-port session)]
                    [op (open-tls-output-port session)])
                (dynamic-wind
                  void
                  (lambda () (proc ip op))
                  (lambda ()
                    (close-port ip)
                    (close-port op)))))))
  )
