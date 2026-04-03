(library (chezpp net socket)
  (export open-socket
          socket?
          close-socket
          socket-closed?
          socket-bind!
          socket-listen!
          socket-accept
          socket-connect!
          socket-shutdown!
          socket-send
          socket-send-all
          socket-recv
          socket-recv!
          socket-send/nonblocking
          socket-send-all/nonblocking
          socket-recv/nonblocking
          socket-recv!/nonblocking
          socket-set-option!
          socket-get-option
          socket-local-address
          socket-peer-address
          socket-fd
          socket-blocking?
          socket-set-blocking!
          call-with-socket
          call-with-connected-socket
          open-socket-port
          open-socket-input-port
          open-socket-output-port
          open-socket-binary-input-port
          open-socket-binary-output-port
          open-socket-text-input-port
          open-socket-text-output-port
          call-with-socket-ports
          socket-accept/nonblocking)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp net errors)
          (chezpp net ffi)
          (chezpp net private)
          (chezpp net address))

  (define-record-type (socket %make-socket socket?)
    (sealed #t)
    (opaque #f)
    (fields (mutable fd socket-fd socket-fd-set!)
            (immutable family socket-family)
            (immutable type socket-type)
            (immutable proto socket-proto)
            (mutable blocking socket-blocking? socket-blocking-set!)
            (mutable closed socket-closed? socket-closed-set!)))

  (define ensure-open
    (lambda (who sock)
      (when (socket-closed? sock)
        (raise-net-error who 'socket "socket is closed" sock))))

  (define check-slice
    (lambda (who len start stop)
      (unless (and (fixnum? start) (fixnum? stop) (fx<= 0 start stop len))
        (errorf who "invalid slice [~a, ~a) for length ~a" start stop len))))

  (define check-size
    (lambda (who size)
      (when (fx< size 0)
        (errorf who "size must be non-negative, given ~s" size))
      size))

  (define ensure-ffi-success
    (lambda (who x kind)
      (when (ffi-error? x)
        (raise-net-error who kind (ffi-error-message x) x))
      x))

  (define maybe-would-block
    (lambda (x)
      (and (ffi-would-block? x) #f)))

  (define dup-socket-fd
    (lambda (who sock)
      (let ([ans (ensure-ffi-success who (ffi-net-socket-dup (socket-fd sock)) 'socket)])
        ans)))

  (define address->ffi-host
    (lambda (address)
      (or (socket-address-host address) "")))

  (define address->ffi-path
    (lambda (address)
      (or (socket-address-path address) "")))

  (define send-result
    (lambda (who x nonblocking?)
      (cond
       [(fixnum? x) x]
       [(ffi-would-block? x) #f]
       [else (ensure-ffi-success who x 'socket)])))

  (define recv-result
    (lambda (who x)
      (cond
       [(or (bytevector? x) (eof-object? x)) x]
       [(ffi-would-block? x) #f]
       [else (ensure-ffi-success who x 'socket)])))

  (define recv-into-result
    (lambda (who x)
      (cond
       [(or (fixnum? x) (eof-object? x)) x]
       [(ffi-would-block? x) #f]
       [else (ensure-ffi-success who x 'socket)])))

  #|proc:open-socket
The `open-socket` procedure opens a new socket and returns a socket object.
|#
  (define-who open-socket
    (case-lambda
      [(family type) (open-socket family type 0)]
      [(family type proto)
       (pcheck ([fixnum? proto])
               (let ([ans (ffi-net-socket-open (family-symbol->int who family)
                                               (type-symbol->int who type)
                                               proto)])
                 (if (fixnum? ans)
                     (%make-socket ans family type proto #t #f)
                     (ensure-ffi-success who ans 'socket))))]))

  #|proc:close-socket
The `close-socket` procedure closes a socket object.
|#
  (define-who close-socket
    (lambda (sock)
      (pcheck ([socket? sock])
              (unless (socket-closed? sock)
                (ensure-ffi-success who (ffi-net-socket-close (socket-fd sock)) 'socket)
                (socket-closed-set! sock #t)
                (socket-fd-set! sock -1)))))

  #|proc:socket-bind!
The `socket-bind!` procedure binds a socket to a socket address.
|#
  (define-who socket-bind!
    (lambda (sock address)
      (pcheck ([socket? sock] [socket-address? address])
              (ensure-open who sock)
              (ensure-ffi-success
               who
               (ffi-net-socket-bind (socket-fd sock)
                                    (family-symbol->int who (socket-address-family address))
                                    (address->ffi-host address)
                                    (or (socket-address-port address) -1)
                                    (address->ffi-path address))
               'socket))))

  #|proc:socket-listen!
The `socket-listen!` procedure marks a bound stream socket as listening.
|#
  (define-who socket-listen!
    (case-lambda
      [(sock) (socket-listen! sock 128)]
      [(sock backlog)
       (pcheck ([socket? sock] [fixnum? backlog])
               (ensure-open who sock)
               (ensure-ffi-success who (ffi-net-socket-listen (socket-fd sock) backlog) 'socket))]))

  (define make-accepted-socket
    (lambda (parent fd address)
      (%make-socket fd
                    (socket-address-family address)
                    (socket-type parent)
                    (socket-proto parent)
                    (socket-blocking? parent)
                    #f)))

  #|proc:socket-accept
The `socket-accept` procedure accepts an incoming connection and returns the child socket and peer address.
|#
  (define-who socket-accept
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (let ([ans (ffi-net-socket-accept (socket-fd sock) 0)])
                (cond
                 [(vector? ans)
                  (let ([address (%socket-address-from-ffi (vector-ref ans 1))])
                    (values (make-accepted-socket sock (vector-ref ans 0) address)
                            address))]
                 [else (ensure-ffi-success who ans 'socket)])))))

  #|proc:socket-accept/nonblocking
The `socket-accept/nonblocking` procedure accepts an incoming connection if one is ready, and returns `#f` otherwise.
|#
  (define-who socket-accept/nonblocking
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (let ([ans (ffi-net-socket-accept (socket-fd sock) 1)])
                (cond
                 [(ffi-would-block? ans) #f]
                 [(vector? ans)
                  (let ([address (%socket-address-from-ffi (vector-ref ans 1))])
                    (values (make-accepted-socket sock (vector-ref ans 0) address)
                            address))]
                 [else (ensure-ffi-success who ans 'socket)])))))

  #|proc:socket-connect!
The `socket-connect!` procedure connects a socket to a remote socket address.
|#
  (define-who socket-connect!
    (lambda (sock address)
      (pcheck ([socket? sock] [socket-address? address])
              (ensure-open who sock)
              (let ([ans (ffi-net-socket-connect (socket-fd sock)
                                                 (family-symbol->int who (socket-address-family address))
                                                 (address->ffi-host address)
                                                 (or (socket-address-port address) -1)
                                                 (address->ffi-path address))])
                (if (ffi-would-block? ans)
                    #f
                    (ensure-ffi-success who ans 'socket))))))

  #|proc:socket-shutdown!
The `socket-shutdown!` procedure shuts down reading, writing, or both directions of a socket.
|#
  (define-who socket-shutdown!
    (lambda (sock how)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (ensure-ffi-success
               who
               (ffi-net-socket-shutdown (socket-fd sock) (shutdown-symbol->int who how))
               'socket))))

  #|proc:socket-send
The `socket-send` procedure writes a bytevector slice to a socket and returns the number of bytes written.
|#
  (define-who socket-send
    (case-lambda
      [(sock bv) (socket-send sock bv 0 (bytevector-length bv))]
      [(sock bv start) (socket-send sock bv start (bytevector-length bv))]
      [(sock bv start stop)
       (pcheck ([socket? sock] [bytevector? bv])
               (ensure-open who sock)
               (check-slice who (bytevector-length bv) start stop)
               (send-result who (ffi-net-socket-send (socket-fd sock) bv start stop 0) #f))]))

  #|proc:socket-send/nonblocking
The `socket-send/nonblocking` procedure attempts to write a bytevector slice without blocking and returns `#f` if the socket would block.
|#
  (define-who socket-send/nonblocking
    (case-lambda
      [(sock bv) (socket-send/nonblocking sock bv 0 (bytevector-length bv))]
      [(sock bv start) (socket-send/nonblocking sock bv start (bytevector-length bv))]
      [(sock bv start stop)
       (pcheck ([socket? sock] [bytevector? bv])
               (ensure-open who sock)
               (check-slice who (bytevector-length bv) start stop)
               (send-result who (ffi-net-socket-send (socket-fd sock) bv start stop 1) #t))]))

  #|proc:socket-send-all
The `socket-send-all` procedure writes an entire bytevector slice to a socket before returning.
|#
  (define-who socket-send-all
    (case-lambda
      [(sock bv) (socket-send-all sock bv 0 (bytevector-length bv))]
      [(sock bv start) (socket-send-all sock bv start (bytevector-length bv))]
      [(sock bv start stop)
       (pcheck ([socket? sock] [bytevector? bv])
               (ensure-open who sock)
               (check-slice who (bytevector-length bv) start stop)
               (let loop ([i start])
                 (if (fx= i stop)
                     (fx- stop start)
                     (loop (fx+ i (socket-send sock bv i stop))))))]))

  #|proc:socket-send-all/nonblocking
The `socket-send-all/nonblocking` procedure writes as much of a bytevector slice as possible without blocking.
|#
  (define-who socket-send-all/nonblocking
    (case-lambda
      [(sock bv) (socket-send-all/nonblocking sock bv 0 (bytevector-length bv))]
      [(sock bv start) (socket-send-all/nonblocking sock bv start (bytevector-length bv))]
      [(sock bv start stop)
       (pcheck ([socket? sock] [bytevector? bv])
               (ensure-open who sock)
               (check-slice who (bytevector-length bv) start stop)
               (let loop ([i start])
                 (if (fx= i stop)
                     (fx- stop start)
                     (let ([n (socket-send/nonblocking sock bv i stop)])
                       (cond
                        [(eq? n #f) (and (fx> i start) (fx- i start))]
                        [(fx= n 0) (fx- i start)]
                        [else (loop (fx+ i n))])))))]))

  #|proc:socket-recv
The `socket-recv` procedure reads up to `size` bytes from a socket and returns a bytevector, an EOF object, or `#f` if a nonblocking socket would block.
|#
  (define-who socket-recv
    (lambda (sock size)
      (pcheck ([socket? sock] [fixnum? size])
              (check-size who size)
              (ensure-open who sock)
              (recv-result who (ffi-net-socket-recv (socket-fd sock) size 0)))))

  #|proc:socket-recv/nonblocking
The `socket-recv/nonblocking` procedure attempts to read up to `size` bytes without blocking and returns `#f` if the socket would block.
|#
  (define-who socket-recv/nonblocking
    (lambda (sock size)
      (pcheck ([socket? sock] [fixnum? size])
              (check-size who size)
              (ensure-open who sock)
              (recv-result who (ffi-net-socket-recv (socket-fd sock) size 1)))))

  #|proc:socket-recv!
The `socket-recv!` procedure reads into a bytevector slice and returns the number of bytes read or an EOF object.
|#
  (define-who socket-recv!
    (case-lambda
      [(sock bv) (socket-recv! sock bv 0 (bytevector-length bv))]
      [(sock bv start) (socket-recv! sock bv start (bytevector-length bv))]
      [(sock bv start stop)
       (pcheck ([socket? sock] [bytevector? bv])
               (ensure-open who sock)
               (check-slice who (bytevector-length bv) start stop)
               (recv-into-result who (ffi-net-socket-recv-into (socket-fd sock) bv start stop 0)))]))

  #|proc:socket-recv!/nonblocking
The `socket-recv!/nonblocking` procedure attempts to read into a bytevector slice without blocking and returns `#f` if the socket would block.
|#
  (define-who socket-recv!/nonblocking
    (case-lambda
      [(sock bv) (socket-recv!/nonblocking sock bv 0 (bytevector-length bv))]
      [(sock bv start) (socket-recv!/nonblocking sock bv start (bytevector-length bv))]
      [(sock bv start stop)
       (pcheck ([socket? sock] [bytevector? bv])
               (ensure-open who sock)
               (check-slice who (bytevector-length bv) start stop)
               (recv-into-result who (ffi-net-socket-recv-into (socket-fd sock) bv start stop 1)))]))

  #|proc:socket-set-option!
The `socket-set-option!` procedure updates a supported socket option.
|#
  (define-who socket-set-option!
    (lambda (sock option value)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (unless (or (boolean? value) (fixnum? value))
                (errorf who "expected boolean or fixnum socket option value, given ~s" value))
              (ensure-ffi-success
               who
               (ffi-net-socket-set-option (socket-fd sock) (symbol->string option) value)
               'socket))))

  #|proc:socket-get-option
The `socket-get-option` procedure returns a supported socket option value.
|#
  (define-who socket-get-option
    (lambda (sock option)
      (pcheck ([socket? sock] [symbol? option])
              (ensure-open who sock)
              (ensure-ffi-success
               who
               (ffi-net-socket-get-option (socket-fd sock) (symbol->string option))
               'socket))))

  #|proc:socket-local-address
The `socket-local-address` procedure returns the current local socket address.
|#
  (define-who socket-local-address
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (%socket-address-from-ffi
               (ensure-ffi-success who (ffi-net-socket-local-address (socket-fd sock)) 'socket)))))

  #|proc:socket-peer-address
The `socket-peer-address` procedure returns the current peer socket address.
|#
  (define-who socket-peer-address
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (%socket-address-from-ffi
               (ensure-ffi-success who (ffi-net-socket-peer-address (socket-fd sock)) 'socket)))))

  #|proc:socket-set-blocking!
The `socket-set-blocking!` procedure toggles blocking mode on a socket.
|#
  (define-who socket-set-blocking!
    (lambda (sock blocking?)
      (pcheck ([socket? sock] [boolean? blocking?])
              (ensure-open who sock)
              (ensure-ffi-success who (ffi-net-socket-set-blocking (socket-fd sock)
                                                                   (if blocking? 1 0))
                                  'socket)
              (socket-blocking-set! sock blocking?)
              blocking?)))

  #|proc:call-with-socket
The `call-with-socket` procedure opens a socket, passes it to a thunk, and always closes it afterwards.
|#
  (define-who call-with-socket
    (lambda (family type proto proc)
      (pcheck ([procedure? proc])
              (let ([sock (open-socket family type proto)])
                (dynamic-wind
                  void
                  (lambda () (proc sock))
                  (lambda () (close-socket sock)))))))

  #|proc:call-with-connected-socket
The `call-with-connected-socket` procedure opens, connects, passes, and closes a socket around a thunk.
|#
  (define-who call-with-connected-socket
    (lambda (family type proto address proc)
      (pcheck ([socket-address? address] [procedure? proc])
              (call-with-socket family type proto
                (lambda (sock)
                  (socket-connect! sock address)
                  (proc sock))))))

  (define open-dup-input-port
    (lambda (who sock transcoder)
      (open-fd-input-port (dup-socket-fd who sock) 'block transcoder)))

  (define open-dup-output-port
    (lambda (who sock transcoder)
      (open-fd-output-port (dup-socket-fd who sock) 'block transcoder)))

  #|proc:open-socket-port
The `open-socket-port` procedure opens a bidirectional binary port for a socket using a duplicated file descriptor.
|#
  (define-who open-socket-port
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (open-fd-input/output-port (dup-socket-fd who sock) 'block #f))))

  #|proc:open-socket-input-port
The `open-socket-input-port` procedure opens a binary input port for a socket using a duplicated file descriptor.
|#
  (define-who open-socket-input-port
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (open-dup-input-port who sock #f))))

  #|proc:open-socket-output-port
The `open-socket-output-port` procedure opens a binary output port for a socket using a duplicated file descriptor.
|#
  (define-who open-socket-output-port
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (open-dup-output-port who sock #f))))

  #|proc:open-socket-binary-input-port
The `open-socket-binary-input-port` procedure is an alias for `open-socket-input-port`.
|#
  (define-who open-socket-binary-input-port
    (lambda (sock)
      (open-socket-input-port sock)))

  #|proc:open-socket-binary-output-port
The `open-socket-binary-output-port` procedure is an alias for `open-socket-output-port`.
|#
  (define-who open-socket-binary-output-port
    (lambda (sock)
      (open-socket-output-port sock)))

  #|proc:open-socket-text-input-port
The `open-socket-text-input-port` procedure opens a text input port for a socket using the native transcoder.
|#
  (define-who open-socket-text-input-port
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (open-dup-input-port who sock (native-transcoder)))))

  #|proc:open-socket-text-output-port
The `open-socket-text-output-port` procedure opens a text output port for a socket using the native transcoder.
|#
  (define-who open-socket-text-output-port
    (lambda (sock)
      (pcheck ([socket? sock])
              (ensure-open who sock)
              (open-dup-output-port who sock (native-transcoder)))))

  #|proc:call-with-socket-ports
The `call-with-socket-ports` procedure opens binary input and output ports for a socket, passes them to a thunk, and closes them afterwards.
|#
  (define-who call-with-socket-ports
    (lambda (sock proc)
      (pcheck ([socket? sock] [procedure? proc])
              (ensure-open who sock)
              (let ([ip (open-socket-input-port sock)]
                    [op (open-socket-output-port sock)])
                (dynamic-wind
                  void
                  (lambda () (proc ip op))
                  (lambda ()
                    (close-port ip)
                    (close-port op)))))))
  )
