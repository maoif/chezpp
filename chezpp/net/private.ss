(library (chezpp net private)
  (export socket-address?
          socket?
          socket-fd
          socket-fd-set!
          socket-family
          socket-type
          socket-proto
          socket-blocking?
          socket-blocking-set!
          socket-closed?
          socket-closed-set!
          %make-socket
          socket-address-family
          socket-address-host
          socket-address-port
          socket-address-path
          dns-result?
          dns-result-addresses
          dns-result-canonname
          %make-socket-address
          %make-dns-result
          %socket-address-from-ffi
          %dns-result-from-ffi
          family-symbol->int
          type-symbol->int
          shutdown-symbol->int
          check-port
          ffi-error?
          ffi-would-block?
          ffi-would-block-read?
          ffi-would-block-write?
          ffi-would-block-event
          ffi-would-block-events
          ffi-error-message)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp net ffi))

  (define-record-type (socket %make-socket socket?)
    (sealed #t)
    (opaque #f)
    (fields (mutable fd socket-fd socket-fd-set!)
            (immutable family socket-family)
            (immutable type socket-type)
            (immutable proto socket-proto)
            (mutable blocking socket-blocking? socket-blocking-set!)
            (mutable closed socket-closed? socket-closed-set!)))

  (define-record-type (socket-address %make-socket-address socket-address?)
    (sealed #t)
    (opaque #f)
    (fields (immutable family socket-address-family)
            (immutable host socket-address-host)
            (immutable port socket-address-port)
            (immutable path socket-address-path)))

  (define-record-type (dns-result %make-dns-result dns-result?)
    (sealed #t)
    (opaque #f)
    (fields (immutable addresses dns-result-addresses)
            (immutable canonname dns-result-canonname)))

  (define ffi-error?
    (lambda (x)
      (and (vector? x)
           (= (vector-length x) 2)
           (eq? (vector-ref x 0) 'error))))

  (define ffi-would-block?
    (lambda (x)
      (and (vector? x)
           (= (vector-length x) 2)
           (memq (vector-ref x 0) '(would-block would-block-read would-block-write)))))

  (define ffi-would-block-read?
    (lambda (x)
      (and (vector? x)
           (= (vector-length x) 2)
           (eq? (vector-ref x 0) 'would-block-read))))

  (define ffi-would-block-write?
    (lambda (x)
      (and (vector? x)
           (= (vector-length x) 2)
           (eq? (vector-ref x 0) 'would-block-write))))

  (define ffi-would-block-event
    (lambda (answer)
      (car (ffi-would-block-events answer))))

  (define ffi-would-block-events
    (lambda (answer)
      (case (vector-ref answer 0)
        [(would-block-read) '(read)]
        [(would-block-write) '(write)]
        [(would-block)
         (let ([event* (vector-ref answer 1)])
           (if (pair? event*) event* (list event*)))]
        [else (assert-unreachable)])))

  (define ffi-error-message
    (lambda (x)
      (and (ffi-error? x)
           (vector-ref x 1))))

  (define %socket-address-from-ffi
    (lambda (v)
      (%make-socket-address (vector-ref v 0)
                            (vector-ref v 1)
                            (vector-ref v 2)
                            (vector-ref v 3))))

  (define %dns-result-from-ffi
    (lambda (v)
      (%make-dns-result (map %socket-address-from-ffi (vector-ref v 1))
                        (vector-ref v 0))))

  (define family-symbol->int
    (lambda (who family)
      (case family
        [(#f) 0]
        [(inet) (net-af-inet)]
        [(inet6) (net-af-inet6)]
        [(unix) (net-af-unix)]
        [else (errorf who "invalid socket family ~s" family)])))

  (define type-symbol->int
    (lambda (who type)
      (case type
        [(#f) 0]
        [(stream) (net-sock-stream)]
        [(datagram) (net-sock-datagram)]
        [(seqpacket) (net-sock-seqpacket)]
        [else (errorf who "invalid socket type ~s" type)])))

  (define shutdown-symbol->int
    (lambda (who how)
      (case how
        [(read) (net-shut-read)]
        [(write) (net-shut-write)]
        [(read/write) (net-shut-read/write)]
        [else (errorf who "invalid shutdown mode ~s" how)])))

  (define check-port
    (lambda (who port)
      (when (or (fx< port 0) (fx> port 65535))
        (errorf who "port must be between 0 and 65535, given ~s" port))
      port))
  )
