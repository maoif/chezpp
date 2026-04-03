(library (chezpp net address)
  (export make-socket-address
          socket-address?
          socket-address-family
          socket-address-host
          socket-address-port
          socket-address-path
          resolve-address
          resolve-addresses
          name->address
          address->name)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp net errors)
          (chezpp net ffi)
          (chezpp net private))

  (define ensure-success
    (lambda (who x)
      (when (ffi-error? x)
        (raise-net-error who 'address (ffi-error-message x) x))
      x))

  (define check-port
    (lambda (who port)
      (when (or (fx< port 0) (fx> port 65535))
        (errorf who "port must be between 0 and 65535, given ~s" port))
      port))

  #|proc:make-socket-address
The `make-socket-address` procedure constructs a socket address record for internet or Unix-domain sockets.
|#
  (define-who make-socket-address
    (case-lambda
      [(family path)
       (pcheck ([string? path])
               (unless (eq? family 'unix)
                 (errorf who "two-argument socket addresses require family 'unix"))
               (%make-socket-address family #f #f path))]
      [(family host port)
       (pcheck ([string? host] [fixnum? port])
               (unless (memq family '(inet inet6))
                 (errorf who "three-argument socket addresses require family 'inet or 'inet6"))
               (check-port who port)
               (%make-socket-address family host port #f))]))

  #|proc:resolve-addresses
The `resolve-addresses` procedure resolves a host and port into a list of socket addresses.
|#
  (define-who resolve-addresses
    (case-lambda
      [(host port) (resolve-addresses host port #f #f)]
      [(host port family) (resolve-addresses host port family #f)]
      [(host port family type)
       (pcheck ([string? host] [fixnum? port])
               (check-port who port)
               (let ([ans (ensure-success
                           who
                           (ffi-net-resolve-addresses host port
                                                      (family-symbol->int who family)
                                                      (type-symbol->int who type)))])
                 (map %socket-address-from-ffi (vector-ref ans 1))))]))

  #|proc:resolve-address
The `resolve-address` procedure resolves a host and port and returns the first matching socket address, or `#f`.
|#
  (define-who resolve-address
    (case-lambda
      [(host port) (resolve-address host port #f #f)]
      [(host port family) (resolve-address host port family #f)]
      [(host port family type)
       (let ([addresses (resolve-addresses host port family type)])
         (and (pair? addresses) (car addresses)))]))

  #|proc:name->address
The `name->address` procedure is an alias for `resolve-address`.
|#
  (define-who name->address
    (case-lambda
      [(host port) (resolve-address host port)]
      [(host port family) (resolve-address host port family)]
      [(host port family type) (resolve-address host port family type)]))

  #|proc:address->name
The `address->name` procedure performs a reverse lookup for a socket address and returns a hostname string.
|#
  (define-who address->name
    (lambda (address)
      (pcheck ([socket-address? address])
              (ensure-success
               who
               (ffi-net-address->name
                (family-symbol->int who (socket-address-family address))
                (or (socket-address-host address) "")
                (or (socket-address-port address) -1)
                (or (socket-address-path address) ""))))))
  )
