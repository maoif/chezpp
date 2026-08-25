(library (chezpp net lws ffi)
  (export lws-cap-http1
          lws-cap-http2
          lws-cap-tls
          lws-cap-socks5
          lws-cap-external-poll
          lws-status
          lws-capability?
          lws-require-capability!)
  (import (chezpp chez)
          (chezpp net errors)
          (chezpp utils))

  (define lws-cap-http1 (fxsll 1 0))
  (define lws-cap-http2 (fxsll 1 1))
  (define lws-cap-tls (fxsll 1 2))
  (define lws-cap-socks5 (fxsll 1 3))
  (define lws-cap-external-poll (fxsll 1 4))

  (define ffi-lws-status
    (foreign-procedure "chezpp_lws_status" () scheme-object))

  (define valid-lws-status?
    (lambda (status)
      (and (vector? status)
           (= (vector-length status) 4)
           (boolean? (vector-ref status 0))
           (natural? (vector-ref status 1))
           (or (not (vector-ref status 2))
               (string? (vector-ref status 2)))
           (or (not (vector-ref status 3))
               (string? (vector-ref status 3))))))

  (define capability-name
    (lambda (capability)
      (cond
       [(fx= capability lws-cap-http1) 'http1]
       [(fx= capability lws-cap-http2) 'http2]
       [(fx= capability lws-cap-tls) 'tls]
       [(fx= capability lws-cap-socks5) 'socks5]
       [(fx= capability lws-cap-external-poll) 'external-poll]
       [else 'unknown])))

  (define capability-value?
    (lambda (capability)
      (and (fixnum? capability)
           (memv capability
                 (list lws-cap-http1
                       lws-cap-http2
                       lws-cap-tls
                       lws-cap-socks5
                       lws-cap-external-poll)))))

  (define capability-mask?
    (lambda (capability-mask)
      (and (fixnum? capability-mask)
           (fx>= capability-mask 0))))

  #|proc:lws-status
The `lws-status` procedure returns `#(available? capability-mask version error)` for the optional
libwebsockets HTTP runtime. Version and error are strings or `#f`.
|#
  (define-who lws-status
    (lambda ()
      (let ([status (ffi-lws-status)])
        (unless (valid-lws-status? status)
          (raise-net-error who 'internal-ffi
                           "malformed libwebsockets HTTP loader status" status))
        status)))

  #|proc:lws-capability?
The `lws-capability?` procedure returns whether `capability-mask` contains the single capability
bit `capability`.
|#
  (define-who lws-capability?
    (lambda (capability-mask capability)
      (pcheck ([capability-mask? capability-mask] [capability-value? capability])
        (not (fxzero? (fxand capability-mask capability))))))

  #|proc:lws-require-capability!
The `lws-require-capability!` procedure requires the single capability bit `capability`.
The `name` parameter identifies that capability in an error. It returns an unspecified value or
raises a network error when libwebsockets is unavailable or lacks the capability.
|#
  (define-who lws-require-capability!
    (lambda (capability name)
      (pcheck ([capability-value? capability] [symbol? name])
        (let* ([status (lws-status)]
               [available? (vector-ref status 0)]
               [capabilities (vector-ref status 1)])
          (unless available?
            (raise-net-error who 'unsupported
                             (or (vector-ref status 3)
                                 "libwebsockets HTTP is unavailable")
                             name))
          (unless (lws-capability? capabilities capability)
            (raise-net-error
             who 'unsupported
             (format "libwebsockets HTTP is missing capability ~a"
                     (capability-name capability))
             name))))))
  )
