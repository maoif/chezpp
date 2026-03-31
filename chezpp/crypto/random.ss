(library (chezpp crypto random)
  (export random-bytes
          random-key-bytes
          random-nonce
          random-bytes!
          random-status)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp crypto ffi)
          (chezpp crypto private))

  #|proc:random-status
The `random-status` procedure returns `#t` when the OpenSSL random source is ready.
|#
  (define random-status
    (lambda ()
      (not (fx= 0 (ffi-random-status)))))

  #|proc:random-bytes
The `random-bytes` procedure returns a fresh random bytevector of length `len`.
|#
  (define-who random-bytes
    (lambda (len)
      (pcheck ([natural? len])
              (let ([bv (ffi-random-bytevector len)])
                (when (eq? bv #f)
                  (errorf who "failed to obtain random bytes"))
                bv))))

  #|proc:random-key-bytes
The `random-key-bytes` procedure returns a fresh random key bytevector of length `len`.
|#
  (define random-key-bytes
    random-bytes)

  #|proc:random-nonce
The `random-nonce` procedure returns a fresh random nonce bytevector of length `len`.
|#
  (define random-nonce
    random-bytes)

  #|proc:random-bytes!
The `random-bytes!` procedure fills a bytevector slice with random bytes.
|#
  (define-who random-bytes!
    (case-lambda
      [(bv)
       (pcheck ([bytevector? bv])
               (random-bytes! bv 0 (bytevector-length bv)))]
      [(bv start)
       (pcheck ([bytevector? bv])
               (random-bytes! bv start (bytevector-length bv)))]
      [(bv start stop)
       (pcheck ([bytevector? bv])
               (check-slice who (bytevector-length bv) start stop)
               (when (fx= 0 (ffi-random-fill! bv start stop))
                 (errorf who "failed to fill bytevector with random bytes"))
               bv)]))
  )
