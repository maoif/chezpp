(library (chezpp crypto unsafe)
  (export unsafe-hash-algorithms
          unsafe-hash
          unsafe-hash-bytevector
          unsafe-hash-string)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp crypto ffi)
          (chezpp crypto private))

  (define unsafe-hash-algorithms '(md5 sha1))

  (define check-unsafe-hash
    (lambda (who alg)
      (unless (memq alg unsafe-hash-algorithms)
        (errorf who "valid unsafe hash algorithm is one of ~a" unsafe-hash-algorithms))))

  #|proc:unsafe-hash-bytevector
The `unsafe-hash-bytevector` procedure hashes a bytevector slice with a hazardous legacy digest.
|#
  (define-who unsafe-hash-bytevector
    (case-lambda
      [(alg bv) (unsafe-hash-bytevector alg bv 0 (bytevector-length bv))]
      [(alg bv start) (unsafe-hash-bytevector alg bv start (bytevector-length bv))]
      [(alg bv start stop)
       (pcheck ([bytevector? bv])
               (check-unsafe-hash who alg)
               (check-slice who (bytevector-length bv) start stop)
               (let ([ans (ffi-hash-bytevector alg bv start stop)])
                 (when (eq? ans #f)
                   (errorf who "failed to hash bytevector using ~s" alg))
                 ans))]))

  #|proc:unsafe-hash-string
The `unsafe-hash-string` procedure hashes a string slice with a hazardous legacy digest.
|#
  (define-who unsafe-hash-string
    (case-lambda
      [(alg str) (unsafe-hash-string alg str 0 (string-length str))]
      [(alg str start) (unsafe-hash-string alg str start (string-length str))]
      [(alg str start stop)
       (pcheck ([string? str])
               (check-unsafe-hash who alg)
               (check-slice who (string-length str) start stop)
               (let ([ans (ffi-hash-string alg str start stop)])
                 (when (eq? ans #f)
                   (errorf who "failed to hash string using ~s" alg))
                 ans))]))

  #|proc:unsafe-hash
The `unsafe-hash` procedure hashes a string or bytevector with a hazardous legacy digest.
|#
  (define-who unsafe-hash
    (lambda (alg x)
      (pcase x
        [bytevector? (unsafe-hash-bytevector alg x)]
        [string? (unsafe-hash-string alg x)]
        [else (errorf who "expected string or bytevector, given ~s" x)])))
  )
