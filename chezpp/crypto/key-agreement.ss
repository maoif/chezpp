(library (chezpp crypto key-agreement)
  (export derive-shared-secret)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp crypto ffi)
          (chezpp crypto private)
          (chezpp crypto pkey))

  (define key-agreement-algorithms '(x25519 ecdh))
  (define check-key-agreement-algorithm
    (lambda (who alg)
      (unless (memq alg key-agreement-algorithms)
        (errorf who "valid key agreement algorithm is one of ~a" key-agreement-algorithms))))

  #|proc:derive-shared-secret
The `derive-shared-secret` procedure derives a shared secret from `private-key` and `peer-public-key`.
|#
  (define-who derive-shared-secret
    (lambda (alg private-key peer-public-key)
      (pcheck ([private-key? private-key] [public-key? peer-public-key])
              (check-key-agreement-algorithm who alg)
              (let ([ans (ffi-derive-shared-secret alg
                                                   (crypto-private-key-handle private-key)
                                                   (crypto-public-key-handle peer-public-key))])
                (when (eq? ans #f)
                  (errorf who "failed to derive shared secret using ~s" alg))
                ans))))
  )
