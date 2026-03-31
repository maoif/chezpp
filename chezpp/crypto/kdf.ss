(library (chezpp crypto kdf)
  (export kdf-algorithms
          hkdf
          hkdf-extract
          hkdf-expand
          pbkdf2
          scrypt)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp crypto ffi)
          (chezpp crypto private))

  (define kdf-algorithms
    '(hkdf pbkdf2 scrypt))

  (define digest-algorithms
    '(sha224 sha256 sha384 sha512 sha512-224 sha512-256
      sha3-224 sha3-256 sha3-384 sha3-512
      blake2b-512 blake2s-256))

  (define check-digest-algorithm
    (lambda (who which)
      (unless (memq which digest-algorithms)
        (errorf who "valid digest algorithm is one of ~a" digest-algorithms))))

  (define normalized-bytevector
    (lambda (who x)
      (pcase x
        [bytevector? x]
        [string? (string->utf8 x)]
        [else (errorf who "expected string or bytevector, given ~s" x)])))

  #|proc:hkdf
The `hkdf` procedure performs HKDF extract-and-expand and returns `len` bytes.
|#
  (define-who hkdf
    (lambda (which ikm salt info len)
      (check-digest-algorithm who which)
      (let* ([ikm-bv (normalized-bytevector who ikm)]
             [salt-bv (normalized-bytevector who salt)]
             [info-bv (normalized-bytevector who info)]
             [ans (ffi-hkdf which
                            ikm-bv 0 (bytevector-length ikm-bv)
                            salt-bv 0 (bytevector-length salt-bv)
                            info-bv 0 (bytevector-length info-bv)
                            len)])
        (when (eq? ans #f)
          (errorf who "HKDF failed for ~s" which))
        ans)))

  #|proc:hkdf-extract
The `hkdf-extract` procedure returns the HKDF pseudorandom key for `ikm` and `salt`.
|#
  (define-who hkdf-extract
    (lambda (which ikm salt)
      (check-digest-algorithm who which)
      (let* ([ikm-bv (normalized-bytevector who ikm)]
             [salt-bv (normalized-bytevector who salt)]
             [ans (ffi-hkdf-extract which
                                    ikm-bv 0 (bytevector-length ikm-bv)
                                    salt-bv 0 (bytevector-length salt-bv))])
        (when (eq? ans #f)
          (errorf who "HKDF extract failed for ~s" which))
        ans)))

  #|proc:hkdf-expand
The `hkdf-expand` procedure expands a pseudorandom key with `info` into `len` bytes.
|#
  (define-who hkdf-expand
    (lambda (which prk info len)
      (check-digest-algorithm who which)
      (let* ([prk-bv (normalized-bytevector who prk)]
             [info-bv (normalized-bytevector who info)]
             [ans (ffi-hkdf-expand which
                                   prk-bv 0 (bytevector-length prk-bv)
                                   info-bv 0 (bytevector-length info-bv)
                                   len)])
        (when (eq? ans #f)
          (errorf who "HKDF expand failed for ~s" which))
        ans)))

  #|proc:pbkdf2
The `pbkdf2` procedure derives a key from `password` and `salt`.
|#
  (define-who pbkdf2
    (lambda (which password salt iterations len)
      (pcheck ([natural? iterations len])
              (check-digest-algorithm who which)
              (let* ([password-bv (normalized-bytevector who password)]
                     [salt-bv (normalized-bytevector who salt)]
                     [ans (ffi-pbkdf2 which
                                      password-bv 0 (bytevector-length password-bv)
                                      salt-bv 0 (bytevector-length salt-bv)
                                      iterations len)])
                (when (eq? ans #f)
                  (errorf who "PBKDF2 failed for ~s" which))
                ans))))

  #|proc:scrypt
The `scrypt` procedure derives a key from `password` and `salt` using the given scrypt parameters.
|#
  (define-who scrypt
    (lambda (password salt n r p len)
      (pcheck ([natural? n r p len])
              (let* ([password-bv (normalized-bytevector who password)]
                     [salt-bv (normalized-bytevector who salt)]
                     [ans (ffi-scrypt password-bv 0 (bytevector-length password-bv)
                                      salt-bv 0 (bytevector-length salt-bv)
                                      n r p len)])
                (when (eq? ans #f)
                  (errorf who "scrypt failed"))
                ans))))
  )
