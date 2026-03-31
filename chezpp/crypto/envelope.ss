(library (chezpp crypto envelope)
  (export seal-envelope
          seal-envelope/aad
          open-envelope
          open-envelope/aad
          envelope?
          envelope-kem
          envelope-cipher
          envelope-ephemeral-key
          envelope-nonce
          envelope-ciphertext
          envelope-tag)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp crypto private)
          (chezpp crypto random)
          (chezpp crypto pkey)
          (chezpp crypto aead)
          (chezpp crypto kdf)
          (chezpp crypto key-agreement))

  (define envelope? crypto-envelope?)
  (define envelope-kem crypto-envelope-kem)
  (define envelope-cipher crypto-envelope-cipher)
  (define envelope-ephemeral-key crypto-envelope-ephemeral-key)
  (define envelope-nonce crypto-envelope-nonce)
  (define envelope-ciphertext crypto-envelope-ciphertext)
  (define envelope-tag crypto-envelope-tag)

  (define derive-envelope-key
    (lambda (shared)
      (hkdf 'sha256 shared (bytevector) (string->utf8 "chezpp-envelope") 32)))

  #|proc:seal-envelope
The `seal-envelope` procedure encrypts `plaintext` for `recipient-public-key` and returns an envelope record.
|#
  (define-who seal-envelope
    (lambda (recipient-public-key plaintext)
      (seal-envelope/aad recipient-public-key (bytevector) plaintext)))

  #|proc:seal-envelope/aad
The `seal-envelope/aad` procedure encrypts `plaintext` with associated data for `recipient-public-key`.
|#
  (define-who seal-envelope/aad
    (lambda (recipient-public-key aad plaintext)
      (pcheck ([public-key? recipient-public-key] [bytevector? aad])
              (unless (eq? (public-key-algorithm recipient-public-key) 'x25519)
                (errorf who "safe envelope currently requires an x25519 recipient public key"))
              (let* ([pt (string/bytevector->utf8 plaintext)]
                     [epriv (generate-private-key 'x25519)]
                     [epub (private-key->public-key epriv)]
                     [shared (derive-shared-secret 'x25519 epriv recipient-public-key)]
                     [key (derive-envelope-key shared)]
                     [nonce (random-bytes 12)])
                (let-values ([(ct tag) (aead-encrypt/aad 'chacha20-poly1305 key nonce aad pt)])
                  (make-crypto-envelope 'x25519
                                        'chacha20-poly1305
                                        (store-public-key epub 'pem)
                                        nonce
                                        ct
                                        tag))))))

  #|proc:open-envelope
The `open-envelope` procedure decrypts `envelope` using `recipient-private-key`.
|#
  (define-who open-envelope
    (lambda (recipient-private-key envelope)
      (open-envelope/aad recipient-private-key (bytevector) envelope)))

  #|proc:open-envelope/aad
The `open-envelope/aad` procedure decrypts `envelope` with associated data using `recipient-private-key`.
|#
  (define-who open-envelope/aad
    (lambda (recipient-private-key aad envelope)
      (pcheck ([private-key? recipient-private-key] [bytevector? aad] [envelope? envelope])
              (unless (eq? (private-key-algorithm recipient-private-key) 'x25519)
                (errorf who "safe envelope currently requires an x25519 recipient private key"))
              (unless (and (eq? (envelope-kem envelope) 'x25519)
                           (eq? (envelope-cipher envelope) 'chacha20-poly1305))
                (errorf who "unsupported envelope"))
              (let* ([epub (pem->public-key (envelope-ephemeral-key envelope))]
                     [shared (derive-shared-secret 'x25519 recipient-private-key epub)]
                     [key (derive-envelope-key shared)])
                (aead-decrypt/aad 'chacha20-poly1305 key
                                  (envelope-nonce envelope)
                                  aad
                                  (envelope-ciphertext envelope)
                                  (envelope-tag envelope))))))
  )
