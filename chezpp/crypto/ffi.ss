(library (chezpp crypto ffi)
  (export ffi-random-status
          ffi-random-bytevector
          ffi-random-fill!
          ffi-constant-time-eq
          ffi-hash-bytevector
          ffi-hash-string
          ffi-hash-output-size
          ffi-hash-block-size
          ffi-hash-state-create
          ffi-hash-state-destroy
          ffi-hash-state-get
          ffi-hash-state-finalize
          ffi-hash-state-reset!
          ffi-hash-state-update-bytevector!
          ffi-hash-state-update-string!
          ffi-hmac-state-create
          ffi-hmac-state-destroy
          ffi-hmac-state-get
          ffi-hmac-state-finalize
          ffi-hmac-state-reset!
          ffi-hmac-state-update-bytevector!
          ffi-hmac-state-update-string!
          ffi-hkdf
          ffi-hkdf-extract
          ffi-hkdf-expand
          ffi-pbkdf2
          ffi-scrypt
          ffi-aead-encrypt
          ffi-aead-decrypt
          ffi-cipher-key-size
          ffi-cipher-iv-size
          ffi-cipher-block-size
          ffi-cipher-state-create
          ffi-cipher-state-destroy
          ffi-cipher-state-update
          ffi-cipher-state-finalize
          ffi-cipher-state-reset!
          ffi-pkey-generate
          ffi-pkey-free
          ffi-pkey-algorithm
          ffi-pkey-bits
          ffi-pkey-public-from-private
          ffi-pkey-store-private-pem
          ffi-pkey-store-public-pem
          ffi-pkey-store-private-der
          ffi-pkey-store-public-der
          ffi-pkey-load-private-pem
          ffi-pkey-load-public-pem
          ffi-pkey-load-private-der
          ffi-pkey-load-public-der
          ffi-sign-message
          ffi-verify-message
          ffi-derive-shared-secret
          ffi-cert-load-pem
          ffi-cert-load-der
          ffi-cert-free
          ffi-cert-subject
          ffi-cert-issuer
          ffi-cert-not-before
          ffi-cert-not-after
          ffi-cert-subject-alt-names
          ffi-cert-hostname-matches
          ffi-cert-public-key-der
          ffi-cert-serial-number
          ffi-cert-fingerprint
          ffi-cert-store-create
          ffi-cert-store-destroy
          ffi-cert-store-add
          ffi-cert-store-load-defaults
          ffi-cert-verify-state-create
          ffi-cert-verify-state-add-chain-cert
          ffi-cert-verify-state-verify
          ffi-cert-verify-state-destroy)
  (import (chezpp chez))

  (define ffi-random-status (foreign-procedure "crypto_random_status" () int))
  (define ffi-random-bytevector (foreign-procedure "crypto_random_bytevector" (unsigned-64) ptr))
  (define ffi-random-fill! (foreign-procedure "crypto_random_fill" (ptr unsigned-64 unsigned-64) int))
  (define ffi-constant-time-eq
    (foreign-procedure "crypto_constant_time_eq"
                       (ptr unsigned-64 unsigned-64 ptr unsigned-64 unsigned-64)
                       int))

  (define ffi-hash-bytevector
    (foreign-procedure "crypto_hash_bytevector" (ptr ptr unsigned-64 unsigned-64) ptr))
  (define ffi-hash-string
    (foreign-procedure "crypto_hash_string" (ptr ptr unsigned-64 unsigned-64) ptr))
  (define ffi-hash-output-size
    (foreign-procedure "crypto_hash_output_size" (ptr) int))
  (define ffi-hash-block-size
    (foreign-procedure "crypto_hash_block_size" (ptr) int))
  (define ffi-hash-state-create
    (foreign-procedure "crypto_hash_state_create" (ptr) void*))
  (define ffi-hash-state-destroy
    (foreign-procedure "crypto_hash_state_destroy" (void*) void))
  (define ffi-hash-state-get
    (foreign-procedure "crypto_hash_state_get" (void*) ptr))
  (define ffi-hash-state-finalize
    (foreign-procedure "crypto_hash_state_finalize" (void*) ptr))
  (define ffi-hash-state-reset!
    (foreign-procedure "crypto_hash_state_reset" (void*) int))
  (define ffi-hash-state-update-bytevector!
    (foreign-procedure "crypto_hash_state_update_bytevector"
                       (void* ptr unsigned-64 unsigned-64)
                       int))
  (define ffi-hash-state-update-string!
    (foreign-procedure "crypto_hash_state_update_string"
                       (void* ptr unsigned-64 unsigned-64)
                       int))

  (define ffi-hmac-state-create
    (foreign-procedure "crypto_hmac_state_create" (ptr ptr unsigned-64 unsigned-64) void*))
  (define ffi-hmac-state-destroy
    (foreign-procedure "crypto_hmac_state_destroy" (void*) void))
  (define ffi-hmac-state-get
    (foreign-procedure "crypto_hmac_state_get" (void*) ptr))
  (define ffi-hmac-state-finalize
    (foreign-procedure "crypto_hmac_state_finalize" (void*) ptr))
  (define ffi-hmac-state-reset!
    (foreign-procedure "crypto_hmac_state_reset" (void*) int))
  (define ffi-hmac-state-update-bytevector!
    (foreign-procedure "crypto_hmac_state_update_bytevector"
                       (void* ptr unsigned-64 unsigned-64)
                       int))
  (define ffi-hmac-state-update-string!
    (foreign-procedure "crypto_hmac_state_update_string"
                       (void* ptr unsigned-64 unsigned-64)
                       int))

  (define ffi-hkdf
    (foreign-procedure "crypto_hkdf"
                       (ptr ptr unsigned-64 unsigned-64
                            ptr unsigned-64 unsigned-64
                            ptr unsigned-64 unsigned-64 int)
                       ptr))
  (define ffi-hkdf-extract
    (foreign-procedure "crypto_hkdf_extract"
                       (ptr ptr unsigned-64 unsigned-64 ptr unsigned-64 unsigned-64)
                       ptr))
  (define ffi-hkdf-expand
    (foreign-procedure "crypto_hkdf_expand"
                       (ptr ptr unsigned-64 unsigned-64 ptr unsigned-64 unsigned-64 int)
                       ptr))
  (define ffi-pbkdf2
    (foreign-procedure "crypto_pbkdf2"
                       (ptr ptr unsigned-64 unsigned-64 ptr unsigned-64 unsigned-64 int int)
                       ptr))
  (define ffi-scrypt
    (foreign-procedure "crypto_scrypt"
                       (ptr unsigned-64 unsigned-64 ptr unsigned-64 unsigned-64 int int int int)
                       ptr))

  (define ffi-aead-encrypt
    (foreign-procedure "crypto_aead_encrypt"
                       (ptr
                        ptr unsigned-64 unsigned-64
                        ptr unsigned-64 unsigned-64
                        ptr unsigned-64 unsigned-64
                        ptr unsigned-64 unsigned-64
                        int)
                       ptr))
  (define ffi-aead-decrypt
    (foreign-procedure "crypto_aead_decrypt"
                       (ptr
                        ptr unsigned-64 unsigned-64
                        ptr unsigned-64 unsigned-64
                        ptr unsigned-64 unsigned-64
                        ptr unsigned-64 unsigned-64
                        ptr unsigned-64 unsigned-64)
                       ptr))

  (define ffi-cipher-key-size
    (foreign-procedure "crypto_cipher_key_size" (ptr) int))
  (define ffi-cipher-iv-size
    (foreign-procedure "crypto_cipher_iv_size" (ptr) int))
  (define ffi-cipher-block-size
    (foreign-procedure "crypto_cipher_block_size" (ptr) int))
  (define ffi-cipher-state-create
    (foreign-procedure "crypto_cipher_state_create"
                       (ptr int ptr unsigned-64 unsigned-64 ptr unsigned-64 unsigned-64)
                       void*))
  (define ffi-cipher-state-destroy
    (foreign-procedure "crypto_cipher_state_destroy" (void*) void))
  (define ffi-cipher-state-update
    (foreign-procedure "crypto_cipher_state_update"
                       (void* ptr unsigned-64 unsigned-64)
                       ptr))
  (define ffi-cipher-state-finalize
    (foreign-procedure "crypto_cipher_state_finalize" (void*) ptr))
  (define ffi-cipher-state-reset!
    (foreign-procedure "crypto_cipher_state_reset" (void*) int))

  (define ffi-pkey-generate
    (foreign-procedure "crypto_pkey_generate" (ptr int ptr) void*))
  (define ffi-pkey-free
    (foreign-procedure "crypto_pkey_free" (void*) void))
  (define ffi-pkey-algorithm
    (foreign-procedure "crypto_pkey_algorithm" (void*) ptr))
  (define ffi-pkey-bits
    (foreign-procedure "crypto_pkey_bits" (void*) int))
  (define ffi-pkey-public-from-private
    (foreign-procedure "crypto_pkey_public_from_private" (void*) void*))
  (define ffi-pkey-store-private-pem
    (foreign-procedure "crypto_pkey_store_private_pem" (void*) ptr))
  (define ffi-pkey-store-public-pem
    (foreign-procedure "crypto_pkey_store_public_pem" (void*) ptr))
  (define ffi-pkey-store-private-der
    (foreign-procedure "crypto_pkey_store_private_der" (void*) ptr))
  (define ffi-pkey-store-public-der
    (foreign-procedure "crypto_pkey_store_public_der" (void*) ptr))
  (define ffi-pkey-load-private-pem
    (foreign-procedure "crypto_pkey_load_private_pem" (ptr unsigned-64 unsigned-64) void*))
  (define ffi-pkey-load-public-pem
    (foreign-procedure "crypto_pkey_load_public_pem" (ptr unsigned-64 unsigned-64) void*))
  (define ffi-pkey-load-private-der
    (foreign-procedure "crypto_pkey_load_private_der" (ptr unsigned-64 unsigned-64) void*))
  (define ffi-pkey-load-public-der
    (foreign-procedure "crypto_pkey_load_public_der" (ptr unsigned-64 unsigned-64) void*))
  (define ffi-sign-message
    (foreign-procedure "crypto_sign_message" (ptr ptr void* ptr unsigned-64 unsigned-64) ptr))
  (define ffi-verify-message
    (foreign-procedure "crypto_verify_message"
                       (ptr ptr void* ptr unsigned-64 unsigned-64 ptr unsigned-64 unsigned-64)
                       int))
  (define ffi-derive-shared-secret
    (foreign-procedure "crypto_derive_shared_secret" (ptr void* void*) ptr))

  (define ffi-cert-load-pem
    (foreign-procedure "crypto_cert_load_pem" (ptr unsigned-64 unsigned-64) void*))
  (define ffi-cert-load-der
    (foreign-procedure "crypto_cert_load_der" (ptr unsigned-64 unsigned-64) void*))
  (define ffi-cert-free
    (foreign-procedure "crypto_cert_free" (void*) void))
  (define ffi-cert-subject
    (foreign-procedure "crypto_cert_subject" (void*) ptr))
  (define ffi-cert-issuer
    (foreign-procedure "crypto_cert_issuer" (void*) ptr))
  (define ffi-cert-not-before
    (foreign-procedure "crypto_cert_not_before" (void*) ptr))
  (define ffi-cert-not-after
    (foreign-procedure "crypto_cert_not_after" (void*) ptr))
  (define ffi-cert-subject-alt-names
    (foreign-procedure "crypto_cert_subject_alt_names" (void*) ptr))
  (define ffi-cert-hostname-matches
    (foreign-procedure "crypto_cert_hostname_matches" (void* ptr) int))
  (define ffi-cert-public-key-der
    (foreign-procedure "crypto_cert_public_key_der" (void*) ptr))
  (define ffi-cert-serial-number
    (foreign-procedure "crypto_cert_serial_number" (void*) ptr))
  (define ffi-cert-fingerprint
    (foreign-procedure "crypto_cert_fingerprint" (void* ptr) ptr))
  (define ffi-cert-store-create
    (foreign-procedure "crypto_cert_store_create" (int) void*))
  (define ffi-cert-store-destroy
    (foreign-procedure "crypto_cert_store_destroy" (void*) void))
  (define ffi-cert-store-add
    (foreign-procedure "crypto_cert_store_add" (void* void*) int))
  (define ffi-cert-store-load-defaults
    (foreign-procedure "crypto_cert_store_load_defaults" (void*) int))
  (define ffi-cert-verify-state-create
    (foreign-procedure "crypto_cert_verify_state_create" (void* void* ptr) void*))
  (define ffi-cert-verify-state-add-chain-cert
    (foreign-procedure "crypto_cert_verify_state_add_chain_cert" (void* void*) int))
  (define ffi-cert-verify-state-verify
    (foreign-procedure "crypto_cert_verify_state_verify" (void*) int))
  (define ffi-cert-verify-state-destroy
    (foreign-procedure "crypto_cert_verify_state_destroy" (void*) void))
  )
