(library (chezpp crypto hash)
  (export hash-algorithms
          hash
          hash-bytevector
          hash-string
          hash-file
          hash-output-size
          hash-block-size
          make-hash-state
          hash-state?
          hash-update-bytevector!
          hash-update-string!
          hash-get
          hash-finalize!
          hash-reset!
          hash-destroy!
          call-with-hash-state)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp file)
          (chezpp crypto ffi)
          (chezpp crypto private))

  (define hash-algorithms
    '(sha224 sha256 sha384 sha512 sha512-224 sha512-256
      sha3-224 sha3-256 sha3-384 sha3-512
      blake2b-512 blake2s-256))

  (define check-hash-algorithm
    (lambda (who which)
      (unless (memq which hash-algorithms)
        (errorf who "valid hash algorithm is one of ~a" hash-algorithms))))

  (define-record-type (hash-state %make-hash-state hash-state?)
    (opaque #t)
    (fields (mutable handle)
            algorithm
            (mutable destroyed?)
            (mutable finalized?)))

  (define check-live-state
    (lambda (who st)
      (when (hash-state-destroyed? st)
        (errorf who "hash state is destroyed"))
      (when (hash-state-finalized? st)
        (errorf who "hash state is finalized"))))

  #|proc:hash-output-size
The `hash-output-size` procedure returns the digest size in bytes for `which`.
|#
  (define-who hash-output-size
    (lambda (which)
      (check-hash-algorithm who which)
      (let ([n (ffi-hash-output-size which)])
        (when (fx< n 0)
          (errorf who "failed to query output size for ~s" which))
        n)))

  #|proc:hash-block-size
The `hash-block-size` procedure returns the digest block size in bytes for `which`.
|#
  (define-who hash-block-size
    (lambda (which)
      (check-hash-algorithm who which)
      (let ([n (ffi-hash-block-size which)])
        (when (fx< n 0)
          (errorf who "failed to query block size for ~s" which))
        n)))

  #|proc:hash-bytevector
The `hash-bytevector` procedure hashes a bytevector slice.
|#
  (define-who hash-bytevector
    (case-lambda
      [(which bv)
       (pcheck ([bytevector? bv])
               (hash-bytevector which bv 0 (bytevector-length bv)))]
      [(which bv start)
       (pcheck ([bytevector? bv])
               (hash-bytevector which bv start (bytevector-length bv)))]
      [(which bv start stop)
       (pcheck ([bytevector? bv])
               (check-hash-algorithm who which)
               (check-slice who (bytevector-length bv) start stop)
               (let ([ans (ffi-hash-bytevector which bv start stop)])
                 (when (eq? ans #f)
                   (errorf who "failed to hash bytevector using ~s" which))
                 ans))]))

  #|proc:hash-string
The `hash-string` procedure hashes a string slice using the string's UTF-32 code units.
|#
  (define-who hash-string
    (case-lambda
      [(which str)
       (pcheck ([string? str])
               (hash-string which str 0 (string-length str)))]
      [(which str start)
       (pcheck ([string? str])
               (hash-string which str start (string-length str)))]
      [(which str start stop)
       (pcheck ([string? str])
               (check-hash-algorithm who which)
               (check-slice who (string-length str) start stop)
               (let ([ans (ffi-hash-string which str start stop)])
                 (when (eq? ans #f)
                   (errorf who "failed to hash string using ~s" which))
                 ans))]))

  #|proc:hash-file
The `hash-file` procedure hashes a file slice by streaming it through a hash state.
|#
  (define-who hash-file
    (case-lambda
      [(which path)
       (pcheck ([file-regular? path])
               (hash-file which path 0 (file-size path)))]
      [(which path start)
       (pcheck ([file-regular? path])
               (hash-file which path start (file-size path)))]
      [(which path start stop)
       (pcheck ([file-regular? path])
               (check-hash-algorithm who which)
               (check-slice who (file-size path) start stop)
               (call-with-hash-state
                which
                (lambda (st)
                  (let ([p (open-file-input-port path)]
                        [buf (make-bytevector 4096 0)])
                    (dynamic-wind
                      void
                      (lambda ()
                        (set-port-position! p start)
                        (let loop ([remaining (fx- stop start)])
                          (if (fx= remaining 0)
                              (hash-finalize! st)
                              (let ([n (get-bytevector-n! p buf 0
                                                          (if (fx< remaining 4096)
                                                              remaining
                                                              4096))])
                                (hash-update-bytevector! st buf 0 n)
                                (loop (fx- remaining n))))))
                      (lambda ()
                        (close-port p)))))))]))

  #|proc:hash
The `hash` procedure hashes a string or bytevector value.
|#
  (define-who hash
    (lambda (which x)
      (pcase x
        [bytevector? (hash-bytevector which x)]
        [string? (hash-string which x)]
        [else (errorf who "expected string or bytevector, given ~s" x)])))

  #|proc:make-hash-state
The `make-hash-state` procedure creates an incremental hash state for `which`.
|#
  (define-who make-hash-state
    (lambda (which)
      (check-hash-algorithm who which)
      (let ([handle (ffi-hash-state-create which)])
        (when (eq? handle 0)
          (errorf who "failed to create hash state for ~s" which))
        (%make-hash-state handle which #f #f))))

  #|proc:hash-update-bytevector!
The `hash-update-bytevector!` procedure updates a hash state with a bytevector slice.
|#
  (define-who hash-update-bytevector!
    (case-lambda
      [(st bv)
       (pcheck ([hash-state? st] [bytevector? bv])
               (hash-update-bytevector! st bv 0 (bytevector-length bv)))]
      [(st bv start)
       (pcheck ([hash-state? st] [bytevector? bv])
               (hash-update-bytevector! st bv start (bytevector-length bv)))]
      [(st bv start stop)
       (pcheck ([hash-state? st] [bytevector? bv])
               (check-live-state who st)
               (check-slice who (bytevector-length bv) start stop)
               (when (fx= 0 (ffi-hash-state-update-bytevector! (hash-state-handle st) bv start stop))
                 (errorf who "failed to update hash state"))
               st)]))

  #|proc:hash-update-string!
The `hash-update-string!` procedure updates a hash state with a string slice.
|#
  (define-who hash-update-string!
    (case-lambda
      [(st str)
       (pcheck ([hash-state? st] [string? str])
               (hash-update-string! st str 0 (string-length str)))]
      [(st str start)
       (pcheck ([hash-state? st] [string? str])
               (hash-update-string! st str start (string-length str)))]
      [(st str start stop)
       (pcheck ([hash-state? st] [string? str])
               (check-live-state who st)
               (check-slice who (string-length str) start stop)
               (when (fx= 0 (ffi-hash-state-update-string! (hash-state-handle st) str start stop))
                 (errorf who "failed to update hash state"))
               st)]))

  #|proc:hash-get
The `hash-get` procedure returns the current digest without finalizing the state.
|#
  (define-who hash-get
    (lambda (st)
      (pcheck ([hash-state? st])
              (check-live-state who st)
              (let ([ans (ffi-hash-state-get (hash-state-handle st))])
                (when (eq? ans #f)
                  (errorf who "failed to obtain hash state digest"))
                ans))))

  #|proc:hash-finalize!
The `hash-finalize!` procedure finalizes a hash state and returns the digest.
|#
  (define-who hash-finalize!
    (lambda (st)
      (pcheck ([hash-state? st])
              (check-live-state who st)
              (let ([ans (ffi-hash-state-finalize (hash-state-handle st))])
                (when (eq? ans #f)
                  (errorf who "failed to finalize hash state"))
                (ffi-hash-state-destroy (hash-state-handle st))
                (hash-state-handle-set! st 0)
                (hash-state-finalized?-set! st #t)
                ans))))

  #|proc:hash-reset!
The `hash-reset!` procedure resets a hash state back to its initial algorithm state.
|#
  (define-who hash-reset!
    (lambda (st)
      (pcheck ([hash-state? st])
              (check-live-state who st)
              (when (fx= 0 (ffi-hash-state-reset! (hash-state-handle st)))
                (errorf who "failed to reset hash state"))
              st)))

  #|proc:hash-destroy!
The `hash-destroy!` procedure releases the foreign resources owned by a hash state.
|#
  (define-who hash-destroy!
    (lambda (st)
      (pcheck ([hash-state? st])
              (unless (hash-state-destroyed? st)
                (unless (fx= (hash-state-handle st) 0)
                  (ffi-hash-state-destroy (hash-state-handle st))
                  (hash-state-handle-set! st 0))
                (hash-state-destroyed?-set! st #t))
              st)))

  #|proc:call-with-hash-state
The `call-with-hash-state` procedure creates a hash state, passes it to `proc`, and always destroys it afterwards.
|#
  (define-who call-with-hash-state
    (lambda (which proc)
      (pcheck ([procedure? proc])
              (check-hash-algorithm who which)
              (let ([st (make-hash-state which)])
                (dynamic-wind
                  void
                  (lambda () (proc st))
                  (lambda () (hash-destroy! st)))))))
  )
