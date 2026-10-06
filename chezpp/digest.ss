(library (chezpp digest)
  (export md5-bytevector
          sha224-bytevector
          sha256-bytevector
          sha384-bytevector
          sha512-bytevector
          sha512-224-bytevector
          sha512-256-bytevector
          sha3-224-bytevector
          sha3-256-bytevector
          sha3-384-bytevector
          sha3-512-bytevector
          blake2b-512-bytevector
          blake2s-256-bytevector
          blake3-bytevector

          md5-string
          sha224-string
          sha256-string
          sha384-string
          sha512-string
          sha512-224-string
          sha512-256-string
          sha3-224-string
          sha3-256-string
          sha3-384-string
          sha3-512-string
          blake2b-512-string
          blake2s-256-string
          blake3-string

          md5-file
          sha224-file
          sha256-file
          sha384-file
          sha512-file
          sha512-224-file
          sha512-256-file
          sha3-224-file
          sha3-256-file
          sha3-384-file
          sha3-512-file
          blake2b-512-file
          blake2s-256-file
          blake3-file

          make-digester digester?
          digester-update-string! digester-update-bytevector!
          digester-get digester-reset! digester-finalize!

          call-with-digester)
  (import (chezpp optional-library-check) (chezpp chez)
          (chezpp utils)
          (chezpp internal)
          (chezpp file))


  (define *digesters* '(md5
                        sha224 sha256 sha384 sha512
                        sha512-224 sha512-256
                        sha3-224 sha3-256 sha3-384 sha3-512
                        blake2b-512 blake2s-256
                        blake3))

  (define check-digester
    (lambda (who which)
      (unless (memq which *digesters*)
        (errorf who "valid digest algorithm is one of ~a" *digesters*))))

  (define ffi-blake3-load-error
    (foreign-procedure "chezpp_blake3_load_error" () ptr))
  (define ffi-openssl-load-error
    (foreign-procedure "crypto_openssl_load_error" () ptr))

  (define ensure-blake3
    (lambda (who)
      (let ([message (ffi-blake3-load-error)])
        (when message
          (error who message)))))

  (define ensure-openssl
    (lambda (who)
      (let ([message (ffi-openssl-load-error)])
        (when message
          (error who message)))))


;;;;===----------------------------------------------------------------------===
;;;;  specialized interface
;;;;===----------------------------------------------------------------------===

  (define ffi-md5-bv         (foreign-procedure "digest_md5_bv"        (ptr int int) ptr))
  (define ffi-sha224-bv      (foreign-procedure "digest_sha224_bv"     (ptr int int) ptr))
  (define ffi-sha256-bv      (foreign-procedure "digest_sha256_bv"     (ptr int int) ptr))
  (define ffi-sha384-bv      (foreign-procedure "digest_sha384_bv"     (ptr int int) ptr))
  (define ffi-sha512-bv      (foreign-procedure "digest_sha512_bv"     (ptr int int) ptr))
  (define ffi-sha512-224-bv  (foreign-procedure "digest_sha512_224_bv" (ptr int int) ptr))
  (define ffi-sha512-256-bv  (foreign-procedure "digest_sha512_256_bv" (ptr int int) ptr))
  (define ffi-sha3-224-bv    (foreign-procedure "digest_sha3_224_bv"   (ptr int int) ptr))
  (define ffi-sha3-256-bv    (foreign-procedure "digest_sha3_256_bv"   (ptr int int) ptr))
  (define ffi-sha3-384-bv    (foreign-procedure "digest_sha3_384_bv"   (ptr int int) ptr))
  (define ffi-sha3-512-bv    (foreign-procedure "digest_sha3_512_bv"   (ptr int int) ptr))
  (define ffi-blake2b-512-bv (foreign-procedure "digest_blake2b512_bv" (ptr int int) ptr))
  (define ffi-blake2s-256-bv (foreign-procedure "digest_blake2s256_bv" (ptr int int) ptr))

  (define ffi-md5-str         (foreign-procedure "digest_md5_str"        (ptr int int) ptr))
  (define ffi-sha224-str      (foreign-procedure "digest_sha224_str"     (ptr int int) ptr))
  (define ffi-sha256-str      (foreign-procedure "digest_sha256_str"     (ptr int int) ptr))
  (define ffi-sha384-str      (foreign-procedure "digest_sha384_str"     (ptr int int) ptr))
  (define ffi-sha512-str      (foreign-procedure "digest_sha512_str"     (ptr int int) ptr))
  (define ffi-sha512-224-str  (foreign-procedure "digest_sha512_224_str" (ptr int int) ptr))
  (define ffi-sha512-256-str  (foreign-procedure "digest_sha512_256_str" (ptr int int) ptr))
  (define ffi-sha3-224-str    (foreign-procedure "digest_sha3_224_str"   (ptr int int) ptr))
  (define ffi-sha3-256-str    (foreign-procedure "digest_sha3_256_str"   (ptr int int) ptr))
  (define ffi-sha3-384-str    (foreign-procedure "digest_sha3_384_str"   (ptr int int) ptr))
  (define ffi-sha3-512-str    (foreign-procedure "digest_sha3_512_str"   (ptr int int) ptr))
  (define ffi-blake2b-512-str (foreign-procedure "digest_blake2b512_str" (ptr int int) ptr))
  (define ffi-blake2s-256-str (foreign-procedure "digest_blake2s256_str" (ptr int int) ptr))

  (define ffi-blake3-bv/raw
    (foreign-procedure "digest_blake3_bv" (ptr int int) ptr))
  (define ffi-blake3-str/raw
    (foreign-procedure "digest_blake3_str" (ptr int int) ptr))

  (define ffi-blake3-bv
    (lambda (bytevector start stop)
      (ensure-blake3 'blake3-bytevector)
      (ffi-blake3-bv/raw bytevector start stop)))

  (define ffi-blake3-str
    (lambda (string start stop)
      (ensure-blake3 'blake3-string)
      (ffi-blake3-str/raw string start stop)))

  (define-syntax define-digester
    (syntax-rules ()
      [(_ name ffi ensure-library x? x-length)
       (define-who name
         (case-lambda
           [(x)
            (pcheck ([x? x]) (name x 0 (x-length x)))]
           [(x start)
            (pcheck ([x? x]) (name x start (x-length x)))]
           [(x start stop)
            (pcheck ([x? x] [natural? start stop])
                    (let ([len (x-length x)])
                      (when (fx> start stop)
                        (errorf who "start index ~a is greater than stop index ~a" start stop))
                      (when (fx> stop len)
                        (errorf who "stop index ~a is greater than total length ~a" stop len))
                      (ensure-library who)
                      (ffi x start stop)))]))]))

  (define-digester md5-bytevector ffi-md5-bv ensure-openssl bytevector? bytevector-length)
  (define-digester sha224-bytevector ffi-sha224-bv ensure-openssl bytevector? bytevector-length)
  (define-digester sha256-bytevector ffi-sha256-bv ensure-openssl bytevector? bytevector-length)
  (define-digester sha384-bytevector ffi-sha384-bv ensure-openssl bytevector? bytevector-length)
  (define-digester sha512-bytevector ffi-sha512-bv ensure-openssl bytevector? bytevector-length)
  (define-digester sha512-224-bytevector ffi-sha512-224-bv ensure-openssl bytevector? bytevector-length)
  (define-digester sha512-256-bytevector ffi-sha512-256-bv ensure-openssl bytevector? bytevector-length)
  (define-digester sha3-224-bytevector ffi-sha3-224-bv ensure-openssl bytevector? bytevector-length)
  (define-digester sha3-256-bytevector ffi-sha3-256-bv ensure-openssl bytevector? bytevector-length)
  (define-digester sha3-384-bytevector ffi-sha3-384-bv ensure-openssl bytevector? bytevector-length)
  (define-digester sha3-512-bytevector ffi-sha3-512-bv ensure-openssl bytevector? bytevector-length)
  (define-digester blake2b-512-bytevector ffi-blake2b-512-bv ensure-openssl bytevector? bytevector-length)
  (define-digester blake2s-256-bytevector ffi-blake2s-256-bv ensure-openssl bytevector? bytevector-length)
  (define-digester blake3-bytevector ffi-blake3-bv ensure-blake3 bytevector? bytevector-length)

  (define-digester md5-string ffi-md5-str ensure-openssl string? string-length)
  (define-digester sha224-string ffi-sha224-str ensure-openssl string? string-length)
  (define-digester sha256-string ffi-sha256-str ensure-openssl string? string-length)
  (define-digester sha384-string ffi-sha384-str ensure-openssl string? string-length)
  (define-digester sha512-string ffi-sha512-str ensure-openssl string? string-length)
  (define-digester sha512-224-string ffi-sha512-224-str ensure-openssl string? string-length)
  (define-digester sha512-256-string ffi-sha512-256-str ensure-openssl string? string-length)
  (define-digester sha3-224-string ffi-sha3-224-str ensure-openssl string? string-length)
  (define-digester sha3-256-string ffi-sha3-256-str ensure-openssl string? string-length)
  (define-digester sha3-384-string ffi-sha3-384-str ensure-openssl string? string-length)
  (define-digester sha3-512-string ffi-sha3-512-str ensure-openssl string? string-length)
  (define-digester blake2b-512-string ffi-blake2b-512-str ensure-openssl string? string-length)
  (define-digester blake2s-256-string ffi-blake2s-256-str ensure-openssl string? string-length)
  (define-digester blake3-string ffi-blake3-str ensure-blake3 string? string-length)


  (define-syntax define-file-digester
    (syntax-rules ()
      [(_ name which)
       (define-who name
         (case-lambda
           [(path)
            (pcheck ([file-regular? path]) (name path 0 (file-size path)))]
           [(path start)
            (pcheck ([file-regular? path]) (name path start (file-size path)))]
           [(path start stop)
            (pcheck ([file-regular? path] [natural? start stop])
                    (let ([len (file-size path)])
                      (when (fx> start stop)
                        (errorf who "start index ~a is greater than stop index ~a" start stop))
                      (when (fx> stop len)
                        (errorf who "stop index ~a is greater than string length ~a" stop len))
                      (let* ([digester (make-digester 'which)]
                             [port (open-file-input-port path)]
                             [bv (make-bytevector 4096 0)])
                        (set-port-position! port start)
                        (let loop ([remaining (fx- stop start)])
                          (if (fx= 0 remaining)
                              (begin (close-port port)
                                     (digester-finalize! digester))
                              (let ([x (get-bytevector-n! port bv 0
                                                          (if (fx>= remaining 4096) 4096 remaining))])
                                (digester-update-bytevector! digester bv 0 x)
                                (loop (fx- remaining x))))))))]))]))

  #|doc
  |#
  (define-file-digester md5-file md5)
  (define-file-digester sha224-file sha224)
  (define-file-digester sha256-file sha256)
  (define-file-digester sha384-file sha384)
  (define-file-digester sha512-file sha512)
  (define-file-digester sha512-224-file sha512-224)
  (define-file-digester sha512-256-file sha512-256)
  (define-file-digester sha3-224-file sha3-224)
  (define-file-digester sha3-256-file sha3-256)
  (define-file-digester sha3-384-file sha3-384)
  (define-file-digester sha3-512-file sha3-512)
  (define-file-digester blake2b-512-file blake2b-512)
  (define-file-digester blake2s-256-file blake2s-256)
  (define-file-digester blake3-file blake3)


;;;;===----------------------------------------------------------------------===
;;;;  incremental API
;;;;===----------------------------------------------------------------------===

  (define-record-type (digester mk-digester digester?)
    (opaque #t)
    (fields ffi-ctx
            ffi-get
            ffi-string-update!
            ffi-bytevector-update!
            ffi-finalize!
            ffi-reset!
            (mutable finalized?)))


  #|proc:ffi-blake3-create
  The `ffi-blake3-create` procedure calls the native blake3 operation `digester_blake3_create`.
  It returns the native result and raises an error when the dependency is unavailable.
  |#
  (define ffi-blake3-create
    (let ([native (foreign-procedure "digester_blake3_create" () void*)])
      (lambda ()
        (pcheck ()
                (require-optional-library 'ffi-blake3-create 'blake3)
                (native )))))
  (define ffi-blake3-get
    (foreign-procedure "digester_blake3_get" (void*) ptr))
  #|proc:ffi-blake3-string-update!
  The `ffi-blake3-string-update!` procedure calls the native blake3 operation
  `digester_blake3_update_string`.
  Parameters `context`, `text`, `start`, `stop` are passed to the native operation in that order.
  `context` is the native context handle.
  `text` is the input text.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-blake3-string-update!
    (let ([native (foreign-procedure "digester_blake3_update_string" (void* ptr int int) void)])
      (lambda (context text start stop)
        (pcheck ([natural? context] [integer? start] [integer? stop])
                (require-optional-library 'ffi-blake3-string-update! 'blake3)
                (native context text start stop)))))
  #|proc:ffi-blake3-bytevector-update!
  The `ffi-blake3-bytevector-update!` procedure calls the native blake3 operation
  `digester_blake3_update_bytevector`.
  Parameters `context`, `bytevector`, `start`, `stop` are passed to the native operation in that
  order.
  `context` is the native context handle.
  `bytevector` is the input bytevector.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-blake3-bytevector-update!
    (let ([native (foreign-procedure "digester_blake3_update_bytevector" (void* ptr int int) void)])
      (lambda (context bytevector start stop)
        (pcheck ([natural? context] [integer? start] [integer? stop])
                (require-optional-library 'ffi-blake3-bytevector-update! 'blake3)
                (native context bytevector start stop)))))
  (define ffi-blake3-finalize!
    (foreign-procedure "digester_blake3_finalize" (void*) ptr))
  #|proc:ffi-blake3-reset!
  The `ffi-blake3-reset!` procedure calls the native blake3 operation `digester_blake3_reset`.
  Parameters `context` are passed to the native operation in that order.
  `context` is the native context handle.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-blake3-reset!
    (let ([native (foreign-procedure "digester_blake3_reset" (void*) void)])
      (lambda (context)
        (pcheck ([natural? context])
                (require-optional-library 'ffi-blake3-reset! 'blake3)
                (native context)))))


  #|proc:ffi-openssl-create
  The `ffi-openssl-create` procedure calls the native openssl operation `digester_openssl_create`.
  Parameters `algorithm` are passed to the native operation in that order.
  `algorithm` is the algorithm identifier.
  It returns the native result and raises an error when the dependency is unavailable.
  |#
  (define ffi-openssl-create
    (let ([native (foreign-procedure "digester_openssl_create" (ptr) void*)])
      (lambda (algorithm)
        (pcheck ()
                (require-optional-library 'ffi-openssl-create 'openssl)
                (native algorithm)))))
  (define ffi-openssl-get
    (foreign-procedure "digester_openssl_get" (void*) ptr))
  #|proc:ffi-openssl-string-update!
  The `ffi-openssl-string-update!` procedure calls the native openssl operation
  `digester_openssl_update_string`.
  Parameters `context`, `text`, `start`, `stop` are passed to the native operation in that order.
  `context` is the native context handle.
  `text` is the input text.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-openssl-string-update!
    (let ([native (foreign-procedure "digester_openssl_update_string" (void* ptr int int) void)])
      (lambda (context text start stop)
        (pcheck ([natural? context] [integer? start] [integer? stop])
                (require-optional-library 'ffi-openssl-string-update! 'openssl)
                (native context text start stop)))))
  #|proc:ffi-openssl-bytevector-update!
  The `ffi-openssl-bytevector-update!` procedure calls the native openssl operation
  `digester_openssl_update_bytevector`.
  Parameters `context`, `bytevector`, `start`, `stop` are passed to the native operation in that
  order.
  `context` is the native context handle.
  `bytevector` is the input bytevector.
  `start` is the inclusive start index of the input slice.
  `stop` is the exclusive end index of the input slice.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-openssl-bytevector-update!
    (let ([native (foreign-procedure "digester_openssl_update_bytevector" (void* ptr int int) void)])
      (lambda (context bytevector start stop)
        (pcheck ([natural? context] [integer? start] [integer? stop])
                (require-optional-library 'ffi-openssl-bytevector-update! 'openssl)
                (native context bytevector start stop)))))
  (define ffi-openssl-finalize!
    (foreign-procedure "digester_openssl_finalize" (void*) ptr))
  #|proc:ffi-openssl-reset!
  The `ffi-openssl-reset!` procedure calls the native openssl operation `digester_openssl_reset`.
  Parameters `context` are passed to the native operation in that order.
  `context` is the native context handle.
  It returns unspecified values and raises an error when the dependency is unavailable.
  |#
  (define ffi-openssl-reset!
    (let ([native (foreign-procedure "digester_openssl_reset" (void*) void)])
      (lambda (context)
        (pcheck ([natural? context])
                (require-optional-library 'ffi-openssl-reset! 'openssl)
                (native context)))))


  (define check-finalized
    (lambda (who digester)
      (when (digester-finalized? digester)
        (errorf who "digester is already finalized"))))


  #|doc
  |#
  (define-who make-digester
    (case-lambda
      [() (make-digester 'sha256)]
      [(which)
       (check-digester who which)
       (case which
         [blake3 (ensure-blake3 who)
                 (mk-digester (ffi-blake3-create)
                              ffi-blake3-get
                              ffi-blake3-string-update!
                              ffi-blake3-bytevector-update!
                              ffi-blake3-finalize!
                              ffi-blake3-reset!
                              #f)]
         [(md5
           sha224 sha256 sha384 sha512
           sha512-224 sha512-256
           sha3-224 sha3-256 sha3-384 sha3-512
           blake2b-512 blake2s-256)
          (ensure-openssl who)
          (mk-digester (ffi-openssl-create which)
                       ffi-openssl-get
                       ffi-openssl-string-update!
                       ffi-openssl-bytevector-update!
                       ffi-openssl-finalize!
                       ffi-openssl-reset!
                       #f)]
         [else (assert-unreachable)])]))


  #|doc
  This is based on the UTF-32 representation of the string.
  |#
  (define-who digester-update-string!
    (case-lambda
      [(digester x)
       (pcheck ([digester? digester] [string? x])
               (digester-update-string! digester x 0 (string-length x)))]
      [(digester x start)
       (pcheck ([digester? digester] [string? x])
               (digester-update-string! digester x start (string-length x)))]
      [(digester x start stop)
       (check-finalized who digester)
       (pcheck ([digester? digester] [string? x] [natural? start stop])
               (let ([len (string-length x)])
                 (when (fx> start stop)
                   (errorf who "start index ~a is greater than stop index ~a" start stop))
                 (when (fx> stop len)
                   (errorf who "stop index ~a is greater than string length ~a" stop len))
                 ((digester-ffi-string-update! digester) (digester-ffi-ctx digester) x start stop)))]))


  #|doc
  |#
  (define-who digester-update-bytevector!
    (case-lambda
      [(digester x)
       (pcheck ([digester? digester] [bytevector? x])
               (digester-update-bytevector! digester x 0 (bytevector-length x)))]
      [(digester x start)
       (pcheck ([digester? digester] [bytevector? x])
               (digester-update-bytevector! digester x start (bytevector-length x)))]
      [(digester x start stop)
       (check-finalized who digester)
       (pcheck ([digester? digester] [bytevector? x] [natural? start stop])
               (let ([len (bytevector-length x)])
                 (when (fx> start stop)
                   (errorf who "start index ~a is greater than stop index ~a" start stop))
                 (when (fx> stop len)
                   (errorf who "stop index ~a is greater than bytevector length ~a" stop len))
                 ((digester-ffi-bytevector-update! digester) (digester-ffi-ctx digester) x start stop)))]))


  #|doc
  |#
  (define-who digester-get
    (lambda (digester)
      (pcheck ([digester? digester])
              (check-finalized who digester)
              ((digester-ffi-get digester) (digester-ffi-ctx digester)))))


  #|doc
  |#
  (define-who digester-reset!
    (lambda (digester)
      (pcheck ([digester? digester])
              (check-finalized who digester)
              ((digester-ffi-reset! digester) (digester-ffi-ctx digester)))))


  #|doc
  |#
  (define-who digester-finalize!
    (lambda (digester)
      (pcheck ([digester? digester])
              (check-finalized who digester)
              (let ([res ((digester-ffi-finalize! digester) (digester-ffi-ctx digester))])
                (digester-finalized?-set! digester #t)
                res))))


  #|doc
  |#
  (define-who call-with-digester
    (case-lambda
      [(proc) (call-with-digester 'sha256 proc)]
      [(which proc)
       (pcheck ([procedure? proc])
               (check-digester who which)
               (let ([digester #f])
                 (dynamic-wind (lambda () (set! digester (make-digester which)))
                               (lambda ()
                                 (proc digester)
                                 (digester-get digester))
                               (lambda ()
                                 (digester-finalize! digester)
                                 (set! digester #f)))))]))


  )
