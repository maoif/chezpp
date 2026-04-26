(library (chezpp hash)
  (export xxhash32-fixnum
          xxhash32-flonum
          xxhash32-ratnum
          xxhash32-cflonum
          xxhash32-bool
          xxhash32-char
          xxhash32-string
          xxhash32-fxvector
          xxhash32-flvector
          xxhash32-bytevector
          xxhash32-file

          xxhash64-fixnum
          xxhash64-flonum
          xxhash64-ratnum
          xxhash64-cflonum
          xxhash64-bool
          xxhash64-char
          xxhash64-string
          xxhash64-fxvector
          xxhash64-flvector
          xxhash64-bytevector
          xxhash64-file

          xxhash3-64-fixnum
          xxhash3-64-flonum
          xxhash3-64-ratnum
          xxhash3-64-cflonum
          xxhash3-64-bool
          xxhash3-64-char
          xxhash3-64-string
          xxhash3-64-fxvector
          xxhash3-64-flvector
          xxhash3-64-bytevector
          xxhash3-64-file

          make-hasher hasher? hasher-get hasher-finalize! hasher-reset!
          hasher-update-fixnum!
          hasher-update-flonum!
          hasher-update-ratnum!
          hasher-update-cflonum!
          hasher-update-bool!
          hasher-update-char!
          hasher-update-string!
          hasher-update-fxvector!
          hasher-update-bytevector!
          hasher-update-flvector!

          call-with-hasher)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp internal)
          (chezpp io)
          (chezpp file))

  ;; TOOD move to vector.ss
  #|doc
  |#
  (define bytevector->hex
    (lambda (bv)
      (pcheck ([bytevector? bv])
              (let ([str (make-string (fx* 2 (bytevector-length bv)) #\space)])
                (let loop ([i 0] [j 0])
                  (if (fx= i (bytevector-length bv))
                      str
                      (let* ([x (bytevector-u8-ref bv i)]
                             [a (number->string (fxlogand x #xf) 16)]
                             [b (number->string (fxsrl x 4) 16)])
                        (assert (and (= 1 (string-length a)) (= 1 (string-length b))))
                        (string-set! str j (char-downcase (string-ref b 0)))
                        (string-set! str (fx1+ j) (char-downcase (string-ref a 0)))
                        (loop (fx1+ i) (fx+ j 2)))))))))


  (define *hashers* '(xxhash32 xxhash64 xxhash3-64))
  (define check-hasher
    (lambda (who which)
      (unless (memq which *hashers*)
        (errorf who "valid hash algorithm is one of ~a" *hashers*))))


;;;;===----------------------------------------------------------------------===
;;;;  specialized interface
;;;;===----------------------------------------------------------------------===


  (define ffi-xxh32   (foreign-procedure "hash_XXH32"   (ptr unsigned-int)  unsigned-int))
  (define ffi-xxh64   (foreign-procedure "hash_XXH64"   (ptr unsigned-long) ptr))
  (define ffi-xxh3-64 (foreign-procedure "hash_XXH3_64" (ptr unsigned-long) ptr))

  (define ffi-xxh32-fixnum   (foreign-procedure "hash_XXH32_fixnum" (fixnum unsigned-32) unsigned-32))
  (define ffi-xxh32-flonum   (foreign-procedure "hash_XXH32_flonum" (double unsigned-32) unsigned-32))
  (define ffi-xxh32-ratnum   (foreign-procedure "hash_XXH32_ratnum" (fixnum fixnum unsigned-32) unsigned-32))
  (define ffi-xxh32-cflonum  (foreign-procedure "hash_XXH32_cflonum" (double double unsigned-32) unsigned-32))
  (define ffi-xxh32-string   (foreign-procedure "hash_XXH32_string" (ptr int int unsigned-32) unsigned-32))
  (define ffi-xxh32-fxvector (foreign-procedure "hash_XXH32_fxvector" (ptr int int unsigned-32) unsigned-32))
  (define ffi-xxh32-flvector (foreign-procedure "hash_XXH32_flvector" (ptr int int unsigned-32) unsigned-32))
  (define ffi-xxh32-bytevector (foreign-procedure "hash_XXH32_bytevector" (ptr int int unsigned-32) unsigned-32))

  (define ffi-xxh64-fixnum   (foreign-procedure "hash_XXH64_fixnum" (fixnum unsigned-64) ptr))
  (define ffi-xxh64-flonum   (foreign-procedure "hash_XXH64_flonum" (double unsigned-64) ptr))
  (define ffi-xxh64-ratnum   (foreign-procedure "hash_XXH64_ratnum" (fixnum fixnum unsigned-64) ptr))
  (define ffi-xxh64-cflonum  (foreign-procedure "hash_XXH64_cflonum" (double double unsigned-64) ptr))
  (define ffi-xxh64-string   (foreign-procedure "hash_XXH64_string" (ptr int int unsigned-64) ptr))
  (define ffi-xxh64-fxvector (foreign-procedure "hash_XXH64_fxvector" (ptr int int unsigned-64) ptr))
  (define ffi-xxh64-flvector (foreign-procedure "hash_XXH64_flvector" (ptr int int unsigned-64) ptr))
  (define ffi-xxh64-bytevector (foreign-procedure "hash_XXH64_bytevector" (ptr int int unsigned-64) ptr))

  (define ffi-xxh3-64-fixnum     (foreign-procedure "hash_XXH3_64_fixnum" (fixnum unsigned-64) ptr))
  (define ffi-xxh3-64-flonum     (foreign-procedure "hash_XXH3_64_flonum" (double unsigned-64) ptr))
  (define ffi-xxh3-64-ratnum     (foreign-procedure "hash_XXH3_64_ratnum" (fixnum fixnum unsigned-64) ptr))
  (define ffi-xxh3-64-cflonum    (foreign-procedure "hash_XXH3_64_cflonum" (double double unsigned-64) ptr))
  (define ffi-xxh3-64-string     (foreign-procedure "hash_XXH3_64_string" (ptr int int unsigned-64) ptr))
  (define ffi-xxh3-64-fxvector   (foreign-procedure "hash_XXH3_64_fxvector" (ptr int int unsigned-64) ptr))
  (define ffi-xxh3-64-flvector   (foreign-procedure "hash_XXH3_64_flvector" (ptr int int unsigned-64) ptr))
  (define ffi-xxh3-64-bytevector (foreign-procedure "hash_XXH3_64_bytevector" (ptr int int unsigned-64) ptr))


  (define-syntax define-hasher-scalar
    (syntax-rules ()
      [(_ name ffi x? cvt tag)
       (define-who name
         (case-lambda
           [(x) (name x 0)]
           [(x salt)
            (pcheck ([x? x] [fixnum? salt])
                    ;; salt + tag safe?
                    (ffi (cvt x) (fx+ salt tag)))]))]))

  (define-hasher-scalar xxhash32-fixnum   ffi-xxh32-fixnum fixnum? id 0)
  (define-hasher-scalar xxhash64-fixnum   ffi-xxh64-fixnum fixnum? id 0)
  (define-hasher-scalar xxhash3-64-fixnum ffi-xxh3-64-fixnum fixnum? id 0)

  (define-hasher-scalar xxhash32-bool   ffi-xxh32-fixnum boolean? (lambda (x) (if x 1 0)) 9)
  (define-hasher-scalar xxhash64-bool   ffi-xxh64-fixnum boolean? (lambda (x) (if x 1 0)) 9)
  (define-hasher-scalar xxhash3-64-bool ffi-xxh3-64-fixnum boolean? (lambda (x) (if x 1 0)) 9)

  (define-hasher-scalar xxhash32-char   ffi-xxh32-fixnum char? char->integer 11)
  (define-hasher-scalar xxhash64-char   ffi-xxh64-fixnum char? char->integer 11)
  (define-hasher-scalar xxhash3-64-char ffi-xxh3-64-fixnum char? char->integer 11)

  (define-hasher-scalar xxhash32-flonum   ffi-xxh32-flonum flonum? id 1)
  (define-hasher-scalar xxhash64-flonum   ffi-xxh64-flonum flonum? id 1)
  (define-hasher-scalar xxhash3-64-flonum ffi-xxh3-64-flonum flonum? id 1)


  (define-syntax define-hasher-rat/cfl
    (syntax-rules ()
      [(_ name ffi x? get-p1 get-p2 tag)
       (define-who name
         (case-lambda
           [(x) (name x 0)]
           [(x salt)
            (pcheck ([x? x] [fixnum? salt])
                    (let ([p1 (get-p1 x)] [p2 (get-p2 x)])
                      (ffi p1 p2 (fx+ salt tag))))]))]))

  (define-hasher-rat/cfl xxhash32-ratnum   ffi-xxh32-ratnum ratnum? numerator denominator 2)
  (define-hasher-rat/cfl xxhash64-ratnum   ffi-xxh64-ratnum ratnum? numerator denominator 2)
  (define-hasher-rat/cfl xxhash3-64-ratnum ffi-xxh3-64-ratnum ratnum? numerator denominator 2)

  (define-hasher-rat/cfl xxhash32-cflonum   ffi-xxh32-cflonum cflonum? real-part imag-part 3)
  (define-hasher-rat/cfl xxhash64-cflonum   ffi-xxh64-cflonum cflonum? real-part imag-part 3)
  (define-hasher-rat/cfl xxhash3-64-cflonum ffi-xxh3-64-cflonum cflonum? real-part imag-part 3)


  (define-syntax define-hasher-indexable
    (syntax-rules ()
      [(_ name ffi x? x-length tag)
       (define-who name
         (case-lambda
           [(x)
            (pcheck ([x? x]) (name x 0 0 (x-length x)))]
           [(x salt)
            (pcheck ([x? x]) (name x salt 0 (x-length x)))]
           [(x salt start)
            (pcheck ([x? x]) (name x salt start (x-length x)))]
           [(x salt start stop)
            (pcheck ([x? x] [natural? start stop] [fixnum? salt])
                    (let ([len (x-length x)])
                      (when (fx> start stop)
                        (errorf who "start index ~a is greater than stop index ~a" start stop))
                      (when (fx> stop len)
                        (errorf who "stop index ~a is greater than total length ~a" stop len))
                      (ffi x start stop (fx+ salt tag))))]))]))

  (define-hasher-indexable xxhash32-string   ffi-xxh32-string string? string-length 4)
  (define-hasher-indexable xxhash64-string   ffi-xxh64-string string? string-length 4)
  (define-hasher-indexable xxhash3-64-string ffi-xxh3-64-string string? string-length 4)

  (define-hasher-indexable xxhash32-fxvector   ffi-xxh32-fxvector fxvector? fxvector-length 5)
  (define-hasher-indexable xxhash64-fxvector   ffi-xxh64-fxvector fxvector? fxvector-length 5)
  (define-hasher-indexable xxhash3-64-fxvector ffi-xxh3-64-fxvector fxvector? fxvector-length 5)

  (define-hasher-indexable xxhash32-flvector   ffi-xxh32-flvector flvector? flvector-length 6)
  (define-hasher-indexable xxhash64-flvector   ffi-xxh64-flvector flvector? flvector-length 6)
  (define-hasher-indexable xxhash3-64-flvector ffi-xxh3-64-flvector flvector? flvector-length 6)

  (define-hasher-indexable xxhash32-bytevector   ffi-xxh32-bytevector bytevector? bytevector-length 7)
  (define-hasher-indexable xxhash64-bytevector   ffi-xxh64-bytevector bytevector? bytevector-length 7)
  (define-hasher-indexable xxhash3-64-bytevector ffi-xxh3-64-bytevector bytevector? bytevector-length 7)


  (define-syntax define-file-hasher
    (syntax-rules ()
      [(_ name which)
       (define-who name
         (case-lambda
           [(path)
            (pcheck ([file-regular? path]) (name path 0 0 (file-size path)))]
           [(path salt)
            (pcheck ([file-regular? path]) (name path salt 0 (file-size path)))]
           [(path salt start)
            (pcheck ([file-regular? path]) (name path salt start (file-size path)))]
           [(path salt start stop)
            (pcheck ([file-regular? path] [natural? start stop] [fixnum? salt])
                    (let ([len (file-size path)])
                      (when (fx> start stop)
                        (errorf who "start index ~a is greater than stop index ~a" start stop))
                      (when (fx> stop len)
                        (errorf who "stop index ~a is greater than file length ~a" stop len))
                      (let ([port (open-file-input-port path)]
                            [bv (make-bytevector 4096 0)]
                            [hashsher (make-hasher 'which salt)])
                        (set-port-position! port start)
                        (let loop ([remaining (fx- stop start)])
                          (if (fx= 0 remaining)
                              (begin (close-port port)
                                     (hasher-finalize! hashsher))
                              (let ([x (get-bytevector-n! port bv 0
                                                          (if (fx>= remaining 4096) 4096 remaining))])
                                (hasher-update-bytevector! hashsher bv 0 x)
                                (loop (fx- remaining x))))))))]))]))


  #|doc
  |#
  (define-file-hasher xxhash32-file xxhash32)
  (define-file-hasher xxhash64-file xxhash64)
  (define-file-hasher xxhash3-64-file xxhash3-64)


;;;;===----------------------------------------------------------------------===
;;;;  incremental API
;;;;===----------------------------------------------------------------------===

  (define-record-type (hasher mk-hasher hasher?)
    (opaque #t)
    (fields ffi-ctx
            ffi-get
            ffi-finalize!
            ffi-reset!

            ffi-fixnum-update!
            ffi-flonum-update!
            ffi-ratnum-update!
            ffi-cflonum-update!
            ffi-string-update!
            ffi-fxvector-update!
            ffi-flvector-update!
            ffi-bytevector-update!

            (mutable finalized?)))


  (define ffi-xxh32-create    (foreign-procedure "hasher_XXH32_create" (unsigned-32) void*))
  (define ffi-xxh32-get       (foreign-procedure "hasher_XXH32_get" (void*) unsigned-int))
  (define ffi-xxh32-finalize! (foreign-procedure "hasher_XXH32_finalize" (void*) unsigned-int))
  (define ffi-xxh32-reset!    (foreign-procedure "hasher_XXH32_reset" (void* unsigned-32) void))
  (define ffi-xxh32-fixnum-update!   (foreign-procedure "hasher_XXH32_update_fixnum" (void* fixnum int) void))
  (define ffi-xxh32-flonum-update!   (foreign-procedure "hasher_XXH32_update_flonum" (void* double int) void))
  (define ffi-xxh32-ratnum-update!   (foreign-procedure "hasher_XXH32_update_ratnum" (void* fixnum fixnum int) void))
  (define ffi-xxh32-cflonum-update!  (foreign-procedure "hasher_XXH32_update_cflonum" (void* double double int) void))
  (define ffi-xxh32-string-update!   (foreign-procedure "hasher_XXH32_update_string" (void* ptr int int int) void))
  (define ffi-xxh32-fxvector-update! (foreign-procedure "hasher_XXH32_update_fxvector" (void* ptr int int int) void))
  (define ffi-xxh32-flvector-update! (foreign-procedure "hasher_XXH32_update_flvector" (void* ptr int int int) void))
  (define ffi-xxh32-bytevector-update! (foreign-procedure "hasher_XXH32_update_bytevector" (void* ptr int int int) void))

  (define ffi-xxh64-create    (foreign-procedure "hasher_XXH64_create" (unsigned-64) void*))
  (define ffi-xxh64-get       (foreign-procedure "hasher_XXH64_get" (void*) ptr))
  (define ffi-xxh64-finalize! (foreign-procedure "hasher_XXH64_finalize" (void*) ptr))
  (define ffi-xxh64-reset!    (foreign-procedure "hasher_XXH64_reset" (void* unsigned-64) void))
  (define ffi-xxh64-fixnum-update!   (foreign-procedure "hasher_XXH64_update_fixnum" (void* fixnum int) void))
  (define ffi-xxh64-flonum-update!   (foreign-procedure "hasher_XXH64_update_flonum" (void* double int) void))
  (define ffi-xxh64-ratnum-update!   (foreign-procedure "hasher_XXH64_update_ratnum" (void* fixnum fixnum int) void))
  (define ffi-xxh64-cflonum-update!  (foreign-procedure "hasher_XXH64_update_cflonum" (void* double double int) void))
  (define ffi-xxh64-string-update!   (foreign-procedure "hasher_XXH64_update_string" (void* ptr int int int) void))
  (define ffi-xxh64-fxvector-update! (foreign-procedure "hasher_XXH64_update_fxvector" (void* ptr int int int) void))
  (define ffi-xxh64-flvector-update! (foreign-procedure "hasher_XXH64_update_flvector" (void* ptr int int int) void))
  (define ffi-xxh64-bytevector-update! (foreign-procedure "hasher_XXH64_update_bytevector" (void* ptr int int int) void))

  (define ffi-xxh3-64-create    (foreign-procedure "hasher_XXH3_64_create" (unsigned-64) void*))
  (define ffi-xxh3-64-get       (foreign-procedure "hasher_XXH3_64_get" (void*) ptr))
  (define ffi-xxh3-64-finalize! (foreign-procedure "hasher_XXH3_64_finalize" (void*) ptr))
  (define ffi-xxh3-64-reset!    (foreign-procedure "hasher_XXH3_64_reset" (void* unsigned-64) void))
  (define ffi-xxh3-64-fixnum-update!   (foreign-procedure "hasher_XXH3_64_update_fixnum" (void* fixnum int) void))
  (define ffi-xxh3-64-flonum-update!   (foreign-procedure "hasher_XXH3_64_update_flonum" (void* double int) void))
  (define ffi-xxh3-64-ratnum-update!   (foreign-procedure "hasher_XXH3_64_update_ratnum" (void* fixnum fixnum int) void))
  (define ffi-xxh3-64-cflonum-update!  (foreign-procedure "hasher_XXH3_64_update_cflonum" (void* double double int) void))
  (define ffi-xxh3-64-string-update!   (foreign-procedure "hasher_XXH3_64_update_string" (void* ptr int int int) void))
  (define ffi-xxh3-64-fxvector-update! (foreign-procedure "hasher_XXH3_64_update_fxvector" (void* ptr int int int) void))
  (define ffi-xxh3-64-flvector-update! (foreign-procedure "hasher_XXH3_64_update_flvector" (void* ptr int int int) void))
  (define ffi-xxh3-64-bytevector-update! (foreign-procedure "hasher_XXH3_64_update_bytevector" (void* ptr int int int) void))


  (define check-finalized
    (lambda (who hasher)
      (when (hasher-finalized? hasher)
        (errorf who "hasher is already finalized"))))


  #|doc
  |#
  (define-who make-hasher
    (case-lambda
      [() (make-hasher 'xxhash32 0)]
      [(which) (make-hasher which 0)]
      [(which salt)
       (pcheck ([fixnum? salt])
               (check-hasher who which)
               (case which
                 [xxhash32 (mk-hasher (ffi-xxh32-create salt)
                                      ffi-xxh32-get
                                      ffi-xxh32-finalize!
                                      ffi-xxh32-reset!

                                      ffi-xxh32-fixnum-update!
                                      ffi-xxh32-flonum-update!
                                      ffi-xxh32-ratnum-update!
                                      ffi-xxh32-cflonum-update!
                                      ffi-xxh32-string-update!
                                      ffi-xxh32-fxvector-update!
                                      ffi-xxh32-flvector-update!
                                      ffi-xxh32-bytevector-update!

                                      #f)]
                 [xxhash64 (mk-hasher (ffi-xxh64-create salt)
                                      ffi-xxh64-get
                                      ffi-xxh64-finalize!
                                      ffi-xxh64-reset!

                                      ffi-xxh64-fixnum-update!
                                      ffi-xxh64-flonum-update!
                                      ffi-xxh64-ratnum-update!
                                      ffi-xxh64-cflonum-update!
                                      ffi-xxh64-string-update!
                                      ffi-xxh64-fxvector-update!
                                      ffi-xxh64-flvector-update!
                                      ffi-xxh64-bytevector-update!

                                      #f)]
                 [xxhash3-64 (mk-hasher (ffi-xxh3-64-create salt)
                                        ffi-xxh3-64-get
                                        ffi-xxh3-64-finalize!
                                        ffi-xxh3-64-reset!

                                        ffi-xxh3-64-fixnum-update!
                                        ffi-xxh3-64-flonum-update!
                                        ffi-xxh3-64-ratnum-update!
                                        ffi-xxh3-64-cflonum-update!
                                        ffi-xxh3-64-string-update!
                                        ffi-xxh3-64-fxvector-update!
                                        ffi-xxh3-64-flvector-update!
                                        ffi-xxh3-64-bytevector-update!

                                        #f)]
                 [else (assert-unreachable)]))]))


  #|doc
  |#
  (define-who hasher-get
    (lambda (hasher)
      (pcheck ([hasher? hasher])
              (check-finalized who hasher)
              ((hasher-ffi-get hasher) (hasher-ffi-ctx hasher)))))


  (define-syntax define-hasher-update-scalar
    (syntax-rules ()
      [(_ name x? get-ffi cvt tag)
       (define-who name
         (lambda (hasher x)
           (pcheck ([hasher? hasher] [x? x])
                   (check-finalized who hasher)
                   ((get-ffi hasher) (hasher-ffi-ctx hasher) (cvt x) tag))))]))

  (define-hasher-update-scalar hasher-update-fixnum! fixnum?  hasher-ffi-fixnum-update! id 0)
  (define-hasher-update-scalar hasher-update-bool!   boolean? hasher-ffi-fixnum-update! (lambda (x) (if x 1 0)) 1)
  (define-hasher-update-scalar hasher-update-char!   char?    hasher-ffi-fixnum-update! char->integer 2)
  (define-hasher-update-scalar hasher-update-flonum! flonum?  hasher-ffi-flonum-update! id 3)


  (define-syntax define-hasher-update-rat/cfl
    (syntax-rules ()
      [(_ name x? get-ffi get-x get-y tag)
       (define-who name
         (lambda (hasher x)
           (pcheck ([hasher? hasher] [x? x])
                   (check-finalized who hasher)
                   ((get-ffi hasher)
                    (hasher-ffi-ctx hasher) (get-x x) (get-y x) tag))))]))

  (define-hasher-update-rat/cfl hasher-update-ratnum!  ratnum?  hasher-ffi-ratnum-update!  numerator denominator 4)
  (define-hasher-update-rat/cfl hasher-update-cflonum! cflonum? hasher-ffi-cflonum-update! real-part imag-part 5)


  (define-syntax define-hasher-update-indexable
    (syntax-rules ()
      [(_ name x? get-ffi x-length tag)
       (define-who name
         (case-lambda
           [(hasher x)
            (pcheck ([hasher? hasher] [x? x])
                    (name hasher x 0 (x-length x)))]
           [(hasher x start)
            (pcheck ([hasher? hasher] [x? x])
                    (name hasher x start (x-length x)))]
           [(hasher x start stop)
            (check-finalized who hasher)
            (pcheck ([hasher? hasher] [x? x] [natural? start stop])
                    (let ([len (x-length x)])
                      (when (fx> start stop)
                        (errorf who "start index ~a is greater than stop index ~a" start stop))
                      (when (fx> stop len)
                        (errorf who "stop index ~a is greater than total length ~a" stop len))
                      ((get-ffi hasher) (hasher-ffi-ctx hasher) x start stop tag)))]))]))

  (define-hasher-update-indexable hasher-update-string!   string?   hasher-ffi-string-update!   string-length 4)
  (define-hasher-update-indexable hasher-update-fxvector! fxvector? hasher-ffi-fxvector-update! fxvector-length 5)
  (define-hasher-update-indexable hasher-update-flvector! flvector? hasher-ffi-flvector-update! flvector-length 6)
  (define-hasher-update-indexable hasher-update-bytevector! bytevector? hasher-ffi-bytevector-update! bytevector-length 7)


  #|doc
  |#
  (define-who hasher-finalize!
    (lambda (hasher)
      (pcheck ([hasher? hasher])
              (check-finalized who hasher)
              (let ([res ((hasher-ffi-finalize! hasher) (hasher-ffi-ctx hasher))])
                (hasher-finalized?-set! hasher #t)
                res))))


  #|doc
  |#
  (define-who hasher-reset!
    (case-lambda
      [(hasher) (hasher-reset! hasher 0)]
      [(hasher salt)
       (pcheck ([hasher? hasher] [fixnum? salt])
               (check-finalized who hasher)
               ((hasher-ffi-reset! hasher) (hasher-ffi-ctx hasher) salt))]))


  #|doc
  |#
  (define-who call-with-hasher
    (case-lambda
      [(proc) (call-with-hasher 'xxhash32 0 proc)]
      [(which proc) (call-with-hasher which 0 proc)]
      [(which salt proc)
       (pcheck ([fixnum? salt] [procedure? proc])
               (check-hasher who which)
               (let ([hasher #f])
                 (dynamic-wind (lambda () (set! hasher (make-hasher which salt)))
                               (lambda ()
                                 (proc hasher)
                                 (hasher-get hasher))
                               (lambda ()
                                 (hasher-finalize! hasher)
                                 (set! hasher #f)))))]))


  )
