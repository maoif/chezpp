(library (chezpp uuid)
  (export make-uuid make-uuid-from-time make-uuid-from-md5 make-uuid-from-sha1
          uuid?
          uuid->string uuid->string-upcase uuid->string-downcase
          string->uuid
          uuid->bytevector bytevector->uuid
          uuid-time
          uuid=? uuid<? uuid<=? uuid>? uuid>=?)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp internal))


  (define-record-type (uuid mk-uuid uuid?)
    (opaque #f)
    (sealed #t)
    (fields data))

  (define ffi-generate-uuid
    (foreign-procedure "chezpp_generate_uuid" () ptr))
  (define ffi-generate-uuid-time
    (foreign-procedure "chezpp_generate_uuid_time" () ptr))
  (define ffi-generate-uuid-md5
    (foreign-procedure "chezpp_generate_uuid_md5" (ptr string) ptr))
  (define ffi-generate-uuid-sha1
    (foreign-procedure "chezpp_generate_uuid_sha1" (ptr string) ptr))
  (define ffi-uuid-to-string
    (foreign-procedure "chezpp_uuid_to_string" (ptr) ptr))
  (define ffi-uuid-to-string-upcase
    (foreign-procedure "chezpp_uuid_to_string_upcase" (ptr) ptr))
  (define ffi-uuid-to-string-downcase
    (foreign-procedure "chezpp_uuid_to_string_downcase" (ptr) ptr))
  (define ffi-uuid-compare
    (foreign-procedure "chezpp_uuid_compare" (ptr ptr) int))
  (define ffi-uuid-time
    (foreign-procedure "chezpp_uuid_time" (ptr) ptr))
  (define ffi-string-to-uuid
    (foreign-procedure "chezpp_string_to_uuid" (string) ptr))


  #|doc
  Generate a new random universally unique identifier (UUID).
  |#
  (define make-uuid
    (case-lambda
      [()
       (mk-uuid (ffi-generate-uuid))]))


  #|doc
  Generate a new universally unique identifier (UUID) the current time and the
  local ethernet MAC address (if available).
  This procedure returns two values: the first is a boolean that indicates
  whether the UUID has been generated in a safe manner, the second is the generated
  UUID. For more info, see `man uuid_generate`.
  |#
  (define make-uuid-from-time
    (case-lambda
      [()
       (let ([vec (ffi-generate-uuid-time)])
         (values (vector-ref vec 0) (mk-uuid (vector-ref vec 1))))]))


  #|doc
  Generate a MD5-based UUID from the given `uuid` namespace and an arbirary string.
  |#
  (define make-uuid-from-md5
    (case-lambda
      [(uuid str)
       (let ([bv (ffi-generate-uuid-md5 (uuid-data uuid) str)])
         (mk-uuid bv))]))


  #|doc
  Generate a SHA1-based UUID from the given `uuid` namespace and an arbirary string.
  |#
  (define make-uuid-from-sha1
    (case-lambda
      [(uuid str)
       (let ([bv (ffi-generate-uuid-sha1 (uuid-data uuid) str)])
         (mk-uuid bv))]))


  #|doc
  Convert the given UUID into a 36-char hexidecimal string representation,
  e.g., "1b4e28ba-2fa1-11d2-883f-0016d3cca427",
  |#
  (define uuid->string
    (lambda (uuid)
      (pcheck ([uuid? uuid])
              (ffi-uuid-to-string (uuid-data uuid)))))


  #|doc
  Convert the given UUID into a 36-char hexidecimal uppercase string representation,
  e.g., "1B4E28BA-2FA1-11D2-883F-0016D3CCA427".
  |#
  (define uuid->string-upcase
    (lambda (uuid)
      (pcheck ([uuid? uuid])
              (ffi-uuid-to-string-upcase (uuid-data uuid)))))


  #|doc
  Convert the given UUID into a 36-char hexidecimal lowercase string representation,
  e.g., "1b4e28ba-2fa1-11d2-883f-0016d3cca427",
  |#
  (define uuid->string-downcase
    (lambda (uuid)
      (pcheck ([uuid? uuid])
              (ffi-uuid-to-string-downcase (uuid-data uuid)))))


  #|doc
  Convert a string to a UUID object.
  Upon success, the UUID object is returned; otherwise #f is returned.
  |#
  (define string->uuid
    (lambda (str)
      (pcheck ([string? str])
              (let ([res (ffi-string-to-uuid str)])
                (and res (mk-uuid res))))))


  #|doc
  Convert the UUID to its 16-byte bytevector representation.
  |#
  (define uuid->bytevector
    (lambda (uuid)
      (pcheck ([uuid? uuid])
              (bytevector-copy (uuid-data uuid)))))


  #|proc:bytevector->uuid
  Convert a 16-byte bytevector to a UUID.
  |#
  (define bytevector->uuid
    (lambda (bv)
      (pcheck ([bytevector? bv])
              (if (fx= 16 (bytevector-length bv))
                  (mk-uuid (bytevector-copy bv))
                  (errorf 'bytevector->uuid "expected 16-byte bytevector, got length ~a"
                          (bytevector-length bv))))))


  #|doc
  Make a copy of the given UUID object.
  |#
  (define uuid-copy
    (lambda (uuid)
      (mk-uuid (bytevector-copy (uuid-data uuid)))))


  #|proc:uuid-time
  Return the creation time of the given time-based UUID as a UTC time object.
  |#
  (define uuid-time
    (lambda (uuid)
      (pcheck ([uuid? uuid])
              (let ([bv (ffi-uuid-time (uuid-data uuid))])
                (make-time 'time-utc
                           (fx* (bytevector-s64-native-ref bv 8) 1000)
                           (bytevector-s64-native-ref bv 0))))))


  #|doc
  |#
  (define uuid=?
    (lambda (x y)
      (pcheck ([uuid? x y])
              (fx= 0 (ffi-uuid-compare (uuid-data x) (uuid-data y))))))


  #|doc
  |#
  (define uuid<?
    (lambda (x y)
      (pcheck ([uuid? x y])
              (fx< (ffi-uuid-compare (uuid-data x) (uuid-data y)) 0))))


  #|doc
  |#
  (define uuid<=?
    (lambda (x y)
      (pcheck ([uuid? x y])
              (fx<= (ffi-uuid-compare (uuid-data x) (uuid-data y)) 0))))


  #|doc
  |#
  (define uuid>?
    (lambda (x y)
      (pcheck ([uuid? x y])
              (fx> (ffi-uuid-compare (uuid-data x) (uuid-data y)) 0))))


  #|doc
  |#
  (define uuid>=?
    (lambda (x y)
      (pcheck ([uuid? x y])
              (fx>= (ffi-uuid-compare (uuid-data x) (uuid-data y)) 0))))


  (record-writer (type-descriptor uuid)
                 (lambda (r p wr)
                   (display "#[uuid " p)
                   (display (ffi-uuid-to-string (uuid-data r)) p)
                   (display "]" p)))

  (record-type-equal-procedure (type-descriptor uuid)
                               (lambda (x1 x2 =?)
                                 (uuid=? x1 x2)))
  )
