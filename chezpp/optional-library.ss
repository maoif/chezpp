(library (chezpp optional-library)
  (export optional-library-info?
          optional-library-info
          optional-library-name
          optional-library-available?
          optional-library-version
          optional-library-capabilities
          optional-library-error)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp net ffi))

  #|record:optional-library-info
The `optional-library-info` record describes one supported native optional dependency.
|#
  (define-record-type (optional-library-info-record %make-optional-library-info
                                             optional-library-info?)
    (opaque #t)
    (fields (immutable name optional-library-name)
            (immutable available? optional-library-available?)
            (immutable version optional-library-version)
            (immutable capabilities optional-library-capabilities)
            (immutable error optional-library-error)))

  (define supported-optional-libraries
    '(openssl xxhash blake3 curl ssh websockets grpc zlib nghttp2 cares))

  #|proc:optional-library-info
The `optional-library-info` procedure probes the supported native library named by `name`.
It returns an `optional-library-info` record with availability, version, capabilities, and error.
|#
  (define-who optional-library-info
    (lambda (name)
      (pcheck ([symbol? name])
              (unless (memq name supported-optional-libraries)
                (errorf who "supported optional libraries are ~s"
                        supported-optional-libraries))
              (let ([value (ffi-optional-library-info (symbol->string name))])
                (unless (and (vector? value) (= (vector-length value) 5))
                  (errorf who "optional library probe returned an invalid result"))
                (%make-optional-library-info
                 (vector-ref value 0)
                 (vector-ref value 1)
                 (vector-ref value 2)
                 (vector-ref value 3)
                 (vector-ref value 4))))))
  )
