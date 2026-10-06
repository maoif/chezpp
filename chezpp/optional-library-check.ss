(library (chezpp optional-library-check)
  (export require-optional-library)
  (import (chezpp chez) (chezpp utils))

  (define native-library-info
    (foreign-procedure "chezpp_optional_library_info" (string) scheme-object))

  #|proc:require-optional-library
  The `require-optional-library` procedure checks the native dependency named by `library`.
  `who` is the caller's symbol used in error reports, and `library` is a dependency symbol.
  It returns unspecified values when available, and raises the native diagnostic otherwise.
  |#
  (define require-optional-library
    (lambda (who library)
      (pcheck ([symbol? who library])
              (let ([info (native-library-info (symbol->string library))])
                (unless (and (vector? info) (vector-ref info 1))
                  (error who (if (vector? info) (vector-ref info 4)
                                 "unknown optional library")))))))
  )
