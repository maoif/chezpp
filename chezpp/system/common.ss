(library (chezpp system common)
  (export system-error? make-system-error system-error-operation system-error-code
          system-error-message system-error-context system-unsupported-error?
          make-system-unsupported-error system-not-found-error? make-system-not-found-error
          system-permission-error? make-system-permission-error system-timeout-error?
          make-system-timeout-error system-exit-error? make-system-exit-error
          raise-system-error raise-system-unsupported ffi-result-ref)
  (import (chezpp system errors) (chezpp system ffi)))
