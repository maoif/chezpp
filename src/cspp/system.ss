(library (cspp system)
  (export
          ;; raw process APIs retained at the facade
          fork
          vfork
          getpid
          gettid
          getppid

          ;; dynamic loader information
          shared-object-list

          ;; compatibility condition predicate
          os-error?)
  (import (cspp chez)
          (cspp system common)
          (except (cspp system darwin) darwin?)
          (cspp system filesystem)
          (cspp system info)
          (except (cspp system linux) linux?)
          (cspp system process)
          (cspp system signal)
          (cspp system user)
          (except (cspp system windows) windows?)
          (cspp utils))

  (export
          ;; focused system libraries re-exported by the facade
          (import (cspp system common)
                  (except (cspp system darwin) darwin?)
                  (cspp system filesystem)
                  (cspp system info)
                  (except (cspp system linux) linux?)
                  (cspp system process)
                  (cspp system signal)
                  (cspp system user)
                  (except (cspp system windows) windows?)))

;;;;===----------------------------------------------------------------------===
;;;; OS errors
;;;;===----------------------------------------------------------------------===

  #|proc:os-error?
The `os-error?` procedure returns `#t` when its argument is an operating system error condition, otherwise `#f`.
The `condition` parameter is the object to test.
|#
  (define-condition-type &os &error make-os-error os-error?)

  (define $err-os
    (lambda (who msg)
      (raise (condition (make-os-error)
                        (make-who-condition who)
                        (make-message-condition msg)))))

;;;;===----------------------------------------------------------------------===
;;;; processes
;;;;===----------------------------------------------------------------------===

  #|proc:shared-object-list
The `shared-object-list` procedure returns a list of shared objects currently loaded by the process, in load order.
|#
  (define-who shared-object-list
    (let ([ffi (foreign-procedure "cspp_shared_object_list" () ptr)])
      (lambda ()
        (let* ([x (ffi)] [rx (reverse x)]
               [res (if (string=? "" (car rx)) (cdr rx) rx)])
          res))))

  )
