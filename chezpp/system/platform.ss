(library (chezpp system platform)
  (export unix? windows? darwin? linux? system-platform hostname system-hostname
          cpu-arch cpu-count system-machine)
  (import (chezpp chez) (chezpp private os) (chezpp utils))

;;;;===----------------------------------------------------------------------===
;;;; platform info
;;;;===----------------------------------------------------------------------===

  #|proc:linux?
The `linux?` procedure returns `#t` when the current platform is treated as Unix by Chezpp, otherwise `#f`.
|#
  (define linux?
    (lambda ()
      (unix?)))

  #|proc:hostname
The `hostname` procedure returns the hostname of the current operating system.
|#
  (define hostname
    (foreign-procedure "chezpp_hostname" () ptr))

  #|proc:system-hostname
The `system-hostname` procedure returns the hostname of the current operating system.
|#
  (define system-hostname hostname)

  #|proc:cpu-arch
The `cpu-arch` procedure returns the instruction set architecture name of the current processor.
|#
  (define cpu-arch
    (foreign-procedure "chezpp_cpu_arch" () ptr))

  #|proc:cpu-count
The `cpu-count` procedure returns the number of available logical processors.
|#
  (define cpu-count
    (foreign-procedure "chezpp_cpu_count" () int))

  #|proc:system-machine
The `system-machine` procedure returns the machine type symbol reported by Chez Scheme.
|#
  (define system-machine
    (lambda ()
      (machine-type)))

  #|proc:system-platform
The `system-platform` procedure returns a symbol naming the current platform family.
|#
  (define system-platform
    (lambda ()
      (cond [(windows?) 'windows]
            [(darwin?) 'darwin]
            [(linux?) 'linux]
            [else 'unknown])))

  )
