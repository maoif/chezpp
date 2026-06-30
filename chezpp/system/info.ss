(library (chezpp system info)
  (export sleep milisleep nanosleep
          sleep-seconds sleep-milliseconds sleep-nanoseconds

          unix? windows? darwin? linux?
          hostname system-hostname
          cpu-arch cpu-count
          system-machine system-platform)
  (import (chezpp chez)
          (chezpp private os)
          (chezpp utils))

;;;;===----------------------------------------------------------------------===
;;;; time
;;;;===----------------------------------------------------------------------===

  #|proc:sleep
The `sleep` procedure pauses the current thread for `t` seconds.
The `t` parameter is a natural number of seconds.
|#
  (define sleep
    (lambda (t)
      (pcheck-natural (t)
                      ($sleep (make-time 'time-duration 0 t)))))

  #|proc:milisleep
The `milisleep` procedure pauses the current thread for `t` milliseconds.
The `t` parameter is a natural number of milliseconds.
|#
  (define milisleep
    (lambda (t)
      (pcheck-natural (t)
                      (if (fx>= t 1000)
                          (let ([sec  (fx/ t 1000)]
                                [nsec (fx* 1000000 (fxmod t 1000))])
                            ($sleep (make-time 'time-duration nsec sec)))
                          ($sleep (make-time 'time-duration (fx* t 1000000) 0))))))

  #|proc:nanosleep
The `nanosleep` procedure pauses the current thread for `t` nanoseconds.
The `t` parameter is a natural number of nanoseconds.
|#
  (define nanosleep
    (lambda (t)
      (pcheck-natural (t)
                      (if (fx>= t 1000000000)
                          (let ([sec  (fx/ t 1000000000)]
                                [nsec (fxmod t 1000000000)])
                            ($sleep (make-time 'time-duration nsec sec)))
                          ($sleep (make-time 'time-duration t 0))))))

  #|proc:sleep-seconds
The `sleep-seconds` procedure pauses the current thread for `seconds` seconds.
The `seconds` parameter is a natural number of seconds.
|#
  (define sleep-seconds sleep)

  #|proc:sleep-milliseconds
The `sleep-milliseconds` procedure pauses the current thread for `milliseconds` milliseconds.
The `milliseconds` parameter is a natural number of milliseconds.
|#
  (define sleep-milliseconds milisleep)

  #|proc:sleep-nanoseconds
The `sleep-nanoseconds` procedure pauses the current thread for `nanoseconds` nanoseconds.
The `nanoseconds` parameter is a natural number of nanoseconds.
|#
  (define sleep-nanoseconds nanosleep)

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
