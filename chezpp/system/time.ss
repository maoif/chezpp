(library (chezpp system time)
  (export sleep milisleep nanosleep sleep-seconds sleep-milliseconds sleep-nanoseconds)
  (import (chezpp chez) (chezpp utils))

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


)
