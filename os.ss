(library (chezpp os)
  (export sleep milisleep nanosleep

          unix? windows? darwin?

          os-error?)
  (import (chezpp chez)
          (chezpp private os)
          (chezpp internal)
          (chezpp utils))


  (define-condition-type &os &error make-os-error os-error?)

  (define $err-os
    (lambda (who msg) (raise (condition (make-os-error) (make-who-condition who) (make-message-condition msg)))))


  #|doc
  Make the current thread sleep for `t` seconds.
  |#
  (define sleep
    (lambda (t)
      (pcheck-natural (t)
                      ($sleep (make-time 'time-duration 0 t)))))

  #|doc
  Make the current thread sleep for `t` miliseconds.
  |#
  (define milisleep
    (lambda (t)
      (pcheck-natural (t)
                      (if (fx>= t 1000)
                          (let ([sec  (fx/ t 1000)]
                                [nsec (fx* 1000000 (fxmod t 1000))])
                            ($sleep (make-time 'time-duration nsec sec)))
                          ($sleep (make-time 'time-duration (fx* t 1000000) 0))))))
  #|doc
  Make the current thread sleep for `t` nanoseconds.
  |#
  (define nanosleep
    (lambda (t)
      (pcheck-natural (t)
                      (if (fx>= t 1000000000)
                          (let ([sec  (fx/ t 1000000000)]
                                [nsec (fxmod t 1000000000)])
                            ($sleep (make-time 'time-duration nsec sec)))
                          ($sleep (make-time 'time-duration t 0))))))


  )
