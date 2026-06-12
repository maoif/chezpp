#!chezscheme
(library (chezpp logging async)
  (export make-async-logger
          async-logger?
          async-logger-queue-size
          async-logger-overflow-policy
          async-logger-dropped-count
          async-logger-start!
          async-logger-stop!)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp logging level)
          (chezpp logging sink)
          (chezpp logging logger)
          (chezpp logging private common))

  (define-record-type $async-state
    (fields (immutable queue-size $async-state-queue-size)
            (immutable overflow-policy $async-state-overflow-policy)
            (mutable dropped-count $async-state-dropped-count $async-state-dropped-count-set!)
            (immutable queue $async-state-queue)
            (mutable head $async-state-head $async-state-head-set!)
            (mutable count $async-state-count $async-state-count-set!)
            (mutable in-flight-count $async-state-in-flight-count $async-state-in-flight-count-set!)
            (mutable worker $async-state-worker $async-state-worker-set!)
            (mutable running? $async-state-running? $async-state-running?-set!)
            (mutable stopping? $async-state-stopping? $async-state-stopping?-set!)
            (immutable lock $async-state-lock)
            (immutable not-empty $async-state-not-empty)
            (immutable not-full $async-state-not-full)
            (immutable drained $async-state-drained)))

  (define $entry
    (lambda (logger-name level timestamp thread source kind payload args sinks)
      (vector logger-name level timestamp thread source kind payload args sinks)))

  (define $entry-ref vector-ref)

  (define $logger-async-record
    (lambda (logger)
      (let ([state (logger-async-state logger)])
        (if state
            (vector-ref state 3)
            #f))))

  #|proc:async-logger?
  The `async-logger?` procedure returns whether `x` is an async logger.
  |#
  (define async-logger?
    (lambda (x)
      (and (logger? x) ($logger-async-record x) #t)))

  (define $require-async-state
    (lambda (logger)
      (let ([state ($logger-async-record logger)])
        (if state
            state
            (errorf 'async-logger "not an async logger: ~a" logger)))))

  (define $queue-tail
    (lambda (state)
      (let ([size ($async-state-queue-size state)])
        (mod (+ ($async-state-head state) ($async-state-count state)) size))))

  (define $enqueue!
    (lambda (logger logger-name level timestamp thread source kind payload args sinks)
      (let* ([state ($require-async-state logger)]
             [entry ($entry logger-name level timestamp thread source kind payload args sinks)]
             [lock ($async-state-lock state)])
        (mutex-acquire lock)
        (let loop ()
          (cond [(not ($async-state-running? state))
                 (mutex-release lock)
                 (logger-dispatch-sync! logger logger-name level timestamp thread source kind payload args sinks)]
                [(< ($async-state-count state) ($async-state-queue-size state))
                 (vector-set! ($async-state-queue state) ($queue-tail state) entry)
                 ($async-state-count-set! state (+ ($async-state-count state) 1))
                 (condition-broadcast ($async-state-not-empty state))
                 (mutex-release lock)]
                [else
                 (case ($async-state-overflow-policy state)
                   [(block)
                    (condition-wait ($async-state-not-full state) lock)
                    (loop)]
                   [(drop-newest)
                    ($async-state-dropped-count-set! state (+ ($async-state-dropped-count state) 1))
                    (mutex-release lock)]
                   [(drop-oldest)
                    (let ([head ($async-state-head state)])
                      ($async-state-head-set! state (mod (+ head 1) ($async-state-queue-size state)))
                      (vector-set! ($async-state-queue state) head entry)
                      ($async-state-dropped-count-set! state (+ ($async-state-dropped-count state) 1))
                      (condition-broadcast ($async-state-not-empty state))
                      (mutex-release lock))])])))))

  (define $dequeue!
    (lambda (state)
      (let ([head ($async-state-head state)])
        (let ([entry (vector-ref ($async-state-queue state) head)])
          (vector-set! ($async-state-queue state) head #f)
          ($async-state-head-set! state (mod (+ head 1) ($async-state-queue-size state)))
          ($async-state-count-set! state (- ($async-state-count state) 1))
          ($async-state-in-flight-count-set! state (+ ($async-state-in-flight-count state) 1))
          (condition-broadcast ($async-state-not-full state))
          entry))))

  (define $complete-entry!
    (lambda (state)
      (mutex-acquire ($async-state-lock state))
      ($async-state-in-flight-count-set! state (- ($async-state-in-flight-count state) 1))
      (when (and (= ($async-state-count state) 0)
                 (= ($async-state-in-flight-count state) 0))
        (condition-broadcast ($async-state-drained state)))
      (mutex-release ($async-state-lock state))))

  (define $worker-loop
    (lambda (logger state)
      (let loop ()
        (mutex-acquire ($async-state-lock state))
        (let wait-loop ()
          (when (and (= ($async-state-count state) 0)
                     (not ($async-state-stopping? state)))
            (condition-wait ($async-state-not-empty state) ($async-state-lock state))
            (wait-loop)))
        (if (and (= ($async-state-count state) 0)
                 ($async-state-stopping? state))
            (begin
              ($async-state-running?-set! state #f)
              (when (= ($async-state-in-flight-count state) 0)
                (condition-broadcast ($async-state-drained state)))
              (condition-broadcast ($async-state-not-full state))
              (mutex-release ($async-state-lock state)))
            (let ([entry ($dequeue! state)])
              (mutex-release ($async-state-lock state))
              (dynamic-wind
                (lambda () (void))
                (lambda ()
                  (logger-dispatch-sync! logger
                                         ($entry-ref entry 0)
                                         ($entry-ref entry 1)
                                         ($entry-ref entry 2)
                                         ($entry-ref entry 3)
                                         ($entry-ref entry 4)
                                         ($entry-ref entry 5)
                                         ($entry-ref entry 6)
                                         ($entry-ref entry 7)
                                         ($entry-ref entry 8)))
                (lambda () ($complete-entry! state)))
              (loop))))))

  (define $flush!
    (lambda (logger)
      (let ([state ($require-async-state logger)])
        (mutex-acquire ($async-state-lock state))
        (let loop ()
          (when (or (> ($async-state-count state) 0)
                    (> ($async-state-in-flight-count state) 0))
            (condition-wait ($async-state-drained state) ($async-state-lock state))
            (loop)))
        (mutex-release ($async-state-lock state))
        (for-each log-sink-flush! (logger-sinks logger)))))

  (define $stop!
    (lambda (logger)
      (let ([state ($require-async-state logger)])
        ($flush! logger)
        (mutex-acquire ($async-state-lock state))
        (let ([worker ($async-state-worker state)])
          (when ($async-state-running? state)
            ($async-state-stopping?-set! state #t)
            (condition-broadcast ($async-state-not-empty state)))
          (mutex-release ($async-state-lock state))
          (when worker
            (thread-join worker)
            (mutex-acquire ($async-state-lock state))
            ($async-state-worker-set! state #f)
            (mutex-release ($async-state-lock state)))))))

  #|proc:make-async-logger
  The `make-async-logger` procedure creates an async logger from `logger` using
  a bounded queue of `queue-size` entries and `overflow-policy`, one of `block`,
  `drop-newest`, or `drop-oldest`.
  |#
  (define make-async-logger
    (lambda (logger queue-size overflow-policy)
      (pcheck ([logger? logger]
               [positive-natural? queue-size]
               [log-overflow-policy? overflow-policy])
              (let* ([state (make-$async-state queue-size
                                               overflow-policy
                                               0
                                               (make-vector queue-size #f)
                                               0
                                               0
                                               0
                                               #f
                                               #f
                                               #f
                                               (make-mutex 'async-logger)
                                               (make-condition 'async-logger-not-empty)
                                               (make-condition 'async-logger-not-full)
                                               (make-condition 'async-logger-drained))]
                     [async-state (vector $enqueue! $flush! $stop! state)])
                (make-async-logger/internal logger async-state)))))

  #|proc:async-logger-queue-size
  The `async-logger-queue-size` procedure returns the queue capacity of
  `logger`.
  |#
  (define async-logger-queue-size
    (lambda (logger)
      (pcheck ([async-logger? logger])
              ($async-state-queue-size ($require-async-state logger)))))

  #|proc:async-logger-overflow-policy
  The `async-logger-overflow-policy` procedure returns the overflow policy of
  `logger`.
  |#
  (define async-logger-overflow-policy
    (lambda (logger)
      (pcheck ([async-logger? logger])
              ($async-state-overflow-policy ($require-async-state logger)))))

  #|proc:async-logger-dropped-count
  The `async-logger-dropped-count` procedure returns the number of entries
  dropped by `logger`.
  |#
  (define async-logger-dropped-count
    (lambda (logger)
      (pcheck ([async-logger? logger])
              ($async-state-dropped-count ($require-async-state logger)))))

  #|proc:async-logger-start!
  The `async-logger-start!` procedure starts the worker thread for `logger` if
  it is not already running.
  |#
  (define async-logger-start!
    (lambda (logger)
      (pcheck ([async-logger? logger])
              (let ([state ($require-async-state logger)])
                (mutex-acquire ($async-state-lock state))
                (unless ($async-state-running? state)
                  ($async-state-stopping?-set! state #f)
                  ($async-state-running?-set! state #t)
                  ($async-state-worker-set! state
                                            (fork-thread
                                             (lambda () ($worker-loop logger state)))))
                (mutex-release ($async-state-lock state))))))

  #|proc:async-logger-stop!
  The `async-logger-stop!` procedure flushes and stops the worker thread for
  `logger`.
  |#
  (define async-logger-stop!
    (lambda (logger)
      (pcheck ([async-logger? logger])
              ($stop! logger))))

  )
