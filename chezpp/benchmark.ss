(library (chezpp benchmark)
  (export benchmark? benchmark-suite? benchmark-state? benchmark-result?
          benchmark-sample? benchmark-config? benchmark-fixture?
          benchmark-reporter?

          benchmark-name
          benchmark-state benchmark-state-name benchmark-state-suite
          benchmark-state-args benchmark-state-arg
          benchmark-state-template-args benchmark-state-template-arg
          benchmark-state-iteration benchmark-state-iterations
          benchmark-state-sample benchmark-state-user-data
          benchmark-state-set-user-data!
          benchmark-state-counter-set! benchmark-state-counter-add!

          make-benchmark benchmark-register! benchmark-unregister!
          benchmark-clear-registry! benchmark-registry-benchmarks
          current-benchmark-registry

          make-benchmark-config default-benchmark-config benchmark-config-with
          benchmark-config-warmup benchmark-config-samples
          benchmark-config-min-time benchmark-config-max-iterations
          benchmark-config-output benchmark-config-filter
          benchmark-config-reporter benchmark-config-cost-center?

          benchmark-run benchmark-run-one benchmark-run-suite benchmark-select
          benchmark-expand benchmark-summarize
          benchmark-pause-timing benchmark-resume-timing
          benchmark-do-not-optimize benchmark-clobber-memory

          make-benchmark-reporter benchmark-text-reporter
          benchmark-datum-reporter benchmark-csv-reporter
          benchmark-json-reporter benchmark-report

          benchmark-result-name benchmark-result-args
          benchmark-result-template-args benchmark-result-samples
          benchmark-result-summary benchmark-result-counters
          benchmark-result-error
          benchmark-sample-iterations benchmark-sample-sstats
          benchmark-sample-cost-center benchmark-sample-counters

          define-benchmark define-benchmark-suite define-benchmark-fixture
          define-fixture-benchmark define-benchmark-template
          instantiate-benchmark-template benchmark-options)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp internal))


;;;;===----------------------------------------------------------------------===
;;;; records and low-level helpers
;;;;===----------------------------------------------------------------------===

  (define-record-type ($benchmark $mk-benchmark benchmark?)
    (fields name body options args arg-names setup teardown
            (immutable fixture $benchmark-fixture-ref)
            (immutable suite $benchmark-suite-ref)
            template-args))

  (define-record-type ($benchmark-fixture $mk-benchmark-fixture benchmark-fixture?)
    (fields name setup teardown))

  (define-record-type ($benchmark-suite $mk-benchmark-suite benchmark-suite?)
    (fields name benchmarks setup teardown))

  (define-record-type ($benchmark-state $mk-benchmark-state benchmark-state?)
    (fields name suite args template-args iteration iterations sample
            (mutable user-data benchmark-state-user-data benchmark-state-set-user-data!)
            (mutable counters $benchmark-state-counters $benchmark-state-counters-set!)
            (mutable timing-paused? $benchmark-state-timing-paused? $benchmark-state-timing-paused?-set!)))

  (define-record-type ($benchmark-sample $mk-benchmark-sample benchmark-sample?)
    (fields iterations sstats cost-center counters))

  (define-record-type ($benchmark-result $mk-benchmark-result benchmark-result?)
    (fields name args template-args samples summary counters error))

  (define-record-type ($benchmark-config $mk-benchmark-config benchmark-config?)
    (fields warmup samples min-time max-iterations output filter reporter cost-center? stop-on-error?))

  (define-record-type ($benchmark-reporter $mk-benchmark-reporter benchmark-reporter?)
    (fields start result finish))

  (define-record-type ($benchmark-registry $mk-benchmark-registry benchmark-registry?)
    (fields (mutable benchmarks $benchmark-registry-benchmarks $benchmark-registry-benchmarks-set!)))

  (define $void-procedure (lambda args (void)))

  (define $false-procedure (lambda args #f))

  (define $identity (lambda (x) x))

  (define $alist-ref
    (case-lambda
      [(alist key)
       (let ([probe (assq key alist)])
         (if probe
             (cdr probe)
             (errorf '$alist-ref "missing key: ~a" key)))]
      [(alist key default)
       (let ([probe (assq key alist)])
         (if probe (cdr probe) default))]))

  (define $option-ref
    (case-lambda
      [(options key default)
       (let ([probe (assq key options)])
         (if probe (cdr probe) default))]))

  (define $remove-key
    (lambda (key alist)
      (let loop ([alist alist] [out '()])
        (cond
         [(null? alist) (reverse out)]
         [(eq? key (caar alist)) (loop (cdr alist) out)]
         [else (loop (cdr alist) (cons (car alist) out))]))))

  (define $replace-option
    (lambda (options key value)
      (cons (cons key value) ($remove-key key options))))

  (define $integer-sqrt
    (lambda (n)
      (inexact->exact (floor (sqrt (exact->inexact n))))))

  (define $list-last
    (lambda (xs)
      (if (null? (cdr xs))
          (car xs)
          ($list-last (cdr xs)))))

  (define $sort-ascending
    (lambda (xs)
      (list-sort < xs)))

  (define $time->ns
    (lambda (t)
      (+ (* (time-second t) 1000000000)
         (time-nanosecond t))))

  (define $nonnegative-time
    (lambda (t)
      (if (< ($time->ns t) 0)
          (make-time 'time-duration 0 0)
          t)))

  (define $nonnegative-count
    (lambda (n)
      (if (< n 0) 0 n)))

  (define $sanitize-sstats
    (lambda (s)
      (make-sstats ($nonnegative-time (sstats-cpu s))
                   ($nonnegative-time (sstats-real s))
                   ($nonnegative-count (sstats-bytes s))
                   ($nonnegative-count (sstats-gc-count s))
                   ($nonnegative-time (sstats-gc-cpu s))
                   ($nonnegative-time (sstats-gc-real s))
                   ($nonnegative-count (sstats-gc-bytes s)))))

  (define $sample-cpu-ns
    (lambda (sample)
      (let ([iterations (benchmark-sample-iterations sample)])
        (if (zero? iterations)
            0
            (quotient ($time->ns (sstats-cpu (benchmark-sample-sstats sample)))
                      iterations)))))

  (define $sample-real-ns
    (lambda (sample)
      (let ([iterations (benchmark-sample-iterations sample)])
        (if (zero? iterations)
            0
            (quotient ($time->ns (sstats-real (benchmark-sample-sstats sample)))
                      iterations)))))

  (define $sample-bytes
    (lambda (sample)
      (let ([iterations (benchmark-sample-iterations sample)])
        (if (zero? iterations)
            0
            (quotient (sstats-bytes (benchmark-sample-sstats sample))
                      iterations)))))

  (define $mean
    (lambda (xs)
      (if (null? xs)
          0
          (/ (apply + xs) (length xs)))))

  (define $median
    (lambda (xs)
      (if (null? xs)
          0
          (let* ([sorted ($sort-ascending xs)]
                 [len (length sorted)]
                 [mid (quotient len 2)])
            (if (odd? len)
                (list-ref sorted mid)
                (/ (+ (list-ref sorted (- mid 1))
                      (list-ref sorted mid))
                   2))))))

  (define $stddev
    (lambda (xs)
      (if (or (null? xs) (null? (cdr xs)))
          0
          (let ([mean ($mean xs)])
            (sqrt (/ (apply + (map (lambda (x)
                                     (let ([delta (- x mean)])
                                       (* delta delta)))
                                   xs))
                     (length xs)))))))

  (define $summary-values
    (lambda (xs)
      (if (null? xs)
          '((median . 0) (mean . 0) (min . 0) (max . 0) (stddev . 0))
          `((median . ,($median xs))
            (mean . ,($mean xs))
            (min . ,(apply min xs))
            (max . ,(apply max xs))
            (stddev . ,($stddev xs))))))

  (define $merge-counters
    (lambda (samples)
      (let loop-samples ([samples samples] [out '()])
        (if (null? samples)
            (reverse out)
            (let loop-counters ([counters (benchmark-sample-counters (car samples))]
                                [out out])
              (if (null? counters)
                  (loop-samples (cdr samples) out)
                  (let* ([entry (car counters)]
                         [key (car entry)]
                         [value (cdr entry)]
                         [old (assq key out)])
                    (if old
                        (begin
                          (set-cdr! old (+ (cdr old) value))
                          (loop-counters (cdr counters) out))
                        (loop-counters (cdr counters) (cons (cons key value) out))))))))))

  (define $symbol-append
    (lambda (left right)
      (string->symbol (string-append (symbol->string left)
                                     "/"
                                     (symbol->string right)))))

  (define $seconds->ns
    (lambda (seconds)
      (inexact->exact (floor (* (exact->inexact seconds) 1000000000.0)))))

  (define $sstats-real-ns
    (lambda (s)
      ($time->ns (sstats-real s))))


;;;;===----------------------------------------------------------------------===
;;;; public state and descriptor procedures
;;;;===----------------------------------------------------------------------===

  #|proc:benchmark-name
The `benchmark-name` procedure returns the symbolic name of benchmark descriptor
`benchmark`.
|#
  (define benchmark-name
    (lambda (benchmark)
      (pcheck ([benchmark? benchmark])
              ($benchmark-name benchmark))))

  #|proc:benchmark-state
The `benchmark-state` procedure creates a benchmark state for low-level tests or
custom runners. `name` is the concrete run name, `suite` is a suite name or
`#f`, `args` is an argument alist, `template-args` is a template argument alist,
`iteration` is the current iteration index, `iterations` is the current sample's
iteration count, and `sample` is the current sample index.
|#
  (define benchmark-state
    (lambda (name suite args template-args iteration iterations sample)
      (pcheck ([symbol? name] [list? args template-args] [natural? iteration iterations sample])
              ($mk-benchmark-state name suite args template-args iteration iterations sample '() '() #f))))

  #|proc:benchmark-state-name
The `benchmark-state-name` procedure returns the concrete run name stored in
`state`.
|#
  (define benchmark-state-name
    (lambda (state)
      (pcheck ([benchmark-state? state])
              ($benchmark-state-name state))))

  #|proc:benchmark-state-suite
The `benchmark-state-suite` procedure returns the suite name stored in `state`,
or `#f` when the run is not part of a suite.
|#
  (define benchmark-state-suite
    (lambda (state)
      (pcheck ([benchmark-state? state])
              ($benchmark-state-suite state))))

  #|proc:benchmark-state-args
The `benchmark-state-args` procedure returns the runtime argument alist stored
in `state`.
|#
  (define benchmark-state-args
    (lambda (state)
      (pcheck ([benchmark-state? state])
              ($benchmark-state-args state))))

  #|proc:benchmark-state-arg
The `benchmark-state-arg` procedure returns the value associated with symbolic
argument `name` in `state`. When `default` is supplied, it is returned if the
argument is absent; otherwise absence is an error.
|#
  (define benchmark-state-arg
    (case-lambda
      [(state name)
       (pcheck ([benchmark-state? state] [symbol? name])
               ($alist-ref ($benchmark-state-args state) name))]
      [(state name default)
       (pcheck ([benchmark-state? state] [symbol? name])
               ($alist-ref ($benchmark-state-args state) name default))]))

  #|proc:benchmark-state-template-args
The `benchmark-state-template-args` procedure returns the template argument
alist stored in `state`.
|#
  (define benchmark-state-template-args
    (lambda (state)
      (pcheck ([benchmark-state? state])
              ($benchmark-state-template-args state))))

  #|proc:benchmark-state-template-arg
The `benchmark-state-template-arg` procedure returns the value associated with
symbolic template argument `name` in `state`. When `default` is supplied, it is
returned if the template argument is absent; otherwise absence is an error.
|#
  (define benchmark-state-template-arg
    (case-lambda
      [(state name)
       (pcheck ([benchmark-state? state] [symbol? name])
               ($alist-ref ($benchmark-state-template-args state) name))]
      [(state name default)
       (pcheck ([benchmark-state? state] [symbol? name])
               ($alist-ref ($benchmark-state-template-args state) name default))]))

  #|proc:benchmark-state-iteration
The `benchmark-state-iteration` procedure returns the current iteration index
stored in `state`.
|#
  (define benchmark-state-iteration
    (lambda (state)
      (pcheck ([benchmark-state? state])
              ($benchmark-state-iteration state))))

  #|proc:benchmark-state-iterations
The `benchmark-state-iterations` procedure returns the iteration count for the
current sample stored in `state`.
|#
  (define benchmark-state-iterations
    (lambda (state)
      (pcheck ([benchmark-state? state])
              ($benchmark-state-iterations state))))

  #|proc:benchmark-state-sample
The `benchmark-state-sample` procedure returns the current sample index stored
in `state`.
|#
  (define benchmark-state-sample
    (lambda (state)
      (pcheck ([benchmark-state? state])
              ($benchmark-state-sample state))))

  #|proc:benchmark-state-counter-set!
The `benchmark-state-counter-set!` procedure sets counter `name` in `state` to
numeric `value` for the current sample.
|#
  (define benchmark-state-counter-set!
    (lambda (state name value)
      (pcheck ([benchmark-state? state] [symbol? name] [number? value])
              ($benchmark-state-counters-set!
               state
               (cons (cons name value)
                     ($remove-key name ($benchmark-state-counters state)))))))

  #|proc:benchmark-state-counter-add!
The `benchmark-state-counter-add!` procedure adds numeric `value` to counter
`name` in `state` for the current sample.
|#
  (define benchmark-state-counter-add!
    (lambda (state name value)
      (pcheck ([benchmark-state? state] [symbol? name] [number? value])
              (benchmark-state-counter-set!
               state name
               (+ value ($alist-ref ($benchmark-state-counters state) name 0))))))

  #|proc:make-benchmark
The `make-benchmark` procedure creates a benchmark descriptor named by symbol
`name`. `body` is a procedure with signature `(lambda (state arg ... setup-value)
...)`; `options` is a benchmark option alist, usually produced by
`benchmark-options`.
|#
  (define make-benchmark
    (lambda (name body options)
      (pcheck ([symbol? name] [procedure? body] [list? options])
              (let ([args ($normalize-arguments options)])
                ($mk-benchmark name
                               body
                               options
                               args
                               ($argument-names args)
                               ($option-ref options 'setup #f)
                               ($option-ref options 'teardown #f)
                               ($option-ref options 'fixture #f)
                               ($option-ref options 'suite #f)
                               ($option-ref options 'template-args '()))))))

  #|proc:benchmark-register!
The `benchmark-register!` procedure adds `benchmark` to `registry`. `registry`
must be a benchmark registry, usually `(current-benchmark-registry)`.
|#
  (define benchmark-register!
    (lambda (registry benchmark)
      (pcheck ([benchmark-registry? registry] [benchmark? benchmark])
              (begin
                ($benchmark-registry-benchmarks-set!
                 registry
                 (append ($benchmark-registry-benchmarks registry)
                         (list benchmark)))
                benchmark))))

  #|proc:benchmark-unregister!
The `benchmark-unregister!` procedure removes benchmarks named by symbol `name`
from `registry`.
|#
  (define benchmark-unregister!
    (lambda (registry name)
      (pcheck ([benchmark-registry? registry] [symbol? name])
              ($benchmark-registry-benchmarks-set!
               registry
               (let loop ([benchmarks ($benchmark-registry-benchmarks registry)] [out '()])
                 (cond
                  [(null? benchmarks) (reverse out)]
                  [(eq? name ($benchmark-name (car benchmarks)))
                   (loop (cdr benchmarks) out)]
                  [else (loop (cdr benchmarks) (cons (car benchmarks) out))]))))))

  #|proc:benchmark-clear-registry!
The `benchmark-clear-registry!` procedure removes all benchmarks from
`registry`.
|#
  (define benchmark-clear-registry!
    (lambda (registry)
      (pcheck ([benchmark-registry? registry])
              ($benchmark-registry-benchmarks-set! registry '()))))

  #|proc:benchmark-registry-benchmarks
The `benchmark-registry-benchmarks` procedure returns the benchmark descriptors
currently stored in `registry`.
|#
  (define benchmark-registry-benchmarks
    (lambda (registry)
      (pcheck ([benchmark-registry? registry])
              ($benchmark-registry-benchmarks registry))))

  #|proc:current-benchmark-registry
The `current-benchmark-registry` parameter stores the default benchmark
registry used by definition macros and `(benchmark-run)`.
|#
  (define current-benchmark-registry
    (make-parameter ($mk-benchmark-registry '())))


;;;;===----------------------------------------------------------------------===
;;;; configs and reporters
;;;;===----------------------------------------------------------------------===

  (define $default-config #f)

  #|proc:benchmark-result-name
The `benchmark-result-name` procedure returns the concrete name stored in
benchmark `result`.
|#
  (define benchmark-result-name
    (lambda (result)
      (pcheck ([benchmark-result? result])
              ($benchmark-result-name result))))

  #|proc:benchmark-result-args
The `benchmark-result-args` procedure returns the runtime argument alist stored
in benchmark `result`.
|#
  (define benchmark-result-args
    (lambda (result)
      (pcheck ([benchmark-result? result])
              ($benchmark-result-args result))))

  #|proc:benchmark-result-template-args
The `benchmark-result-template-args` procedure returns the template argument
alist stored in benchmark `result`.
|#
  (define benchmark-result-template-args
    (lambda (result)
      (pcheck ([benchmark-result? result])
              ($benchmark-result-template-args result))))

  #|proc:benchmark-result-samples
The `benchmark-result-samples` procedure returns the measured sample records
stored in benchmark `result`.
|#
  (define benchmark-result-samples
    (lambda (result)
      (pcheck ([benchmark-result? result])
              ($benchmark-result-samples result))))

  #|proc:benchmark-result-summary
The `benchmark-result-summary` procedure returns the summary alist stored in
benchmark `result`.
|#
  (define benchmark-result-summary
    (lambda (result)
      (pcheck ([benchmark-result? result])
              ($benchmark-result-summary result))))

  #|proc:benchmark-result-counters
The `benchmark-result-counters` procedure returns the merged custom counter
alist stored in benchmark `result`.
|#
  (define benchmark-result-counters
    (lambda (result)
      (pcheck ([benchmark-result? result])
              ($benchmark-result-counters result))))

  #|proc:benchmark-result-error
The `benchmark-result-error` procedure returns the captured exception stored in
benchmark `result`, or `#f` when the run completed.
|#
  (define benchmark-result-error
    (lambda (result)
      (pcheck ([benchmark-result? result])
              ($benchmark-result-error result))))

  #|proc:benchmark-sample-iterations
The `benchmark-sample-iterations` procedure returns the iteration count stored
in sample `sample`.
|#
  (define benchmark-sample-iterations
    (lambda (sample)
      (pcheck ([benchmark-sample? sample])
              ($benchmark-sample-iterations sample))))

  #|proc:benchmark-sample-sstats
The `benchmark-sample-sstats` procedure returns the sanitized Chez sstats
record stored in sample `sample`.
|#
  (define benchmark-sample-sstats
    (lambda (sample)
      (pcheck ([benchmark-sample? sample])
              ($benchmark-sample-sstats sample))))

  #|proc:benchmark-sample-cost-center
The `benchmark-sample-cost-center` procedure returns optional cost-center data
stored in sample `sample`, or `#f` in v1.
|#
  (define benchmark-sample-cost-center
    (lambda (sample)
      (pcheck ([benchmark-sample? sample])
              ($benchmark-sample-cost-center sample))))

  #|proc:benchmark-sample-counters
The `benchmark-sample-counters` procedure returns the custom counter alist
stored in sample `sample`.
|#
  (define benchmark-sample-counters
    (lambda (sample)
      (pcheck ([benchmark-sample? sample])
              ($benchmark-sample-counters sample))))

  #|proc:make-benchmark-reporter
The `make-benchmark-reporter` procedure creates a reporter descriptor from
callbacks. `start` receives `(results output-port)`, `result` receives
`(result output-port)`, and `finish` receives `(results output-port)`.
|#
  (define make-benchmark-reporter
    (lambda (start result finish)
      (pcheck ([procedure? start result finish])
              ($mk-benchmark-reporter start result finish))))

  #|proc:benchmark-text-reporter
The `benchmark-text-reporter` procedure returns the default plain textual
reporter.
|#
  (define benchmark-text-reporter
    (lambda ()
      (make-benchmark-reporter
       (lambda (results out)
         (fprintf out "name iterations cpu/ns real/ns bytes/iter counters~%"))
       (lambda (result out)
         (let* ([summary (benchmark-result-summary result)]
                [cpu (cdr (assq 'mean (cdr (assq 'cpu-ns summary))))]
                [real (cdr (assq 'mean (cdr (assq 'real-ns summary))))]
                [bytes (cdr (assq 'mean (cdr (assq 'bytes summary))))]
                [iterations (if (null? (benchmark-result-samples result))
                                0
                                (benchmark-sample-iterations
                                 (car (benchmark-result-samples result))))])
           (fprintf out "~a ~a ~a ~a ~a ~s~%"
                    (benchmark-result-name result)
                    iterations
                    cpu
                    real
                    bytes
                    (benchmark-result-counters result))))
       (lambda (results out)
         (flush-output-port out)))))

  #|proc:benchmark-datum-reporter
The `benchmark-datum-reporter` procedure returns a reporter that writes one
Scheme datum per result to the output port.
|#
  (define benchmark-datum-reporter
    (lambda ()
      (make-benchmark-reporter
       $void-procedure
       (lambda (result out)
         (write `((name . ,(benchmark-result-name result))
                  (args . ,(benchmark-result-args result))
                  (template-args . ,(benchmark-result-template-args result))
                  (summary . ,(benchmark-result-summary result))
                  (counters . ,(benchmark-result-counters result))
                  (error . ,(and (benchmark-result-error result) #t)))
                out)
         (newline out))
       (lambda (results out)
         (flush-output-port out)))))

  #|proc:benchmark-csv-reporter
The `benchmark-csv-reporter` procedure is reserved for the v2 CSV reporter.
|#
  (define benchmark-csv-reporter
    (lambda ()
      (errorf 'benchmark-csv-reporter "CSV reporting is planned for benchmark v2")))

  #|proc:benchmark-json-reporter
The `benchmark-json-reporter` procedure is reserved for the v2 JSON reporter.
|#
  (define benchmark-json-reporter
    (lambda ()
      (errorf 'benchmark-json-reporter "JSON reporting is planned for benchmark v2")))

  #|proc:make-benchmark-config
The `make-benchmark-config` procedure creates a runner configuration. The
parameters are `warmup`, `samples`, `min-time`, `max-iterations`, `output`,
`filter`, `reporter`, `cost-center?`, and `stop-on-error?`.
|#
  (define make-benchmark-config
    (lambda (warmup samples min-time max-iterations output filter reporter cost-center? stop-on-error?)
      (pcheck ([natural? warmup samples max-iterations] [number? min-time])
              ($mk-benchmark-config warmup samples min-time max-iterations output filter reporter cost-center? stop-on-error?))))

  #|proc:benchmark-config-warmup
The `benchmark-config-warmup` procedure returns the warmup sample count stored
in runner `config`.
|#
  (define benchmark-config-warmup
    (lambda (config)
      (pcheck ([benchmark-config? config])
              ($benchmark-config-warmup config))))

  #|proc:benchmark-config-samples
The `benchmark-config-samples` procedure returns the measured sample count
stored in runner `config`.
|#
  (define benchmark-config-samples
    (lambda (config)
      (pcheck ([benchmark-config? config])
              ($benchmark-config-samples config))))

  #|proc:benchmark-config-min-time
The `benchmark-config-min-time` procedure returns the target minimum sample time
stored in runner `config`.
|#
  (define benchmark-config-min-time
    (lambda (config)
      (pcheck ([benchmark-config? config])
              ($benchmark-config-min-time config))))

  #|proc:benchmark-config-max-iterations
The `benchmark-config-max-iterations` procedure returns the iteration cap stored
in runner `config`.
|#
  (define benchmark-config-max-iterations
    (lambda (config)
      (pcheck ([benchmark-config? config])
              ($benchmark-config-max-iterations config))))

  #|proc:benchmark-config-output
The `benchmark-config-output` procedure returns the output target stored in
runner `config`.
|#
  (define benchmark-config-output
    (lambda (config)
      (pcheck ([benchmark-config? config])
              ($benchmark-config-output config))))

  #|proc:benchmark-config-filter
The `benchmark-config-filter` procedure returns the benchmark selector stored
in runner `config`.
|#
  (define benchmark-config-filter
    (lambda (config)
      (pcheck ([benchmark-config? config])
              ($benchmark-config-filter config))))

  #|proc:benchmark-config-reporter
The `benchmark-config-reporter` procedure returns the reporter stored in runner
`config`, or `#f` when reporting is disabled.
|#
  (define benchmark-config-reporter
    (lambda (config)
      (pcheck ([benchmark-config? config])
              ($benchmark-config-reporter config))))

  #|proc:benchmark-config-cost-center?
The `benchmark-config-cost-center?` procedure returns whether optional
cost-center measurement is enabled in runner `config`.
|#
  (define benchmark-config-cost-center?
    (lambda (config)
      (pcheck ([benchmark-config? config])
              ($benchmark-config-cost-center? config))))

  #|proc:default-benchmark-config
The `default-benchmark-config` procedure returns the default benchmark runner
configuration.
|#
  (define default-benchmark-config
    (lambda ()
      (or $default-config
          (begin
            (set! $default-config
                  (make-benchmark-config 1 10 0.01 1000000
                                         (current-output-port)
                                         #f
                                         (benchmark-text-reporter)
                                         #f
                                         #f))
            $default-config))))

  #|proc:benchmark-config-with
The `benchmark-config-with` procedure returns a copy of `config` with fields
changed according to option alist `options`.
|#
  (define benchmark-config-with
    (lambda (config options)
      (pcheck ([benchmark-config? config] [list? options])
              (let loop ([options options] [config config])
                (if (null? options)
                    config
                    (let ([option (car options)])
                      (case (car option)
                        [(warmup)
                         (loop (cdr options)
                               ($mk-benchmark-config (cdr option)
                                                     (benchmark-config-samples config)
                                                     (benchmark-config-min-time config)
                                                     (benchmark-config-max-iterations config)
                                                     (benchmark-config-output config)
                                                     (benchmark-config-filter config)
                                                     (benchmark-config-reporter config)
                                                     (benchmark-config-cost-center? config)
                                                     ($benchmark-config-stop-on-error? config)))]
                        [(samples)
                         (loop (cdr options)
                               ($mk-benchmark-config (benchmark-config-warmup config)
                                                     (cdr option)
                                                     (benchmark-config-min-time config)
                                                     (benchmark-config-max-iterations config)
                                                     (benchmark-config-output config)
                                                     (benchmark-config-filter config)
                                                     (benchmark-config-reporter config)
                                                     (benchmark-config-cost-center? config)
                                                     ($benchmark-config-stop-on-error? config)))]
                        [(min-time)
                         (loop (cdr options)
                               ($mk-benchmark-config (benchmark-config-warmup config)
                                                     (benchmark-config-samples config)
                                                     (cdr option)
                                                     (benchmark-config-max-iterations config)
                                                     (benchmark-config-output config)
                                                     (benchmark-config-filter config)
                                                     (benchmark-config-reporter config)
                                                     (benchmark-config-cost-center? config)
                                                     ($benchmark-config-stop-on-error? config)))]
                        [(max-iterations)
                         (loop (cdr options)
                               ($mk-benchmark-config (benchmark-config-warmup config)
                                                     (benchmark-config-samples config)
                                                     (benchmark-config-min-time config)
                                                     (cdr option)
                                                     (benchmark-config-output config)
                                                     (benchmark-config-filter config)
                                                     (benchmark-config-reporter config)
                                                     (benchmark-config-cost-center? config)
                                                     ($benchmark-config-stop-on-error? config)))]
                        [(output)
                         (loop (cdr options)
                               ($mk-benchmark-config (benchmark-config-warmup config)
                                                     (benchmark-config-samples config)
                                                     (benchmark-config-min-time config)
                                                     (benchmark-config-max-iterations config)
                                                     (cdr option)
                                                     (benchmark-config-filter config)
                                                     (benchmark-config-reporter config)
                                                     (benchmark-config-cost-center? config)
                                                     ($benchmark-config-stop-on-error? config)))]
                        [(filter)
                         (loop (cdr options)
                               ($mk-benchmark-config (benchmark-config-warmup config)
                                                     (benchmark-config-samples config)
                                                     (benchmark-config-min-time config)
                                                     (benchmark-config-max-iterations config)
                                                     (benchmark-config-output config)
                                                     (cdr option)
                                                     (benchmark-config-reporter config)
                                                     (benchmark-config-cost-center? config)
                                                     ($benchmark-config-stop-on-error? config)))]
                        [(reporter)
                         (loop (cdr options)
                               ($mk-benchmark-config (benchmark-config-warmup config)
                                                     (benchmark-config-samples config)
                                                     (benchmark-config-min-time config)
                                                     (benchmark-config-max-iterations config)
                                                     (benchmark-config-output config)
                                                     (benchmark-config-filter config)
                                                     (cdr option)
                                                     (benchmark-config-cost-center? config)
                                                     ($benchmark-config-stop-on-error? config)))]
                        [(cost-center?)
                         (loop (cdr options)
                               ($mk-benchmark-config (benchmark-config-warmup config)
                                                     (benchmark-config-samples config)
                                                     (benchmark-config-min-time config)
                                                     (benchmark-config-max-iterations config)
                                                     (benchmark-config-output config)
                                                     (benchmark-config-filter config)
                                                     (benchmark-config-reporter config)
                                                     (cdr option)
                                                     ($benchmark-config-stop-on-error? config)))]
                        [(stop-on-error?)
                         (loop (cdr options)
                               ($mk-benchmark-config (benchmark-config-warmup config)
                                                     (benchmark-config-samples config)
                                                     (benchmark-config-min-time config)
                                                     (benchmark-config-max-iterations config)
                                                     (benchmark-config-output config)
                                                     (benchmark-config-filter config)
                                                     (benchmark-config-reporter config)
                                                     (benchmark-config-cost-center? config)
                                                     (cdr option)))]
                        [else (errorf 'benchmark-config-with "unknown config option: ~a" (car option))])))))))


;;;;===----------------------------------------------------------------------===
;;;; argument normalization and syntax
;;;;===----------------------------------------------------------------------===

  (define $range-values
    (lambda (start end step geometric?)
      (when (zero? step)
        (errorf '$range-values "range step must not be zero"))
      (let loop ([value start] [out '()])
        (if (> value end)
            (reverse out)
            (loop (if geometric? (* value step) (+ value step))
                  (cons value out))))))

  (define $product
    (lambda (dimensions)
      (let loop ([dimensions dimensions])
        (if (null? dimensions)
            '(())
            (let ([tail (loop (cdr dimensions))]
                  [dimension (car dimensions)])
              (apply append
                     (map (lambda (value)
                            (map (lambda (rest) (cons value rest)) tail))
                          dimension)))))))

  (define $rows->args
    (lambda (rows)
      (map (lambda (row)
             (let loop ([row row] [names '()] [values '()] [alist '()])
               (cond
                [(null? row)
                 (list (reverse names) (reverse values) (reverse alist))]
                [(or (null? (cdr row)) (not (symbol? (car row))))
                 (errorf '$rows->args "invalid argument row: ~s" row)]
                [else
                 (loop (cddr row)
                       (cons (car row) names)
                       (cons (cadr row) values)
                       (cons (cons (car row) (cadr row)) alist))])))
           rows)))

  (define $normalize-arguments
    (lambda (options)
      (let ([args-options (filter (lambda (option)
                                    (memq (car option) '(args arg-range dense-arg-range arg-product)))
                                  options)])
        (if (null? args-options)
            (list (list '() '() '()))
            (let loop ([options args-options] [rows (list (list '() '() '()))])
              (if (null? options)
                  rows
                  (let* ([option (car options)]
                         [new-rows
                          (case (car option)
                            [(args)
                             ($rows->args (cdr option))]
                            [(arg-range)
                             (let* ([spec (cdr option)]
                                    [name (car spec)]
                                    [values ($range-values (cadr spec) (caddr spec) (cadddr spec) #t)])
                               (map (lambda (value)
                                      (list (list name) (list value) (list (cons name value))))
                                    values))]
                            [(dense-arg-range)
                             (let* ([spec (cdr option)]
                                    [name (car spec)]
                                    [values ($range-values (cadr spec) (caddr spec) (cadddr spec) #f)])
                               (map (lambda (value)
                                      (list (list name) (list value) (list (cons name value))))
                                    values))]
                            [(arg-product)
                             (let* ([specs (cdr option)]
                                    [names (map car specs)]
                                    [dimensions (map cdr specs)])
                               (map (lambda (values)
                                      (list names values (map cons names values)))
                                    ($product dimensions)))]
                            [else (errorf '$normalize-arguments "unknown argument option: ~a" (car option))])])
                    (loop (cdr options)
                          (apply append
                                 (map (lambda (left)
                                        (map (lambda (right)
                                               (list (append (car left) (car right))
                                                     (append (cadr left) (cadr right))
                                                     (append (caddr left) (caddr right))))
                                             new-rows))
                                      rows))))))))))

  (define $argument-names
    (lambda (args)
      (if (null? args)
          '()
          (car (car args)))))

  (define-syntax benchmark-options
    (lambda (stx)
      (define keyword?
        (lambda (x)
          (and (identifier? x)
               (let ([datum (syntax->datum x)])
                 (and (symbol? datum)
                      (let ([text (symbol->string datum)])
                        (and (> (string-length text) 0)
                             (char=? (string-ref text 0) #\:))))))))
      (define keyword->option-name
        (lambda (datum)
          (string->symbol (substring (symbol->string datum) 1
                                     (string-length (symbol->string datum))))))
      (define parse-row
        (lambda (row)
          (syntax-case row ()
            [(name value rest ...)
             (identifier? #'name)
             #'(name value rest ...)]
            [_ (syntax-error row "invalid benchmark argument row")])))
      (define quote-row
        (lambda (context row)
          #`'#,(datum->syntax context (syntax->datum (parse-row row)))))
      (define parse-option
        (lambda (key rest)
          (let ([datum (syntax->datum key)])
            (case datum
              [(:args)
               #`(cons 'args (list #,@(map (lambda (row) (quote-row key row)) rest)))]
              [(:arg-product)
               #`(cons 'arg-product (list #,@(map (lambda (row) (quote-row key row)) rest)))]
              [(:arg-range)
               (unless (= (length rest) 1)
                 (syntax-error key ":arg-range expects one row"))
               #`(cons 'arg-range #,(quote-row key (car rest)))]
              [(:dense-arg-range)
               (unless (= (length rest) 1)
                 (syntax-error key ":dense-arg-range expects one row"))
               #`(cons 'dense-arg-range #,(quote-row key (car rest)))]
              [(:setup)
               (unless (= (length rest) 1)
                 (syntax-error key ":setup expects one expression"))
               #`(cons 'setup #,(car rest))]
              [(:teardown)
               (unless (= (length rest) 1)
                 (syntax-error key ":teardown expects one expression"))
               #`(cons 'teardown #,(car rest))]
              [(:suite-setup)
               (unless (= (length rest) 1)
                 (syntax-error key ":suite-setup expects one expression"))
               #`(cons 'suite-setup #,(car rest))]
              [(:suite-teardown)
               (unless (= (length rest) 1)
                 (syntax-error key ":suite-teardown expects one expression"))
               #`(cons 'suite-teardown #,(car rest))]
              [(:unit :complexity)
               (unless (= (length rest) 1)
                 (syntax-error key "benchmark option expects one value"))
               (if (identifier? (car rest))
                   #`(cons '#,(datum->syntax key (keyword->option-name datum))
                           '#,(car rest))
                   #`(cons '#,(datum->syntax key (keyword->option-name datum))
                           #,(car rest)))]
              [(:throughput)
               #`(cons 'throughput '#,(datum->syntax key (map syntax->datum rest)))]
              [(:warmup :samples :min-time :max-iterations :reporter :output :filter :cost-center? :stop-on-error?)
               (unless (= (length rest) 1)
                 (syntax-error key "benchmark option expects one expression"))
               #`(cons '#,(datum->syntax key (keyword->option-name datum))
                       #,(car rest))]
              [else (syntax-error key "unknown benchmark option")]))))
      (define split-options
        (lambda (items)
          (let loop ([items items] [key #f] [current '()] [out '()])
            (cond
             [(null? items)
              (if key
                  (reverse (cons (cons key (reverse current)) out))
                  (reverse out))]
             [(keyword? (car items))
              (loop (cdr items)
                    (car items)
                    '()
                    (if key
                        (cons (cons key (reverse current)) out)
                        out))]
             [key
              (loop (cdr items) key (cons (car items) current) out)]
             [else (syntax-error (car items) "benchmark option list must begin with a keyword")]))))
      (syntax-case stx ()
        [(_ (item ...))
         #`(list #,@(map (lambda (clause)
                           (parse-option (car clause) (cdr clause)))
                         (split-options #'(item ...))))]
        [_ (syntax-error stx "invalid benchmark-options form")])))

  #|macro:define-benchmark
The `define-benchmark` macro defines and registers a benchmark descriptor named
by `name`. `options` configures arguments, lifecycle hooks, and runner defaults;
`body` is a procedure that receives state, argument values, and optional setup
value.
|#
  (define-syntax define-benchmark
    (syntax-rules ()
      [(_ name options body)
       (define name
         (let ([benchmark (make-benchmark 'name body (benchmark-options options))])
           (benchmark-register! (current-benchmark-registry) benchmark)
           benchmark))]))

  #|macro:define-benchmark-fixture
The `define-benchmark-fixture` macro defines a reusable fixture descriptor named
by `name`. Its setup hook receives state and its teardown hook receives state
and the setup value.
|#
  (define-syntax define-benchmark-fixture
    (syntax-rules ()
      [(_ name options)
       (define name
         (let ([opts (benchmark-options options)])
           ($mk-benchmark-fixture 'name
                                  ($option-ref opts 'setup $false-procedure)
                                  ($option-ref opts 'teardown $void-procedure))))]))

  #|macro:define-fixture-benchmark
The `define-fixture-benchmark` macro defines and registers benchmark `name`
using fixture descriptor `fixture`.
|#
  (define-syntax define-fixture-benchmark
    (syntax-rules ()
      [(_ name fixture-descriptor options body)
       (define name
         (let* ([opts (benchmark-options options)]
                [benchmark (make-benchmark 'name body
                                           (cons (cons 'fixture fixture-descriptor) opts))])
           (benchmark-register! (current-benchmark-registry) benchmark)
           benchmark))]))

  #|macro:define-benchmark-suite
The `define-benchmark-suite` macro defines a suite descriptor named by `name`
containing the listed benchmark descriptors.
|#
  (define-syntax define-benchmark-suite
    (syntax-rules ()
      [(_ name options benchmark ...)
       (define name
         (let ([opts (benchmark-options options)])
           ($mk-benchmark-suite 'name
                                (list benchmark ...)
                                ($option-ref opts 'suite-setup $false-procedure)
                                ($option-ref opts 'suite-teardown $void-procedure))))]))

  #|macro:define-benchmark-template
The `define-benchmark-template` macro defines a template procedure named by
`name`. The generated procedure accepts template parameters `param ...` and
returns a benchmark descriptor.
|#
  (define-syntax define-benchmark-template
    (syntax-rules ()
      [(_ name (param ...) options body)
       (define name
         (lambda (param ...)
           (make-benchmark 'name body
                           (append (benchmark-options options)
                                   (list (cons 'template-args
                                               (list (cons 'param param) ...)))))))]))

  #|macro:instantiate-benchmark-template
The `instantiate-benchmark-template` macro registers concrete benchmark
descriptors from template `template`. Each row supplies values positionally for
the template parameters, and the first row value becomes the concrete name
suffix.
|#
  (define-syntax instantiate-benchmark-template
    (syntax-rules ()
      [(_ template ([label value ...] ...))
       (begin
         (define template-instance-marker (void))
         (let ([benchmark (template 'label value ...)])
           (let ([renamed (make-benchmark ($symbol-append 'template 'label)
                                          ($benchmark-body benchmark)
                                          ($replace-option ($benchmark-options benchmark)
                                                           'template-args
                                                           ($benchmark-template-args benchmark)))])
             (benchmark-register! (current-benchmark-registry) renamed)))
         ...)]))


;;;;===----------------------------------------------------------------------===
;;;; expansion and runner
;;;;===----------------------------------------------------------------------===

  #|proc:benchmark-expand
The `benchmark-expand` procedure expands `benchmark` into concrete run pairs
for `config`. Each pair contains a benchmark state in the car and the descriptor
to run in the cdr.
|#
  (define benchmark-expand
    (lambda (benchmark config)
      (pcheck ([benchmark? benchmark] [benchmark-config? config])
              (map (lambda (arg-row)
                     (let ([args (caddr arg-row)])
                       (cons ($mk-benchmark-state ($benchmark-name benchmark)
                                                  ($benchmark-suite-ref benchmark)
                                                  args
                                                  ($benchmark-template-args benchmark)
                                                  0
                                                  1
                                                  0
                                                  '()
                                                  '()
                                                  #f)
                             benchmark)))
                   ($benchmark-args benchmark)))))

  #|proc:benchmark-select
The `benchmark-select` procedure selects benchmarks from `registry` according
to `selector`, which may be `#f`, a symbol, a string, or a predicate procedure.
|#
  (define benchmark-select
    (lambda (registry selector)
      (pcheck ([benchmark-registry? registry])
              (let ([benchmarks ($benchmark-registry-benchmarks registry)])
                (cond
                 [(not selector) benchmarks]
                 [(symbol? selector)
                  (filter (lambda (benchmark)
                            (eq? selector ($benchmark-name benchmark)))
                          benchmarks)]
                 [(string? selector)
                  (filter (lambda (benchmark)
                            (let ([name (symbol->string ($benchmark-name benchmark))])
                              (and (<= (string-length selector) (string-length name))
                                   (string=? selector
                                             (substring name 0 (string-length selector))))))
                          benchmarks)]
                 [(procedure? selector)
                  (filter selector benchmarks)]
                 [else (errorf 'benchmark-select "invalid selector: ~s" selector)])))))

  (define $call-with-values-list
    (lambda (producer consumer)
      (call-with-values producer
        (lambda values
          (consumer values)))))

  (define $apply-body
    (lambda (benchmark state values setup-value setup-value?)
      (if setup-value?
          (apply ($benchmark-body benchmark) state (append values (list setup-value)))
          (apply ($benchmark-body benchmark) state values))))

  (define $run-body-iterations
    (lambda (benchmark state values setup-value setup-value? iterations)
      (let loop ([i 0])
        (when (< i iterations)
          ($apply-body benchmark state values setup-value setup-value?)
          (loop (+ i 1))))))

  (define $setup-for-sample
    (lambda (benchmark state)
        (let ([fixture ($benchmark-fixture-ref benchmark)])
        (cond
         [fixture
          (values (($benchmark-fixture-setup fixture) state)
                  ($benchmark-fixture-teardown fixture)
                  #t)]
         [($benchmark-setup benchmark)
          (values (($benchmark-setup benchmark) state)
                  (or ($benchmark-teardown benchmark) $void-procedure)
                  #t)]
         [else
          (values #f $void-procedure #f)]))))

  (define $measure-sample
    (lambda (benchmark state values iterations)
      ($call-with-values-list
       (lambda ()
         ($setup-for-sample benchmark state))
       (lambda (setup-results)
         (let ([setup-value (car setup-results)]
               [teardown (cadr setup-results)]
               [setup-value? (caddr setup-results)]
               [cleanup? #t]
               [sample-state ($mk-benchmark-state ($benchmark-state-name state)
                                                  ($benchmark-state-suite state)
                                                  ($benchmark-state-args state)
                                                  ($benchmark-state-template-args state)
                                                  0
                                                  iterations
                                                  ($benchmark-state-sample state)
                                                  (benchmark-state-user-data state)
                                                  '()
                                                  #f)])
           (dynamic-wind
             (lambda () (void))
             (lambda ()
               (let* ([b1 (statistics)]
                      [b2 (statistics)])
                 ($run-body-iterations benchmark sample-state values setup-value setup-value? iterations)
                 (let* ([after (statistics)]
                        [elapsed (sstats-difference after b2)]
                        [overhead (sstats-difference b2 b1)]
                        [adjusted ($sanitize-sstats (sstats-difference elapsed overhead))])
                   ($mk-benchmark-sample iterations adjusted #f ($benchmark-state-counters sample-state)))))
             (lambda ()
               (when cleanup?
                 (set! cleanup? #f)
                 (when setup-value?
                   (teardown state setup-value))))))))))

  (define $warmup
    (lambda (benchmark state values config)
      (let loop ([i 0])
        (when (< i (benchmark-config-warmup config))
          ($measure-sample benchmark state values 1)
          (loop (+ i 1))))))

  (define $choose-sample
    (lambda (benchmark state values config sample-index)
      (let loop ([iterations 1])
        (let* ([sample-state ($mk-benchmark-state ($benchmark-state-name state)
                                                  ($benchmark-state-suite state)
                                                  ($benchmark-state-args state)
                                                  ($benchmark-state-template-args state)
                                                  0
                                                  iterations
                                                  sample-index
                                                  (benchmark-state-user-data state)
                                                  '()
                                                  #f)]
               [sample ($measure-sample benchmark sample-state values iterations)]
               [target ($seconds->ns (benchmark-config-min-time config))])
          (if (or (zero? target)
                  (>= ($sstats-real-ns (benchmark-sample-sstats sample)) target)
                  (>= iterations (benchmark-config-max-iterations config)))
              sample
              (loop (min (benchmark-config-max-iterations config)
                         (* iterations 2))))))))

  (define $run-expanded
    (lambda (expanded config)
      (let* ([state (car expanded)]
             [benchmark (cdr expanded)]
             [arg-row (car (filter (lambda (row)
                                     (equal? (caddr row) ($benchmark-state-args state)))
                                   ($benchmark-args benchmark)))]
             [values (cadr arg-row)])
        (guard (exn
                [else
                 ($mk-benchmark-result ($benchmark-state-name state)
                                       ($benchmark-state-args state)
                                       ($benchmark-state-template-args state)
                                       '()
                                       (benchmark-summarize '())
                                       '()
                                       exn)])
          ($warmup benchmark state values config)
          (let loop ([i 0] [samples '()])
            (if (< i (benchmark-config-samples config))
                (loop (+ i 1)
                      (cons ($choose-sample benchmark state values config i) samples))
                (let* ([samples (reverse samples)]
                       [summary (benchmark-summarize samples)]
                       [counters ($merge-counters samples)])
                  ($mk-benchmark-result ($benchmark-state-name state)
                                        ($benchmark-state-args state)
                                        ($benchmark-state-template-args state)
                                        samples
                                        summary
                                        counters
                                        #f))))))))

  #|proc:benchmark-run-one
The `benchmark-run-one` procedure runs one concrete expanded benchmark pair
`expanded` with runner `config` and returns one benchmark result.
|#
  (define benchmark-run-one
    (lambda (expanded config)
      (pcheck ([pair? expanded] [benchmark-config? config])
              ($run-expanded expanded config))))

  #|proc:benchmark-run-suite
The `benchmark-run-suite` procedure runs suite descriptor `suite` with runner
`config` and returns the resulting benchmark results.
|#
  (define benchmark-run-suite
    (lambda (suite config)
      (pcheck ([benchmark-suite? suite] [benchmark-config? config])
              (let ([suite-state (benchmark-state ($benchmark-suite-name suite)
                                                  ($benchmark-suite-name suite)
                                                  '()
                                                  '()
                                                  0
                                                  1
                                                  0)])
                ($call-with-values-list
                 (lambda ()
                   (values (($benchmark-suite-setup suite) suite-state)))
                 (lambda (values)
                   (dynamic-wind
                     (lambda () (void))
                     (lambda ()
                       (benchmark-run ($benchmark-suite-benchmarks suite) config))
                     (lambda ()
                       (($benchmark-suite-teardown suite) suite-state (car values))))))))))

  #|proc:benchmark-run
The `benchmark-run` procedure runs registered benchmarks, descriptors, or a
registry with a runner configuration. It supports `(benchmark-run)`,
`(benchmark-run config)`, `(benchmark-run registry config)`, and
`(benchmark-run benchmarks config)`.
|#
  (define benchmark-run
    (case-lambda
      [()
       (benchmark-run (current-benchmark-registry) (default-benchmark-config))]
      [(config)
       (pcheck ([benchmark-config? config])
               (benchmark-run (current-benchmark-registry) config))]
      [(source config)
       (pcheck ([benchmark-config? config])
               (let* ([benchmarks
                       (cond
                        [(benchmark-registry? source)
                         (benchmark-select source (benchmark-config-filter config))]
                        [(benchmark? source) (list source)]
                        [(benchmark-suite? source)
                         (benchmark-run-suite source config)]
                        [(list? source) source]
                        [else (errorf 'benchmark-run "invalid benchmark source: ~s" source)])])
                 (if (and (pair? benchmarks) (benchmark-result? (car benchmarks)))
                     benchmarks
                     (let ([results
                            (apply append
                                   (map (lambda (benchmark)
                                          (map (lambda (expanded)
                                                 (benchmark-run-one expanded config))
                                               (benchmark-expand benchmark config)))
                                        benchmarks))])
                       (when (and (benchmark-config-reporter config)
                                  (benchmark-config-output config))
                         (benchmark-report results
                                           (benchmark-config-reporter config)
                                           (benchmark-config-output config)))
                       results))))]))

  #|proc:benchmark-summarize
The `benchmark-summarize` procedure computes CPU nanosecond, real nanosecond,
and allocated-byte per-iteration summary alists from measured `samples`.
|#
  (define benchmark-summarize
    (lambda (samples)
      (pcheck ([list? samples])
              `((cpu-ns . ,($summary-values (map $sample-cpu-ns samples)))
                (real-ns . ,($summary-values (map $sample-real-ns samples)))
                (bytes . ,($summary-values (map $sample-bytes samples)))))))

  #|proc:benchmark-pause-timing
The `benchmark-pause-timing` procedure marks timing as paused in `state`.
Subtraction of paused deltas is reserved for the v2 measurement backend.
|#
  (define benchmark-pause-timing
    (lambda (state)
      (pcheck ([benchmark-state? state])
              ($benchmark-state-timing-paused?-set! state #t))))

  #|proc:benchmark-resume-timing
The `benchmark-resume-timing` procedure marks timing as resumed in `state`.
Subtraction of paused deltas is reserved for the v2 measurement backend.
|#
  (define benchmark-resume-timing
    (lambda (state)
      (pcheck ([benchmark-state? state])
              ($benchmark-state-timing-paused?-set! state #f))))

  #|proc:benchmark-do-not-optimize
The `benchmark-do-not-optimize` procedure returns `value` through Chez's
`black-box` optimization barrier so benchmark inputs or results appear to be
consumed by an unknown context.
|#
  (define benchmark-do-not-optimize
    (lambda (value)
      (black-box value)))

  #|proc:benchmark-clobber-memory
The `benchmark-clobber-memory` procedure is a weak v1 mutation visibility
barrier. It returns `value` through `benchmark-do-not-optimize`; it does not
flush CPU caches or enforce hardware memory ordering.
|#
  (define benchmark-clobber-memory
    (lambda (value)
      (benchmark-do-not-optimize value)))

  #|proc:benchmark-report
The `benchmark-report` procedure writes existing benchmark `results` through
`reporter` to output port `out`.
|#
  (define benchmark-report
    (lambda (results reporter out)
      (pcheck ([list? results] [benchmark-reporter? reporter] [output-port? out])
              (($benchmark-reporter-start reporter) results out)
              (for-each (lambda (result)
                          (($benchmark-reporter-result reporter) result out))
                        results)
              (($benchmark-reporter-finish reporter) results out)))))
