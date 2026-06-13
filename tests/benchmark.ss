(import (chezpp))

(define (benchmark-test-config)
  (benchmark-config-with (default-benchmark-config)
    '((warmup . 0)
      (samples . 2)
      (min-time . 0)
      (max-iterations . 1)
      (reporter . #f)
      (output . #f))))

(define (benchmark-temp-file)
  (format "benchmark-baseline-~a-~a.dat" (random 1000000) (time-nanosecond (current-time))))

(define (benchmark-string-contains? text needle)
  (let ([text-len (string-length text)]
        [needle-len (string-length needle)])
    (let loop ([i 0])
      (cond
       [(> (+ i needle-len) text-len) #f]
       [(string=? needle (substring text i (+ i needle-len))) #t]
       [else (loop (+ i 1))]))))

(benchmark-clear-registry! (current-benchmark-registry))

(define-benchmark bench-basic
  (:args [n 1] [n 2]
   :warmup 0
   :samples 2
   :min-time 0
   :max-iterations 1
   :unit nanosecond)
  (lambda (state n)
    (benchmark-state-counter-add! state 'calls 1)
    (benchmark-do-not-optimize n)))

(define-benchmark-fixture bench-fixture
  (:setup (lambda (state)
            (vector (benchmark-state-arg state 'n)))
   :teardown (lambda (state value)
               (vector-set! value 0 'closed))))

(define-fixture-benchmark bench-with-fixture bench-fixture
  (:args [n 3]
   :warmup 0
   :samples 1
   :min-time 0
   :max-iterations 1)
  (lambda (state n value)
    (benchmark-do-not-optimize (vector-ref value 0))))

(define-benchmark-template bench-template (kind make-seq ref)
  (:args [n 4]
   :warmup 0
   :samples 1
   :min-time 0
   :max-iterations 1)
  (lambda (state n)
    (let ([seq (make-seq n 9)])
      (benchmark-do-not-optimize (ref seq 0)))))

(instantiate-benchmark-template bench-template
  ([vector make-vector vector-ref]))

(define-benchmark bench-error
  (:args [n 1]
   :warmup 0
   :samples 1
   :min-time 0
   :max-iterations 1
   :reporter #f)
  (lambda (state n)
    (error 'bench-error "expected benchmark body error")))

(define-benchmark bench-product
  (:arg-product [n 1 2]
                [m 10 20]
   :warmup 0
   :samples 1
   :min-time 0
   :max-iterations 1)
  (lambda (state n m)
    (benchmark-do-not-optimize (+ n m))))

(define-benchmark bench-paused
  (:args [n 1]
   :warmup 0
   :samples 1
   :min-time 0
   :max-iterations 1)
  (lambda (state n)
    (benchmark-pause-timing state)
    (let loop ([i 0])
      (when (< i 50000)
        (benchmark-do-not-optimize i)
        (loop (+ i 1))))
    (benchmark-resume-timing state)
    (benchmark-do-not-optimize n)))

(define-benchmark-suite bench-suite
  (:suite-setup (lambda (state) 'suite-open)
   :suite-teardown (lambda (state value) (void)))
  bench-basic
  bench-with-fixture)

(mat benchmark-records
     (benchmark? (make-benchmark 'manual (lambda (state) (void)) '()))
     (benchmark-config? (default-benchmark-config))
     (benchmark-reporter? (benchmark-text-reporter))
     (benchmark-fixture? bench-fixture)
     (benchmark-suite? bench-suite)
     (let ([state (benchmark-state 'state-test #f '((n . 7)) '() 0 1 0)])
       (and (benchmark-state? state)
            (eq? (benchmark-state-name state) 'state-test)
            (= (benchmark-state-arg state 'n) 7)
            (not (benchmark-state-arg state 'missing #f))))
     ;; unknown argument names are rejected
     (error? (benchmark-state-arg (benchmark-state 'state-test #f '() '() 0 1 0) 'missing))
     ;; public constructors validate boundary argument types
     (error? (make-benchmark "bad-name" (lambda (state) (void)) '())))

(mat benchmark-registry-and-expansion
     (let ([names (map benchmark-name
                       (benchmark-registry-benchmarks (current-benchmark-registry)))])
       (and (memq 'bench-basic names)
            (memq 'bench-with-fixture names)
            (if (memq 'bench-template/vector names) #t #f)))
     (= (length (benchmark-expand bench-basic (benchmark-test-config))) 2)
     (= (length (benchmark-expand bench-product (benchmark-test-config))) 4)
     (equal? (map benchmark-state-args
                  (map car (benchmark-expand bench-product (benchmark-test-config))))
             '(((n . 1) (m . 10))
               ((n . 1) (m . 20))
               ((n . 2) (m . 10))
               ((n . 2) (m . 20))))
     (let* ([selected (benchmark-select (current-benchmark-registry) "bench-basic")]
            [names (map benchmark-name selected)])
       (equal? names '(bench-basic))))

(mat benchmark-runner
     (let ([results (benchmark-run (list bench-basic) (benchmark-test-config))])
       (and (= (length results) 2)
            (andmap benchmark-result? results)
            (andmap (lambda (result)
                      (and (= (length (benchmark-result-samples result)) 2)
                           (not (benchmark-result-error result))
                           (assq 'cpu-ns (benchmark-result-summary result))
                           (assq 'real-ns (benchmark-result-summary result))
                           (assq 'bytes (benchmark-result-summary result))
                           (>= (cdr (assq 'calls (benchmark-result-counters result))) 1)
                           #t))
                    results)))
     (let ([results (benchmark-run (list bench-with-fixture) (benchmark-test-config))])
       (and (= (length results) 1)
            (not (benchmark-result-error (car results)))))
     (let ([results (benchmark-run (list bench-error) (benchmark-test-config))])
       (and (= (length results) 1)
            (if (benchmark-result-error (car results)) #t #f))))

(mat benchmark-reporters
     (let ([out (open-output-string)])
       (benchmark-report (benchmark-run (list bench-basic) (benchmark-test-config))
                         (benchmark-text-reporter)
                         out)
       (let ([text (get-output-string out)])
         (and (string? text)
              (> (string-length text) 0))))
     (let ([out (open-output-string)])
       (benchmark-report (benchmark-run (list bench-basic) (benchmark-test-config))
                         (benchmark-datum-reporter)
                         out)
       (let ([text (get-output-string out)])
         (and (string? text)
              (> (string-length text) 0)))))

(mat benchmark-v2-reporters
     (let ([out (open-output-string)])
       (benchmark-report (benchmark-run (list bench-basic) (benchmark-test-config))
                         (benchmark-csv-reporter)
                         out)
       (let ([text (get-output-string out)])
         (and (string? text)
              (benchmark-string-contains? text "name,args,template_args")
              (benchmark-string-contains? text "bench-basic"))))
     (let ([out (open-output-string)])
       (benchmark-report (benchmark-run (list bench-basic) (benchmark-test-config))
                         (benchmark-json-reporter)
                         out)
       (let ([text (get-output-string out)])
         (and (string? text)
              (benchmark-string-contains? text "\"results\"")
              (benchmark-string-contains? text "\"bench-basic\""))))
     (let ([reporter (benchmark-rich-reporter)])
       (benchmark-reporter? reporter)))

(mat benchmark-v2-baselines-and-comparison
     (let* ([results (benchmark-run (list bench-basic) (benchmark-test-config))]
            [path (benchmark-temp-file)])
       (dynamic-wind
         (lambda () (void))
         (lambda ()
           (benchmark-save-baseline results path)
           (let ([loaded (benchmark-load-baseline path)])
             (and (= (length loaded) (length results))
                  (equal? (map benchmark-result-name loaded)
                          (map benchmark-result-name results)))))
         (lambda ()
           (when (file-exists? path)
             (delete-file path)))))
     (let* ([base (benchmark-run (list bench-basic) (benchmark-test-config))]
            [current (benchmark-run (list bench-basic) (benchmark-test-config))]
            [comparisons (benchmark-compare-results base current
                           '((metric . real-ns)
                             (threshold-percent . 1000000)
                             (threshold-absolute . 1000000000)
                             (noise-threshold-percent . 1000000)))])
       (and (= (length comparisons) (length current))
            (andmap benchmark-comparison? comparisons)
            (andmap (lambda (comparison)
                      (and (benchmark-comparison-name comparison)
                           (number? (benchmark-comparison-percent-difference comparison))
                           (not (benchmark-comparison-regression? comparison))))
                    comparisons)))
     (= (benchmark-percent-difference 100 110) 10)
     (= (benchmark-absolute-difference 100 90) -10))

(mat benchmark-v2-summaries-and-pauses
     (let* ([results (benchmark-run (list bench-basic) (benchmark-test-config))]
            [summary (benchmark-result-summary (car results))]
            [real (cdr (assq 'real-ns summary))])
       (and (assq 'confidence-interval real)
            (= (length (cdr (assq 'confidence-interval real))) 2)
            #t))
     (let* ([results (benchmark-run (list bench-paused) (benchmark-test-config))]
            [sample (car (benchmark-result-samples (car results)))])
       (and (benchmark-result? (car results))
            (not (benchmark-result-error (car results)))
            (if (benchmark-sample-sstats sample) #t #f))))
