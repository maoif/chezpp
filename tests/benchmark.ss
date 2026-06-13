(import (chezpp))

(define (benchmark-test-config)
  (benchmark-config-with (default-benchmark-config)
    '((warmup . 0)
      (samples . 2)
      (min-time . 0)
      (max-iterations . 1)
      (reporter . #f)
      (output . #f))))

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

