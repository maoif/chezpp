(import (chezpp)
        (chezpp concurrency fiber)
        (chezpp system))

;;;;===----------------------------------------------------------------------===
;;;; Configurable CPU stress workloads
;;;;===----------------------------------------------------------------------===

;; These values are deliberately near the top of the file so a developer can
;; make a local run shorter or longer without changing any algorithm below.
;; The default worker count matches the scheduler's logical CPU count.
(define fiber-stress-workers (cpu-count))
(define fiber-stress-quantum 300000)
;; CPU kernels already preempt automatically. Extra cooperative suspensions
;; allocate continuations and contend on runtime locks without adding useful work.
;; Use a positive interval (for example 4096) to stress explicit handoffs instead.
(define fiber-stress-yield-interval 0)

;; All workloads use the same scheduler controls. Set the quantum to 3000 to
;; exercise frequent preemption, or reduce `fiber-stress-workers` for scaling runs.
;; A zero yield interval disables cooperative yields; automatic preemption stays on.
(define fiber-stress-run
  (lambda (thunk)
    (run-fibers thunk fiber-stress-workers fiber-stress-quantum)))

;; Larger defaults keep the optimized scheduler busy: sieve checks 100 million
;; numbers, Mandelbrot renders 19.7 million pixels with up to 2048 iterations,
;; sort handles 262144 values per worker, and Monte Carlo samples 100 million
;; points per worker. Full runtime includes the scalar checks below.
;; The sieve's reference values are for primes strictly below its limit;
;; update both reference values when changing the limit. The 100-million count
;; and sum were also checked with an independent scalar byte-array sieve.
;; Each sieve fiber owns one reusable byte buffer. Its size stays bounded as the
;; prime limit grows; 64 KiB reduces memory traffic and fits comfortably in cache.
(define fiber-stress-sieve-segment-size 65536)
(define fiber-stress-sieve-limit 100000000)
(define fiber-stress-sieve-expected-count 5761455)
(define fiber-stress-sieve-expected-sum 279209790387276)
(define fiber-stress-mandelbrot-width 5120)
(define fiber-stress-mandelbrot-height 3840)
(define fiber-stress-mandelbrot-iterations 2048)
(define fiber-stress-sort-items-per-worker 262144)
(define fiber-stress-monte-carlo-samples-per-worker 100000000)

;; Allow five minutes per parallel workload on slower machines. The watchdog
;; bounds fiber work; scalar validation runs afterward on the calling thread.
(define fiber-stress-watchdog-milliseconds 300000)

;; Each stress result is checked exactly. Mandelbrot and Monte Carlo also run
;; separate scalar references, so changing dimensions, sample count, or worker
;; count needs no new checksum. Reference work adds time after the parallel phase.

(define fiber-stress-yield-if-needed
  (lambda (index)
    ;; Optional scheduling stress. Throughput runs leave this off; timer-driven
    ;; preemption still lets blocked peers and watchdog deadlines make progress.
    (when (and (positive? fiber-stress-yield-interval)
               (positive? index)
               (zero? (modulo index fiber-stress-yield-interval)))
      (fiber-yield))))

(define fiber-stress-ceiling-div
  (lambda (numerator denominator)
    (quotient (+ numerator denominator -1) denominator)))

(define fiber-stress-watchdog
  (lambda ()
    (spawn-fiber
     (lambda ()
       (fiber-milisleep fiber-stress-watchdog-milliseconds)
       (error 'fiber-stress "workload exceeded watchdog deadline")))))

;;;;===----------------------------------------------------------------------===
;;;; Segmented Sieve of Eratosthenes
;;;;===----------------------------------------------------------------------===

(define fiber-stress-base-primes
  (lambda (limit)
    (let ([marked (make-vector (add1 limit) #t)])
      (vector-set! marked 0 #f)
      (vector-set! marked 1 #f)
      (let mark ([prime 2])
        (when (<= (* prime prime) limit)
          (when (vector-ref marked prime)
            (let loop ([multiple (* prime prime)])
              (when (<= multiple limit)
                (vector-set! marked multiple #f)
                (loop (+ multiple prime)))))
          (mark (add1 prime))))
      (let collect ([value 2] [primes '()])
        (if (> value limit)
            (list->vector (reverse primes))
            (collect (add1 value)
                     (if (vector-ref marked value)
                         (cons value primes)
                         primes)))))))

(define fiber-stress-sieve-segment
  (lambda (low high primes marked)
    ;; `marked` belongs to this fiber and may contain marks from its previous range.
    ;; Reset the entire fixed buffer; the scan below reads only `[low, high)`.
    (bytevector-fill! marked 1)
    (let prime-loop ([prime-index 0])
      (when (< prime-index (vector-length primes))
        (let* ([prime (vector-ref primes prime-index)]
               [first (max (* prime prime)
                           (* prime (fiber-stress-ceiling-div low prime)))])
          (let mark ([multiple first])
            (when (< multiple high)
              (bytevector-u8-set! marked (- multiple low) 0)
              (mark (+ multiple prime)))))
        (prime-loop (add1 prime-index))))
    (let count ([index 0] [number low] [total 0] [sum 0])
      (if (= number high)
          (cons total sum)
          (begin
            (fiber-stress-yield-if-needed index)
            (if (= 1 (bytevector-u8-ref marked index))
                (count (add1 index) (add1 number)
                       (add1 total) (+ sum number))
                (count (add1 index) (add1 number) total sum)))))))

(define fiber-stress-run-sieve
  (case-lambda
    [(limit workers)
     (fiber-stress-run-sieve limit workers fiber-stress-sieve-segment-size)]
    [(limit workers segment-size)
     (fiber-stress-run
      (lambda ()
        (fiber-stress-watchdog)
        (let* ([primes (fiber-stress-base-primes
                        (inexact->exact (floor (sqrt limit))))]
               [segment-count (fiber-stress-ceiling-div (- limit 2) segment-size)]
               [results (make-channel)])
          ;; Every worker owns every Nth segment and reuses one byte array. Memory
          ;; use is O(workers * segment-size), not O(limit); fibers never share marks.
          (do ([worker 0 (add1 worker)])
              ((= worker workers))
            (spawn-fiber
             (lambda ()
               (let ([marked (make-bytevector segment-size)])
                 (let loop ([segment worker] [total 0] [sum 0])
                   (if (>= segment segment-count)
                       (channel-put! results (cons total sum))
                       (let* ([low (+ 2 (* segment segment-size))]
                              [high (min limit (+ low segment-size))]
                              [part (fiber-stress-sieve-segment low high primes marked)])
                         (loop (+ segment workers)
                               (+ total (car part))
                               (+ sum (cdr part))))))))))
          (let collect ([remaining workers] [total 0] [sum 0])
            (if (= remaining 0)
                (cons total sum)
                (let ([part (channel-get results)])
                  (collect (sub1 remaining)
                           (+ total (car part))
                           (+ sum (cdr part)))))))))]))

(mat fiber-stress-sieve-small-partitions
     ;; Small ranges must work even when most workers own no segment at all.
     (equal? '(25 . 1060) (fiber-stress-run-sieve 100 16))

     (equal? '(0 . 0) (fiber-stress-run-sieve 2 16)))

(mat fiber-stress-sieve-buffer-boundaries
     ;; Literal prime counts/sums check empty segments, exact boundaries and
     ;; partial tails. A reused buffer must clear composite marks from prior ranges.
     (for-all
      (lambda (segment-size)
        (for-all
         (lambda (case)
           (equal? (cdr case) (fiber-stress-run-sieve (car case) 4 segment-size)))
         '((2 . (0 . 0)) (3 . (1 . 2)) (7 . (3 . 10)) (8 . (4 . 17))
           (11 . (4 . 17)) (12 . (5 . 28)) (100 . (25 . 1060)))))
      '(1 7 16)))

(mat fiber-stress-segmented-sieve
     (equal? (cons fiber-stress-sieve-expected-count fiber-stress-sieve-expected-sum)
             (fiber-stress-run-sieve fiber-stress-sieve-limit fiber-stress-workers)))

;;;;===----------------------------------------------------------------------===
;;;; Mandelbrot tile rendering
;;;;===----------------------------------------------------------------------===

(define fiber-stress-mandelbrot-pixel
  (lambda (x y width height iterations)
    ;; Fixed-point arithmetic makes every worker produce identical pixels on
    ;; every run, without depending on floating-point rounding modes.
    (let* ([scale 1000000]
           [cx (- (quotient (* x 3 scale) width) (* 2 scale))]
           [cy (- (quotient (* y 2 scale) height) scale)])
      (let loop ([step 0] [zr 0] [zi 0])
        (if (or (= step iterations)
                (> (+ (quotient (* zr zr) scale)
                      (quotient (* zi zi) scale))
                   (* 4 scale)))
            step
            (loop (add1 step)
                  (+ (quotient (- (* zr zr) (* zi zi)) scale) cx)
                  (+ (quotient (* 2 (* zr zi)) scale) cy)))))))

(define fiber-stress-render-mandelbrot
  (lambda (width height iterations workers)
    (fiber-stress-run
     (lambda ()
       (fiber-stress-watchdog)
       (let ([results (make-channel)])
         ;; A worker renders complete rows in a strided tile. This
         ;; avoids shared pixels and gives every worker similar work.
         (do ([worker 0 (add1 worker)])
             ((= worker workers))
           (spawn-fiber
            (lambda ()
              (let rows ([y worker] [checksum 0] [pixels 0])
                (if (>= y height)
                    (channel-put! results (cons pixels checksum))
                    (let columns ([x 0] [row-sum 0])
                      (if (= x width)
                          (begin
                            (fiber-stress-yield-if-needed y)
                            (rows (+ y workers)
                                  (+ checksum row-sum)
                                  (+ pixels width)))
                          (columns (add1 x)
                                   (+ row-sum
                                      (fiber-stress-mandelbrot-pixel
                                       x y width height iterations))))))))))
         (let collect ([remaining workers] [pixels 0] [checksum 0])
           (if (= remaining 0)
               (cons pixels checksum)
               (let ([part (channel-get results)])
                 (collect (sub1 remaining)
                          (+ pixels (car part))
                          (+ checksum (cdr part)))))))))))

(define fiber-stress-mandelbrot-reference
  (lambda (width height iterations)
    ;; A scalar raster scan checks the strided worker partition independently.
    ;; Deliberately do not call the worker's pixel procedure: replacing that
    ;; procedure with a constant must fail, even with custom image dimensions.
    (let ([checksum 0])
      (do ([pixel 0 (add1 pixel)])
          ((= pixel (* width height)) checksum)
        (let ([real-coordinate (- (quotient (* (modulo pixel width) 3000000) width)
                                  2000000)]
              [imag-coordinate (- (quotient (* (quotient pixel width) 2000000) height)
                                  1000000)])
          (set! checksum
            (+ checksum
               (do ([step 0 (add1 step)]
                    [real 0 (+ (quotient (- (* real real) (* imag imag)) 1000000)
                               real-coordinate)]
                    [imag 0 (+ (quotient (* real imag) 500000)
                               imag-coordinate)])
                   ((or (= step iterations)
                        (> (+ (quotient (* real real) 1000000)
                              (quotient (* imag imag) 1000000))
                           4000000))
                    step)))))))))

(mat fiber-stress-mandelbrot-small-image
     ;; Independently calculated for the fixed-point grid, not the float variant.
     (equal? '(48 . 406) (fiber-stress-render-mandelbrot 8 6 16 2))

     (= 406 (fiber-stress-mandelbrot-reference 8 6 16)))

(mat fiber-stress-mandelbrot
     (let ([result (fiber-stress-render-mandelbrot
                    fiber-stress-mandelbrot-width fiber-stress-mandelbrot-height
                    fiber-stress-mandelbrot-iterations fiber-stress-workers)])
       (and (= (car result)
               (* fiber-stress-mandelbrot-width fiber-stress-mandelbrot-height))
            (= (cdr result)
               (fiber-stress-mandelbrot-reference
                fiber-stress-mandelbrot-width fiber-stress-mandelbrot-height
                fiber-stress-mandelbrot-iterations)))))

;;;;===----------------------------------------------------------------------===
;;;; Parallel merge sort
;;;;===----------------------------------------------------------------------===

(define fiber-stress-lcg-step
  ;; For a 32-bit state the largest product-plus-increment is 7149081450614098.
  ;; Select fixed-width arithmetic only if that bound fits this Chez machine.
  ;; The mask implements reduction modulo 2^32 without generic integer division.
  (if (<= 7149081450614098 (most-positive-fixnum))
      (lambda (state)
        (if (and (fixnum? state) (fx>= state 0) (fx<= state #xffffffff))
            (fxand (fx+ (fx* state 1664525) 1013904223) #xffffffff)
            ;; Configured seeds or direct helper calls may start outside 32 bits.
            ;; One generic step normalizes them before later fast-path steps.
            (modulo (+ (* state 1664525) 1013904223) 4294967296)))
      ;; Keep the same deterministic streams on machines with narrower fixnums.
      (lambda (state)
        (modulo (+ (* state 1664525) 1013904223) 4294967296))))

(mat fiber-stress-lcg-boundaries
     ;; These literal unsigned results also exercise the generic path for states
     ;; outside 32 bits; the worker fast path must preserve the same recurrence.
     (= 1013904223 (fiber-stress-lcg-step 0))

     (= 1042201148 (fiber-stress-lcg-step 17))

     (= 1012239698 (fiber-stress-lcg-step 4294967295))

     (= 1012239698 (fiber-stress-lcg-step -1))

     (= 1013904223 (fiber-stress-lcg-step 4294967296))

     (= 1013904223 (fiber-stress-lcg-step (expt 2 100))))

(define fiber-stress-merge-two
  (lambda (left right)
    ;; Each chunk/pair has one consumer, and its sender never uses it again.
    ;; `merge!` can relink those private pairs without a reversed accumulator,
    ;; copied prefix or new list nodes. This removes most merge-stage GC work.
    (merge! < left right)))

(define fiber-stress-merge-round
  (lambda (sorted-lists)
    (if (or (null? sorted-lists) (null? (cdr sorted-lists)))
        (if (null? sorted-lists) '() (car sorted-lists))
        (let ([outputs '()])
          ;; Each pair owns disjoint lists, so concurrent destructive merges are safe.
          ;; Later rounds have fewer pairs: the final merge still uses only one fiber.
          ;; Retaining the original input vector below permits a full independent check.
          (let loop ([rest sorted-lists])
            (unless (null? rest)
              (let ([output (make-channel)])
                (set! outputs (cons output outputs))
                (spawn-fiber
                 (lambda ()
                   (channel-put! output
                                 (if (null? (cdr rest))
                                     (car rest)
                                     (fiber-stress-merge-two
                                      (car rest) (cadr rest))))))
                (loop (if (null? (cdr rest)) '() (cddr rest))))))
          (let collect ([channels (reverse outputs)] [merged '()])
            (if (null? channels)
                (fiber-stress-merge-round (reverse merged))
                (collect (cdr channels)
                         (cons (channel-get (car channels)) merged))))))))

(mat fiber-stress-merge-boundaries
     ;; Fresh input lists are owned by the merge and consumed exactly once.
     ;; Duplicates, empty chunks and an odd tree width must conserve every item.
     (equal? '(1 2 2 2 3 9)
             (fiber-stress-merge-two (list 1 2 9) (list 2 2 3)))

     (equal? '(1 3) (fiber-stress-merge-two '() (list 1 3)))

     (equal? '(1 3) (fiber-stress-merge-two (list 1 3) '()))

     (fiber-stress-run
      (lambda ()
        (and (null? (fiber-stress-merge-round '()))
             (equal? '(1 2 2 3 9)
                     (fiber-stress-merge-round
                      (list (list 1 9) '() (list 2 2 3))))))))

(mat fiber-stress-parallel-merge-sort
     (let* ([input (make-vector (* fiber-stress-workers fiber-stress-sort-items-per-worker))]
            [result
            (fiber-stress-run
             (lambda ()
               (fiber-stress-watchdog)
               (let* ([workers fiber-stress-workers]
                      [items-per-worker fiber-stress-sort-items-per-worker]
                      [total (* workers items-per-worker)]
                      [results (make-channel)])
                 ;; Generate one deterministic input vector, then hand each
                 ;; worker a disjoint chunk to sort independently.
                 (let fill ([index 0] [state 17])
                   (when (< index total)
                     (vector-set! input index (modulo state 1000000))
                     (fill (add1 index) (fiber-stress-lcg-step state))))
                 (do ([worker 0 (add1 worker)])
                     ((= worker workers))
                   (spawn-fiber
                    (lambda ()
                      (let ([start (* worker items-per-worker)])
                        (channel-put!
                         results
                         (sort <
                               (let loop ([index start] [values '()])
                                 (if (= index (+ start items-per-worker))
                                     values
                                     (loop (add1 index)
                                           (cons (vector-ref input index) values))))))))))
                 (let collect ([remaining workers] [chunks '()])
                   (if (= remaining 0)
                       (fiber-stress-merge-round chunks)
                       (collect (sub1 remaining)
                                (cons (channel-get results) chunks)))))))])
       ;; Compare every element with a scalar sort of the original input. Sorted
       ;; output with the right count and sum can still lose or duplicate values.
       (equal? result (sort < (vector->list input)))))

;;;;===----------------------------------------------------------------------===
;;;; Deterministic Monte Carlo pi
;;;;===----------------------------------------------------------------------===

(define fiber-stress-monte-carlo
  (lambda (samples workers)
    (fiber-stress-run
     (lambda ()
       (fiber-stress-watchdog)
       (let ([results (make-channel)])
         ;; Each worker has a seed derived only from its index. The
         ;; result is reproducible, while the independent streams keep
         ;; this workload free of shared random-state contention.
         (do ([worker 0 (add1 worker)])
             ((= worker workers))
           (spawn-fiber
            (lambda ()
              (let sample ([index 0] [state (+ 17 (* worker 7919))]
                           [hits 0])
                (if (= index samples)
                    (channel-put! results hits)
                    (let* ([state-x (fiber-stress-lcg-step state)]
                           [state-y (fiber-stress-lcg-step state-x)]
                           [x (modulo state-x 1000000)]
                           [y (modulo state-y 1000000)])
                      (fiber-stress-yield-if-needed index)
                      (sample (add1 index) state-y
                              (if (<= (+ (* x x) (* y y))
                                     1000000000000)
                                  (add1 hits)
                                  hits))))))))
         (let collect ([remaining workers] [total 0])
           (if (= remaining 0)
               total
               (collect (sub1 remaining)
                        (+ total (channel-get results))))))))))

(define fiber-stress-monte-carlo-reference
  (lambda (samples workers)
    ;; Compute each seed's exact count without the worker's RNG helper or any
    ;; fiber/channel operations. Bit masking implements the unsigned 32-bit
    ;; recurrence independently of the worker's modulo implementation.
    (let ([total 0])
      (do ([worker 0 (add1 worker)])
          ((= worker workers) total)
        (let ([state (+ 17 (* worker 7919))])
          (do ([sample 0 (add1 sample)])
              ((= sample samples))
            (set! state (bitwise-and (+ (* 1664525 state) 1013904223) #xffffffff))
            (let ([x (remainder state 1000000)])
              (set! state (bitwise-and (+ (* 1664525 state) 1013904223) #xffffffff))
              (let ([y (remainder state 1000000)])
                (when (<= (* y y) (- 1000000000000 (* x x)))
                  (set! total (add1 total)))))))))))

(mat fiber-stress-monte-carlo-small-streams
     ;; Literal counts for seeds 17 and 7936; counts were calculated separately.
     (= 1556 (fiber-stress-monte-carlo 1000 2))

     (= 1556 (fiber-stress-monte-carlo-reference 1000 2)))

(mat fiber-stress-monte-carlo-pi
     (let ([hits (fiber-stress-monte-carlo
                  fiber-stress-monte-carlo-samples-per-worker fiber-stress-workers)])
       (= hits (fiber-stress-monte-carlo-reference
                fiber-stress-monte-carlo-samples-per-worker fiber-stress-workers))))
