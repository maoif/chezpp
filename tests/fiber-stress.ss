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
;; sort handles 262144 values per worker in four runs, and Monte Carlo samples
;; 100 million points per worker. Full runtime includes the scalar checks below.
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
;; A positive number of runs with earlier results live exercises GC between runs.
;; Vector chunks avoid the long GC pauses that mixed-owner linked lists can cause.
(define fiber-stress-sort-repetitions 4)
(define fiber-stress-monte-carlo-samples-per-worker 100000000)

;; Allow five minutes per parallel run on slower machines. GC pauses delay the
;; fiber watchdog too; use an external process timeout when investigating stalls.
;; Scalar validation runs on the calling thread, outside this watchdog.
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
    ;; Chez's parallel GC assigns segments to the threads that allocated them.
    ;; Relinking pairs from different workers with `merge!` creates long chains
    ;; that GC traverses by repeatedly waking another segment owner. A contiguous
    ;; vector avoids that dependent traversal and allocates one object per merge.
    ;; Source chunks remain read-only; only this fiber writes `merged`.
    (let* ([left-size (vector-length left)] [right-size (vector-length right)]
           [merged (make-vector (+ left-size right-size))])
      (let merge ([left-index 0] [right-index 0] [output-index 0])
        (cond
         [(fx= left-index left-size)
          (vector-copy! right right-index merged output-index
                        (fx- right-size right-index))]
         [(fx= right-index right-size)
          (vector-copy! left left-index merged output-index
                        (fx- left-size left-index))]
         [(< (vector-ref left left-index) (vector-ref right right-index))
          (vector-set! merged output-index (vector-ref left left-index))
          (merge (fx1+ left-index) right-index (fx1+ output-index))]
         [else
          (vector-set! merged output-index (vector-ref right right-index))
          (merge left-index (fx1+ right-index) (fx1+ output-index))]))
      merged)))

(define fiber-stress-merge-round
  (lambda (sorted-chunks)
    (if (or (null? sorted-chunks) (null? (cdr sorted-chunks)))
        (if (null? sorted-chunks) '#() (car sorted-chunks))
        (let ([outputs '()])
          ;; Each pair reads two complete chunks and publishes one private vector.
          ;; Later rounds have fewer pairs: the final merge still uses only one fiber.
          (let loop ([rest sorted-chunks])
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
     ;; Merging preserves every duplicate and leaves both source chunks intact.
     (let ([left (vector 1 2 9)] [right (vector 2 2 3)])
       (and (equal? '#(1 2 2 2 3 9) (fiber-stress-merge-two left right))
            (equal? '#(1 2 9) left)
            (equal? '#(2 2 3) right)))

     (equal? '#(1 3) (fiber-stress-merge-two '#() '#(1 3)))

     (equal? '#(1 3) (fiber-stress-merge-two '#(1 3) '#()))

     (equal? '#() (fiber-stress-merge-two '#() '#()))

     (fiber-stress-run
      (lambda ()
        ;; An empty tree, a single chunk and an odd tree width must all work.
        (and (equal? '#() (fiber-stress-merge-round '()))
             (equal? '#(1 3) (fiber-stress-merge-round (list '#(1 3))))
             (equal? '#(1 2 2 3 9)
                     (fiber-stress-merge-round
                      (list '#(1 9) '#() '#(2 2 3))))))))

(define fiber-stress-sort
  (lambda (input workers items-per-worker)
    (fiber-stress-run
     (lambda ()
       (fiber-stress-watchdog)
       (let ([results (make-channel)])
         (do ([worker 0 (fx1+ worker)]) ((fx= worker workers))
           (spawn-fiber
            (lambda ()
              ;; Copy the assigned range once. Sorting mutates only this fiber's
              ;; chunk, while `input` stays available for independent validation.
              (let ([chunk (make-vector items-per-worker)])
                (vector-copy! input (* worker items-per-worker) chunk 0 items-per-worker)
                (vector-sort! < chunk)
                (channel-put! results chunk)))))
         (let collect ([remaining workers] [chunks '()])
           (if (fxzero? remaining)
               (fiber-stress-merge-round chunks)
               (collect (fx1- remaining) (cons (channel-get results) chunks)))))))))

(mat fiber-stress-sort-small-partitions
     ;; Empty chunks, one worker and odd merge trees must preserve the input and
     ;; match a separate list sort, including duplicate values and partial tails.
     (for-all
      (lambda (workers)
        (for-all
         (lambda (items-per-worker)
           (let* ([original (list->vector
                             (map (lambda (index) (modulo (- 17 index) 7))
                                  (iota (* workers items-per-worker))))]
                  [input (vector-copy original)])
             (and (equal? (list->vector (sort < (vector->list original)))
                          (fiber-stress-sort input workers items-per-worker))
                  (equal? original input))))
         '(0 1 7)))
      '(1 2 3 5)))

(mat fiber-stress-parallel-merge-sort
     (let ([input (make-vector (* fiber-stress-workers fiber-stress-sort-items-per-worker))])
       (assert (positive-natural? fiber-stress-sort-repetitions))
       (let fill ([index 0] [state 17])
         (when (fx< index (vector-length input))
           (vector-set! input index (modulo state 1000000))
           (fill (fx1+ index) (fiber-stress-lcg-step state))))
       ;; A scalar sort of the full input checks partitioning and our merge
       ;; independently. Compare every element: count and sum can miss corruption.
       ;; Compute the reference before any parallel run, so scalar validation
       ;; cannot hide GC problems by normalizing the preceding run's allocations.
       (let* ([expected (list->vector (sort < (vector->list input)))]
              [first (fiber-stress-sort input fiber-stress-workers
                                        fiber-stress-sort-items-per-worker)])
         (and (equal? expected first)
              (let repeat ([remaining (sub1 fiber-stress-sort-repetitions)])
                (or (zero? remaining)
                    (let ([result (fiber-stress-sort input fiber-stress-workers
                                                    fiber-stress-sort-items-per-worker)])
                      ;; Keep the first output alive while later contexts allocate
                      ;; and collect; all repetitions must preserve the exact data.
                      (and (equal? expected result)
                           (equal? first result)
                           (repeat (sub1 remaining))))))))))

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
