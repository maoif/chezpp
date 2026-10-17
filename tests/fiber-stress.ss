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
(define fiber-stress-yield-interval 4096)

;; All workloads use the same scheduler controls. Set the quantum to 3000 to
;; exercise frequent preemption, or reduce `fiber-stress-workers` for scaling runs.
;; A zero yield interval disables cooperative yields; automatic preemption stays on.
(define fiber-stress-run
  (lambda (thunk)
    (run-fibers thunk fiber-stress-workers fiber-stress-quantum)))

;; Larger problems keep computation running long enough to observe CPU use.
;; The sieve's reference values are for primes strictly below its limit;
;; update both reference values when changing the limit.
(define fiber-stress-sieve-limit 10000000)
(define fiber-stress-sieve-expected-count 664579)
(define fiber-stress-sieve-expected-sum 3203324994356)
(define fiber-stress-mandelbrot-width 2560)
(define fiber-stress-mandelbrot-height 1920)
(define fiber-stress-mandelbrot-iterations 1024)
(define fiber-stress-sort-items-per-worker 65536)
(define fiber-stress-monte-carlo-samples-per-worker 20000000)

(define fiber-stress-watchdog-milliseconds 120000)

;; Each stress result is checked exactly. Mandelbrot and Monte Carlo also run
;; separate scalar references, so changing dimensions, sample count, or worker
;; count needs no new checksum. Reference work adds time after the parallel phase.

(define fiber-stress-yield-if-needed
  (lambda (index)
    ;; Yield often enough to let the scheduler move runnable fibers between
    ;; worker threads, while keeping the inner loop mostly computation-only.
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
  (lambda (low high primes)
    (let ([marked (make-vector (- high low) #t)])
      (let prime-loop ([prime-index 0])
        (when (< prime-index (vector-length primes))
          (let* ([prime (vector-ref primes prime-index)]
                 [first (max (* prime prime)
                             (* prime (fiber-stress-ceiling-div low prime)))])
            (let mark ([multiple first])
              (when (< multiple high)
                (vector-set! marked (- multiple low) #f)
                (mark (+ multiple prime)))))
          (prime-loop (add1 prime-index))))
      (let count ([index 0] [number low] [total 0] [sum 0])
        (if (= number high)
            (cons total sum)
            (begin
              (fiber-stress-yield-if-needed index)
              (if (vector-ref marked index)
                  (count (add1 index) (add1 number)
                         (add1 total) (+ sum number))
                  (count (add1 index) (add1 number) total sum))))))))

(define fiber-stress-run-sieve
  (lambda (limit workers)
    (fiber-stress-run
     (lambda ()
       (fiber-stress-watchdog)
       (let* ([primes (fiber-stress-base-primes
                       (inexact->exact (floor (sqrt limit))))]
              [segment-count (* workers 4)]
              [segment-size
               (max 1 (fiber-stress-ceiling-div (- limit 1)
                                                 segment-count))]
              [results (make-channel)])
         ;; Each worker owns every Nth segment, so large ranges stay
         ;; balanced even when one segment contains more composites.
         (do ([worker 0 (add1 worker)])
             ((= worker workers))
           (spawn-fiber
            (lambda ()
              (let loop ([segment worker] [total 0] [sum 0])
                (if (>= segment segment-count)
                    (channel-put! results (cons total sum))
                    ;; Ceiling division pads the final segments. Clamp both
                    ;; ends so an empty tail is [limit, limit), never reversed.
                    (let* ([low (min limit (+ 2 (* segment segment-size)))]
                           [high (min limit (+ low segment-size))]
                           [part (fiber-stress-sieve-segment
                                   low high primes)])
                      (loop (+ segment workers)
                            (+ total (car part))
                            (+ sum (cdr part)))))))))
         (let collect ([remaining workers] [total 0] [sum 0])
           (if (= remaining 0)
               (cons total sum)
               (let ([part (channel-get results)])
                 (collect (sub1 remaining)
                          (+ total (car part))
                          (+ sum (cdr part)))))))))))

(mat fiber-stress-sieve-small-partitions
     ;; Small ranges must work even when there are more padded segments than numbers.
     (equal? '(25 . 1060) (fiber-stress-run-sieve 100 16))

     (equal? '(0 . 0) (fiber-stress-run-sieve 2 16)))

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
  (lambda (state)
    (modulo (+ (* state 1664525) 1013904223) 4294967296)))

(define fiber-stress-merge-two
  (lambda (left right)
    (let loop ([left left] [right right] [out '()])
      (cond
       [(null? left) (append (reverse out) right)]
       [(null? right) (append (reverse out) left)]
       [(<= (car left) (car right))
        (loop (cdr left) right (cons (car left) out))]
       [else
        (loop left (cdr right) (cons (car right) out))]))))

(define fiber-stress-merge-round
  (lambda (sorted-lists)
    (if (null? (cdr sorted-lists))
        (if (null? sorted-lists) '() (car sorted-lists))
        (let ([outputs '()])
          ;; Each merge pair is independent, so the merge tree also runs as
          ;; fibers instead of making only the initial sorts parallel.
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

(mat fiber-stress-parallel-merge-sort
     (let ([result
            (fiber-stress-run
             (lambda ()
               (fiber-stress-watchdog)
               (let* ([workers fiber-stress-workers]
                      [items-per-worker fiber-stress-sort-items-per-worker]
                      [total (* workers items-per-worker)]
                      [input (make-vector total)]
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
       ;; Verify ordering, cardinality, and conservation of the generated data.
       (let loop ([values result] [previous #f] [count 0] [sum 0])
         (if (null? values)
             (and (= count (* fiber-stress-workers
                              fiber-stress-sort-items-per-worker))
                  (= sum
                     (let fill ([index 0] [state 17] [total 0])
                       (if (= index count)
                           total
                           (fill (add1 index)
                                 (fiber-stress-lcg-step state)
                                 (+ total (modulo state 1000000)))))))
             (and (or (not previous) (<= previous (car values)))
                  (loop (cdr values) (car values) (add1 count)
                        (+ sum (car values))))))))

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
