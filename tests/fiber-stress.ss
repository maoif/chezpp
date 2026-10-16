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
(define fiber-stress-yield-interval 4096)

(define fiber-stress-sieve-limit 100000)
(define fiber-stress-mandelbrot-width 320)
(define fiber-stress-mandelbrot-height 240)
(define fiber-stress-mandelbrot-iterations 128)
(define fiber-stress-sort-items-per-worker 4096)
(define fiber-stress-monte-carlo-samples-per-worker 100000)

(define fiber-stress-watchdog-milliseconds 120000)

(define fiber-stress-yield-if-needed
  (lambda (index)
    ;; Yield often enough to let the scheduler move runnable fibers between
    ;; worker threads, while keeping the inner loop mostly computation-only.
    (when (and (positive? index)
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

(mat fiber-stress-segmented-sieve
     (let ([result
            (run-fibers
             (lambda ()
               (fiber-stress-watchdog)
               (let* ([limit fiber-stress-sieve-limit]
                      [workers fiber-stress-workers]
                      [primes (fiber-stress-base-primes
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
                            (let* ([low (+ 2 (* segment segment-size))]
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
                                  (+ sum (cdr part)))))))))])
       ;; These are the known count and sum of primes below 100,000. If the
       ;; configurable limit changes, update this expected pair accordingly.
       (and (= (car result) 9592)
            (= (cdr result) 454396537))))

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

(mat fiber-stress-mandelbrot
     (let ([result
            (run-fibers
             (lambda ()
               (fiber-stress-watchdog)
               (let ([results (make-channel)]
                     [width fiber-stress-mandelbrot-width]
                     [height fiber-stress-mandelbrot-height]
                     [iterations fiber-stress-mandelbrot-iterations])
                 ;; A worker renders complete rows in a strided tile. This
                 ;; avoids shared pixels and gives every worker similar work.
                 (do ([worker 0 (add1 worker)])
                     ((= worker fiber-stress-workers))
                   (spawn-fiber
                    (lambda ()
                      (let rows ([y worker] [checksum 0] [pixels 0])
                        (if (>= y height)
                            (channel-put! results (cons pixels checksum))
                            (let columns ([x 0] [row-sum 0])
                              (if (= x width)
                                  (begin
                                    (fiber-stress-yield-if-needed y)
                                    (rows (+ y fiber-stress-workers)
                                          (+ checksum row-sum)
                                          (+ pixels width)))
                                  (columns (add1 x)
                                           (+ row-sum
                                              (fiber-stress-mandelbrot-pixel
                                               x y width height iterations))))))))))
                 (let collect ([remaining fiber-stress-workers] [pixels 0] [checksum 0])
                   (if (= remaining 0)
                       (cons pixels checksum)
                       (let ([part (channel-get results)])
                         (collect (sub1 remaining)
                                  (+ pixels (car part))
                                  (+ checksum (cdr part)))))))))])
       ;; A nonzero checksum and an exact pixel count catch empty or truncated
       ;; tiles while leaving width, height, and iteration count configurable.
       (and (= (car result)
               (* fiber-stress-mandelbrot-width
                  fiber-stress-mandelbrot-height))
            (> (cdr result) 0))))

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
            (run-fibers
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

(mat fiber-stress-monte-carlo-pi
     (let ([hits
            (run-fibers
             (lambda ()
               (fiber-stress-watchdog)
               (let ([results (make-channel)]
                     [samples fiber-stress-monte-carlo-samples-per-worker]
                     [workers fiber-stress-workers])
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
                                (+ total (channel-get results))))))))])
       ;; The exact stream is deterministic; this broad bound checks that the
       ;; result has the expected pi/4 geometry without relying on randomness.
       (let ([samples (* fiber-stress-workers
                         fiber-stress-monte-carlo-samples-per-worker)])
         (and (> hits (quotient (* samples 70) 100))
              (< hits (quotient (* samples 82) 100))))))
