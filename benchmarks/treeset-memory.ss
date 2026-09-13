(import (chezpp))

;; Run this same script against the baseline and optimized libraries.
;; Compile the procedures before the measurement; use retained graph size as the primary metric.
;; Subtract an empty set's graph so shared comparator code is not charged per entry.
(define measure-set
  (lambda (count)
    (let* ([empty (make-treeset fx= fx<)]
           [set (make-treeset fx= fx<)]
           [empty-size (compute-size empty)])
      (collect)
      (let ([before (statistics)])
        (do ([item 0 (fx1+ item)]) ((fx= item count))
          (treeset-add! set item))
        (let ([allocated (sstats-bytes (sstats-difference (statistics) before))])
          (collect)
          (let ([retained (- (compute-size set) empty-size)])
            (unless (= count (treeset-size set)) (error 'memory "incorrect set size"))
            (printf "~a retained=~a bytes/item=~a allocated=~a\n"
                    count retained (/ retained count) allocated)))))))

(printf "~a ~a\n" (scheme-version) (machine-type))
(for-each measure-set '(1000 10000 100000))
