(import (chezpp))

(collect)
(define n 10000)
(define before (sstats-bytes (statistics)))
(define ts (make-treeset fx= fx<))
(let loop ([i 0])
  (unless (= i n)
    (treeset-add! ts i)
    (loop (+ i 1))))
(printf "treeset bytes total=~a per-element=~a\n"
        (- (sstats-bytes (statistics)) before)
        (/ (- (sstats-bytes (statistics)) before) n))
