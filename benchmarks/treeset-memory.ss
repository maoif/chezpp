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

(collect)
(define fx-before (sstats-bytes (statistics)))
(define fx-ts (make-fixnum-treeset fx= fx<))
(let loop ([i 0])
  (unless (= i n)
    (treeset-add! fx-ts i)
    (loop (+ i 1))))
(printf "fixnum-treeset bytes total=~a per-element=~a\n"
        (- (sstats-bytes (statistics)) fx-before)
        (/ (- (sstats-bytes (statistics)) fx-before) n))
