(import (chezpp))

(define measure
  (lambda (name maker add)
    (let ([start (current-time 'time-monotonic)] [tm (maker)])
      (let loop ([i 0])
        (if (= i 10000)
            (let ([stop (current-time 'time-monotonic)])
              (printf "~a ~a ns\n" name
                      (+ (* (- (time-second stop) (time-second start)) 1000000000)
                         (- (time-nanosecond stop) (time-nanosecond start)))))
            (begin (add tm i) (loop (+ i 1))))))))

(measure 'generic-treemap
         (lambda () (make-treemap fx= fx<))
         (lambda (tm i) (treemap-set! tm i i)))
(measure 'fixnum-treemap
         (lambda () (make-fixnum-treemap fx= fx<))
         (lambda (tm i) (treemap-set! tm i i)))
