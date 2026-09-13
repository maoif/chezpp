(import (chezpp))

(define measure
  (lambda (name maker add n)
    (let ([start (current-time 'time-monotonic)] [tm (maker)])
      (let loop ([i 0])
        (if (= i n)
            (let ([stop (current-time 'time-monotonic)])
              (printf "~a ~a ns\n" name
                      (+ (* (- (time-second stop) (time-second start)) 1000000000)
                         (- (time-nanosecond stop) (time-nanosecond start)))))
            (begin (add tm i) (loop (+ i 1))))))))

(measure 'generic-treemap-1000
         (lambda () (make-treemap fx= fx<))
         (lambda (tm i) (treemap-set! tm i i)) 1000)
(measure 'fixnum-treemap-1000
         (lambda () (make-fixnum-treemap fx= fx<))
         (lambda (tm i) (treemap-set! tm i i)) 1000)
(measure 'generic-treemap-10000
         (lambda () (make-treemap fx= fx<))
         (lambda (tm i) (treemap-set! tm i i)) 10000)
(measure 'fixnum-treemap-10000
         (lambda () (make-fixnum-treemap fx= fx<))
         (lambda (tm i) (treemap-set! tm i i)) 10000)
(measure 'generic-treemap-100000
         (lambda () (make-treemap fx= fx<))
         (lambda (tm i) (treemap-set! tm i i)) 100000)
(measure 'fixnum-treemap-100000
         (lambda () (make-fixnum-treemap fx= fx<))
         (lambda (tm i) (treemap-set! tm i i)) 100000)
