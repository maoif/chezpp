;; Shared, throttled stderr display for the wget and SCP examples.
(define example-progress-enabled?
  (lambda (who ref)
    (let ([value (or (ref 'progress) "yes")])
      (unless (member value '("yes" "no"))
        (errorf who "--progress must be yes or no"))
      (string=? value "yes"))))

;; PROC receives (report path completed-bytes total-bytes-or-#f).
;; Unknown totals use a moving marker rather than an invented percentage.
(define call-with-example-progress
  (lambda (enabled? proc)
    (let ([port (current-error-port)] [previous-path #f] [previous-count #f]
          [previous-total #f] [last-render #f] [line-open? #f] [tick 0])
      (define end-line!
        (lambda ()
          (when line-open?
            (newline port)
            (flush-output-port port)
            (set! line-open? #f))))
      (define report
        (lambda (path completed total)
          (when enabled?
            (let* ([time (current-time 'time-monotonic)]
                   [now (+ (time-second time) (/ (time-nanosecond time) 1000000000))]
                   [new-path? (not (equal? path previous-path))]
                   [changed? (or new-path? (not (equal? completed previous-count))
                                 (not (equal? total previous-total)))])
              (when (and changed?
                         (or new-path? (not last-render) (>= (- now last-render) 1/10)
                             (and (equal? previous-count 0) (positive? completed))
                             (and total (= completed total))))
                (when new-path? (end-line!))
                (let* ([width 24]
                       [filled (and total (if (zero? total) width
                                              (min width (quotient (* completed width) total))))]
                       [bar (make-string width #\-)])
                  (if filled
                      (do ([i 0 (fx1+ i)]) ((fx= i filled)) (string-set! bar i #\=))
                      (string-set! bar (modulo tick width) #\>))
                  (fprintf port "\r~s [~a] ~a ~a bytes"
                           path bar
                           (if total
                               (format "~a%" (if (zero? total) 100
                                                  (min 100 (quotient (* completed 100) total))))
                               " --%")
                           (if total (format "~a/~a" completed total) completed))
                  (flush-output-port port))
                (set! tick (fx1+ tick))
                (set! previous-path path)
                (set! previous-count completed)
                (set! previous-total total)
                (set! last-render now)
                (set! line-open? #t))))))
      (dynamic-wind void (lambda () (proc report)) end-line!))))
