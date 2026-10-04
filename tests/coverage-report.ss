(load "mat.so")

(define args (command-line-arguments))
(unless (>= (length args) 3)
  (errorf 'coverage-report "expected input count, output path, and coverage files"))

(define covin-count (string->number (car args)))
(define covout (cadr args))
(define files (cddr args))
(define covin* (let loop ([n covin-count] [files files] [result '()])
                 (if (= n 0) (reverse result)
                     (loop (sub1 n) (cdr files) (cons (car files) result)))))
(define covout* (list-tail files covin-count))
(combine-coverage-files covout covout*)

(define source-count
  (lambda (path)
    (source-table-size (load-coverage-files path))))

(define covered-count
  (lambda (covout covin)
    (let ([table (load-coverage-files covout)] [universe (load-coverage-files covin)] [count 0])
      (for-each
        (lambda (entry)
          (when (and (source-table-contains? universe (car entry)) (> (cdr entry) 0))
            (set! count (+ count 1))))
        (source-table-dump table))
      count)))

(define pad-left
  (lambda (text width)
    (string-append (make-string (max 0 (- width (string-length text))) #\space) text)))

(define pad-right
  (lambda (text width)
    (string-append text (make-string (max 0 (- width (string-length text))) #\space))))

(define rows
  (map (lambda (covin)
         (let ([total (source-count covin)])
           (list covin (if (= total 0) 0 (covered-count covout covin)) total)))
       covin*))
(when (null? rows)
  (printf "~%Coverage Report~%No source coverage tables were generated.~%"))
(define path-width (apply max 4 (map (lambda (row) (string-length (car row))) rows)))
(define covered-width (apply max 7 (map (lambda (row) (string-length (number->string (cadr row)))) rows)))
(define total-width (apply max 5 (map (lambda (row) (string-length (number->string (caddr row)))) rows)))

(printf "~%Coverage Report~%")
(printf "~a  ~a  ~a  ~a~%"
  (pad-right "Source" path-width)
  (pad-left "Covered" covered-width)
  (pad-left "Total" total-width)
  "Percent")
(printf "~a  ~a  ~a  ~a~%"
  (make-string path-width #\-)
  (make-string covered-width #\-)
  (make-string total-width #\-)
  "-------")
(for-each
  (lambda (row)
    (let* ([path (car row)] [covered (cadr row)] [total (caddr row)]
           [percent (if (= total 0) 0 (round (* 100.0 (/ covered total))))])
      (printf "~a  ~a  ~a  ~a%~%"
        (pad-right path path-width)
        (pad-left (number->string covered) covered-width)
        (pad-left (number->string total) total-width)
        (pad-left (number->string percent) 7))))
  rows)
