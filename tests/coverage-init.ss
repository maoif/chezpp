(load "mat.so")

(define args (command-line-arguments))
(unless (>= (length args) 2)
  (errorf 'coverage-init "expected test object and coverage file paths"))

(coverage-table (apply load-coverage-files (cddr args)))
(record-run-coverage
  (cadr args)
  (lambda () (load (car args))))
