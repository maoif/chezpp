(import (chezpp)
        (chezpp net transfer))

(define capture-condition
  (lambda (thunk)
    (guard (failure [else failure])
      (thunk)
      #f)))

(mat net-transfer-policy
     (let ([policy (make-transfer-policy 'resume 'replace 65536 #f)])
       (and (transfer-policy? policy)
            (eq? 'resume (transfer-policy-resume policy))
            (eq? 'replace (transfer-policy-overwrite policy))
            (= 65536 (transfer-policy-chunk-size policy))
            (not (transfer-policy-progress policy))))

     (let ([policy (make-transfer-policy 12 'skip 4096 #f)])
       (and (= 12 (transfer-policy-resume policy))
            (eq? 'skip (transfer-policy-overwrite policy))))

     ;; Progress receives protocol, direction, path, completed bytes, and total or #f.
     (let ([seen '()])
       (transfer-report-progress!
        (make-transfer-policy
         'never 'error 4096
         (lambda (protocol direction path completed total)
           (set! seen (list protocol direction path completed total))))
        'ftp 'upload "/remote/a" 4 10)
       (equal? seen '(ftp upload "/remote/a" 4 10)))

     (eq? default-transfer-policy
          default-transfer-policy)

     ;; An unknown resume mode is rejected.
     (condition? (capture-condition
                  (lambda () (make-transfer-policy 'restart 'error 4096 #f))))

     ;; A zero chunk size is rejected.
     (condition? (capture-condition
                  (lambda () (make-transfer-policy 'never 'error 0 #f)))))
