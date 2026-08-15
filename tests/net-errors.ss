(import (chezpp))

(mat net-error-fields
     (let* ([cause (condition (make-message-condition "inner"))]
            [error (make-net-error 'socket 'connect "refused"
                                   'connection-refused 111
                                   "127.0.0.1:1" #t cause '())])
       (and (net-error? error)
            (eq? 'socket (net-error-kind error))
            (eq? 'connect (net-error-operation error))
            (eq? 'connection-refused (net-error-status error))
            (= 111 (net-error-errno error))
            (net-error-retryable? error)
            (eq? cause (net-error-cause error)))))

(mat net-error-matching
     (net-error-matches?
      (make-net-error 'dns 'resolve "timeout" 'timeout #f "example.test" #t #f '())
      '((kind . dns) (operation . resolve) (status . timeout) (retryable? . #t))))

(mat net-error-handler
     (= 42
        (call-with-net-error
         (lambda () (raise-net-error 'socket 'connect "refused"))
         (lambda (error) (if (net-error? error) 42 0))))
     (let ([handled? #f])
       (guard (condition [else (not handled?)])
         (call-with-net-error
          (lambda () (error "ordinary"))
          (lambda (error) (set! handled? #t)))
         #f)))
