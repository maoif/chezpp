(import (chezpp))

(define list-copy*
  (lambda (x)
    (if (pair? x)
        (cons (list-copy* (car x)) (list-copy* (cdr x)))
        x)))

(define ht
  (lambda args
    (let ([table (make-eq-hashtable)])
      (let loop ([args args])
        (if (null? args)
            table
            (begin
              (hashtable-set! table (car args) (cadr args))
              (loop (cddr args))))))))

(define ht-ref
  (lambda (table key)
    (hashtable-ref table key #f)))

(define capture-written
  (lambda (proc value)
    (call-with-string-output-port
      (lambda (port)
        (proc value port)))))

(define has-same-keys?
  (lambda (table keys)
    (let ([ks (vector->list (hashtable-keys table))])
      (and (= (length ks) (length keys))
           (andmap (lambda (key) (memq key ks)) keys)))))

(mat navigator-core
     (nav? nav-stay)
     (nav-path? nav-empty-path)
     (nav-empty-path? nav-empty-path)
     (equal? '(nav-path) (nav->datum nav-empty-path))
     (equal? '(nav-path (nav-key user) (nav-nth 0))
             (nav->datum (nav-path 'user 0)))
     (equal? '(nav-path (nav-key user) nav-all (nav-key name))
             (nav->datum (nav-path (nav-path (nav-key 'user) nav-all)
                                   (nav-key 'name))))
     (eq? nav-stay (nav-coerce nav-stay))
     (nav-path? (nav-coerce (nav-path nav-stay)))
     (equal? '(nav-key user) (nav->datum (nav-coerce 'user)))
     (equal? '(nav-nth 3) (nav->datum (nav-coerce 3)))
     ;; negative: negative indexes are not valid path coercions.
     (error? (nav-coerce -1))
     ;; negative: arbitrary data cannot be coerced into a path step.
     (error? (nav-coerce '#(bad))))
