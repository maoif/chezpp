(library (chezpp navigator private recursive)
  (export nav/rec nav/letrec nav/children nav/leaves nav/walker nav/before nav/after)
  (import (chezscheme)
          (chezpp utils)
          (chezpp navigator private core)
          (chezpp navigator private engine))

  (define make-recursive-nav-group
    (lambda (bindings root)
      (let loop ([names (map car bindings)] [seen '()])
        (cond [(null? names) (make-$recursive-nav-group bindings root)]
              [(memq (car names) seen)
               (nav-error 'nav/letrec "duplicate recursive navigator name: ~s" (car names))]
              [else (loop (cdr names) (cons (car names) seen))]))))

  (define emit-children
    (lambda (value emit)
      (cond [(pair? value)
             (when (list? value)
               (for-each emit value))]
            [(vector? value)
             (let ([len (vector-length value)])
               (let loop ([i 0])
                 (when (< i len)
                   (emit (vector-ref value i))
                   (loop (+ i 1)))))]
            [(hashtable? value)
             (let ([vals (hashtable-values value)])
               (let ([len (vector-length vals)])
                 (let loop ([i 0])
                   (when (< i len)
                     (emit (vector-ref vals i))
                     (loop (+ i 1))))))]
            [else (void)])))

  (define leaf?
    (lambda (value)
      (or (and (pair? value) (not (list? value)))
          ;; Use a one-shot continuation to stop after discovering one child.
          (call/1cc
           (lambda (return)
             (emit-children value (lambda (child) (return #f)))
             #t)))))

  (define emit-current-focus
    (lambda (value emit)
      (let ([emitted? #f])
        (emit-children value
                       (lambda (child)
                         (when (leaf? child)
                           (set! emitted? #t)
                           (emit child))))
        (unless emitted?
          (emit value)))))

  (define remove-missing-values
    (lambda (items)
      (let loop ([items items])
        (cond [(null? items) '()]
              [(nav-missing? (car items)) (loop (cdr items))]
              [else (cons (car items) (loop (cdr items)))]))))

  (define make-tree-nav
    (lambda (name metadata select-proc transform-proc transform!-proc clear-proc clear!-proc)
      (make-$navigator
       name 'recursive metadata
       select-proc
       transform-proc transform!-proc clear-proc clear!-proc)))

  (define tree-unsupported-transform
    (lambda (name)
      (lambda (value update)
        (nav-error 'nav-transform "recursive navigator ~s does not support transform yet" name))))

  (define tree-unsupported-transform!
    (lambda (name)
      (lambda (value update!)
        (nav-error 'nav-transform! "recursive navigator ~s does not support transform! yet" name))))

  (define tree-unsupported-clear
    (lambda (name)
      (lambda (value clear)
        (nav-error 'nav-clearval "recursive navigator ~s does not support clear yet" name))))

  (define tree-unsupported-clear!
    (lambda (name)
      (lambda (value clear!)
        (nav-error 'nav-clearval! "recursive navigator ~s does not support clear! yet" name))))

  #|proc:nav/children
  The `nav/children` navigator focuses immediate children of supported nested
  values when composed with later path steps. As a final selection step, it
  focuses collection descendants in tree order.
  |#
  (define nav/children
    (make-tree-nav
     'nav/children 'nav/children
     (lambda (value emit)
       (emit-children value emit))
     (lambda (value update)
       (cond [(and (pair? value) (list? value))
              (remove-missing-values (map update value))]
             [(vector? value)
              (let* ([len (vector-length value)]
                     [copy (make-vector len)])
                (let loop ([i 0])
                  (when (< i len)
                    (let ([new-value (update (vector-ref value i))])
                      (vector-set! copy i (if (nav-missing? new-value)
                                              (vector-ref value i)
                                              new-value)))
                    (loop (+ i 1))))
                copy)]
             [(hashtable? value)
              (let ([copy (hashtable-copy value #t)])
                (let-values ([(keys vals) (hashtable-entries value)])
                  (let ([len (vector-length keys)])
                    (let loop ([i 0])
                      (when (< i len)
                        (let ([new-value (update (vector-ref vals i))])
                          (if (nav-missing? new-value)
                              (hashtable-delete! copy (vector-ref keys i))
                              (hashtable-set! copy (vector-ref keys i) new-value)))
                        (loop (+ i 1))))))
                copy)]
             [else value]))
     (lambda (value update!)
       (cond [(and (pair? value) (list? value))
              (let loop ([xs value])
                (unless (null? xs)
                  (let ([new-value (update! (car xs))])
                    (unless (nav-missing? new-value)
                      (set-car! xs new-value)))
                  (loop (cdr xs))))
              value]
             [(vector? value)
              (let ([len (vector-length value)])
                (let loop ([i 0])
                  (when (< i len)
                    (let ([new-value (update! (vector-ref value i))])
                      (unless (nav-missing? new-value)
                        (vector-set! value i new-value)))
                    (loop (+ i 1)))))
              value]
             [(hashtable? value)
              (let-values ([(keys vals) (hashtable-entries value)])
                (let ([len (vector-length keys)])
                  (let loop ([i 0])
                    (when (< i len)
                      (let ([new-value (update! (vector-ref vals i))])
                        (if (nav-missing? new-value)
                            (hashtable-delete! value (vector-ref keys i))
                            (hashtable-set! value (vector-ref keys i) new-value)))
                      (loop (+ i 1))))))
              value]
             [else value]))
     (lambda (value clear)
       (cond [(and (pair? value) (list? value))
              (remove-missing-values
               (map clear value))]
             [(vector? value) value]
             [(hashtable? value)
              (let ([copy (hashtable-copy value #t)])
                (let-values ([(keys vals) (hashtable-entries value)])
                  (let ([len (vector-length keys)])
                    (let loop ([i 0])
                      (when (< i len)
                        (when (nav-missing? (clear (vector-ref vals i)))
                          (hashtable-delete! copy (vector-ref keys i)))
                        (loop (+ i 1))))))
                copy)]
             [else value]))
     (lambda (value clear!)
       (cond [(hashtable? value)
              (let-values ([(keys vals) (hashtable-entries value)])
                (let ([len (vector-length keys)])
                  (let loop ([i 0])
                    (when (< i len)
                      (when (nav-missing? (clear! (vector-ref vals i)))
                        (hashtable-delete! value (vector-ref keys i)))
                      (loop (+ i 1))))))
              value]
             [(and (pair? value) (list? value))
              (for-each clear! value)
              value]
             [(vector? value)
              (let ([len (vector-length value)])
                (let loop ([i 0])
                  (when (< i len)
                    (clear! (vector-ref value i))
                    (loop (+ i 1)))))
              value]
             [else value]))))

  #|proc:nav/leaves
  The `nav/leaves` navigator focuses nested values with no supported children.
  |#
  (define nav/leaves
    (make-tree-nav
     'nav/leaves 'nav/leaves
     (lambda (value emit)
       (let walk ([value value])
         (if (leaf? value)
             (emit value)
             (emit-children value walk))))
     (tree-unsupported-transform 'nav/leaves)
     (tree-unsupported-transform! 'nav/leaves)
     (tree-unsupported-clear 'nav/leaves)
     (tree-unsupported-clear! 'nav/leaves)))

  #|proc:nav/walker
  The `nav/walker` procedure returns a navigator that recursively walks nested
  values and focuses every value for which `predicate` returns true.
  |#
  (define nav/walker
    (lambda (predicate)
     (pcheck ([procedure? predicate])
             (make-tree-nav
              'nav/walker '(nav/walker)
              (lambda (value emit)
                (let walk ([value value])
                  (when (predicate value)
                    (emit value))
                  (emit-children value walk)))
              (tree-unsupported-transform 'nav/walker)
              (tree-unsupported-transform! 'nav/walker)
              (tree-unsupported-clear 'nav/walker)
              (tree-unsupported-clear! 'nav/walker)))))

  #|proc:nav/before
  The `nav/before` procedure returns a navigator that focuses the current value
  before values selected by `path`.
  |#
  (define nav/before
    (lambda (path)
      (let ([compiled (nav-compile-path path)])
        (make-tree-nav
         'nav/before '(nav/before)
         (lambda (value emit)
           (emit-current-focus value emit)
           (select-path compiled value emit))
         (tree-unsupported-transform 'nav/before)
         (tree-unsupported-transform! 'nav/before)
         (tree-unsupported-clear 'nav/before)
         (tree-unsupported-clear! 'nav/before)))))

  #|proc:nav/after
  The `nav/after` procedure returns a navigator that focuses values selected by
  `path` before focusing the current value.
  |#
  (define nav/after
    (lambda (path)
      (let ([compiled (nav-compile-path path)])
        (make-tree-nav
         'nav/after '(nav/after)
         (lambda (value emit)
           (select-path compiled value emit)
           (emit-current-focus value emit))
         (tree-unsupported-transform 'nav/after)
         (tree-unsupported-transform! 'nav/after)
         (tree-unsupported-clear 'nav/after)
         (tree-unsupported-clear! 'nav/after)))))

  #|macro:nav/rec
  The `nav/rec` macro creates a recursive navigator named by identifier `name`
  whose body is `body`.
  |#
  (define-syntax nav/rec
    (lambda (stx)
      (syntax-case stx ()
        [(_ name body)
         (identifier? #'name)
         #'(nav/letrec ([name body]) name)]
        [_ (syntax-error stx "invalid nav/rec form:")])))

  #|macro:nav/letrec
  The `nav/letrec` macro creates a mutually recursive navigator group from
  bindings and returns `root`.
  |#
  (define-syntax nav/letrec
    (lambda (stx)
      (syntax-case stx ()
        [(_ ([name body] ...) root)
         (andmap identifier? #'(name ...))
         #'(let-syntax ([name (identifier-syntax (make-nav-ref 'name))] ...)
             (make-recursive-nav-group
              (list (cons 'name body) ...)
              root))]
        [_ (syntax-error stx "invalid nav/letrec form:")]))))
