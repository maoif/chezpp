(library (chezpp stack)
  (export make-stack stack stack?
          stack-size stack-empty?
          stack-push! stack-pop! stack-pop-all!
          stack-peek stack-contains? stack-contains/p?
          stack-clear!
          stack-copy stack->list)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp internal))


  (define-record-type ($stack mk-stack stack?)
    (fields (mutable stk stack-stk stack-stk-set!)))


  #|doc
  Construct a stack object.
  |#
  (define-who make-stack
    (lambda () (mk-stack '())))


  #|doc
  Construct a stack object from a list of items.
  The items in the list are pushed onto the stack from left to right.
  |#
  (define-who stack
    (lambda args (mk-stack (reverse args))))


  #|doc
  Push one or more items onto the stack `stk`.
  |#
  (define-who stack-push!
    (case-lambda
      [(stk v)
       (pcheck ([stack? stk])
               (stack-stk-set! stk (cons v (stack-stk stk))))]
      [(stk . v*)
       (pcheck ([stack? stk])
               (let ([s (stack-stk stk)])
                 (stack-stk-set! stk
                                 (fold-left (lambda (s v)
                                              (cons v s))
                                            s v*))))]))


  #|doc
  Pop the newest item from the stack `stk`.
  It is an error if the stack is empty.
  |#
  (define-who stack-pop!
    (lambda (stk)
      (pcheck ([stack? stk])
              (let ([s (stack-stk stk)])
                (if (eq? s '())
                    (errorf who "stack is empty")
                    (let ([v (car s)])
                      (stack-stk-set! stk (cdr s))
                      v))))))


  #|doc
  Pop all items from the stack `stk` into a list.
  The first item in the list corresponds to the newest item in the stack.
  If an additional procedure `proc` is given, it is apply to
  each item popped from the stack for effect.
  |#
  (define-who stack-pop-all!
    (case-lambda
      [(stk)
       (pcheck ([stack? stk])
               (let ([s (stack-stk stk)])
                 (stack-stk-set! stk '())
                 s))]
      [(stk proc)
       (pcheck ([stack? stk] [procedure? proc])
               (let ([s (stack-stk stk)])
                 (stack-stk-set! stk '())
                 (for-each proc s)))]))


  #|doc
  Get the newest item in the stack without removing it.
  It is an error if the stack is empty.
  |#
  (define-who stack-peek
    (lambda (stk)
      (pcheck ([stack? stk])
              (let ([s (stack-stk stk)])
                (if (eq? s '())
                    (errorf who "stack is empty")
                    (car s))))))


  #|doc
  Return the number of items currently in the stack.
  |#
  (define-who stack-size
    (lambda (stk)
      (pcheck ([stack? stk])
              (length (stack-stk stk)))))


  #|doc
  Check whether the stack is empty.
  |#
  (define-who stack-empty?
    (lambda (stk)
      (pcheck ([stack? stk])
              (eq? '() (stack-stk stk)))))


  #|doc
  Remove all items in the stack.
  |#
  (define-who stack-clear!
    (lambda (stk)
      (pcheck ([stack? stk])
              (stack-stk-set! stk '()))))


  #|doc
  Return whether the stack `stk` contains the given item `v`.
  If it does, the procedure returns #t;
  otheriwse it returns #f.
  Items are compared using `equal?`.
  |#
  (define-who stack-contains?
    (lambda (stk v)
      (pcheck ([stack? stk])
              (bool (member v (stack-stk stk))))))


  #|doc
  Return whether the stack `stk` contains an item that satisfies the predicate `=?`.
  If it does, the procedure returns the index of the given item;
  otheriwse it returns #f.
  |#
  (define-who stack-contains/p?
    (lambda (stk =?)
      (pcheck ([stack? stk] [procedure? =?])
              (bool (memp =? (stack-stk stk))))))


  #|doc
  Make a copy of the stack `stk`.
  |#
  (define-who stack-copy
    (lambda (stk)
      (pcheck ([stack? stk])
              (mk-stack (list-copy (stack-stk stk))))))


  #|doc
  Convert the stack into a list, without popping the stack's items.
  |#
  (define-who stack->list
    (lambda (stk)
      (pcheck ([stack? stk])
              ;; return a copy in case it is mutated
              (list-copy (stack-stk stk)))))


  (record-writer
   (type-descriptor $stack)
   (lambda (r p wr)
     (display "#[stack " p)
     (wr (stack->list r) p)
     (display "]" p)))


  (record-type-equal-procedure
   (type-descriptor $stack)
   (lambda (stk1 stk2 =?)
     (=? (stack-stk stk1) (stack-stk stk2))))


  )
