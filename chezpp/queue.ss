(library (chezpp queue)
  (export make-queue queue queue?
          queue-empty? queue-size
          queue-push! queue-pop! queue-pop-all!
          queue-peek queue-contains? queue-contains/p?
          queue-clear!
          queue-copy queue->list)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp internal)
          (chezpp dlist))


  (define-record-type ($queue mk-queue queue?)
    (fields (immutable dl queue-dl)))


  #|doc
  Construct a queue object.
  |#
  (define-who make-queue
    (lambda () (mk-queue (make-dlist))))


  #|doc
  Construct a queue object from a list of items.
  The items in the list are pushed into the queue from left to right.
  |#
  (define-who queue
    (lambda args (mk-queue (apply dlist args))))


  #|doc
  Push one or more items into the queue `q`.
  |#
  (define-who queue-push!
    (case-lambda
      [(q v)
       (pcheck ([queue? q])
               (dlist-push-back! (queue-dl q) v))]
      [(q . v*)
       (pcheck ([queue? q])
               (let ([dl (queue-dl q)])
                 (for-each (lambda (x)
                             (dlist-push-back! dl x))
                           v*)))]))


  #|doc
  Pop the oldest item from the queue `q`.
  It is an error if the queue is empty.
  |#
  (define-who queue-pop!
    (lambda (q)
      (pcheck ([queue? q])
              (let ([dl (queue-dl q)])
                (if (dlist-empty? dl)
                    (errorf who "queue is empty")
                    (dlist-pop! dl))))))


  #|doc
  Pop all items from the queue `q` into a list.
  The first item in the list corresponds to the oldest item in the queue.
  If an additional procedure `proc` is given, it is apply to
  each item popped from the queue for effect.
  |#
  (define-who queue-pop-all!
    (case-lambda
      [(q)
       (pcheck ([queue? q])
               (let ([dl (queue-dl q)])
                 (let ([res (dlist->list dl)])
                   (dlist-clear! dl)
                   res)))]
      [(q proc)
       (let ([dl (queue-dl q)] [procedure? proc])
         (dlist-for-each proc dl)
         (dlist-clear! dl))]))


  #|doc
  Get the oldest item in the queue without removing it.
  It is an error if the queue is empty.
  |#
  (define-who queue-peek
    (lambda (q)
      (pcheck ([queue? q])
              (let ([dl (queue-dl q)])
                (if (dlist-empty? dl)
                    (errorf who "queue is empty")
                    (dlist-ref dl 0))))))


  #|doc
  Return the number of items currently in the queue.
  |#
  (define-who queue-size
    (lambda (q)
      (pcheck ([queue? q])
              (dlist-length (queue-dl q)))))


  #|doc
  Check whether the queue is empty.
  |#
  (define-who queue-empty?
    (lambda (q)
      (pcheck ([queue? q])
              (dlist-empty? (queue-dl q)))))


  #|doc
  Remove all items in the queue.
  |#
  (define-who queue-clear!
    (lambda (q)
      (pcheck ([queue? q])
              (dlist-clear! (queue-dl q)))))


  #|doc
  Return whether the queue `q` contains the given item `v`.
  If it does, the procedure returns #t; otheriwse it returns #f.
  Items are compared using `equal?`.
  |#
  (define-who queue-contains?
    (lambda (q v)
      (pcheck ([queue? q])
              (dlist-contains? (queue-dl q) v))))


  #|doc
  Return whether the queue `q` contains an item that satisfies the predicate `=?`.
  If it does, the procedure returns the index of the given item;
  otheriwse it returns #f.
  |#
  (define-who queue-contains/p?
    (lambda (q =?)
      (pcheck ([queue? q] [procedure? =?])
              (dlist-contains/p? (queue-dl q) =?))))


  #|doc
  Make a copy of the queue `q`.
  |#
  (define-who queue-copy
    (lambda (q)
      (pcheck ([queue? q])
              (mk-queue (dlist-copy (queue-dl q))))))


  #|doc
  Convert the queue into a list, without popping the queue's items.
  |#
  (define-who queue->list
    (lambda (q)
      (pcheck ([queue? q])
              (dlist->list (queue-dl q)))))


  (record-writer
   (type-descriptor $queue)
   (lambda (r p wr)
     (display "#[queue " p)
     (wr (queue->list r) p)
     (display "]" p)))


  (record-type-equal-procedure
   (type-descriptor $queue)
   (lambda (q1 q2 =?)
     (=? (queue-dl q1) (queue-dl q2))))


  )
