(library (chezpp iter)
  (export range
          list->iter vector->iter string->iter
          port->iter port-lines->iter port-chars->iter port-data->iter
          file->iter file-lines->iter file-chars->iter file-data->iter
          iter->list

          get-iter iter-end iter-end? iter-next! iter-reset! iter-finalize!
          (rename ($iter-finalized? iter-finalized?))
          iter-for-each iter-map iter-filter iter-take iter-drop iter-fold
          iter-append iter-zip iter-interleave

          iter-sum iter-product iter-avg
          iter-fxsum iter-fxproduct iter-fxavg
          iter-flsum iter-flproduct iter-flavg
          iter-max iter-min)
  (import (chezscheme)
          (chezpp list)
          (chezpp vector)
          (chezpp utils)
          (chezpp internal)
          (chezpp control))

  (define-record-type ($iter mk-$iter $iter?)
    (fields (immutable next!-proc) (immutable reset!-proc) (immutable fini-proc) (mutable ops) (mutable finalized?))
    (opaque #t)
    (sealed #t)
    (protocol (lambda (p)
                (case-lambda
                  [(next!-proc reset!-proc fini-proc)
                   (pcheck ([procedure? next!-proc reset!-proc fini-proc])
                           (p next!-proc reset!-proc fini-proc (make-list-builder) #f))]
                  [(next!-proc reset!-proc)
                   (pcheck ([procedure? next!-proc reset!-proc])
                           (p next!-proc reset!-proc void (make-list-builder) #f))]))))
  (define iter-end (mk-$iter void void))
  (define iter-end? (sect eq? _ iter-end))
  ;; Get the next item from the iterator,
  ;; also run the item through the ops pipeline, if any.
  (define iter-next!
    (lambda (iter)
      (pcheck ([$iter? iter])
              (let ([ops (($iter-ops iter))])
                (if (null? ops)
                    (($iter-next!-proc iter))
                    (let iter-loop ()
                      (let ([x (($iter-next!-proc iter))])
                        (if (eq? x iter-end)
                            x
                            (let op-loop ([op* ops] [x x])
                              (if (null? op*)
                                  x
                                  (let* ([ty (caar op*)]
                                         [proc (cdar op*)])
                                    (case ty
                                      [proc (op-loop (cdr op*) (proc x))]
                                      [filter (if (proc x)
                                                  (op-loop (cdr op*) x)
                                                  (iter-loop))]))))))))))))
  (define iter-reset!
    (lambda (iter)
      (pcheck ([$iter? iter])
              (if ($iter-finalized? iter)
                  (errorf 'iter-reset! "finalized iterator cannot be reset!")
                  (begin (($iter-reset!-proc iter))
                         ($iter-ops-set! iter (make-list-builder)))))))
  (define iter-finalize!
    (lambda (iter)
      (pcheck ([$iter? iter])
              (if ($iter-finalized? iter)
                  (errorf 'iter-finalize! "iterator is already finalized")
                  (begin (($iter-fini-proc iter))
                         ($iter-finalized?-set! iter #t))))))
  (define iter-ops-add!
    (lambda (iter op)
      (($iter-ops iter) op)))

  (define list->iter
    (case-lambda
      [(val)
       (pcheck-list (val)
                    (let ([v val])
                      (mk-$iter
                       (lambda ()
                         (if (null? v)
                             iter-end
                             (let ([next (car v)])
                               (cdr! v)
                               next)))
                       (lambda () (set! v val)))))]
      [(val stop)
       (pcheck-list (val) (list->iter val 0 stop 1))]
      [(val start stop)
       (pcheck-list (val) (list->iter val start stop 1))]
      [(val start stop step)
       ;; TODO handle start < stop and step < 0
       (pcheck ([list? val] [integer? start stop step])
               ;; run to the start first
               (let ([val (let loop ([v val] [c start])
                            (cond [(null? v) '()]
                                  [(fx= c 0) v]
                                  [else (loop (cdr v) (sub1 c))]))])
                 (let ([i start] [v val])
                   (mk-$iter
                    (lambda ()
                      (if (or (null? v) (fx>= i stop))
                          iter-end
                          (let ([res (car v)])
                            (let loop ([v1 v] [c step])
                              (cond [(null? v1)
                                     (set! v '())]
                                    [(fx= c 0)
                                     (set! i (fx+ i step))
                                     (set! v v1)]
                                    [else (loop (cdr v1) (sub1 c))]))
                            res)))
                    (lambda () (set! i start) (set! v val))))))]))

  (define vector->iter
    (case-lambda
      [(val)
       (pcheck-vector (val)
                      (let ([len (vector-length val)] [i 0])
                        (mk-$iter
                         (lambda ()
                           (if (fx= i len)
                               iter-end
                               (let ([next (vector-ref val i)])
                                 (incr! i)
                                 next)))
                         (lambda () (set! i 0)))))]
      [(val stop)
       (pcheck-vector (val) (vector->iter val 0 stop 1))]
      [(val start stop)
       (pcheck-vector (val) (vector->iter val start stop 1))]
      [(val start stop step)
       (pcheck ([vector? val] [integer? start stop step])
               ;; TODO handle start < stop and step < 0
               (let* ([i start]
                      [vlen (vector-length val)]
                      [stop (if (fx>= stop vlen) vlen stop)])
                 (mk-$iter
                  (lambda ()
                    (if (fx>= i stop)
                        iter-end
                        (let ([v (vector-ref val i)])
                          (set! i (fx+ i step))
                          v)))
                  (lambda () (set! i start)))))]))

  (define hashtable->iter
    (lambda (val)
      ;; return (values iter-end #f) when reaching end
      (pcheck-hashtable (val)
                        (todo 'hashtable->iter))))

  (define string->iter
    (case-lambda
      [(val)
       (pcheck-string (val)
                      (let ([len (string-length val)] [i 0])
                        (mk-$iter
                         (lambda ()
                           (if (fx= i len)
                               iter-end
                               (let ([next (string-ref val i)])
                                 (incr! i)
                                 next)))
                         (lambda () (set! i 0)))))]
      [(val stop)
       (pcheck-string (val) (string->iter val 0 stop 1))]
      [(val start stop)
       (pcheck-string (val) (string->iter val start stop 1))]
      [(val start stop step)
       (pcheck ([string? val] [integer? start stop step])
               ;; TODO handle start < stop and step < 0
               (let* ([i start]
                      [vlen (string-length val)]
                      [stop (if (fx>= stop vlen) vlen stop)])
                 (mk-$iter
                  (lambda ()
                    (if (fx>= i stop)
                        iter-end
                        (let ([v (string-ref val i)])
                          (set! i (fx+ i step))
                          v)))
                  (lambda () (set! i start)))))]))


;;;; ports are opened and closed by the caller

  (define define-textual-port->iter
    (lambda (who get-proc)
      (lambda (port)
        (pcheck-open-textual-port
         (port)
         (mk-$iter
          (lambda () (let ([x (get-proc port)])
                       (if (eof-object? x)
                           iter-end
                           x)))
          (lambda () (set-port-position! port 0))
          (lambda () (close-port port)))))))
  (define port->iter
    (define-textual-port->iter 'port->iter get-line))
  (define port-lines->iter
    (define-textual-port->iter 'port-lines->iter get-line))
  (define port-chars->iter
    (define-textual-port->iter 'port-chars->iter get-char))
  (define port-data->iter
    (define-textual-port->iter 'port-data->iter get-datum))

  (define port-bytes->iter
    (lambda (port)
      (pcheck-open-binary-port
       (port)
       (let ([port (open-file-input-port port)])
         (mk-$iter
          (lambda () (let ([x (get-u8 port)])
                       (if (eof-object? x)
                           iter-end
                           x)))
          (lambda () (set-port-position! port 0))
          (lambda () (close-port port)))))))



;;;; files are opened by iter, and closed in fini!

  (define define-file->iter
    (lambda (who open-proc get-proc)
      (lambda (file)
        (pcheck-file (file)
                     (let ([port (open-proc file)])
                       (mk-$iter
                        (lambda () (let ([x (get-proc port)])
                                     (if (eof-object? x)
                                         iter-end
                                         x)))
                        (lambda () (set-port-position! port 0))
                        (lambda () (close-port port))))))))
  ;; the same as `file-lines->iter`
  (define file->iter
    (define-file->iter 'file->iter open-input-file get-line))
  (define file-bytes->iter
    (define-file->iter 'file-bytes->iter open-file-input-port get-u8))
  (define file-lines->iter
    (define-file->iter 'file-lines->iter open-input-file get-line))
  (define file-chars->iter
    (define-file->iter 'file-chars->iter open-input-file get-char))
  (define file-data->iter
    (define-file->iter 'file-data->iter open-input-file get-datum))

  (define iter-table '())

  (define add-for-iter-type!
    (lambda (pred iter-gen)
      (pcheck-proc (pred iter-gen)
                   (set! iter-table (cons (cons pred iter-gen) iter-table)))))

  (define get-iter
    (lambda (who val)
      (if ($iter? val)
          val
          (cond [(list? val) (list->iter val)]
                [(vector? val) (vector->iter val)]
                [(string? val) (string->iter val)]
                [(hashtable? val) (hashtable->iter val)]
                [else (let loop ([table iter-table])
                        (if (null? table)
                            (errorf who "cannot be iterated: ~a" val)
                            (if ((caar table) val)
                                ((cdar table) val)
                                (loop (cdr table)))))]))))

  #|doc
  Returns a number iterator.
  |#
  (define range
    (case-lambda
      [(stop) (range 0 stop 1)]
      [(start stop) (range start stop 1)]
      [(start stop step)
       (let ([val start])
         (mk-$iter
          (lambda ()
            (if (>= val stop)
                iter-end
                (let ([n (+ val step)] [v val])
                  ;; TODO optimize this update
                  (set! val n)
                  v)))
          (lambda () (set! val start))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   iterator operations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define all-iters? (lambda (x*) (andmap $iter? x*)))


;;; intermediate ops
;;; op types:
;;; - proc:   processes the item and returns a result for later processing
;;; - filter: processes the item and returns a boolean indicating
;;;           whether the item will be used for later processing
;;; Intermediate ops always return an iterator.

  (define iter-copy
    (lambda (iter)
      (pcheck ([$iter? iter])
              (if ($iter-finalized? iter)
                  (errorf 'iter-copy "cannot copy a finalized iterator")
                  (let ([new (mk-$iter ($iter-next!-proc iter) ($iter-reset!-proc iter)
                                       ($iter-fini-proc iter)   ($iter-ops iter))])
                    new)))))
  (define iter-map
    (lambda (proc iter)
      (pcheck ([$iter? iter] [procedure? proc])
              (iter-ops-add! iter (cons 'proc proc))
              iter)))
  (define iter-filter
    (lambda (proc iter)
      (pcheck ([$iter? iter] [procedure? proc])
              (iter-ops-add! iter (cons 'filter proc))
              iter)))
  (define iter-take
    (lambda (n iter)
      (pcheck ([$iter? iter] [fixnum? n])
              (if (>= n 0)
                  (begin (iter-ops-add! iter (cons 'filter
                                                   (let ([n n])
                                                     (lambda (x)
                                                       (if (= n 0)
                                                           #f
                                                           (begin (set! n (fx- n 1))
                                                                  #t))))))
                         iter)
                  (errorf 'iter-take "item count must be positive: ~a" n)))))
  ;; take consecutive items that satisfies `pred`
  (define iter-take-while
    (lambda (pred iter)
      (pcheck ([$iter? iter] [procedure? pred])
              (iter-ops-add! iter (cons 'filter (let ([rest-bad? #f])
                                                  (lambda (x)
                                                    (if rest-bad?
                                                        #f
                                                        (if (pred x)
                                                            #t
                                                            (begin (set! rest-bad? #t)
                                                                   #f)))))))
              iter)))
  (define iter-drop
    (lambda (n iter)
      (pcheck ([$iter? iter] [fixnum? n])
              (if (>= n 0)
                  (begin (iter-ops-add! iter (cons 'filter
                                                   (let ([n n])
                                                     (lambda (x)
                                                       (if (= n 0)
                                                           #t
                                                           (begin (set! n (fx- n 1))
                                                                  #f))))))
                         iter)
                  (errorf 'iter-take "item count must be positive: ~a" n)))))
  ;; drop consecutive items that satisfies `pred`
  (define iter-drop-while
    (lambda (pred iter)
      (pcheck ([$iter? iter] [procedure? pred])
              (iter-ops-add! iter (cons 'filter (let ([rest-good? #f])
                                                  (lambda (x)
                                                    (if rest-good?
                                                        #t
                                                        (if (pred x)
                                                            #f
                                                            (begin (set! rest-good? #t)
                                                                   #t)))))))
              iter)))

  ;; (iter-append (a0 a1 ...) (b0 b1 ...)) ->
  ;; (a0 a1 ... b0 b1 ...)
  (define iter-append
    (lambda (iter . iter*)
      (pcheck ([$iter? iter])
              (if (null? iter*)
                  iter
                  (pcheck ([all-iters? iter*])
                          (let ([it iter] [it-rest iter*])
                            (mk-$iter
                             (lambda () (let loop ()
                                          (let ([x (iter-next! it)])
                                            (if (iter-end? x)
                                                (if (null? it-rest)
                                                    iter-end
                                                    (begin (set! it (car it-rest))
                                                           (set! it-rest (cdr it-rest))
                                                           (loop)))
                                                x))))
                             (lambda ()
                               (set! it iter)
                               (set! it-rest iter*)
                               (for-each iter-reset! (cons iter iter*))))))))))
  ;; (iter-zip (a0 a1 ...) (b0 b1 ...)) ->
  ;; ((a0 b0) (a1 b1) ...)
  (define iter-zip
    (lambda (iter . iter*)
      (pcheck ([$iter? iter])
              (if (null? iter*)
                  iter
                  (pcheck ([all-iters? iter*])
                          (let ([iter* (cons iter iter*)])
                            (mk-$iter
                             (lambda () (let ([v* (map iter-next! iter*)])
                                          (if (memq iter-end v*)
                                              iter-end
                                              v*)))
                             (lambda () (for-each iter-reset! iter*)))))))))
  ;; (iter-interleave (a0 a1 ...) (b0 b1 ...)) ->
  ;; (a0 b0 a1 b1 ...)
  (define iter-interleave
    (lambda (iter . iter*)
      (pcheck ([$iter? iter])
              (if (null? iter*)
                  iter
                  (pcheck ([all-iters? iter*])
                          (let* ([iter* (cons iter iter*)]
                                 [itvec (list->vector iter*)]
                                 [len (length iter*)]
                                 [idx 0])
                            (mk-$iter
                             (lambda ()
                               (let loop ([i idx])
                                 (if (vandmap iter-end? itvec)
                                     iter-end
                                     (if (fx>= i len)
                                         (loop 0)
                                         (let ([it (vector-ref itvec i)])
                                           (if (iter-end? it)
                                               (loop (add1 i))
                                               (let ([x (iter-next! it)])
                                                 (if (eq? x iter-end)
                                                     (begin (vector-set! itvec i iter-end)
                                                            (loop (add1 i)))
                                                     (begin (set! idx (add1 i))
                                                            x)))))))))
                             (lambda ()
                               (for-each iter-reset! iter*)
                               (set! itvec (list->vector iter*))
                               (set! idx 0)))))))))
  ;; iter of iters -> iter
  (define iter-concat
    (lambda (iter)
      (todo)))
  ;; remove duplicates
  (define iter-distinct
    (lambda (proc iter)
      (todo)))
  (define iter-sorted
    (lambda (proc iter)
      (todo)))

;;; terminal ops

  (define iter-for-each
    (lambda (proc iter)
      (pcheck ([$iter? iter] [procedure? proc])
              (let iter-loop ()
                (let ([x (iter-next! iter)])
                  (if (eq? x iter-end)
                      (iter-finalize! iter)
                      (begin (proc x)
                             (iter-loop))))))))
  (define iter-fold
    (lambda (proc acc iter)
      (let iter-loop ([acc acc])
        (let ([x (iter-next! iter)])
          (if (eq? x iter-end)
              (begin (iter-finalize! iter)
                     acc)
              (iter-loop (proc acc x)))))))

  (define iter-max
    (case-lambda
      [(iter) (iter-max > iter)]
      [(f iter) (iter-fold (lambda (acc x) (if acc (if (f x acc) x acc) x)) #f iter)]))
  (define iter-min
    (case-lambda
      [(iter) (iter-min < iter)]
      [(f iter) (iter-fold (lambda (acc x) (if acc (if (f x acc) x acc) x)) #f iter)]))
  (define iter-avg
    (lambda (iter)
      (let loop ([i 0] [sum 0])
        (let ([x (iter-next! iter)])
          (if (iter-end? x)
              (if (= i 0) #f (/ sum i))
              (loop (add1 i) (+ x sum)))))))
  (define iter-sum
    (lambda (iter)
      (iter-fold (lambda (acc x) (+ acc x)) 0 iter)))
  (define iter-product
    (lambda (iter)
      (iter-fold (lambda (acc x) (* acc x)) 1 iter)))

  (define iter-fxmax
    (lambda (iter)
      (iter-max fx> iter)))
  (define iter-fxmin
    (lambda (iter)
      (iter-min fx< iter)))
  (define iter-fxavg
    (lambda (iter)
      (let loop ([i 0] [sum 0])
        (let ([x (iter-next! iter)])
          (if (iter-end? x)
              (if (fx= i 0) #f (fx/ sum i))
              (loop (add1 i) (fx+ x sum)))))))
  (define iter-fxsum
    (lambda (iter)
      (iter-fold (lambda (acc x) (fx+ acc x)) 0 iter)))
  (define iter-fxproduct
    (lambda (iter)
      (iter-fold (lambda (acc x) (fx* acc x)) 1 iter)))

  (define iter-flmax
    (lambda (iter)
      (iter-max fl> iter)))
  (define iter-flmin
    (lambda (iter)
      (iter-min fl< iter)))
  (define iter-flavg
    (lambda (iter)
      (let loop ([i 0] [sum 0.0])
        (let ([x (iter-next! iter)])
          (if (iter-end? x)
              (if (fx= i 0) #f (fl/ sum (inexact i)))
              (loop (add1 i) (fl+ x sum)))))))
  (define iter-flsum
    (lambda (iter)
      (iter-fold (lambda (acc x) (fl+ acc x)) 0.0 iter)))
  (define iter-flproduct
    (lambda (iter)
      (iter-fold (lambda (acc x) (fl* acc x)) 1.0 iter)))

  ;; TODO type-specialized ops


;;; conversions

  (define iter->list
    (lambda (iter)
      (pcheck ([$iter? iter])
              (let ([lb (make-list-builder)])
                (let iter-loop ()
                  (let ([x (iter-next! iter)])
                    (if (eq? x iter-end)
                        (begin (iter-finalize! iter)
                               (lb))
                        (begin (lb x)
                               (iter-loop)))))))))
  (define iter->vector
    (lambda (iter)
      (todo)))
  (define iter->dynvec
    (lambda (iter)
      (todo)))



  #|doc
  Build iterator pipeline.
  For the sake of extensibility, iter ops should take the iter argument last,
  so `iter>>>` can expand properly.
  |#
  (define-syntax iter>>>
    (lambda (stx)
      (syntax-case stx ()
        [(k iter op ops ...)
         (todo)])))





  )
