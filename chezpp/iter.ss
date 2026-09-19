(library (chezpp iter)
  (export range make-indexed-iter
          list->iter vector->iter string->iter
          bytevector->iter fxvector->iter flvector->iter
          port->iter port-lines->iter port-chars->iter port-data->iter
          file->iter file-lines->iter file-chars->iter file-data->iter
          iter->list iter-source->iter iterable? iter-register-source!

          get-iter make-iter iter-end iter-end? iter-next! iter-reset! iter-finalize!
          (rename ($iter-finalized? iter-finalized?)
                  ($iter? iter?))
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

  (define source-adapters '())

  #|proc:iter-register-source!
  Register `predicate` and `iterator-maker` as an iterator source adapter.
  The maker receives one matching source and returns an iterator.
  Newer registrations are checked before older registrations.
  |#
  (define-who iter-register-source!
    (lambda (predicate iterator-maker)
      (pcheck ([procedure? predicate iterator-maker])
              (set! source-adapters
                    (cons (cons predicate iterator-maker) source-adapters)))))

  (define find-source-adapter
    (lambda (source)
      (let loop ([adapters source-adapters])
        (cond [(null? adapters) #f]
              [((caar adapters) source) (cdar adapters)]
              [else (loop (cdr adapters))]))))

  #|proc:make-iter
  The `make-iter` procedure returns an iterator using `next!-proc`,
  `reset!-proc`, and optional `fini-proc` procedures.
  |#
  (define make-iter
    (case-lambda
      [(next!-proc reset!-proc)
       (mk-$iter next!-proc reset!-proc)]
      [(next!-proc reset!-proc fini-proc)
       (mk-$iter next!-proc reset!-proc fini-proc)]))

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

  #|proc:make-indexed-iter
  The `make-indexed-iter` procedure returns an indexed iterator constructor named by
  `constructor-name`. The `source-predicate` procedure has signature `(source) -> boolean`.
  The `source-length` procedure has signature `(source) -> nonnegative integer`, and the
  `source-ref` procedure has signature `(source index) -> value`. The returned constructor
  accepts a source with optional `start`, `stop`, and nonzero `step` integers and returns an
  iterator. Negative bounds count from the source end, and reset renormalizes the bounds.
  |#
  (define make-indexed-iter
    (lambda (constructor-name source-predicate source-length source-ref)
      (pcheck ([symbol? constructor-name]
               [procedure? source-predicate source-length source-ref])
              (letrec ([make-source-iter
                        (lambda (source original-start original-stop step full-source?)
                          (let ([index 0] [stop 0])
                            (define reset!
                              (lambda ()
                                (let* ([length (source-length source)]
                                       [start-index
                                        (if (>= original-start 0)
                                            original-start
                                            (+ length original-start))]
                                       [stop-index
                                        (if full-source?
                                            length
                                            (if (>= original-stop 0)
                                                original-stop
                                                (+ length original-stop)))])
                                  (if (= length 0)
                                      (begin
                                        (set! index 0)
                                        (set! stop 0))
                                      (begin
                                        (set! index
                                              (cond [(< start-index 0) 0]
                                                    [(> start-index length) (- length 1)]
                                                    [else start-index]))
                                        (set! stop
                                              (cond [(<= stop-index -1) -1]
                                                    [(>= stop-index length) length]
                                                    [else stop-index])))))))
                            (reset!)
                            (mk-$iter
                             (lambda ()
                               (if (if (> step 0)
                                       (>= index stop)
                                       (<= index stop))
                                   iter-end
                                   (let ([value (source-ref source index)])
                                     (set! index (+ index step))
                                     value)))
                             reset!)))])
                (case-lambda
                  [(source)
                   (pcheck ([source-predicate source])
                           (make-source-iter source 0 0 1 #t))]
                  [(source stop)
                   (pcheck ([source-predicate source] [integer? stop])
                           (make-source-iter source 0 stop 1 #f))]
                  [(source start stop)
                   (pcheck ([source-predicate source] [integer? start stop])
                           (make-source-iter source start stop 1 #f))]
                  [(source start stop step)
                   (pcheck ([source-predicate source] [integer? start stop step])
                           (when (= step 0)
                             (errorf constructor-name "step cannot be 0"))
                           (make-source-iter source start stop step #f))])))))

  #|proc:list->iter
  The `list->iter` procedure returns a forward iterator over `source`. The optional integer
  `start` and `stop` parameters select a half-open range, and positive `step` selects the
  distance between values. The iterator returns each selected value until it reaches its end.
  |#
  (define-who list->iter
    (case-lambda
      [(source)
       (pcheck-list (source)
                    (let ([remaining source])
                      (mk-$iter
                       (lambda ()
                         (if (null? remaining)
                             iter-end
                             (let ([next (car remaining)])
                               (cdr! remaining)
                               next)))
                       (lambda () (set! remaining source)))))]
      [(source stop)
       (pcheck-list (source) (list->iter source 0 stop 1))]
      [(source start stop)
       (pcheck-list (source) (list->iter source start stop 1))]
      [(source start stop step)
       (pcheck ([list? source] [integer? start stop step])
               (when (<= step 0)
                 (errorf who "step must be positive"))
               ;; run to the start first
               (let ([initial-tail
                      (let loop ([remaining source] [count start])
                        (cond [(null? remaining) '()]
                              [(= count 0) remaining]
                              [else (loop (cdr remaining) (sub1 count))]))])
                 (let ([index start] [remaining initial-tail])
                   (mk-$iter
                    (lambda ()
                      (if (or (null? remaining) (>= index stop))
                          iter-end
                          (let ([value (car remaining)])
                            (let loop ([tail remaining] [count step])
                              (cond [(null? tail)
                                     (set! remaining '())]
                                    [(= count 0)
                                     (set! index (+ index step))
                                     (set! remaining tail)]
                                    [else (loop (cdr tail) (sub1 count))]))
                            value)))
                    (lambda ()
                      (set! index start)
                      (set! remaining initial-tail))))))]))

  #|proc:vector->iter
  The `vector->iter` procedure returns an iterator over `source`. The optional integer
  `start`, `stop`, and nonzero `step` parameters select and direct a half-open indexed range.
  The iterator returns each selected vector value until it reaches its end.
  |#
  (define vector->iter
    (make-indexed-iter 'vector->iter vector? vector-length vector-ref))

  (define hashtable->iter
    (lambda (val)
      (pcheck-hashtable (val)
                        (let ([keys #f] [i 0] [len 0])
                          (define reset!
                            (lambda ()
                              (set! keys (hashtable-keys val))
                              (set! len (vector-length keys))
                              (set! i 0)))
                          (reset!)
                          (mk-$iter
                           (lambda ()
                             (if (fx= i len)
                                 iter-end
                                 (let ([key (vector-ref keys i)])
                                   (set! i (fx1+ i))
                                   (hashtable-ref val key #f))))
                           reset!)))))

  #|proc:string->iter
  The `string->iter` procedure returns an iterator over `source`. The optional integer
  `start`, `stop`, and nonzero `step` parameters select and direct a half-open indexed range.
  The iterator returns each selected character until it reaches its end.
  |#
  (define string->iter
    (make-indexed-iter 'string->iter string? string-length string-ref))

  #|proc:bytevector->iter
  The `bytevector->iter` procedure returns an iterator over unsigned bytes in `source`. The
  optional integer `start`, `stop`, and nonzero `step` parameters select and direct a half-open
  indexed range. The iterator returns each selected byte until it reaches its end.
  |#
  (define bytevector->iter
    (make-indexed-iter 'bytevector->iter bytevector? bytevector-length bytevector-u8-ref))

  #|proc:fxvector->iter
  The `fxvector->iter` procedure returns an iterator over fixnums in `source`. The optional
  integer `start`, `stop`, and nonzero `step` parameters select and direct a half-open indexed
  range. The iterator returns each selected fixnum until it reaches its end.
  |#
  (define fxvector->iter
    (make-indexed-iter 'fxvector->iter fxvector? fxvector-length fxvector-ref))

  #|proc:flvector->iter
  The `flvector->iter` procedure returns an iterator over flonums in `source`. The optional
  integer `start`, `stop`, and nonzero `step` parameters select and direct a half-open indexed
  range. The iterator returns each selected flonum until it reaches its end.
  |#
  (define flvector->iter
    (make-indexed-iter 'flvector->iter flvector? flvector-length flvector-ref))

  #|proc:iterable?
  Return whether `source` is a built-in or registered iterator source.
  |#
  (define iterable?
    (lambda (source)
      (or ($iter? source)
          (list? source)
          (vector? source)
          (string? source)
          (bytevector? source)
          (fxvector? source)
          (flvector? source)
          (hashtable? source)
          (bool (find-source-adapter source)))))

  #|proc:iter-source->iter
  Convert a built-in or registered `source` to an iterator.
  Registered adapters are consulted after all built-in source types.
  This is the low-level iterator-library conversion procedure. The transducer
  library's `source->iter` wrapper additionally accepts transducer-specific sources.
  Hashtable passes snapshot keys because ChezScheme provides no lazy table cursor;
  values are read from the source table as they are requested, and reset snapshots keys again.
  |#
  (define-who iter-source->iter
    (lambda (source)
      (pcheck ([iterable? source])
              (cond [($iter? source) source]
                    [(list? source) (list->iter source)]
                    [(vector? source) (vector->iter source)]
                    [(string? source) (string->iter source)]
                    [(bytevector? source) (bytevector->iter source)]
                    [(fxvector? source) (fxvector->iter source)]
                    [(flvector? source) (flvector->iter source)]
                    [(hashtable? source) (hashtable->iter source)]
                    [else ((find-source-adapter source) source)]))))


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

  (define get-iter
    (lambda (who val)
      (if (iterable? val)
          (iter-source->iter val)
          (errorf who "cannot be iterated: ~a" val))))

  #|proc:range
  The `range` procedure returns an iterator over numbers from `start` toward exclusive `stop`.
  The nonzero `step` parameter is added after each value and must point toward `stop`. When only
  `stop` is supplied, `start` is zero; when `step` is omitted, it is one. Equal bounds produce
  an empty iterator.
  |#
  (define-who range
    (case-lambda
      [(stop) (range 0 stop 1)]
      [(start stop) (range start stop 1)]
      [(start stop step)
       (pcheck ([number? start stop step])
               (when (= step 0)
                 (errorf who "step cannot be 0"))
               (unless (or (= start stop)
                           (and (< start stop) (> step 0))
                           (and (> start stop) (< step 0)))
                 (errorf who "step does not point from ~a toward ~a" start stop))
               (let ([value start])
                 (mk-$iter
                  (lambda ()
                    (if (if (> step 0)
                            (>= value stop)
                            (<= value stop))
                        iter-end
                        (let ([current value])
                          (set! value (+ value step))
                          current)))
                  (lambda () (set! value start)))))]))


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
