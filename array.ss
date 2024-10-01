(library (chezpp array)
  (export array make-array array? array-length array-empty?
          array-ref array-add! array-delete! array-set! array-clear!
          array-slice array-slice! array-copy!
          array-filter array-filter! array-partition
          array-append array-append!
          array-reverse array-reverse!
          array-map array-map/i array-map! array-map/i!
          array-for-each array-for-each/i
          array-map-rev array-map/i-rev
          array-for-each-rev array-for-each/i-rev
          array-fold-left array-fold-left/i array-fold-right array-fold-right/i


          fxarray make-fxarray fxarray? fxarray-length fxarray-empty?
          fxarray-ref fxarray-add! fxarray-delete! fxarray-set! fxarray-clear!
          fxarray-slice fxarray-slice! fxarray-copy!
          fxarray-filter fxarray-filter! fxarray-partition
          fxarray-append fxarray-append!
          fxarray-reverse fxarray-reverse!
          fxarray-map fxarray-map/i fxarray-map! fxarray-map/i!
          fxarray-for-each fxarray-for-each/i
          fxarray-map-rev fxarray-map/i-rev
          fxarray-for-each-rev fxarray-for-each/i-rev
          fxarray-fold-left fxarray-fold-left/i fxarray-fold-right fxarray-fold-right/i

          u8array make-u8array u8array? u8array-length u8array-empty?
          u8array-ref u8array-add! u8array-delete! u8array-set! u8array-clear!
          u8array-slice u8array-slice! u8array-copy!
          u8array-filter u8array-filter! u8array-partition
          u8array-append u8array-append!
          u8array-reverse u8array-reverse!
          u8array-map u8array-map/i u8array-map! u8array-map/i!
          u8array-for-each u8array-for-each/i
          u8array-map-rev u8array-map/i-rev
          u8array-for-each-rev u8array-for-each/i-rev
          u8array-fold-left u8array-fold-left/i u8array-fold-right u8array-fold-right/i

          array->list fxarray->list u8array->list)
  (import (chezpp chez)
          (chezpp internal)
          (chezpp list)
          (chezpp vector)
          (chezpp utils))

  ;; TODO allow change incr-factor?
  ;; TODO shrink the array when memory is low?

  (define-record-type ($array mk-array array?)
    (nongenerative)
    (fields
     ;; the backing vector, whose length is the capacity
     (mutable vec array-vec array-vec-set!)
     (mutable incr-factor array-incr-factor array-incr-factor-set!)
     ;; the actual number of items in vec
     (mutable length array-length array-length-set!)))

  (define-record-type ($fxarray mk-fxarray fxarray?)
    (parent $array))
  (define-record-type ($flarray mk-flarray flarray?)
    (parent $array))
  (define-record-type ($u8array mk-u8array u8array?)
    (parent $array))

  (define u8? (lambda (x) (and (fixnum? x) (fx<= 0 x 255))))
  ;; default min capacity
  (define *mincap* 64)

  (define all-arrays?   (lambda (x*) (andmap array?   x*)))
  (define all-fxarrays? (lambda (x*) (andmap fxarray? x*)))
  (define all-flarrays? (lambda (x*) (andmap flarray? x*)))
  (define all-u8arrays? (lambda (x*) (andmap u8array? x*)))


  (define-syntax define-array-procedure
    (lambda (stx)
      (define valid-ty*?
        (lambda (ty*)
          (if (null? (remp (lambda (x) (memq x '(a fxa u8a))) ty*))
              #t
              (syntax-error ty* "define-array-procedure: bad array type flags (allowed ones: a fxa u8a):"))))
      (define handle-ty*
        (lambda (ty*)
          (values (memq 'a ty*) (memq 'fxa ty*) (memq 'u8a ty*))))
      (define get-name
        (lambda (which name)
          (let ([n (symbol->string (syntax->datum name))]
                [pre1 '((a . array-) (fxa . fxarray-) (u8a . u8array-))])
            ($construct-name name (cdr (assoc which pre1)) n))))
      (syntax-case stx ()
        ;; case-lambda
        [(k (ty* ...) name [args body body* ...] ...)
         (and (identifier? #'name) (valid-ty*? (datum (ty* ...))))
         (let-values ([(pa? pfxa? pu8a?) (handle-ty* (datum (ty* ...)))])
           (with-implicit (k v v? vmake vref vset! vcopy! vlength vpcheck vcheck-length all-which? thisproc who
                             t+ t- t* t/ t+id t*id t> t<
                             a amake aadd! a? avec alength avec-set! apcheck aval?)
             #`(begin
                 #,(if pa?
                       (with-syntax ([name (get-name 'a #'name)])
                         #`(module (name)
                             (define a         array)
                             (define amake     make-array)
                             (define aadd!     array-add!)
                             (define a?        array?)
                             (define avec      array-vec)
                             (define avec-set! array-vec-set!)
                             (define alength   array-length)
                             (define aval?     (lambda (x) #t))
                             (define v     vector)
                             (define v?    vector?)
                             (define vmake make-vector)
                             (define vref  vector-ref)
                             (define vset! vector-set!)
                             (define vlength vector-length)
                             (define vcopy!  vector-copy!)
                             ;;(define vcheck-length check-length)
                             (define all-which? all-arrays?)
                             (define who 'name)
                             (define t+ +)   (define t- -)
                             (define t* *)   (define t/ /)
                             (define t+id 0) (define t*id 1)
                             (define t> >)   (define t< <)
                             (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-vector e* (... ...))])]
                                          [apcheck (syntax-rules () [(_ (a* (... ...)) e* (... ...)) (pcheck ([array? a* (... ...)]) e* (... ...))])])
                               (define name
                                 (case-lambda
                                   [args body body* ...] ...))
                               (define thisproc name))))
                       ;; to create a proper definition context
                       #'(define dummy0 'dummy))
                 #,(if pfxa?
                       (with-syntax ([name (get-name 'fxa #'name)])
                         #`(module (name)
                             (define a         fxarray)
                             (define amake     make-fxarray)
                             (define aadd!     fxarray-add!)
                             (define a?        fxarray?)
                             (define avec      array-vec)
                             (define avec-set! array-vec-set!)
                             (define alength   array-length)
                             (define aval?     (lambda (x) (unless (fixnum? x) (errorf 'name "not a fixnum: ~a" x))))
                             (define v     fxvector)
                             (define v?    fxvector?)
                             (define vmake make-fxvector)
                             (define vref  fxvector-ref)
                             (define vset! fxvector-set!)
                             (define vlength fxvector-length)
                             (define vcopy!  fxvcopy!)
                             ;;(define vcheck-length check-fxlength)
                             (define all-which? all-fxarrays?)
                             (define who 'name)
                             (define t+ fx+)
                             (define t- fx-)
                             (define t* fx*)
                             (define t/ fx/)
                             (define t+id 0)
                             (define t*id 1)
                             (define t> fx>)   (define t< fx<)
                             (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-fxvector e* (... ...))])]
                                          [apcheck (syntax-rules () [(_ (a* (... ...)) e* (... ...)) (pcheck ([fxarray? a* (... ...)]) e* (... ...))])])
                               (define name
                                 (case-lambda
                                   [args body body* ...] ...))
                               (define thisproc name))))
                       #'(define dummy1 'dummy))
                 #,(if pu8a?
                       (with-syntax ([name (get-name 'u8a #'name)])
                         #`(module (name)
                             (define a         u8array)
                             (define amake     make-u8array)
                             (define aadd!     u8array-add!)
                             (define a?        u8array?)
                             (define avec      array-vec)
                             (define avec-set! array-vec-set!)
                             (define alength   array-length)
                             (define aval?     (lambda (x) (unless (u8? x) (errorf 'name "not a byte: ~a" x))))
                             (define v     bytevector)
                             (define v?    bytevector?)
                             (define vmake make-bytevector)
                             (define vref  bytevector-u8-ref)
                             (define vset! bytevector-u8-set!)
                             (define vlength bytevector-length)
                             (define vcopy!  bytevector-copy!)
                             ;;(define vcheck-length check-u8length)
                             (define all-which? all-u8arrays?)
                             (define who 'name)
                             (define t+ fx+)
                             (define t- fx-)
                             (define t* fx*)
                             (define t/ fx/)
                             (define t+id 0)
                             (define t*id 1)
                             (define t> fl>)   (define t< fl<)
                             (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-bytevector e* (... ...))])]
                                          [apcheck (syntax-rules () [(_ (a* (... ...)) e* (... ...)) (pcheck ([u8array? a* (... ...)]) e* (... ...))])])
                               (define name
                                 (case-lambda
                                   [args body body* ...] ...))
                               (define thisproc name))))
                       #'(define dummy2 'dummy)))))]
        ;; lambda
        [(k (ty* ...) (name . args) body* ...)
         (and (identifier? #'name) (valid-ty*? (datum (ty* ...))))
         (let-values ([(pa? pfxa? pu8a?) (handle-ty* (datum (ty* ...)))])
           (with-implicit (k v? vmake vref vset! vlength vcopy! vpcheck vcheck-length all-which? thisproc who
                             t+ t- t* t/ t+id t*id t> t<
                             a amake aadd! a? avec alength avec-set! apcheck aval?)
             #`(begin
                 #,(if pa?
                       (with-syntax ([name (get-name 'a #'name)])
                         #`(define name
                             (lambda args
                               (let ([a         array]
                                     [amake     make-array]
                                     [aadd!     array-add!]
                                     [a?        array?]
                                     [avec      array-vec]
                                     [avec-set! array-vec-set!]
                                     [alength   array-length]
                                     ;; check value type
                                     [aval?     (lambda (x) #t)]
                                     [v     vector]
                                     [v?    vector?]
                                     [vmake make-vector]
                                     [vref  vector-ref]
                                     [vset! vector-set!]
                                     [vlength vector-length]
                                     [vcopy!  vector-copy!]
                                     ;;[vcheck-length check-length]
                                     [all-which? all-arrays?]
                                     [who 'name]
                                     [thisproc name]
                                     [t+ +] [t- -] [t* *] [t/ /] [t+id 0] [t*id 1]
                                     [t> >] [t< <])
                                 ;; this piece of syntax needs care
                                 (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-vector e* (... ...))])]
                                              [apcheck (syntax-rules () [(_ (a* (... ...)) e* (... ...)) (pcheck ([array? a* (... ...)]) e* (... ...))])])
                                   body* ...)))))
                       ;; to create a proper definition context
                       #'(define dummy0 'dummy))
                 #,(if pfxa?
                       (with-syntax ([name (get-name 'fxa #'name)])
                         #`(define name
                             (lambda args
                               (let ([a         fxarray]
                                     [amake     make-fxarray]
                                     [aadd!     fxarray-add!]
                                     [a?        fxarray?]
                                     [avec      array-vec]
                                     [avec-set! array-vec-set!]
                                     [alength   array-length]
                                     [aval? (lambda (x) (unless (fixnum? x) (errorf 'name "not a fixnum: ~a" x)))]
                                     [v     fxvector]
                                     [v?    fxvector?]
                                     [vmake make-fxvector]
                                     [vref  fxvector-ref]
                                     [vset! fxvector-set!]
                                     [vlength fxvector-length]
                                     [vcopy!  fxvcopy!]
                                     ;;[vcheck-length check-fxlength]
                                     [all-which? all-fxarrays?]
                                     [who 'name]
                                     [thisproc name]
                                     [t+ fx+] [t- fx-] [t* fx*] [t/ fx/] [t+id 0] [t*id 1]
                                     [t> fx>] [t< fx<])
                                 (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-fxvector e* (... ...))])]
                                              [apcheck (syntax-rules () [(_ (a* (... ...)) e* (... ...)) (pcheck ([fxarray? a* (... ...)]) e* (... ...))])])
                                   body* ...)))))
                       #'(define dummy1 'dummy))
                 #,(if pu8a?
                       (with-syntax ([name (get-name 'u8a #'name)])
                         #`(define name
                             (lambda args
                               (let ([a         u8array]
                                     [amake     make-u8array]
                                     [aadd!     u8array-add!]
                                     [a?        u8array?]
                                     [avec      array-vec]
                                     [avec-set! array-vec-set!]
                                     [alength   array-length]
                                     [aval?     (lambda (x) (unless (u8? x) (errorf 'name "not a byte: ~a" x)))]
                                     [v     bytevector]
                                     [v?    bytevector?]
                                     [vmake make-bytevector]
                                     [vref  bytevector-u8-ref]
                                     [vset! bytevector-u8-set!]
                                     [vlength bytevector-length]
                                     [vcopy!  bytevector-copy!]
                                     ;;[vcheck-length check-u8length]
                                     [all-which? all-u8arrays?]
                                     [who 'name]
                                     [thisproc name]
                                     [t+ fx+] [t- fx-] [t* fx*] [t/ fx/] [t+id 0] [t*id 1]
                                     [t> fx>] [t< fx<])
                                 (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-bytevector e* (... ...))])]
                                              [apcheck (syntax-rules () [(_ (a* (... ...)) e* (... ...)) (pcheck ([u8array? a* (... ...)]) e* (... ...))])])
                                   body* ...)))))
                       #'(define dummy2 'dummy)))))])))


  (define $make-array
    (lambda (who cap v len)
      (pcheck ([natural? cap len])
              (mk-array (make-vector cap v) 2 len))))
  (define $make-fxarray
    (lambda (who cap v len)
      (pcheck ([natural? cap len] [fixnum? v])
              (mk-fxarray (make-fxvector cap v) 2 len))))
  (define $make-u8array
    (lambda (who cap v len)
      (pcheck ([natural? cap len] [u8? v])
              (mk-u8array (make-bytevector cap v) 2 len))))


  #|doc
  Create an array.

  If no arguments are given, an empty array is created.
  If `len` is given, an array with `len` items all set to #f is returned.
  If both `len` and `v` are given, an array with `len` items all set to `v` is returned.

  The other types of array makers have similar semantics, with the exception that
  if only `len` is given, the items are set to 0 by default.
  |#
  (define-who make-array
    (case-lambda
      [()      ($make-array who *mincap* #f 0)]
      [(len)   ($make-array who len      #f len)]
      [(len v) ($make-array who len      v  len)]))

  (define-who make-fxarray
    (case-lambda
      [()      ($make-fxarray who *mincap* #f 0)]
      [(len)   ($make-fxarray who len      0  len)]
      [(len v) ($make-fxarray who len      v  len)]))

  (define-who make-u8array
    (case-lambda
      [()      ($make-u8array who *mincap* #f 0)]
      [(len)   ($make-u8array who len      0  len)]
      [(len v) ($make-u8array who len      v  len)]))


  #|doc
  Create an array from the given arguments.
  |#
  (define-who array
    (lambda args
      (let ([len (length args)])
        (let* ([arr (make-array (if (fx< len *mincap*) *mincap* len))] [vec (array-vec arr)])
          (let loop ([i 0] [args args])
            (if (null? args)
                (begin (array-length-set! arr len)
                       arr)
                (begin (vector-set! vec i (car args))
                       (loop (fx1+ i) (cdr args)))))))))

  (define-who fxarray
    (lambda args
      (unless (andmap fixnum? args)
        (errorf who "arguments must be fixnums: ~a" args))
      (let ([len (length args)])
        (let* ([arr (make-fxarray (if (fx< len *mincap*) *mincap* len))] [vec (array-vec arr)])
          (let loop ([i 0] [args args])
            (if (null? args)
                (begin (array-length-set! arr len)
                       arr)
                (begin (fxvector-set! vec i (car args))
                       (loop (fx1+ i) (cdr args)))))))))

  (define-who u8array
    (lambda args
      (unless (andmap u8? args)
        (errorf who "arguments must be bytes: ~a" args))
      (let ([len (length args)])
        (let* ([arr (make-u8array (if (fx< len *mincap*) *mincap* len))] [vec (array-vec arr)])
          (let loop ([i 0] [args args])
            (if (null? args)
                (begin (array-length-set! arr len)
                       arr)
                (begin (bytevector-u8-set! vec i (car args))
                       (loop (fx1+ i) (cdr args)))))))))


  #|doc
  Return the length of the array.
  |#
  (define-array-procedure (fxa u8a)
    (length arr)
    (apcheck (arr)
             (array-length arr)))


  #|doc
  Return whether the array is empty.
  |#
  (define-array-procedure (a fxa u8a)
    (empty? arr)
    (apcheck (arr)
             (fx= 0 (array-length arr))))


  (define $grow-array!
    (lambda (arr)
      (let* ([len (array-length arr)] [vec (array-vec arr)]
             [vmake (cond [(vector?     vec) make-vector]
                          [(fxvector?   vec) make-fxvector]
                          [(bytevector? vec) make-bytevector]
                          [else (assert-unreachable)])]
             [vlength (cond [(vector?     vec) vector-length]
                            [(fxvector?   vec) fxvector-length]
                            [(bytevector? vec) bytevector-length]
                            [else (assert-unreachable)])]
             [vcopy! (cond [(vector?     vec) vcopy!]
                           [(fxvector?   vec) fxvcopy!]
                           [(bytevector? vec) bytevector-copy!]
                           [else (assert-unreachable)])]
             [cap (vlength vec)]
             [newvec (vmake (fx* (if (fx= cap 0) *mincap* cap) (array-incr-factor arr)) 0)])
        (printf "growing array from ~a to ~a~n" cap (vlength newvec))
        (vcopy! vec 0 newvec 0 len)
        (array-vec-set! arr newvec))))


  #|doc
  Add a value either to the end of the array `arr` or at a specified index.
  |#
  ;; this is used in `define-array-procdure`, so need to defined separately
  (define-syntax define-array-add!
    (syntax-rules ()
      [(_ thisproc a? aval? vlength vset! vcopy!)
       (define thisproc
         (case-lambda
           [(arr v) (pcheck ([a? arr]) (thisproc arr (array-length arr) v))]
           [(arr i v)
            (pcheck ([a? arr] [natural? i]) (aval? v)
                    (let* ([len (array-length arr)] [vec (array-vec arr)] [cap (vlength vec)])
                      (when (fx> i len) (errorf 'thisproc "index ~a out of range ~a" i len))
                      (when (fx= len cap) ($grow-array! arr))
                      (when (fx< i len) (vcopy! (array-vec arr) i (array-vec arr) (fx1+ i) (fx- len i)))
                      (vset! (array-vec arr) i v)
                      (array-length-set! arr (fx1+ len))))]))]))

  (define-array-add! array-add!   array?   (lambda (x) #t) vector-length     vector-set!        vcopy!)
  (define-array-add! fxarray-add! fxarray? fixnum?         fxvector-length   fxvector-set!      fxvcopy!)
  (define-array-add! u8array-add! u8array? u8?             bytevector-length bytevector-u8-set! u8vcopy!)


  #|doc
  Add multiple values either to the end of the array `arr` or at a specified index.
  This is faster than `array-add!` when adding multiple values.
  |#
  (define-array-procedure (a fxa u8a) add*!
    [(arr v . v*) (apcheck (arr) (apply thisproc arr (alength arr) v v*))]
    [(arr i v . v*)
     (apcheck (arr)
              (pcheck ([natural? i])
                      (let ([len (alength arr)] [vec (array-vec arr)])
                        (cond
                         [(fx= i len) (todo)]
                         [(fx< i len) (todo)]
                         [else (errorf who "index ~a out of range ~a" i len)]))))])


  #|doc
  Return the value at the specified index.
  TODO default value?
  |#
  (define-array-procedure (a fxa u8a)
    (ref arr i)
    (apcheck (arr)
             (pcheck ([natural? i])
                     (let* ([len (alength arr)] [vec (array-vec arr)])
                       (if (and (fx<= 0 i) (fx< i len))
                           (vref vec i)
                           (errorf who "index ~a out of range ~a" i len))))))


  #|doc
  Update the value in the array at the specified index.
  |#
  (define-array-procedure (a fxa u8a)
    (set! arr i v)
    (apcheck (arr)
             (pcheck ([natural? i])
                     (aval? v)
                     (let* ([len (alength arr)] [vec (array-vec arr)])
                       (if (and (fx<= 0 i) (fx< i len))
                           (vset! vec i v)
                           (errorf who "index ~a out of range ~a" i len))))))


  #|doc
  Delete the value at the specified index.
  |#
  (define-array-procedure (a fxa u8a)
    (delete! arr i)
    (apcheck (arr)
             (pcheck ([natural? i])
                     (let ([len (array-length arr)] [vec (array-vec arr)])
                       (when (fx>= i len) (errorf who "index ~a out of range ~a" i len))
                       (cond [(fx= i 0) (vcopy! vec 1 vec 0 (fx- len 1))]
                             [(fx= i (fx1- len)) (void)]
                             [else (vcopy! vec i vec (fx1- i) (fx- len i))])
                       (array-length-set! arr (fx1- len))))))

  ;; TODO delete in range


  #|doc
  Remove all items in the array.
  |#
  (define-array-procedure (a fxa u8a)
    (clear! arr)
    (apcheck (arr)
             ;; just set length to 0 for now
             (array-length-set! arr 0)))


  #|doc
  Apply `pred` to every item of the array `arr` and return a new array
  of the items of `arr` for which `pred` returns #t.
  |#
  (define-array-procedure (a fxa u8a)
    (filter pred arr)
    (apcheck (arr)
             (pcheck ([procedure? pred])
                     (let ([newarr (amake)] [len (array-length arr)] [vec (array-vec arr)])
                       (let loop ([i 0])
                         (if (fx= i len)
                             newarr
                             (let ([v (vref vec i)])
                               (when (pred v) (aadd! newarr v))
                               (loop (fx1+ i)))))))))


  #|doc
  Similar to `array-filter`, but array `arr` is modified in place to contain
  only items `x` such that `(pred x)` returns #t.
  |#
  (define-array-procedure (a fxa u8a)
    (filter! pred arr)
    (apcheck (arr)
             (pcheck ([procedure? pred])
                     ;; This may incur memory waste when the remaining items are few...
                     (let ([len (array-length arr)] [vec (array-vec arr)])
                       ;; i: store index, j: scan index
                       (let loop ([i 0] [j 0])
                         (if (fx= j len)
                             (array-length-set! arr i)
                             (let ([v (vref vec j)])
                               (if (pred v)
                                   (begin (unless (fx= i j)
                                            (vset! vec i v))
                                          (loop (fx1+ i) (fx1+ j)))
                                   (loop i (fx1+ j))))))))))


  #|doc
  Return two arrays, the first array contains values `x` such that `(proc x)` returns #t,
  the second contains values `x` such that `(proc x)` returns #f.
  |#
  (define-array-procedure (a fxa u8a)
    (partition proc arr)
    (apcheck (arr)
             (pcheck ([procedure? proc])
                     (let ([T (amake)] [F (amake)] [len (array-length arr)] [vec (array-vec arr)])
                       (let loop ([i 0])
                         (if (fx= i len)
                             (values T F)
                             (let ([v (vref vec i)])
                               (if (proc v)
                                   (aadd! T v)
                                   (aadd! F v))
                               (loop (fx1+ i)))))))))


  #|doc
  Return a new array whose items are those from the given array, in the given order.
  |#
  (define-array-procedure (a fxa u8a)
    (append arr . arr*)
    (apcheck (arr)
             (pcheck ([all-which? arr*])
                     (let* ([arr* (cons arr arr*)]
                            [len (apply fx+ (map array-length arr*))]
                            [newarr (amake len)] [newvec (array-vec newarr)])
                       (array-length-set! newarr len)
                       (let next ([i 0] [arr* arr*])
                         (if (null? arr*)
                             newarr
                             (let* ([arr (car arr*)] [vec (array-vec arr)] [len (array-length arr)])
                               (let loop ([i i] [j 0])
                                 (if (fx= j len)
                                     (next i (cdr arr*))
                                     (begin (vset! newvec i (vref vec j))
                                            (loop (fx1+ i) (fx1+ j))))))))))))


  #|doc
  Imperatively append items of given arrays `arr*` to array `arr`,
  then return the first array `arr`.
  |#
  (define-array-procedure (a fxa u8a)
    (append! arr . arr*)
    (apcheck (arr)
             (unless (null? arr*)
               (pcheck ([all-which? arr*])
                       (let* ([len1 (array-length arr)] [vec1 (array-vec arr)] [cap1 (vlength vec1)]
                              [len* (apply fx+ (map array-length arr*))]
                              [fillvec! (lambda (tgtvec i arr*)
                                          (let next ([i i] [arr* arr*])
                                            (unless (null? arr*)
                                              (let* ([arr (car arr*)]
                                                     [vec (array-vec arr)] [len (array-length arr)])
                                                (let loop ([i i] [j 0])
                                                  (if (fx= j len)
                                                      (next i (cdr arr*))
                                                      (begin (vset! tgtvec i (vref vec j))
                                                             (loop (fx1+ i) (fx1+ j)))))))))])
                         (if (fx<= len* (fx- cap1 len1))
                             (fillvec! vec1 len1 arr*)
                             (let ([newvec (vmake (fx+ len1 len*))])
                               (fillvec! newvec 0 (cons arr arr*))
                               (array-vec-set! arr newvec)))
                         (array-length-set! arr (fx+ len1 len*))
                         arr)))))



  #|doc
  Return a newly allocated array consisting of the items of `arr` in reverse order.
  |#
  (define-array-procedure (a fxa u8a)
    (reverse arr)
    (apcheck (arr)
             (let* ([len (array-length arr)] [vec (array-vec arr)]
                    [newarr (amake len)]     [newvec (array-vec newarr)])
               (let loop ([i 0] [j (fx1- len)])
                 (unless (fx= i len)
                   (vset! newvec j (vref i vec))
                   (loop (fx1+ i) (fx1- j))))
               (array-length-set! newarr len))))


  #|doc
  Reverse the items in the array in place, then return the array.
  |#
  (define-array-procedure (a fxa u8a)
    (reverse! arr)
    (apcheck (arr)
             (let ([len (array-length arr)] [vec (array-vec arr)])
               (let loop ([i 0] [j (fx1- len)])
                 (when (fx< i j)
                   (let ([x (vref vec i)] [y (vref vec j)])
                     (vset! vec i y)
                     (vset! vec j x)
                     (loop (fx1+ i) (fx1- j)))))
               arr)))


  #|doc
  Return whether the array contains the given item.
  Items are compared using `equal?`.
  |#
  (define-array-procedure (a fxa u8a)
    (contains? arr v)
    (apcheck (arr)
             (aval? v)
             (let ([len (array-length arr)] [vec (array-vec arr)])
               (let loop ([i 0])
                 (if (fx= i len)
                     #f
                     (if (equal? v (vref vec i))
                         #t
                         (loop (fx1+ i))))))))


  #|doc
  Return whether the array contains the given item.
  Items are compared using the given procedure `=?`, hence "/p" in the name.
  |#
  (define-array-procedure (a fxa u8a)
    (contains/p? =? arr)
    (apcheck (arr)
             (let ([len (array-length arr)] [vec (array-vec arr)])
               (let loop ([i 0])
                 (if (fx= i len)
                     #f
                     (if (=? (vref vec i))
                         #t
                         (loop (fx1+ i))))))))


  #|doc
  Return the first item in the array that satisfies the predicate `pred`.
  If no such item is found, #f is returned.
  |#
  (define-array-procedure (a fxa u8a)
    (search pred arr)
    (apcheck (arr)
             (let ([len (array-length arr)] [vec (array-vec arr)])
               (let loop ([i 0])
                 (if (fx= i len)
                     #f
                     (let ([v (vref vec i)])
                       (if (pred v)
                           v
                           (loop (fx1+ i)))))))))


  #|doc
  Search for items in array `dl` that satisfies the predicate `pred`.

  By default the items satisfying `pred` are returned in a list.

  The `collect` argument has the same semantics as in `dlist-search*`.
  |#
  (define-array-procedure (a fxa u8a) search*
    [(pred arr)
     (apcheck (arr)
              (pcheck ([procedure? pred])
                      (let ([lb (make-list-builder)])
                        (thisproc pred arr (lambda (x) (lb x)))
                        (lb))))]
    [(pred arr collect)
     (apcheck (arr)
              (pcheck ([pred collect])
                      (let ([len (array-length arr)] [vec (array-vec arr)])
                        (let loop ([i 0])
                          (unless (fx= i len)
                            (let ([v (vref vec i)])
                              (when (pred v) (collect v))
                              (loop (fx1+ i))))))))])


  (define-array-procedure (a fxa u8a) slice
    [(arr end) (thisproc arr 0 end 1)]
    [(arr start end) (thisproc arr start end 1)]
    [(arr start end step)
     (pcheck ([a? arr] [fixnum? start end step])
             (todo who))])


  (define-array-procedure (a fxa u8a) slice!
    [(arr end) (thisproc arr 0 end 1)]
    [(arr start end) (thisproc arr start end 1)]
    [(arr start end step)
     (pcheck ([a? arr] [fixnum? start end step])
             (todo who))])


  (define-array-procedure (a fxa u8a)
    (copy! src src-start tgt tgt-start k)
    (apcheck (src tgt)
             (pcheck ([natural? src-start tgt-start k])
                     (let ([len1 (array-length src)] [vec1 (array-vec src)]
                           [len2 (array-length tgt)] [vec2 (array-vec tgt)])
                       (when (> (fx+ src-start k) len1)
                         (errorf who "range ~a is too large in source array" k))
                       (when (> (fx+ tgt-start k) len2)
                         (errorf who "range ~a is too large in target array" k))
                       (when (fx> k 0)
                         (if (eq? src tgt)
                             (let ([src-end (fx+ src-start k)] [tgt-end (fx+ tgt-start k)])
                               (cond
                                [(or
                                  ;; disjoint, left to right
                                  (fx<= src-end tgt-start)
                                  ;; disjoint, right to left
                                  (fx<= tgt-end src-start)
                                  ;; overlapping, right to left
                                  (fx<= tgt-start src-start))
                                 (let loop ([i src-start] [j tgt-start] [k k])
                                   (unless (fx= k 0)
                                     (vset! vec2 j (vref vec1 i))
                                     (loop (fx1+ i) (fx1+ j) (fx1- k))))]
                                [(fx< src-start tgt-start)
                                 ;; overlapping, left to right, copy from last to first
                                 (let loop ([i (fx1- src-end)] [j (fx1- tgt-end)] [k k])
                                   (unless (fx= k 0)
                                     (vset! vec2 j (vref vec1 i))
                                     (loop (fx1- i) (fx1- j) (fx1- k))))]
                                [else (assert-unreachable)]))
                             (let loop ([i src-start] [j tgt-start] [k k])
                               (unless (fx= k 0)
                                 (vset! vec2 j (vref vec1 i))
                                 (loop (fx1+ i) (fx1+ j) (fx1- k))))))))))



  (define-array-procedure (a fxa u8a)
    (sort arr <)
    (todo))


  (define-array-procedure (a fxa u8a)
    (sort! arr <)
    (todo))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   iterations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define check-length
    (case-lambda
      [(who arr0 arr1)
       (unless (fx= (array-length arr0) (array-length arr1))
         (errorf who "arrays are not of the same length"))]
      [(who arr0 . arr*)
       (unless (null? arr*)
         (unless (apply fx= (array-length arr0) (map array-length arr*))
           (errorf who "arrays are not of the same length")))]))


  (define-array-procedure (a fxa u8a) map
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              newarr
                              (begin (vset! newvec i (proc (vref vec0 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length arr0 arr1)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              newarr
                              (begin (vset! newvec i (proc (vref vec0 i) (vref vec1 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length arr0 arr*)
             (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)]
                    [newarr (amake len0)]      [newvec (array-vec newarr)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     newarr
                     (begin (vset! newvec i (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1+ i)))))))])


  (define-array-procedure (a fxa u8a) map/i
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              newarr
                              (begin (vset! newvec i (proc i (vref vec0 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (check-length arr0 arr1)
             (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)]
                    [newarr (amake len0)]      [newvec (array-vec newarr)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     newarr
                     (begin (vset! newvec i (proc i (vref vec0 i) (vref vec1 i)))
                            (loop (fx1+ i)))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length arr0 arr*)
             (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)]
                    [newarr (amake len0)]      [newvec (array-vec newarr)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     newarr
                     (begin (vset! newvec i (apply proc i (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1+ i)))))))])


;;;; in-place maps

  (define-array-procedure (a fxa u8a) map!
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              arr0
                              (begin (vset! vec0 i (proc (vref vec0 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length arr0 arr1)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              arr0
                              (begin (vset! vec0 i (proc (vref vec0 i) (vref vec1 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length arr0 arr*)
             (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     arr0
                     (begin (vset! vec0 i (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1+ i)))))))])


  (define-array-procedure (a fxa u8a) map/i!
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              arr0
                              (begin (vset! vec0 i (proc i (vref vec0 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length arr0 arr1)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              arr0
                              (begin (vset! vec0 i (proc i (vref vec0 i) (vref vec1 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length arr0 arr*)
             (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     arr0
                     (begin (vset! vec0 i (apply proc i (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1+ i)))))))])


  (define-array-procedure (a fxa u8a) for-each
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i 0])
                          (unless (fx= i len0)
                            (proc (vref vec0 i))
                            (loop (fx1+ i)))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length arr0 arr1)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i 0])
                          (unless (fx= i len0)
                            (proc (vref vec0 i) (vref vec1 i))
                            (loop (fx1+ i)))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length arr0 arr*)
             (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i 0])
                 (unless (fx= i len0)
                   (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                   (loop (fx1+ i))))))])


  (define-array-procedure (a fxa u8a) for-each/i
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i 0])
                          (unless (fx= i len0)
                            (proc i (vref vec0 i))
                            (loop (fx1+ i)))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length arr0 arr1)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i 0])
                          (unless (fx= i len0)
                            (proc i (vref vec0 i) (vref vec1 i))
                            (loop (fx1+ i)))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length arr0 arr*)
             (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i 0])
                 (unless (fx= i len0)
                   (apply proc i (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                   (loop (fx1+ i))))))])


;;;; reverse order

  (define-array-procedure (a fxa u8a) map-rev
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i (fx1- len0)] [j 0])
                          (if (fx= i -1)
                              newarr
                              (begin (vset! newvec j (proc (vref vec0 i)))
                                     (loop (fx1- i) (fx1+ j))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length arr0 arr1)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i (fx1- len0)] [j 0])
                          (if (fx= i -1)
                              newarr
                              (begin (vset! newvec j (proc (vref vec0 i) (vref vec1 i)))
                                     (loop (fx1- i) (fx1+ j))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length arr0 arr*)
             (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)]
                    [newarr (amake len0)]      [newvec (array-vec newarr)])
               (let loop ([i (fx1- len0)] [j 0])
                 (if (fx= i -1)
                     newarr
                     (begin (vset! newvec j (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1- i) (fx1+ j)))))))])


  (define-array-procedure (a fxa u8a) map/i-rev
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i (fx1- len0)] [j 0])
                          (if (fx= i -1)
                              newarr
                              (begin (vset! newvec j (proc i (vref vec0 i)))
                                     (loop (fx1- i) (fx1+ j))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length arr0 arr1)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i (fx1- len0)] [j 0])
                          (if (fx= i -1)
                              newarr
                              (begin (vset! newvec j (proc i (vref vec0 i) (vref vec1 i)))
                                     (loop (fx1- i) (fx1+ j))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length arr0 arr*)
             (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)]
                    [newarr (amake len0)]      [newvec (array-vec newarr)])
               (let loop ([i (fx1- len0)] [j 0])
                 (if (fx= i -1)
                     newarr
                     (begin (vset! newvec j (apply proc i (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1- i) (fx1+ j)))))))])


  (define-array-procedure (a fxa u8a) for-each-rev
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i (fx1- len0)])
                          (unless (fx= i -1)
                            (proc (vref vec0 i))
                            (loop (fx1- i)))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length arr0 arr1)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i (fx1- len0)])
                          (unless (fx= i -1)
                            (proc (vref vec0 i) (vref vec1 i))
                            (loop (fx1- i)))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length arr0 arr*)
             (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i (fx1- len0)] [j 0])
                 (unless (fx= i -1)
                   (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                   (loop (fx1- i) (fx1+ j))))))])


  (define-array-procedure (a fxa u8a) for-each/i-rev
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i (fx1- len0)])
                          (unless (fx= i -1)
                            (proc i (vref vec0 i))
                            (loop (fx1- i)))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length arr0 arr1)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i (fx1- len0)])
                          (unless (fx= i -1)
                            (proc i (vref vec0 i) (vref vec1 i))
                            (loop (fx1- i)))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length arr0 arr*)
             (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i (fx1- len0)] [j 0])
                 (unless (fx= i -1)
                   (apply proc i (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                   (loop (fx1- i) (fx1+ j))))))])


;;;; folds


  (define-array-procedure (a fxa u8a) fold-left
    [(proc acc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)])
                        (let loop ([acc acc] [i 0])
                          (if (fx= i len0)
                              acc
                              (loop (proc acc (vref vec0 i))
                                    (fx1+ i)))))))]
    [(proc acc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length arr0 arr1)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([acc acc] [i 0])
                          (if (fx= i len0)
                              acc
                              (loop (proc acc (vref vec0 i) (vref vec1 i))
                                    (fx1+ i)))))))]
    [(proc acc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length arr0 arr*)
             (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([acc acc] [i 0])
                 (if (fx= i len0)
                     acc
                     (loop (apply proc acc (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                           (fx1+ i))))))])


  (define-array-procedure (a fxa u8a) fold-left/i
    [(proc acc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)])
                        (let loop ([acc acc] [i 0])
                          (if (fx= i len0)
                              acc
                              (loop (proc i acc (vref vec0 i))
                                    (fx1+ i)))))))]
    [(proc acc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length arr0 arr1)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([acc acc] [i 0])
                          (if (fx= i len0)
                              acc
                              (loop (proc i acc (vref vec0 i) (vref vec1 i))
                                    (fx1+ i)))))))]
    [(proc acc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length arr0 arr*)
             (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([acc acc] [i 0])
                 (if (fx= i len0)
                     acc
                     (loop (apply proc i acc (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                           (fx1+ i))))))])


  (define-array-procedure (a fxa u8a) fold-right
    [(proc acc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)])
                        (let loop ([acc acc] [i (fx1- len0)])
                          (if (fx= i -1)
                              acc
                              (loop (proc (vref vec0 i) acc)
                                    (fx1- i)))))))]
    [(proc acc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([acc acc] [i (fx1- len0)])
                          (if (fx= i -1)
                              acc
                              (loop (proc (vref vec0 i) (vref vec1 i) acc)
                                    (fx1- i)))))))]
    [(proc acc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length arr0 arr*)
             (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([acc acc] [i (fx1- len0)])
                 (if (fx= i -1)
                     acc
                     (loop (apply proc (vref vec0 i) `(,@(map (lambda (x) (vref x i)) vec*) ,acc))
                           (fx1- i))))))])


  (define-array-procedure (a fxa u8a) fold-right/i
    [(proc acc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)])
                        (let loop ([acc acc] [i (fx1- len0)])
                          (if (fx= i -1)
                              acc
                              (loop (proc i (vref vec0 i) acc)
                                    (fx1- i)))))))]
    [(proc acc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([acc acc] [i (fx1- len0)])
                          (if (fx= i -1)
                              acc
                              (loop (proc i (vref vec0 i) (vref vec1 i) acc)
                                    (fx1- i)))))))]
    [(proc acc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length arr0 arr*)
             (let* ([len0 (array-length arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([acc acc] [i (fx1- len0)])
                 (if (fx= i -1)
                     acc
                     (loop (apply proc i (vref vec0 i) `(,@(map (lambda (x) (vref x i)) vec*) ,acc))
                           (fx1- i))))))])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   conversions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  #|doc
  Convert a list to an array.
  |#
  (define-who list->array
    (lambda (ls)
      (pcheck ([list? ls])
              (apply array ls))))


  #|doc
  Convert a vector to an array.
  |#
  (define-who vector->array
    (lambda (vec)
      (todo)))

  (define-who fxvector->fxarray
    (lambda (vec)
      (todo)))

  (define-who u8vector->u8array
    (lambda (vec)
      (todo)))


  #|doc
  Convert an array to a list.
  |#
  ;; defines {,fx,u8}array->list
  (define-array-procedure (a fxa u8a)
    (>list arr)
    (apcheck (arr)
             (let ([lb (make-list-builder)] [vec (array-vec arr)] [len (array-length arr)])
               (let loop ([i 0])
                 (if (fx= i len)
                     (lb)
                     (begin (lb (vref vec i))
                            (loop (fx1+ i))))))))


  (define-who array->vector
    (lambda (arr)
      (todo)))


  (define-who fxarray->fxvector
    (lambda (arr)
      (todo)))


  (define-who u8array->u8vector
    (lambda (arr)
      (todo)))


  (define-syntax gen-array-record-writer
    (syntax-rules ()
      [(_ arr header vref)
       (record-writer (type-descriptor arr)
                      (lambda (r p wr)
                        (display header p)
                        (let ([v (array-vec r)] [len (array-length r)])
                          (when (fx>= len 1) (wr (vref v 0) p))
                          (when (fx> len 1)
                            (let loop ([i 1])
                              (unless (fx= i len)
                                (display " " p)
                                (wr (vref v i) p)
                                (loop (fx1+ i))))))
                        (display ")]" p)))]))

  (define-syntax gen-array-record-type-equal-procedure
    (syntax-rules ()
      [(_ arr vref)
       (record-type-equal-procedure (type-descriptor arr)
                                    (lambda (arr1 arr2 =?)
                                      (let ([len1 (array-length arr1)] [vec1 (array-vec arr1)]
                                            [len2 (array-length arr2)] [vec2 (array-vec arr2)])
                                        (and (fx= len1 len2)
                                             (let loop ([i 0])
                                               (if (fx= i len1)
                                                   #t
                                                   (and (=? (vref vec1 i) (vref vec2 i))
                                                        (loop (fx1+ i)))))))))]))

  (gen-array-record-writer $array   "#[array ("   vector-ref)
  (gen-array-record-writer $fxarray "#[fxarray (" fxvector-ref)
  (gen-array-record-writer $u8array "#[u8array (" bytevector-u8-ref)

  (gen-array-record-type-equal-procedure $array   vector-ref)
  (gen-array-record-type-equal-procedure $fxarray fxvector-ref)
  (gen-array-record-type-equal-procedure $u8array bytevector-u8-ref)

  )
