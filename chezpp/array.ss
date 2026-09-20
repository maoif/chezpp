(library (chezpp array)
  (export array make-array array? array-size array-empty?
          array-ref array-add! array-add*! array-delete! array-set! array-clear!
          array-slice array-slice! array-copy array-copy!
          array-push! array-pop! array-push-back! array-pop-back!
          array-filter array-filter! array-partition
          array-contains? array-contains/p? array-index-of array-find-index
          array-search array-search*
          array-append array-append!
          array-reverse array-reverse!
          array-map array-map/i array-map! array-map/i!
          array-for-each array-for-each/i
          array-map-rev array-map/i-rev
          array-for-each-rev array-for-each/i-rev
          array-andmap array-ormap
          array-fold-left array-fold-left/i array-fold-right array-fold-right/i
          array-sorted? array-sort array-sort!
          array-iota array-nums


          fxarray make-fxarray fxarray? fxarray-size fxarray-empty?
          fxarray-ref fxarray-add! fxarray-add*! fxarray-delete! fxarray-set! fxarray-clear!
          fxarray-slice fxarray-slice! fxarray-copy fxarray-copy!
          fxarray-push! fxarray-pop! fxarray-push-back! fxarray-pop-back!
          fxarray-filter fxarray-filter! fxarray-partition
          fxarray-contains? fxarray-contains/p? fxarray-index-of fxarray-find-index
          fxarray-search fxarray-search*
          fxarray-append fxarray-append!
          fxarray-reverse fxarray-reverse!
          fxarray-map fxarray-map/i fxarray-map! fxarray-map/i!
          fxarray-for-each fxarray-for-each/i
          fxarray-map-rev fxarray-map/i-rev
          fxarray-for-each-rev fxarray-for-each/i-rev
          fxarray-andmap fxarray-ormap
          fxarray-fold-left fxarray-fold-left/i fxarray-fold-right fxarray-fold-right/i
          fxarray-sorted? fxarray-sort fxarray-sort!
          fxarray-iota fxarray-nums

          flarray make-flarray flarray? flarray-size flarray-empty?
          flarray-ref flarray-add! flarray-add*! flarray-delete! flarray-set! flarray-clear!
          flarray-slice flarray-slice! flarray-copy flarray-copy!
          flarray-push! flarray-pop! flarray-push-back! flarray-pop-back!
          flarray-filter flarray-filter! flarray-partition
          flarray-contains? flarray-contains/p? flarray-index-of flarray-find-index
          flarray-search flarray-search*
          flarray-append flarray-append!
          flarray-reverse flarray-reverse!
          flarray-map flarray-map/i flarray-map! flarray-map/i!
          flarray-for-each flarray-for-each/i
          flarray-map-rev flarray-map/i-rev
          flarray-for-each-rev flarray-for-each/i-rev
          flarray-andmap flarray-ormap
          flarray-fold-left flarray-fold-left/i flarray-fold-right flarray-fold-right/i
          flarray-sorted? flarray-sort flarray-sort!
          flarray-iota flarray-nums
          flarray->list flarray->iter flarray->flvector flvector->flarray

          u8array make-u8array u8array? u8array-size u8array-empty?
          u8array-ref u8array-add! u8array-add*! u8array-delete! u8array-set! u8array-clear!
          u8array-slice u8array-slice! u8array-copy u8array-copy!
          u8array-push! u8array-pop! u8array-push-back! u8array-pop-back!
          u8array-filter u8array-filter! u8array-partition
          u8array-contains? u8array-contains/p? u8array-index-of u8array-find-index
          u8array-search u8array-search*
          u8array-append u8array-append!
          u8array-reverse u8array-reverse!
          u8array-map u8array-map/i u8array-map! u8array-map/i!
          u8array-for-each u8array-for-each/i
          u8array-map-rev u8array-map/i-rev
          u8array-for-each-rev u8array-for-each/i-rev
          u8array-andmap u8array-ormap
          u8array-fold-left u8array-fold-left/i u8array-fold-right u8array-fold-right/i
          u8array-sorted? u8array-sort u8array-sort!
          u8array-iota u8array-nums

          ;; Bytearray is the public name for the raw unsigned-byte array API.
          bytearray make-bytearray bytearray? bytearray-size bytearray-empty?
          bytearray-ref bytearray-add! bytearray-add*! bytearray-delete! bytearray-set! bytearray-clear!
          bytearray-slice bytearray-slice! bytearray-copy bytearray-copy!
          bytearray-push! bytearray-pop! bytearray-push-back! bytearray-pop-back!
          bytearray-filter bytearray-filter! bytearray-partition
          bytearray-contains? bytearray-contains/p? bytearray-index-of bytearray-find-index
          bytearray-search bytearray-search*
          bytearray-append bytearray-append!
          bytearray-reverse bytearray-reverse!
          bytearray-map bytearray-map/i bytearray-map! bytearray-map/i!
          bytearray-for-each bytearray-for-each/i
          bytearray-map-rev bytearray-map/i-rev
          bytearray-for-each-rev bytearray-for-each/i-rev
          bytearray-andmap bytearray-ormap
          bytearray-fold-left bytearray-fold-left/i bytearray-fold-right bytearray-fold-right/i
          bytearray-sorted? bytearray-sort bytearray-sort!
          bytearray-iota bytearray-nums
          bytearray-u16-ref bytearray-U16-ref bytearray-u16-set! bytearray-U16-set!
          bytearray-s16-ref bytearray-S16-ref bytearray-s16-set! bytearray-S16-set!
          bytearray-u8-ref bytearray-U8-ref bytearray-u8-set! bytearray-U8-set!
          bytearray-s8-ref bytearray-S8-ref bytearray-s8-set! bytearray-S8-set!
          bytearray-u16-ref bytearray-U16-ref bytearray-u16-set! bytearray-U16-set!
          bytearray-s16-ref bytearray-S16-ref bytearray-s16-set! bytearray-S16-set!
          bytearray-u24-ref bytearray-U24-ref bytearray-u24-set! bytearray-U24-set!
          bytearray-s24-ref bytearray-S24-ref bytearray-s24-set! bytearray-S24-set!
          bytearray-u32-ref bytearray-U32-ref bytearray-u32-set! bytearray-U32-set!
          bytearray-s32-ref bytearray-S32-ref bytearray-s32-set! bytearray-S32-set!
          bytearray-u40-ref bytearray-U40-ref bytearray-u40-set! bytearray-U40-set!
          bytearray-s40-ref bytearray-S40-ref bytearray-s40-set! bytearray-S40-set!
          bytearray-u48-ref bytearray-U48-ref bytearray-u48-set! bytearray-U48-set!
          bytearray-s48-ref bytearray-S48-ref bytearray-s48-set! bytearray-S48-set!
          bytearray-u56-ref bytearray-U56-ref bytearray-u56-set! bytearray-U56-set!
          bytearray-s56-ref bytearray-S56-ref bytearray-s56-set! bytearray-S56-set!
          bytearray-u64-ref bytearray-U64-ref bytearray-u64-set! bytearray-U64-set!
          bytearray-s64-ref bytearray-S64-ref bytearray-s64-set! bytearray-S64-set!
          bytearray-fp32-ref bytearray-FP32-ref bytearray-fp32-set! bytearray-FP32-set!
          bytearray-fp64-ref bytearray-FP64-ref bytearray-fp64-set! bytearray-FP64-set!
          bytearray-u16-add! bytearray-U16-add! bytearray-u16-delete! bytearray-U16-delete!
          bytearray-u16->list bytearray-U16->list
          bytearray-u16-map bytearray-U16-map bytearray-u16-for-each bytearray-U16-for-each
          bytearray-u16->iter bytearray-U16->iter

          array->list fxarray->list u8array->list bytearray->list
          array->iter fxarray->iter u8array->iter bytearray->iter
          array->vector fxarray->fxvector u8array->u8vector bytearray->bytevector
          vector->array fxvector->fxarray u8vector->u8array bytevector->bytearray)
  (import (chezpp chez)
          (chezpp internal)
          (chezpp list)
          (chezpp vector)
          (chezpp utils)
          (only (chezpp iter) iter-register-source! make-indexed-iter)
          (only (chezpp navigator) nav-register-indexed!))

  ;; TODO allow change incr-factor?
  ;; TODO shrink the array when memory is low?

  #|record:$array
  Mutable array storage shared by generic, fixnum, and byte arrays.
  |#
  (define-record-type ($array mk-array array?)
    (nongenerative)
    (fields
     ;; the backing vector, whose length is the capacity
     (mutable vec array-vec array-vec-set!)
     (mutable incr-factor array-incr-factor array-incr-factor-set!)
     ;; the actual number of items in vec
     (mutable size $array-size $array-size-set!)))

  #|record:$fxarray
  Mutable fixnum-array record derived from `$array`.
  |#
  (define-record-type ($fxarray mk-fxarray fxarray?)
    (parent $array))
  #|record:$flarray
  Mutable flonum-array record derived from `$array`.
  |#
  (define-record-type ($flarray mk-flarray flarray?)
    (parent $array))
  #|record:$u8array
  Mutable unsigned-byte-array record derived from `$array`.
  |#
  (define-record-type ($u8array mk-u8array u8array?)
    (parent $array))

  #|proc:array-size
  Return the number of items in the array.
  |#
  (define-who array-size
    (lambda (arr)
      (pcheck ([array? arr])
              ($array-size arr))))

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
          (if (null? (remp (lambda (x) (memq x '(a fxa fla u8a))) ty*))
              #t
              (syntax-error ty* "define-array-procedure: bad array type flags:"))))
      (define handle-ty*
        (lambda (ty*)
          (values (memq 'a ty*) (memq 'fxa ty*) (memq 'fla ty*) (memq 'u8a ty*))))
      (define get-name
        (lambda (which name)
          (let ([n (symbol->string (syntax->datum name))]
                [pre1 '((a . array-) (fxa . fxarray-) (fla . flarray-) (u8a . u8array-))])
            ($construct-name name (cdr (assoc which pre1)) n))))
      (syntax-case stx ()
        ;; case-lambda
        [(k (ty* ...) name [args body body* ...] ...)
         (and (identifier? #'name) (valid-ty*? (datum (ty* ...))))
         (let-values ([(pa? pfxa? pfla? pu8a?) (handle-ty* (datum (ty* ...)))])
           (with-implicit (k v v? vmake vref vset! vcopy vcopy! vlength vpcheck vcheck-length all-which? thisproc who
                             t+ t- t* t/ t+id t*id t> t<
                             a amk amake aadd! a? avec asize avec-set! apcheck aval?)
             #`(begin
                 #,(if pa?
                       (with-syntax ([name (get-name 'a #'name)])
                         #`(module (name)
                             (define a         array)
                             (define amk       mk-array)
                             (define amake     make-array)
                             (define aadd!     array-add!)
                             (define a?        array?)
                             (define avec      array-vec)
                             (define avec-set! array-vec-set!)
                             (define asize   $array-size)
                             (define aval?     (lambda (x) #t))
                             (define v     vector)
                             (define v?    vector?)
                             (define vmake make-vector)
                             (define vref  vector-ref)
                             (define vset! vector-set!)
                             (define vlength vector-length)
                             (define vcopy!  vector-copy!)
                             (define vcopy   vector-copy)
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
                             (define amk       mk-fxarray)
                             (define amake     make-fxarray)
                             (define aadd!     fxarray-add!)
                             (define a?        fxarray?)
                             (define avec      array-vec)
                             (define avec-set! array-vec-set!)
                             (define asize   $array-size)
                             (define aval?     (lambda (x) (unless (fixnum? x) (errorf 'name "not a fixnum: ~a" x))))
                             (define v     fxvector)
                             (define v?    fxvector?)
                             (define vmake make-fxvector)
                             (define vref  fxvector-ref)
                             (define vset! fxvector-set!)
                             (define vlength fxvector-length)
                             (define vcopy!  fxvcopy!)
                             (define vcopy   fxvector-copy)
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
                 #,(if pfla?
                       (with-syntax ([name (get-name 'fla #'name)])
                         #`(module (name)
                             (define a flarray)
                             (define amk mk-flarray)
                             (define amake make-flarray)
                             (define aadd! flarray-add!)
                             (define a? flarray?)
                             (define avec array-vec)
                             (define avec-set! array-vec-set!)
                             (define asize $array-size)
                             (define aval?
                               (lambda (x)
                                 (unless (flonum? x)
                                   (errorf 'name "not a flonum: ~a" x))))
                             (define v flvector)
                             (define v? flvector?)
                             (define vmake make-flvector)
                             (define vref flvector-ref)
                             (define vset! flvector-set!)
                             (define vlength flvector-length)
                             (define vcopy! flvcopy!)
                             (define vcopy flvector-copy)
                             (define all-which? all-flarrays?)
                             (define who 'name)
                             (define t+ fl+) (define t- fl-)
                             (define t* fl*) (define t/ fl/)
                             (define t+id 0.0) (define t*id 1.0)
                             (define t> fl>) (define t< fl<)
                             (let-syntax
                                 ([vpcheck
                                   (syntax-rules ()
                                     [(_ e* (... ...))
                                      (pcheck-flvector e* (... ...))])]
                                  [apcheck
                                   (syntax-rules ()
                                     [(_ (a* (... ...)) e* (... ...))
                                      (pcheck ([flarray? a* (... ...)]) e* (... ...))])])
                               (define name
                                 (case-lambda
                                   [args body body* ...] ...))
                               (define thisproc name))))
                       #'(define dummy-fl 'dummy))
                 #,(if pu8a?
                       (with-syntax ([name (get-name 'u8a #'name)])
                         #`(module (name)
                             (define a         u8array)
                             (define amk       mk-u8array)
                             (define amake     make-u8array)
                             (define aadd!     u8array-add!)
                             (define a?        u8array?)
                             (define avec      array-vec)
                             (define avec-set! array-vec-set!)
                             (define asize   $array-size)
                             (define aval?     (lambda (x) (unless (u8? x) (errorf 'name "not a byte: ~a" x))))
                             (define v     bytevector)
                             (define v?    bytevector?)
                             (define vmake make-bytevector)
                             (define vref  bytevector-u8-ref)
                             (define vset! bytevector-u8-set!)
                             (define vlength bytevector-length)
                             (define vcopy!  bytevector-copy!)
                             (define vcopy   bytevector-copy)
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
         (let-values ([(pa? pfxa? pfla? pu8a?) (handle-ty* (datum (ty* ...)))])
           (with-implicit (k v? vmake vref vset! vlength vcopy vcopy! vpcheck vcheck-length all-which? thisproc who
                             t+ t- t* t/ t+id t*id t> t<
                             a amk amake aadd! a? avec asize avec-set! apcheck aval?)
             #`(begin
                 #,(if pa?
                       (with-syntax ([name (get-name 'a #'name)])
                         #`(define name
                             (lambda args
                               (let ([a         array]
                                     [amk       mk-array]
                                     [amake     make-array]
                                     [aadd!     array-add!]
                                     [a?        array?]
                                     [avec      array-vec]
                                     [avec-set! array-vec-set!]
                                     [asize   $array-size]
                                     ;; check value type
                                     [aval?     (lambda (x) #t)]
                                     [v     vector]
                                     [v?    vector?]
                                     [vmake make-vector]
                                     [vref  vector-ref]
                                     [vset! vector-set!]
                                     [vlength vector-length]
                                     [vcopy!  vector-copy!]
                                     [vcopy   vector-copy]
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
                                     [amk       mk-fxarray]
                                     [amake     make-fxarray]
                                     [aadd!     fxarray-add!]
                                     [a?        fxarray?]
                                     [avec      array-vec]
                                     [avec-set! array-vec-set!]
                                     [asize   $array-size]
                                     [aval? (lambda (x) (unless (fixnum? x) (errorf 'name "not a fixnum: ~a" x)))]
                                     [v     fxvector]
                                     [v?    fxvector?]
                                     [vmake make-fxvector]
                                     [vref  fxvector-ref]
                                     [vset! fxvector-set!]
                                     [vlength fxvector-length]
                                     [vcopy!  fxvcopy!]
                                     [vcopy   fxvector-copy]
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
                 #,(if pfla?
                       (with-syntax ([name (get-name 'fla #'name)])
                         #`(define name
                             (lambda args
                               (let ([a flarray]
                                     [amk mk-flarray]
                                     [amake make-flarray]
                                     [aadd! flarray-add!]
                                     [a? flarray?]
                                     [avec array-vec]
                                     [avec-set! array-vec-set!]
                                     [asize $array-size]
                                     [aval? (lambda (x)
                                              (unless (flonum? x)
                                                (errorf 'name "not a flonum: ~a" x)))]
                                     [v flvector]
                                     [v? flvector?]
                                     [vmake make-flvector]
                                     [vref flvector-ref]
                                     [vset! flvector-set!]
                                     [vlength flvector-length]
                                     [vcopy! flvcopy!]
                                     [vcopy flvector-copy]
                                     [all-which? all-flarrays?]
                                     [who 'name]
                                     [thisproc name]
                                     [t+ fl+] [t- fl-] [t* fl*] [t/ fl/]
                                     [t+id 0.0] [t*id 1.0]
                                     [t> fl>] [t< fl<])
                                 (let-syntax
                                     ([vpcheck
                                       (syntax-rules ()
                                         [(_ e* (... ...))
                                          (pcheck-flvector e* (... ...))])]
                                      [apcheck
                                       (syntax-rules ()
                                         [(_ (a* (... ...)) e* (... ...))
                                          (pcheck ([flarray? a* (... ...)]) e* (... ...))])])
                                   body* ...)))))
                       #'(define dummy-fl 'dummy))
                 #,(if pu8a?
                       (with-syntax ([name (get-name 'u8a #'name)])
                         #`(define name
                             (lambda args
                               (let ([a         u8array]
                                     [amk       mk-u8array]
                                     [amake     make-u8array]
                                     [aadd!     u8array-add!]
                                     [a?        u8array?]
                                     [avec      array-vec]
                                     [avec-set! array-vec-set!]
                                     [asize   $array-size]
                                     [aval?     (lambda (x) (unless (u8? x) (errorf 'name "not a byte: ~a" x)))]
                                     [v     bytevector]
                                     [v?    bytevector?]
                                     [vmake make-bytevector]
                                     [vref  bytevector-u8-ref]
                                     [vset! bytevector-u8-set!]
                                     [vlength bytevector-length]
                                     [vcopy!  bytevector-copy!]
                                     [vcopy   bytevector-copy]
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
  (define $make-flarray
    (lambda (who cap v len)
      (pcheck ([natural? cap len] [flonum? v])
              (mk-flarray (make-flvector cap v) 2 len))))
  (define $make-u8array
    (lambda (who cap v len)
      (pcheck ([natural? cap len] [u8? v])
              (mk-u8array (make-bytevector cap v) 2 len))))


  #|proc:make-array
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

  #|proc:make-fxarray
  Return a fixnum array of optional length `len`, filled with optional fixnum `v`.
  |#
  (define-who make-fxarray
    (case-lambda
      [()      ($make-fxarray who *mincap* #f 0)]
      [(len)   ($make-fxarray who len      0  len)]
      [(len v) ($make-fxarray who len      v  len)]))

  #|proc:make-flarray
  Return a flonum array of optional length `len`, filled with optional flonum `v`.
  |#
  (define-who make-flarray
    (case-lambda
      [() ($make-flarray who *mincap* 0.0 0)]
      [(len) ($make-flarray who len 0.0 len)]
      [(len v) ($make-flarray who len v len)]))

  #|proc:make-u8array
  Return a byte array of optional length `len`, filled with optional byte `v`.
  |#
  (define-who make-u8array
    (case-lambda
      [()      ($make-u8array who *mincap* #f 0)]
      [(len)   ($make-u8array who len      0  len)]
      [(len v) ($make-u8array who len      v  len)]))


  #|proc:array
  Create an array from the given arguments.
  |#
  (define-who array
    (lambda args
      (let ([len (length args)])
        (let* ([arr (make-array (if (fx< len *mincap*) *mincap* len))] [vec (array-vec arr)])
          (let loop ([i 0] [args args])
            (if (null? args)
                (begin ($array-size-set! arr len)
                       arr)
                (begin (vector-set! vec i (car args))
                       (loop (fx1+ i) (cdr args)))))))))

  #|proc:fxarray
  Return a fixnum array containing `args` in argument order.
  |#
  (define-who fxarray
    (lambda args
      (unless (andmap fixnum? args)
        (errorf who "arguments must be fixnums: ~a" args))
      (let ([len (length args)])
        (let* ([arr (make-fxarray (if (fx< len *mincap*) *mincap* len))] [vec (array-vec arr)])
          (let loop ([i 0] [args args])
            (if (null? args)
                (begin ($array-size-set! arr len)
                       arr)
                (begin (fxvector-set! vec i (car args))
                       (loop (fx1+ i) (cdr args)))))))))

  #|proc:flarray
  Return a flonum array containing `args` in argument order.
  |#
  (define-who flarray
    (lambda args
      (unless (andmap flonum? args) (errorf who "arguments must be flonums: ~a" args))
      (let* ([len (length args)] [arr (make-flarray (max *mincap* len))]
             [vec (array-vec arr)])
        (let loop ([i 0] [xs args])
          (if (null? xs)
              (begin ($array-size-set! arr len) arr)
              (begin (flvector-set! vec i (car xs)) (loop (fx1+ i) (cdr xs))))))))

  #|proc:flarray-size
  Return the number of items in the flarray `arr`.
  |#
  (define-who flarray-size (lambda (arr) (pcheck ([flarray? arr]) ($array-size arr))))
  #|proc:flarray-empty?
  Return whether `arr` contains no items.
  |#
  (define-who flarray-empty? (lambda (arr) (pcheck ([flarray? arr]) (fx= 0 ($array-size arr)))))
  #|proc:flarray-ref
  Return the flonum at index `i` in `arr`.
  |#
  (define-who flarray-ref
    (lambda (arr i) (pcheck ([flarray? arr] [natural? i])
                             (if (fx< i ($array-size arr))
                                 (flvector-ref (array-vec arr) i)
                                 (errorf who "index ~a out of range" i)))))
  #|proc:flarray-set!
  Set index `i` of `arr` to flonum `v`.
  |#
  (define-who flarray-set!
    (lambda (arr i v) (pcheck ([flarray? arr] [natural? i] [flonum? v])
                              (if (fx< i ($array-size arr))
                                  (flvector-set! (array-vec arr) i v)
                                  (errorf who "index ~a out of range" i)))))
  #|proc:flarray-add!
  Append flonum `v` to `arr`.
  |#
  (define-who flarray-add!
    (case-lambda
      [(arr v) (flarray-add! arr ($array-size arr) v)]
      [(arr i v)
       (pcheck ([flarray? arr] [natural? i] [flonum? v])
               ($array-add-values! who arr i (list v) (lambda (x) (void))
                                   make-flvector flvector-length flvector-set!
                                   flvcopy! 0.0))]))
  #|proc:flarray-add*!
  Add multiple flonum values to `arr` in order.
  |#
  (define-who flarray-add*!
    (lambda (arr first . rest)
      (pcheck ([flarray? arr])
              (let ([check (lambda (x)
                             (unless (flonum? x)
                               (errorf who "not a flonum: ~a" x)))])
                (if (and (pair? rest) (natural? first) (fx<= first ($array-size arr)))
                    ($array-add-values! who arr first rest check make-flvector
                                        flvector-length flvector-set! flvcopy! 0.0)
                    ($array-add-values! who arr ($array-size arr) (cons first rest) check
                                        make-flvector flvector-length flvector-set!
                                        flvcopy! 0.0))))))
  #|proc:flarray-delete!
  Remove the flonum at index `i` from `arr`.
  |#
  (define-who flarray-delete!
    (lambda (arr i)
      (pcheck ([flarray? arr] [natural? i])
              (let ([len ($array-size arr)] [vec (array-vec arr)])
                (if (fx< i len)
                    (begin (when (fx< i (fx1- len))
                             (flvcopy! vec (fx1+ i) vec i (fx- len i 1)))
                           ($array-size-set! arr (fx1- len)))
                    (errorf who "index ~a out of range" i))))))
  #|proc:flarray-clear!
  Remove all values from `arr`.
  |#
  (define-who flarray-clear!
    (lambda (arr) (pcheck ([flarray? arr]) ($array-size-set! arr 0))))
  #|proc:flarray->flvector
  Convert `arr` to an exact-size flvector.
  |#
  (define-who flarray->flvector
    (lambda (arr) (pcheck ([flarray? arr])
                          (let ([v (make-flvector ($array-size arr) 0.0)])
                            (flvcopy! (array-vec arr) 0 v 0 ($array-size arr)) v))))
  #|proc:flvector->flarray
  Convert flvector `vec` to a flarray.
  |#
  (define-who flvector->flarray
    (lambda (vec) (pcheck-flvector (vec)
                                   (let* ([n (flvector-length vec)] [arr (make-flarray n)])
                                     (flvcopy! vec 0 (array-vec arr) 0 n)
                                     arr))))

  #|proc:u8array
  Return a byte array containing `args` in argument order.
  |#
  (define-who u8array
    (lambda args
      (unless (andmap u8? args)
        (errorf who "arguments must be bytes: ~a" args))
      (let ([len (length args)])
        (let* ([arr (make-u8array (if (fx< len *mincap*) *mincap* len))] [vec (array-vec arr)])
          (let loop ([i 0] [args args])
            (if (null? args)
                (begin ($array-size-set! arr len)
                       arr)
                (begin (bytevector-u8-set! vec i (car args))
                       (loop (fx1+ i) (cdr args)))))))))


  #|proc:fxarray-size
  Return the number of items in the fxarray.
  |#
  #|proc:u8array-size
  Return the number of items in the u8array.
  |#
  (define-array-procedure (fxa u8a)
    (size arr)
    (apcheck (arr)
             ($array-size arr)))


  #|proc:list->array
  Return whether the array is empty.
  |#
  (define-array-procedure (a fxa u8a)
    (empty? arr)
    (apcheck (arr)
             (fx= 0 ($array-size arr))))


  (define $grow-array!
    (lambda (arr)
      (let* ([len ($array-size arr)] [vec (array-vec arr)]
             [vmake (cond [(vector?     vec) make-vector]
                          [(fxvector?   vec) make-fxvector]
                          [(flvector?   vec) make-flvector]
                          [(bytevector? vec) make-bytevector]
                          [else (assert-unreachable)])]
             [vlength (cond [(vector?     vec) vector-length]
                            [(fxvector?   vec) fxvector-length]
                            [(flvector?   vec) flvector-length]
                            [(bytevector? vec) bytevector-length]
                            [else (assert-unreachable)])]
             [vcopy! (cond [(vector?     vec) vcopy!]
                           [(fxvector?   vec) fxvcopy!]
                           [(flvector?   vec) flvcopy!]
                           [(bytevector? vec) bytevector-copy!]
                           [else (assert-unreachable)])]
             [cap (vlength vec)]
             [newvec (vmake (fx* (if (fx= cap 0) *mincap* cap) (array-incr-factor arr)) 0)])
        ;;(printf "growing array from ~a to ~a~n" cap (vlength newvec))
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
           [(arr v) (pcheck ([a? arr]) (thisproc arr ($array-size arr) v))]
           [(arr i v)
            (pcheck ([a? arr] [natural? i] [aval? v])
                    (let* ([len ($array-size arr)] [vec (array-vec arr)] [cap (vlength vec)])
                      (when (fx> i len) (errorf 'thisproc "index ~a out of range ~a" i len))
                      (when (fx= len cap) ($grow-array! arr))
                      (when (fx< i len) (vcopy! (array-vec arr) i (array-vec arr) (fx1+ i) (fx- len i)))
                      (vset! (array-vec arr) i v)
                      ($array-size-set! arr (fx1+ len))))]))]))

  (define-array-add! array-add!   array?   (lambda (x) #t) vector-length     vector-set!        vcopy!)
  (define-array-add! fxarray-add! fxarray? fixnum?         fxvector-length   fxvector-set!      fxvcopy!)
  (define-array-add! u8array-add! u8array? u8?             bytevector-length bytevector-u8-set! u8vcopy!)

  (define $array-add-values!
    (lambda (who arr i values value-check vector-make vector-length vector-set vector-copy! fill)
      (let* ([len ($array-size arr)] [count (length values)] [newlen (fx+ len count)]
             [old (array-vec arr)] [capacity (vector-length old)])
        (when (fx> i len)
          (errorf who "index ~a out of range ~a" i len))
        (for-each value-check values)
        (if (fx>= capacity newlen)
            (when (fx< i len)
              (vector-copy! old i old (fx+ i count) (fx- len i)))
            (let capacity-loop ([new-capacity (if (fx= capacity 0) *mincap* capacity)])
              (if (fx>= new-capacity newlen)
                  (let ([new (vector-make new-capacity fill)])
                    (vector-copy! old 0 new 0 i)
                    (when (fx< i len)
                      (vector-copy! old i new (fx+ i count) (fx- len i)))
                    (array-vec-set! arr new))
                  (capacity-loop (fx* new-capacity (array-incr-factor arr))))))
        (let ([vec (array-vec arr)])
          (let write ([j i] [rest values])
            (unless (null? rest)
              (vector-set vec j (car rest))
              (write (fx1+ j) (cdr rest)))))
        ($array-size-set! arr newlen))))


  #|doc
  Add multiple values either to the end of the array `arr` or at a specified index.
  This is faster than `array-add!` when adding multiple values.
  |#
  (define-array-procedure (a fxa u8a) add*!
    [(arr first . rest)
     (apcheck (arr)
              (if (and (pair? rest) (natural? first) (fx<= first (asize arr)))
                  ($array-add-values! who arr first rest aval?
                                      vmake vlength vset! vcopy! t+id)
                  ($array-add-values! who arr (asize arr) (cons first rest) aval?
                                      vmake vlength vset! vcopy! t+id)))])


  #|doc
  Return the value at the specified index.
  TODO default value?
  |#
  (define-array-procedure (a fxa u8a)
    (ref arr i)
    (apcheck (arr)
             (pcheck ([natural? i])
                     (let* ([len (asize arr)] [vec (array-vec arr)])
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
                     (let* ([len (asize arr)] [vec (array-vec arr)])
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
                     (let ([len ($array-size arr)] [vec (array-vec arr)])
                       (when (fx>= i len) (errorf who "index ~a out of range ~a" i len))
                       (cond [(fx= i 0) (vcopy! vec 1 vec 0 (fx- len 1))]
                             [(fx= i (fx1- len)) (void)]
                             [else (vcopy! vec (fx1+ i) vec i (fx- len i 1))])
                       ($array-size-set! arr (fx1- len))))))

  ;; TODO delete in range


  #|doc
  Remove all items in the array.
  |#
  (define-array-procedure (a fxa u8a)
    (clear! arr)
    (apcheck (arr)
             ;; just set length to 0 for now
             ($array-size-set! arr 0)))


  #|doc
  Apply `pred` to every item of the array `arr` and return a new array
  of the items of `arr` for which `pred` returns #t.
  |#
  (define-array-procedure (a fxa fla u8a)
    (filter pred arr)
    (apcheck (arr)
             (pcheck ([procedure? pred])
                     (let ([newarr (amake)] [len ($array-size arr)] [vec (array-vec arr)])
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
  (define-array-procedure (a fxa fla u8a)
    (filter! pred arr)
    (apcheck (arr)
             (pcheck ([procedure? pred])
                     ;; This may incur memory waste when the remaining items are few...
                     (let ([len ($array-size arr)] [vec (array-vec arr)])
                       ;; i: store index, j: scan index
                       (let loop ([i 0] [j 0])
                         (if (fx= j len)
                             ($array-size-set! arr i)
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
  (define-array-procedure (a fxa fla u8a)
    (partition proc arr)
    (apcheck (arr)
             (pcheck ([procedure? proc])
                     (let ([T (amake)] [F (amake)] [len ($array-size arr)] [vec (array-vec arr)])
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
  (define-array-procedure (a fxa fla u8a)
    (append arr . arr*)
    (apcheck (arr)
             (pcheck ([all-which? arr*])
                     (let* ([arr* (cons arr arr*)]
                            [len (apply fx+ (map $array-size arr*))]
                            [newarr (amake len)] [newvec (array-vec newarr)])
                       ($array-size-set! newarr len)
                       (let next ([i 0] [arr* arr*])
                         (if (null? arr*)
                             newarr
                             (let* ([arr (car arr*)] [vec (array-vec arr)] [len ($array-size arr)])
                               (let loop ([i i] [j 0])
                                 (if (fx= j len)
                                     (next i (cdr arr*))
                                     (begin (vset! newvec i (vref vec j))
                                            (loop (fx1+ i) (fx1+ j))))))))))))


  #|doc
  Imperatively append items of given arrays `arr*` to array `arr`,
  then return the first array `arr`.
  |#
  (define-array-procedure (a fxa fla u8a)
    (append! arr . arr*)
    (apcheck (arr)
             (unless (null? arr*)
               (pcheck ([all-which? arr*])
                       (let* ([len1 ($array-size arr)] [vec1 (array-vec arr)] [cap1 (vlength vec1)]
                              [len* (apply fx+ (map $array-size arr*))]
                              [fillvec! (lambda (tgtvec i arr*)
                                          (let next ([i i] [arr* arr*])
                                            (unless (null? arr*)
                                              (let* ([arr (car arr*)]
                                                     [vec (array-vec arr)] [len ($array-size arr)])
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
                         ($array-size-set! arr (fx+ len1 len*))
                         arr)))))



  #|doc
  Return a newly allocated array consisting of the items of `arr` in reverse order.
  |#
  (define-array-procedure (a fxa fla u8a)
    (reverse arr)
    (apcheck (arr)
             (let* ([len ($array-size arr)] [vec (array-vec arr)]
                    [newarr (amake len)]     [newvec (array-vec newarr)])
               (let loop ([i 0] [j (fx1- len)])
                 (unless (fx= i len)
                   (vset! newvec j (vref i vec))
                   (loop (fx1+ i) (fx1- j))))
               ($array-size-set! newarr len))))


  #|doc
  Reverse the items in the array in place, then return the array.
  |#
  (define-array-procedure (a fxa fla u8a)
    (reverse! arr)
    (apcheck (arr)
             (let ([len ($array-size arr)] [vec (array-vec arr)])
               (let loop ([i 0] [j (fx1- len)])
                 (when (fx< i j)
                   (let ([x (vref vec i)] [y (vref vec j)])
                     (vset! vec i y)
                     (vset! vec j x)
                     (loop (fx1+ i) (fx1- j)))))
               arr)))


  #|proc:array-contains?
  Return whether the array contains the given item using `equal?`.
  |#
  #|proc:fxarray-contains?
  Return whether the fxarray contains the given item using `equal?`.
  |#
  #|proc:u8array-contains?
  Return whether the array contains the given item.
  Items are compared using `equal?`.
  |#
  (define-array-procedure (a fxa fla u8a)
    (contains? arr v)
    (apcheck (arr)
             (aval? v)
             (let ([len ($array-size arr)] [vec (array-vec arr)])
               (let loop ([i 0])
                 (if (fx= i len)
                     #f
                     (if (equal? v (vref vec i))
                         #t
                         (loop (fx1+ i))))))))


  #|proc:array-contains/p?
  Return whether the array contains an item that satisfies `pred`.
  |#
  #|proc:fxarray-contains/p?
  Return whether the fxarray contains an item that satisfies `pred`.
  |#
  #|proc:u8array-contains/p?
  Return whether the array contains an item that satisfies `pred`.
  |#
  (define-array-procedure (a fxa fla u8a)
    (contains/p? arr pred)
    (apcheck (arr)
             (pcheck ([procedure? pred])
                     (let ([len ($array-size arr)] [vec (array-vec arr)])
                       (let loop ([i 0])
                         (if (fx= i len)
                             #f
                             (if (pred (vref vec i))
                                 #t
                                 (loop (fx1+ i)))))))))


  #|proc:array-index-of
  Return the index of the first item equal to `v`, or #f if no item matches.
  |#
  #|proc:fxarray-index-of
  Return the index of the first item equal to `v`, or #f if no item matches.
  |#
  #|proc:u8array-index-of
  Return the index of the first item equal to `v`, or #f if no item matches.
  |#
  (define-array-procedure (a fxa fla u8a)
    (index-of arr v)
    (apcheck (arr)
             (aval? v)
             (let ([len ($array-size arr)] [vec (array-vec arr)])
               (let loop ([i 0])
                 (if (fx= i len)
                     #f
                     (if (equal? v (vref vec i))
                         i
                         (loop (fx1+ i))))))))


  #|proc:array-find-index
  Return the index of the first item satisfying `pred`, or #f if no item matches.
  |#
  #|proc:fxarray-find-index
  Return the index of the first item satisfying `pred`, or #f if no item matches.
  |#
  #|proc:u8array-find-index
  Return the index of the first item satisfying `pred`, or #f if no item matches.
  |#
  (define-array-procedure (a fxa fla u8a)
    (find-index arr pred)
    (apcheck (arr)
             (pcheck ([procedure? pred])
                     (let ([len ($array-size arr)] [vec (array-vec arr)])
                       (let loop ([i 0])
                         (if (fx= i len)
                             #f
                             (if (pred (vref vec i))
                                 i
                                 (loop (fx1+ i)))))))))


  #|proc:array-search
  Return the first item satisfying `pred`, or #f if no item matches.
  |#
  #|proc:fxarray-search
  Return the first item satisfying `pred`, or #f if no item matches.
  |#
  #|proc:u8array-search
  Return the first item in the array that satisfies the predicate `pred`.
  If no such item is found, #f is returned.
  |#
  (define-array-procedure (a fxa fla u8a)
    (search arr pred)
    (apcheck (arr)
             (pcheck ([procedure? pred])
                     (let ([len ($array-size arr)] [vec (array-vec arr)])
                       (let loop ([i 0])
                         (if (fx= i len)
                             #f
                             (let ([v (vref vec i)])
                               (if (pred v)
                                   v
                                   (loop (fx1+ i))))))))))


  #|doc
  Search for items in array `dl` that satisfies the predicate `pred`.

  By default the items satisfying `pred` are returned in a list.

  The `collect` argument has the same semantics as in `dlist-search*`.
  |#
  (define-array-procedure (a fxa fla u8a) search*
    [(arr pred)
     (apcheck (arr)
              (pcheck ([procedure? pred])
                      (let ([lb (make-list-builder)])
                        (thisproc arr pred (lambda (x) (lb x)))
                        (lb))))]
    [(arr pred collect)
     (apcheck (arr)
              (pcheck ([procedure? pred collect])
                      (let ([len ($array-size arr)] [vec (array-vec arr)])
                        (let loop ([i 0])
                          (unless (fx= i len)
                            (let ([v (vref vec i)])
                              (when (pred v) (collect v))
                              (loop (fx1+ i))))))))])


  #|doc
  Return a slice (sub-array) of the array `arr` specified by `start`, `end` and `step`.

  Meanings of `start`, `end` and `step` are the same as in list:slice.

  If the indices are out of range in any way, an empty array is returned.
  |#
  (define-array-procedure (a fxa fla u8a) slice
    [(arr end) (thisproc arr 0 end 1)]
    [(arr start end) (thisproc arr start end 1)]
    [(arr start end step)
     (pcheck ([a? arr] [fixnum? start end step])
             (when (fx= step 0) (errorf who "step cannot be 0"))
             (let* ([vec (array-vec arr)] [len ($array-size arr)]
                    [s (let ([s (if (fx>= start 0) start (fx+ len start))])
                         (cond [(fx< s 0) 0]
                               [(fx> s len) (fx1- len)]
                               [else s]))]
                    [e (let ([e (if (fx>= end 0) end (fx+ len end))])
                         (cond [(fx<= e -1) -1]
                               [(fx>= e len) len]
                               [else e]))])
               (if (fx= len 0)
                   (amake 0)
                   (let ([newv (cond [(and (fx< s e) (fx> step 0))
                                      (let ([newv (vmake (ceiling (/ (fx- e s) step)))])
                                        ;; forward
                                        (let loop ([s s] [i 0])
                                          (if (fx>= s e)
                                              newv
                                              (begin (vset! newv i (vref vec s))
                                                     (loop (fx+ s step) (fx1+ i))))))]
                                     [(and (fx> s e) (fx< step 0))
                                      (let ([newv (vmake (ceiling (/ (fx- e s) step)))])
                                        ;; backward
                                        (let loop ([s s] [i 0])
                                          (if (fx<= s e)
                                              newv
                                              (begin (vset! newv i (vref vec s))
                                                     (loop (fx+ s step) (fx1+ i))))))]
                                     [else (vmake 0)])])
                     (amk newv (array-incr-factor arr) (vlength newv))))))])


  #|doc
  Imperatively slice the array `arr` to the range specified by `start`, `end` and `step`.

  Meanings of `start`, `end` and `step` are the same as in list:slice.

  If the indices are out of range in any way, this procedure has no effect on the array.

  After the operation, `arr` is returned.
  |#
  (define-array-procedure (a fxa fla u8a) slice!
    [(arr end) (thisproc arr 0 end 1)]
    [(arr start end) (thisproc arr start end 1)]
    [(arr start end step)
     (pcheck ([a? arr] [fixnum? start end step])
             (when (fx= step 0) (errorf who "step cannot be 0"))
             (let* ([vec (array-vec arr)] [len ($array-size arr)]
                    [s (let ([s (if (fx>= start 0) start (fx+ len start))])
                         (cond [(fx< s 0) 0]
                               [(fx> s len) (fx1- len)]
                               [else s]))]
                    [e (let ([e (if (fx>= end 0) end (fx+ len end))])
                         (cond [(fx<= e -1) -1]
                               [(fx>= e len) len]
                               [else e]))])
               (when (fx> len 0)
                 ;; TODO try to reuse `vec`
                 (let ([newv (cond [(and (fx< s e) (fx> step 0))
                                    (let ([newv (vmake (ceiling (/ (fx- e s) step)))])
                                      ;; forward
                                      (let loop ([s s] [i 0])
                                        (if (fx>= s e)
                                            newv
                                            (begin (vset! newv i (vref vec s))
                                                   (loop (fx+ s step) (fx1+ i))))))]
                                   [(and (fx> s e) (fx< step 0))
                                    (let ([newv (vmake (ceiling (/ (fx- e s) step)))])
                                      ;; backward
                                      (let loop ([s s] [i 0])
                                        (if (fx<= s e)
                                            newv
                                            (begin (vset! newv i (vref vec s))
                                                   (loop (fx+ s step) (fx1+ i))))))]
                                   [else #f])])
                   (when newv
                     (array-vec-set!    arr newv)
                     ($array-size-set! arr (vlength newv)))))
               arr))])


  #|doc
  Make a copy of the array `arr`.
  |#
  (define-array-procedure (a fxa fla u8a)
    (copy arr)
    (apcheck (arr)
             (amk (vcopy (array-vec arr))
                  (array-incr-factor arr)
                  ($array-size arr))))


  #|doc
  Copy items in `src` from indices src-start, ..., src-start + k - 1
  to consecutive indices in `tgt` starting at `tgt-start`.

  `src` and `tgt` must be arrays of the same type.
  `src-start`, `tgt-start`, and `k` must be exact nonnegative integers.
  The sum of `src-start` and `k` must not exceed the length of `src`,
  and the sum of `tgt-start` and `k` must not exceed the length of `tgt`.

  `src` and `tgt` may or may not be the same array.
  |#
  (define-array-procedure (a fxa fla u8a)
    (copy! src src-start tgt tgt-start k)
    (apcheck (src tgt)
             (pcheck ([natural? src-start tgt-start k])
                     (let ([len1 ($array-size src)] [vec1 (array-vec src)]
                           [len2 ($array-size tgt)] [vec2 (array-vec tgt)])
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


  (define $sorted?
    (lambda (vec <? start stop vref)
      (let loop ([i start])
        (if (fx= i (fx1- stop))
            #t
            (and (<? (vref vec i) (vref vec (fx1+ i)))
                 (loop (fx1+ i)))))))


  #|doc
  Check whether the array is sorted according to the comparison procedure `<?`.
  If `stop` is given, only the items with indices [0, stop) are checked;
  If both `start` and `stop` are given, only the items with indices [start, stop) are checked.

  `start` and `stop` must satisfy the requirement that `0 <= start <= stop <= length of array`.
  |#
  (define-array-procedure (a fxa fla u8a) sorted?
    [(<? arr)
     (apcheck (arr)
              (thisproc <? arr 0 (asize arr)))]
    [(<? arr stop)
     (apcheck (arr)
              (thisproc <? arr 0 stop))]
    [(<? arr start stop)
     (apcheck (arr)
              (pcheck ([procedure? <?] [natural? start stop])
                      (let ([len ($array-size arr)] [vec (array-vec arr)])
                        (when (fx> stop len)
                          (errorf who "stop index ~a out of bound ~a" stop len))
                        (when (fx> start stop)
                          (errorf who "start index ~a greater than stop index ~a" start stop))
                        (if (fx<= len 1)
                            #t
                            ($sorted? vec <? start stop vref)))))])


  #|doc
  The `*array-sort` procedures use the binary comparison procedure `<?` to sort the array `arr`.
  If only two arguments are given, the entire array is sorted;
  If the `stop` argument is given, the range from 0 to `stop-1` in `arr` is sorted;
  If both `start` and `stop` are given, the range from `start` to `stop-1` in `arr` is sorted.

  `start` and `stop` must satisfy the requirement that `0 <= start <= stop <= length of arr`.

  The `*array-sort` procedures return the sorted array or the subarray.
  |#
  (define-array-procedure (a fxa fla u8a) sort
    [(<? arr)
     (apcheck (arr)
              (thisproc <? arr 0 (asize arr)))]
    [(<? arr stop)
     (apcheck (arr)
              (thisproc <? arr 0 stop))]
    [(<? arr start stop)
     (apcheck (arr)
              (pcheck ([procedure? <?] [natural? start stop])
                      (let ([len (asize arr)])
                        (when (fx> stop len)
                          (errorf who "stop index ~a out of bound ~a" stop len))
                        (when (fx> start stop)
                          (errorf who "start index ~a greater than stop index ~a" start stop))
                        (let* ([vsort! (cond [(fxarray? arr) fxvsort!]
                                             [(flarray? arr) flvsort!]
                                             [(u8array? arr) (todo who)]
                                             [else vsort!])]
                               [acopy! (cond [(fxarray? arr) fxarray-copy!]
                                             [(flarray? arr) flarray-copy!]
                                             [(u8array? arr) u8array-copy!]
                                             [else array-copy!])]
                               [newarr (amake (fx- stop start))])
                          (acopy! arr start newarr 0 (fx- stop start))
                          (let ([vec (array-vec newarr)])
                            (vsort! <? vec)
                            newarr)))))])



  #|doc
  The `*array-sort!` procedures use the binary comparison procedure `<?` to sort the array `arr`, in place.
  If only two arguments are given, the entire array is sorted;
  If the `stop` argument is given, the range from 0 to `stop-1` in `arr` is sorted;
  If both `start` and `stop` are given, the range from `start` to `stop-1` in `arr` is sorted.

  `start` and `stop` must satisfy the requirement that `0 <= start <= stop <= length of arr`.
  |#
  (define-array-procedure (a fxa fla u8a) sort!
    [(<? arr)
     (apcheck (arr)
              (thisproc <? arr 0 (asize arr)))]
    [(<? arr stop)
     (apcheck (arr)
              (thisproc <? arr 0 stop))]
    [(<? arr start stop)
     (apcheck (arr)
              (pcheck ([procedure? <?] [natural? start stop])
                      (let ([len (asize arr)])
                        (when (fx> stop len)
                          (errorf who "stop index ~a out of bound ~a" stop len))
                        (when (fx> start stop)
                          (errorf who "start index ~a greater than stop index ~a" start stop))
                        (let ([vsort! (cond [(fxarray? arr) fxvsort!]
                                            [(flarray? arr) flvsort!]
                                            [(u8array? arr) (todo who)]
                                            [else vsort!])]
                              [vec (array-vec arr)])
                          (vsort! <? vec start stop)))))])


  #|doc
  `n` must be a natural number.
  This procedure creates an array that contains numbers ranging from 0 to n-1, inclusive.
  This is similar to `iota` for lists.

  Note that for u8arrays, it is an error if `n` exceeds 257.
  |#
  (define-array-procedure (a fxa fla u8a)
    (iota n)
    (pcheck ([natural? n])
            (let ([v (vmake n)])
              (let loop ([i 0])
                (if (fx= i n)
                    (amk v 2 n)
                    (begin (vset! v i (if (flvector? v) (inexact i) i))
                           (loop (fx1+ i))))))))


  #|doc
  Generate an array of of numbers: start, start+step*1, start+step*2, ...

  `start`, `stop` and `step` must be numbers that meet the following requirements:
  If `start` is less than `stop`, then `step` must be greater than 0,
  in which case the sequence terminates when the value is greater than or equal to `stop`;
  If `start` is greater than `stop`, then `step` must be less than 0,
  in which case the sequence terminates when the value is less than or equal to `stop`.

  Note that for u8arrays, it is an error if the numbers contain values that are negative or greater than 256.
  For fxarrays, the generated numbers must be fixnums.
  |#
  (define-array-procedure (a) nums
    [(stop) (thisproc 0 stop 1)]
    [(start stop) (thisproc start stop 1)]
    [(start stop step)
     (pcheck ([number? start stop step])
             (if (or (and (<= start stop) (> step 0))
                     (and (>= start stop) (< step 0)))
                 (let* ([len (exact (ceiling (/ (- stop start) step)))]
                        [vec (vmake len 0)])
                   (let loop ([i 0] [x start])
                     (if (fx= i len)
                         (amk vec 2 len)
                         (begin (vset! vec i x)
                                (loop (fx1+ i) (+ x step))))))
                 (errorf who "invalid range: ~a, ~a, ~a" start stop step)))])

  (define-array-procedure (fxa u8a) nums
    [(stop) (thisproc 0 stop 1)]
    [(start stop) (thisproc start stop 1)]
    [(start stop step)
     (pcheck ([integer? start stop step])
             (if (or (and (<= start stop) (> step 0))
                     (and (>= start stop) (< step 0)))
                 (let* ([len (ceiling (/ (fx- stop start) step))]
                        [vec (vmake len 0)])
                   (let loop ([i 0] [x start])
                     (if (fx= i len)
                         (amk vec 2 len)
                         (begin (vset! vec i x)
                                (loop (fx1+ i) (+ x step))))))
                 (errorf who "invalid range: ~a, ~a, ~a" start stop step)))])

  #|proc:flarray-nums
  Return a flarray containing the progression from `start` toward `stop` by `step`.
  |#
  (define-who flarray-nums
    (case-lambda
      [(stop) (flarray-nums 0.0 stop 1.0)]
      [(start stop) (flarray-nums start stop 1.0)]
      [(start stop step)
       (pcheck ([number? start stop step])
               (let ([start (inexact start)] [stop (inexact stop)] [step (inexact step)])
                 (unless (or (and (fl<= start stop) (fl> step 0.0))
                             (and (fl>= start stop) (fl< step 0.0)))
                   (errorf who "invalid range: ~a, ~a, ~a" start stop step))
                 (let* ([len (exact (ceiling (fl/ (fl- stop start) step)))]
                        [arr (make-flarray len)])
                   (let loop ([i 0] [value start])
                     (if (fx= i len)
                         arr
                         (begin
                           (flarray-set! arr i value)
                           (loop (fx1+ i) (fl+ value step))))))))]))


  #|doc
  Add the item `v` to the front of the array `arr`.
  |#
  (define-array-procedure (a fxa fla u8a)
    (push! arr v)
    (apcheck (arr)
             (aval? v)
             (let* ([len (asize arr)] [vec (array-vec arr)] [cap (vlength vec)])
               (when (fx= len cap) ($grow-array! arr))
               (vcopy! (array-vec arr) 0 (array-vec arr) 1 len)
               (vset! (array-vec arr) 0 v)
               ($array-size-set! arr (fx1+ len)))))


  #|doc
  Remove the first item from the array `arr` and return it.
  It is an error if the array is empty.
  |#
  (define-array-procedure (a fxa fla u8a)
    (pop! arr)
    (apcheck (arr)
             (let ([len (asize arr)])
               (if (fx= len 0)
                   (errorf who "array is empty")
                   (let* ([len (asize arr)] [vec (array-vec arr)]
                          [v (vref vec 0)])
                     (vcopy! vec 1 vec 0 (fx1- len))
                     ($array-size-set! arr (fx1- len))
                     v)))))


  #|doc
  Add the item `v` to the back of the array `arr`.
  |#
  (define-array-procedure (a fxa fla u8a)
    (push-back! arr v)
    (apcheck (arr)
             (aval? v)
             (let* ([len (asize arr)] [vec (array-vec arr)] [cap (vlength vec)])
               (when (fx= len cap) ($grow-array! arr))
               (vset! (array-vec arr) len v)
               ($array-size-set! arr (fx1+ len)))))


  #|doc
  Remove the last item from the array `arr` and return it.
  It is an error if the array is empty.
  |#
  (define-array-procedure (a fxa fla u8a)
    (pop-back! arr)
    (apcheck (arr)
             (let ([len (asize arr)])
               (if (fx= len 0)
                   (errorf who "array is empty")
                   (let* ([len (asize arr)] [vec (array-vec arr)]
                          [v (vref vec (fx1- len))])
                     ($array-size-set! arr (fx1- len))
                     v)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   iterations
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (define check-length
    (case-lambda
      [(who arr0 arr1)
       (unless (fx= ($array-size arr0) ($array-size arr1))
         (errorf who "arrays are not of the same length"))]
      [(who arr0 . arr*)
       (unless (null? arr*)
         (unless (apply fx= ($array-size arr0) (map $array-size arr*))
           (errorf who "arrays are not of the same length")))]))


  (define-array-procedure (a fxa fla u8a) map
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              newarr
                              (begin (vset! newvec i (proc (vref vec0 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              newarr
                              (begin (vset! newvec i (proc (vref vec0 i) (vref vec1 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)]
                    [newarr (amake len0)]      [newvec (array-vec newarr)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     newarr
                     (begin (vset! newvec i (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1+ i)))))))])


  (define-array-procedure (a fxa fla u8a) map/i
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              newarr
                              (begin (vset! newvec i (proc i (vref vec0 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (check-length who arr0 arr1)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)]
                    [newarr (amake len0)]      [newvec (array-vec newarr)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     newarr
                     (begin (vset! newvec i (proc i (vref vec0 i) (vref vec1 i)))
                            (loop (fx1+ i)))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)]
                    [newarr (amake len0)]      [newvec (array-vec newarr)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     newarr
                     (begin (vset! newvec i (apply proc i (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1+ i)))))))])


;;;; in-place maps

  (define-array-procedure (a fxa fla u8a) map!
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              arr0
                              (begin (vset! vec0 i (proc (vref vec0 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              arr0
                              (begin (vset! vec0 i (proc (vref vec0 i) (vref vec1 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     arr0
                     (begin (vset! vec0 i (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1+ i)))))))])


  (define-array-procedure (a fxa fla u8a) map/i!
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              arr0
                              (begin (vset! vec0 i (proc i (vref vec0 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              arr0
                              (begin (vset! vec0 i (proc i (vref vec0 i) (vref vec1 i)))
                                     (loop (fx1+ i))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     arr0
                     (begin (vset! vec0 i (apply proc i (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1+ i)))))))])


  (define-array-procedure (a fxa fla u8a) for-each
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i 0])
                          (unless (fx= i len0)
                            (proc (vref vec0 i))
                            (loop (fx1+ i)))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i 0])
                          (unless (fx= i len0)
                            (proc (vref vec0 i) (vref vec1 i))
                            (loop (fx1+ i)))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i 0])
                 (unless (fx= i len0)
                   (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                   (loop (fx1+ i))))))])


  (define-array-procedure (a fxa fla u8a) for-each/i
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i 0])
                          (unless (fx= i len0)
                            (proc i (vref vec0 i))
                            (loop (fx1+ i)))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i 0])
                          (unless (fx= i len0)
                            (proc i (vref vec0 i) (vref vec1 i))
                            (loop (fx1+ i)))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i 0])
                 (unless (fx= i len0)
                   (apply proc i (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                   (loop (fx1+ i))))))])


;;;; reverse order

  (define-array-procedure (a fxa fla u8a) map-rev
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i (fx1- len0)] [j 0])
                          (if (fx= i -1)
                              newarr
                              (begin (vset! newvec j (proc (vref vec0 i)))
                                     (loop (fx1- i) (fx1+ j))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i (fx1- len0)] [j 0])
                          (if (fx= i -1)
                              newarr
                              (begin (vset! newvec j (proc (vref vec0 i) (vref vec1 i)))
                                     (loop (fx1- i) (fx1+ j))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)]
                    [newarr (amake len0)]      [newvec (array-vec newarr)])
               (let loop ([i (fx1- len0)] [j 0])
                 (if (fx= i -1)
                     newarr
                     (begin (vset! newvec j (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1- i) (fx1+ j)))))))])


  (define-array-procedure (a fxa fla u8a) map/i-rev
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i (fx1- len0)] [j 0])
                          (if (fx= i -1)
                              newarr
                              (begin (vset! newvec j (proc i (vref vec0 i)))
                                     (loop (fx1- i) (fx1+ j))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)]
                             [newarr (amake len0)]      [newvec (array-vec newarr)])
                        (let loop ([i (fx1- len0)] [j 0])
                          (if (fx= i -1)
                              newarr
                              (begin (vset! newvec j (proc i (vref vec0 i) (vref vec1 i)))
                                     (loop (fx1- i) (fx1+ j))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)]
                    [newarr (amake len0)]      [newvec (array-vec newarr)])
               (let loop ([i (fx1- len0)] [j 0])
                 (if (fx= i -1)
                     newarr
                     (begin (vset! newvec j (apply proc i (vref vec0 i) (map (lambda (x) (vref x i)) vec*)))
                            (loop (fx1- i) (fx1+ j)))))))])


  (define-array-procedure (a fxa fla u8a) for-each-rev
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i (fx1- len0)])
                          (unless (fx= i -1)
                            (proc (vref vec0 i))
                            (loop (fx1- i)))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i (fx1- len0)])
                          (unless (fx= i -1)
                            (proc (vref vec0 i) (vref vec1 i))
                            (loop (fx1- i)))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i (fx1- len0)] [j 0])
                 (unless (fx= i -1)
                   (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                   (loop (fx1- i) (fx1+ j))))))])


  (define-array-procedure (a fxa fla u8a) for-each/i-rev
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i (fx1- len0)])
                          (unless (fx= i -1)
                            (proc i (vref vec0 i))
                            (loop (fx1- i)))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i (fx1- len0)])
                          (unless (fx= i -1)
                            (proc i (vref vec0 i) (vref vec1 i))
                            (loop (fx1- i)))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i (fx1- len0)] [j 0])
                 (unless (fx= i -1)
                   (apply proc i (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                   (loop (fx1- i) (fx1+ j))))))])


  (define-array-procedure (a fxa fla u8a) andmap
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              #t
                              (and (proc (vref vec0 i))
                                   (loop (fx1+ i))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              #t
                              (and (proc (vref vec0 i) (vref vec1 i))
                                   (loop (fx1+ i))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     #t
                     (and (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                          (loop (fx1+ i)))))))])


  (define-array-procedure (a fxa fla u8a) ormap
    [(proc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              #f
                              (or (proc (vref vec0 i))
                                  (loop (fx1+ i))))))))]
    [(proc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([i 0])
                          (if (fx= i len0)
                              #f
                              (or (proc (vref vec0 i) (vref vec1 i))
                                  (loop (fx1+ i))))))))]
    [(proc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([i 0])
                 (if (fx= i len0)
                     #f
                     (or (apply proc (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                         (loop (fx1+ i)))))))])


;;;; folds


  (define-array-procedure (a fxa fla u8a) fold-left
    [(proc acc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([acc acc] [i 0])
                          (if (fx= i len0)
                              acc
                              (loop (proc acc (vref vec0 i))
                                    (fx1+ i)))))))]
    [(proc acc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([acc acc] [i 0])
                          (if (fx= i len0)
                              acc
                              (loop (proc acc (vref vec0 i) (vref vec1 i))
                                    (fx1+ i)))))))]
    [(proc acc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([acc acc] [i 0])
                 (if (fx= i len0)
                     acc
                     (loop (apply proc acc (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                           (fx1+ i))))))])


  (define-array-procedure (a fxa fla u8a) fold-left/i
    [(proc acc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([acc acc] [i 0])
                          (if (fx= i len0)
                              acc
                              (loop (proc i acc (vref vec0 i))
                                    (fx1+ i)))))))]
    [(proc acc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (check-length who arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([acc acc] [i 0])
                          (if (fx= i len0)
                              acc
                              (loop (proc i acc (vref vec0 i) (vref vec1 i))
                                    (fx1+ i)))))))]
    [(proc acc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([acc acc] [i 0])
                 (if (fx= i len0)
                     acc
                     (loop (apply proc i acc (vref vec0 i) (map (lambda (x) (vref x i)) vec*))
                           (fx1+ i))))))])


  (define-array-procedure (a fxa fla u8a) fold-right
    [(proc acc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([acc acc] [i (fx1- len0)])
                          (if (fx= i -1)
                              acc
                              (loop (proc (vref vec0 i) acc)
                                    (fx1- i)))))))]
    [(proc acc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([acc acc] [i (fx1- len0)])
                          (if (fx= i -1)
                              acc
                              (loop (proc (vref vec0 i) (vref vec1 i) acc)
                                    (fx1- i)))))))]
    [(proc acc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
               (let loop ([acc acc] [i (fx1- len0)])
                 (if (fx= i -1)
                     acc
                     (loop (apply proc (vref vec0 i) `(,@(map (lambda (x) (vref x i)) vec*) ,acc))
                           (fx1- i))))))])


  (define-array-procedure (a fxa fla u8a) fold-right/i
    [(proc acc arr0)
     (pcheck ([procedure? proc])
             (apcheck (arr0)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)])
                        (let loop ([acc acc] [i (fx1- len0)])
                          (if (fx= i -1)
                              acc
                              (loop (proc i (vref vec0 i) acc)
                                    (fx1- i)))))))]
    [(proc acc arr0 arr1)
     (pcheck ([procedure? proc])
             (apcheck (arr0 arr1)
                      (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec1 (array-vec arr1)])
                        (let loop ([acc acc] [i (fx1- len0)])
                          (if (fx= i -1)
                              acc
                              (loop (proc i (vref vec0 i) (vref vec1 i) acc)
                                    (fx1- i)))))))]
    [(proc acc arr0 . arr*)
     (pcheck ([procedure? proc] [a? arr0] [all-which? arr*])
             (apply check-length who arr0 arr*)
             (let* ([len0 ($array-size arr0)] [vec0 (array-vec arr0)] [vec* (map array-vec arr*)])
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


  #|proc:vector->array
  Convert a vector `vec` to an array.
  |#
  (define-who vector->array
    (lambda (vec)
      (pcheck ([vector? vec])
              (let* ([len (vector-length vec)]
                     [arr (make-array len)])
                (let loop ([i 0])
                  (if (fx= i len)
                      arr
                      (begin (array-set! arr i (vector-ref vec i))
                             (loop (fx1+ i)))))))))

  #|proc:fxvector->fxarray
  Convert a fxvector `vec` to a fxarray.
  |#
  (define-who fxvector->fxarray
    (lambda (vec)
      (pcheck ([fxvector? vec])
              (let* ([len (fxvector-length vec)]
                     [arr (make-fxarray len)])
                (let loop ([i 0])
                  (if (fx= i len)
                      arr
                      (begin (fxarray-set! arr i (fxvector-ref vec i))
                             (loop (fx1+ i)))))))))

  #|proc:u8vector->u8array
  Convert a bytevector/u8vector `vec` to a u8array.
  |#
  (define-who u8vector->u8array
    (lambda (vec)
      (pcheck ([bytevector? vec])
              (let* ([len (bytevector-length vec)]
                     [arr (make-u8array len)])
                (let loop ([i 0])
                  (if (fx= i len)
                      arr
                      (begin (u8array-set! arr i (bytevector-u8-ref vec i))
                             (loop (fx1+ i)))))))))


  #|
  Convert an array to a list.
  |#
  ;; defines {,fx,u8}array->list
  (define-array-procedure (a fxa fla u8a)
    (>list arr)
    (apcheck (arr)
             (let ([lb (make-list-builder)] [vec (array-vec arr)] [len ($array-size arr)])
               (let loop ([i 0])
                 (if (fx= i len)
                     (lb)
                     (begin (lb (vref vec i))
                            (loop (fx1+ i))))))))


  #|proc:array->iter
  The `array->iter` procedure returns an iterator over values in the array `source`.
  `(source)` traverses the current full array and reevaluates its size on reset.
  `(source stop)` defaults `start` to 0, and `(source start stop)` defaults `step` to 1.
  `(source start stop step)` selects a half-open indexed range with a nonzero integer `step`.
  A positive `step` visits increasing indexes at that stride; a negative `step` visits
  decreasing indexes at the absolute stride. The iterator returns each selected value.
  |#
  (define array->iter
    (make-indexed-iter 'array->iter array? array-size array-ref))
  #|proc:flarray->iter
  Return an iterator over the current values of flarray `source`.
  |#
  (define flarray->iter
    (make-indexed-iter 'flarray->iter flarray? flarray-size flarray-ref))

  #|proc:fxarray->iter
  The `fxarray->iter` procedure returns an iterator over fixnums in the fxarray `source`.
  `(source)` traverses the current full fxarray and reevaluates its size on reset.
  `(source stop)` defaults `start` to 0, and `(source start stop)` defaults `step` to 1.
  `(source start stop step)` selects a half-open indexed range with a nonzero integer `step`.
  A positive `step` visits increasing indexes at that stride; a negative `step` visits
  decreasing indexes at the absolute stride. The iterator returns each selected fixnum.
  |#
  (define fxarray->iter
    (make-indexed-iter 'fxarray->iter fxarray? fxarray-size fxarray-ref))

  #|proc:u8array->iter
  The `u8array->iter` procedure returns an iterator over bytes in the u8array `source`.
  `(source)` traverses the current full u8array and reevaluates its size on reset.
  `(source stop)` defaults `start` to 0, and `(source start stop)` defaults `step` to 1.
  `(source start stop step)` selects a half-open indexed range with a nonzero integer `step`.
  A positive `step` visits increasing indexes at that stride; a negative `step` visits
  decreasing indexes at the absolute stride. The iterator returns each selected byte.
  |#
  (define u8array->iter
    (make-indexed-iter 'u8array->iter u8array? u8array-size u8array-ref))


  #|proc:array->vector
  Convert an array `arr` into a vector.
  |#
  (define-who array->vector
    (lambda (arr)
      (pcheck ([array? arr])
              (let* ([len ($array-size arr)]
                     [vec (make-vector len)])
                (let loop ([i 0])
                  (if (fx= i len)
                      vec
                      (begin (vector-set! vec i (array-ref arr i))
                             (loop (fx1+ i)))))))))


  #|proc:fxarray->fxvector
  Convert a fxarray `arr` into a fxvector.
  |#
  (define-who fxarray->fxvector
    (lambda (arr)
      (pcheck ([fxarray? arr])
              (let* ([len (fxarray-size arr)]
                     [vec (make-fxvector len)])
                (let loop ([i 0])
                  (if (fx= i len)
                      vec
                      (begin (fxvector-set! vec i (fxarray-ref arr i))
                             (loop (fx1+ i)))))))))

  #|proc:u8array->u8vector
  Convert a u8array `arr` into a bytevector.
  |#
  (define-who u8array->u8vector
    (lambda (arr)
      (pcheck ([u8array? arr])
              (let* ([len (u8array-size arr)]
                     [vec (make-bytevector len)])
                (let loop ([i 0])
                  (if (fx= i len)
                      vec
                      (begin (bytevector-u8-set! vec i (u8array-ref arr i))
                             (loop (fx1+ i)))))))))

  ;;;;===----------------------------------------------------------------------===
  ;;;; Public bytearray names (the u8array implementation is retained for compatibility)
  ;;;;===----------------------------------------------------------------------===
  #|proc:bytearray
  Construct a mutable bytearray containing the supplied octets.  The bytearray
  procedures below are aliases of corresponding u8array procedures; a bytearray
  stores values in the inclusive range 0 through 255.
  |#
  (define bytearray u8array)
  (define make-bytearray make-u8array)
  (define bytearray? u8array?)
  (define bytearray-size u8array-size)
  (define bytearray-empty? u8array-empty?)
  (define bytearray-ref u8array-ref)
  (define bytearray-add! u8array-add!)
  (define bytearray-add*! u8array-add*!)
  (define bytearray-delete! u8array-delete!)
  (define bytearray-set! u8array-set!)
  (define bytearray-clear! u8array-clear!)
  (define bytearray-slice u8array-slice)
  (define bytearray-slice! u8array-slice!)
  (define bytearray-copy u8array-copy)
  (define bytearray-copy! u8array-copy!)
  (define bytearray-push! u8array-push!)
  (define bytearray-pop! u8array-pop!)
  (define bytearray-push-back! u8array-push-back!)
  (define bytearray-pop-back! u8array-pop-back!)
  (define bytearray-filter u8array-filter)
  (define bytearray-filter! u8array-filter!)
  (define bytearray-partition u8array-partition)
  (define bytearray-contains? u8array-contains?)
  (define bytearray-contains/p? u8array-contains/p?)
  (define bytearray-index-of u8array-index-of)
  (define bytearray-find-index u8array-find-index)
  (define bytearray-search u8array-search)
  (define bytearray-search* u8array-search*)
  (define bytearray-append u8array-append)
  (define bytearray-append! u8array-append!)
  (define bytearray-reverse u8array-reverse)
  (define bytearray-reverse! u8array-reverse!)
  (define bytearray-map u8array-map)
  (define bytearray-map/i u8array-map/i)
  (define bytearray-map! u8array-map!)
  (define bytearray-map/i! u8array-map/i!)
  (define bytearray-for-each u8array-for-each)
  (define bytearray-for-each/i u8array-for-each/i)
  (define bytearray-map-rev u8array-map-rev)
  (define bytearray-map/i-rev u8array-map/i-rev)
  (define bytearray-for-each-rev u8array-for-each-rev)
  (define bytearray-for-each/i-rev u8array-for-each/i-rev)
  (define bytearray-andmap u8array-andmap)
  (define bytearray-ormap u8array-ormap)
  (define bytearray-fold-left u8array-fold-left)
  (define bytearray-fold-left/i u8array-fold-left/i)
  (define bytearray-fold-right u8array-fold-right)
  (define bytearray-fold-right/i u8array-fold-right/i)
  (define bytearray-sorted? u8array-sorted?)
  (define bytearray-sort u8array-sort)
  (define bytearray-sort! u8array-sort!)
  (define bytearray-iota u8array-iota)
  (define bytearray-nums u8array-nums)
  (define bytearray->list u8array->list)
  (define bytearray->iter u8array->iter)
  (define bytearray->bytevector u8array->u8vector)
  (define bytevector->bytearray u8vector->u8array)

  (define-syntax define-bytearray-width
    (syntax-rules ()
      [(_ ref-name set-name width-ref width-set width)
       (define-bytearray-width ref-name set-name width-ref width-set width (lambda (v) #t))]
      [(_ ref-name set-name width-ref width-set width pred)
       (begin
         (define-who ref-name
           (lambda (arr i)
             (pcheck ([u8array? arr] [natural? i])
                     (let ([n (u8array-size arr)])
                       (when (not (fx= (modulo n width) 0))
                         (errorf who "bytearray length is not aligned to width ~a" width))
                       (if (fx< i (fx/ n width))
                           (width-ref (array-vec arr) (fx* i width))
                           (errorf who "index ~a out of range" i))))))
         (define-who set-name
           (lambda (arr i v)
             (pcheck ([u8array? arr] [natural? i])
                     (let ([n (u8array-size arr)])
                       (when (not (fx= (modulo n width) 0))
                         (errorf who "bytearray length is not aligned to width ~a" width))
                       (if (fx< i (fx/ n width))
                           (begin (unless (pred v) (errorf who "value out of range for width ~a: ~a" width v))
                                  (width-set (array-vec arr) (fx* i width) v))
                                  (errorf who "index ~a out of range" i)))))))]))
  (define bytearray-u8-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 255))))
  (define bytearray-s8-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= -128 v 127))))
  (define bytearray-u16-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 65535))))
  (define bytearray-s16-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= -32768 v 32767))))
  (define bytearray-u24-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 16777215))))
  (define bytearray-s24-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= -8388608 v 8388607))))
  (define bytearray-u32-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 4294967295))))
  (define bytearray-s32-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= -2147483648 v 2147483647))))
  (define bytearray-u40-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 1099511627775))))
  (define bytearray-s40-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= -549755813888 v 549755813887))))
  (define bytearray-u48-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 281474976710655))))
  (define bytearray-s48-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= -140737488355328 v 140737488355327))))
  (define bytearray-u56-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 72057594037927935))))
  (define bytearray-s56-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= -36028797018963968 v 36028797018963967))))
  (define bytearray-u64-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 18446744073709551615))))
  (define bytearray-s64-value? (lambda (v) (and (and (integer? v) (exact? v)) (<= -9223372036854775808 v 9223372036854775807))))
  (define-bytearray-width bytearray-u16-ref bytearray-u16-set!
    (lambda (bv i) (bytevector-u16-ref bv i (endianness little)))
    (lambda (bv i v) (bytevector-u16-set! bv i v (endianness little))) 2
    (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 65535))))
  (define-bytearray-width bytearray-U16-ref bytearray-U16-set!
    (lambda (bv i) (bytevector-u16-ref bv i (endianness big)))
    (lambda (bv i v) (bytevector-u16-set! bv i v (endianness big))) 2
    (lambda (v) (and (and (integer? v) (exact? v)) (<= 0 v 65535))))
  (define-bytearray-width bytearray-s16-ref bytearray-s16-set!
    (lambda (bv i) (bytevector-s16-ref bv i (endianness little)))
    (lambda (bv i v) (bytevector-s16-set! bv i v (endianness little))) 2)
  (define-bytearray-width bytearray-S16-ref bytearray-S16-set!
    (lambda (bv i) (bytevector-s16-ref bv i (endianness big)))
    (lambda (bv i v) (bytevector-s16-set! bv i v (endianness big))) 2)
  (define-bytearray-width bytearray-fp32-ref bytearray-fp32-set!
    (lambda (bv i) (bytevector-ieee-single-ref bv i (endianness little)))
    (lambda (bv i v) (bytevector-ieee-single-set! bv i v (endianness little))) 4)
  (define-bytearray-width bytearray-FP32-ref bytearray-FP32-set!
    (lambda (bv i) (bytevector-ieee-single-ref bv i (endianness big)))
    (lambda (bv i v) (bytevector-ieee-single-set! bv i v (endianness big))) 4)
  (define-bytearray-width bytearray-u8-ref bytearray-u8-set!
    (lambda (bv i) (bytevector-u8-ref bv i))
    (lambda (bv i v) (bytevector-u8-set! bv i v)) 1 bytearray-u8-value?)
  (define-bytearray-width bytearray-U8-ref bytearray-U8-set!
    (lambda (bv i) (bytevector-u8-ref bv i))
    (lambda (bv i v) (bytevector-u8-set! bv i v)) 1 bytearray-u8-value?)
  (define-bytearray-width bytearray-s8-ref bytearray-s8-set!
    (lambda (bv i) (bytevector-s8-ref bv i))
    (lambda (bv i v) (bytevector-s8-set! bv i v)) 1 bytearray-s8-value?)
  (define-bytearray-width bytearray-S8-ref bytearray-S8-set!
    (lambda (bv i) (bytevector-s8-ref bv i))
    (lambda (bv i v) (bytevector-s8-set! bv i v)) 1 bytearray-s8-value?)
  (define-bytearray-width bytearray-u24-ref bytearray-u24-set! (lambda (bv i) (bytevector-u24-ref bv i (endianness little))) (lambda (bv i v) (bytevector-u24-set! bv i v (endianness little))) 3 bytearray-u24-value?)
  (define-bytearray-width bytearray-U24-ref bytearray-U24-set! (lambda (bv i) (bytevector-u24-ref bv i (endianness big))) (lambda (bv i v) (bytevector-u24-set! bv i v (endianness big))) 3 bytearray-u24-value?)
  (define-bytearray-width bytearray-s24-ref bytearray-s24-set! (lambda (bv i) (bytevector-s24-ref bv i (endianness little))) (lambda (bv i v) (bytevector-s24-set! bv i v (endianness little))) 3 bytearray-s24-value?)
  (define-bytearray-width bytearray-S24-ref bytearray-S24-set! (lambda (bv i) (bytevector-s24-ref bv i (endianness big))) (lambda (bv i v) (bytevector-s24-set! bv i v (endianness big))) 3 bytearray-s24-value?)
  (define-bytearray-width bytearray-u32-ref bytearray-u32-set! (lambda (bv i) (bytevector-u32-ref bv i (endianness little))) (lambda (bv i v) (bytevector-u32-set! bv i v (endianness little))) 4 bytearray-u32-value?)
  (define-bytearray-width bytearray-U32-ref bytearray-U32-set! (lambda (bv i) (bytevector-u32-ref bv i (endianness big))) (lambda (bv i v) (bytevector-u32-set! bv i v (endianness big))) 4 bytearray-u32-value?)
  (define-bytearray-width bytearray-s32-ref bytearray-s32-set! (lambda (bv i) (bytevector-s32-ref bv i (endianness little))) (lambda (bv i v) (bytevector-s32-set! bv i v (endianness little))) 4 bytearray-s32-value?)
  (define-bytearray-width bytearray-S32-ref bytearray-S32-set! (lambda (bv i) (bytevector-s32-ref bv i (endianness big))) (lambda (bv i v) (bytevector-s32-set! bv i v (endianness big))) 4 bytearray-s32-value?)
  (define-bytearray-width bytearray-u40-ref bytearray-u40-set! (lambda (bv i) (bytevector-u40-ref bv i (endianness little))) (lambda (bv i v) (bytevector-u40-set! bv i v (endianness little))) 5 bytearray-u40-value?)
  (define-bytearray-width bytearray-U40-ref bytearray-U40-set! (lambda (bv i) (bytevector-u40-ref bv i (endianness big))) (lambda (bv i v) (bytevector-u40-set! bv i v (endianness big))) 5 bytearray-u40-value?)
  (define-bytearray-width bytearray-s40-ref bytearray-s40-set! (lambda (bv i) (bytevector-s40-ref bv i (endianness little))) (lambda (bv i v) (bytevector-s40-set! bv i v (endianness little))) 5 bytearray-s40-value?)
  (define-bytearray-width bytearray-S40-ref bytearray-S40-set! (lambda (bv i) (bytevector-s40-ref bv i (endianness big))) (lambda (bv i v) (bytevector-s40-set! bv i v (endianness big))) 5 bytearray-s40-value?)
  (define-bytearray-width bytearray-u48-ref bytearray-u48-set! (lambda (bv i) (bytevector-u48-ref bv i (endianness little))) (lambda (bv i v) (bytevector-u48-set! bv i v (endianness little))) 6 bytearray-u48-value?)
  (define-bytearray-width bytearray-U48-ref bytearray-U48-set! (lambda (bv i) (bytevector-u48-ref bv i (endianness big))) (lambda (bv i v) (bytevector-u48-set! bv i v (endianness big))) 6 bytearray-u48-value?)
  (define-bytearray-width bytearray-s48-ref bytearray-s48-set! (lambda (bv i) (bytevector-s48-ref bv i (endianness little))) (lambda (bv i v) (bytevector-s48-set! bv i v (endianness little))) 6 bytearray-s48-value?)
  (define-bytearray-width bytearray-S48-ref bytearray-S48-set! (lambda (bv i) (bytevector-s48-ref bv i (endianness big))) (lambda (bv i v) (bytevector-s48-set! bv i v (endianness big))) 6 bytearray-s48-value?)
  (define-bytearray-width bytearray-u56-ref bytearray-u56-set! (lambda (bv i) (bytevector-u56-ref bv i (endianness little))) (lambda (bv i v) (bytevector-u56-set! bv i v (endianness little))) 7 bytearray-u56-value?)
  (define-bytearray-width bytearray-U56-ref bytearray-U56-set! (lambda (bv i) (bytevector-u56-ref bv i (endianness big))) (lambda (bv i v) (bytevector-u56-set! bv i v (endianness big))) 7 bytearray-u56-value?)
  (define-bytearray-width bytearray-s56-ref bytearray-s56-set! (lambda (bv i) (bytevector-s56-ref bv i (endianness little))) (lambda (bv i v) (bytevector-s56-set! bv i v (endianness little))) 7 bytearray-s56-value?)
  (define-bytearray-width bytearray-S56-ref bytearray-S56-set! (lambda (bv i) (bytevector-s56-ref bv i (endianness big))) (lambda (bv i v) (bytevector-s56-set! bv i v (endianness big))) 7 bytearray-s56-value?)
  (define-bytearray-width bytearray-u64-ref bytearray-u64-set! (lambda (bv i) (bytevector-u64-ref bv i (endianness little))) (lambda (bv i v) (bytevector-u64-set! bv i v (endianness little))) 8 bytearray-u64-value?)
  (define-bytearray-width bytearray-U64-ref bytearray-U64-set! (lambda (bv i) (bytevector-u64-ref bv i (endianness big))) (lambda (bv i v) (bytevector-u64-set! bv i v (endianness big))) 8 bytearray-u64-value?)
  (define-bytearray-width bytearray-s64-ref bytearray-s64-set! (lambda (bv i) (bytevector-s64-ref bv i (endianness little))) (lambda (bv i v) (bytevector-s64-set! bv i v (endianness little))) 8 bytearray-s64-value?)
  (define-bytearray-width bytearray-S64-ref bytearray-S64-set! (lambda (bv i) (bytevector-s64-ref bv i (endianness big))) (lambda (bv i v) (bytevector-s64-set! bv i v (endianness big))) 8 bytearray-s64-value?)
  (define-bytearray-width bytearray-fp64-ref bytearray-fp64-set! (lambda (bv i) (bytevector-ieee-double-ref bv i (endianness little))) (lambda (bv i v) (bytevector-ieee-double-set! bv i v (endianness little))) 8 flonum?)
  (define-bytearray-width bytearray-FP64-ref bytearray-FP64-set! (lambda (bv i) (bytevector-ieee-double-ref bv i (endianness big))) (lambda (bv i v) (bytevector-ieee-double-set! bv i v (endianness big))) 8 flonum?)

  #|proc:bytearray-u16-add!
  Add a logical unsigned 16-bit value to bytearray `arr`.
  |#
  (define-who bytearray-u16-add!
    (case-lambda
      [(arr v) (bytearray-u16-add! arr (fx/ (u8array-size arr) 2) v)]
      [(arr i v)
       (pcheck ([u8array? arr] [natural? i])
               (let* ([old (bytearray->bytevector arr)] [n (bytevector-length old)]
                      [nv (make-bytevector (fx+ n 2) 0)])
                 (bytevector-copy! old 0 nv 0 (fx* i 2))
                 (bytevector-u16-set! nv (fx* i 2) v (endianness little))
                 (bytevector-copy! old (fx* i 2) nv (fx* (fx1+ i) 2) (fx- n (fx* i 2)))
                 (u8array-clear! arr)
                 (let loop ([j 0])
                   (when (fx< j (bytevector-length nv))
                     (u8array-add! arr (bytevector-u8-ref nv j))
                     (loop (fx1+ j))))))]))
  (define bytearray-U16-add! bytearray-u16-add!)
  #|proc:bytearray-u16-delete!
  Delete the logical unsigned 16-bit value at index `i` from `arr`.
  |#
  (define-who bytearray-u16-delete!
    (lambda (arr i)
      (pcheck ([u8array? arr] [natural? i])
              (let* ([old (bytearray->bytevector arr)] [n (bytevector-length old)]
                     [nv (make-bytevector (fx- n 2) 0)])
                (bytevector-copy! old 0 nv 0 (fx* i 2))
                (bytevector-copy! old (fx* (fx1+ i) 2) nv (fx* i 2) (fx- n (fx* (fx1+ i) 2)))
                (u8array-clear! arr)
                (let loop ([j 0])
                  (when (fx< j (bytevector-length nv))
                    (u8array-add! arr (bytevector-u8-ref nv j))
                    (loop (fx1+ j))))))))
  (define bytearray-U16-delete! bytearray-u16-delete!)
  #|proc:bytearray-u16->list
  Convert logical unsigned 16-bit values in bytearray `arr` to a list.
  |#
  (define bytearray-u16->list
    (lambda (arr) (pcheck ([u8array? arr]) (let loop ([i 0] [r '()])
      (if (fx= i (fx/ (u8array-size arr) 2)) (reverse r)
          (loop (fx1+ i) (cons (bytearray-u16-ref arr i) r)))))))
  (define bytearray-U16->list bytearray-u16->list)
  #|proc:bytearray-u16-map
  Map `proc` over logical little-endian 16-bit values in bytearray `arr`.
  |#
  (define bytearray-u16-map
    (lambda (proc arr)
      (pcheck ([procedure? proc] [u8array? arr])
              (let* ([n (u8array-size arr)] [out (make-bytevector n 0)])
                (when (not (fx= 0 (modulo n 2))) (errorf 'bytearray-u16-map "unaligned bytearray"))
                (let loop ([i 0])
                  (if (fx= i n) (bytevector->bytearray out)
                      (begin (bytevector-u16-set! out i (proc (bytevector-u16-ref (array-vec arr) i (endianness little))) (endianness little))
                             (loop (fx+ i 2)))))))))
  (define bytearray-U16-map bytearray-u16-map)
  #|proc:bytearray-u16-for-each
  Call `proc` for each logical little-endian 16-bit value in bytearray `arr`.
  |#
  (define bytearray-u16-for-each
    (lambda (proc arr)
      (pcheck ([procedure? proc] [u8array? arr])
              (let ([n (u8array-size arr)])
                (when (not (fx= 0 (modulo n 2))) (errorf 'bytearray-u16-for-each "unaligned bytearray"))
                (let loop ([i 0])
                  (unless (fx= i n)
                    (proc (bytevector-u16-ref (array-vec arr) i (endianness little)))
                    (loop (fx+ i 2))))))))
  (define bytearray-U16-for-each bytearray-u16-for-each)
  #|proc:bytearray-u16->iter
  Return an iterator over logical little-endian 16-bit values in `arr`.
  |#
  (define bytearray-u16->iter
    (make-indexed-iter 'bytearray-u16->iter u8array?
      (lambda (arr) (fx/ (u8array-size arr) 2)) bytearray-u16-ref))
  (define bytearray-U16->iter bytearray-u16->iter)


  (define-syntax gen-array-record-writer
    (syntax-rules ()
      [(_ arr header vref)
       (record-writer (type-descriptor arr)
                      (lambda (r p wr)
                        (display header p)
                        (let ([v (array-vec r)] [len ($array-size r)])
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
                                      (let ([len1 ($array-size arr1)] [vec1 (array-vec arr1)]
                                            [len2 ($array-size arr2)] [vec2 (array-vec arr2)])
                                        (and (fx= len1 len2)
                                             (let loop ([i 0])
                                               (if (fx= i len1)
                                                   #t
                                                   (and (=? (vref vec1 i) (vref vec2 i))
                                                        (loop (fx1+ i)))))))))]))

;;;;===----------------------------------------------------------------------===
;;;; Iterator extension registration
;;;;===----------------------------------------------------------------------===

  (iter-register-source!
   array?
   (lambda (arr)
     (cond [(fxarray? arr) (fxarray->iter arr)]
           [(flarray? arr) (flarray->iter arr)]
           [(u8array? arr) (u8array->iter arr)]
           [else (array->iter arr)])))

;;;;===----------------------------------------------------------------------===
;;;; Navigator extension registration
;;;;===----------------------------------------------------------------------===

  (nav-register-indexed!
   array? array-size
   (lambda (arr index)
     (cond [(fxarray? arr) (fxarray-ref arr index)]
           [(flarray? arr) (flarray-ref arr index)]
           [(u8array? arr) (u8array-ref arr index)]
           [else (array-ref arr index)]))
   (lambda (arr index value)
     (let ([copy (cond [(fxarray? arr) (fxarray-copy arr)]
                       [(flarray? arr) (flarray-copy arr)]
                       [(u8array? arr) (u8array-copy arr)]
                       [else (array-copy arr)])])
       (cond [(fxarray? copy) (fxarray-set! copy index value)]
             [(flarray? copy) (flarray-set! copy index value)]
             [(u8array? copy) (u8array-set! copy index value)]
             [else (array-set! copy index value)])
       copy))
   (lambda (arr index value)
     (cond [(fxarray? arr) (fxarray-set! arr index value)]
           [(flarray? arr) (flarray-set! arr index value)]
           [(u8array? arr) (u8array-set! arr index value)]
           [else (array-set! arr index value)])
     arr))

  (gen-array-record-writer $array   "#[array ("   vector-ref)
  (gen-array-record-writer $fxarray "#[fxarray (" fxvector-ref)
  (gen-array-record-writer $flarray "#[flarray (" flvector-ref)
  (gen-array-record-writer $u8array "#[u8array (" bytevector-u8-ref)

  (gen-array-record-type-equal-procedure $array   vector-ref)
  (gen-array-record-type-equal-procedure $fxarray fxvector-ref)
  (gen-array-record-type-equal-procedure $flarray flvector-ref)
  (gen-array-record-type-equal-procedure $u8array bytevector-u8-ref)

  )
