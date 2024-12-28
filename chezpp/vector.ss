(library (chezpp vector)
  (export vmap vmap/i vmap! vmap!/i vfor-each vfor-each/i
          fxvmap fxvmap/i fxvmap! fxvmap!/i fxvfor-each fxvfor-each/i
          flvmap flvmap/i flvmap! flvmap!/i flvfor-each flvfor-each/i

          vector-map/i vector-for-each/i vector-map! vector-map!/i
          fxvector-map/i fxvector-for-each/i fxvector-map! fxvector-map!/i
          flvector-map/i flvector-for-each/i flvector-map! flvector-map!/i

          vslice fxvslice flvslice
          vfilter fxvfilter flvfilter

          vormap vandmap vexists vfor-all
          fxvormap fxvandmap fxvexists fxvfor-all
          flvormap flvandmap flvexists flvfor-all

          vmemp vmember vmemq vmemv
          fxvmemp fxvmember fxvmemq fxvmemv
          flvmemp flvmember flvmemq flvmemv

          vfold-left vfold-right vfold-left/i vfold-right/i
          fxvfold-left fxvfold-right fxvfold-left/i fxvfold-right/i
          flvfold-left flvfold-right flvfold-left/i flvfold-right/i

          vreverse fxvreverse flvreverse
          vreverse! fxvreverse! flvreverse!
          vzip fxvzip flvzip vzipv fxvzipv flvzipv
          vshuffle  fxvshuffle  flvshuffle
          vshuffle! fxvshuffle! flvshuffle!

          vcopy fxvcopy flvcopy
          vcopy! fxvcopy! flvcopy! u8vcopy! vector-copy! fxvector-copy! flvector-copy!
          vsum fxvsum flvsum
          vextreme fxvextreme flvextreme
          vmax vmin fxvmax fxvmin flvmax flvmin
          vavg fxvavg flvavg
          viota fxviota
          vnums fxvnums flvnums

          random-vector random-fxvector random-flvector)
  (import (chezscheme)
          (chezpp utils)
          (chezpp internal))

  (define-syntax gen-random-vector
    (syntax-rules ()
      [(_ name vmake set seed nproc)
       (define name
         (case-lambda
           [(len) (name len seed)]
           [(len n) (pcheck-natural (len)
                                    (let ([vec (vmake len)])
                                      (let loop ([i 0])
                                        (if (fx= i len)
                                            vec
                                            (begin (set vec i (random (nproc n)))
                                                   (loop (add1 i)))))))]))]))
  (gen-random-vector random-vector   make-vector   vector-set!   (most-positive-fixnum) id)
  (gen-random-vector random-fxvector make-fxvector fxvector-set! (most-positive-fixnum) id)
  (gen-random-vector random-flvector make-flvector flvector-set! (inexact (most-positive-fixnum)) inexact)

  (define all-vecs?   (lambda (x*) (andmap vector?   x*)))
  (define all-fxvecs? (lambda (x*) (andmap fxvector? x*)))
  (define all-flvecs? (lambda (x*) (andmap flvector? x*)))

  (define-syntax gen-check-length
    (syntax-rules ()
      [(_ name vlength)
       (define name
         (lambda (who . vecs)
           (unless (null? vecs)
             (unless (apply fx= (map vlength vecs))
               (errorf who "vectors are not of the same length")))))]))
  (gen-check-length check-length   vector-length)
  (gen-check-length check-fxlength fxvector-length)
  (gen-check-length check-fllength flvector-length)



  ;; A generic vector procedure definition macro
  ;; that automatically expands into procedure definitions for 3 types of vectors:
  ;; vector, fxvector, and flvector.
  ;; Both lambda and case-lambda like definition are possible.
  ;; Use any of the type flags (v fxv flv) to decide which types to specialize for.
  ;;
  ;; Type-specialized implicit bindings available in the body:
  ;; - v?: vector?, fxvector?, flvector?, depending on the given type flags
  ;; - vmake: make-vector, ...
  ;; - vref: vector-ref, ...
  ;; - vset!: vector-set!, ...
  ;; - vlength: vector-length, ...
  ;; - vpcheck: pcheck-vector, ...
  ;; - vcheck-length: check-length, check-fxlength, ...
  ;; - all-which?: all-vecs?, all-fxvecs, ...
  ;; - procname: this procedure name. Given `foo`, defines `vfoo`, `fxvfoo` and `flvfoo`, if all flags are given.
  ;;             If the given name starts with `v`, e.g, `vfoo`, then the names are still as the above.
  ;; - thisproc: bound to the current procedure, used for recursive call
  ;; - t+, t-, t*, t/: fx+, fx-, ..., in the case of fxv
  ;; - t+id t*id: the identity element in the additive/multiplicative group of fixnums/flonums
  ;; - t> t<: fx>, fx< in the case of fxv
  (define-syntax define-vector-procedure
    (lambda (stx)
      (define valid-ty*?
        (lambda (ty*)
          (if (null? (remp (lambda (x) (memq x '(v fxv flv))) ty*))
              #t
              (syntax-error ty* "define-vector-procedure: bad vector type flags (allowed ones: v fxv flv):"))))
      (define handle-ty*
        (lambda (ty*)
          (values (memq 'v ty*) (memq 'fxv ty*) (memq 'flv ty*))))
      (define get-name
        (lambda (which name)
          (let ([n (symbol->string (syntax->datum name))]
                [pre1 '((v . v) (fxv . fxv) (flv . flv))]
                [pre2 '((v . "") (fxv . fx) (flv . fl))])
            (if (eqv? (string-ref n 0) #\v)
                ($construct-name name (cdr (assoc which pre2)) n)
                ($construct-name name (cdr (assoc which pre1)) n)))))
      (syntax-case stx ()
        [(k (ty* ...) name [args body body* ...] ...)
         (and (identifier? #'name) (valid-ty*? (datum (ty* ...))))
         (let-values ([(pv? pfxv? pflv?) (handle-ty* (datum (ty* ...)))])
           (with-implicit (k v v? vmake vref vset! vlength vpcheck vcheck-length all-which? procname thisproc
                             t+ t- t* t/ t+id t*id t> t<)
             #`(begin
                 #,(if pv?
                       (with-syntax ([name (get-name 'v #'name)])
                         #`(module (name)
                             (define v     vector)
                             (define v?    vector?)
                             (define vmake make-vector)
                             (define vref  vector-ref)
                             (define vset! vector-set!)
                             (define vlength vector-length)
                             (define vcheck-length check-length)
                             (define all-which? all-vecs?)
                             (define procname 'name)
                             (define t+ +)   (define t- -)
                             (define t* *)   (define t/ /)
                             (define t+id 0) (define t*id 1)
                             (define t> >)   (define t< <)
                             (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-vector e* (... ...))])])
                               (define name
                                 (case-lambda
                                   [args body body* ...] ...))
                               (define thisproc name))))
                       ;; to create a proper definition context
                       #'(define dummy0 'dummy))
                 #,(if pfxv?
                       (with-syntax ([name (get-name 'fxv #'name)])
                         #`(module (name)
                             (define v     fxvector)
                             (define v?    fxvector?)
                             (define vmake make-fxvector)
                             (define vref  fxvector-ref)
                             (define vset! fxvector-set!)
                             (define vlength fxvector-length)
                             (define vcheck-length check-fxlength)
                             (define all-which? all-fxvecs?)
                             (define procname 'name)
                             (define t+ fx+)
                             (define t- fx-)
                             (define t* fx*)
                             (define t/ fx/)
                             (define t+id 0)
                             (define t*id 1)
                             (define t> fx>)   (define t< fx<)
                             (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-fxvector e* (... ...))])])
                               (define name
                                 (case-lambda
                                   [args body body* ...] ...))
                               (define thisproc name))))
                       #'(define dummy1 'dummy))
                 #,(if pflv?
                       (with-syntax ([name (get-name 'flv #'name)])
                         #`(module (name)
                             (define v     flvector)
                             (define v?    flvector?)
                             (define vmake make-flvector)
                             (define vref  flvector-ref)
                             (define vset! flvector-set!)
                             (define vlength flvector-length)
                             (define vcheck-length check-fllength)
                             (define all-which? all-flvecs?)
                             (define procname 'name)
                             (define t+ fl+)
                             (define t- fl-)
                             (define t* fl*)
                             (define t/ fl/)
                             (define t+id 0.0)
                             (define t*id 1.0)
                             (define t> fl>)   (define t< fl<)
                             (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-flvector e* (... ...))])])
                               (define name
                                 (case-lambda
                                   [args body body* ...] ...))
                               (define thisproc name))))
                       #'(define dummy2 'dummy)))))]
        [(k (ty* ...) (name . args) body* ...)
         (and (identifier? #'name) (valid-ty*? (datum (ty* ...))))
         (let-values ([(pv? pfxv? pflv?) (handle-ty* (datum (ty* ...)))])
           (with-implicit (k v? vmake vref vset! vlength vpcheck vcheck-length all-which? procname
                             t+ t- t* t/ t+id t*id t> t<)
             #`(begin
                 #,(if pv?
                       (with-syntax ([name (get-name 'v #'name)])
                         #`(define name
                             (lambda args
                               (let ([v     vector]
                                     [v?    vector?]
                                     [vmake make-vector]
                                     [vref  vector-ref]
                                     [vset! vector-set!]
                                     [vlength vector-length]
                                     [vcheck-length check-length]
                                     [all-which? all-vecs?]
                                     [procname 'name]
                                     [thisproc name]
                                     [t+ +] [t- -] [t* *] [t/ /] [t+id 0] [t*id 1]
                                     [t> >] [t< <])
                                 ;; this piece of syntax needs care
                                 (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-vector e* (... ...))])])
                                   body* ...)))))
                       ;; to create a proper definition context
                       #'(define dummy0 'dummy))
                 #,(if pfxv?
                       (with-syntax ([name (get-name 'fxv #'name)])
                         #`(define name
                             (lambda args
                               (let ([v     fxvector]
                                     [v?    fxvector?]
                                     [vmake make-fxvector]
                                     [vref  fxvector-ref]
                                     [vset! fxvector-set!]
                                     [vlength fxvector-length]
                                     [vcheck-length check-fxlength]
                                     [all-which? all-fxvecs?]
                                     [procname 'name]
                                     [thisproc name]
                                     [t+ fx+] [t- fx-] [t* fx*] [t/ fx/] [t+id 0] [t*id 1]
                                     [t> fx>] [t< fx<])
                                 (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-fxvector e* (... ...))])])
                                   body* ...)))))
                       #'(define dummy1 'dummy))
                 #,(if pflv?
                       (with-syntax ([name (get-name 'flv #'name)])
                         #`(define name
                             (lambda args
                               (let ([v     flvector]
                                     [v?    flvector?]
                                     [vmake make-flvector]
                                     [vref  flvector-ref]
                                     [vset! flvector-set!]
                                     [vlength flvector-length]
                                     [vcheck-length check-fllength]
                                     [all-which? all-flvecs?]
                                     [procname 'name]
                                     [thisproc name]
                                     [t+ fl+] [t- fl-] [t* fl*] [t/ fl/] [t+id 0.0] [t*id 1.0]
                                     [t> fl>] [t< fl<])
                                 (let-syntax ([vpcheck (syntax-rules () [(_ e* (... ...)) (pcheck-flvector e* (... ...))])])
                                   body* ...)))))
                       #'(define dummy2 'dummy)))))])))


  (define-vector-procedure (v fxv flv) fold-left
    ;; specialize for 3 cases to avoid `apply` overhead
    [(proc acc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let ([l (vlength vec0)])
               (let loop ([i 0] [acc acc])
                 (if (fx= i l)
                     acc
                     (loop (add1 i) (proc acc (vref vec0 i)))))))]
    [(proc acc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let ([l (vlength vec0)])
               (let loop ([i 0] [acc acc])
                 (if (fx= i l)
                     acc
                     (loop (add1 i) (proc acc (vref vec0 i) (vref vec1 i)))))))]
    [(proc acc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let ([l (vlength vec0)])
               (let loop ([i 0] [acc acc])
                 (if (fx= i l)
                     acc
                     (loop (add1 i) (proc acc (vref vec0 i) (vref vec1 i) (vref vec2 i)))))))]
    [(proc acc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let ([l (vlength vec0)])
                 (let loop ([i 0] [acc acc])
                   (if (fx= i l)
                       acc
                       (loop (add1 i) (apply proc acc (vecref* i))))))))])


  (define-vector-procedure (v fxv flv) fold-left/i
    [(proc acc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let ([l (vlength vec0)])
               (let loop ([i 0] [acc acc])
                 (if (fx= i l)
                     acc
                     (loop (add1 i) (proc i acc (vref vec0 i)))))))]
    [(proc acc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let ([l (vlength vec0)])
               (let loop ([i 0] [acc acc])
                 (if (fx= i l)
                     acc
                     (loop (add1 i) (proc i acc (vref vec0 i) (vref vec1 i)))))))]
    [(proc acc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let ([l (vlength vec0)])
               (let loop ([i 0] [acc acc])
                 (if (fx= i l)
                     acc
                     (loop (add1 i) (proc i acc (vref vec0 i) (vref vec1 i) (vref vec2 i)))))))]
    [(proc acc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let ([l (vlength vec0)])
                 (let loop ([i 0] [acc acc])
                   (if (fx= i l)
                       acc
                       (loop (add1 i) (apply proc i acc (vecref* i))))))))])


  (define-vector-procedure (v fxv flv) fold-right
    [(proc acc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let ([l (vlength vec0)])
               (let loop ([i (sub1 l)] [acc acc])
                 (if (fx= i -1)
                     acc
                     (loop (sub1 i) (proc (vref vec0 i) acc))))))]
    [(proc acc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let ([l (vlength vec0)])
               (let loop ([i (sub1 l)] [acc acc])
                 (if (fx= i -1)
                     acc
                     (loop (sub1 i) (proc (vref vec0 i) (vref vec1 i) acc))))))]
    [(proc acc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let ([l (vlength vec0)])
               (let loop ([i (sub1 l)] [acc acc])
                 (if (fx= i -1)
                     acc
                     (loop (sub1 i) (proc (vref vec0 i) (vref vec1 i) (vref vec2 i) acc))))))]
    [(proc acc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let* ([l (vlength vec0)])
                 (let loop ([i (sub1 l)] [acc acc])
                   (if (fx= i -1)
                       acc
                       (loop (sub1 i) (apply proc `(,@(vecref* i) ,acc))))))))])


  (define-vector-procedure (v fxv flv) fold-right/i
    [(proc acc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let ([l (vlength vec0)])
               (let loop ([i (sub1 l)] [acc acc])
                 (if (fx= i -1)
                     acc
                     ;; this has overhead
                     (loop (sub1 i) (proc i (vref vec0 i) acc))))))]
    [(proc acc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let ([l (vlength vec0)])
               (let loop ([i (sub1 l)] [acc acc])
                 (if (fx= i -1)
                     acc
                     (loop (sub1 i) (proc i (vref vec0 i) (vref vec1 i) acc))))))]
    [(proc acc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let ([l (vlength vec0)])
               (let loop ([i (sub1 l)] [acc acc])
                 (if (fx= i -1)
                     acc
                     (loop (sub1 i) (proc i (vref vec0 i) (vref vec1 i) (vref vec2 i) acc))))))]
    [(proc acc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let* ([l (vlength vec0)])
                 (let loop ([i (sub1 l)] [acc acc])
                   (if (fx= i -1)
                       acc
                       (loop (sub1 i) (apply proc i `(,@(vecref* i) ,acc))))))))])


  (define-vector-procedure (v fxv flv) andmap
    [(proc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     #t
                     (if (proc (vref vec0 i))
                         (loop (add1 i))
                         #f)))))]
    [(proc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     #t
                     (if (proc (vref vec0 i) (vref vec1 i))
                         (loop (add1 i))
                         #f)))))]
    [(proc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     #t
                     (if (proc (vref vec0 i) (vref vec1 i) (vref vec2 i))
                         (loop (add1 i))
                         #f)))))]
    [(proc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let ([l (vlength vec0)])
                 (let loop ([i 0])
                   (if (fx= i l)
                       #t
                       (if (apply proc (vecref* i))
                           (loop (add1 i))
                           #f))))))])


  (define-vector-procedure (v fxv flv) ormap
    [(proc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     #f
                     (if (proc (vref vec0 i))
                         #t
                         (loop (add1 i)))))))]
    [(proc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     #f
                     (if (proc (vref vec0 i) (vref vec1 i))
                         #t
                         (loop (add1 i)))))))]
    [(proc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     #f
                     (if (proc (vref vec0 i) (vref vec1 i) (vref vec2 i))
                         #t
                         (loop (add1 i)))))))]
    [(proc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let ([l (vlength vec0)])
                 (let loop ([i 0])
                   (if (fx= i l)
                       #f
                       (if (apply proc (vecref* i))
                           #t
                           (loop (add1 i))))))))])


  (define-vector-procedure (fxv flv) vector-map
    [(proc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let* ([l (vlength vec0)]
                    [res (vmake l)])
               (let loop ([i 0])
                 (if (fx= i l)
                     res
                     (begin (vset! res i (proc (vref vec0 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let* ([l (vlength vec0)]
                    [res (vmake l)])
               (let loop ([i 0])
                 (if (fx= i l)
                     res
                     (begin (vset! res i (proc (vref vec0 i) (vref vec1 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let* ([l (vlength vec0)]
                    [res (vmake l)])
               (let loop ([i 0])
                 (if (fx= i l)
                     res
                     (begin (vset! res i (proc (vref vec0 i) (vref vec1 i) (vref vec2 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let* ([l (vlength vec0)]
                      [res (vmake l)])
                 (let loop ([i 0])
                   (if (fx= i l)
                       res
                       (begin (vset! res i (apply proc (vecref* i)))
                              (loop (add1 i))))))))])


  (define-vector-procedure (fxv flv) vector-for-each
    [(proc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (unless (fx= i l)
                   (proc (vref vec0 i))
                   (loop (add1 i))))))]
    [(proc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (unless (fx= i l)
                   (proc (vref vec0 i) (vref vec1 i))
                   (loop (add1 i))))))]
    [(proc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (unless (fx= i l)
                   (proc (vref vec0 i) (vref vec1 i) (vref vec2 i))
                   (loop (add1 i))))))]
    [(proc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let ([l (vlength vec0)])
                 (let loop ([i 0])
                   (unless (fx= i l)
                     (apply proc (vecref* i))
                     (loop (add1 i)))))))])


  (define-vector-procedure (v fxv flv) vector-map/i
    [(proc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let* ([l (vlength vec0)]
                    [res (vmake l)])
               (let loop ([i 0])
                 (if (fx= i l)
                     res
                     (begin (vset! res i (proc i (vref vec0 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let* ([l (vlength vec0)]
                    [res (vmake l)])
               (let loop ([i 0])
                 (if (fx= i l)
                     res
                     (begin (vset! res i (proc i (vref vec0 i) (vref vec1 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let* ([l (vlength vec0)]
                    [res (vmake l)])
               (let loop ([i 0])
                 (if (fx= i l)
                     res
                     (begin (vset! res i (proc i (vref vec0 i) (vref vec1 i) (vref vec2 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let* ([l (vlength vec0)]
                      [res (vmake l)])
                 (let loop ([i 0])
                   (if (fx= i l)
                       res
                       (begin (vset! res i (apply proc i (vecref* i)))
                              (loop (add1 i))))))))])


  (define-vector-procedure (v fxv flv) vector-for-each/i
    [(proc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (unless (fx= i l)
                   (proc i (vref vec0 i))
                   (loop (add1 i))))))]
    [(proc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (unless (fx= i l)
                   (proc i (vref vec0 i) (vref vec1 i))
                   (loop (add1 i))))))]
    [(proc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let ([l (vlength vec0)])
               (let loop ([i 0])
                 (unless (fx= i l)
                   (proc i (vref vec0 i) (vref vec1 i) (vref vec2 i))
                   (loop (add1 i))))))]
    [(proc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let ([l (vlength vec0)])
                 (let loop ([i 0])
                   (unless (fx= i l)
                     (apply proc i (vecref* i))
                     (loop (add1 i)))))))])


  (define-vector-procedure (v fxv flv) vector-map!
    [(proc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let* ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     vec0
                     (begin (vset! vec0 i (proc (vref vec0 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let* ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     vec0
                     (begin (vset! vec0 i (proc (vref vec0 i) (vref vec1 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let* ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     vec0
                     (begin (vset! vec0 i (proc (vref vec0 i) (vref vec1 i) (vref vec2 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let* ([l (vlength vec0)])
                 (let loop ([i 0])
                   (if (fx= i l)
                       vec0
                       (begin (vset! vec0 i (apply proc (vecref* i)))
                              (loop (add1 i))))))))])


  (define-vector-procedure (v fxv flv) vector-map!/i
    [(proc vec0)
     (pcheck ([procedure? proc]
              [v? vec0])
             (let* ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     vec0
                     (begin (vset! vec0 i (proc i (vref vec0 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 vec1)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1])
             (vcheck-length procname vec0 vec1)
             (let* ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     vec0
                     (begin (vset! vec0 i (proc i (vref vec0 i) (vref vec1 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 vec1 vec2)
     (pcheck ([procedure? proc]
              [v? vec0]
              [v? vec1]
              [v? vec2])
             (vcheck-length procname vec0 vec1 vec2)
             (let* ([l (vlength vec0)])
               (let loop ([i 0])
                 (if (fx= i l)
                     vec0
                     (begin (vset! vec0 i (proc i (vref vec0 i) (vref vec1 i) (vref vec2 i)))
                            (loop (add1 i)))))))]
    [(proc vec0 . vecs)
     (let* ([vec* (cons vec0 vecs)]
            [vecref* (lambda (i) (map (lambda (v) (vref v i)) vec*))])
       (pcheck ([procedure? proc]
                [all-which? vec*])
               (apply vcheck-length procname vec*)
               (let* ([l (vlength vec0)])
                 (let loop ([i 0])
                   (if (fx= i l)
                       vec0
                       (begin (vset! vec0 i (apply proc i (vecref* i)))
                              (loop (add1 i))))))))])


  #|doc
  Return a slice, or subvector of the original vector.

  Meanings of `start`, `end` and `step` are the same as in list:slice.

  If the indices are out of range in any way, an empty vector is returned.
  |#
  (define-vector-procedure (v fxv flv) slice
    [(vec end) (thisproc vec 0 end 1)]
    [(vec start end) (thisproc vec start end 1)]
    [(vec start end step)
     (vpcheck (vec)
              (pcheck ([fixnum? start end step])
                      (when (fx= step 0) (errorf procname "step cannot be 0"))
                      (let* ([len (vlength vec)]
                             [s (let ([s (if (fx>= start 0) start (fx+ len start))])
                                  (cond [(fx< s 0) 0]
                                        [(fx> s len) (fx1- len)]
                                        [else s]))]
                             [e (let ([e (if (fx>= end 0) end (fx+ len end))])
                                  (cond [(fx<= e -1) -1]
                                        [(fx>= e len) len]
                                        [else e]))])
                        (if (fx= len 0)
                            (vmake 0)
                            (cond [(and (fx< s e) (fx> step 0))
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
                                  [else (vmake 0)])))))])


  #|doc
  Return a newly allocated vector consisting of the items of `vec` in reverse order.
  |#
  (define-vector-procedure (v fxv flv)
    (reverse vec)
    (vpcheck (vec)
             (let* ([l (vlength vec)] [res (vmake l)])
               (let loop ([i 0])
                 (if (fx= i l)
                     res
                     (begin (vset! res i (vref vec (fx- l 1 i)))
                            (loop (add1 i))))))))


  #|doc
  Reverse the items in the vector in place, then return the vector.
  |#
  (define-vector-procedure (v fxv flv)
    (reverse! vec)
    (vpcheck (vec)
             (let ([l (vlength vec)])
               (unless (fx= l 0)
                 (let ([mid (fx/ l 2)])
                   (let loop ([i 0])
                     (unless (fx= i mid)
                       (let* ([righti (fx- l 1 i)]
                              [left  (vref vec i)]
                              [right (vref vec righti)])
                         (vset! vec i right)
                         (vset! vec righti left)
                         (loop (add1 i)))))))
               vec)))


  #|doc
  Return a generic vector whose items are items from each vector zipped into a list.
  |#
  (define-vector-procedure (v fxv flv) zip
    [(vec0 vec1)
     (vpcheck (vec0 vec1)
              (vcheck-length procname vec0 vec1)
              (let* ([l (vlength vec0)]
                     ;; has to be a vector
                     [res (make-vector l)])
                (let loop ([i 0])
                  (if (fx= i l)
                      res
                      (begin (vector-set! res i (list (vref vec0 i) (vref vec1 i)))
                             (loop (add1 i)))))))]
    [(vec0 vec1 . vecs)
     (let ([vec* (cons* vec0 vec1 vecs)])
       (pcheck ([all-which? vec*])
               (apply vcheck-length procname vec0 vec1 vec*)
               (let* ([l (vlength vec0)]
                      [res (make-vector l)])
                 (let loop ([i 0])
                   (if (fx= i l)
                       res
                       (begin (vector-set! res i (map (lambda (v) (vref v i)) vec*))
                              (loop (add1 i))))))))])


  #|doc
  Return a generic vector whose items are items from each vector zipped into a vector,
  hence the `v` in the end.
  |#
  (define-vector-procedure (v fxv flv) zipv
    [(vec0 vec1)
     (vpcheck (vec0 vec1)
              (vcheck-length procname vec0 vec1)
              (let* ([l (vlength vec0)]
                     [res (make-vector l)])
                (let loop ([i 0])
                  (if (fx= i l)
                      res
                      (begin (vector-set! res i (v (vref vec0 i) (vref vec1 i)))
                             (loop (add1 i)))))))]
    [(vec0 vec1 . vecs)
     (let ([vec* (cons* vec0 vec1 vecs)])
       (pcheck ([all-which? vec*])
               (apply vcheck-length procname vec0 vec1 vec*)
               (let* ([l (vlength vec0)]
                      [res (make-vector l)])
                 (let loop ([i 0])
                   (if (fx= i l)
                       res
                       (let* ([subl (length vec*)] [item (vmake subl)])
                         (let lp ([j 0] [vs vec*])
                           (if (fx= j subl)
                               (begin (vector-set! res i item)
                                      (loop (add1 i)))
                               (begin (vset! item j (vref (car vs) i))
                                      (lp (add1 j) (cdr vs)))))))))))])


  #|doc
  Return a new vector randomly permutated from `vec`.
  |#
  (define-vector-procedure (v fxv flv)
    (shuffle vec)
    (vpcheck (vec)
             (let* ([len (vlength vec)]
                    [newv (vmake len)])
               (define swap!
                 (lambda (i j)
                   (let ([x (vref newv i)] [y (vref newv j)])
                     (vset! newv i y) (vset! newv j x))))
               (let loop ([i 0])
                 (unless (fx= i len)
                   (vset! newv i (vref vec i))
                   (loop (fx1+ i))))
               (cond [(>= len 3)
                      (let loop ([i (fx1- len)])
                        (unless (fx= i 0)
                          (swap! i (random i))
                          (loop (fx1- i))))]
                     [(= len 2) (swap! 0 1)])
               newv)))


  #|doc
  Shuffle the vector in place.
  |#
  ;; Fisher–Yates shuffle
  (define-vector-procedure (v fxv flv)
    (shuffle! vec)
    (vpcheck (vec)
             (define swap!
               (lambda (i j)
                 (let ([x (vref vec i)] [y (vref vec j)])
                   (vset! vec i y) (vset! vec j x))))
             (let ([len (vlength vec)])
               (cond [(>= len 3)
                      (let loop ([i (fx1- len)])
                        (unless (fx= i 0)
                          (swap! i (random i))
                          (loop (fx1- i))))]
                     [(= len 2) (swap! 0 1)]))
             vec))


  #|doc
  The procedure applies `pred` to each element of the  vector `vec` and
  returns a new vector of the same type consisting of the elements of `vec`
  for which `pred` returned a true value.
  |#
  (define-vector-procedure (v fxv flv)
    (filter pred vec)
    (vpcheck (vec)
             (pcheck ([procedure? pred])
                     ;; use a bitvec to record index of nice items
                     (let* ([len (vlength vec)]
                            [bitvec (make-bytevector (add1 (div len 8)) 0)])
                       (define setbit!
                         (lambda (x)
                           (let-values ([(i1 i2) (div-and-mod x 8)])
                             (let ([v (bytevector-u8-ref bitvec i1)])
                               (bytevector-u8-set! bitvec i1 (logbit1 i2 v))))))
                       (define bitset?
                         (lambda (x)
                           (let-values ([(i1 i2) (div-and-mod x 8)])
                             (let ([v (bytevector-u8-ref bitvec i1)])
                               (logbit? i2 v)))))
                       (define popcount
                         (lambda ()
                           (let loop ([i 0] [p 0])
                             (if (fx= i (bytevector-length bitvec))
                                 p
                                 (loop (fx1+ i)
                                       (+ p (fxpopcount (bytevector-u8-ref bitvec i))))))))
                       (let loop ([i 0])
                         (if (fx= i len)
                             ;; fill newvec
                             (let ([newvec (vmake (popcount))])
                               (let lp ([i 0] [j 0])
                                 (if (fx= i len)
                                     newvec
                                     (if (bitset? i)
                                         (begin
                                           (vset! newvec j (vref vec i))
                                           (lp (fx1+ i) (fx1+ j)))
                                         (lp (fx1+ i) j)))))
                             (begin
                               (when (pred (vref vec i))
                                 (setbit! i))
                               (loop (fx1+ i)))))))))


  (define-vector-procedure (v fxv flv)
    (partition pred vec)
    (vpcheck (vec)
             (pcheck ([procedure? pred])
                     (todo))))


  (define-vector-procedure (fxv flv)
    (sort <? vec)
    (vpcheck (vec)
             (pcheck ([procedure? <?])
                     (todo))))


  (define-vector-procedure (fxv flv)
    (sort! <? vec)
    (vpcheck (vec)
             (pcheck ([procedure? <?])
                     (todo))))


  #|doc
  Return the index of the first vector item such that (proc item) => #t,
  otherwise return #f.
  |#
  (define-vector-procedure (v fxv flv)
    (memp proc vec)
    (pcheck ([procedure? proc] [v? vec])
            (let ([l (vlength vec)])
              (let loop ([i 0])
                (if (fx= i l)
                    #f
                    (if (proc (vref vec i))
                        i
                        (loop (add1 i))))))))


  (define-syntax gen-vmember
    (lambda (stx)
      (syntax-case stx ()
        [(_ procname pcheck memp)
         (identifier? #'procname)
         #'(define procname
             (lambda (obj vec)
               (pcheck (vec)
                       (memp (lambda (x) (eqv? x obj)) vec))))])))
  (gen-vmember vmember   pcheck-vector   vmemp)
  (gen-vmember fxvmember pcheck-fxvector fxvmemp)
  (gen-vmember flvmember pcheck-flvector flvmemp)


  (define-syntax gen-vmemq
    (lambda (stx)
      (syntax-case stx ()
        [(_ procname pcheck memp)
         (identifier? #'procname)
         #'(define procname
             (lambda (obj vec)
               (pcheck (vec)
                       (memp (lambda (x) (eqv? x obj)) vec))))])))
  (gen-vmemq vmemq   pcheck-vector   vmemp)
  (gen-vmemq fxvmemq pcheck-fxvector fxvmemp)
  (gen-vmemq flvmemq pcheck-flvector flvmemp)


  (define-syntax gen-vmemv
    (lambda (stx)
      (syntax-case stx ()
        [(_ procname pcheck memp)
         (identifier? #'procname)
         #'(define procname
             (lambda (obj vec)
               (pcheck (vec)
                       (memp (lambda (x) (eqv? x obj)) vec))))])))
  (gen-vmemv vmemv   pcheck-vector   vmemp)
  (gen-vmemv fxvmemv pcheck-fxvector fxvmemp)
  (gen-vmemv flvmemv pcheck-flvector flvmemp)


  #|doc
  Copy items in `src` from indices src-start, ..., src-start + k - 1
  to consecutive indices in `tgt` starting at `tgt-start`.

  `src` and `tgt` must be vectors of the same type, and `tgt` must be mutable.
  `src-start`, `tgt-start`, and `k` must be exact nonnegative integers.
  The sum of `src-start` and `k` must not exceed the length of `src`,
  and the sum of `tgt-start` and `k` must not exceed the length of `tgt`.

  `src` and `tgt` may or may not be the same vector.
  |#
  (define-vector-procedure (v fxv flv)
    (copy! src src-start tgt tgt-start k)
    (pcheck ([v? src tgt] [natural? src-start tgt-start k])
            (let ([len1 (vlength src)] [len2 (vlength tgt)])
              (when (> (fx+ src-start k) len1)
                (errorf procname "range ~a is too large in source vector" k))
              (when (> (fx+ tgt-start k) len2)
                (errorf procname "range ~a is too large in target vector" k))
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
                            (vset! tgt j (vref src i))
                            (loop (fx1+ i) (fx1+ j) (fx1- k))))]
                       [(fx< src-start tgt-start)
                        ;; overlapping, left to right, copy from last to first
                        (let loop ([i (fx1- src-end)] [j (fx1- tgt-end)] [k k])
                          (unless (fx= k 0)
                            (vset! tgt j (vref src i))
                            (loop (fx1- i) (fx1- j) (fx1- k))))]
                       [else (assert-unreachable)]))
                    (let loop ([i src-start] [j tgt-start] [k k])
                      (unless (fx= k 0)
                        (vset! tgt j (vref src i))
                        (loop (fx1+ i) (fx1+ j) (fx1- k)))))))))


  #|doc
  Similar to `iota`, generate a vector of the corresponding type
  consisting of integers from 0 to n-1.
  |#
  (define-vector-procedure (v fxv)
    (iota n)
    (pcheck ([natural? n])
            (let ([v (vmake n)])
              (let loop ([i 0])
                (if (fx= i n)
                    v
                    (begin (vset! v i i)
                           (loop (fx1+ i))))))))


  #|doc
  Generate a vector of the corresponding type consisting of integers
  start, start+step*1, start+step*2, ...

  `start`, `stop` and `step` must be integers that meet the following requirements:
  If `start` is less than `stop`, then `step` must be greater than 0,
  in which case the sequence terminates when the value is greater than or equal to `stop`;
  If `start` is greater than `stop`, then `step` must be less than 0,
  in which case the sequence terminates when the value is less than or equal to `stop`.
  |#
  (define-vector-procedure (v fxv) nums
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
                         vec
                         (begin (vset! vec i x)
                                (loop (fx1+ i) (+ x step))))))
                 (errorf procname "invalid range: ~a, ~a, ~a" start stop step)))])


  #|doc
  Generate a flvector consisting of flonums
  start, start+step*1, start+step*2, ...

  `start`, `stop` and `step` must be flonums that meet the following requirements:
  If `start` is less than `stop`, then `step` must be greater than 0,
  in which case the sequence terminates when the value is greater than or equal to `stop`;
  If `start` is greater than `stop`, then `step` must be less than 0,
  in which case the sequence terminates when the value is less than or equal to `stop`.
  |#
  (define-vector-procedure (flv) nums
    [(stop) (thisproc 0 stop 1)]
    [(start stop) (thisproc start stop 1)]
    [(start stop step)
     (pcheck ([flonum? start stop step])
             (if (or (and (<= start stop) (> step 0.0))
                     (and (>= start stop) (< step 0.0)))
                 (let* ([len (flonum->fixnum (ceiling (/ (- stop start) step)))]
                        [vec (vmake len 0.0)])
                   (let loop ([i 0] [x start])
                     (if (fx= i len)
                         vec
                         (begin (vset! vec i x)
                                (loop (fx1+ i) (+ x step))))))
                 (errorf procname "invalid range: ~a, ~a, ~a" start stop step)))])


  ;; aliases
  (define vmap    vector-map)
  (define vmap/i  vector-map/i)
  (define vmap!   vector-map!)
  (define vmap!/i vector-map!/i)
  (define vsort   vector-sort)
  (define vsort!  vector-sort!)
  (define vfor-each   vector-for-each)
  (define vfor-each/i vector-for-each/i)

  (define fxvmap    fxvector-map)
  (define fxvmap/i  fxvector-map/i)
  (define fxvmap!   fxvector-map!)
  (define fxvmap!/i fxvector-map!/i)
  ;; (define vsort   vector-sort)
  ;; (define vsort!  vector-sort!)
  (define fxvfor-each   fxvector-for-each)
  (define fxvfor-each/i fxvector-for-each/i)

  (define flvmap    flvector-map)
  (define flvmap/i  flvector-map/i)
  (define flvmap!   flvector-map!)
  (define flvmap!/i flvector-map!/i)
  ;; (define vsort   vector-sort)
  ;; (define vsort!  vector-sort!)
  (define flvfor-each   flvector-for-each)
  (define flvfor-each/i flvector-for-each/i)

  (define vcopy   vector-copy)
  (define fxvcopy fxvector-copy)
  (define flvcopy flvector-copy)

  (define vector-copy!   vcopy!)
  (define fxvector-copy! fxvcopy!)
  (define flvector-copy! flvcopy!)
  (define u8vcopy!       bytevector-copy!)

  (define vfor-all   vandmap)
  (define fxvfor-all fxvandmap)
  (define flvfor-all flvandmap)

  (define vexists   vormap)
  (define fxvexists fxvormap)
  (define flvexists flvormap)


;;;; arithmetics

  ;; TODO optimize for loop and use that

  (define-vector-procedure (v fxv flv)
    (sum vec)
    (vpcheck (vec)
             (let ([l (vlength vec)])
               (let loop ([i 0] [acc t+id])
                 (if (fx= i l)
                     acc
                     (loop (add1 i) (t+ acc (vref vec i))))))))


  (define-vector-procedure (v fxv flv)
    (product vec)
    (vpcheck (vec)
             (let ([l (vlength vec)])
               (let loop ([i 0] [acc t*id])
                 (if (fx= i l)
                     acc
                     (loop (add1 i) (t* acc (vref vec i))))))))


  (define-vector-procedure (v fxv flv)
    (avg vec)
    (vpcheck (vec)
             (let ([l (vlength vec)])
               (let loop ([i 0] [acc t+id])
                 (if (fx= i l)
                     (/ acc i)
                     (loop (add1 i) (t+ acc (vref vec i))))))))


  #|doc
  Return the extreme value in a vector, using `f` as the comparison function.
  |#
  (define-vector-procedure (v fxv flv)
    (extreme f vec)
    (vpcheck (vec)
             (let ([l (vlength vec)])
               (if (fx= l 0)
                   #f
                   (let loop ([i 1] [res (vref vec 0)])
                     (if (fx= i l)
                         res
                         (let ([x (vref vec i)])
                           (loop (add1 i) (if (f x res) x res)))))))))


  (define vmax   (lambda (vec) (vextreme   >   vec)))
  (define fxvmax (lambda (vec) (fxvextreme fx> vec)))
  (define flvmax (lambda (vec) (flvextreme fl> vec)))
  (define vmin   (lambda (vec) (vextreme   <   vec)))
  (define fxvmin (lambda (vec) (fxvextreme fx< vec)))
  (define flvmin (lambda (vec) (flvextreme fl< vec)))


  )
