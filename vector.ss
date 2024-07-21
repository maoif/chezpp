(library (chezpp vector)
  (export vmap vmap/i vmap! vmap!/i vfor-each vfor-each/i
          fxvmap fxvmap/i fxvmap! fxvmap!/i fxvfor-each fxvfor-each/i
          flvmap flvmap/i flvmap! flvmap!/i flvfor-each flvfor-each/i

          vector-map/i vector-for-each/i vector-map! vector-map!/i
          fxvector-map/i fxvector-for-each/i fxvector-map! fxvector-map!/i
          flvector-map/i flvector-for-each/i flvector-map! flvector-map!/i

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

          vcopy fxvcopy flvcopy
          vsum fxvsum flvsum
          vextreme fxvextreme flvextreme
          vmax vmin fxvmax fxvmin flvmax flvmin
          vavg fxvavg flvavg

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
           (with-implicit (k v v? vmake vref vset! vlength vpcheck vcheck-length all-which? procname
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
                                   [args body body* ...] ...)))))
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
                                   [args body body* ...] ...)))))
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
                                   [args body body* ...] ...)))))
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


  (define-vector-procedure (v fxv flv) slice
    [(vec) (vpcheck (vec) vec)]
    [(vec end) (vpcheck (vec) (procname vec 0 end 1))]
    [(vec start end) (vpcheck (vec) (procname vec start end 1))]
    [(vec start end step)
     (todo 'procname)])


  (define-vector-procedure (v fxv flv)
    (reverse vec)
    (vpcheck (vec)
             (let* ([l (vlength vec)] [res (vmake l)])
               (let loop ([i 0])
                 (if (fx= i l)
                     res
                     (begin (vset! res i (vref vec (fx- l 1 i)))
                            (loop (add1 i))))))))


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
                         (loop (add1 i))))))))))


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
             (todo)))


  #|doc
  Shuffle the vector in place.
  |#
  (define-vector-procedure (v fxv flv)
    (shuffle! vec)
    (vpcheck (vec)
             (todo)))


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
