(library (chezpp for)
  (export for/fold  for  for/list  for/vector
          for*/fold for* for*/list for*/vector)
  (import (chezpp chez)
          (chezpp private for)
          (chezpp internal)
          (chezpp iter)
          (chezpp list)
          (chezpp vector)
          (chezpp utils)
          (chezpp io))



  (define-syntax for/fold
    (lambda (stx)
      ;; this differs from that of for*
      (define handler
        (make-clause-handler
         ;; order matters
         ;; state0 state1 action
         `(start init ,handle-init)
         `(start e0   epsilon)

         `(init index ,handle-index)
         `(init e1    epsilon)

         `(e0   index ,handle-index)
         `(e0   e1    epsilon)

         `(index finish ,handle-finish)
         `(index e2     epsilon)

         `(e1    finish ,handle-finish)
         `(e1    e2     epsilon)

         `(finish iter0 ,handle-iter)
         `(e2     iter0 ,handle-iter)

         `(iter0 iter ,handle-iter)
         `(iter0 e3   epsilon)

         `(iter iter   ,handle-iter)
         `(iter config ,handle-config)
         `(iter e4     epsilon)

         `(e3  config  ,handle-config)
         `(e3  e4      epsilon)

         `(e4  end     epsilon)

         `(config config ,handle-config)
         `(config end    epsilon)))
      (syntax-case stx ()
        [(_ (cl* ...) body* ...)
         (let* ([fragments (handler (make-eq-hashtable) #'(cl* ...))]
                [frag-init   (hashtable-ref fragments 'init #f)]
                [frag-index  (hashtable-ref fragments 'index #f)]
                [frag-finish (hashtable-ref fragments 'finish #f)]
                [frag-config (hashtable-ref fragments 'config #f)])
           (with-syntax ([(for-loop t-acc t-finish) (generate-temporaries '(for-loop t-acc t-finish))])
             (let* ([iter-frags (process-iter-clauses (hashtable-ref fragments 'iter #f))]
                    [init-def (if frag-init
                                  (syntax-case frag-init () [(_ v e) #'[v e]])
                                  #f)]
                    [index-def (if frag-index
                                   (syntax-case frag-index () [(_ v) #'[v 0]])
                                   #f)]
                    [init-var (if frag-init
                                  (syntax-case frag-init () [(_ v e) #'v])
                                  #f)]
                    [index-var (if frag-index
                                   (syntax-case frag-index () [(_ v) #'v])
                                   #f)]
                    [gen-preloops (fold-left (lambda (res x*)
                                               (if (list-ref x* 0)
                                                   (cons (list-ref x* 0) res)
                                                   res))
                                             '() iter-frags)]
                    [loop-vars (let ([res (map (lambda (x*) (list-ref x* 1)) iter-frags)])
                                 ;; order matters
                                 (when frag-index (set! res (cons index-def res)))
                                 (when frag-init  (set! res (cons init-def  res)))
                                 res)]
                    [tychecks  (map (lambda (x*) (list-ref x* 2)) iter-frags)]
                    [termination-checks (map (lambda (x*) (list-ref x* 3)) iter-frags)]
                    [updates (let ([res (map (lambda (x*) (list-ref x* 4)) iter-frags)])
                               (when frag-index
                                 (set! res (cons (syntax-case frag-index ()
                                                   [(_ v) #'(fx1+ v)])
                                                 res)))
                               (when frag-init
                                 (set! res (cons #'t-acc res)))
                               res)]
                    [gen-getters (map (lambda (x*) (list-ref x* 5)) iter-frags)]
                    [call/finish-proc (if frag-finish
                                          (let ([params '()])
                                            (when frag-index
                                              (set! params (cons (syntax-case frag-index ()
                                                                   [(_ v) #'v])
                                                                 params)))
                                            (when frag-init
                                              (set! params (cons (syntax-case frag-init ()
                                                                   [(_ v _) #'v])
                                                                 params)))
                                            ;; code to call `t-finish` and its def
                                            (list #`(t-finish #,@params)
                                                  #`(lambda #,params
                                                      #,(syntax-case frag-finish ()
                                                          [(_ e) #'e]))))
                                          (list #'(t-finish)
                                                #'(lambda () (void))))])
               (unless (= (length loop-vars) (length updates))
                 (syntax-error stx "loop vars and recur call args do not match:"))
               (println "for -------------------------")
               (printf "gen-preloops: ~a~n" gen-preloops)
               (printf "loop-vars:    ~a~n" loop-vars)
               (printf "tychecks:     ~a~n" tychecks)
               (printf "termination-checks: ~a~n" termination-checks)
               (printf "updates:      ~a~n" updates)
               (printf "gen-getters:  ~a~n" gen-getters)
               (printf "call/finish-proc: ~a~n" call/finish-proc)
               (printf "frag-config:  ~a~n" frag-config)
               (println "for -------------------------")
               #`(let ([t-finish #,(cadr call/finish-proc)])
                   #,(let lp-preloops ([gen-preloops gen-preloops])
                       (if (null? gen-preloops)
                           #`(let for-loop (#,@loop-vars)
                               #,(let lp-checks ([termination-checks termination-checks])
                                   (if (null? termination-checks)
                                       (let lp-getters ([gen-getters gen-getters])
                                         (if (null? gen-getters)
                                             (let lp-configs ([configs (if frag-config frag-config '())])
                                               (if (null? configs)
                                                   (if frag-init
                                                       #`(let ([t-acc (begin body* ...)])
                                                           (for-loop #,@updates))
                                                       #`(begin body* ...
                                                                (for-loop #,@updates)))
                                                   (let ([conf (car configs)])
                                                     (syntax-case conf ()
                                                       [(:break e)
                                                        (kw:break? #':break)
                                                        #`(if e
                                                              (void)
                                                              #,(lp-configs (cdr configs)))]
                                                       [(:break e1 e2)
                                                        (kw:break? #':break)
                                                        #`(if e1
                                                              e2
                                                              #,(lp-configs (cdr configs)))]
                                                       [(:guard e)
                                                        (kw:guard? #':guard)
                                                        #`(if e
                                                              #,(lp-configs (cdr configs))
                                                              #,(if frag-init
                                                                    #`(let ([t-acc #,(syntax-case frag-init ()
                                                                                       [(_ v e) #'v])])
                                                                        (for-loop #,@updates))
                                                                    #`(for-loop #,@updates)))]
                                                       [(:let v e)
                                                        (kw:let? #':let)
                                                        #`(let ([v e])
                                                            #,(lp-configs (cdr configs)))]
                                                       [(:let-values (v* ...) e)
                                                        (kw:let-values? #':let-values)
                                                        #`(let-values ([(v* ...) e])
                                                            #,(lp-configs (cdr configs)))]
                                                       [_ (syntax-error conf "unknown config clause:")]))))
                                             (if (car gen-getters)
                                                 ((car gen-getters) (lp-getters (cdr gen-getters)))
                                                 (lp-getters (cdr gen-getters)))))
                                       #`(if #,(car termination-checks)
                                             #,(car call/finish-proc)
                                             #,(lp-checks (cdr termination-checks))))))
                           (if (car gen-preloops)
                               ((car gen-preloops) (lp-preloops (cdr gen-preloops)))
                               (lp-preloops (cdr gen-preloops)))))))))])))



  (define-syntax for*/fold
    (lambda (stx)
      (define handler
        (make-clause-handler
         `(start init ,handle-init)
         `(start e0   epsilon)

         `(init index ,handle-index)
         `(init e1    epsilon)

         `(e0   index ,handle-index)
         `(e0   e1    epsilon)

         `(index finish ,handle-finish)
         `(index e2     epsilon)

         `(e1    finish ,handle-finish)
         `(e1    e2     epsilon)

         `(finish iter0 ,handle-iter/config-iter)
         `(e2     iter0 ,handle-iter/config-iter)

         `(iter0  config0 ,handle-iter/config-config)
         `(iter0  e3      epsilon)

         `(config0 config0 ,handle-iter/config-config)
         `(config0 e4     epsilon)

         `(e3 e4 epsilon)

         `(e4 iter ,handle-iter/config-iter)
         `(e4 e5   epsilon)

         `(iter config ,handle-iter/config-config)
         `(iter e5     epsilon)

         `(config config ,handle-iter/config-config)
         `(config iter   ,handle-iter/config-iter)
         `(config end    epsilon)

         `(e5 iter ,handle-iter/config-iter)
         `(e5 end  epsilon)))
      (syntax-case stx ()
        [(_ (cl* ...) body* ...)
         (let* ([fragments (handler (make-eq-hashtable) #'(cl* ...))]
                [frag-init        (hashtable-ref fragments 'init #f)]
                [frag-index       (hashtable-ref fragments 'index #f)]
                [frag-finish      (hashtable-ref fragments 'finish #f)]
                [frag-iter/config (hashtable-ref fragments 'iter/config #f)])
           (println "for*/fold fragments: ~a" (hashtable-cells fragments))
           (with-syntax ([(t-acc t-finish) (generate-temporaries '(t-acc t-finish))])
             (let* ([init-def (if frag-init
                                  (syntax-case frag-init () [(_ v e) #'[v e]])
                                  #f)]
                    [index-def (if frag-index
                                   (syntax-case frag-index () [(_ v) #'[v 0]])
                                   #f)]
                    [init-var (if frag-init
                                  (syntax-case frag-init () [(_ v e) #'v])
                                  #f)]
                    [index-var (if frag-index
                                   (syntax-case frag-index () [(_ v) #'v])
                                   #f)]
                    [init/index-vars (let* ([vars (if frag-index (list index-var)    '())]
                                            [vars (if frag-init  (cons init-var vars) vars)])
                                       vars)]
                    ;; (((gen-preloop loop-var tycheck term-check update gen-getter) config0 config1 ...) ...)
                    [iters/configs (map (lambda (i/c)
                                          (cons (process-iter-clause (car i/c))
                                                (cdr i/c)))
                                        frag-iter/config)]
                    [gen-preloops (map caar iters/configs)]
                    [call/finish-proc (if frag-finish
                                          (let ([params '()])
                                            (when frag-index
                                              (set! params (cons (syntax-case frag-index ()
                                                                   [(_ v) #'v])
                                                                 params)))
                                            (when frag-init
                                              (set! params (cons (syntax-case frag-init ()
                                                                   [(_ v _) #'v])
                                                                 params)))
                                            ;; code to call `t-finish` and its def
                                            (list #`(t-finish #,@params)
                                                  #`(lambda #,params
                                                      #,(syntax-case frag-finish ()
                                                          [(_ e) #'e]))))
                                          (list #'(t-finish)
                                                #'(lambda () (void))))])
               (println "for* -------------------------")
               (for-each displayln iters/configs)
               (printf "index-def: ~a~n" index-def)
               (printf "index-var: ~a~n" index-var)
               (println "for* -------------------------")
               #`(let ([t-finish #,(cadr call/finish-proc)])
                   #,(let lp-preloops ([gen-preloops gen-preloops])
                       (if (null? gen-preloops)
                           (let lp-for-loop ([i/c* iters/configs] [last-forloop #f] [last-update #f])
                             (let ([call-lastforloop #`(#,last-forloop #,@init/index-vars #,last-update)])
                               (if (null? i/c*)
                                   (let ([call-loop (lambda (init?)
                                                      (if frag-index
                                                          #`(#,last-forloop #,@init? (fx1+ #,index-var) #,last-update)
                                                          #`(#,last-forloop #,@init?                    #,last-update)))])
                                     (if frag-init
                                         #`(let ([t-acc (begin body* ...)])
                                             #,(call-loop (list #'t-acc)))
                                         #`(begin body* ...
                                                  #,(call-loop '()))))
                                   (with-syntax ([(forloop) (generate-temporaries '(forloop))])
                                     (let* ([i/c (car i/c*)] [iter-cl (car i/c)] [configs (cdr i/c)]
                                            [loop-var          (list-ref iter-cl 1)]
                                            [termination-check (list-ref iter-cl 3)]
                                            [update            (list-ref iter-cl 4)]
                                            [gen-getter        (list-ref iter-cl 5)]
                                            [loop-var
                                             (let ([vars (if last-forloop
                                                             (let* ([vars (if frag-index (list #`[#,index-var #,index-var])     '())]
                                                                    [vars (if frag-init  (cons #`[#,init-var  #,init-var] vars) vars)])
                                                               vars)
                                                             (let* ([vars (if frag-index (list index-def)     '())]
                                                                    [vars (if frag-init  (cons init-def vars) vars)])
                                                               vars))])
                                               `(,@vars ,loop-var))])
                                       ;; if current level terminates, call forloop of last level
                                       #`(let forloop (#,@loop-var)
                                           (if #,termination-check
                                               ;; continue on the outer loop
                                               #,(if last-forloop
                                                     call-lastforloop
                                                     ;; already outermost loop, stop
                                                     (car call/finish-proc))
                                               #,(let ([gen-getter (lambda (e) (if gen-getter (gen-getter e) e))])
                                                   (gen-getter
                                                    (let lp-configs ([configs (if configs configs '())])
                                                      (if (null? configs)
                                                          (lp-for-loop (cdr i/c*) #'forloop update)
                                                          (let ([conf (car configs)])
                                                            (syntax-case conf ()
                                                              [(:break e)
                                                               (kw:break? #':break)
                                                               #`(if e
                                                                     #,(if last-forloop
                                                                           call-lastforloop
                                                                           ;; already outermost loop, stop
                                                                           #`(void))
                                                                     #,(lp-configs (cdr configs)))]
                                                              [(:stop e1 e2)
                                                               (kw:stop? #':stop)
                                                               #`(if e1
                                                                     e2
                                                                     #,(lp-configs (cdr configs)))]
                                                              [(:guard e)
                                                               (kw:guard? #':guard)
                                                               #`(if e
                                                                     #,(lp-configs (cdr configs))
                                                                     (forloop #,@init/index-vars #,update))]
                                                              [(:let v e)
                                                               (kw:let? #':let)
                                                               #`(let ([v e])
                                                                   #,(lp-configs (cdr configs)))]
                                                              [(:let-values (v* ...) e)
                                                               (kw:let-values? #':let-values)
                                                               #`(let-values ([(v* ...) e])
                                                                   #,(lp-configs (cdr configs)))]
                                                              [_ (syntax-error conf "unknown config clause:")])))))))))))))
                           (if (car gen-preloops)
                               ((car gen-preloops) (lp-preloops (cdr gen-preloops)))
                               (lp-preloops (cdr gen-preloops)))))))))])))



  #|
  (<for> (<index-clause>?
          <iter-clause>+
          <for-config-clause>*)
    <body>+)

  (<for*> (<index-clause>?
           <for*-clause>+)
    <body>+)
  |#
  (define-syntax for
    (lambda (stx)
      (define handler
        (make-clause-handler
         `(start index ,handle-index)
         `(start e0    epsilon)

         `(index iter0 ,handle-iter)
         `(e0    iter0 ,handle-iter)

         `(iter0 iter ,handle-iter)
         `(iter0 e1   epsilon)

         `(iter iter   ,handle-iter)
         `(iter config ,handle-config)
         `(iter e2     epsilon)

         `(e1 config ,handle-config)
         `(e1 e2     epsilon)

         `(config config ,handle-config)
         `(config end    epsilon)

         `(e2 end epsilon)))
      (syntax-case stx ()
        [(_ (cl* ...) body* ...)
         (let ([_ (handler (make-eq-hashtable) #'(cl* ...))])
           #'(for/fold (cl* ...) body* ...))])))


  (define-syntax for/list
    (lambda (stx)
      (syntax-case stx ()
        [(_ (cl* ...) body* ...)
         #'(let ([lb (make-list-builder)])
             (for (cl* ...)
               (lb (begin body* ...)))
             (lb))])))


  (define-syntax for/vector
    (lambda (stx)
      (define (classify-clauses cl*)
        (trace-define (handle-index-clause cl* res)
          (let loop ([cl* cl*])
            (if (null? cl*)
                res
                (let ([cl (car cl*)])
                  (syntax-case cl ()
                    [(:index v)
                     (kw:index? #':index)
                     (begin (displayln cl)
                            (set-cdr! (assoc 'index res) cl)
                            (handle-length-clause (cdr cl*) res))]
                    [_ (handle-length-clause cl* res)])))))
        (trace-define (handle-length-clause cl* res)
          (let loop ([cl* cl*])
            (if (null? cl*)
                res
                (let ([cl (car cl*)])
                  (syntax-case cl ()
                    [(:length v)
                     (kw:length? #':length)
                     (begin (displayln cl)
                            (set-cdr! (assoc 'length res) cl)
                            (handle-fill-clause (cdr cl*) res))]
                    [_ (handle-fill-clause cl* res)])))))
        (trace-define (handle-fill-clause cl* res)
          (let loop ([cl* cl*])
            (if (null? cl*)
                res
                (let ([cl (car cl*)])
                  (syntax-case cl ()
                    [(:fill v)
                     (kw:fill? #':fill)
                     (begin (displayln cl)
                            (set-cdr! (assoc 'fill res) cl)
                            (handle-iter-clauses (cdr cl*) res))]
                    [_ (handle-iter-clauses cl* res)])))))
        (trace-define (handle-iter-clauses cl* res)
          (define (add-clause cl)
            (let ([cls (assoc 'iter res)])
              (if (cdr cls)
                  (snoc! (cdr cls) cl)
                  (set-cdr! cls (list cl)))))
          (let loop ([cl* cl*])
            (if (null? cl*)
                res
                (let ([cl (car cl*)])
                  (syntax-case cl ()
                    [((v* ...) iter-ty op* ...)
                     (and (andmap identifier? #'(v* ...))
                          (valid-iter-ty? #'iter-ty))
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(v iter-ty op* ...)
                     (and (identifier? #'v)
                          (valid-iter-ty? #'iter-ty))
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(v lit)
                     (and (identifier? #'v) (literal? #'lit))
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(x . rest)
                     (kw-for-config-clause? #'x)
                     (handle-config-clauses cl* res)]
                    [_ (syntax-error cl "unknown clause:")])))))
        (trace-define (handle-config-clauses cl* res)
          (define (add-clause cl)
            (let ([cls (assoc 'config res)])
              (if (cdr cls)
                  (snoc! (cdr cls) cl)
                  (set-cdr! cls (list cl)))))
          (let loop ([cl* cl*])
            (if (null? cl*)
                res
                (let ([cl (car cl*)])
                  (syntax-case cl ()
                    [(:break e)
                     (kw:break? #':break)
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(:break e1 e2)
                     (kw:break? #':break)
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(:guard e)
                     (kw:guard? #':guard)
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(:let v e)
                     (and (kw:let? #':let) (identifier? #'v))
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(:let-values (v* ...) e)
                     (and (kw:let-values? #':let-values)
                          (andmap identifier? #'(v* ...)))
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [_ (syntax-error cl "unknown clause:")])))))
        (handle-index-clause cl* (list (cons 'index  #f)
                                       (cons 'iter   #f)
                                       (cons 'config #f)
                                       (cons 'length #f)
                                       (cons 'fill   #f))))
      (syntax-case stx ()
        [(_ (cl* ...) body* ...)
         (let* ([fragments  (classify-clauses #'(cl* ...))]
                [frag-len   (cdr (assoc 'length fragments))]
                [frag-index (cdr (assoc 'index  fragments))]
                [frag-fill  (cdr (assoc 'fill   fragments))]
                ;; feed the clauses w/o for/vector extensions
                [cl* (filter id `(,(cdr (assoc 'index  fragments))
                                  ;; TODO what if there's no iter clauses?
                                  ,@(cdr (assoc 'iter  fragments))
                                  ,(cdr (assoc 'config fragments))))])
           (println "len: ~a~ncl* ~a" frag-len cl*)
           (if frag-len
               #`(let ([len #,(syntax-case frag-len () [(_ l) #'l])])
                   (unless (and (fixnum? len) (nonnegative? len))
                     (errorf 'for/vector "length is not a nonnegative fixnum: ~a" len))
                   (let ([vec (make-vector len
                                           #,(if frag-fill
                                                 ;; TODO lift `v` first
                                                 (syntax-case frag-fill () [(_ v) #'v])
                                                 #'#f))])
                     ;; need index to index the vec
                     ;; if :index is given, use it; otherwise, we add one
                     #,(if frag-index
                           (let ([i (syntax-case frag-index () [(_ i) #'i])])
                             #`(for ( #,@cl*
                                      [:break (fx= #,i len)])
                                 (vector-set! vec #,i (begin body* ...))))
                           #`(for ([:index i]
                                   #,@cl*
                                   [:break (fx= i len)])
                               (vector-set! vec i (begin body* ...))))
                     vec))
               #`(let ([lb (make-list-builder)])
                   (for (#,@cl*)
                     (lb (begin body* ...)))
                   (list->vector (lb)))))])))


  (define-syntax for*
    (lambda (stx)
      ;; just check clauses
      (define (classify-clauses cl*)
        (trace-define (handle-index-clause cl* res)
          (let loop ([cl* cl*])
            (if (null? cl*)
                res
                (let ([cl (car cl*)])
                  (syntax-case cl ()
                    [(:index v)
                     (kw:index? #':index)
                     (begin (set-cdr! (assoc 'index res) cl)
                            (handle-iter-clauses (cdr cl*) res))]
                    [_ (handle-iter-clauses cl* res)])))))
        ;; iter/config: ((<iter> <config> ...) ...)
        (trace-define (handle-iter-clauses cl* res)
          (define (add-clause cl)
            (let ([cls (assoc 'iter/config res)])
              (if (cdr cls)
                  (snoc! (cdr cls) (list cl))
                  (set-cdr! cls (list (list cl))))))
          (let loop ([cl* cl*])
            (if (null? cl*)
                res
                (let ([cl (car cl*)])
                  (syntax-case cl ()
                    [((v* ...) iter-ty op* ...)
                     (and (andmap identifier? #'(v* ...))
                          (valid-iter-ty? #'iter-ty))
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(v iter-ty op* ...)
                     (and (identifier? #'v)
                          (valid-iter-ty? #'iter-ty))
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(v lit)
                     (and (identifier? #'v) (literal? #'lit))
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(x . rest)
                     (kw-for*-config-clause? #'x)
                     (handle-config-clauses cl* res)]
                    [_ (syntax-error cl "handle-iter-clauses: unknown clause:")])))))
        ;; iter/config: ((<iter> <config> ...) ...)
        (trace-define (handle-config-clauses cl* res)
          (define (add-clause cl)
            ;; `cls` must have at least one item
            (let ([cls (assoc 'iter/config res)])
              (if (cdr cls)
                  (snoc! (list-last (cdr cls)) cl)
                  (syntax-error cl* "bad iter/config clause parsing:"))))
          (let loop ([cl* cl*])
            (if (null? cl*)
                res
                (let ([cl (car cl*)])
                  (syntax-case cl ()
                    [(:break e)
                     (kw:break? #':break)
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(:stop e1 e2)
                     (kw:stop? #':stop)
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(:guard e)
                     (kw:guard? #':guard)
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(:let v e)
                     (and (kw:let? #':let) (identifier? #'v))
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(:let-values (v* ...) e)
                     (and (kw:let-values? #':let-values)
                          (andmap identifier? #'(v* ...)))
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(x . rest)
                     (kw-iter-clause? #'x)
                     (handle-iter-clauses cl* res)]
                    [(v lit)
                     (and (identifier? #'v) (literal? #'lit))
                     (handle-iter-clauses cl* res)]
                    [_ (syntax-error cl "handle-config-clauses: unknown clause:")])))))
        (handle-index-clause cl* (list (cons 'init   #f)
                                       (cons 'index  #f)
                                       (cons 'finish #f)
                                       (cons 'iter/config #f))))
      (syntax-case stx ()
        [(_ (cl* ...) body* ...)
         (let ([_ (classify-clauses #'(cl* ...))])
           #'(for*/fold (cl* ...) body* ...))])))


  (define-syntax for*/list
    (lambda (stx)
      (syntax-case stx ()
        [(_ (cl* ...) body* ...)
         #'(let ([lb (make-list-builder)])
             (for* (cl* ...)
               (lb (begin body* ...)))
             (lb))])))

  (define-syntax for*/vector
    (lambda (stx)
      (define (classify-clauses cl*)
        (trace-define (handle-index-clause cl* res)
          (let loop ([cl* cl*])
            (if (null? cl*)
                res
                (let ([cl (car cl*)])
                  (syntax-case cl ()
                    [(:index v)
                     (kw:index? #':index)
                     (begin (displayln cl)
                            (set-cdr! (assoc 'index res) cl)
                            (handle-length-clause (cdr cl*) res))]
                    [_ (handle-length-clause cl* res)])))))
        (trace-define (handle-length-clause cl* res)
          (let loop ([cl* cl*])
            (if (null? cl*)
                res
                (let ([cl (car cl*)])
                  (syntax-case cl ()
                    [(:length v)
                     (kw:length? #':length)
                     (begin (displayln cl)
                            (set-cdr! (assoc 'length res) cl)
                            (handle-fill-clause (cdr cl*) res))]
                    [_ (handle-fill-clause cl* res)])))))
        (trace-define (handle-fill-clause cl* res)
          (let loop ([cl* cl*])
            (if (null? cl*)
                res
                (let ([cl (car cl*)])
                  (syntax-case cl ()
                    [(:fill v)
                     (kw:fill? #':fill)
                     (begin (displayln cl)
                            (set-cdr! (assoc 'fill res) cl)
                            (handle-iter-clauses (cdr cl*) res))]
                    [_ (handle-iter-clauses cl* res)])))))
        ;; iter/config: ((<iter> <config> ...) ...)
        (trace-define (handle-iter-clauses cl* res)
          (define (add-clause cl)
            (let ([cls (assoc 'iter/config res)])
              (if (cdr cls)
                  (snoc! (cdr cls) (list cl))
                  (set-cdr! cls (list (list cl))))))
          (let loop ([cl* cl*])
            (if (null? cl*)
                res
                (let ([cl (car cl*)])
                  (syntax-case cl ()
                    [((v* ...) iter-ty op* ...)
                     (and (andmap identifier? #'(v* ...))
                          (valid-iter-ty? #'iter-ty))
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(v iter-ty op* ...)
                     (and (identifier? #'v)
                          (valid-iter-ty? #'iter-ty))
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(v lit)
                     (and (identifier? #'v) (literal? #'lit))
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(x . rest)
                     (kw-for*-config-clause? #'x)
                     (handle-config-clauses cl* res)]
                    [_ (syntax-error cl "handle-iter-clauses: unknown clause:")])))))
        ;; iter/config: ((<iter> <config> ...) ...)
        (trace-define (handle-config-clauses cl* res)
          (define (add-clause cl)
            ;; `cls` must have at least one item
            (let ([cls (assoc 'iter/config res)])
              (if (cdr cls)
                  (snoc! (list-last (cdr cls)) cl)
                  (syntax-error cl* "bad iter/config clause parsing:"))))
          (let loop ([cl* cl*])
            (if (null? cl*)
                res
                (let ([cl (car cl*)])
                  (syntax-case cl ()
                    [(:break e)
                     (kw:break? #':break)
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(:stop e1 e2)
                     (kw:stop? #':stop)
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(:guard e)
                     (kw:guard? #':guard)
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(:let v e)
                     (and (kw:let? #':let) (identifier? #'v))
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(:let-values (v* ...) e)
                     (and (kw:let-values? #':let-values)
                          (andmap identifier? #'(v* ...)))
                     (begin (add-clause cl)
                            (loop (cdr cl*)))]
                    [(x . rest)
                     (kw-iter-clause? #'x)
                     (handle-iter-clauses cl* res)]
                    [(v lit)
                     (and (identifier? #'v) (literal? #'lit))
                     (handle-iter-clauses cl* res)]
                    [_ (syntax-error cl "handle-config-clauses: unknown clause:")])))))
        ;; TODO some should use '() than #f
        (handle-index-clause cl* (list (cons 'index  #f)
                                       (cons 'iter/config #f)
                                       (cons 'length #f)
                                       (cons 'fill   #f))))
      (syntax-case stx ()
        [(_ (cl* ...) body* ...)
         (let* ([fragments  (classify-clauses #'(cl* ...))]
                [frag-len   (cdr (assoc 'length fragments))]
                [frag-index (cdr (assoc 'index  fragments))]
                [frag-fill  (cdr (assoc 'fill   fragments))]
                ;; feed the clauses w/o for/vector extensions
                [cl* (filter id `(,(cdr (assoc 'index  fragments))
                                  ;; TODO what if there's no iter clauses?
                                  ,@(apply append (cdr (assoc 'iter/config  fragments)))))])
           (println "len: ~a~ncl* ~a" frag-len cl*)
           (if frag-len
               #`(let ([len #,(syntax-case frag-len () [(_ l) #'l])])
                   (unless (and (fixnum? len) (nonnegative? len))
                     (errorf 'for*/vector "length is not a nonnegative fixnum: ~a" len))
                   (let ([vec (make-vector len
                                           #,(if frag-fill
                                                 ;; TODO lift `v` first
                                                 (syntax-case frag-fill () [(_ v) #'v])
                                                 #'#f))])
                     ;; need index to index the vec
                     ;; if :index is given, use it; otherwise, we add one
                     #,(if frag-index
                           (let ([i (syntax-case frag-index () [(_ i) #'i])])
                             #`(for* ( #,@cl*
                                       [:break (fx= #,i len)])
                                 (vector-set! vec #,i (begin body* ...))))
                           #`(for* ([:index i]
                                    #,@cl*
                                    [:break (fx= i len)])
                               (vector-set! vec i (begin body* ...))))
                     vec))
               #`(let ([lb (make-list-builder)])
                   (for* (#,@cl*)
                     (lb (begin body* ...)))
                   (list->vector (lb)))))])))

  )
