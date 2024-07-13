(library (chezpp private comprehension)
  (export $generate-for/reduce
          $generate-for*/reduce)
  (import (chezscheme)
          (chezpp iter))

  (define $generate-for/reduce
    (lambda (acc-e* ?for-index cl* brk ?fini e)
      (define generate-clause-loop
        (lambda (iter binding ?index guard-e)
          (with-syntax ([(cloop dummy) (generate-temporaries '(cloop  dummy))])
            ;; return:
            ;; - bindings, e.g, (i), (i idx) ...
            ;; - index var for clauses with (index i)
            ;; - [bindings rhs] ...
            (if ?index
                ;; `binding` and `?index` are used both in this loop and outside
                ;; reuse `?index` so that the clause index var is visible in the guard
                (list #`(#,@binding #,?index)
                      #`[#,?index -1]
                      #`(let cloop ()
                          (let-values ([#,binding (iter-next! #,iter)])
                            (incr! #,?index)
                            (if (or (iter-end? #,(car binding)) #,guard-e)
                                (values #,@binding #,?index)
                                (cloop)))))
                (list binding
                      #'[dummy 0] ;; not used at all
                      #`(let cloop ()
                          (let-values ([#,binding (iter-next! #,iter)])
                            (if (or (iter-end? #,(car binding)) #,guard-e)
                                (values #,@binding)
                                (cloop)))))))))
      (define parse-clauses
        (lambda (cl*)
          (map (lambda (cl)
                 (syntax-case cl ()
                   [(binding . rest)
                    (let ([binding (syntax-case #'binding ()
                                     [var #'(var)]
                                     [(var) #'binding]
                                     ;; TODO how to support iterables that return more values?
                                     [(var0 var1) #'binding]
                                     [_ (syntax-error #'binding "bad comprehension variable:")])])
                      (syntax-case #'rest (guard)
                        [((index i) seq-e (guard ge ge* ...))
                         (and (eq? 'index (datum index)) (identifier? #'i))
                         (with-syntax ([(iter) (generate-temporaries '(iter))])
                           (printf ">>>5~n")
                           (list #'[iter (get-iter 'for seq-e)]
                                 (generate-clause-loop #'iter binding #'i #'(and ge ge* ...))))]
                        [((index i) seq-e)
                         (and (eq? 'index (datum index)) (identifier? #'i))
                         (with-syntax ([(iter) (generate-temporaries '(iter))])
                           (printf ">>>4~n")
                           (list #'[iter (get-iter 'for seq-e)]
                                 (generate-clause-loop #'iter binding #'i #'(begin #t))))]
                        [(seq-e (guard ge ge* ...))
                         (with-syntax ([(iter) (generate-temporaries '(iter))])
                           (printf ">>>6~n")
                           (list #'[iter (get-iter 'for seq-e)]
                                 (generate-clause-loop #'iter binding #f #'(and ge ge* ...))))]
                        [(seq-e)
                         (with-syntax ([(iter) (generate-temporaries '(iter))])
                           (printf ">>>1~n")
                           (list #'[iter (get-iter 'for seq-e)]
                                 (generate-clause-loop #'iter binding #f #'(begin #t))))]
                        [_ (syntax-error #'rest "invalid comprehension clause:")]))]))
               cl*)))
      (define parse-acc-clauses
        (lambda (acc-e*)
          (syntax-case acc-e* ()
            ;; TODO more checks?
            [() #'(() ())]
            [([var expr] ...)
             (andmap identifier? #'(var ...))
             #'((var ...) (expr ...))])))
      (trace-define generate-outer-loop
        (lambda (acc-e* ?for-index cl* brk ?fini e)
          (with-syntax ([((acc-var* ...) (acc-e* ...)) (parse-acc-clauses acc-e*)]
                        [((get-iters ((iter-v iter-v* ...) idx clause-loops)) ...)
                         (parse-clauses cl*)]
                        [(for-loop) (generate-temporaries '(for-loop))])
            (if ?for-index
                #`(let (get-iters ... idx ...)
                    (let for-loop ([#,?for-index 0] [acc-var* acc-e*] ...)
                      (let*-values ([(iter-v iter-v* ...) clause-loops] ...)
                        ;; test the 1st variable in each binding for iter-end
                        (if (or #,brk (ormap iter-end? (list iter-v ...)))
                            ;; TODO how to restrict the scope of fini-e?
                            #,(if ?fini
                                  ?fini
                                  ;; also valid then no acc is present
                                  #'(values acc-var* ...))
                            #,(let ([nacc (length #'(acc-var* ...))])
                                (cond [(= 0 nacc)
                                       #`(begin #,e (for-loop (add1 #,?for-index)))]
                                      [(= 1 nacc)
                                       ;; optimize the 1 value case
                                       #`(for-loop (add1 #,?for-index) #,e)]
                                      [else (with-syntax ([(v* ...) (generate-temporaries (iota nacc))])
                                              #`(let-values ([(v* ...) #,e])
                                                  (for-loop (add1 #,?for-index) v* ...)))]))))))
                #`(let (get-iters ...)
                    (let for-loop ([acc-var* acc-e*] ...)
                      (let*-values ([(iter-v iter-v* ...) clause-loops] ...)
                        (if (or #,brk (ormap iter-end? (list iter-v ...)))
                            #,(if ?fini
                                  ?fini
                                  #'(values acc-var* ...))
                            #,(let ([nacc (length #'(acc-var* ...))])
                                (cond [(= 0 nacc)
                                       #`(begin #,e (for-loop))]
                                      [(= 1 nacc)
                                       #`(for-loop #,e)]
                                      [else (with-syntax ([(v* ...) (generate-temporaries (iota nacc))])
                                              #`(let-values ([(v* ...) #,e])
                                                  (for-loop v* ...)))]))))))))))
      (generate-outer-loop acc-e* ?for-index cl* brk ?fini e)))


  (define $generate-for*/reduce
    (lambda (acc-e* ?for-index cl* brk ?fini e)
      (trace-define generate-clause-loop
        (lambda (iter binding ?index guard-e)
          (with-syntax ([(cloop dummy) (generate-temporaries '(cloop  dummy))])
            ;; return:
            ;; - bindings, e.g, (i), (i idx) ...
            ;; - index var for clauses with (index i)
            ;; - [bindings rhs] ...
            (if ?index
                ;; `binding` and `?index` are used both in this loop and outside
                ;; reuse `?index` so that the clause index var is visible in the guard
                (list #`(#,@binding #,?index)
                      #`[#,?index -1]
                      #`(let cloop ()
                          (let-values ([#,binding (iter-next! #,iter)])
                            (incr! #,?index)
                            (if (or (iter-end? #,(car binding)) #,guard-e)
                                (values #,@binding #,?index)
                                (cloop)))))
                (list binding
                      #'[dummy 0] ;; not used at all
                      #`(let cloop ()
                          (let-values ([#,binding (iter-next! #,iter)])
                            (if (or (iter-end? #,(car binding)) #,guard-e)
                                (values #,@binding)
                                (cloop)))))))))
      (define parse-clauses
        (lambda (cl*)
          (map (lambda (cl)
                 (syntax-case cl ()
                   [(binding . rest)
                    (let ([binding (syntax-case #'binding ()
                                     [var #'(var)]
                                     [(var) #'binding]
                                     ;; TODO how to support iterables that return more values?
                                     [(var0 var1) #'binding]
                                     [_ (syntax-error #'binding "bad comprehension variable:")])])
                      (syntax-case #'rest (guard)
                        [((index i) seq-e (guard ge ge* ...))
                         (and (eq? 'index (datum index)) (identifier? #'i))
                         (with-syntax ([(iter) (generate-temporaries '(iter))])
                           (printf ">>>5~n")
                           (list #'[iter (get-iter 'for seq-e)]
                                 (generate-clause-loop #'iter binding #'i #'(and ge ge* ...))))]
                        [((index i) seq-e)
                         (and (eq? 'index (datum index)) (identifier? #'i))
                         (with-syntax ([(iter) (generate-temporaries '(iter))])
                           (printf ">>>4~n")
                           (list #'[iter (get-iter 'for seq-e)]
                                 (generate-clause-loop #'iter binding #'i #'(begin #t))))]
                        [(seq-e (guard ge ge* ...))
                         (with-syntax ([(iter) (generate-temporaries '(iter))])
                           (printf ">>>6~n")
                           (list #'[iter (get-iter 'for seq-e)]
                                 (generate-clause-loop #'iter binding #f #'(and ge ge* ...))))]
                        [(seq-e)
                         (with-syntax ([(iter) (generate-temporaries '(iter))])
                           (printf ">>>1~n")
                           (list #'[iter (get-iter 'for seq-e)]
                                 (generate-clause-loop #'iter binding #f #'(begin #t))))]
                        [_ (syntax-error #'rest "invalid comprehension clause:")]))]))
               cl*)))
      (define parse-acc-clauses
        (lambda (acc-e*)
          (syntax-case acc-e* ()
            ;; TODO more checks?
            [() #'(() ())]
            [([var expr] ...)
             (andmap identifier? #'(var ...))
             #'((var ...) (expr ...))])))
      (trace-define generate-outer-loop
        (lambda (acc-e* ?for-index cl* brk ?fini e)
          (with-syntax ([((acc-var* ...) (acc-e* ...)) (parse-acc-clauses acc-e*)]
                        ;; the structure is a little complex...
                        [(([iter-vars iter-inits] . (rest)) ...)
                         (parse-clauses cl*)]
                        [(outer-loop) (generate-temporaries '(outer-loop))])
            (with-syntax ([(((iter-v0 iter-v0* ...) idx0 clause-loop0)
                            ((iter-v iter-v* ...) idx* clause-loops) ...)
                           #'(rest ...)])
              (if ?for-index
                  (with-syntax ([?for-index ?for-index])
                    #`(let ([iter-vars iter-inits] ... idx0 idx* ...)
                        ;; the outermost loop initilizes the global index and the accumulators
                        (let outer-loop ([?for-index 0] [acc-var* acc-e*] ...)
                          (let-values ([(iter-v0 iter-v0* ...) clause-loop0])
                            (if (iter-end? iter-v0)
                                #,(if ?fini
                                      ?fini
                                      #'(values acc-var* ...))
                                #,(let gen-nests ([itervar*  (cdr #'(iter-vars ...))]
                                                  [iterv*    #'((iter-v iter-v* ...) ...)]
                                                  [cl-loop*  #'(clause-loops ...)]
                                                  [prev-loop #'outer-loop])
                                    (with-syntax ([(for-loop) (generate-temporaries '(for-loop))])
                                      (if (null? iterv*)
                                          #`(if #,brk
                                                #,(if ?fini
                                                      ?fini
                                                      #'(values acc-var* ...))
                                                ;; run innermost loop
                                                #,(let ([nacc (length #'(acc-var* ...))])
                                                    (cond [(= 0 nacc)
                                                           #`(begin #,e (#,prev-loop (add1 ?for-index)))]
                                                          [(= 1 nacc)
                                                           ;; optimize the 1 value case
                                                           #`(#,prev-loop (add1 ?for-index) #,e)]
                                                          [else (with-syntax ([(v* ...) (generate-temporaries (iota nacc))])
                                                                  #`(let-values ([(v* ...) #,e])
                                                                      (#,prev-loop (add1 ?for-index) v* ...)))])))
                                          ;; pass ?for-index and acc-var* down to each loop nest for better performance
                                          #`(let for-loop ([?for-index ?for-index] [acc-var* acc-var*] ...)
                                              (let-values ([#,(car iterv*) #,(car cl-loop*)])
                                                (if (iter-end? #,(caar iterv*))
                                                    (begin (iter-reset! #,(car itervar*))
                                                           (#,prev-loop ?for-index acc-var* ...))
                                                    #,(gen-nests (cdr itervar*) (cdr iterv*) (cdr cl-loop*) #'for-loop))))))))))))
                  #`(let ([iter-vars iter-inits] ... idx0 idx* ...)
                      (let outer-loop ([acc-var* acc-e*] ...)
                        (let-values ([(iter-v0 iter-v0* ...) clause-loop0])
                          (if (iter-end? iter-v0)
                              #,(if ?fini
                                    ?fini
                                    #'(values acc-var* ...))
                              #,(let gen-nests ([itervar*  (cdr #'(iter-vars ...))]
                                                [iterv*    #'((iter-v iter-v* ...) ...)]
                                                [cl-loop*  #'(clause-loops ...)]
                                                [prev-loop #'outer-loop])
                                  (with-syntax ([(for-loop) (generate-temporaries '(for-loop))])
                                    (if (null? iterv*)
                                        #`(if #,brk
                                              #,(if ?fini
                                                    ?fini
                                                    #'(values acc-var* ...))
                                              #,(let ([nacc (length #'(acc-var* ...))])
                                                  (cond [(= 0 nacc)
                                                         #`(begin #,e (#,prev-loop))]
                                                        [(= 1 nacc)
                                                         ;; optimize the 1 value case
                                                         #`(#,prev-loop #,e)]
                                                        [else (with-syntax ([(v* ...) (generate-temporaries (iota nacc))])
                                                                #`(let-values ([(v* ...) #,e])
                                                                    (#,prev-loop v* ...)))])))
                                        #`(let for-loop ([acc-var* acc-var*] ...)
                                            (let-values ([#,(car iterv*) #,(car cl-loop*)])
                                              (if (iter-end? #,(caar iterv*))
                                                  (begin (iter-reset! #,(car itervar*))
                                                         (#,prev-loop acc-var* ...))
                                                  #,(gen-nests (cdr itervar*) (cdr iterv*) (cdr cl-loop*) #'for-loop))))))))))))))))
      (generate-outer-loop acc-e* ?for-index cl* brk ?fini e)))




  )
