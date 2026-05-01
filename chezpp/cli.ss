(library (chezpp cli)
  (export run-cli-command!
          cli-command cli-option cli-enum
          dump-command

          make-command command?
          command-name
          command-help
          command-overview
          command-version
          command-author
          command-copyright
          command-about
          command-style
          command-category
          command-exec
          command-multi-personality?
          command-maybe-no-subcommand?
          command-flatten-help?
          command-trace?
          command-quit?
          command-options
          command-subcommands

          command-name-set!
          command-help-set!
          command-overview-set!
          command-version-set!
          command-author-set!
          command-copyright-set!
          command-about-set!
          command-style-set!
          command-category-set!
          command-exec-set!
          command-multi-personality?-set!
          command-maybe-no-subcommand?-set!
          command-flatten-help?-set!
          command-trace?-set!
          command-quit?-set!
          command-options-add!
          command-subcommands-add!

          make-option option?
          option-help
          option-short
          option-long
          option-category
          option-default
          option-number
          option-value-number
          option-value-name
          option-value-parser
          option-value-seperator
          option-callback
          option-alias
          option-conflicts
          option-overrides
          option-requires
          option-positional?
          option-sink?
          option-no-short?
          option-no-long?
          option-no-grouping?
          option-hidden?

          option-help-set!
          option-short-set!
          option-long-set!
          option-category-set!
          option-default-set!
          option-number-set!
          option-value-number-set!
          option-value-name-set!
          option-value-parser-set!
          option-value-seperator-set!
          option-callback-set!
          option-alias-set!
          option-conflicts-set!
          option-overrides-set!
          option-requires-set!
          option-positional?-set!
          option-sink?-set!
          option-no-short?-set!
          option-no-long?-set!
          option-no-grouping?-set!
          option-hidden?-set!

          parser-bool
          parser-natural
          parser-integer
          parser-fixnum
          parser-flonum
          parser-string
          parser-enum)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp internal)
          (chezpp adt)
          (chezpp string)
          (chezpp list)
          (chezpp vector)
          (chezpp io))

  (define-record-type command
    (fields (mutable name command-name $command-name-set!)
            (mutable help command-help $command-help-set!)
            (mutable overview command-overview $command-overview-set!)
            (mutable version command-version $command-version-set!)
            (mutable author command-author $command-author-set!)
            (mutable copyright command-copyright $command-copyright-set!)
            (mutable about command-about $command-about-set!)
            (mutable style command-style $command-style-set!)
            (mutable category command-category $command-category-set!)
            (mutable exec command-exec $command-exec-set!)

            ;; TODO how to handle positionals?
            (mutable options command-options $command-options-set!)
            (mutable subcommands command-subcommands $command-subcommands-set!)

            (mutable multi-personality? command-multi-personality? $command-multi-personality?-set!)
            (mutable maybe-no-subcommand? command-maybe-no-subcommand? $command-maybe-no-subcommand?-set!)
            (mutable flatten-help? command-flatten-help? $command-flatten-help?-set!)
            (mutable trace? command-trace? $command-trace?-set!)
            (mutable quit? command-quit? $command-quit?-set!))
    (protocol (lambda (n)
                (case-lambda
                  [() (todo)]
                  [(name)
                   (pcheck ([symbol? name])
                           ;; TODO just use string?
                           (let ([name-str (symbol->string name)])
                             (when (string-startswith? name-str #\-)
                               (errorf 'make-command "command name cannot start with -: ~a" name))
                             (n name-str
                                "" "" "" "" "" "" "" ""
                                #f                  ; exec
                                (make-list-builder) ; options
                                (make-list-builder) ; subcommands
                                #f #f #f #f #f)))]))))

  (define-record-type option
    (fields (mutable name option-name $option-name-set!)
            (mutable help option-help $option-help-set!)
            (mutable short option-short $option-short-set!)
            (mutable long option-long $option-long-set!)
            (mutable category option-category $option-category-set!)
            (mutable default option-default $option-default-set!)
            (mutable number option-number $option-number-set!)

            (mutable value-number option-value-number $option-value-number-set!)
            (mutable value-name option-value-name $option-value-name-set!)
            (mutable value-parser option-value-parser $option-value-parser-set!)
            (mutable value-seperator option-value-seperator $option-value-seperator-set!)

            (mutable callback option-callback $option-callback-set!)
            (mutable alias option-alias $option-alias-set!)
            (mutable conflicts option-conflicts $option-conflicts-set!)
            (mutable overrides option-overrides $option-overrides-set!)
            (mutable requires option-requires $option-requires-set!)

            (mutable positional? option-positional? $option-positional?-set!)
            (mutable sink? option-sink? $option-sink?-set!)
            ;; TODO maybe not needed
            (mutable no-short? option-no-short? $option-no-short?-set!)
            (mutable no-long? option-no-long? $option-no-long?-set!)
            (mutable no-grouping? option-no-grouping? $option-no-grouping?-set!)
            (mutable hidden? option-hidden? $option-hidden?-set!))
    (protocol (lambda (n)
                (case-lambda
                  [(name)
                   (pcheck ([symbol? name])
                           ;; TODO just use string?
                           (let ([name-str (symbol->string name)])
                             (when (string-startswith? name-str #\-)
                               (errorf 'make-option "option name cannot start with -: ~a" name))
                             (n name
                                ""                      ; help
                                (string (string-ref name-str 0)) ; short
                                name-str                ; long
                                ""                      ; category
                                #f                      ; default
                                '*                      ; number
                                '0                      ; value-number
                                #f                      ; value-name
                                parser-bool             ; value-parser
                                #\,     ; value-seperator
                                #f      ; callback
                                #f      ; alias
                                '()     ; conflicts
                                '()     ; overrides
                                '()     ; requires
                                #f      ; positional?
                                #f      ; sink?
                                #f      ; no-short?
                                #f      ; no-long?
                                #f      ; no-grouping?
                                #f      ; hidden?
                                )))]))))


;;===----------------------------------------------------------------------===
;; procedural interface
;;===----------------------------------------------------------------------===


  (define command-name-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [symbol? v])
              ($command-name-set! cmd v))))
  (define command-help-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [string? v])
              ($command-help-set! cmd v))))
  (define command-overview-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [string? v])
              ($command-overview-set! cmd v))))
  (define command-version-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [string? v])
              ($command-version-set! cmd v))))
  (define command-author-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [string? v])
              ($command-author-set! cmd v))))
  (define command-copyright-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [string? v])
              ($command-copyright-set! cmd v))))
  (define command-about-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [string? v])
              ($command-about-set! cmd v))))
  (define command-style-set!
    (lambda (cmd v)
      (pcheck ([command? cmd])
              (todo))))
  (define command-category-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [string? v])
              ($command-category-set! cmd v))))
  (define command-exec-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [procedure? v])
              ($command-exec-set! cmd v))))
  (define command-multi-personality?-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [boolean? v])
              ($command-multi-personality?-set! cmd v))))
  (define command-maybe-no-subcommand?-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [boolean? v])
              ($command-maybe-no-subcommand?-set! cmd v))))
  (define command-flatten-help?-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [boolean? v])
              ($command-flatten-help?-set! cmd v))))
  (define command-trace?-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [boolean? v])
              ($command-trace?-set! cmd v))))
  (define command-quit?-set!
    (lambda (cmd v)
      (pcheck ([command? cmd] [boolean? v])
              ($command-quit?-set! cmd v))))
  (define-who command-options-add!
    (lambda (cmd . opts)
      (pcheck ([command? cmd])
              (unless (andmap option? opts)
                (errorf who "only option objects are allowed: ~a" opts))
              (for-each (lambda (opt)
                          (let ([lb-opts (command-options cmd)])
                            (unless (memq opt (lb-opts))
                              (lb-opts opt))))
                        opts))))
  (define-who command-subcommands-add!
    (lambda (cmd . subcmds)
      (pcheck ([command? cmd])
              (unless (andmap command? subcmds)
                (errorf who "only command objects are allowed: ~a" subcmds))
              (for-each (lambda (subcmd)
                          (when (eq? cmd subcmd)
                            (errorf who "cannot add command ~a to its own subcommands" cmd))
                          (let ([lb-subcmds (command-subcommands cmd)])
                            (unless (memq subcmd (lb-subcmds))
                              (lb-subcmds subcmd))))
                        subcmds))))


  (define *numbers* '(1 ? + *))
  (define *value-numbers* (cons 0 *numbers*))
  (define *none* (list 'none))

  (define option-name-set!
    (lambda (opt v)
      (pcheck ([option? opt] [symbol? v])
              ;; automatically set long/short
              (todo))))
  (define option-help-set!
    (lambda (opt v)
      (pcheck ([option? opt] [string? v])
              ($option-help-set! opt v))))
  ;; TODO where to check duplicate options?
  (define-who option-short-set!
    (lambda (opt v)
      (pcheck ([option? opt] [(p/or string? boolean?) v])
              (when (string? v)
                (unless (fx= (string-length v) 1)
                  (errorf who "option's short form must have length 1: ~s" v)))
              ;; TODO disallow things like -?
              ($option-short-set! opt v))))
  (define option-long-set!
    (lambda (opt v)
      (pcheck ([option? opt] [(p/or string? boolean?) v])
              ;; TODO strip leading "--"
              ($option-long-set! opt v))))
  (define option-category-set!
    (lambda (opt v)
      (pcheck ([option? opt] [string? v])
              ($option-category-set! opt v))))
  (define option-default-set!
    (lambda (opt v)
      (pcheck ([option? opt])
              ($option-default-set! opt v))))
  (define-who option-number-set!
    (lambda (opt v)
      (pcheck ([option? opt])
              (unless (memq v *numbers*)
                (errorf who "invalid number specifier: ~a" v))
              ($option-number-set! opt v))))
  (define-who option-value-number-set!
    (lambda (opt v)
      (pcheck ([option? opt])
              (unless (memq v *value-numbers*)
                (errorf who "invalid value number specifier: ~a" v))
              ($option-value-number-set! opt v))))
  (define option-value-name-set!
    (lambda (opt v)
      (pcheck ([option? opt] [string? v])
              ($option-value-name-set! opt v))))
  (define option-value-parser-set!
    (lambda (opt v)
      (pcheck ([option? opt] [procedure? v])
              ($option-value-parser-set! opt v))))
  (define option-value-seperator-set!
    (lambda (opt v)
      (pcheck ([option? opt] [char? v])
              ($option-value-seperator-set! opt v))))
  (define option-callback-set!
    (lambda (opt v)
      (pcheck ([option? opt] [procedure? v])
              ($option-callback-set! opt v))))
  (define option-alias-set!
    (lambda (opt v)
      (pcheck ([option? opt])
              (todo))))
  (define option-conflicts-set!
    (lambda (opt v)
      (pcheck ([option? opt])
              (todo))))
  (define option-overrides-set!
    (lambda (opt v)
      (pcheck ([option? opt])
              (todo))))
  (define option-requires-set!
    (lambda (opt v)
      (pcheck ([option? opt])
              (todo))))
  (define option-positional?-set!
    (lambda (opt v)
      (pcheck ([option? opt] [boolean? v])
              ($option-positional?-set! opt v)
              ($option-long-set!   opt #f)
              ($option-short-set!  opt #f)
              ($option-number-set! opt 1)
              ;;($option-value-number-set! opt 1)
              )))
  (define-who option-sink?-set!
    (lambda (opt v)
      (pcheck ([option? opt] [boolean? v])
              (unless (option-positional? opt)
                (errorf who "option ~a is not positional" opt))
              ($option-sink?-set! opt v)
              ($option-number-set! opt '+)
              ;;($option-value-number-set! opt '+)
              )))
  (define option-no-short?-set!
    (lambda (opt v)
      (pcheck ([option? opt] [boolean? v])
              ($option-no-short?-set! opt v))))
  (define option-no-long?-set!
    (lambda (opt v)
      (pcheck ([option? opt] [boolean? v])
              ($option-no-long?-set! opt v))))
  (define option-no-grouping?-set!
    (lambda (opt v)
      (pcheck ([option? opt] [boolean? v])
              ($option-no-grouping?-set! opt v))))
  (define option-hidden?-set!
    (lambda (opt v)
      (pcheck ([option? opt] [boolean? v])
              ($option-hidden?-set! opt v))))


  (record option-group
          (options))


  (record enum
          (name value help))


;;===----------------------------------------------------------------------===
;; builtin value parsers
;;===----------------------------------------------------------------------===


  (define parser-bool
    (lambda (cmd opt x)
      (todo)))

  (define parser-natural
    (lambda (cmd opt x)
      (todo)))

  (define parser-integer
    (lambda (cmd opt x)
      (todo)))

  (define parser-fixnum
    (lambda (cmd opt x)
      (todo)))

#;
  (define parser-double
    (lambda (cmd opt x)
      (let ([string->number x])
        (if (flonum?)))))

  (define-who parser-flonum
    (lambda (cmd opt x)
      (let ([v (string->number x)])
        (if v v (errorf who "failed to parse ~s as flonum" x)))))

  (define parser-string
    (lambda (cmd opt x)
      x))

  (define parser-enum
    (lambda (cmd opt x)
      (todo)))



;;===----------------------------------------------------------------------===
;; syntactic interface
;;===----------------------------------------------------------------------===


  (define-syntax cli-command
    (lambda (stx)
      (syntax-case stx ()
        [(name config* options commands)
         (todo)])))


  ;; TODO share code with `cli-command`
  (define-syntax cli-option
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))


  (define-syntax cli-option-group
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))


  (define-syntax cli-enum
    (lambda (stx)
      (syntax-case stx ()
        [_ (todo)])))


  ;; TODO check duplicate short/long
  (define-who run-cli-command!
    (case-lambda
      [(cmd)
       (pcheck ([command? cmd])
               (run-cli-command! cmd (command-line)))]
      [(cmd args)
       (pcheck ([list? args])
               ;; option name -> parsed values
               (define opts-state (make-eq-hashtable))
               (define cmds-seen (make-list-builder))
               (define (leaf-command? cmd) (null? ((command-subcommands cmd))))
               (define (find-subcommand arg subcmds)
                 (find (lambda (cmd) (string=? arg (command-name cmd))) subcmds))
               (define (split-opt-arg short? opt-arg)
                 ;; --o, --o=v, --o=, --o=v=x=x=
                 (let ([pos= (string-search opt-arg "=")]
                       [len (string-length opt-arg)]
                       [split-point (if short? 1 2)])
                   (if pos=
                       (values (string->symbol (substring opt-arg split-point pos=))
                               (substring opt-arg (fx1+ pos=) len))
                       ;; TODO #f enough?
                       (values (string->symbol (substring opt-arg split-point len)) #f))))
               (define (handle-opt-arg cmd opt opt-name v)
                 (let ([number       (option-number opt)]
                       [value-number (option-value-number opt)]
                       [value-parser (option-value-parser opt)])
                   ;; Note: if opts-state is not filled, opt may take default value later.
                   ;; checks:
                   ;; if number is 1/? and opts-state is filled, error
                   ;; if value-number is 0 and v is not #f/"", error
                   ;; if value-number is 1/+ and v is #f/"", error
                   (when (and (eq? 1 number)
                              (eq-hashtable-contains? opts-state opt-name))
                     (println "error: option ~a can only appear once!" opt-name)
                     (print-help-and-quit (cmds-seen)))
                   (when (and (eq? '? number)
                              (eq-hashtable-contains? opts-state opt-name))
                     (println "error: option ~a can only appear zero times or once!" opt-name)
                     (print-help-and-quit (cmds-seen)))
                   (when (and (eq? 0 value-number)
                              v)
                     (println "error: option ~a does not take value(s)!" opt-name)
                     (print-help-and-quit (cmds-seen)))
                   (when (and (or (eq? 1 value-number) (eq? '+ value-number))
                              (not v))
                     (println "error: option ~a must take value(s)!" opt-name)
                     (print-help-and-quit (cmds-seen)))

                   (cond
                    [(or (eq? '+ value-number) (eq? '* value-number))
                     (let* ([vals (map (lambda (v) (value-parser cmd opt v))
                                       (string-split v (option-value-seperator opt)))])
                       (println "adding value(s) ~s for ~a" vals opt-name)
                       (eq-hashtable-set! opts-state opt-name
                                          (append (eq-hashtable-ref opts-state opt-name '()) vals)))]
                    ;; TODO refactor the conditional branches
                    [(option-sink? opt)
                     (if (or (eq? '+ value-number) (eq? '* value-number))
                         (let* ([vals (map (lambda (v) (value-parser cmd opt v))
                                           (string-split v (option-value-seperator opt)))])
                           (println "adding value(s) ~s for positional ~a" vals opt-name)
                           (eq-hashtable-set! opts-state opt-name
                                              (append (eq-hashtable-ref opts-state opt-name '()) vals)))
                         (begin (println "adding value ~s for positional ~a (state ~a)" v opt-name (hashtable-cells opts-state))
                                (eq-hashtable-set! opts-state opt-name
                                                   (append (eq-hashtable-ref opts-state opt-name '()) (list v)))))]
                    [else
                     (println "adding value ~s for ~a" v opt-name)
                     ;; may override previous value
                     (eq-hashtable-set! opts-state opt-name (value-parser cmd opt v))])))
               (define (check-option-values cmd tbl)
                 (vector-for-each
                  (lambda (kv)
                    (let* ([o (car kv)] [opt (cdr kv)]
                           [number (option-number opt)]
                           [opt-name (option-name opt)])
                      (when (and (memq number '(1 +))
                                 (not (eq-hashtable-contains? opts-state opt-name)))
                        ;; TODO use --o/-o
                        (begin (println "option ~a is required but not given" opt-name)
                               (print-help-and-quit (cmds-seen))))
                      (when (and (memq number '(? *))
                                 (not (eq-hashtable-contains? opts-state opt-name)))
                        ;; TODO what if default is not given?
                        (println "check-option-values setting default of ~a to ~a" opt-name (option-default opt))
                        (eq-hashtable-set! opts-state opt-name (option-default opt)))))
                  (hashtable-cells tbl)))
               (define (check-positional-option-values cmd positionals)
                 (for-each (lambda (opt)
                             (let ([number (option-number opt)] [opt-name (option-name opt)])
                               (assert (memq number '(1 +)))
                               (unless (eq-hashtable-contains? opts-state opt-name)
                                 (println "error: positional argument ~a is required but not given" opt-name)
                                 (print-help-and-quit (cmds-seen)))))
                           positionals))
               (define (check-missing-subcommand cmd)
                 (unless (command-maybe-no-subcommand? cmd)
                   (let ([subcmds ((command-subcommands cmd))])
                     (unless (null? subcmds)
                       (println "error: (sub)command expected: ~a" (map command-name subcmds))
                       (print-help-and-quit (cmds-seen))))))

               (unless (andmap string? args)
                 (errorf who "expected list of strings: ~a" args))
               (println "~a: args ~a" who args)


               ;; main parse loop
               (let lp-cmd ([cmd cmd] [args args])
                 ;; short/long symbol -> option
                 (define-values (opts-table-short opts-table-long)
                   (let ([ht-short (make-eq-hashtable)] [ht-long (make-eq-hashtable)])
                     (for-each (lambda (opt)
                                 ;; TODO alias
                                 (when (option-short opt)
                                   (assert (not (option-positional? opt)))
                                   (eq-hashtable-set! ht-short
                                                      (string->symbol (option-short opt)) opt))
                                 (when (option-long opt)
                                   (assert (not (option-positional? opt)))
                                   (eq-hashtable-set! ht-long
                                                      (string->symbol (option-long opt)) opt)))
                               ((command-options cmd)))
                     (values ht-short ht-long)))
                 ;;(println "opts-table-short ~a" (hashtable-cells opts-table-short))
                 ;;(println "opts-table-long  ~a" (hashtable-cells opts-table-long))
                 (cmds-seen cmd)

                 (let lp-arg ([args args])
                   (if (null? args)
                       ;; call exec
                       ;; TODO handle args = '()
                       (begin
                         (check-option-values cmd opts-table-short)
                         (check-option-values cmd opts-table-long)
                         (check-positional-option-values cmd
                                                         (filter option-positional? ((command-options cmd))))
                         (check-missing-subcommand cmd)
                         (when (command-exec cmd)
                           ((command-exec cmd)
                            (lambda (opt-name)
                              (pcheck ([symbol? opt-name])
                                      ;;(println "state table: ~s" (hashtable-cells opts-state))
                                      (let ([v (eq-hashtable-ref opts-state opt-name *none*)])
                                        (if (eq? v *none*)
                                            (errorf 'command-exec-1 "option ~a does not exist" opt-name)
                                            v)))))))
                       (let ([arg (car args)])
                         (cond
                          [(string=? arg "-h")
                           (if (eq-hashtable-contains? opts-table-short 'h)
                               (let ([opt (eq-hashtable-ref opts-table-short 'h #f)])
                                 (handle-opt-arg cmd opt (option-name opt) #f))
                               (begin (print-help (cmds-seen) #f #f) (exit 0)))]
                          [(string=? arg "--help")
                           (if (eq-hashtable-contains? opts-table-long 'help)
                               (let ([opt (eq-hashtable-ref opts-table-long 'help #f)])
                                 (handle-opt-arg cmd opt (option-name opt) #f))
                               (begin (print-help (cmds-seen) #f #f) (exit 0)))]
                          [(string=? arg "--help-all")
                           (if (eq-hashtable-contains? opts-table-long 'help-all)
                               (let ([opt (eq-hashtable-ref opts-table-long 'help-all #f)])
                                 (handle-opt-arg cmd opt (option-name opt) #f))
                               (begin (print-help (cmds-seen) #t #f) (exit 0)))]
                          [(string=? arg "--help-commands")
                           (if (eq-hashtable-contains? opts-table-long 'help-commands)
                               (let ([opt (eq-hashtable-ref opts-table-long 'help-commands #f)])
                                 (handle-opt-arg cmd opt (option-name opt) #f))
                               (begin (print-help (cmds-seen) #f #t) (exit 0)))]
                          [(and (string-startswith? arg "--") (not (string=? arg "--")))
                           (let-values ([(o v) (split-opt-arg #f arg)])
                             (if (eq-hashtable-contains? opts-table-long o)
                                 (let ([opt (eq-hashtable-ref opts-table-long o #f)])
                                   (handle-opt-arg cmd opt (option-name opt) v))
                                 (begin
                                   (println "error: unknown long option: ~a" arg)
                                   (print-help-and-quit (cmds-seen)))))
                           (lp-arg (cdr args))]
                          [(and (string-startswith? arg "-") (not (string=? arg "-")))
                           ;; no grouping yet
                           (let-values ([(o v) (split-opt-arg #t arg)])
                             (if (eq-hashtable-contains? opts-table-short o)
                                 (let ([opt (eq-hashtable-ref opts-table-short o #f)])
                                   (handle-opt-arg cmd opt (option-name opt) v))
                                 (begin
                                   (println "error: unknown short option: ~a" arg)
                                   (print-help-and-quit (cmds-seen)))))
                           (lp-arg (cdr args))]
                          [else
                           (if (leaf-command? cmd)
                               ;; handle positionals
                               (let* ([positionals (filter option-positional? ((command-options cmd)))]
                                      [len-args (length args)]
                                      [len-positionals (length positionals)])
                                 ;;(println "positionals (len ~a) ~a" (length positionals) positionals)
                                 ;;(println "args        (len ~a) ~a" (length args) args)
                                 (when (null? positionals)
                                   (begin (println "unknown arguments supplied: ~a" args)
                                          (print-help-and-quit (cmds-seen))))
                                 (cond [(member "--" args)
                                        ;; TODO `args` may be (aa bb -- xx yy zz), where (xx yy zz) must be accepted by a sink
                                        (let* ([rest (member "--" args)]
                                               [sink-args (cdr rest)]
                                               [non-sink-args (list-head args (fx- (length args) (length rest)))])
                                          ;;(println "all args --   ~a" args)
                                          ;;(println "non-sink-args ~a" non-sink-args)
                                          ;;(println "sink-args     ~a" sink-args)
                                          (unless (= (length non-sink-args) (fx1- len-positionals))
                                            (println "error: invalid number of arguments supplied: ~a" args)
                                            (print-help-and-quit (cmds-seen)))
                                          (unless (option-sink? (list-last positionals))
                                            (println "error: invalid number of arguments supplied, expected one: ~a" args)
                                            (print-help-and-quit (cmds-seen)))
                                          (let ([opt-sink (list-last positionals)])
                                            (for-each (lambda (opt arg)
                                                        (handle-opt-arg cmd opt (option-name opt) arg))
                                                      (list-head positionals (fx1- len-positionals)) non-sink-args)
                                            (for-each (lambda (arg)
                                                        (handle-opt-arg cmd opt-sink (option-name opt-sink) arg))
                                                      sink-args)))]
                                       [(= len-args len-positionals)
                                        (for-each (lambda (opt arg)
                                                    (handle-opt-arg cmd opt (option-name opt) arg))
                                                  positionals args)]
                                       [(and (option-sink? (list-last positionals))
                                             (>= len-args len-positionals))
                                        (let* ([non-sink-args (list-head args (fx1- len-positionals))]
                                               [sink-args     (list-tail args (fx1- len-positionals))]
                                               [opt-sink      (list-last positionals)])
                                          ;;(println "non-sink-args ~a" non-sink-args)
                                          ;;(println "sink-args     ~a" sink-args)
                                          (for-each (lambda (opt arg)
                                                      (handle-opt-arg cmd opt (option-name opt) arg))
                                                    (list-head positionals (fx1- len-positionals)) non-sink-args)
                                          (for-each (lambda (arg)
                                                      (handle-opt-arg cmd opt-sink (option-name opt-sink) arg))
                                                    sink-args))]
                                       [else (begin (println "error: incorrect number of positional arguments supplied: ~a" args)
                                                    (print-help-and-quit (cmds-seen)))])
                                 ;; finish
                                 (lp-arg '()))
                               ;; try next command
                               (let* ([subcmds ((command-subcommands cmd))] [subcmd (find-subcommand arg subcmds)])
                                 (if subcmd
                                     ;; check whether option args are fully supplied, and maybe fill defaults
                                     ;; - if number is 1/+ but opts-state has no value, error
                                     ;; - if number is ?/* but opts-state has no value, use default, or error
                                     (begin
                                       (check-option-values cmd opts-table-short)
                                       (check-option-values cmd opts-table-long)
                                       (when (command-exec cmd)
                                         ((command-exec cmd)
                                          (lambda (opt-name)
                                            (pcheck ([symbol? opt-name])
                                                    (let ([v (eq-hashtable-ref opts-state opt-name *none*)])
                                                      (if (eq? v *none*)
                                                          (errorf 'command-exec-2 "option ~a does not exist" opt-name)
                                                          v))))))
                                       (lp-cmd subcmd (cdr args)))
                                     (begin (println "error: unknown (sub)command ~a" arg)
                                            (print-help-and-quit (cmds-seen))))))]))))))]))



;;===----------------------------------------------------------------------===
;; internals
;;===----------------------------------------------------------------------===

  ;; print usage of current cmd and if last cmd supports `--help`,
  ;; print "for more info, run "xxx --help""
  (define-who print-help-and-quit
    (case-lambda
      [(cmds-seen)
       ;; TODO call print-usage
       (define (get/make-positional-value-name opt)
         (let ([vn (option-value-name opt)]
               [tail (if (option-sink? opt) " ..." "")])
           ;; TODO make sure `vn` is like <XXX>
           (string-append (if vn
                              vn
                              (string-append "<"
                                             (string-upcase (symbol->string (option-name opt)))
                                             ">"))
                          tail)))
       (assert (fx> (length cmds-seen) 0))
       ;; Usage: CMD0 COMMAND
       ;; Usage: CMD0 [OPTIONS] CMD1 [OPTIONS] <POS0> <POS1> ...
       ;; For more info, run 'CMD0 CMD1 --help'
       ;;(println ">>> print-help-and-quit ~a" (map command-name cmds-seen))
       (display "Usage: ")
       (let loop ([cmds cmds-seen])
         (unless (null? cmds)
           (let ([cmd (car cmds)])
             (print "~a " (command-name cmd))
             (unless (null? ((command-options cmd)))
               (print "[OPTIONS] "))
             (let* ([positionals (filter option-positional? ((command-options cmd)))])
               (if (null? positionals)
                   (if (and (not (null? ((command-subcommands cmd))))
                            (null? (cdr cmds)))
                       (print "COMMAND "))
                   (begin (assert (null? ((command-subcommands cmd))))
                          (let ([value-names (map get/make-positional-value-name positionals)])
                            (for-each (lambda (vn) (print "~a " vn)) value-names)))))
             (loop (cdr cmds)))))
       (let* ([cmd-last (list-last cmds-seen)] [opts ((command-options cmd-last))]
              [?help (filter (lambda (opt)
                               (and (option-long opt) (string=? (option-long opt) "help")))
                             opts)])
         (if (null? ?help)
             (newline)
             (begin (print "For more info, run '")
                    (for-each (lambda (cmd)
                                (print "~a " (command-name cmd)))
                              cmds-seen)
                    (print " --help'\n"))))
       (todo 'quit-or-what?)]))


  (define-who dump-command
    (case-lambda
      [(cmd) (dump-command cmd #t)]
      [(cmd simple?)
       (pcheck ([command? cmd] [boolean? simple?])
               (define indent (lambda (w) (make-string w #\space)))
               (define println/indent
                 (lambda (w fmt . args)
                   (print (indent w))
                   (apply println fmt args)))
               (let lp-cmd ([cmd cmd] [level 0])
                 (if simple?
                     (println/indent level "* ~s" (command-name cmd))
                     (println/indent level "command:   ~s" (command-name      cmd)))
                 (unless simple?
                   (println/indent (+ level 2) "version:   ~s" (command-version   cmd))
                   (println/indent (+ level 2) "author:    ~s" (command-author    cmd))
                   (println/indent (+ level 2) "copyright: ~s" (command-copyright cmd))
                   (println/indent (+ level 2) "overview:  ~s" (command-overview  cmd))
                   (println/indent (+ level 2) "help:      ~s" (command-help      cmd)))
                 (println/indent (+ level 2) "options:~a" (if (null? ((command-options cmd))) " none" ""))
                 (for-each (lambda (opt)
                             (if simple?
                                 (println/indent (+ level 4) "- ~s" (option-name opt))
                                 (println/indent (+ level 4) "name:  ~s" (option-name opt)))
                             (unless simple?
                               (println/indent (+ level 4) "help:  ~s" (option-help opt))
                               (println/indent (+ level 4) "short: ~s" (option-short opt))
                               (println/indent (+ level 4) "long:  ~s" (option-long opt))
                               (println/indent (+ level 4) "category: ~s" (option-category opt))
                               (println/indent (+ level 4) "default:  ~s" (option-default opt))
                               (println/indent (+ level 4) "number:   ~s" (option-number opt))
                               (println/indent (+ level 4) "value-number: ~s" (option-value-number opt))
                               (println/indent (+ level 4) "value-name:   ~s" (option-value-name opt))
                               (println/indent (+ level 4) "value-parser: ~s" (option-value-parser opt))
                               (println/indent (+ level 4) "value-seperator: ~s" (option-value-seperator opt))
                               (println/indent (+ level 4) "callback:  ~s" (option-callback opt))
                               (println/indent (+ level 4) "alias:     ~s" (option-alias opt))
                               (println/indent (+ level 4) "conflicts: ~s" (option-conflicts opt))
                               (println/indent (+ level 4) "overrides: ~s" (option-overrides opt))
                               (println/indent (+ level 4) "requires:  ~s" (option-requires opt))
                               (println/indent (+ level 4) "positional?:  ~s" (option-positional? opt))
                               (println/indent (+ level 4) "sink?:        ~s" (option-sink? opt))
                               (println/indent (+ level 4) "no-grouping?: ~s" (option-no-grouping? opt))
                               (println/indent (+ level 4) "hidden?:      ~s" (option-hidden? opt))
                               (newline)))
                           ((command-options cmd)))
                 (println/indent (+ level 2) "subcommands:~a" (if (null? ((command-subcommands cmd))) " none" ""))
                 (for-each (lambda (subcmd)
                             (lp-cmd subcmd (+ level 4))
                             (unless simple?
                               (newline)))
                           ((command-subcommands cmd)))))]))


  ;; for top command, print overview...
  ;; print current command or print all subcommands
  ;; to stdout or stderr?
  ;; Outut:
  ;; if top cmd:
  ;;    - overview
  ;;    - author, version
  ;;    - copyright
  ;;  - usage
  ;;  - options & subcommands
  ;;  - about
  (define-who print-help
    (lambda (cmds-seen all? commands?)
      (define (print-overview cmd)
        (let ([str (command-overview cmd)])
          (unless (string=? str "")
            (println "~a" str))))
      (define (print-author-and-version cmd)
        (let ([str1 (command-copyright cmd)]
              [str2 (command-version cmd)])
          ;; TODO
          (unless (string=? str1 "")
            (println "~a" str1))))
      (define (print-copyright cmd)
        (let ([str (command-copyright cmd)])
          (unless (string=? str "")
            (println "~a" str))))
      (define (print-all-commands)
        (todo))
      ;; print options and commands of current command
      (define (print-options-and-commands)
        ;; categorize options and commands
        ;; TODO handle hidden?
        (define (categorize getter sorter objs)
          (let ([ht (make-hashtable string-hash string=?)])
            (for-each (lambda (obj)
                        (let* ([cat (getter obj)] [ls (hashtable-ref ht cat '())])
                          (hashtable-set! ht cat (cons obj ls))))
                      objs)
            ;; default category is ""
            ;; TODO sort
            (vector-for-each
             (lambda (cat)
               (let ([ls (hashtable-ref ht cat '())])
                 (hashtable-set! ht cat (sort sorter ls))))
             (hashtable-keys ht))
            ht))
        ;; category string -> list of commands/options
        (define ht-cmds (categorize command-category
                                    (lambda (c1 c2)
                                      (string<? (command-name c1) (command-name c2)))
                                    ((command-subcommands (list-last cmds-seen)))))
        (define ht-opts (categorize option-category
                                    (lambda (o1 o2)
                                      (let ([short1 (option-short o1)]
                                            [short2 (option-short o2)]
                                            [long1 (option-long o1)]
                                            [long2 (option-long o2)])
                                        (cond [(and short1 short2)
                                               (string<? short1 short2)]
                                              [(and long1 long2)
                                               (string<? long1 long2)]
                                              [else #t])))
                                    (filter (p/not option-positional?)
                                            ((command-options (list-last cmds-seen))))))
        (define ls-opts-positional (sort (lambda (o1 o2)
                                           (string<? (symbol->string (option-name o1))
                                                     (symbol->string (option-name o2))))
                                         (filter option-positional?
                                                 ((command-options (list-last cmds-seen))))))
        ;; opt -> short/long usage summaries
        (define ht-opts-short-summaries (make-eq-hashtable))
        (define ht-opts-long-summaries (make-eq-hashtable))
        (define ls-opts-positional-summaries
          (map (lambda (opt)
                 (let ([vn (option-value-name opt)]
                       [tail (if (option-sink? opt) " ..." "")])
                   (string-append (if vn
                                      vn
                                      (string-append "<"
                                                     (string-upcase (symbol->string (option-name opt)))
                                                     ">"))
                                  tail)))
               ls-opts-positional))
        ;; (println ">>> ht-cmds ~a" (hashtable-cells ht-cmds))
        ;; (println ">>> ht-opts ~a" (hashtable-cells ht-opts))
        ;; generate option usage summaries
        (vector-for-each
         (lambda (cat opts)
           (for-each (lambda (opt)
                       ;; generate option usage summaries
                       (let* ([short (option-short opt)] [long (option-long opt)]
                              [number (option-number opt)] [value-number (option-value-number opt)]
                              [vn (let ([vn (option-value-name opt)])
                                    (if vn
                                        vn
                                        (string-append "<"
                                                       (string-upcase (symbol->string (option-name opt)))
                                                       ">")))]
                              [val-str
                               (case value-number
                                 [0 ""]
                                 [1 (string-append "=" vn)]
                                 [? (string-append "=[" vn "]")]
                                 [+ (string-append "="  vn (string (option-value-seperator opt)) "...")]
                                 [* (string-append "=[" vn (string (option-value-seperator opt)) "...]")]
                                 [else (assert-unreachable)])])
                         (when short
                           (hashtable-set! ht-opts-short-summaries opt
                                           (string-append "-" short val-str)))
                         (when long
                           (hashtable-set! ht-opts-long-summaries opt
                                           (string-append "--" long val-str)))))
                     opts))
         (hashtable-keys ht-opts) (hashtable-values ht-opts))
        ;; (println "ht-opts-short-summaries ~a" (hashtable-cells ht-opts-short-summaries))
        ;; (println "ht-opts-long-summaries  ~a" (hashtable-cells ht-opts-long-summaries))
        ;; print positional options
        (when (fx> (length ls-opts-positional-summaries) 0)
          (let ([max-len (fx+ 2 (apply max (map string-length ls-opts-positional-summaries)))])
            (println "ARGUMENTS")
            (for-each (lambda (opt str)
                        (println "    ~a~a~a"
                                 str
                                 (make-string (fx- max-len (string-length str)) #\space)
                                 (option-help opt)))
                      ls-opts-positional ls-opts-positional-summaries)
            (newline)))
        ;; print options
        (when (or (fx> (hashtable-size ht-opts-short-summaries) 0)
                  (fx> (hashtable-size ht-opts-long-summaries)  0))
          (let ()
            ;; compute max length of option usage summaries and paddings for each summary
            (define (max-len str-vec)
              (vfold-left (lambda (acc str)
                            (if (fx> (string-length str) acc) (string-length str) acc))
                          0 str-vec))
            ;; + x for padding before the long opt or before the option help text
            (define max-short (fx+ 1 (max-len (hashtable-values ht-opts-short-summaries))))
            (define max-long  (fx+ 2 (max-len (hashtable-values ht-opts-long-summaries))))
            (vector-for-each
             (lambda (cat opts)
               (println "~a" (if (string=? cat "") "OPTIONS" cat))
               (for-each (lambda (opt)
                           (let* ([short (hashtable-ref ht-opts-short-summaries opt #f)]
                                  [long  (hashtable-ref ht-opts-long-summaries opt #f)]
                                  [?short-len (if short (string-length short) #f)]
                                  [?long-len  (if long  (string-length long) #f)])
                             (cond
                              [(and short long)
                               (println "    ~a,~a~a~a~a"
                                        short
                                        (make-string (fx- max-short ?short-len) #\space)
                                        long
                                        (make-string (fx- max-long ?long-len) #\space)
                                        (option-help opt))]
                              [(and short (not long))
                               (println "    ~a~a~a~a"
                                        short
                                        (make-string (fx- max-short ?short-len) #\space)
                                        (make-string (fx+ max-long) #\space)
                                        (option-help opt))]
                              [(and (not short) long)
                               (println "    ~a~a~a~a"
                                        (make-string (fx+ max-short 1) #\space)
                                        long
                                        (make-string (fx- max-long ?long-len) #\space)
                                        (option-help opt))]
                              [else (assert-unreachable)])))
                         opts)
               (newline))
             (hashtable-keys ht-opts) (hashtable-values ht-opts))))
        ;; print subcommands
        (vector-for-each
         (lambda (cat cmds)
           (println "~a" (if (string=? cat "") "COMMANDS" cat))
           (map (lambda (cmd)
                  (let ([len (string-length (command-name cmd))])
                    (println "    ~a~a  ~a"
                             (command-name cmd)
                             ;; TODO
                             (make-string (fx- 10 len) #\space)
                             (command-help cmd))))
                cmds)
           (newline))
         (hashtable-keys ht-cmds) (hashtable-values ht-cmds)))

      ;; top cmd
      (when (= 1 (length cmds-seen))
        (let ([cmd (car cmds-seen)])
          (print-overview cmd)
          (print-author-and-version cmd)
          (print-copyright cmd)))
      (print-usage cmds-seen) (newline)
      ;; cmds-seen: (cmd0 cmd1 cmd2 ...)
      (if commands?
          (print-all-commands)
          (print-options-and-commands))))


  (define-who print-usage
    (lambda (cmds-seen)
      (define (get/make-positional-value-name opt)
        (let ([vn (option-value-name opt)]
              [tail (if (option-sink? opt) " ..." "")])
          ;; TODO make sure `vn` is like <XXX>
          (string-append (if vn
                             vn
                             (string-append "<"
                                            (string-upcase (symbol->string (option-name opt)))
                                            ">"))
                         tail)))
      (assert (fx> (length cmds-seen) 0))
      ;; Usage: CMD0 COMMAND
      ;; Usage: CMD0 [OPTIONS] CMD1 [OPTIONS] <POS0> <POS1> ...
      ;; For more info, run 'CMD0 CMD1 --help'
      ;;(println ">>> print-help-and-quit ~a" (map command-name cmds-seen))
      (display "Usage: ")
      (let loop ([cmds cmds-seen])
        (unless (null? cmds)
          (let ([cmd (car cmds)])
            (print "~a " (command-name cmd))
            (unless (null? ((command-options cmd)))
              (print "[OPTIONS] "))
            (let* ([positionals (filter option-positional? ((command-options cmd)))])
              (if (null? positionals)
                  (if (and (not (null? ((command-subcommands cmd))))
                           (null? (cdr cmds)))
                      (print "COMMAND "))
                  (begin (assert (null? ((command-subcommands cmd))))
                         (let ([value-names (map get/make-positional-value-name positionals)])
                           (for-each (lambda (vn) (print "~a " vn)) value-names)))))
            (loop (cdr cmds)))))
      (newline)))

  )
