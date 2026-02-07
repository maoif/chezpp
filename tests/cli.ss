(import (chezpp))


;;===----------------------------------------------------------------------===
;; procedural interface
;;===----------------------------------------------------------------------===


(mat simple-p

     ;; ./prog <ARGS> ...
     (begin (define (main-prog res-proc)
              (println "args: ~s" (res-proc 'args))
              (res-proc 'args))

            (define cmd-prog (make-command 'prog))
            (command-overview-set! cmd-prog "some test program")
            (command-version-set!  cmd-prog "1.0.0")
            (command-author-set!   cmd-prog "maoif")
            (command-help-set!     cmd-prog "this program does fancy things")
            (command-exec-set!     cmd-prog main-prog)

            (define opt-args (make-option 'args))
            (option-positional?-set!  opt-args #t)
            (option-sink?-set!        opt-args #t)
            (option-value-parser-set! opt-args parser-string)

            (command-options-add! cmd-prog opt-args)
            #t)

     (error? (run-cli-command! cmd-prog '()))
     (equal? (run-cli-command! cmd-prog '("a0"))
             '("a0"))
     (equal? (run-cli-command! cmd-prog '("a0" "a1"))
             '("a0" "a1"))
     (equal? (run-cli-command! cmd-prog '("a0" "a1" "a2"))
             '("a0" "a1" "a2"))
     (equal? (run-cli-command! cmd-prog '("a0" "a1" "a2" "a3" "a4" "a5"))
             '("a0" "a1" "a2" "a3" "a4" "a5"))

     ;; ./prog [OPTIONS]
     (begin (define (main-prog res-proc)
              (println "opt0: ~s" (res-proc 'opt0))
              (println "opt1: ~s" (res-proc 'opt1))
              (list (res-proc 'opt0) (res-proc 'opt1)))

            (define cmd-prog (make-command 'prog))
            (command-overview-set! cmd-prog "some test program")
            (command-version-set!  cmd-prog "1.0.0")
            (command-author-set!   cmd-prog "maoif")
            (command-help-set!     cmd-prog "this program does fancy things")
            (command-exec-set!     cmd-prog main-prog)

            (define opt0 (make-option 'opt0))
            (option-short-set! opt0 "o")
            (option-long-set!  opt0 "opt0")
            (option-default-set! opt0 "default0")
            (option-value-parser-set! opt0 parser-string)

            (define opt1 (make-option 'opt1))
            (option-short-set! opt1 "O")
            (option-long-set!  opt1 "opt1")
            (option-default-set! opt1 "default1")
            (option-value-parser-set! opt1 parser-string)

            (command-options-add! cmd-prog opt0 opt1)
            #t)

     (equal? (run-cli-command! cmd-prog '())
             (list "default0" "default1"))
     (equal? (run-cli-command! cmd-prog '("-o=x"))
             (list "x" "default1"))
     (equal? (run-cli-command! cmd-prog '("-O=y"))
             (list "default0" "y"))
     (equal? (run-cli-command! cmd-prog '("--opt0=xxx" "--opt1=yyy"))
             (list "xxx" "yyy"))
     (equal? (run-cli-command! cmd-prog '("--opt0=xxx"))
             (list "xxx" "default1"))
     (equal? (run-cli-command! cmd-prog '("--opt1=yyy"))
             (list "default0" "yyy"))


     ;; ./prog <ARG0> <ARG1> <ARG2>
     (begin (define (main-prog res-proc)
              (println "arg0: ~s" (res-proc 'arg0))
              (println "arg1: ~s" (res-proc 'arg1))
              (println "args: ~s" (res-proc 'arg2))
              (list (res-proc 'arg0) (res-proc 'arg1) (res-proc 'arg2)))

            (define cmd-prog (make-command 'prog))
            (command-overview-set! cmd-prog "some test program")
            (command-exec-set!     cmd-prog main-prog)

            (define opt-arg0 (make-option 'arg0))
            (option-positional?-set!  opt-arg0 #t)
            (option-value-parser-set! opt-arg0 parser-string)

            (define opt-arg1 (make-option 'arg1))
            (option-positional?-set!  opt-arg1 #t)
            (option-value-parser-set! opt-arg1 parser-string)

            (define opt-arg2 (make-option 'arg2))
            (option-positional?-set!  opt-arg2 #t)
            (option-value-parser-set! opt-arg2 parser-string)

            (command-options-add! cmd-prog opt-arg0 opt-arg1 opt-arg2)
            #t)

     (error? (run-cli-command! cmd-prog '()))
     (error? (run-cli-command! cmd-prog '("a0")))
     (error? (run-cli-command! cmd-prog '("a0" "a1")))
     (error? (run-cli-command! cmd-prog '("a0" "a1" "a2" "a3")))

     (equal? (run-cli-command! cmd-prog '("a0" "a1" "a2"))
             '("a0" "a1" "a2"))


     ;; ./prog <ARG0> <ARG1> <ARGS> ...
     (begin (define (main-prog res-proc)
              (println "arg0: ~s" (res-proc 'arg0))
              (println "arg1: ~s" (res-proc 'arg1))
              (println "args: ~s" (res-proc 'args))
              (list (res-proc 'arg0) (res-proc 'arg1) (res-proc 'args)))

            (define cmd-prog (make-command 'prog))
            (command-overview-set! cmd-prog "some test program")
            (command-exec-set!     cmd-prog main-prog)

            (define opt-arg0 (make-option 'arg0))
            (option-positional?-set!  opt-arg0 #t)
            (option-value-parser-set! opt-arg0 parser-string)

            (define opt-arg1 (make-option 'arg1))
            (option-positional?-set!  opt-arg1 #t)
            (option-value-parser-set! opt-arg1 parser-string)

            (define opt-args (make-option 'args))
            (option-positional?-set!  opt-args #t)
            (option-sink?-set!        opt-args #t)
            (option-value-parser-set! opt-args parser-string)

            (command-options-add! cmd-prog opt-arg0 opt-arg1 opt-args)
            #t)

     (error? (run-cli-command! cmd-prog '()))
     (error? (run-cli-command! cmd-prog '("a0")))
     (error? (run-cli-command! cmd-prog '("a0" "a1")))
     (equal? (run-cli-command! cmd-prog '("a0" "a1" "a2"))
             (list "a0" "a1" '("a2")))
     (equal? (run-cli-command! cmd-prog '("a0" "a1" "a2" "a3" "a4" "a5"))
             (list "a0" "a1" '("a2" "a3" "a4" "a5")))

     ;; ./prog [OPTIONS] <ARG0> <ARG1> <ARGS> ...
     (begin (define (main-prog res-proc)
              (println "foo  ~s" (res-proc 'foo))
              (println "bar  ~s" (res-proc 'bar))
              (println "arg0: ~s" (res-proc 'arg0))
              (println "arg1: ~s" (res-proc 'arg1))
              (println "args: ~s" (res-proc 'args))
              (list (res-proc 'foo) (res-proc 'bar) (res-proc 'arg0) (res-proc 'arg1) (res-proc 'args)))

            (define cmd-prog (make-command 'prog))
            (command-overview-set! cmd-prog "some test program")
            (command-exec-set!     cmd-prog main-prog)

            (define opt0 (make-option 'foo))
            (option-short-set! opt0 "f")
            (option-long-set!  opt0 "foo")
            (option-default-set! opt0 "default0")
            (option-value-parser-set! opt0 parser-string)

            (define opt1 (make-option 'bar))
            (option-short-set! opt1 "b")
            (option-long-set!  opt1 "bar")
            (option-default-set! opt1 "default1")
            (option-value-parser-set! opt1 parser-string)

            (define opt-arg0 (make-option 'arg0))
            (option-positional?-set!  opt-arg0 #t)
            (option-value-parser-set! opt-arg0 parser-string)

            (define opt-arg1 (make-option 'arg1))
            (option-positional?-set!  opt-arg1 #t)
            (option-value-parser-set! opt-arg1 parser-string)

            (define opt-args (make-option 'args))
            (option-positional?-set!  opt-args #t)
            (option-sink?-set!        opt-args #t)
            (option-value-parser-set! opt-args parser-string)

            (command-options-add! cmd-prog opt0 opt1 opt-arg0 opt-arg1 opt-args)
            #t)

     (error? (run-cli-command! cmd-prog '()))
     (error? (run-cli-command! cmd-prog '("a0" "a1")))
     (equal? (run-cli-command! cmd-prog '("a0" "a1" "a2"))
             (list "default0" "default1" "a0" "a1" '("a2")))
     (equal? (run-cli-command! cmd-prog '("a0" "a1" "a2" "a3" "a4"))
             (list "default0" "default1" "a0" "a1" '("a2" "a3" "a4")))

     ;; TODO make it error
     (error? (run-cli-command! cmd-prog '("-f" "a0" "a1")))
     ;;(run-cli-command! cmd-prog '("-f" "a0" "a1" "a2"))
     ;;(run-cli-command! cmd-prog '("-b" "a0" "a1"))
     ;;(run-cli-command! cmd-prog '("-f -b a0" "a1" "a2"))
     (error? (run-cli-command! cmd-prog '("-f=few" "a0" "a1")))
     (equal? (run-cli-command! cmd-prog '("-f=few" "a0" "a1" "a2"))
             (list "few" "default1" "a0" "a1" '("a2")))
     (error? (run-cli-command! cmd-prog '("-b=baz" "a0" "a1")))
     (equal? (run-cli-command! cmd-prog '("-b=baz" "a0" "a1" "a2"))
             (list "default0" "baz" "a0" "a1" '("a2")))

     ;; TODO foo takes value?
     ;;(run-cli-command! cmd-prog '("--foo" "a0" "a1" "a2"))
     ;;(run-cli-command! cmd-prog '("--bar" "a0" "a1" "a2"))
     (equal? (run-cli-command! cmd-prog '("--foo=few" "a0" "a1" "a2"))
             (list "few" "default1" "a0" "a1" '("a2")))
     (equal? (run-cli-command! cmd-prog '("--bar=baz" "a0" "a1" "a2"))
             (list "default0" "baz" "a0" "a1" '("a2")))
     (equal? (run-cli-command! cmd-prog '("--foo=few" "--bar=baz" "a0" "a1" "a2" "a3"))
             (list "few" "baz" "a0" "a1" '("a2" "a3")))
     (equal? (run-cli-command! cmd-prog '("--bar=baz" "--foo=few" "a0" "a1" "a2" "a3"))
             (list "few" "baz" "a0" "a1" '("a2" "a3")))

     (error? (run-cli-command! cmd-prog '("--bar=baz" "--foo=few" "a0" "a1" "a2" "--" "a3")))
     (error? (run-cli-command! cmd-prog '("--bar=baz" "--foo=few" "a0" "--" "a1" "a2" "a3")))
     (equal? (run-cli-command! cmd-prog '("--bar=baz" "--foo=few" "a0" "a1" "--" "a2" "a3"))
             (list "few" "baz" "a0" "a1" '("a2" "a3")))

     )


(mat value-number-p

     ;; ./prog [OPTIONS] <ARG0> <ARG1> <ARGS> ...
     (begin (define (main-prog res-proc)
              (println "foo  ~s" (res-proc 'foo))
              (println "bar  ~s" (res-proc 'bar))
              (println "arg0: ~s" (res-proc 'arg0))
              (println "arg1: ~s" (res-proc 'arg1))
              (println "args: ~s" (res-proc 'args))
              (list (res-proc 'foo) (res-proc 'bar)
                    (res-proc 'options)
                    (res-proc 'passes)
                    (res-proc 'mode)
                    (res-proc 'arg0) (res-proc 'arg1) (res-proc 'args)))

            (define cmd-prog (make-command 'prog))
            (command-overview-set! cmd-prog "some test program")
            (command-exec-set!     cmd-prog main-prog)

            (define opt0 (make-option 'foo))
            (option-short-set! opt0 "f")
            (option-long-set!  opt0 "foo")
            (option-default-set! opt0 "default0")
            (option-value-parser-set! opt0 parser-string)

            (define opt1 (make-option 'bar))
            (option-short-set! opt1 "b")
            (option-long-set!  opt1 "bar")
            (option-value-number-set! opt1 '1) ;; must have one value
            (option-value-parser-set! opt1 parser-string)

            (define opt3 (make-option 'options))
            (option-short-set! opt3 "O")
            (option-long-set!  opt3 "options")
            (option-number-set! opt3 '*)
            (option-value-number-set! opt3 '+) ;; one or more values
            (option-value-parser-set! opt3 parser-string)

            (define opt4 (make-option 'passes))
            (option-short-set! opt4 "p")
            (option-long-set!  opt4 "passes")
            (option-number-set! opt4 '+)        ;; must appear once or more
            (option-value-number-set! opt4 '*)  ;; zero or more values
            (option-value-parser-set! opt4 parser-string)

            (define opt5 (make-option 'mode))
            (option-short-set! opt5 "m")
            (option-long-set!  opt5 "mode")
            (option-default-set! opt5 "mode0")
            (option-number-set!   opt5 '?)
            (option-value-number-set! opt5 '1)
            (option-value-parser-set! opt5 parser-string)


            (define opt-arg0 (make-option 'arg0))
            (option-positional?-set!  opt-arg0 #t)
            (option-value-parser-set! opt-arg0 parser-string)

            (define opt-arg1 (make-option 'arg1))
            (option-positional?-set!  opt-arg1 #t)
            (option-value-number-set! opt-arg1 '+)
            (option-value-parser-set! opt-arg1 parser-string)

            (define opt-args (make-option 'args))
            (option-positional?-set!  opt-args #t)
            (option-sink?-set!        opt-args #t)
            (option-value-parser-set! opt-args parser-string)

            (command-options-add! cmd-prog opt0 opt1 opt3 opt4 opt5 opt-arg0 opt-arg1 opt-args)
            #t)

     (error? (run-cli-command! cmd-prog '()))

     ;; -b has no value
     (error? (run-cli-command! cmd-prog '("-b")))
     ;; -p/--passes not given
     (error? (run-cli-command! cmd-prog '("-b=bbb")))
     ;; -m has no value
     (error? (run-cli-command! cmd-prog '("-b=bbb" "--passes=a,b,c,d" "-m" "p0" "p1" "p2")))
     (error? (run-cli-command! cmd-prog '("-b=bbb" "--passes=a,b,c,d" "--mode" "p0" "p1" "p2")))

     (equal? (run-cli-command! cmd-prog '("-b=bbb" "-p=a,b,c,d" "p0" "p1" "p2"))
             '("default0" "bbb" #f ("a" "b" "c" "d") "mode0" "p0" ("p1") ("p2")))
     (equal? (run-cli-command! cmd-prog '("-b=bbb" "--passes=a,b,c,d" "p0" "p1" "p2"))
             '("default0" "bbb" #f ("a" "b" "c" "d") "mode0" "p0" ("p1")
               ("p2")))
     (equal? (run-cli-command! cmd-prog '("-b=bbb" "--passes=a,b,c,d" "--passes=ww,xx,yy,zz" "p0" "p1" "p2"))
             '("default0" "bbb" #f ("a" "b" "c" "d" "ww" "xx" "yy" "zz") "mode0" "p0" ("p1") ("p2")))
     (equal? (run-cli-command! cmd-prog '("-b=bbb" "--passes=a,b,c,d" "p0" "p1,1,2,3" "p2"))
             '("default0" "bbb" #f ("a" "b" "c" "d") "mode0" "p0" ("p1" "1" "2" "3") ("p2")))
     (equal? (run-cli-command! cmd-prog '("-m=mode1" "-b=bbb" "--passes=a,b,c,d" "p0" "p1,1,2,3" "p2"))
             '("default0" "bbb" #f ("a" "b" "c" "d") "mode1" "p0" ("p1" "1" "2" "3") ("p2")))
     (equal? (run-cli-command! cmd-prog '("--mode=mode1" "--bar=bbb" "--passes=a,b,c,d" "p0" "p1,1,2,3" "p2"))
             '("default0" "bbb" #f ("a" "b" "c" "d") "mode1" "p0" ("p1" "1" "2" "3") ("p2")))

     )


(mat simple-subcmd-p

     ;; TODO
     ;; ./prog <COMMAND>, commands: cmd1, cmd2
     ;; ./prog cmd1 <ARGS> ...
     ;; ./prog cmd2 <COMMAND>, commands: cmd2-1, cmd2-2
     ;; ./prog cmd2 cmd2-1 -f/--foo
     ;; ./prog cmd2 cmd2-2 -b/--bar
     (begin (define (main-prog res-proc)
              (println "args: ~s" (res-proc 'args))
              (res-proc 'args))
            (define (exec-cmd1 res-proc)
              (println "cmd1 args: ~s" (res-proc 'args))
              (list (res-proc 'args)))
            (define (exec-cmd2-1 res-proc)
              (println "cmd2-1 args: ~s" (res-proc 'foo))
              (list (res-proc 'foo)))
            (define (exec-cmd2-2 res-proc)
              (println "cmd2-2 args: ~s" (res-proc 'bar))
              (list (res-proc 'bar)))

            (define cmd-prog (make-command 'prog))
            (command-overview-set! cmd-prog "some test program")
            ;;(command-exec-set!     cmd-prog main-prog)

            (define cmd1 (make-command 'cmd1))
            (command-exec-set! cmd1 exec-cmd1)

            (define opt-args (make-option 'args))
            (option-positional?-set!  opt-args #t)
            (option-sink?-set!        opt-args #t)
            (option-value-parser-set! opt-args parser-string)
            (command-options-add! cmd1 opt-args)

            (define cmd2 (make-command 'cmd2))

            (define cmd2-1 (make-command 'cmd2-1))
            (command-exec-set! cmd2-1 exec-cmd2-1)

            (define opt0 (make-option 'foo))
            (option-short-set! opt0 "f")
            (option-long-set!  opt0 "foo")
            (option-default-set! opt0 "default0")
            (option-value-parser-set! opt0 parser-string)
            (command-options-add! cmd2-1 opt0)

            (define cmd2-2 (make-command 'cmd2-2))
            (command-exec-set! cmd2-2 exec-cmd2-2)

            (define opt1 (make-option 'bar))
            (option-short-set! opt1 "b")
            (option-long-set!  opt1 "bar")
            (option-default-set! opt1 "default1")
            (option-value-parser-set! opt1 parser-string)
            (command-options-add! cmd2-2 opt1)

            (command-subcommands-add! cmd2 cmd2-1 cmd2-2)
            (command-subcommands-add! cmd-prog cmd1 cmd2)

            #t)

     (error? (run-cli-command! cmd-prog '()))
     ;; no positional arg
     (error? (run-cli-command! cmd-prog '("cmd1")))
     ;; unknown cmd
     (error? (run-cli-command! cmd-prog '("cmdX")))
     ;; unknown subcmd
     (error? (run-cli-command! cmd-prog '("cmd2" "cmdX")))

     (equal? (run-cli-command! cmd-prog '("cmd1" "a1"))
             (list '("a1")))
     (equal? (run-cli-command! cmd-prog '("cmd1" "a1" "a2" "a3"))
             (list '("a1" "a2" "a3")))

     ;; expect subcommand
     (error? (run-cli-command! cmd-prog '("cmd2")))

     (equal? (run-cli-command! cmd-prog '("cmd2" "cmd2-1"))
             (list "default0"))
     (equal? (run-cli-command! cmd-prog '("cmd2" "cmd2-1" "-f=foo"))
             (list "foo"))
     (equal? (run-cli-command! cmd-prog '("cmd2" "cmd2-1" "--foo=foo"))
             (list "foo"))
     (equal? (run-cli-command! cmd-prog '("cmd2" "cmd2-2"))
             (list "default1"))
     (equal? (run-cli-command! cmd-prog '("cmd2" "cmd2-2" "-b=bar"))
             (list "bar"))
     (equal? (run-cli-command! cmd-prog '("cmd2" "cmd2-2" "--bar=bar"))
             (list "bar"))

     (error? (run-cli-command! cmd-prog '("cmd2" "cmd2-1" "-x=foo")))
     (error? (run-cli-command! cmd-prog '("cmd2" "cmd2-1" "--xoo=foo")))
     (error? (run-cli-command! cmd-prog '("cmd2" "cmd2-2" "-x=bar")))
     (error? (run-cli-command! cmd-prog '("cmd2" "cmd2-2" "--xar=bar")))

     '(begin (define (docker-run res-proc)
               (displayln (res-proc 'memory))
               (displayln (res-proc 'env))
               (displayln (res-proc 'interactive))
               (todo))
             (define (docker-image-ls res-proc)
               (displayln (res-proc 'all))
               (displayln (res-proc 'digest))
               (displayln (res-proc 'filter))
               (todo))
             (define (docker-image-build res-proc)
               (displayln (res-proc 'file))
               (displayln (res-proc 'memory))
               (displayln (res-proc 'compress))
               (todo))
             (define (docker-image-pull res-proc)
               (displayln (res-proc 'all-tags))
               (displayln (res-proc 'quiet))
               (todo))

             (define cmdcat  "Common Commands")
             (define cmdcat2 "Less Common Commands")

             (define cmd-docker (make-command 'docker))
             (command-overview-set! cmd-docker "docker container")
             (command-version-set!  cmd-docker "1.0.0")
             (command-author-set!   cmd-docker "maoif")
             (let ([opt-config (make-option 'config)]
                   [opt-debug  (make-option 'debug)])
               (option-help-set! opt-config "Location of client config files")
               (option-help-set! opt-debug  "Enable debug mode")

               (option-short-set! opt-config #f)
               (option-short-set! opt-debug  #\D)

               (option-value-parser-set! opt-config parser-string)
               (option-value-number-set! opt-config '1)

               (command-options-add! cmd-docker opt-config opt-debug))

             ;; Usage:  docker run [OPTIONS] IMAGE [COMMAND] [ARG...]
             (define cmd-docker-run (make-command 'run))
             (command-subcommands-add! cmd-docker cmd-docker-run)
             (command-help-set!     cmd-docker-run "Create and run a new container from an image")
             (command-category-set! cmd-docker-run cmdcat)
             (command-exec-set!     cmd-docker-run docker-run)
             (let ([opt-memory (make-option 'memory)]
                   [opt-env    (make-option 'env)]
                   [opt-interactive (make-option 'interactive)])
               (option-help-set! opt-memory "Memory limit")
               (option-help-set! opt-env    "Set environment variables")
               (option-help-set! opt-interactive "Keep STDIN open even if not attached")

               (option-value-parser-set! opt-memory parser-natural)
               (option-value-parser-set! opt-env    parser-string)

               (option-value-name-set! opt-memory "bytes")

               (command-options-add! cmd-docker-run opt-memory opt-env opt-interactive))

             ;; Usage:  docker image COMMAND
             (define cmd-docker-image (make-command 'image))
             (command-subcommands-add! cmd-docker cmd-docker-image)
             (command-help-set!     cmd-docker-image "Manage images")
             (command-category-set! cmd-docker-image cmdcat2)

             (define cmd-docker-image-ls (make-command 'ls))
             (define cmd-docker-image-build (make-command 'build))
             (define cmd-docker-image-pull (make-command 'pull))
             (command-subcommands-add! cmd-docker-image
                                       cmd-docker-image-ls cmd-docker-image-build cmd-docker-image-pull)

             (command-help-set! cmd-docker-image-ls "Manage images")
             (command-exec-set! cmd-docker-image-ls docker-image-ls)
             (let ([opt-all    (make-option 'all)]
                   [opt-digest (make-option 'digest)]
                   [opt-filter (make-option 'filter)])
               (option-help-set! opt-all    "Show all containers (default shows just running)")
               (option-help-set! opt-digest "Show digests")
               (option-help-set! opt-filter "Filter output based on conditions provided")

               (option-short-set! opt-digest #f)

               (option-value-parser-set! opt-filter parser-string)
               (option-value-number-set! opt-filter '1)

               (command-options-add! cmd-docker-image-ls opt-all opt-digest opt-filter))

             (command-help-set! cmd-docker-image-build "Build an image from a Dockerfile")
             (command-exec-set! cmd-docker-image-build docker-image-build)
             (let ([opt-file     (make-option 'file)]
                   [opt-memory   (make-option 'memory)]
                   [opt-compress (make-option 'compress)])
               (option-help-set! opt-file     "Name of the Dockerfile (Default is \"./Dockerfile\")")
               (option-help-set! opt-memory   "Memory limit")
               (option-help-set! opt-compress "Compress the build context using gzip")

               (option-value-parser-set! opt-file   parser-string)
               (option-value-parser-set! opt-memory parser-natural)

               (option-value-name-set! opt-memory "bytes")

               (command-options-add! cmd-docker-image-build opt-file opt-memory opt-compress))

             (command-help-set! cmd-docker-image-pull "Download an image from a registry")
             (command-exec-set! cmd-docker-image-pull docker-image-pull)
             (let ([opt-all-tags (make-option 'all-tags)]
                   [opt-quiet    (make-option 'quiet)])
               (option-help-set! opt-all-tags "Download all tagged images in the repository")
               (option-help-set! opt-quiet    "Suppress verbose output")

               (command-options-add! cmd-docker-image-pull opt-all-tags opt-quiet))

             #t)

     '(run-cli-command! cmd-docker)

     )
#!eof

;;===----------------------------------------------------------------------===
;; syntactic interface
;;===----------------------------------------------------------------------===


(mat simple-s

     (begin (define (main-prog res-proc)
              (println (res-proc 'args))
              (todo))

            (cli-command cmd-prog
                         :overview "some test program"
                         :version "1.0.0"
                         :author "maoif"
                         :exec main-prog
                         :options
                         [args
                          :positional
                          :sink
                          :value-parser string])
            #t)

     (run-cli-command! cmd-prog)

     )

(mat simple-subcmd-s

     (begin (define (docker-image-pull res-proc)
              (todo))
            (define (docker-run res-proc)
              (displayln (res-proc 'memory))
              (displayln (res-proc 'env))
              (displayln (res-proc 'interactive))
              (todo))
            (define (docker-image-ls res-proc)
              (displayln (res-proc 'all))
              (displayln (res-proc 'digest))
              (displayln (res-proc 'filter))
              (todo))
            (define (docker-image-build res-proc)
              (displayln (res-proc 'file))
              (displayln (res-proc 'memory))
              (displayln (res-proc 'compress))
              (todo))
            (define (docker-image-pull res-proc)
              (displayln (res-proc 'all-tags))
              (displayln (res-proc 'quiet))
              (todo))

            (cli-command docker-prog
                         :overview "docker container"
                         :version "1.0.0"
                         :author "maoif"

                         :options
                         [config
                          :help "Location of client config files"
                          :no-short
                          :value-parser string
                          :value-numer 1]
                         [debug
                          :help "Enable debug mode"
                          :short #\D]

                         :subcommands
                         ;; Usage:  docker run [OPTIONS] IMAGE [COMMAND] [ARG...]
                         [run
                          :help "Create and run a new container from an image"
                          :category cmdcat1
                          :exec docker-run
                          :options
                          [memory
                           :help "Memory limit"
                           :value-parser natural
                           :value-name "bytes"]
                          [env
                           :help "Set environment variables"
                           :value-parser string]
                          [interactive
                           :help "Keep STDIN open even if not attached"]]
                         ;; Usage:  docker image COMMAND
                         [image
                          :category cmdcat2
                          :help "Manage images"
                          :subcommands
                          [ls
                           :exec docker-image-ls
                           :help "List images"
                           :options
                           [all
                            :help "Show all containers (default shows just running)"]
                           [digest
                            :help "Show digests"
                            :no-short]
                           [filter
                            :help "Filter output based on conditions provided"
                            :value-parser string
                            :value-number '1]]
                          [build
                           :exec docker-image-build
                           :help "Build an image from a Dockerfile"
                           :options
                           [file
                            :help "Name of the Dockerfile (Default is \"./Dockerfile\")"
                            :value-parser string]
                           [memory
                            :help "Memory limit"
                            :value-parser natural
                            :value-name "bytes"]
                           [compress
                            :help "Compress the build context using gzip"]]
                          [pull
                           :exec docker-image-pull
                           :help "Download an image from a registry"
                           :options
                           [all-tags
                            :help "Download all tagged images in the repository"]
                           [quiet
                            :help "Suppress verbose output"]]])

            #t)

     (run-cli-command! docker-prog)

     )


(mat csbs-s

     (begin (define)

            (cli-command cmd-csbs
                         :overview "ChezScheme Build System"
                         :version "0.0.1"
                         :author "毛奕夫"

                         :options
                         :subcommands
                         [init
                          :options
                          [exe]
                          [lib
                           :conflits exe]]
                         [build]
                         [run]
                         [test]
                         [bench]
                         [format]
                         [clean]
                         [info])))

;;;; procedural & syntactic
