(import (chezpp))

(define capture-output
  (lambda (thunk)
    (let ([v #f])
      (let ([out (call-with-string-output-port
                   (lambda (port)
                     (parameterize ([current-output-port port])
                       (set! v (thunk)))))])
        (cons v out)))))

(define output-contains?
  (lambda (str needle)
    (if (string-search str needle) #t #f)))

(define silent
  (lambda (thunk)
    (let ([v #f])
      (let ([out (call-with-string-output-port
                   (lambda (port)
                     (parameterize ([current-output-port port])
                       (set! v (thunk)))))])
        (and (string=? "" out) v)))))



;;===----------------------------------------------------------------------===
;; procedural interface
;;===----------------------------------------------------------------------===


(mat cli-procedural-p

     (begin
       (define cmd (make-command 'prog))
       (define opt-verbose (make-option 'verbose))
       (option-short-set! opt-verbose "v")
       (option-value-number-set! opt-verbose 0)
       (define opt-out (make-option 'out))
       (option-short-set! opt-out "o")
       (option-default-set! opt-out "a.out")
       (option-value-parser-set! opt-out parser-string)
       (define opt-input (make-option 'input))
       (option-positional?-set! opt-input #t)
       (command-options-add! cmd opt-verbose opt-out opt-input)
       (command-exec-set! cmd
         (lambda (ref)
           (list (ref 'verbose) (ref 'out) (ref 'input))))
       #t)

     (equal? (silent (lambda () (run-cli-command! cmd '("main.ss"))))
             '(#f "a.out" "main.ss"))
     (equal? (silent (lambda () (run-cli-command! cmd '("-v" "--out=prog" "main.ss"))))
             '(#t "prog" "main.ss"))
     (error? (run-cli-command! cmd '("--out" "prog" "main.ss")))
     (error? (run-cli-command! cmd '("--verbose=true" "main.ss")))
     (error? (run-cli-command! cmd '())))


(mat cli-values-p

     (begin
       (define cmd (make-command 'prog))
       (define opt-include (make-option 'include))
       (option-short-set! opt-include "I")
       (option-value-parser-set! opt-include parser-string)
       (option-number-set! opt-include '*)
       (option-value-number-set! opt-include '+)
       (define opt-mode (make-option 'mode))
       (option-value-parser-set! opt-mode parser-string)
       (option-number-set! opt-mode '?)
       (option-default-set! opt-mode "debug")
       (define opt-file (make-option 'file))
       (option-positional?-set! opt-file #t)
       (define opt-args (make-option 'args))
       (option-positional?-set! opt-args #t)
       (option-sink?-set! opt-args #t)
       (command-options-add! cmd opt-include opt-mode opt-file opt-args)
       (command-exec-set! cmd
         (lambda (ref)
           (list (ref 'include) (ref 'mode) (ref 'file) (ref 'args))))
       #t)

     (equal? (silent (lambda () (run-cli-command! cmd '("-I=a,b" "--include=c" "main.ss" "--" "-x" "y"))))
             '(("a" "b" "c") "debug" "main.ss" ("-x" "y")))
     (equal? (silent (lambda () (run-cli-command! cmd '("--mode=release" "main.ss" "x"))))
             '(#f "release" "main.ss" ("x")))
     (error? (run-cli-command! cmd '("--mode" "release" "main.ss")))
     (error? (run-cli-command! cmd '("-I=" "main.ss"))))


(mat cli-parsers-p

     (begin
       (define color-parser (parser-enum '[red :value 1] '[green :value 2] '[blue :value 3]))
       (define cmd (make-command 'prog))
       (define opt-n (make-option 'n))
       (option-value-parser-set! opt-n parser-natural)
       (define opt-i (make-option 'i))
       (option-value-parser-set! opt-i parser-integer)
       (define opt-fx (make-option 'fx))
       (option-value-parser-set! opt-fx parser-fixnum)
       (define opt-fl (make-option 'fl))
       (option-value-parser-set! opt-fl parser-flonum)
       (define opt-color (make-option 'color))
       (option-value-parser-set! opt-color color-parser)
       (command-options-add! cmd opt-n opt-i opt-fx opt-fl opt-color)
       (command-exec-set! cmd
         (lambda (ref)
           (list (ref 'n) (ref 'i) (ref 'fx) (ref 'fl) (ref 'color))))
       #t)

     (equal? (silent (lambda () (run-cli-command! cmd '("--n=7" "--i=-2" "--fx=3" "--fl=1.5" "--color=green"))))
             '(7 -2 3 1.5 2))
     (error? (run-cli-command! cmd '("--n=-1" "--i=-2" "--fx=3" "--fl=1.5" "--color=green")))
     (error? (run-cli-command! cmd '("--n=7" "--i=x" "--fx=3" "--fl=1.5" "--color=green")))
     (error? (run-cli-command! cmd '("--n=7" "--i=-2" "--fx=3.2" "--fl=1.5" "--color=green")))
     (error? (run-cli-command! cmd '("--n=7" "--i=-2" "--fx=3" "--fl=x" "--color=green")))
     (error? (run-cli-command! cmd '("--n=7" "--i=-2" "--fx=3" "--fl=1.5" "--color=yellow"))))


(mat cli-relations-p

     (begin
       (define cmd (make-command 'prog))
       (define opt-a (make-option 'a))
       (define opt-b (make-option 'b))
       (define opt-c (make-option 'c))
       (option-value-number-set! opt-a 0)
       (option-value-number-set! opt-b 0)
       (option-value-number-set! opt-c 0)
       (option-requires-set! opt-a '(b))
       (option-conflicts-set! opt-b '(c))
       (option-overrides-set! opt-c '(a b))
       (command-options-add! cmd opt-a opt-b opt-c)
       (command-exec-set! cmd (lambda (ref) (list (ref 'a) (ref 'b) (ref 'c))))
       #t)

     (equal? (silent (lambda () (run-cli-command! cmd '("-a" "-b"))))
             '(#t #t #f))
     (equal? (silent (lambda () (run-cli-command! cmd '("-a" "-b" "-c"))))
             '(#f #f #t))
     (error? (run-cli-command! cmd '("-a")))
     (equal? (silent (lambda () (run-cli-command! cmd '("-b" "-c"))))
             '(#f #f #t)))


(mat cli-subcommands-p

     (begin
       (define seen '())
       (define root (make-command 'prog))
       (define opt-config (make-option 'config))
       (option-value-parser-set! opt-config parser-string)
       (option-default-set! opt-config "default.cfg")
       (command-options-add! root opt-config)
       (command-exec-set! root
         (lambda (ref)
           (set! seen (cons (list 'root (ref 'config)) seen))
           'root-result))

       (define build (make-command 'build))
       (define opt-release (make-option 'release))
       (option-value-number-set! opt-release 0)
       (command-options-add! build opt-release)
       (command-exec-set! build
         (lambda (ref)
           (set! seen (cons (list 'build (ref 'config) (ref 'release)) seen))
           (reverse seen)))

       (command-subcommands-add! root build)
       #t)

     (equal? (silent (lambda () (run-cli-command! root '("--config=app.cfg" "build" "--release"))))
             '((root "app.cfg") (build "app.cfg" #t)))
     (error? (run-cli-command! root '("unknown")))
     (error? (run-cli-command! root '("--release" "build"))))


(mat cli-help-p

     (begin
       (define ran? #f)
       (define root (make-command 'prog))
       (command-overview-set! root "Program overview")
       (command-version-set! root "1.0.0")
       (command-author-set! root "Maoif")
       (define opt-hidden (make-option 'secret))
       (option-hidden?-set! opt-hidden #t)
       (define sub (make-command 'run))
       (command-help-set! sub "Run the program")
       (command-options-add! root opt-hidden)
       (command-subcommands-add! root sub)
       (command-exec-set! root (lambda (ref) (set! ran? #t)))
       #t)

     (let ([ans (capture-output (lambda () (run-cli-command! root '("--help"))))])
       (and (not ran?)
            (output-contains? (cdr ans) "Usage: prog")
            (output-contains? (cdr ans) "COMMANDS")
            (not (output-contains? (cdr ans) "--secret"))))
     (let ([ans (capture-output (lambda () (run-cli-command! root '("--help-all"))))])
       (output-contains? (cdr ans) "--secret"))
     (let ([ans (capture-output (lambda () (run-cli-command! root '("--help-commands"))))])
       (and (output-contains? (cdr ans) "prog")
            (output-contains? (cdr ans) "run"))))



;;===----------------------------------------------------------------------===
;; syntactic interface
;;===----------------------------------------------------------------------===


(mat cli-macro-p

     (begin
       (define color-parser
         (cli-enum [red :value 1] [green :value 2]))
       (define common-options
         (cli-option-group
          [verbose :short #\v]
          [color :value-parser color-parser :default 1]))
       (define cmd
         (cli-command prog
           :overview "Macro program"
           :options common-options
           :subcommands
           [run
            :exec (lambda (ref) (list (ref 'verbose) (ref 'color)))
            :options
            [input :positional]]))
       #t)

     (equal? (silent (lambda () (run-cli-command! cmd '("-v" "--color=green" "run" "file"))))
             '(#t 2)))
