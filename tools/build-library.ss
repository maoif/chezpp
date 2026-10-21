(import (chezscheme))

;; Run from the repository root: key=value options, followed by --.
(load "tools/compiler-options.ss")

(define arguments (command-line-arguments))
(let-values ([(options ignored-sources) (read-build-arguments arguments)])
  (call-with-compiler-options options
    (lambda ()
      (let* ([wpo (compiler-option options 'wpo)]
             [compile-with-options
              (lambda (thunk)
                (call-with-compiler-options options
                  (lambda ()
                    ;; Whole-library compilation needs intermediate WPO files even with wpo=f.
                    (parameterize ([generate-wpo-files
                                    (if (equal? wpo "f") #t (generate-wpo-files))])
                      (thunk)))))])
        (parameterize
            ([compile-file-message #t]
             ;; Chez recursively compiles every library reachable from src/cspp.ss.
             [compile-imported-libraries #t])
          (compile-with-options
           (lambda () (time ((compile-library-handler) "src/cspp.ss" "src/cspp.so"))))
          (compile-with-options
           (lambda ()
             (let ([dependencies (compile-whole-library "src/cspp.wpo" "cspp.lib")])
               (when (and (equal? wpo "t") (not (null? dependencies)))
                 (errorf "cspp.lib" "dependency has to be null"))))))))))
