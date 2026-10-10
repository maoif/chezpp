(import (chezscheme))

;; The Makefile supplies the canonical version and installs this build-only handler.
;; Transform parsed library declarations without rewriting or copying source files.
(module (make-versioned-compile-library-handler)
  (define version-list?
    (lambda (value)
      (and (list? value)
           (for-all (lambda (part) (and (integer? part) (exact? part) (>= part 0))) value))))

  (define unannotated
    (lambda (form)
      (if (annotation? form) (annotation-stripped form) form)))

  (define chezpp-library?
    (lambda (form)
      (let ([datum (unannotated form)])
        (and (list? datum)
             (>= (length datum) 2)
             (eq? (car datum) 'library)
             (pair? (cadr datum))
             (eq? (caadr datum) 'chezpp)))))

  (define versioned-library-name
    (lambda (name version)
      ;; A version is one trailing list in the library name, not additional name components.
      ;; Replace an existing version rather than appending a second version list.
      (let loop ([parts name] [symbols '()])
        (cond
         [(null? parts) (append (reverse symbols) (list version))]
         [(symbol? (car parts)) (loop (cdr parts) (cons (car parts) symbols))]
         [(and (null? (cdr parts)) (version-list? (car parts)))
          (append (reverse symbols) (list version))]
         [else (error 'versioned-compile-library "invalid library name" name)]))))

  (define rewrite-library-form
    (lambda (form version)
      (if (chezpp-library? form)
          (let* ([datum (unannotated form)]
                 [name (versioned-library-name (cadr datum) version)]
                 [expression (if (annotation? form) (annotation-expression form) form)]
                 [rewritten (cons (car expression) (cons name (cddr expression)))])
            ;; Keep body annotations tied to their original source positions.
            (if (annotation? form)
                (make-annotation rewritten (annotation-source form)
                                 (cons 'library (cons name (cddr datum))))
                rewritten))
          form)))

  (define read-source
    (lambda (source)
      ;; Compute the checksum from the original bytes, then reset before reading annotations.
      ;; Keep source positions for diagnostics, procedure inspection, and coverage output.
      (let* ([binary-port (open-file-input-port source)]
             [descriptor (make-source-file-descriptor source binary-port #t)]
             [port (transcoded-port binary-port (current-transcoder))])
        (dynamic-wind
          void
          (lambda ()
            (let loop ([forms '()] [position 0])
              (let-values ([(form next-position)
                            (get-datum/annotations port descriptor position)])
                (if (eof-object? form)
                    (values (reverse forms) descriptor)
                    (loop (cons form forms) next-position)))))
          (lambda () (close-port port))))))

  #|proc:make-versioned-compile-library-handler
  Returns a handler suitable for ChezScheme's `compile-library-handler` parameter.
  `version` is the nonempty list of nonnegative exact integers supplied by the Makefile.
  `fallback` accepts `(source output)` filenames and compiles files without Chezpp declarations.
  Optional `sources` lists source filenames whose libraries should be included in the umbrella.
  The returned handler accepts `(source output)` filenames, updates Chezpp declarations in memory,
  and compiles them to `output`. Its return value is unspecified. The two-argument constructor
  leaves the umbrella's import inventory unchanged.
  |#
  (define make-versioned-compile-library-handler
    (case-lambda
     [(version fallback) (make-versioned-compile-library-handler version fallback '())]
     [(version fallback sources)
      (unless (and (version-list? version) (pair? version))
        (error 'versioned-compile-library "invalid version" version))
      (unless (procedure? fallback)
        (error 'versioned-compile-library "invalid fallback handler" fallback))
      (lambda (source output)
        (let-values ([(forms descriptor) (read-source source)])
          (if (exists chezpp-library? forms)
              (parameterize ([source-directories
                              (cons (path-parent source) (source-directories))])
                ;; Preserve relative include lookup and let Chez emit WPO and coverage sidecars
                ;; according to the active compiler parameters, as with file compilation.
                (compile-to-file
                 (map (lambda (form)
                        (include-standalone-libraries
                         (rewrite-library-form form version) sources version))
                      forms)
                 output descriptor))
              (fallback source output))))]))

  (define include-standalone-libraries
    (lambda (form sources version)
      ;; Only the umbrella needs the full inventory; constituent imports remain unchanged.
      (if (and (chezpp-library? form)
               (equal? (cadr (unannotated form)) (list 'chezpp version)))
          (let* ([datum (unannotated form)]
                 [expression (if (annotation? form) (annotation-expression form) form)]
                 [extra-imports
                  (apply append
                    (map (lambda (source)
                           (let-values ([(forms descriptor) (read-source source)])
                             (map (lambda (library)
                                    ;; (only library) imports no bindings but records a dependency
                                    ;; so whole-library compilation also includes standalone APIs.
                                    (list 'only
                                      (reverse (cdr (reverse
                                        (versioned-library-name
                                         (cadr (unannotated library)) version))))))
                                  (filter chezpp-library? forms))))
                         sources))]
                 [original-imports (cadddr expression)]
                 [imports
                  ;; Extend the annotated expression and its stripped datum together.
                  (if (annotation? original-imports)
                      (make-annotation
                       (append (annotation-expression original-imports) extra-imports)
                       (annotation-source original-imports)
                       (append (annotation-stripped original-imports) extra-imports))
                      (append original-imports extra-imports))]
                 [rewritten (list* (car expression) (cadr expression) (caddr expression)
                                  imports (cddddr expression))])
            ;; Empty import sets compile standalone libraries without exposing their bindings.
            (if (annotation? form)
                (make-annotation rewritten (annotation-source form)
                  (list* (car datum) (cadr datum) (caddr datum)
                         (unannotated imports) (cddddr datum)))
                rewritten))
          form))))
