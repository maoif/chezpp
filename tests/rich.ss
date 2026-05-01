#!chezscheme
(import (chezpp)
        (chezpp rich private common))

(mat rich-import

     (procedure? rich-style)
     (procedure? reset-style)
     (procedure? rich-print)
     (procedure? rich-println)
     (procedure? make-rich-console)

     )

(mat rich-print-smoke

     (equal? "123 abc"
             (with-output-to-string
               (lambda ()
                 (rich-print 123 " " "abc"))))

     (equal? "abc\n"
             (with-output-to-string
               (lambda ()
                 (rich-println "abc"))))

     )

(mat rich-style

     (rich-style? (rich-style 'bold 'red))
     (rich-reset? (reset-style))

     (equal? "\033[1m\033[31m"
             (rich-style->ansi #f (rich-style 'bold 'red)))

     ;; Later foreground colors override earlier foreground colors.
     (equal? "\033[34m"
             (rich-style->ansi #f (rich-style 'red 'blue)))

     ;; Later attributes override earlier conflicting attributes.
     (equal? ""
             (rich-style->ansi #f (rich-style 'underline 'no-underline)))

     (equal? "\033[38;2;255;170;0m"
             (rich-style->ansi #f (rich-style #xffaa00)))

     (equal? "\033[48;2;32;32;32m"
             (rich-style->ansi #f (rich-style '(bg #x202020))))

     (equal? "x"
             (with-output-to-string
               (lambda ()
                 (rich-print (rich-style 'red) "x" (reset-style)))))

     )

(mat rich-style-errors

     ;; Unknown style symbols are rejected.
     (error? (rich-style 'not-a-style))

     ;; Truecolor integers must be in RGB range.
     (error? (rich-style #x1000000))

     ;; Background color forms must have exactly one color value.
     (error? (rich-style '(bg)))

     )

(mat rich-theme

     (rich-color-system? 'auto)

     (let ()
       (rich-theme theme)
       (rich-theme? theme))

     (let ()
       (rich-theme theme :error (rich-style 'red) :ok (rich-style 'green))
       (and (equal? "\033[31m" (rich-style->ansi #f (rich-theme-ref theme 'error)))
            (equal? "\033[32m" (rich-style->ansi #f (rich-theme-ref theme 'ok)))))

     (let ([theme (make-rich-theme (list (cons 'error (rich-style 'red))))])
       (rich-style? (rich-theme-ref theme 'error)))

     (rich-output-target? (current-output-port))
     (rich-output-target? (make-rich-console))

     )

(mat rich-theme-errors

     ;; Theme alist values must be rich style objects.
     (error? (make-rich-theme (list (cons 'error "red"))))

     ;; Theme mutation values must be rich style objects.
     (error? (let ([theme (make-rich-theme)])
               (rich-theme-set! theme 'error "red")))

     )
