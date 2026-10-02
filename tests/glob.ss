(import (chezpp) (chezpp glob))

(mat glob-literals-and-wildcards
     (glob-match? (make-glob "foo.txt") "foo.txt")
     (not (glob-match? (make-glob "foo.txt") "bar.txt"))
     (glob-match? (make-glob "*.txt") "foo.txt")
     (not (glob-match? (make-glob "*.txt") "foo.ss"))
     (glob-match? (make-glob "file?.txt") "file1.txt")
     (not (glob-match? (make-glob "file?.txt") "file10.txt")))

(mat glob-classes-and-escapes
     (glob-match? (make-glob "file[0-9].txt") "file7.txt")
     (not (glob-match? (make-glob "file[0-9].txt") "filex.txt"))
     (glob-match? (make-glob "file[!0-9].txt") "filex.txt")
     (glob-match? (make-glob "literal\\*.txt") "literal*.txt")
     ;; Regex metacharacters in literal glob text must remain literal.
     (glob-match? (make-glob "a.+(b).txt") "a.+(b).txt"))

(mat glob-recursive-and-path-components
     (glob-match? (make-glob "src/**/test?.ss") "src/test1.ss")
     (glob-match? (make-glob "src/**/test?.ss") "src/a/b/test2.ss")
     (not (glob-match? (make-glob "src/*/test?.ss") "src/a/b/test2.ss")))

(mat glob-braces-and-ranges
     (glob-match? (make-glob "*.{ss,sls}") "file.ss")
     (glob-match? (make-glob "*.{ss,sls}") "file.sls")
     (not (glob-match? (make-glob "*.{ss,sls}") "file.txt"))
     (glob-match? (make-glob "file{1..3}.txt") "file2.txt")
     (not (glob-match? (make-glob "file{1..3}.txt") "file4.txt"))
     (glob-match? (make-glob "file{3..1}.txt") "file2.txt")
     (glob-match? (make-glob "file{0..6..2}.txt") "file4.txt"))

(mat glob-tilde
     (let* ([home (path-expand-user (path-parse 'unix "~"))]
            [pattern (path-render (path-add-component home "src"))])
       (glob-match? (make-glob "~/src") pattern)))

(mat glob-windows-flavor
     (glob-match? (make-glob 'windows "src\\*.ss") "src\\file.ss")
     (glob-match? (make-glob 'windows "C:\\src\\*.ss") "C:\\src\\file.ss")
     (not (glob-match? (make-glob 'windows "src\\*.ss") "src\\nested\\file.ss")))

(mat glob-invalid-patterns
     ;; Unterminated bracket class is rejected during compilation.
     (error? (make-glob "[abc"))
     ;; Trailing escape is rejected during compilation.
     (error? (make-glob "abc\\"))
     ;; Empty brace alternatives are rejected during expansion.
     (error? (make-glob "{a,}"))
     ;; Unsupported named-user tilde expansion is rejected.
     (error? (make-glob "~other/src")))

(mat glob-filesystem-expansion
     (pair? (glob "*.ss"))
     (equal? '() (glob "__chezpp_missing_glob_file__"))
     (equal? '("__chezpp_missing_glob_file__")
             (glob* "__chezpp_missing_glob_file__" #f #f 'literal)))

(mat glob-filesystem-iterator
     (let ([iter (glob->iter "*.ss")])
       (and (string? (iter-next! iter))
            (iter? iter)
            (begin (iter-finalize! iter) #t))))
