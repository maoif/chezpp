(import (chezpp file)
        (chezpp control)
        (chezpp string)
        (chezpp utils)
        (chezpp os))


(define $random-file (lambda () (format "testfile_~a_~a" (random 9999) (time-nanosecond (current-time)))))
(define $rand-sign (lambda () (if (= 1 (mod (random 100) 2)) -1 1)))


(mat simple-read/write

     ;; errors
     ;; path not string
     (error? (write-lines  #f (map (lambda (x) (random-string 10)) (iota 100))))
     (error? (write-string 0 (random-string 10 20)))
     (error? (write-chars  'bla (random-char)))
     (error? (write-data   #t (random-list)))
     (error? (write-datum  '() (random-datum)))

     ;; file not exist
     (error? (read-lines  "bla.bla"))
     (error? (read-string "bla.bla"))
     (error? (read-chars  "bla.bla"))
     (error? (read-data   "bla.bla"))
     (error? (read-datum  "bla.bla"))
     (error? (read-datum-fasl  "bla.bla"))

     ;; type errors
     (error? (write-lines  "bla.bla" (map (lambda (x) (random-char)) (iota 100))))
     (error? (write-string "bla.bla" (random-list 10 20)))
     (error? (write-chars  "bla.bla" (random-box)))
     (error? (write-data   "bla.bla" #f))
     (error? (write-datum  "bla.bla" (make-eq-hashtable)))
     (error? (write-u8vec "path" 123))


;;; normal IO
     ;; lines
     (let ([lines (map (lambda (x) (random-string 100)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-lines file lines)
           (equal? lines
                   (read-lines file)))
         (lambda () (delete-file file))))
     (let ([lines (map (lambda (x) (random-string 1000)) (iota 1000))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-lines file lines)
           (equal? lines
                   (read-lines file)))
         (lambda () (delete-file file))))


     ;; string
     (let ([str (random-string 100)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-string file str)
           (equal? str
                   (read-string file)))
         (lambda () (delete-file file))))
     (let ([str (random-string 1000)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-string file str)
           (equal? str
                   (read-string file)))
         (lambda () (delete-file file))))


     ;; chars
     (let ([chars (map (lambda (x) (random-char)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-chars file chars)
           (equal? chars
                   (read-chars file)))
         (lambda () (delete-file file))))
     (let ([chars (map (lambda (x) (random-char)) (iota 1000))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-chars file chars)
           (equal? chars
                   (read-chars file)))
         (lambda () (delete-file file))))


     ;; data
     (let ([data (map (lambda (x) (random-datum)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-data file data)
           (equal? data
                   (read-data file)))
         (lambda () (delete-file file))))
     (let ([data (map (lambda (x) (random-datum)) (iota 1000))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-data file data)
           (equal? data
                   (read-data file)))
         (lambda () (delete-file file))))


     ;; datum
     (andmap (lambda (x)
               (let ([d (random-datum)]
                     [file ($random-file)])
                 (dynamic-wind
                   void
                   (lambda ()
                     (write-datum file d)
                     (equal? d
                             (read-datum file)))
                   (lambda () (delete-file file)))))
             (iota 10))


     ;; write-data-fasl
     (let ([data (map (lambda (x) (random-datum)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-data-fasl file data)
           (equal? data
                   (read-data-fasl file)))
         (lambda () (delete-file file))))
     (let ([data (map (lambda (x) (random-datum)) (iota 1000))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-data-fasl file data)
           (equal? data
                   (read-data-fasl file)))
         (lambda () (delete-file file))))


     ;; write-datum-fasl
     (andmap (lambda (x)
               (let ([d (random-datum)]
                     [file ($random-file)])
                 (dynamic-wind
                   void
                   (lambda ()
                     (write-datum-fasl file d)
                     (equal? d
                             (read-datum-fasl file)))
                   (lambda () (delete-file file)))))
             (iota 10))

     ;; u8vec
     (let ([data (random-u8vec 1024 2048)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-u8vec file data)
           (equal? data
                   (read-u8vec file)))
         (lambda () (delete-file file))))
     (let ([data (random-u8vec 2048 4096)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-u8vec file data)
           (equal? data
                   (read-u8vec file)))
         (lambda () (delete-file file))))


;;; truncate

     (let ([lines (map (lambda (x) (random-string 100)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-lines file lines)
           (write-lines! file lines)
           (write-lines! file lines)
           (equal? lines
                   (read-lines file)))
         (lambda () (delete-file file))))


     (let ([str (random-string 100)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-string file str)
           (write-string! file str)
           (write-string! file str)
           (equal? str
                   (read-string file)))
         (lambda () (delete-file file))))


     (let ([chars (map (lambda (x) (random-char)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-chars file chars)
           (write-chars! file chars)
           (write-chars! file chars)
           (equal? chars
                   (read-chars file)))
         (lambda () (delete-file file))))


     (let ([data (map (lambda (x) (random-datum)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-data file data)
           (write-data! file data)
           (write-data! file data)
           (equal? data
                   (read-data file)))
         (lambda () (delete-file file))))


     ;; write-datum
     (andmap (lambda (x)
               (let ([d (random-datum)]
                     [file ($random-file)])
                 (dynamic-wind
                   void
                   (lambda ()
                     (write-datum-fasl! file d)
                     (write-datum-fasl! file d)
                     (equal? d
                             (read-datum-fasl file)))
                   (lambda () (delete-file file)))))
             (iota 10))


     ;; data-fasl
     (let ([data (map (lambda (x) (random-datum)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-data-fasl file data)
           (write-data-fasl! file data)
           (write-data-fasl! file data)
           (equal? data
                   (read-data-fasl file)))
         (lambda () (delete-file file))))


     ;; datum-fasl
     (andmap (lambda (x)
               (let ([d (random-datum)]
                     [file ($random-file)])
                 (dynamic-wind
                   void
                   (lambda ()
                     (write-datum-fasl! file d)
                     (write-datum-fasl! file d)
                     (equal? d
                             (read-datum-fasl file)))
                   (lambda () (delete-file file)))))
             (iota 10))

     ;; u8vec
     (let ([data (random-u8vec 1024 2048)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-u8vec! file data)
           (write-u8vec! file data)
           (equal? data
                   (read-u8vec file)))
         (lambda () (delete-file file))))
     (let ([data (random-u8vec 2048 4096)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-u8vec! file data)
           (write-u8vec! file data)
           (equal? data
                   (read-u8vec file)))
         (lambda () (delete-file file))))


;;; append

     (let ([lines (map (lambda (x) (random-string 100)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-lines file lines)
           (write-lines>> file lines)
           (write-lines>> file lines)
           (equal? (append lines lines lines)
                   (read-lines file)))
         (lambda () (delete-file file))))


     (let ([str (random-string 100)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-string file str)
           (write-string>> file str)
           (write-string>> file str)
           (equal? (string-append str str str)
                   (read-string file)))
         (lambda () (delete-file file))))


     (let ([chars (map (lambda (x) (random-char)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-chars file chars)
           (write-chars>> file chars)
           (write-chars>> file chars)
           (equal? (append chars chars chars)
                   (read-chars file)))
         (lambda () (delete-file file))))


     (let ([data (map (lambda (x) (random-datum)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-data file data)
           (write-data>> file data)
           (write-data>> file data)
           (equal? (append data data data)
                   (read-data file)))
         (lambda () (delete-file file))))


     ;; write-datum
     (andmap (lambda (x)
               (let ([d (random-datum)]
                     [file ($random-file)])
                 (dynamic-wind
                   void
                   (lambda ()
                     (write-datum>> file d)
                     (write-datum>> file d)
                     (equal? d
                             (read-datum file)))
                   (lambda () (delete-file file)))))
             (iota 10))


     ;; data-fasl
     (let ([data (map (lambda (x) (random-datum)) (iota 100))]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-data-fasl file data)
           (write-data-fasl>> file data)
           (write-data-fasl>> file data)
           (equal? (append data data data)
                   (read-data-fasl file)))
         (lambda () (delete-file file))))


     ;; datum-fasl
     (andmap (lambda (x)
               (let ([d (random-datum)]
                     [file ($random-file)])
                 (dynamic-wind
                   void
                   (lambda ()
                     (write-datum-fasl>> file d)
                     (write-datum-fasl>> file d)
                     (write-datum-fasl>> file d)
                     (equal? (list d d d)
                             ;; not read-datum-fasl
                             (read-data-fasl file)))
                   (lambda () (delete-file file)))))
             (iota 10))

     ;; u8vec
     (let ([data (random-u8vec 1024 2048)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-u8vec>> file data)
           (write-u8vec>> file data)
           (let* ([len (bytevector-length data)] [d (make-bytevector (* 2 len))])
             (bytevector-copy! data 0 d 0 len)
             (bytevector-copy! data 0 d len len)
             (equal? d (read-u8vec file))))
         (lambda () (delete-file file))))
     (let ([data (random-u8vec 2048 4096)]
           [file ($random-file)])
       (dynamic-wind
         void
         (lambda ()
           (write-u8vec>> file data)
           (write-u8vec>> file data)
           (let* ([len (bytevector-length data)] [d (make-bytevector (* 2 len))])
             (bytevector-copy! data 0 d 0 len)
             (bytevector-copy! data 0 d len len)
             (equal? d (read-u8vec file))))
         (lambda () (delete-file file))))

     )


(define-syntax test-put/get
  (syntax-rules ()
    [(_ rseed iota put end1 get end2)
     (let-values ([(p getter) (open-bytevector-output-port)]
                  [(nums) (map (lambda (x) (random rseed)) iota)])
       (call-with-port p
         (lambda (p)
           (for-each (sect put p _ end1) nums)
           (call-with-port (open-bytevector-input-port (getter))
             (lambda (pin)
               (let loop ([nums nums])
                 (if (null? nums)
                     (port-eof? pin)
                     (and (= (car nums) (get pin end2))
                          (loop (cdr nums))))))))))]
    [(_ rseed iota put get)
     (test-put/get rseed iota put 'little get 'little)]))

(mat gets&puts

     (test-put/get (ash 1 16) (iota 4096) put-u16 get-u16)
     (test-put/get (ash 1 32) (iota 4096) put-u32 get-u32)
     (test-put/get (ash 1 64) (iota 4096) put-u64 get-u64)

     (test-put/get (ash 1 15) (iota 4096) put-s16 get-s16)
     (test-put/get (ash 1 31) (iota 4096) put-s32 get-s32)
     (test-put/get (ash 1 63) (iota 4096) put-s64 get-s64)

     ;; big endian
     (test-put/get (ash 1 16) (iota 4096) put-u16 'big get-u16 'big)
     (test-put/get (ash 1 32) (iota 4096) put-u32 'big get-u32 'big)
     (test-put/get (ash 1 64) (iota 4096) put-u64 'big get-u64 'big)

     (test-put/get (ash 1 15) (iota 4096) put-s16 'big get-s16 'big)
     (test-put/get (ash 1 31) (iota 4096) put-s32 'big get-s32 'big)
     (test-put/get (ash 1 63) (iota 4096) put-s64 'big get-s64 'big)

     ;; bad endian
     (not (test-put/get (ash 1 16) (iota 1024) put-u16 'big get-u16 'little))
     (not (test-put/get (ash 1 16) (iota 1024) put-u16 'little get-u16 'big))
     (not (test-put/get (ash 1 15) (iota 1024) put-s16 'big get-s16 'little))
     (not (test-put/get (ash 1 15) (iota 1024) put-s16 'big get-s16 'little))

     ;; error endian
     (error? (test-put/get (ash 1 15) (iota 4096) put-s16 'big get-s16 'giant))
     (error? (not (test-put/get (ash 1 16) (iota 1024) put-u16 'giant get-u16 'little)))
     (error? (test-put/get (ash 1 63) (iota 4096) put-s64 'big get-s64 'unknown))
     (error? (test-put/get (ash 1 63) (iota 4096) put-u64 '? get-u64 'big))

     ;; range error
     (error? (test-put/get (ash 1 32) (iota 1024) put-u16 get-u16))
     (error? (test-put/get (ash 1 64) (iota 1024) put-u32 get-u32))
     (error? (test-put/get (ash 1 87) (iota 1024) put-u64 get-u64))
     (error? (test-put/get (ash 1 32) (iota 1024) put-s16 get-s16))
     (error? (test-put/get (ash 1 34) (iota 1024) put-s32 get-s32))
     (error? (test-put/get (ash 1 66) (iota 1024) put-s64 get-s64))


     ;; TODO test multi-thread

     )


(mat file-stat

     (error? (file-stat "/dev/xxxx"))

     (file-readable? "/proc/meminfo")
     (not (file-writable? "/proc/meminfo"))
     (file-executable? "/usr/bin/bash")
     (if (file-exists? "~/.bashrc") (file-hidden? "~/.bashrc") #t)
     (if (file-exists? "~/.bash_profile") (file-hidden? "~/.bash_profile") #t)

     (file-special? "/dev/tty")
     (file-special? "/dev/zero")
     (file-special? "/dev/random")
     (file-special? "/dev/null")

     (eq? 'FT_chardev (file-type "/dev/zero"))
     (eq? 'FT_chardev (file-type "/dev/random"))
     (eq? 'FT_chardev (file-type "/dev/null"))
     )


(mat file-chmod

     (error? (file-mode->symbols ""))
     (error? (file-mode->symbols #o77771))

     (equal? '((su sg t) (r w x) (r w x) (r w x)) (file-mode->symbols #o7777))
     (equal? '((su sg) (r w) (r w) (r w)) (file-mode->symbols #o6666))
     (equal? '((t) (x) (x) (x)) (file-mode->symbols #o1111))
     (equal? '(() (r w) (r w) (r)) (file-mode->symbols #o664))


     (error? (symbols->file-mode '()))
     (error? (symbols->file-mode '() '()))
     (error? (symbols->file-mode '() '() '()))
     (error? (symbols->file-mode '() '() '() '(s)))
     (error? (symbols->file-mode '() '() '() 123))
     (error? (symbols->file-mode '() '() '(a) '()))
     (error? (symbols->file-mode '() '(bla) '() '()))
     (error? (symbols->file-mode '(a) '() '() '()))
     (error? (symbols->file-mode '() '() '(r r) '()))
     (error? (symbols->file-mode '() '() '() '(r x x)))
     (error? (symbols->file-mode '(t t) '() '() '()))

     (= 0 (symbols->file-mode '() '() '() '()))
     (= #o400 (symbols->file-mode '() '(r) '() '()))
     (= #o444 (symbols->file-mode '() '(r) '(r) '(r)))
     (= #o1111 (symbols->file-mode '(t) '(x) '(x) '(x)))
     (= #o421 (symbols->file-mode '() '(r) '(w) '(x)))
     (= #o5562 (symbols->file-mode '(t su) '(x r) '(w r) '(w)))
     (= #o7777 (symbols->file-mode '(su sg t) '(r w x) '(r w x) '(r w x)))

     )


(mat define-file-tree

     (begin (define testsrc1 "test src 1")
            (define testsrc2 "test src 2")
            (define lines (map (lambda (x) (random-string)) (iota 10)))
            (define-file-tree fstree
              ;; write-string, write-string!
              (file "README.md" (text "This is a fancy project."))
              ;; mkdir
              (dir "src"
                   ;; write-string(!), file-chmod
                   (file "src1.ss" (mode #o777) (text "somestring"))
                   ;; write-datum-fasl(!)
                   (file "src2.ss" (fasl '(a list of symbols)))
                   (dir "native"
                        ;; empty file, file-touch
                        (file "runtime.c")
                        (file "ranstr" (lines lines))))
              (dir "tests" (mode #o777)
                   (file "test1.ss" (text testsrc1))
                   (file "test2.ss" (text testsrc2))
                   ;; file-symlink(!)
                   (file "src1" (symlink "../src/src1.ss"))
                   ;; link to link, simpler syntax
                   (symlink "src1" "src1.ln")))

            (when (file-directory? "./fstree")
              (file-removetree "./fstree"))

            #t)

     (begin (create-fstree "./fstree")
            #t)

     (file-directory? "./fstree")
     (file-directory? "./fstree/src")
     (file-directory? "./fstree/src/native")
     (file-directory? "./fstree/tests")

     (file-regular? "./fstree/README.md")
     (file-regular? "./fstree/src/src1.ss")
     (file-regular? "./fstree/src/src2.ss")
     (file-regular? "./fstree/src/native/runtime.c")
     (file-regular? "./fstree/src/native/ranstr")
     (file-regular? "./fstree/tests/test1.ss")
     (file-regular? "./fstree/tests/test2.ss")

     (file-symbolic-link? "./fstree/tests/src1")
     (file-symbolic-link? "./fstree/tests/src1.ln")
     (string=? "../src/src1.ss" (readlink "./fstree/tests/src1"))
     (string=? "src1" (readlink "./fstree/tests/src1.ln"))

     (string=? "This is a fancy project." (read-string "./fstree/README.md"))

     (string=? "somestring" (read-string "./fstree/src/src1.ss"))
     (= #o777 (get-mode "./fstree/src/src1.ss"))
     (string=? "somestring" (read-string "./fstree/tests/src1"))
     (string=? "somestring" (read-string "./fstree/tests/src1.ln"))

     (equal? '(a list of symbols)
             (read-datum-fasl "./fstree/src/src2.ss"))

     (equal? lines (read-lines "./fstree/src/native/ranstr"))
     (equal? testsrc1 (read-string "./fstree/tests/test1.ss"))
     (equal? testsrc2 (read-string "./fstree/tests/test2.ss"))


     ;; file already exists
     (error? (create-fstree "./fstree"))

     (begin (create-fstree! "./fstree")
            #t)

     (file-removetree "./fstree")
     (not (file-exists? "./fstree"))
     (not (file-directory? "./fstree"))
     )


(mat file-touch

     (begin (define dir "./touch_test")
            (define atdir (lambda (x) (path-build dir x)))
            (when (file-directory? dir) (file-removetree dir))

            (define-file-tree tree
              (dir "d1"
                   (file "f1")
                   (dir "d2"
                        (symlink "../f1" "ln1")
                        (symlink "ln1" "ln2"))))
            (create-tree dir)
            #t)

     (error? (file-touch 'bla))
     ;; path not exist
     (error? (file-touch "a/b/c/d"))

     (let ([at1 (file-access-time (atdir "d1/f1"))]
           [mt1 (file-modification-time (atdir "d1/f1"))])
       (file-touch (atdir "d1/f1"))
       (let ([at2 (file-access-time (atdir "d1/f1"))]
             [mt2 (file-modification-time (atdir "d1/f1"))])
         (and (time<? at1 at2) (time<? mt1 mt2))))

     ;; create file
     (not (file-exists? (atdir "d1/f2")))
     (begin (file-touch (atdir "d1/f2"))
            (file-exists? (atdir "d1/f2")))

     ;; path is symlink, follow
     (let ([time (current-time)])
       (file-touch (atdir "d1/d2/ln1") time #t #f)
       (let ([at2 (file-access-time (atdir "d1/f1"))]
             [mt2 (file-modification-time (atdir "d1/f1"))])
         (and (time=? time at2) (time=? time mt2))))

     (let ([time (current-time)])
       (file-touch (atdir "d1/d2/ln2") time #t #f)
       (let ([at2 (file-access-time (atdir "d1/f1"))]
             [mt2 (file-modification-time (atdir "d1/f1"))])
         (and (time=? time at2) (time=? time mt2))))

     ;; path is symlink, not follow
     (let ([time (current-time)])
       (file-touch (atdir "d1/d2/ln1") time #f #f)
       (let ([at2 (file-access-time (atdir "d1/d2/ln1") #f)]
             [mt2 (file-modification-time (atdir "d1/d2/ln1") #f)])
         (and (time=? time at2) (time=? time mt2))))


     ;; path is dir
     (let ([time (current-time)])
       (file-touch (atdir "d1") time)
       (let ([at2 (file-access-time (atdir "d1"))]
             [mt2 (file-modification-time (atdir "d1"))])
         (and (time=? time at2) (time=? time mt2))))

     (file-removetree dir))



(mat readlink

     (begin (define dir "./readlink_test")
            (when (file-directory? dir) (file-removetree dir))

            (define-file-tree tree
              (dir "d1"
                   (file "f1")
                   (dir "d2"
                        (dir "d3"
                             (symlink "../../f1" "ln1")
                             (symlink "ln1"      "ln2")
                             (symlink "ln2"      "ln3")))))
            (define d1d2d3 (lambda (x) (path-build dir (path-build "d1/d2/d3" x))))

            #t)

     (begin (create-tree dir)
            (file-directory? dir))
     (file-symbolic-link? (d1d2d3 "ln1"))
     (file-symbolic-link? (d1d2d3 "ln2"))
     (file-symbolic-link? (d1d2d3 "ln3"))

     (error? (readlink 123))

     ;; non-rec
     (string=? "../../f1" (readlink (d1d2d3 "ln1")))
     (string=? "ln1" (readlink (d1d2d3 "ln2")))
     (string=? "ln2" (readlink (d1d2d3 "ln3")))

     ;; rec
     (same-file? (path-build dir "d1/f1") (readlink2 (d1d2d3 "ln2") #t))
     (same-file? (path-build dir "d1/f1") (readlink2 (d1d2d3 "ln3") #t))

     (let-values ([(ln pdir) (readlink (d1d2d3 "ln3") #t)])
       (and (string=? ln "../../f1") (string=? pdir (path-build dir "d1/d2/d3"))))

     (file-removetree dir))


(mat file-link

     (begin (define dir "link_test")
            (define atdir (lambda (x) (path-build dir x)))
            (when (file-directory? dir) (file-removetree dir))

            (define testsrc1 "test src 1")
            (define testsrc2 "test src 2")
            (define lines (map (lambda (x) (random-string)) (iota 10)))

            (define-file-tree fstree
              (file "README.md" (text "This is a fancy project."))
              (dir "src"
                   (file "src1.ss" (mode #o777) (text "somestring"))
                   (file "src2.ss" (fasl '(a list of symbols)))
                   (symlink "../tests" "tests")
                   (dir "native"
                        (file "runtime.c")
                        (file "ranstr" (lines lines))))
              (dir "tests" (mode #o777)
                   (file "test1.ss" (text testsrc1))
                   (file "test2.ss" (text testsrc2))
                   (file "src1" (symlink "../src/src1.ss"))
                   (symlink "src1" "src1.ln")))

            #t)

     (begin (create-fstree dir) #t)

     ;; src==dest

     ;; src is regular file
     (= 1 (file-nlinks (atdir "src/src1.ss")))
     (begin (file-link (atdir "src/src1.ss") (atdir "src/src1.cp"))
            #t)
     (file-exists? (atdir "src/src1.cp"))
     (= 2 (file-nlinks (atdir "src/src1.ss")))
     (= 2 (file-nlinks (atdir "src/src1.cp")))

     ;; src is symlink
     (begin (file-link (atdir "tests/src1") (atdir "src1.cp"))
            #t)
     (file-exists? (atdir "src1.cp"))
     (= 3 (file-nlinks (atdir "src/src1.ss")))
     (= 3 (file-nlinks (atdir "src1.cp")))

     ;; dest is file
     (error? (file-link (atdir "tests/src1") (atdir "src1.cp")))
     (not (error? (file-link! (atdir "tests/src1") (atdir "src1.cp"))))

     ;; dest is dir
     (error? (file-link (atdir "src") (atdir "src1")))
     (error? (file-link (atdir "tests1") (atdir "tests1")))

     ;; dest is symlink to file
     (begin (file-link (atdir "tests/src1") (atdir "tests/src2"))
            (= 4 (file-nlinks (atdir "tests/src2"))))
     (begin (file-link (atdir "tests/src1.ln") (atdir "tests/src3"))
            (= 5 (file-nlinks (atdir "tests/src3"))))

     ;; dest is symlink to file (not follow)
     (begin (file-link (atdir "tests/src1.ln") (atdir "tests/src2.ln") #f)
            (= 2
               (file-nlinks (atdir "tests/src1.ln") #f)
               (file-nlinks (atdir "tests/src2.ln") #f)))
     (string=? (readlink (atdir "tests/src1.ln"))
               (readlink (atdir "tests/src2.ln")))

     ;; dest is symlink to dir
     (error? (file-link (atdir "src/tests") (atdir "tests1")))

     ;; path starting with ~ (test expand_pathname() in libchezpp)

     (file-removetree dir))


(mat file-symlink

     (begin (define dir "symlink_test")
            (define atdir (lambda (x) (path-build dir x)))
            (when (file-directory? dir) (file-removetree dir))

            (define lines (map (lambda (x) (random-string)) (iota 10)))
            (define-file-tree fstree
              (file "README.md" (text "This is a fancy project."))
              (dir "src"
                   (file "src1.ss" (mode #o777) (text (random-string 50 100)))
                   (file "src2.ss" (fasl (random-list 20 40)))
                   (symlink "../tests" "tests")
                   (dir "native"
                        (file "runtime.c")
                        (file "ranstr" (lines lines))))
              (dir "tests" (mode #o777)
                   (file "test1.ss" (text (random-string 50 100)))
                   (file "test2.ss" (text (random-string 50 100)))
                   (file "src1" (symlink "../src/src1.ss"))
                   (symlink "src1" "src1.ln")
                   (symlink "../src/native" "native")))

            #t)

     (begin (create-fstree dir) #t)


     ;; src==dest

     ;; src is regular file
     (begin (file-symlink "src/src1.ss" (atdir "src1"))
            (string=? "src/src1.ss" (readlink (atdir "src1"))))
     (string=? (read-string (atdir "src1")) (read-string (atdir "src/src1.ss")))

     (begin (file-symlink "src1" (atdir "tests/src2"))
            (and (string=? "src1" (readlink (atdir "tests/src2")))
                 (string=? (read-string (atdir "tests/src1")) (read-string (atdir "tests/src2")))))

     ;; dest is file
     (error? (file-symlink "src/src1.ss" (atdir "src1")))
     (error? (file-symlink "src/src1.ss" (atdir "src/src2.ss")))

     (begin (file-symlink! "src1.ss" (atdir "src/src2.ss"))
            (string=? (read-string (atdir "src/src1.ss")) (read-string (atdir "src/src2.ss"))))

     ;; dest is dir
     (begin (file-symlink "tests/test1.ss" dir)
            (and (file-symbolic-link? (atdir "test1.ss"))
                 (string=? (read-string (atdir "test1.ss")) (read-string (atdir "tests/test1.ss")))))

     ;; dest is symlink to file
     (error? (file-symlink "../src/src1.ss" (atdir "tests/test1.ss")))
     (begin (file-symlink! "../src/src1.ss" (atdir "tests/test1.ss"))
            (string=? (read-string (atdir "src/src1.ss")) (read-string (atdir "tests/test1.ss"))))

     ;; dest is symlink to dir
     (begin (file-symlink "../../README.md" (atdir "tests/native"))
            (and (string=? "../../README.md" (readlink (atdir "tests/native/README.md")))
                 (string=? (read-string (atdir "README.md")) (read-string (atdir "tests/native/README.md")))))

     ;; path starting with ~

     (file-removetree dir)
     )


(mat file-copy

     (begin (define dir "copy_test")
            (define atdir (lambda (x) (path-build dir x)))
            (when (file-directory? dir) (file-removetree dir))

            (define lines (map (lambda (x) (random-string)) (iota 10)))
            (define-file-tree fstree
              (file "README.md" (text "This is a fancy project."))
              (dir "src"
                   (file "src1.ss" (mode #o777) (text (random-string 50 100)))
                   (file "src2.ss" (fasl (random-list 20 40)))
                   (symlink "../tests" "tests")
                   (dir "native"
                        (file "runtime.c")
                        (file "ranstr" (lines lines))))
              (dir "tests" (mode #o777)
                   (file "big" (fasl (random-list 99999 999999)))
                   (file "test1.ss" (text (random-string 50 100)))
                   (file "test2.ss" (text (random-string 50 100)))
                   (file "src1" (symlink "../src/src1.ss"))
                   (symlink "src1" "src1.ln")
                   (symlink "../src/native" "native")))

            #t)

     (begin (create-fstree dir) #t)

     ;; src==dest

     ;; src is regular file
     (begin (file-copy (atdir "README.md") (atdir "src/md"))
            (and (file-exists? (atdir "src/md"))
                 (string=? (read-string (atdir "README.md")) (read-string (atdir "src/md")))))
     (begin (file-copy (atdir "src/src2.ss") (atdir "tests/src2.ss"))
            (and (file-exists? (atdir "tests/src2.ss"))
                 (equal? (read-datum-fasl (atdir "src/src2.ss")) (read-datum-fasl (atdir "tests/src2.ss")))))

     ;; src is special file
     (error? (file-copy "/dev/tty" (atdir "tty")))

     ;; src is symlink to file
     (begin (file-copy (atdir "tests/src1") (atdir "src1"))
            (string=? (read-string (atdir "src1")) (read-string (atdir "tests/src1"))))
     (begin (file-copy (atdir "tests/src1.ln") (atdir "src11"))
            (string=? (read-string (atdir "src11")) (read-string (atdir "tests/src1.ln"))))

     ;; src is symlink to file, no follow
     (begin (file-copy (atdir "tests/src1") (atdir "src1.ln") #f)
            (and (file-symbolic-link? (atdir "src1.ln"))
                 (string=? "../src/src1.ss" (readlink (atdir "src1.ln")))))

     ;; dest is file
     (error? (file-copy (atdir "README.md") (atdir "tests/test2.ss")))
     (error? (file-copy (atdir "tests/test2.ss") (atdir "README.md")))

     (not (error? (file-copy! (atdir "README.md") (atdir "tests/test2.ss"))))
     (string=? (read-string (atdir "README.md")) (read-string (atdir "tests/test2.ss")))


     ;; dest is dir
     (begin (file-copy (atdir "README.md") (atdir "src/native"))
            (file-exists? (atdir "src/native/README.md")))
     (string=? (read-string (atdir "README.md")) (read-string (atdir "src/native/README.md")))

     ;; dest is symlink to file
     (error? (file-copy (atdir "README.md") (atdir "tests/src1")))
     (begin (file-copy! (atdir "README.md") (atdir "tests/src1"))
            (file-regular? (atdir "tests/src1")))
     (string=? (read-string (atdir "README.md")) (read-string (atdir "tests/src1")))

     ;; dest is symlink to dir
     (begin (file-copy (atdir "README.md") (atdir "src/tests"))
            (and (file-exists? (atdir "src/tests/README.md"))
                 (file-exists? (atdir "tests/README.md"))))
     (string=? (read-string (atdir "README.md")) (read-string (atdir "tests/README.md")))

     ;; big file
     (begin (file-copy (atdir "tests/big") (atdir "src"))
            (file-regular? (atdir "src/big")))
     (equal? (read-datum-fasl (atdir "src/big")) (read-datum-fasl (atdir "tests/big")))

     ;; path starting with ~

     (file-removetree dir)
     )


(mat file-copymode

     (begin (define dir "copymode_test")
            (define atdir (lambda (x) (path-build dir x)))
            (when (file-directory? dir) (file-removetree dir))

            (define-file-tree fstree
              (file "README.md" (text "This is a fancy project."))
              (dir "src"
                   (file "src1.ss" (mode #o777) (text (random-string 50 100)))
                   (file "src2.ss" (mode '(r w x) '(w x) '(r x)) (fasl (random-list 20 40)))
                   (symlink "../tests" "tests")
                   (dir "native"
                        (file "runtime.c")))
              (dir "tests" (mode #o777)
                   (file "test1.ss" (text (random-string 50 100)))
                   (file "test2.ss" (text (random-string 50 100)))
                   (file "src1" (symlink "../src/src1.ss"))
                   (symlink "src1" "src1.ln")
                   (symlink "../src/native" "native")))

            #t)

     (not (file-directory? dir))
     (begin (create-fstree dir) #t)

     (= #o777 (get-mode (atdir "src/src1.ss")))
     (begin (file-copymode (atdir "src/src2.ss") (atdir "src/src1.ss"))
            (= (symbols->file-mode '() '(r w x) '(w x) '(r x)) (get-mode (atdir "src/src1.ss"))))

     (= #o777 (get-mode (atdir "tests")))
     (begin (file-copymode (atdir "src") (atdir "tests"))
            (= (get-mode (atdir "src")) (get-mode (atdir "tests"))))

     (not (= #o777 (get-mode (atdir "tests/native") #t)))
     (begin (file-copymode (atdir "src/src2.ss") (atdir "tests/native"))
            (= (get-mode (atdir "src/src2.ss")) (get-mode (atdir "src/native"))))

     (file-removetree dir))


(mat file-copymeta

     (begin (define dir "copymeta_test")
            (define atdir (lambda (x) (path-build dir x)))
            (when (file-directory? dir) (file-removetree dir))

            (define-file-tree fstree
              (file "README.md" (text "This is a fancy project."))
              (dir "src"
                   (file "src1.ss" (mode #o777) (text (random-string 50 100)))
                   (file "src2.ss" (mode '(r w x) '(w x) '(r x)) (fasl (random-list 20 40)))
                   (symlink "../tests" "tests")
                   (dir "native"
                        (file "runtime.c")))
              (dir "tests" (mode #o777)
                   (file "test1.ss" (text (random-string 50 100)))
                   (file "test2.ss" (text (random-string 50 100)))
                   (file "src1" (symlink "../src/src1.ss"))
                   (symlink "src1" "src1.ln")
                   (symlink "../src/native" "native")))

            #t)

     (not (file-directory? dir))
     (begin (create-fstree dir) #t)

     ;; times of files above may all be the same...
     (let* ([5s (make-time 'time-utc 0 5)]
            [newt (time-difference (file-access-time (atdir "src/src1.ss")) 5s)])
       (set-time-type! newt 'time-utc)
       (file-touch-atime (atdir "tests/test1.ss") newt)
       (file-touch-mtime (atdir "tests/test1.ss") newt)
       #t)

     ;; compare atime, mtime and mode
     (let ([mode1  (get-mode (atdir "src/src1.ss"))]
           [atime1 (file-access-time (atdir "src/src1.ss"))]
           [mtime1 (file-modification-time (atdir "src/src1.ss"))]
           [mode2  (get-mode (atdir "tests/test1.ss"))]
           [atime2 (file-access-time (atdir "tests/test1.ss"))]
           [mtime2 (file-modification-time (atdir "tests/test1.ss"))])
       (and (not (= mode1 mode2))
            (not (time=? atime1 atime2))
            (not (time=? mtime1 mtime2))))
     (begin (file-copymeta (atdir "src/src1.ss") (atdir "tests/test1.ss"))
            (let ([mode1  (get-mode (atdir "src/src1.ss"))]
                  [atime1 (file-access-time (atdir "src/src1.ss"))]
                  [mtime1 (file-modification-time (atdir "src/src1.ss"))]
                  [mode2  (get-mode (atdir "tests/test1.ss"))]
                  [atime2 (file-access-time (atdir "tests/test1.ss"))]
                  [mtime2 (file-modification-time (atdir "tests/test1.ss"))])
              (and (= mode1 mode2)
                   (time=? atime1 atime2)
                   (time=? mtime1 mtime2))))

     ;; TODO src/dest are symlinks

     (file-removetree dir)
     )


(mat file-copytree

     (begin (define dir "copy_test")
            (define dir1 "./copy_test1")

            (define atdir  (lambda (x) (path-build dir  x)))
            (define atdir1 (lambda (x) (path-build dir1 x)))

            (when (file-directory? dir) (file-removetree dir))
            (when (file-directory? dir1) (file-removetree dir1))

            (define lines (map (lambda (x) (random-string)) (iota 10)))
            (define-file-tree fstree
              (file "README.md" (text "This is a fancy project."))
              (dir "src"
                   (file "src1.ss" (mode #o777) (text (random-string 50 100)))
                   (file "src2.ss" (fasl (random-list 20 40)))
                   (symlink "../tests" "tests")
                   (dir "native"
                        (file "runtime.c")
                        (file "ranstr" (lines lines))))
              (dir "tests" (mode #o777)
                   (file "big" (fasl (random-list 99999 999999)))
                   (file "test1.ss" (text (random-string 50 100)))
                   (file "test2.ss" (text (random-string 50 100)))
                   (file "src1" (symlink "../src/src1.ss"))
                   (symlink "src1" "src1.ln")
                   (symlink "../src/native" "native")))

            #t)

     (begin (create-fstree dir) #t)

     (begin (file-copytree dir dir1)
            #t)

     (same-file-contents? (atdir "README.md") (atdir1 "README.md"))
     (same-file-contents? (atdir "README.md") (atdir1 "README.md"))
     (same-file-contents? (atdir "src/src1.ss") (atdir1 "src/src1.ss"))
     (same-file-contents? (atdir "src/src2.ss") (atdir1 "src/src2.ss"))
     (same-file-contents? (atdir "src/native/ranstr") (atdir1 "src/native/ranstr"))
     (same-file-contents? (atdir "tests/big") (atdir1 "tests/big"))
     (same-file-contents? (atdir "tests/test1.ss") (atdir1 "tests/test1.ss"))
     (same-file-contents? (atdir "tests/test2.ss") (atdir1 "tests/test2.ss"))
     (same-file-contents? (atdir "tests/src1") (atdir1 "tests/src1"))
     (same-file-contents? (atdir "tests/src1.ln") (atdir1 "tests/src1.ln"))
     (same-file-contents? (atdir "tests/native/runtime.c") (atdir1 "tests/native/runtime.c"))

     (file-symbolic-link? (atdir1 "src/tests"))
     (file-symbolic-link? (atdir1 "tests/src1.ln"))
     (file-symbolic-link? (atdir1 "tests/native"))
     (string=? "../src/native" (readlink (atdir1 "tests/native")))

     ;; TODO another copy function, dest exists

     (file-removetree dir)
     (file-removetree dir1))


(mat file-move

     (begin (define dir "move_test")
            (define dir1 "./move_test1")

            (define atdir  (lambda (x) (path-build dir  x)))
            (define atdir1 (lambda (x) (path-build dir1 x)))

            (when (file-directory? dir) (file-removetree dir))
            (when (file-directory? dir1) (file-removetree dir1))

            (define big (random-list 99999 999999))
            (define lines (map (lambda (x) (random-string)) (iota 10)))
            (define str (random-string 50 100))
            (define-file-tree fstree
              (file "README.md" (text "This is a fancy project."))
              (dir "src"
                   (file "src1.ss" (mode #o777) (text (random-string 50 100)))
                   (file "src2.ss" (text str))
                   (symlink "../tests" "tests")
                   (dir "native"
                        (file "runtime.c")
                        (file "ranstr" (lines lines))))
              (dir "tests" (mode #o777)
                   (file "big" (fasl big))
                   (file "test1.ss" (text (random-string 50 100)))
                   (file "test2.ss" (text "str"))
                   (file "src1" (symlink "../src/src1.ss"))
                   (symlink "src1" "src1.ln")
                   (symlink "../src/native" "native")))

            #t)

     (begin (create-fstree dir) #t)

     ;; move files
     (begin (file-move (atdir "README.md") (atdir "src"))
            (string=? "This is a fancy project." (read-string (atdir "src/README.md"))))
     (not (file-exists? (atdir "README.md")))

     (begin (file-move (atdir "tests/big") (atdir "src/big1"))
            (equal? big (read-datum-fasl (atdir "src/big1"))))
     (not (file-exists? (atdir "tests/big")))

     (error? (file-move (atdir "src/src2.ss") (atdir "tests/test2.ss")))
     ;; overwrite
     (not (error? (file-move (atdir "src/src2.ss") (atdir "tests/test2.ss") file-copy!)))
     (file-exists? (atdir "tests/test2.ss"))
     (not (file-exists? (atdir "src/src2.ss")))
     (equal? str (read-string (atdir "tests/test2.ss")))

     ;; move tree
     (begin (file-move dir dir1)
            (not (file-directory? dir)))
     (file-directory? (atdir1 "src"))
     (file-directory? (atdir1 "tests"))
     (file-directory? (atdir1 "src/native"))

     (file-exists? (atdir1 "tests/test2.ss"))
     (equal? str (read-string (atdir1 "tests/test2.ss")))
     (equal? big (read-datum-fasl (atdir1 "src/big1")))


     (file-removetree dir1)
     )
