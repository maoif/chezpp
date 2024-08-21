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
