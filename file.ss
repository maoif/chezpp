#!chezscheme

(library (chezpp file)
  (export read-lines read-string read-chars read-data read-datum read-data-fasl read-datum-fasl read-u8vec
          write-lines write-string write-chars write-data write-datum write-data-fasl write-datum-fasl write-u8vec
          write-lines! write-string! write-chars! write-data! write-datum! write-data-fasl! write-datum-fasl! write-u8vec!
          write-lines>> write-string>> write-chars>> write-data>> write-datum>> write-data-fasl>> write-datum-fasl>> write-u8vec>>

          get-u16 get-u32 get-u64 get-s16 get-s32 get-s64
          put-u16 put-u32 put-u64 put-s16 put-s32 put-s64

          GET-U16 GET-U32 GET-U64 GET-S16 GET-S32 GET-S64
          PUT-U16 PUT-U32 PUT-U64 PUT-S16 PUT-S32 PUT-S64

          make-path

          file-stat file-type file-mode file-size file-size-h
          file-access-time file-modification-time file-change-time file-creation-time
          file-inode file-blocks file-nlinks file-owner file-group
          file-dev-major file-dev-minor

          file-readable? file-writable? file-executable? file-hidden? file-special? same-file? same-file-contents?

          file-mode->symbols symbols->file-mode
          file-chmod file-chmod-s file-chmod-u file-chmod-g file-chmod-o file-chmod-a)
  (import (chezpp chez)
          (chezpp private os)
          (chezpp internal)
          (chezpp utils)
          (chezpp list)
          (chezpp io))


  (define-condition-type &i/o-file-not-regular &i/o-filename
    make-i/o-file-not-regular-error i/o-file-not-regular-error?)
  (define-condition-type &i/o-file-not-directory &i/o-filename
    make-i/o-file-not-directory-error i/o-file-not-directory-error?)

  (define $err-file
    (case-lambda
      [(who msg)
       (raise (condition (make-who-condition who) (make-message-condition msg)))]
      [(who x msg)
       (cond [(condition? x)
              (raise (condition (make-who-condition who) x (make-message-condition msg)))]
             [(string? x)
              (raise (condition (make-who-condition who) (make-i/o-filename-error x) (make-message-condition msg)))]
             [else (assert-unreachable)])]))

  (define $err-file-not-regular
    (lambda (who p)
      ($err-file who (make-i/o-file-not-regular-error p)
                 (format "file is not regular: ~a" p))))
  (define $err-file-not-directory
    (lambda (who p)
      ($err-file who (make-i/o-file-not-directory-error p)
                 (format "file is not directory: ~a" p))))
  (define $err-file-exists
    (lambda (who p)
      ($err-file who (make-i/o-file-already-exists-error p)
                 (format "file already exists: ~a" p))))
  (define $err-directory-exists
    (lambda (who p)
      ($err-file who (make-i/o-file-already-exists-error p)
                 (format "directory already exists: ~a" p))))
  (define $err-file-not-found
    (lambda (who p)
      ($err-file who (make-i/o-file-does-not-exist-error p)
                 (format "file not found: ~a" p))))

  (define $err-bad-write-mode
    (lambda (who mode)
      (errorf who "unknown write mode: ~a, should be one of '(error truncate append)" mode)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   simple file read/write
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define read-lines
    (lambda (path)
      (pcheck-file
       (path)
       (call-with-input-file path
         (lambda (p)
           (let ([lb (make-list-builder)])
             (let loop ()
               (let ([x (get-line p)])
                 (if (eof-object? x)
                     (lb)
                     (begin (lb x)
                            (loop)))))))))))
  (define read-string
    (lambda (path)
      (pcheck-file
       (path)
       (call-with-input-file path
         (lambda (p)
           (let ([x (get-string-all p)])
             (if (eof-object? x) "" x)))))))
  (define read-chars
    (lambda (path)
      (pcheck-file
       (path)
       (call-with-input-file path
         (lambda (p)
           (let ([lb (make-list-builder)])
             (let loop ()
               (let ([x (get-char p)])
                 (if (eof-object? x)
                     (lb)
                     (begin (lb x)
                            (loop)))))))))))
  (define read-data
    (lambda (path)
      (pcheck-file
       (path)
       (call-with-input-file path
         (lambda (p)
           (let ([lb (make-list-builder)])
             (let loop ()
               (let ([x (get-datum p)])
                 (if (eof-object? x)
                     (lb)
                     (begin (lb x)
                            (loop)))))))))))
  (define read-datum
    (lambda (path)
      (pcheck-file
       (path)
       (call-with-input-file path
         (lambda (p) (get-datum p))))))
  (define read-data-fasl
    (lambda (path)
      (pcheck-file
       (path)
       (call-with-port (open-file-input-port path)
         (lambda (p)
           (let ([lb (make-list-builder)])
             (let loop ()
               (let ([x (fasl-read p)])
                 (if (eof-object? x)
                     (lb)
                     (begin (lb x)
                            (loop)))))))))))
  (define read-datum-fasl
    (lambda (path)
      (pcheck-file
       (path)
       (call-with-port (open-file-input-port path)
         (lambda (p) (fasl-read p))))))
  (define read-u8vec
    (lambda (path)
      (pcheck-file
       (path)
       (call-with-port (open-file-input-port path)
         (lambda (p) (get-bytevector-all p))))))

  (define all-strings? (lambda (x) (andmap string? x)))
  (define all-chars?   (lambda (x) (andmap char? x)))


  ;; mode when file exists: 'error, 'truncate, 'append
  (define $write-helper
    (lambda (who path mode bin? writer)
      (let ([go (lambda (mode)
                  (let ([op (if mode
                                (case mode
                                  [truncate (file-options replace)]
                                  [append   (file-options no-truncate no-fail append)]
                                  [else ($err-bad-write-mode who mode)])
                                (file-options))])
                    (call-with-port (open-file-output-port path op (buffer-mode block)
                                                           (if bin? #f (current-transcoder)))
                      (lambda (p) (writer p)))))])
        (if (file-exists? path)
            (if (eq? mode 'error)
                ($err-file-exists who path)
                (if (file-regular? path)
                    (go mode)
                    ($err-file-not-regular who path)))
            ;; feel free to write
            (go #f)))))

  ;; a newline is added after writing each string
  (define $write-lines
    (lambda (who path mode lines)
      (pcheck ([all-strings? lines])
              (let ([do-write (lambda (p)
                                (let loop ([lines lines])
                                  (unless (null? lines)
                                    (put-string p (car lines))
                                    (newline p)
                                    (loop (cdr lines)))))])
                ($write-helper who path mode #f do-write)))))
  (define $write-string
    (lambda (who path mode str)
      (pcheck-string
       (str)
       (let ([do-write (lambda (p)
                         (put-string p str))])
         ($write-helper who path mode #f do-write)))))
  (define $write-chars
    (lambda (who path mode chars)
      (pcheck ([all-chars? chars])
              (let ([do-write (lambda (p)
                                (let loop ([chars chars])
                                  (unless (null? chars)
                                    (put-char p (car chars))
                                    (loop (cdr chars)))))])
                ($write-helper who path mode #f do-write)))))
  ;; write a list of datum
  (define $write-data
    (lambda (who path mode data)
      (pcheck-list
       (data)
       (let ([do-write (lambda (p)
                         (let loop ([data data])
                           (unless (null? data)
                             (put-datum p (car data))
                             ;; for better readability
                             (newline p)
                             (loop (cdr data)))))])
         ($write-helper who path mode #f do-write)))))
  ;; write a single datum
  (define $write-datum
    (lambda (who path mode datum)
      (define writable?
        (lambda (x)
          (not (or (procedure? x) (hashtable? x) (port? x)))))
      (pcheck ([writable? datum])
              (let ([do-write (lambda (p)
                                (put-datum p datum)
                                (newline p))])
                ($write-helper who path mode #f do-write)))))
  ;; write a list of datum in fasl format
  (define $write-data-fasl
    (lambda (who path mode data)
      (pcheck-list
       (data)
       (let ([do-write (lambda (p)
                         (let loop ([data data])
                           (unless (null? data)
                             (fasl-write (car data) p)
                             (loop (cdr data)))))])
         ($write-helper who path mode #t do-write)))))
  ;; write a single datum in fasl format
  (define $write-datum-fasl
    (lambda (who path mode datum)
      (let ([do-write (lambda (p) (fasl-write datum p))])
        ($write-helper who path mode #t do-write))))
  (define $write-u8vec
    (lambda (who path mode u8vec)
      (pcheck-bytevector
       (u8vec)
       (let ([do-write (lambda (p) (put-bytevector p u8vec))])
         ($write-helper who path mode #t do-write)))))

  ;; error by default when file already exist
  (define-who write-lines
    (lambda (path lines)
      ($write-lines who path 'error lines)))
  (define-who write-string
    (lambda (path str)
      ($write-string who path 'error str)))
  (define-who write-chars
    (lambda (path chars)
      ($write-chars who path 'error chars)))
  (define-who write-data
    (lambda (path data)
      ($write-data who path 'error data)))
  (define-who write-datum
    (lambda (path datum)
      ($write-datum who path 'error datum)))
  (define-who write-data-fasl
    (lambda (path data)
      ($write-data-fasl who path 'error data)))
  (define-who write-datum-fasl
    (lambda (path datum)
      ($write-datum-fasl who path 'error datum)))
  (define-who write-u8vec
    (lambda (path u8vec)
      ($write-u8vec who path 'error u8vec)))

  ;; truncate by default
  (define-who write-lines!
    (lambda (path lines)
      ($write-lines who path 'truncate lines)))
  (define-who write-string!
    (lambda (path str)
      ($write-string who path 'truncate str)))
  (define-who write-chars!
    (lambda (path chars)
      ($write-chars who path 'truncate chars)))
  (define-who write-data!
    (lambda (path data)
      ($write-data who path 'truncate data)))
  (define-who write-datum!
    (lambda (path datum)
      ($write-datum who path 'truncate datum)))
  (define-who write-data-fasl!
    (lambda (path data)
      ($write-data-fasl who path 'truncate data)))
  (define-who write-datum-fasl!
    (lambda (path datum)
      ($write-datum-fasl who path 'truncate datum)))
  (define-who write-u8vec!
    (lambda (path u8vec)
      ($write-u8vec who path 'truncate u8vec)))

  ;; append by default
  (define-who write-lines>>
    (lambda (path lines)
      ($write-lines who path 'append lines)))
  (define-who write-string>>
    (lambda (path str)
      ($write-string who path 'append str)))
  (define-who write-chars>>
    (lambda (path chars)
      ($write-chars who path 'append chars)))
  (define-who write-data>>
    (lambda (path data)
      ($write-data who path 'append data)))
  (define-who write-datum>>
    (lambda (path datum)
      ($write-datum who path 'append datum)))
  (define-who write-data-fasl>>
    (lambda (path data)
      ($write-data-fasl who path 'append data)))
  (define-who write-datum-fasl>>
    (lambda (path datum)
      ($write-datum-fasl who path 'append datum)))
  (define-who write-u8vec>>
    (lambda (path u8vec)
      ($write-u8vec who path 'append u8vec)))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   get/put operations on ports
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define $bad-endianness
    (lambda (who e)
      (errorf who "bad endianness: ~a, should be either 'big or 'little" e)))
  (define $no-enough-data
    (lambda (who expected)
      (errorf who "failed to get ~a from port" expected)))
  (define $invalid-value
    (lambda (who n expected)
      (errorf who "invalid value: ~a, expected ~a" n expected)))

  ;; from ChezScheme bytevector.ss
  (define-syntax signed-value-pred
    (lambda (x)
      (syntax-case x ()
        [(_ ?bits)
         (let ([bits (syntax->datum #'?bits)])
           (unless (and (fixnum? bits)
                        (fx> bits 0)
                        (fx= (* (fxquotient bits 8) 8) bits))
             (syntax-error #'?bits "invalid bits"))
           (cond
            [(fx<= bits (fixnum-width))
             (with-syntax ([limit- (- (expt 2 (- bits 1)))]
                           [limit+ (- (expt 2 (- bits 1)) 1)])
               #'(lambda (k) (and (fixnum? k) (fx<= limit- k limit+))))]
            [(fx= bits (fixnum-width)) #'fixnum?]
            [else
             (with-syntax ([limit- (- (expt 2 (- bits 1)))]
                           [limit+ (- (expt 2 (- bits 1)) 1)])
               #'(lambda (k)
                   (or (fixnum? k)
                       (and (bignum? k) (<= limit- k limit+)))))]))])))
  (define-syntax unsigned-value-pred
    (lambda (x)
      (syntax-case x ()
        [(_ ?bits)
         (let ([bits (syntax->datum #'?bits)])
           (unless (and (fixnum? bits)
                        (fx> bits 0)
                        (fx= (* (fxquotient bits 8) 8) bits))
             (syntax-error #'?bits "invalid bits"))
           (cond
            [(fx< bits (fixnum-width))
             (with-syntax ([limit+ (expt 2 bits)])
               #'(lambda (k) (and (fixnum? k) (#%$fxu< k limit+))))]
            [(fx= bits (fixnum-width))
             #'(lambda (k) (and (fixnum? k) (fx>= k 0)))]
            [else
             (with-syntax ([limit+ (- (expt 2 bits) 1)])
               #'(lambda (k)
                   (if (fixnum? k)
                       (fx>= k 0)
                       (and (bignum? k) (<= 0 k limit+)))))]))])))
  (define u16? (unsigned-value-pred 16))
  (define u32? (unsigned-value-pred 32))
  (define u64? (unsigned-value-pred 64))
  (define s16? (signed-value-pred 16))
  (define s32? (signed-value-pred 32))
  (define s64? (signed-value-pred 64))


  ;; per-thread bytevectors for IO to avoid
  ;; excessive creations of temporary bytevectors
  (define bv16 (make-thread-parameter (make-bytevector 2 0)))
  (define bv32 (make-thread-parameter (make-bytevector 4 0)))
  (define bv64 (make-thread-parameter (make-bytevector 8 0)))

  ;; All subsequent procedures raise an exception when there are no enough
  ;; bytes to make up the number.

  (define-who get-u16
    (case-lambda
      [(p) (get-u16 p 'little)]
      [(p end)
       (pcheck-input-binary-port
        (p)
        (let* ([count 2] [bv (bv16)]
               [go (lambda (get) (let ([c (get-bytevector-n! p bv 0 count)])
                                   (if (fx< c count)
                                       ($no-enough-data who 'u16)
                                       (get))))])
          (case end
            [big    (go (lambda () (bytevector-u16-ref bv 0 (endianness big))))]
            [little (go (lambda () (bytevector-u16-ref bv 0 (endianness little))))]
            [else ($bad-endianness who end)])))]))
  (define-who get-u32
    (case-lambda
      [(p) (get-u32 p 'little)]
      [(p end)
       (let* ([count 4] [bv (bv32)]
              [go (lambda (get) (let ([c (get-bytevector-n! p bv 0 count)])
                                  (if (fx< c count)
                                      ($no-enough-data who 'u32)
                                      (get))))])
         (case end
           [big    (go (lambda () (bytevector-u32-ref bv 0 (endianness big))))]
           [little (go (lambda () (bytevector-u32-ref bv 0 (endianness little))))]
           [else ($bad-endianness who end)]))]))
  (define-who get-u64
    (case-lambda
      [(p) (get-u64 p 'little)]
      [(p end)
       (let* ([count 8] [bv (bv64)]
              [go (lambda (get) (let ([c (get-bytevector-n! p bv 0 count)])
                                  (if (fx< c count)
                                      ($no-enough-data who 'u64)
                                      (get))))])
         (case end
           [big    (go (lambda () (bytevector-u64-ref bv 0 (endianness big))))]
           [little (go (lambda () (bytevector-u64-ref bv 0 (endianness little))))]
           [else ($bad-endianness who end)]))]))

  (define-who get-s16
    (case-lambda
      [(p) (get-s16 p 'little)]
      [(p end)
       (pcheck-input-binary-port
        (p)
        (let* ([count 2] [bv (bv16)]
               [go (lambda (get) (let ([c (get-bytevector-n! p bv 0 count)])
                                   (if (fx< c count)
                                       ($no-enough-data who 's16)
                                       (get))))])
          (case end
            [big    (go (lambda () (bytevector-s16-ref bv 0 (endianness big))))]
            [little (go (lambda () (bytevector-s16-ref bv 0 (endianness little))))]
            [else ($bad-endianness who end)])))]))
  (define-who get-s32
    (case-lambda
      [(p) (get-s32 p 'little)]
      [(p end)
       (let* ([count 4] [bv (bv32)]
              [go (lambda (get) (let ([c (get-bytevector-n! p bv 0 count)])
                                  (if (fx< c count)
                                      ($no-enough-data who 's32)
                                      (get))))])
         (case end
           [big    (go (lambda () (bytevector-s32-ref bv 0 (endianness big))))]
           [little (go (lambda () (bytevector-s32-ref bv 0 (endianness little))))]
           [else ($bad-endianness who end)]))]))
  (define-who get-s64
    (case-lambda
      [(p) (get-s64 p 'little)]
      [(p end)
       (let* ([count 8] [bv (bv64)]
              [go (lambda (get) (let ([c (get-bytevector-n! p bv 0 count)])
                                  (if (fx< c count)
                                      ($no-enough-data who 's64)
                                      (get))))])
         (case end
           [big    (go (lambda () (bytevector-s64-ref bv 0 (endianness big))))]
           [little (go (lambda () (bytevector-s64-ref bv 0 (endianness little))))]
           [else ($bad-endianness who end)]))]))

  (define-who put-u16
    (case-lambda
      [(p n) (put-u16 p n 'little)]
      [(p n end)
       (pcheck-output-binary-port
        (p)
        (if (u16? n)
            (let ([bv (bv16)])
              (case end
                [big    (bytevector-u16-set! bv 0 n (endianness big))
                        (put-bytevector p bv)]
                [little (bytevector-u16-set! bv 0 n (endianness little))
                        (put-bytevector p bv)]
                [else ($bad-endianness who end)]))
            ($invalid-value who n 'u16)))]))
  (define-who put-u32
    (case-lambda
      [(p n) (put-u32 p n 'little)]
      [(p n end)
       (pcheck-output-binary-port
        (p)
        (if (u32? n)
            (let ([bv (bv32)])
              (case end
                [big    (bytevector-u32-set! bv 0 n (endianness big))
                        (put-bytevector p bv)]
                [little (bytevector-u32-set! bv 0 n (endianness little))
                        (put-bytevector p bv)]
                [else ($bad-endianness who end)]))
            ($invalid-value who n 'u32)))]))
  (define-who put-u64
    (case-lambda
      [(p n) (put-u64 p n 'little)]
      [(p n end)
       (pcheck-output-binary-port
        (p)
        (if (u64? n)
            (let ([bv (bv64)])
              (case end
                [big    (bytevector-u64-set! bv 0 n (endianness big))
                        (put-bytevector p bv)]
                [little (bytevector-u64-set! bv 0 n (endianness little))
                        (put-bytevector p bv)]
                [else ($bad-endianness who end)]))
            ($invalid-value who n 'u64)))]))

  (define-who put-s16
    (case-lambda
      [(p n) (put-s16 p n 'little)]
      [(p n end)
       (pcheck-output-binary-port
        (p)
        (if (s16? n)
            (let ([bv (bv16)])
              (case end
                [big    (bytevector-s16-set! bv 0 n (endianness big))
                        (put-bytevector p bv)]
                [little (bytevector-s16-set! bv 0 n (endianness little))
                        (put-bytevector p bv)]
                [else ($bad-endianness who end)]))
            ($invalid-value who n 's16)))]))
  (define-who put-s32
    (case-lambda
      [(p n) (put-s32 p n 'little)]
      [(p n end)
       (pcheck-output-binary-port
        (p)
        (if (s32? n)
            (let ([bv (bv32)])
              (case end
                [big    (bytevector-s32-set! bv 0 n (endianness big))
                        (put-bytevector p bv)]
                [little (bytevector-s32-set! bv 0 n (endianness little))
                        (put-bytevector p bv)]
                [else ($bad-endianness who end)]))
            ($invalid-value who n 's32)))]))
  (define-who put-s64
    (case-lambda
      [(p n) (put-s64 p n 'little)]
      [(p n end)
       (pcheck-output-binary-port
        (p)
        (if (s64? n)
            (let ([bv (bv64)])
              (case end
                [big    (bytevector-s64-set! bv 0 n (endianness big))
                        (put-bytevector p bv)]
                [little (bytevector-s64-set! bv 0 n (endianness little))
                        (put-bytevector p bv)]
                [else ($bad-endianness who end)]))
            ($invalid-value who n 's64)))]))


  ;; big-endian aliases
  (define GET-U16 (lambda (p) (get-u16 p 'big)))
  (define GET-U32 (lambda (p) (get-u32 p 'big)))
  (define GET-U64 (lambda (p) (get-u64 p 'big)))
  (define GET-S16 (lambda (p) (get-s16 p 'big)))
  (define GET-S32 (lambda (p) (get-s32 p 'big)))
  (define GET-S64 (lambda (p) (get-s64 p 'big)))

  (define PUT-U16 (lambda (p n) (put-u16 p n 'big)))
  (define PUT-U32 (lambda (p n) (put-u32 p n 'big)))
  (define PUT-U64 (lambda (p n) (put-u64 p n 'big)))
  (define PUT-S16 (lambda (p n) (put-s16 p n 'big)))
  (define PUT-S32 (lambda (p n) (put-s32 p n 'big)))
  (define PUT-S64 (lambda (p n) (put-s64 p n 'big)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   path utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  #|doc
  Build paths. A multi-arity variant of builtin `path-build`.
  |#
  (define make-path
    (lambda (p . p*)
      (pcheck ([string? p] [(lambda (x) (andmap string? x)) p*])
              (if (null? p*)
                  p
                  (let loop ([res p] [pp p*])
                    (if (null? pp)
                        res
                        (path-build res (loop (car pp) (cdr pp)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   file info
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; file stats

  (define-record-type $file-stat
    (nongenerative)
    (fields (immutable type   file-stat-type)
            (immutable mode   file-stat-mode)
            (immutable inode  file-stat-inode)
            (immutable owner  file-stat-owner)
            (immutable group  file-stat-group)
            (immutable nlinks file-stat-nlinks)
            (immutable size   file-stat-size)
            (immutable blocks file-stat-blocks)
            (immutable dev-major file-stat-dev-major)
            (immutable dev-minor file-stat-dev-minor)
            ;;(immutable attributes  file-stat-attributes)
            (immutable access-time       file-stat-access-time)
            (immutable modification-time file-stat-modification-time)
            (immutable change-time       file-stat-change-time)
            (immutable creation-time     file-stat-creation-time)))

  (define *file-types* '((#o140000 . FT_socket)
                         (#o120000 . FT_symlink)
                         (#o100000 . FT_regular)
                         (#o60000  . FT_block)
                         (#o40000  . FT_dir)
                         (#o20000  . FT_chardev)
                         (#o10000  . FT_fifo)))

  (define-who file-type->symbol
    (lambda (t)
      (let ([s (assoc t *file-types*)])
        (if s (cdr s) (errorf who "invalid file type: ~a" t)))))

  (define $statx
    (let ([ffi-statx (foreign-procedure "chezpp_statx" (string boolean) scheme-object)])
      (lambda (who path follow-link?)
        (pcheck ([(lambda (x) (file-exists? x follow-link?)) path] [boolean? follow-link?])
                (let ([x (ffi-statx path follow-link?)])
                  (if (vector? x)
                      x
                      ($err-file who path x)))))))
  ;; stx_attributes, stx_mnt_id, etc, are left out
  (define $statx-type (lambda (v) (vector-ref v 0)))
  (define $statx-mode (lambda (v) (vector-ref v 1)))
  (define $statx-uid  (lambda (v) (vector-ref v 3)))
  (define $statx-gid  (lambda (v) (vector-ref v 4)))
  (define $statx-nlinks (lambda (v) (vector-ref v 2)))
  (define $statx-inode  (lambda (v) (vector-ref v 13)))
  (define $statx-size   (lambda (v) (vector-ref v 14)))
  (define $statx-blocks (lambda (v) (vector-ref v 15)))
  (define $statx-atime  (lambda (v) (let ([t (vector-ref v 5)])
                                      (and t (make-time 'time-utc (vector-ref v 6) t)))))
  (define $statx-ctime  (lambda (v) (let ([t (vector-ref v 7)])
                                      (and t (make-time 'time-utc (vector-ref v 8) t)))))
  (define $statx-mtime  (lambda (v) (let ([t (vector-ref v 9)])
                                      (and t (make-time 'time-utc (vector-ref v 10) t)))))
  (define $statx-btime  (lambda (v) (let ([t (vector-ref v 11)])
                                      (and t (make-time 'time-utc (vector-ref v 12) t)))))
  (define $statx-dev-major (lambda (v) (vector-ref v 16)))
  (define $statx-dev-minor (lambda (v) (vector-ref v 17)))

  (define-who file-stat
    (case-lambda [(path)
                  (file-stat path #t)]
                 [(path follow-link?)
                  (let* ([v ($statx who path follow-link?)]
                         [type ($statx-type v)]
                         [mode ($statx-mode v)]
                         [uid  ($statx-uid v)]
                         [gid  ($statx-gid v)]
                         [nlinks ($statx-nlinks v)]
                         [inode  ($statx-inode v)]
                         [size   ($statx-size v)]
                         [blocks ($statx-blocks v)]
                         [atime ($statx-atime v)]
                         [ctime ($statx-ctime v)]
                         [mtime ($statx-mtime v)]
                         [btime ($statx-btime v)]
                         [dev-major ($statx-dev-major v)]
                         [dev-minor ($statx-dev-minor v)])
                    (make-$file-stat
                     type mode inode uid gid nlinks size blocks dev-major dev-minor
                     atime mtime ctime btime))]))


  (define-who file-access-time
    (case-lambda [(path)
                  ($statx-atime ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-atime ($statx who path follow-link?))]))
  (define-who file-modification-time
    (case-lambda [(path)
                  ($statx-mtime ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-mtime ($statx who path follow-link?))]))
  (define-who file-change-time
    (case-lambda [(path)
                  ($statx-ctime ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-ctime ($statx who path follow-link?))]))
  (define-who file-creation-time
    (case-lambda [(path)
                  ($statx-btime ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-btime ($statx who path follow-link?))]))
  (define-who file-type
    (case-lambda [(path)
                  (file-type->symbol ($statx-type ($statx who path #t)))]
                 [(path follow-link?)
                  (file-type->symbol ($statx-type ($statx who path follow-link?)))]))
  (define-who file-mode
    (case-lambda [(path)
                  ($statx-mode ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-mode ($statx who path follow-link?))]))
  (define-who file-inode
    (case-lambda [(path)
                  ($statx-inode ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-inode ($statx who path follow-link?))]))
  (define-who file-nlinks
    (case-lambda [(path)
                  ($statx-nlinks ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-nlinks ($statx who path follow-link?))]))
  (define-who file-dev-major
    (case-lambda [(path)
                  ($statx-dev-major ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-dev-major ($statx who path follow-link?))]))
  (define-who file-dev-minor
    (case-lambda [(path)
                  ($statx-dev-minor ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-dev-minor ($statx who path follow-link?))]))
  (define-who file-blocks
    (case-lambda [(path)
                  ($statx-blocks ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-blocks ($statx who path follow-link?))]))
  ;; this is implemented using port above
  #;
  (define-who file-size
  (case-lambda [(path)
  ($statx-size ($statx who path #t))]
  [(path follow-link?)
  ($statx-size ($statx who path follow-link?))]))

  (define-who file-owner
    (case-lambda [(path)
                  ($statx-uid ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-uid ($statx who path follow-link?))]))
  (define-who file-group
    (case-lambda [(path)
                  ($statx-gid ($statx who path #t))]
                 [(path follow-link?)
                  ($statx-gid ($statx who path follow-link?))]))


  (define $file-size
    (lambda (who path)
      ;; dir and regular files are both OK
      (pcheck ([file-exists? path])
              (call-with-port (open-file-input-port path)
                (lambda (p) (file-length p))))))

  (define-who file-size
    (lambda (path) ($file-size who path)))

  #|doc
  Return a human-readable string of the file size.
  |#
  (define-who file-size-h
    (lambda (path)
      (define 1K 1024)
      (define 1M (fx* 1K 1024))
      (define 1G (fx* 1M 1024))
      (let ([size ($file-size who path)])
        (cond [(fx< size 1K) (format "~,2f B" size)]
              [(fx< size 1M) (format "~,2f KiB" (inexact (/ size 1K)))]
              [(fx< size 1G) (format "~,2f MiB" (inexact (/ size 1M)))]
              [else (format "~,2f GiB" (inexact (/ size 1G)))]))))


  (define-who file-readable?
    (case-lambda [(path) (file-readable? path #t)]
                 [(path follow-link?)
                  (pcheck ([(lambda (x) (file-exists? x follow-link?)) path])
                          (let ([m (get-mode path follow-link?)])
                            (fxlogbit? 8 m)))]))
  (define-who file-writable?
    (case-lambda [(path) (file-writable? path #t)]
                 [(path follow-link?)
                  (pcheck ([(lambda (x) (file-exists? x follow-link?)) path])
                          (let ([m (get-mode path follow-link?)])
                            (fxlogbit? 7 m)))]))
  (define-who file-executable?
    (case-lambda [(path) (file-executable? path #t)]
                 [(path follow-link?)
                  (pcheck ([(lambda (x) (file-exists? x follow-link?)) path])
                          (let ([m (get-mode path follow-link?)])
                            (fxlogbit? 6 m)))]))

  (define-who file-hidden?
    (case-lambda [(path) (file-hidden? path #t)]
                 [(path follow-link?)
                  (pcheck ([(lambda (x) (file-exists? x follow-link?)) path])
                          (cond
                           [(unix?)
                            (let ([path (let ([p (if (and follow-link? (file-symbolic-link? path)) (readlink2 path #t) path)])
                                          (if (file-directory? p follow-link?)
                                              (path-last (path-parent (string-append p (string (directory-separator)))))
                                              (path-last p)))])
                              (and (not (string=? path ".")) (not (string=? path ".."))
                                   (char=? #\. (string-ref path 0))))]
                           [else (todo who)]))]))

  #|doc
  A file is special if it is either a socket, a block/character device, or a FIFO.
  |#
  (define-who file-special?
    (case-lambda
      [(path) (file-special? path #t)]
      [(path follow-link?)
       (pcheck ([string? path] [(lambda (x) (file-exists? x follow-link?)) path])
               (let ([t (file-type path follow-link?)])
                 (bool (memq t '(FT_socket FT_block FT_chardev FT_fifo)))))]))


  #|doc
  Test whether the two paths, `path1` and `path2`, refer to the same file.

  On Linux, this procedure compares the files' inodes and their filesystem IDs.
  |#
  (define-who same-file?
    (case-lambda
      [(path1 path2) (same-file? path1 path2 #t)]
      [(path1 path2 follow-link?)
       (pcheck ([string? path1 path2] [(lambda (x) (file-exists? x follow-link?)) path1 path2])
               (let* ([st1 (file-stat path1 follow-link?)]
                      [st2 (file-stat path2 follow-link?)]
                      [inode1 (file-stat-inode st1)]
                      [major1 (file-stat-dev-major st1)]
                      [minor1 (file-stat-dev-minor st1)]
                      [inode2 (file-stat-inode st2)]
                      [major2 (file-stat-dev-major st2)]
                      [minor2 (file-stat-dev-minor st2)])
                 (and (= inode1 inode2) (= major1 major2) (= minor1 minor2))))]))


  #|doc
  Test whether two files have the same content.
  `path1` and `path2` must point to regular files. File contents are compared byte by byte.
  Symlinks are always followed.
  |#
  (define-who same-file-contents?
    (lambda (path1 path2)
      (pcheck ([string? path1 path2])
              (if (and (file-regular? path1 #t) (file-regular? path2 #t))
                  (call-with-port (open-file-input-port path1)
                    (lambda (p1)
                      (call-with-port (open-file-input-port path2)
                        (lambda (p2)
                          (and (fx= (file-length p1) (file-length p2))
                               (let loop ()
                                 (let ([b1 (get-u8 p1)] [b2 (get-u8 p2)])
                                   (if (eof-object? b1)
                                       #t
                                       (if (fx= b1 b2) (loop) #f)))))))))
                  #f))))



;;;; file modes

  (define $mode-num? (lambda (x) (and (fixnum? x) (fx<= 0 x #b111111111111))))
  (define $valid-mode-sym?  (lambda (x) (memq x '(r w x))))
  (define $valid-smode-sym? (lambda (x) (memq x '(su sg t))))

  (define $valid-modes?
    (lambda (m) (and (list? m) (andmap $valid-mode-sym? m) (unique? m))))
  (define $valid-smodes?
    (lambda (m) (and (list? m) (andmap $valid-smode-sym? m) (unique? m))))

  (define file-mode->symbols
    (lambda (mode)
      (pcheck ([$mode-num? mode])
              (let* ([ss '((su . 11) (sg . 10) (t . 9))]
                     [us '((r . 8) (w . 7) (x . 6))]
                     [gs '((r . 5) (w . 4) (x . 3))]
                     [os '((r . 2) (w . 1) (x . 0))]
                     [scanX (lambda (xs)
                             (lambda (m)
                               (fold-left
                                (lambda (m p) (if (fxlogbit? (cdr p) mode)
                                                  (snoc! m (car p))
                                                  m))
                                m xs)))]
                     [s (scanX ss)]
                     [u (scanX us)]
                     [g (scanX gs)]
                     [o (scanX os)])
                `(,(s '()) ,(u '()) ,(g '()) ,(o '()))))))
  (define symbols->file-mode
    (lambda (smode umode gmode omode)
      (pcheck ([list? smode umode gmode omode]
               [$valid-smodes? smode]
               [$valid-modes? umode gmode omode])
              (let* ([ss '((t . 9) (sg . 10) (su . 11))]
                     [us '((x . 6) (w . 7) (r . 8))]
                     [gs '((x . 3) (w . 4) (r . 5))]
                     [os '((x . 0) (w . 1) (r . 2))]
                     [scanX (lambda (mode xs)
                             (lambda (m)
                               (fold-left
                                (lambda (m s) (if (memq s (map car xs))
                                                  (fxlogbit1 (cdr (assoc s xs)) m)
                                                  m))
                                m mode)))]
                     ;; scan symbols in each type of mode and set the bits
                     [s (scanX smode ss)]
                     [u (scanX umode us)]
                     [g (scanX gmode gs)]
                     [o (scanX omode os)])
                (o (g (u (s 0))))))))


  (define $file-chmod
    (case-lambda
      ;; numeric or symbolic for user mode
      [(who path mode)
       (cond [($mode-num? mode)
              (chmod path mode)]
             [(list? mode)
              ($file-chmod who path '() mode '() '())]
             [else (errorf who "invalid file mode: ~a" mode)])]
      [(who path umode gmode)
       ($file-chmod who path '() umode gmode '())]
      [(who path umode gmode omode)
       ($file-chmod who path '() umode gmode omode)]
      [(who path smode umode gmode omode)
       (pcheck ([file-exists? path]
                [list? smode umode gmode omode])
               (let* ([m (get-mode path #t)]
                      [sm (file-mode->symbols m)]
                      [filter-mode (lambda (mode i)
                                     (if mode
                                         (if (null? mode)
                                             mode
                                             (record-case mode
                                               [+ d* (list+ (list-ref sm i) d*)]
                                               [- d* (list- (list-ref sm i) d*)]
                                               [else mode]))
                                         (list-ref sm i)))]
                      [s (filter-mode smode 0)]
                      [u (filter-mode umode 1)]
                      [g (filter-mode gmode 2)]
                      [o (filter-mode omode 3)])
                 (pcheck ([$valid-smodes? s]
                          [$valid-modes? u g o])
                         (chmod path (symbols->file-mode s u g o)))))]))


  #|doc
  Change the permission bits of a file.

  If `path` is a symlink, it is always dereferenced.

  If only one mode is given, it can be either in numerical form (as returned by `get-mode`),
  or in symbolic form (see below).
  In the former case, it specifies all modes at once.
  In the latter, it specifies the user mode to be modified.

  If more than one mode is given, they must be in symbolic form.
  If two modes are given, they represent user mode and group mode;
  If three modes are given, they represent user mode, group mode and others mode;
  If four modes are given, they represent special mode, user mode, group mode, and others mode.

  Special modes are set-user-ID, set-group-ID and sticky bit.


  Symbolic Permission Form:

  For user, group and others bits, the symbolic form supports specifying permission
  bits in a list of symbols consisting of 'r, 'w, and 'x.
  For example, '(r) means setting the (say, user's) bits to read only;
  and '(r w x) sets all bits.

  Set-user-ID, set-group-ID and sticky bit are represented as 'su, 'sg and 't, respectively.
  If mode is '(), all bits are cleared; if mode is #f, all bits are left unchanged.
  Each symbol in the list can only appear once. They order of symbols is unimportant.

  Adding or Subtracting Bits:

  In addition to the above way of specifying all bits to be set using the symbolic form,
  one can also add '+ and '- in front of the list to only add to or subtract from the file's
  bits the given bits.
  For example, '(- x) removes the execution bit; '(+ w x) adds the write and execution bit.
  Nothing happens when trying to remove a bit that's not set or
  when trying to add a bit that already exists.
  |#
  (define-who file-chmod
    (lambda (path mode . rest)
      (apply $file-chmod who path mode rest)))

  #|doc
  Change the special mode only.
  `mode` must be in symbolic form.
  |#
  (define-who file-chmod-s
    (lambda (path mode)
      ($file-chmod who mode #f #f #f)))
  #|doc
  Change user mode only.
  `mode` must be in symbolic form.
  |#
  (define-who file-chmod-u
    (lambda (path mode)
      ($file-chmod who #f mode #f #f)))
  #|doc
  Change group mode only.
  `mode` must be in symbolic form.
  |#
  (define-who file-chmod-g
    (lambda (path mode)
      ($file-chmod who #f #f mode #f)))
  #|doc
  Change others mode only.
  `mode` must be in symbolic form.
  |#
  (define-who file-chmod-o
    (lambda (path mode)
      ($file-chmod who #f #f #f mode)))
  #|doc
  Change all modes except the special mode.
  `mode` must be in symbolic form.
  |#
  (define-who file-chmod-a
    (lambda (path mode)
      ($file-chmod who #f mode mode mode)))



  (record-writer (type-descriptor $file-stat)
                 (lambda (r p wr)
                   (display "#[stat " p)
                   (newline p)
                   (display (format "~ttype:  ~a~n" (file-type->symbol (file-stat-type r))) p)
                   (display (format "~tmode:  #o~a ~a~n" (number->string (file-stat-mode r) 8) (file-mode->symbols (file-stat-mode r))) p)
                   (display (format "~towner: ~a~n" (file-stat-owner r)) p)
                   (display (format "~tgroup: ~a~n" (file-stat-group r)) p)
                   (display (format "~tinode: ~a~n" (file-stat-inode r)) p)
                   (display (format "~tnlinks: ~a~n" (file-stat-nlinks r)) p)
                   (display (format "~tsize:   ~a~n" (file-stat-size r)) p)
                   (display (format "~tblocks: ~a~n" (file-stat-blocks r)) p)
                   (display (format "~tdev-major: ~a~n" (file-stat-dev-major r)) p)
                   (display (format "~tdev-minor: ~a~n" (file-stat-dev-minor r)) p)
                   (display (format "~tatime: ~a~n" (file-stat-access-time r)) p)
                   (display (format "~tmtime: ~a~n" (file-stat-modification-time r)) p)
                   (display (format "~tctime: ~a~n" (file-stat-change-time r)) p)
                   (display (format "~tbtime: ~a" (file-stat-creation-time r)) p)
                   (display "]" p)))

  )
