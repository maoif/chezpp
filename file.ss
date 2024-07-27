#!chezscheme

(library (chezpp file)
  (export read-lines read-string read-chars read-data read-datum read-data-fasl read-datum-fasl
          write-lines write-string write-chars write-data write-datum write-data-fasl write-datum-fasl
          write-lines! write-string! write-chars! write-data! write-datum! write-data-fasl! write-datum-fasl!
          write-lines>> write-string>> write-chars>> write-data>> write-datum>> write-data-fasl>> write-datum-fasl>>

          get-u16 get-u32 get-u64 get-s16 get-s32 get-s64
          put-u16 put-u32 put-u64 put-s16 put-s32 put-s64

          GET-U16 GET-U32 GET-U64 GET-S16 GET-S32 GET-S64
          PUT-U16 PUT-U32 PUT-U64 PUT-S16 PUT-S32 PUT-S64)
  (import (chezscheme)
          (chezpp internal)
          (chezpp utils)
          (chezpp list)
          (chezpp io))



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

  (define all-strings? (lambda (x) (andmap string? x)))
  (define all-chars?   (lambda (x) (andmap char? x)))

  (define $bad-file
    (lambda (who p)
      (errorf who "existing file is not regular: ~a" p)))
  (define $file-exists-error
    (lambda (who p)
      (errorf who "file already exists: ~a" p)))
  (define $bad-write-mode
    (lambda (who mode)
      (errorf who "unknown write mode: ~a, should be one of '(error truncate append)" mode)))

  ;; mode when file exists: 'error, 'truncate, 'append
  (define $write-helper
    (lambda (who path mode bin? writer)
      (let ([go (lambda (mode)
                  (let ([op (if mode
                                (case mode
                                  [truncate (file-options replace)]
                                  [append   (file-options no-truncate no-fail append)]
                                  [else ($bad-write-mode who mode)])
                                (file-options))])
                    (call-with-port (open-file-output-port path op (buffer-mode block)
                                                           (if bin? #f (current-transcoder)))
                      (lambda (p) (writer p)))))])
        (if (file-exists? path)
            (if (eq? mode 'error)
                ($file-exists-error who path)
                (if (file-regular? path)
                    (go mode)
                    ($bad-file who path)))
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


  )
