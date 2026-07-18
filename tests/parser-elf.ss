(import (chezpp)
        (chezpp parser elf))

(define minimal-elf64le
  (lambda ()
    (let ([bytes (make-bytevector 64 0)]
          [little (endianness little)])
      (bytevector-u8-set! bytes 0 #x7f)
      (bytevector-u8-set! bytes 1 (char->integer #\E))
      (bytevector-u8-set! bytes 2 (char->integer #\L))
      (bytevector-u8-set! bytes 3 (char->integer #\F))
      (bytevector-u8-set! bytes 4 2)
      (bytevector-u8-set! bytes 5 1)
      (bytevector-u8-set! bytes 6 1)
      (bytevector-u16-set! bytes 16 1 little)
      (bytevector-u16-set! bytes 18 62 little)
      (bytevector-u32-set! bytes 20 1 little)
      (bytevector-u16-set! bytes 52 64 little)
      (bytevector-u16-set! bytes 54 56 little)
      (bytevector-u16-set! bytes 58 64 little)
      bytes)))

(define minimal-elf
  (lambda (class endianness-name)
    (let* ([elf64? (eq? class 'elf64)]
           [size (if elf64? 64 52)]
           [bytes (make-bytevector size 0)]
           [endian (if (eq? endianness-name 'little)
                       (endianness little) (endianness big))]
           [sizes-at (if elf64? 52 40)])
      (bytevector-u8-set! bytes 0 #x7f)
      (bytevector-u8-set! bytes 1 (char->integer #\E))
      (bytevector-u8-set! bytes 2 (char->integer #\L))
      (bytevector-u8-set! bytes 3 (char->integer #\F))
      (bytevector-u8-set! bytes 4 (if elf64? 2 1))
      (bytevector-u8-set! bytes 5 (if (eq? endianness-name 'little) 1 2))
      (bytevector-u8-set! bytes 6 1)
      (bytevector-u16-set! bytes 16 2 endian)
      (bytevector-u16-set! bytes 18 (if elf64? 62 3) endian)
      (bytevector-u32-set! bytes 20 1 endian)
      (if elf64?
          (bytevector-u64-set! bytes 24 #x12345678 endian)
          (bytevector-u32-set! bytes 24 #x12345678 endian))
      (bytevector-u16-set! bytes sizes-at size endian)
      (bytevector-u16-set! bytes (+ sizes-at 2) (if elf64? 56 32) endian)
      (bytevector-u16-set! bytes (+ sizes-at 6) (if elf64? 64 40) endian)
      bytes)))

(define elf-with-u8
  (lambda (class endianness-name offset value)
    (let ([bytes (minimal-elf class endianness-name)])
      (bytevector-u8-set! bytes offset value)
      bytes)))

(define elf-with-u16
  (lambda (class endianness-name offset value)
    (let ([bytes (minimal-elf class endianness-name)])
      (bytevector-u16-set! bytes offset value
                           (if (eq? endianness-name 'little)
                               (endianness little) (endianness big)))
      bytes)))

(define truncated-elf-header
  (lambda ()
    (let ([bytes (make-bytevector 20)])
      (bytevector-copy! (minimal-elf 'elf64 'little) 0 bytes 0 20)
      bytes)))

(mat parse-elf-records

     (let* ([file (parse-elf (minimal-elf64le))]
            [identification (elf-file-identification file)]
            [header (elf-file-header file)])
       (and (elf-file? file)
            (elf-identification? identification)
            (eq? 'elf64 (elf-identification-class identification))
            (eq? 'little (elf-identification-endianness identification))
            (= 62 (elf-header-machine header))
            (= 0 (vector-length (elf-file-program-headers file)))
            (= 0 (vector-length (elf-file-sections file)))))

     )
(mat parse-elf-headers

     (for-all
      (lambda (configuration)
        (let* ([class (car configuration)]
               [endianness-name (cadr configuration)]
               [file (parse-elf (minimal-elf class endianness-name))]
               [identification (elf-file-identification file)]
               [header (elf-file-header file)])
          (and (eq? class (elf-identification-class identification))
               (eq? endianness-name (elf-identification-endianness identification))
               (= #x12345678 (elf-header-entry header))
               (= (if (eq? class 'elf64) 62 3) (elf-header-machine header)))))
      '((elf32 little) (elf32 big) (elf64 little) (elf64 big)))

     ;; error: the four-byte ELF magic must match exactly.
     (error? (parse-elf (elf-with-u8 'elf64 'little 0 0)))

     ;; error: the ELF class byte must identify ELF32 or ELF64.
     (error? (parse-elf (elf-with-u8 'elf64 'little 4 3)))

     ;; error: the ELF data byte must identify little- or big-endian encoding.
     (error? (parse-elf (elf-with-u8 'elf64 'little 5 0)))

     ;; error: the declared ELF header size must match the selected class.
     (error? (parse-elf (elf-with-u16 'elf32 'big 40 51)))

     ;; error: a complete class-specific ELF header is required.
     (error? (parse-elf (truncated-elf-header)))

     )

(mat parse-elf-file-smoke

     (elf-file? (parse-elf-file "../libchezpp.so"))

     )
