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

(define rich-elf64le
  (lambda ()
    (let ([bytes (make-bytevector 960 0)]
          [little (endianness little)])
      (define set-section!
        (lambda (index name type offset size link info alignment entry-size)
          (let ([at (+ 512 (* index 64))])
            (bytevector-u32-set! bytes at name little)
            (bytevector-u32-set! bytes (+ at 4) type little)
            (bytevector-u64-set! bytes (+ at 24) offset little)
            (bytevector-u64-set! bytes (+ at 32) size little)
            (bytevector-u32-set! bytes (+ at 40) link little)
            (bytevector-u32-set! bytes (+ at 44) info little)
            (bytevector-u64-set! bytes (+ at 48) alignment little)
            (bytevector-u64-set! bytes (+ at 56) entry-size little))))
      (bytevector-copy! (minimal-elf64le) 0 bytes 0 64)
      (bytevector-u64-set! bytes 32 64 little)
      (bytevector-u64-set! bytes 40 512 little)
      (bytevector-u16-set! bytes 56 1 little)
      (bytevector-u16-set! bytes 60 7 little)
      (bytevector-u16-set! bytes 62 1 little)
      (bytevector-u32-set! bytes 64 1 little)
      (bytevector-u32-set! bytes 68 5 little)
      (bytevector-u64-set! bytes 72 120 little)
      (bytevector-u64-set! bytes 80 120 little)
      (bytevector-u64-set! bytes 96 4 little)
      (bytevector-u64-set! bytes 104 8 little)
      (bytevector-u64-set! bytes 112 1 little)
      (bytevector-copy! (string->utf8
                         "\x0;.shstrtab\x0;.strtab\x0;.symtab\x0;.note\x0;.rela\x0;.shndx\x0;")
                        0 bytes 128 46)
      (bytevector-copy! (string->utf8 "\x0;foo\x0;") 0 bytes 192 5)
      (bytevector-u32-set! bytes 232 1 little)
      (bytevector-u8-set! bytes 236 #x12)
      (bytevector-u16-set! bytes 238 #xffff little)
      (bytevector-u64-set! bytes 240 #x1234 little)
      (bytevector-u64-set! bytes 248 4 little)
      (bytevector-u32-set! bytes 272 4 little)
      (bytevector-u32-set! bytes 276 4 little)
      (bytevector-u32-set! bytes 280 3 little)
      (bytevector-copy! (string->utf8 "GNU\x0;") 0 bytes 284 4)
      (bytevector-copy! (bytevector 1 2 3 4) 0 bytes 288 4)
      (bytevector-u64-set! bytes 304 #x10 little)
      (bytevector-u64-set! bytes 312 (+ (ash 1 32) 7) little)
      (bytevector-s64-set! bytes 320 -4 little)
      (bytevector-u32-set! bytes 336 0 little)
      (bytevector-u32-set! bytes 340 1 little)
      (set-section! 1 1 3 128 46 0 0 1 0)
      (set-section! 2 11 3 192 5 0 0 1 0)
      (set-section! 3 19 2 208 48 2 1 8 24)
      (set-section! 4 27 7 272 20 0 0 4 0)
      (set-section! 5 33 4 304 24 3 1 8 24)
      (set-section! 6 39 18 336 8 3 0 4 4)
      bytes)))

(define rich-elf-with-u32
  (lambda (offset value)
    (let ([bytes (rich-elf64le)])
      (bytevector-u32-set! bytes offset value (endianness little))
      bytes)))

(define rich-elf-with-u64
  (lambda (offset value)
    (let ([bytes (rich-elf64le)])
      (bytevector-u64-set! bytes offset value (endianness little))
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

(mat parse-elf-content

     (let* ([file (parse-elf (rich-elf64le))]
            [program (vector-ref (elf-file-program-headers file) 0)]
            [sections (elf-file-sections file)]
            [symbols (elf-symbol-table-symbols
                      (elf-section-content (vector-ref sections 3)))]
            [symbol (vector-ref symbols 1)]
            [notes (elf-note-table-notes
                    (elf-section-content (vector-ref sections 4)))]
            [relocations (elf-relocation-table-relocations
                          (elf-section-content (vector-ref sections 5)))])
       (and (= 1 (elf-program-header-type program))
            (equal? (bytevector 0 0 0 0) (elf-program-header-data program))
            (equal? ".symtab" (elf-section-header-name
                                (elf-section-header (vector-ref sections 3))))
            (equal? "foo" (elf-symbol-name symbol))
            (= 1 (elf-symbol-section-index symbol))
            (= #x1234 (elf-symbol-value symbol))
            (equal? "GNU" (elf-note-name (vector-ref notes 0)))
            (equal? (bytevector 1 2 3 4)
                    (elf-note-descriptor (vector-ref notes 0)))
            (= 1 (elf-relocation-symbol-index (vector-ref relocations 0)))
            (= 7 (elf-relocation-type (vector-ref relocations 0)))
            (= -4 (elf-relocation-addend (vector-ref relocations 0)))))

     ;; error: a symbol table must link to a string table.
     (error? (parse-elf (rich-elf-with-u32 (+ 512 (* 3 64) 40) 4)))

     ;; error: a symbol name index must identify a terminated linked-table string.
     (error? (parse-elf (rich-elf-with-u32 232 99)))

     ;; error: a relocation table must link to a symbol table.
     (error? (parse-elf (rich-elf-with-u32 (+ 512 (* 5 64) 40) 2)))

     ;; error: note name padding bytes must be zero.
     (error?
      (let ([bytes (rich-elf-with-u32 272 2)])
        (bytevector-u8-set! bytes 284 (char->integer #\G))
        (bytevector-u8-set! bytes 285 0)
        (bytevector-u8-set! bytes 286 1)
        (parse-elf bytes)))

     ;; error: the section-header table must fit in the input.
     (error? (parse-elf (rich-elf-with-u64 40 800)))

     )
