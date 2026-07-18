(library (chezpp parser elf binary)
  (export parser-elf)
  (import (chezpp chez)
          (chezpp parser combinator)
          (chezpp parser elf types)
          (chezpp parser elf validate))

  (define issue (lambda (offset message) (make-elf-issue offset message)))

  (define-record-type decoded-elf-header
    (fields class endianness-name endianness type machine version entry phoff shoff flags
            header-size phentsize raw-phnum shentsize raw-shnum raw-shstrndx sizes-at))

  (define copy-range
    (lambda (bytes offset size)
      (let ([result (make-bytevector size)])
        (bytevector-copy! bytes offset result 0 size)
        result)))

  (define unsigned-ref
    (lambda (bytes offset size endianness)
      (case size
        [(1) (bytevector-u8-ref bytes offset)]
        [(2) (bytevector-u16-ref bytes offset endianness)]
        [(4) (bytevector-u32-ref bytes offset endianness)]
        [(8) (bytevector-u64-ref bytes offset endianness)])))

  (define signed-ref
    (lambda (bytes offset size endianness)
      (case size
        [(4) (bytevector-s32-ref bytes offset endianness)]
        [(8) (bytevector-s64-ref bytes offset endianness)])))

  (define nul-string
    (lambda (bytes index)
      (and (< index (bytevector-length bytes))
           (let loop ([end index])
             (cond [(= end (bytevector-length bytes)) #f]
                   [(zero? (bytevector-u8-ref bytes end))
                    (utf8->string (copy-range bytes index (- end index)))]
                   [else (loop (+ end 1))])))))

  (define raw-section-header
    (lambda (bytes offset class endianness)
      (let ([word (if (eq? class 'elf32) 4 8)])
        (if (eq? class 'elf32)
            (make-elf-section-header
             (unsigned-ref bytes offset 4 endianness) #f
             (unsigned-ref bytes (+ offset 4) 4 endianness)
             (unsigned-ref bytes (+ offset 8) 4 endianness)
             (unsigned-ref bytes (+ offset 12) 4 endianness)
             (unsigned-ref bytes (+ offset 16) 4 endianness)
             (unsigned-ref bytes (+ offset 20) 4 endianness)
             (unsigned-ref bytes (+ offset 24) 4 endianness)
             (unsigned-ref bytes (+ offset 28) 4 endianness)
             (unsigned-ref bytes (+ offset 32) word endianness)
             (unsigned-ref bytes (+ offset 36) word endianness))
            (make-elf-section-header
             (unsigned-ref bytes offset 4 endianness) #f
             (unsigned-ref bytes (+ offset 4) 4 endianness)
             (unsigned-ref bytes (+ offset 8) 8 endianness)
             (unsigned-ref bytes (+ offset 16) 8 endianness)
             (unsigned-ref bytes (+ offset 24) 8 endianness)
             (unsigned-ref bytes (+ offset 32) 8 endianness)
             (unsigned-ref bytes (+ offset 40) 4 endianness)
             (unsigned-ref bytes (+ offset 44) 4 endianness)
             (unsigned-ref bytes (+ offset 48) 8 endianness)
             (unsigned-ref bytes (+ offset 56) 8 endianness))))))

  (define rebuild-section-header
    (lambda (header name)
      (make-elf-section-header
       (elf-section-header-name-index header) name (elf-section-header-type header)
       (elf-section-header-flags header) (elf-section-header-address header)
       (elf-section-header-offset header) (elf-section-header-size header)
       (elf-section-header-link header) (elf-section-header-info header)
       (elf-section-header-address-alignment header) (elf-section-header-entry-size header))))

  (define vector-u32
    (lambda (bytes endianness start count)
      (let ([result (make-vector count)])
        (do ([i 0 (+ i 1)]) ((= i count) result)
          (vector-set! result i (unsigned-ref bytes (+ start (* i 4)) 4 endianness))))))

  (define decode-symbols
    (lambda (bytes class endianness header string-bytes)
      (let* ([size (bytevector-length bytes)]
             [standard (if (eq? class 'elf32) 16 24)]
             [entry-size (elf-section-header-entry-size header)]
             [entry-size (if (zero? entry-size) standard entry-size)])
        (and (= entry-size standard) (zero? (modulo size entry-size))
             (let* ([count (/ size entry-size)] [result (make-vector count)])
               (do ([i 0 (+ i 1)]) ((= i count) (make-elf-symbol-table result))
                 (let* ([at (* i entry-size)]
                        [name-index (unsigned-ref bytes at 4 endianness)]
                        [name (and string-bytes (nul-string string-bytes name-index))])
                   (if (eq? class 'elf32)
                       (vector-set!
                        result i
                        (make-elf-symbol name-index name
                                         (unsigned-ref bytes (+ at 12) 1 endianness)
                                         (unsigned-ref bytes (+ at 13) 1 endianness)
                                         (unsigned-ref bytes (+ at 14) 2 endianness)
                                         (unsigned-ref bytes (+ at 4) 4 endianness)
                                         (unsigned-ref bytes (+ at 8) 4 endianness)))
                       (vector-set!
                        result i
                        (make-elf-symbol name-index name
                                         (unsigned-ref bytes (+ at 4) 1 endianness)
                                         (unsigned-ref bytes (+ at 5) 1 endianness)
                                         (unsigned-ref bytes (+ at 6) 2 endianness)
                                         (unsigned-ref bytes (+ at 8) 8 endianness)
                                         (unsigned-ref bytes (+ at 16) 8 endianness)))))))))))

  (define decode-relocations
    (lambda (bytes class endianness header addends?)
      (let* ([standard (case class [(elf32) (if addends? 12 8)]
                                   [else (if addends? 24 16)])]
             [entry-size (elf-section-header-entry-size header)]
             [entry-size (if (zero? entry-size) standard entry-size)]
             [size (bytevector-length bytes)])
        (and (= entry-size standard) (zero? (modulo size entry-size))
             (let* ([count (/ size entry-size)] [result (make-vector count)]
                    [word (if (eq? class 'elf32) 4 8)])
               (do ([i 0 (+ i 1)]) ((= i count)
                                    (make-elf-relocation-table result addends?))
                 (let* ([at (* i entry-size)]
                        [offset (unsigned-ref bytes at word endianness)]
                        [info (unsigned-ref bytes (+ at word) word endianness)]
                        [symbol-index (if (eq? class 'elf32) (ash info -8) (ash info -32))]
                        [type (if (eq? class 'elf32) (logand info #xff)
                                  (logand info #xffffffff))]
                        [addend (and addends?
                                     (signed-ref bytes (+ at (* 2 word)) word endianness))])
                   (vector-set! result i
                                (make-elf-relocation offset info symbol-index type addend)))))))))

  (define decode-notes
    (lambda (bytes endianness)
      (let ([length (bytevector-length bytes)])
        (let loop ([offset 0] [notes '()])
          (if (= offset length)
              (make-elf-note-table (list->vector (reverse notes)))
              (and (elf-range-valid? offset 12 length)
                   (let* ([name-size (unsigned-ref bytes offset 4 endianness)]
                          [desc-size (unsigned-ref bytes (+ offset 4) 4 endianness)]
                          [type (unsigned-ref bytes (+ offset 8) 4 endianness)]
                          [name-at (+ offset 12)]
                          [desc-at (+ name-at (* 4 (quotient (+ name-size 3) 4)))]
                          [next (+ desc-at (* 4 (quotient (+ desc-size 3) 4)))])
                     (and (elf-range-valid? name-at name-size length)
                          (elf-range-valid? desc-at desc-size length)
                          (<= next length)
                          (let* ([raw-name (copy-range bytes name-at name-size)]
                                 [name-length (if (and (> name-size 0)
                                                       (zero? (bytevector-u8-ref
                                                               raw-name (- name-size 1))))
                                                  (- name-size 1) name-size)]
                                 [name (utf8->string (copy-range raw-name 0 name-length))])
                            (loop next
                                  (cons (make-elf-note
                                         name type (copy-range bytes desc-at desc-size))
                                        notes)))))))))))

  (define decode-section-content
    (lambda (raw class endianness header all-headers raw-sections)
      (let* ([type (elf-section-header-type header)]
             [size (bytevector-length raw)]
             [word (if (eq? class 'elf32) 4 8)]
             [link (elf-section-header-link header)]
             [linked (and (< link (vector-length all-headers))
                          (vector-ref raw-sections link))])
        (case type
          [(3) (make-elf-string-table raw)]
          [(2 11) (or (decode-symbols raw class endianness header linked)
                      (make-elf-raw-section raw))]
          [(4) (or (decode-relocations raw class endianness header #t)
                   (make-elf-raw-section raw))]
          [(9) (or (decode-relocations raw class endianness header #f)
                   (make-elf-raw-section raw))]
          [(19) (and (zero? (modulo size word))
                     (make-elf-relr-table
                      (let ([values (make-vector (/ size word))])
                        (do ([i 0 (+ i 1)]) ((= i (vector-length values)) values)
                          (vector-set! values i
                                       (unsigned-ref raw (* i word) word endianness))))))]
          [(5) (and (>= size 8)
                    (let* ([bucket-count (unsigned-ref raw 0 4 endianness)]
                           [chain-count (unsigned-ref raw 4 4 endianness)]
                           [needed (* 4 (+ 2 bucket-count chain-count))])
                      (and (= needed size)
                           (make-elf-hash-table
                            (vector-u32 raw endianness 8 bucket-count)
                            (vector-u32 raw endianness (+ 8 (* bucket-count 4))
                                        chain-count)))))]
          [(17) (and (>= size 4) (zero? (modulo size 4))
                     (make-elf-group-section
                      (unsigned-ref raw 0 4 endianness)
                      (vector-u32 raw endianness 4 (- (/ size 4) 1))))]
          [(18) (and (zero? (modulo size 4))
                     (make-elf-word-table (vector-u32 raw endianness 0 (/ size 4))))]
          [(14 15 16) (and (zero? (modulo size word))
                           (make-elf-word-table
                            (let ([values (make-vector (/ size word))])
                              (do ([i 0 (+ i 1)]) ((= i (vector-length values)) values)
                                (vector-set! values i
                                             (unsigned-ref raw (* i word) word endianness))))))]
          [(6) (and (zero? (modulo size (* 2 word)))
                    (make-elf-dynamic-table
                     (let ([values (make-vector (/ size (* 2 word)))])
                       (do ([i 0 (+ i 1)]) ((= i (vector-length values)) values)
                         (let ([at (* i 2 word)])
                           (vector-set! values i
                                        (make-elf-dynamic-entry
                                         (signed-ref raw at word endianness)
                                         (unsigned-ref raw (+ at word) word endianness))))))))]
          [(7) (or (decode-notes raw endianness) (make-elf-raw-section raw))]
          [else (make-elf-raw-section raw)]))))

  (define unsigned-parser
    (lambda (size endianness-name)
      (case (cons size endianness-name)
        [((2 . little)) <u16le>] [((4 . little)) <u32le>]
        [((8 . little)) <u64le>] [((2 . big)) <u16be>]
        [((4 . big)) <u32be>] [((8 . big)) <u64be>])))

  (define <elf-class>
    (</> (<as> 'elf32 (<uimm8> 1)) (<as> 'elf64 (<uimm8> 2))))

  (define <elf-endianness>
    (</> (<as> 'little (<uimm8> 1)) (<as> 'big (<uimm8> 2))))

  (define <elf-identification>
    (<~> (<u8*> #x7f #x45 #x4c #x46)
         <elf-class>
         <elf-endianness>
         (<uimm8> 1)
         <u8>
         <u8>
         (<u8vec> 7)))

  (define <elf-header>
    (<bind>
     <elf-identification>
     (lambda (identification)
       (let* ([class (list-ref identification 1)]
              [endianness-name (list-ref identification 2)]
              [endianness (if (eq? endianness-name 'little)
                              (endianness little) (endianness big))]
              [word-size (if (eq? class 'elf32) 4 8)]
              [sizes-at (if (eq? class 'elf32) 40 52)]
              [<half> (unsigned-parser 2 endianness-name)]
              [<word> (unsigned-parser 4 endianness-name)]
              [<address> (unsigned-parser word-size endianness-name)])
         (<map>
          (lambda (fields)
            (make-decoded-elf-header
             class endianness-name endianness
             (list-ref fields 0) (list-ref fields 1) (list-ref fields 2)
             (list-ref fields 3) (list-ref fields 4) (list-ref fields 5)
             (list-ref fields 6) (list-ref fields 7) (list-ref fields 8)
             (list-ref fields 9) (list-ref fields 10) (list-ref fields 11)
             (list-ref fields 12) sizes-at))
          (<~> <half> <half> <word> <address> <address> <address> <word>
               <half> <half> <half> <half> <half> <half>))))))

  (define program-header-parser
    (lambda (class endianness-name)
      (let* ([word-size (if (eq? class 'elf32) 4 8)]
             [<word> (unsigned-parser 4 endianness-name)]
             [<address> (unsigned-parser word-size endianness-name)])
        (<map>
         (lambda (fields)
           (if (eq? class 'elf32)
               (make-elf-program-header
                (list-ref fields 0) (list-ref fields 6) (list-ref fields 1)
                (list-ref fields 2) (list-ref fields 3) (list-ref fields 4)
                (list-ref fields 5) (list-ref fields 7) #f)
               (make-elf-program-header
                (list-ref fields 0) (list-ref fields 1) (list-ref fields 2)
                (list-ref fields 3) (list-ref fields 4) (list-ref fields 5)
                (list-ref fields 6) (list-ref fields 7) #f)))
         (if (eq? class 'elf32)
             (<~> <word> <address> <address> <address> <address> <address> <word> <address>)
             (<~> <word> <word> <address> <address> <address> <address> <address>
                  <address>))))))

  (define section-header-parser
    (lambda (class endianness-name)
      (let* ([word-size (if (eq? class 'elf32) 4 8)]
             [<word> (unsigned-parser 4 endianness-name)]
             [<address> (unsigned-parser word-size endianness-name)])
        (<map>
         (lambda (fields)
           (make-elf-section-header
            (list-ref fields 0) #f (list-ref fields 1) (list-ref fields 2)
            (list-ref fields 3) (list-ref fields 4) (list-ref fields 5)
            (list-ref fields 6) (list-ref fields 7) (list-ref fields 8)
            (list-ref fields 9)))
         (<~> <word> <word> <address> <address> <address> <address>
              <word> <word> <address> <address>)))))

  (define problem-parser
    (lambda (problem length)
      (<pos-at> (min (elf-issue-offset problem) length)
                (<fail-with> (elf-issue-message problem)))))

  (define header-problem
    (lambda (header)
      (let* ([class (decoded-elf-header-class header)]
             [standard-header (if (eq? class 'elf32) 52 64)]
             [standard-program (if (eq? class 'elf32) 32 56)]
             [standard-section (if (eq? class 'elf32) 40 64)]
             [sizes-at (decoded-elf-header-sizes-at header)])
        (cond
          [(not (= 1 (decoded-elf-header-version header)))
           (issue 20 "invalid ELF version")]
          [(not (= standard-header (decoded-elf-header-header-size header)))
           (issue sizes-at "invalid ELF header size")]
          [(and (not (zero? (decoded-elf-header-raw-phnum header)))
                (not (= standard-program (decoded-elf-header-phentsize header))))
           (issue (+ sizes-at 2) "invalid program-header entry size")]
          [(and (or (not (zero? (decoded-elf-header-raw-shnum header)))
                    (not (zero? (decoded-elf-header-shoff header))))
                (not (= standard-section (decoded-elf-header-shentsize header))))
           (issue (+ sizes-at 6) "invalid section-header entry size")]
          [else #f]))))

  (define table-parser
    (lambda (offset entry-size count entry-parser)
      (<pos-at> offset
                (<bounded> (* entry-size count)
                           (<map> list->vector
                                  (<rep> (<bounded> entry-size entry-parser) count))))))

  (define build-programs
    (lambda (bytes header raw-programs)
      (let* ([length (bytevector-length bytes)]
             [count (vector-length raw-programs)]
             [programs (make-vector count)]
             [table-offset (decoded-elf-header-phoff header)]
             [entry-size (decoded-elf-header-phentsize header)])
        (let loop ([index 0])
          (if (= index count)
              (values programs #f)
              (let* ([raw (vector-ref raw-programs index)]
                     [at (+ table-offset (* index entry-size))]
                     [type (elf-program-header-type raw)]
                     [file-offset (elf-program-header-offset raw)]
                     [virtual-address (elf-program-header-virtual-address raw)]
                     [file-size (elf-program-header-file-size raw)]
                     [memory-size (elf-program-header-memory-size raw)]
                     [alignment (elf-program-header-alignment raw)])
                (cond
                  [(not (elf-range-valid? file-offset file-size length))
                   (values #f (issue file-offset "segment exceeds input"))]
                  [(and (= type 1) (> file-size memory-size))
                   (values #f (issue at "load segment file size exceeds memory size"))]
                  [(not (elf-power-of-two? alignment))
                   (values #f (issue at "invalid segment alignment"))]
                  [(and (= type 1) (> alignment 1)
                        (not (= (modulo file-offset alignment)
                                (modulo virtual-address alignment))))
                   (values #f (issue at "misaligned load segment"))]
                  [else
                   (vector-set!
                    programs index
                    (make-elf-program-header
                     type (elf-program-header-flags raw) file-offset virtual-address
                     (elf-program-header-physical-address raw) file-size memory-size alignment
                     (copy-range bytes file-offset file-size)))
                   (loop (+ index 1))])))))))

  (define build-sections
    (lambda (bytes header headers)
      (let* ([length (bytevector-length bytes)]
             [count (vector-length headers)]
             [raw-sections (make-vector count)]
             [table-offset (decoded-elf-header-shoff header)]
             [entry-size (decoded-elf-header-shentsize header)])
        (let loop ([index 0])
          (if (= index count)
              (values raw-sections #f)
              (let* ([section (vector-ref headers index)]
                     [at (+ table-offset (* index entry-size))]
                     [file-offset (elf-section-header-offset section)]
                     [size (elf-section-header-size section)])
                (cond
                  [(not (elf-power-of-two?
                         (elf-section-header-address-alignment section)))
                   (values #f (issue at "invalid section alignment"))]
                  [(and (not (= 8 (elf-section-header-type section)))
                        (not (elf-range-valid? file-offset size length)))
                   (values #f (issue file-offset "section exceeds input"))]
                  [else
                   (vector-set! raw-sections index
                                (if (= 8 (elf-section-header-type section))
                                    (make-bytevector 0)
                                    (copy-range bytes file-offset size)))
                   (loop (+ index 1))])))))))

  (define elf-layout-parser
    (lambda (bytes length header zero)
      (let* ([class (decoded-elf-header-class header)]
             [endianness-name (decoded-elf-header-endianness-name header)]
             [raw-phnum (decoded-elf-header-raw-phnum header)]
             [raw-shnum (decoded-elf-header-raw-shnum header)]
             [raw-shstrndx (decoded-elf-header-raw-shstrndx header)]
             [phnum (if (= raw-phnum #xffff) (elf-section-header-info zero) raw-phnum)]
             [shnum (if (= raw-shnum 0)
                        (if (zero? (decoded-elf-header-shoff header))
                            0
                            (elf-section-header-size zero))
                        raw-shnum)]
             [shstrndx (if (= raw-shstrndx #xffff)
                           (elf-section-header-link zero)
                           raw-shstrndx)])
        (if (and (> shnum 0) (>= shstrndx shnum))
            (problem-parser
             (issue (+ (decoded-elf-header-sizes-at header) 10)
                    "invalid section-name table index")
             length)
            (<bind>
             (table-parser
              (decoded-elf-header-phoff header)
              (decoded-elf-header-phentsize header)
              phnum
              (program-header-parser class endianness-name))
             (lambda (raw-programs)
               (<bind>
                (table-parser
                 (decoded-elf-header-shoff header)
                 (decoded-elf-header-shentsize header)
                 shnum
                 (section-header-parser class endianness-name))
                (lambda (headers)
                  (let-values ([(programs program-problem)
                                (build-programs bytes header raw-programs)]
                               [(raw-sections section-problem)
                                (build-sections bytes header headers)])
                    (cond
                      [program-problem (problem-parser program-problem length)]
                      [section-problem (problem-parser section-problem length)]
                      [else
                       (let-values ([(file finish-problem)
                                     (finish-elf
                                      bytes class endianness-name
                                      (decoded-elf-header-endianness header)
                                      (decoded-elf-header-type header)
                                      (decoded-elf-header-machine header)
                                      (decoded-elf-header-version header)
                                      (decoded-elf-header-entry header)
                                      (decoded-elf-header-phoff header)
                                      (decoded-elf-header-shoff header)
                                      (decoded-elf-header-flags header)
                                      (decoded-elf-header-header-size header)
                                      (decoded-elf-header-phentsize header) phnum
                                      (decoded-elf-header-shentsize header) shnum shstrndx
                                      programs headers raw-sections)])
                         (if finish-problem
                             (problem-parser finish-problem length)
                             (<~ (<result> file)
                                 (<skip> <u8>
                                         (- length
                                            (decoded-elf-header-header-size header))))))]
                      ))))))))))

  (define elf-body-parser
    (lambda (bytes length header)
      (let* ([problem (header-problem header)]
             [class (decoded-elf-header-class header)]
             [standard-section (if (eq? class 'elf32) 40 64)]
             [raw-phnum (decoded-elf-header-raw-phnum header)]
             [raw-shnum (decoded-elf-header-raw-shnum header)]
             [raw-shstrndx (decoded-elf-header-raw-shstrndx header)]
             [shoff (decoded-elf-header-shoff header)]
             [ordinary-empty? (and (= raw-shnum 0) (zero? shoff)
                                   (not (= raw-phnum #xffff))
                                   (not (= raw-shstrndx #xffff)))]
             [needs-zero? (and (not ordinary-empty?)
                               (or (= raw-phnum #xffff) (= raw-shnum 0)
                                   (= raw-shstrndx #xffff)))])
        (cond
          [problem (problem-parser problem length)]
          [(and needs-zero? (zero? shoff))
           (problem-parser (issue shoff "missing section zero for extended counts") length)]
          [else
           (<bind>
            (if needs-zero?
                (<pos-at> shoff
                          (<bounded> standard-section
                                     (section-header-parser
                                      class
                                      (decoded-elf-header-endianness-name header))))
                (<result> #f))
            (lambda (zero) (elf-layout-parser bytes length header zero)))]))))

  (define finish-elf
    (lambda (bytes class endianness-name endianness type machine version entry phoff shoff flags
                   header-size phentsize phnum shentsize shnum shstrndx programs headers
                   raw-sections)
      (let ([name-bytes (and (> shnum 0) (vector-ref raw-sections shstrndx))])
        (cond
          [(and (> shnum 0)
                (not (= 3 (elf-section-header-type (vector-ref headers shstrndx)))))
           (values #f (issue shoff "section-name table is not a string table"))]
          [(and (> shnum 0)
                (or (zero? (bytevector-length name-bytes))
                    (not (zero? (bytevector-u8-ref name-bytes 0)))
                    (not (zero? (bytevector-u8-ref
                                 name-bytes (- (bytevector-length name-bytes) 1))))))
           (values #f (issue (elf-section-header-offset (vector-ref headers shstrndx))
                             "invalid section-name string table"))]
          [else
           (let ([sections (make-vector shnum)])
             (let loop ([i 0])
               (if (= i shnum)
                   (values
                    (make-elf-file
                     (make-elf-identification
                      class endianness-name 1 (bytevector-u8-ref bytes 7)
                      (bytevector-u8-ref bytes 8))
                     (make-elf-header type machine version entry phoff shoff flags header-size
                                      phentsize phnum shentsize shnum shstrndx)
                     programs sections)
                    #f)
                   (let* ([old-header (vector-ref headers i)]
                          [name-index (elf-section-header-name-index old-header)]
                          [name (if name-bytes (nul-string name-bytes name-index) "")])
                     (if (not name)
                         (values #f
                                 (issue (elf-section-header-offset
                                         (vector-ref headers shstrndx))
                                        "invalid section name index or terminator"))
                         (let ([header (rebuild-section-header old-header name)])
                           (vector-set! headers i header)
                           (vector-set!
                            sections i
                            (make-elf-section
                             header
                             (decode-section-content
                              (vector-ref raw-sections i) class endianness header headers
                              raw-sections)))
                           (loop (+ i 1))))))))]))))

  (define-parser parser-elf
    (let ([bytes (binary-input-data inp)]
          [length (input-len inp)])
      (parser-call
       (<bind> <elf-header>
               (lambda (header)
                 (elf-body-parser bytes length header)))
       inp state (fx1+ lvl))))
  )
