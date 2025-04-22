(library (chezpp parser elf)
  (export parse-elf-file)
  (import (chezpp chez)
          (chezpp parser combinator)
          (chezpp internal)
          (chezpp list)
          (chezpp io)
          (chezpp vector)
          (chezpp utils))

  (define parser-elf
    (let ()
      (define EI_NIDENT 16)
      (define-who <EI-CLASS>
        (<map> (lambda (val)
                 (case val
                   [1 'ELFCLASS32]
                   [2 'ELFCLASS64]
                   [else (assert-unreachable)]))
               <u8>))
      (define-who <EI-DATA>
        (<map> (lambda (val)
                 (case val
                   [1 'ELFDATA2LSB]
                   [2 (errorf who "Big-endian not supported")]
                   [else (assert-unreachable)]))
               <u8>))
      (define-who <EI-VERSION>
        (<map> (lambda (val)
                 (case val
                   [1 'EV-CURRENT]
                   [else (assert-unreachable)]))
               <u8>))
      (define <EI-OSABI>
        (<map> (lambda (val)
                 (case val
                   ;;[0 'ELFOSABI-NONE]
                   [0 'ELFOSABI-SYSV]
                   [1 'ELFOSABI-HPUX]
                   [2 'ELFOSABI-NETBSD]
                   [3 'ELFOSABI-LINUX]
                   [6 'ELFOSABI-SOLARIS]
                   [8 'ELFOSABI-IRIX]
                   [9 'ELFOSABI-FREEBSD]
                   [10 'ELFOSABI-TRU64]
                   [97 'ELFOSABI-ARM]
                   [255 'ELFOSABI-STANDALONE]
                   [else 'ELFOSABI-OTHER]))
               <u8>))
      (define-who <EI-ABIVERSION>
        (<map> (lambda (val)
                 (unless (= 0 val)
                   (errorf who "invalid ABI version: ~a, should be 0" val))
                 val)
               <u8>))

      (define <e-machine>
        (<map> (lambda (val)
                 (case val
                   [0 'EM-NONE]
                   [2 'EM-SPARC]
                   [3 'EM-386]
                   [8 'EM-MIPS]
                   [20 'EM-PPC]
                   [21 'EM-PPC64]
                   [22 'EM-S390]
                   [40 'EM-ARM]
                   [50 'EM-IA_64]
                   [62 'EM-X86_64]
                   [183 'EM-AARCH64]
                   [243 'EM-RISCV]
                   [258 'EM-LOONGARCH]
                   [else 'EM-OTHER]))
               <u16>))
      (define <e-type>
        (<map> (lambda (val)
                 (case val
                   [0 'ET-NONE]
                   [1 'ET-REL]
                   [2 'ET-EXEC]
                   [3 'ET-DYN]
                   [4 'ET-CORE]
                   [5 'ET-NUM]
                   [else 'ET-OTHER]))
               <u16>))
      (define <e-version>
        (<map> (lambda (val)
                 (case val
                   [0 'EV-NONE]
                   [1 'EV-CURRENT]
                   [else (assert-unreachable)]))
               <u32>))
      (define <e-phoff> <u64>)
      (define <e-shoff> <u64>)
      (define <e-phentsize> <u16>)
      (define <e-phnum> <u16>)
      (define <e-shentsize> <u16>)
      (define <e-shnum> <u16>)
      (define <e-shstrndx> <u16>)
      (define <elf-header>
        (<~> (<u8*> #x7f 69 76 70)
             <EI-CLASS> <EI-DATA> <EI-VERSION>
             <EI-OSABI> <EI-ABIVERSION>
             (<skip> <u8> 7)
             <e-type> <e-machine> <e-version>
             <u64> <e-phoff> <e-shoff>
             <u32>
             <u16> <e-phentsize> <e-phnum> <e-shentsize> <e-shnum> <e-shstrndx>))

      (define <p-type>
        (<map> (lambda (val)
                 (case val
                   [0 'PT-NULL]
                   [1 'PT-LOAD]
                   [2 'PT-DYNAMIC]
                   [3 'PT-INTERP]
                   [4 'PT-NOTE]
                   [5 'PT-SHLIB]
                   [6 'PT-PHDR]
                   [7 'PT-TLS]
                   [8 'PT-NUM]
                   [else 'PT-OTHER]))
               <u32>))
      (define <program-header>
        (<~> <p-type> <u32>
             <u64> <u64> <u64>
             <u64> <u64> <u64>))


      ;; get the null-terminated string starting from `s` in bytevector
      (define-who get-string
        (lambda (bv s)
          (pcheck ([bytevector? bv] [natural? s])
                  ;; TODO bounds check?
                  (let loop ([s s] [c* '()])
                    (let ([c (bytevector-u8-ref bv s)])
                      (if (= 0 c)
                          (apply string (map! integer->char (reverse c*)))
                          (loop (fx1+ s) (cons c c*))))))))
      (define <sh-name>
        (<map-st> (lambda (val st)
                    (let ([secname-strtab (hashtable-ref st 'secname-strtab #f)])
                      (if secname-strtab
                          (get-string secname-strtab val)
                          ;; secname-strtab not parsed yet, maybe we are just parsing its header
                          #f)))
                  <u32>))
      (define <sh-type>
        (<map> (lambda (val)
                 (case val
                   [0 'SHT-NULL]
                   [1 'SHT-PROGBITS]
                   [2 'SHT-SYMTAB]
                   [3 'SHT-STRTAB]
                   [4 'SHT-RELA]
                   [5 'SHT-HASH]
                   [6 'SHT-DYNAMIC]
                   [7 'SHT-NOTE]
                   [8 'SHT-NOBITS]
                   [9 'SHT-REL]
                   [10 'SHT-SHLIB]
                   [11 'SHT-DYNSYM]
                   [14 'SHT-INIT_ARRAY]
                   [15 'SHT-FINI_ARRAY]
                   [16 'SHT-PREINIT_ARRAY]
                   [17 'SHT-GROUP]
                   [18 'SHT-SYMTAB_SHNDX]
                   [19 'SHT-RELR]
                   [20 'SHT-NUM]
                   [else 'SHT-OTHER]))
               <u32>))
      (define <section-header>
        (<~> <sh-name> <sh-type>
             <u64> <u64> <u64> <u64>
             <u32> <u32>
             <u64> <u64>))
      (define <string-table> (<~>))

      (define <sym>
        ;; TODO symbol name, type, visibility
        (<~> <u32> <u8> <u8> <u16> <u64> <u64>))

      (define <elf64>
        (<bind> <elf-header>
                (lambda (hd)
                  (println hd)
                  (let ([shoff     (list-ref hd 12)]
                        [shentsize (list-ref hd 17)]
                        [shnum     (list-ref hd 18)]
                        [shstrndx  (list-ref hd 19)])
                    ;; #xffff: SHN_XINDEX
                    (when (or (= 0 shstrndx) (= #xffff shstrndx)) (assert-unreachable))
                    (let ([sh-str-off (+ shoff (* shentsize shstrndx))])
                      (<~> (<result> hd)
                           (<bind> (<pos-at> sh-str-off <section-header>)
                                   ;; parse the sec name header table
                                   (lambda (sec-hd)
                                     (println sec-hd)
                                     (assert (not (eq? 'SHT-NOBITS (list-ref sec-hd 1))))
                                     ;; parse the sec name str section
                                     (let ([off  (list-ref sec-hd 4)]
                                           [size (list-ref sec-hd 5)])
                                       ;; return the section name string section
                                       (<~> (<result> sec-hd)
                                            (<map-st> (lambda (val st)
                                                        (hashtable-set! st 'secname-strtab val)
                                                        val)
                                                      (<pos-at> off (<u8vec> size)))))))
                           ;; parse all sec headers and corresponding sections
                           (<bind> (<pos-at> shoff (<rep> <section-header> shnum))
                                   (lambda (sec-hdrs)
                                     (println sec-hdrs)
                                     (let ([sh-type* (map (lambda (hd) (list-ref hd 1)) sec-hdrs)]
                                           [sh-off*  (map (lambda (hd) (list-ref hd 4)) sec-hdrs)]
                                           [sh-size* (map (lambda (hd) (list-ref hd 5)) sec-hdrs)])
                                       (apply <~>
                                              (map (lambda (sh sh-type sh-off sh-size)
                                                     ;; TODO dispatch on sh-type
                                                     (<map> (lambda (val) (list 'section sh val))
                                                            (if (eq? sh-type 'SHT-NOBITS)
                                                                (<result> '())
                                                                (<pos-at> sh-off (<u8vec> sh-size)))))
                                                   sec-hdrs sh-type* sh-off* sh-size*)))))))))))

      <elf64>))

  (define parse-elf-file
    (lambda (path)
      (pcheck ([file-regular? path])
              (parse-binary-file parser-elf path
                                 ;; hold string table
                                 (make-eq-hashtable)))))

  )
