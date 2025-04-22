(library (chezpp parser wasm)
  (export parse-wasm-file)
  (import (chezpp chez)
          (chezpp parser combinator)
          (chezpp internal)
          (chezpp list)
          (chezpp io)
          (chezpp vector)
          (chezpp utils))

  (define parser-wasm
    (let ()
      (define <u32> <uleb128>)
      (define <s33> (<sleb128> 33))
      (define (<uimm32> x)
        (<bind> <uleb128>
                (lambda (n)
                  (if (= x n)
                      (<result> n)
                      (<fail-with> (format "read ~a, expected ~a" n x))))))

      (define (<vec> p) ;; TODO optimize
        (<bind> <u32> (lambda (n) (<map> list->vector (<rep> p n)))))

      (define <magic>   (<u8*> #x00 #x61 #x73 #x6D))
      (define <version> (<u8*> 1 0 0 0))
      (define <name> (<bind> (<vec> <u8>)
                             (lambda (b*)
                               ;; (println '<name>)
                               ;; (println b*)
                               (printf "<name>: ~a~n" (utf8->string (apply bytevector (vector->list b*))))
                               ;; TODO optimize
                               (<result> (utf8->string (apply bytevector (vector->list b*)))))))

      (define <numtype> (</> (<as> 'i32 (<uimm8> #x7F))
                             (<as> 'i64 (<uimm8> #x7E))
                             (<as> 'f32 (<uimm8> #x7D))
                             (<as> 'f64 (<uimm8> #x7C))))
      (define <vectype> (<as> 'v128 (<uimm8> #x7B)))
      (define <reftype> (</> (<as> 'funcref   (<uimm8> #x70))
                             (<as> 'externref (<uimm8> #x6F))))
      (define <valtype> (</> <numtype> <vectype> <reftype>))
      (define <resulttype> (<vec> <valtype>))
      (define <limits> (</> (<map> (lambda (val) (cons val 'max))
                                   (~> (<uimm8> #x00) <u32>))
                            (<map> (lambda (val) (cons (cadr val) (caddr val)))
                                   (<~> (<uimm8> #x01) <u32> <u32>))))
      (define <functype> (<~> (<as> 'functype (<uimm8> #x60)) <resulttype> <resulttype>))
      (define <tabletype> (<~> <reftype> <limits>))
      (define <memtype> <limits>)
      (define <globaltype> (<~> <valtype> (</> (<as> 'const (<uimm8> #x00))
                                               (<as> 'var   (<uimm8> #x01)))))

      (define <end> (<uimm8> #x0B))
      (define <block-type> (</> (<uimm8> #x40) <valtype> <s33>))
      (define <memarg> (<~> <u32> <u32>))
      (declare-lazy-parser <instr>)
      (define-lazy-parser <instr>
        (</>
         ;; control
         ;; (<msg-f> "instr")
         (<uimm8> #x00)
         (<uimm8> #x01)
         (<~> (<uimm8> #x02) <block-type> (<many> <instr>) <end>)
         (<~> (<uimm8> #x03) <block-type> (<many> <instr>) <end>)
         (<~> (<uimm8> #x04) <block-type> (<many> <instr>)
              (</> <end>
                   (<~> (<uimm8> #x05) (<many> <instr>) <end>)))
         (<~> (<uimm8> #x0C) <u32>)
         (<~> (<uimm8> #x0D) <u32>)
         (<~> (<uimm8> #x0E) (<vec> <u32>) <u32>)
         (<uimm8> #x0F)
         (<~> (<uimm8> #x10) <u32>)
         (<~> (<uimm8> #x11) <u32> <u32>)

         ;; (<msg-f> "control")
         ;; reference
         (<~> (<uimm8> #xD0) <reftype>)
         (<uimm8> #xD1)
         (<~> (<uimm8> #xD2) <u32>)
         ;; (<msg-f> "ref")
         ;; parametric
         (<uimm8> #x1A)
         (<uimm8> #x1B)
         (<~> (<uimm8> #x1C) (<vec> <valtype>))
         ;; (<msg-f> "param")
         ;; variable
         (<~> (<uimm8> #x20) <u32>)
         (<~> (<uimm8> #x21) <u32>)
         (<~> (<uimm8> #x22) <u32>)
         (<~> (<uimm8> #x23) <u32>)
         (<~> (<uimm8> #x24) <u32>)
         ;; (<msg-f> "var")
         ;; table
         (<~> (<uimm8> #x25) <u32>)
         (<~> (<uimm8> #x26) <u32>)
         (<~> (<uimm8> #xFC) (</> (<~> (<uimm32> 12) <u32> <u32>)
                                  (<~> (<uimm32> 13) <u32>)
                                  (<~> (<uimm32> 14) <u32> <u32>)
                                  (<~> (<uimm32> 15) <u32>)
                                  (<~> (<uimm32> 16) <u32>)
                                  (<~> (<uimm32> 17) <u32>)))
         ;; (<msg-f> "table")
         ;; memory
         (<~> (<satisfy> <u8> (lambda (x) (fx<= #x28 x #x3E))) <memarg>)
         (<~> (<uimm8> #x3F) (<uimm8> #x00))
         (<~> (<uimm8> #x40) (<uimm8> #x00))
         (<~> (<uimm8> #xFC) (</> (<~> (<uimm32> 8) <u32> (<uimm8> #x00))
                                  (<~> (<uimm32> 9) <u32>)
                                  (<~> (<uimm32> 10) (<uimm8> #x00) (<uimm8> #x00))
                                  (<~> (<uimm32> 11) (<uimm8> #x00))))
         ;; (<msg-f> "mem")
         ;; numeric
         (<~> (<uimm8> #x41) (<sleb128> 32))
         (<~> (<uimm8> #x42) (<sleb128> 64))
         (<~> (<uimm8> #x43) <f32>)
         (<~> (<uimm8> #x44) <f64>)
         ;; TODO expand
         (<satisfy> <u8> (lambda (x) (fx<= #x45 x #xC4)))
         (<~> (<uimm8> #xFC) (</> (<~> (<uimm32> 0))
                                  (<~> (<uimm32> 1))
                                  (<~> (<uimm32> 2))
                                  (<~> (<uimm32> 3))
                                  (<~> (<uimm32> 4))
                                  (<~> (<uimm32> 5))
                                  (<~> (<uimm32> 6))
                                  (<~> (<uimm32> 7))))
         ;; (<msg-f> "vec")
         ;; vector
         ;; TODO redefine <satisfy>?
         (<~> (<uimm8> #xFD)
              (<bind> <uleb128>
                      (lambda (x)
                        (println x)
                        (cond [(or (<= 0 x 11) (= x 92) (= x 93)) <memarg>]
                              [(<= 84 x 91) <memarg> <u8>]
                              [(= x 12) (<rep> <u8> 16)]
                              [(= x 13) (<rep> <u8> 16)]
                              [(<= 21 x 34) <u8>]
                              [else x]))))
         ;; (<msg-f> "instr end")
         (<bind> <u8>
                 (lambda (n)
                   ;;(println n)
                   (<fail-with> "fail to read instr")))
         ))
      (define <expr> (<~> (<many> <instr>) (<uimm8> #x0B)))

      (define <import>
        (<~> <name> <name> ;; mod nm
             ;; importdesc
             (</> (~> (<uimm8> #x00) <u32>) ;; typeidx
                  (~> (<uimm8> #x01) <tabletype>)
                  (~> (<uimm8> #x02) <memtype>)
                  (~> (<uimm8> #x03) <globaltype>))))
      (define <table> <tabletype>)
      (define <mem> <memtype>)
      (define <global> (<~> <globaltype> <expr>))
      (define <export> (<~> <name> (</> (~> (<uimm8> #x00) <u32>)
                                        (~> (<uimm8> #x01) <u32>)
                                        (~> (<uimm8> #x02) <u32>)
                                        (~> (<uimm8> #x03) <u32>))))

      (define <vec-funcidx> (<vec> <u32>))
      (define <vec-expr>    (<vec> <expr>))
      (define <elemkind>    (<uimm32> #x00))
      (define <elem> (</> (<~> (<uimm32> #x00) <expr>     <vec-funcidx>)
                          (<~> (<uimm32> #x01) <elemkind> <vec-funcidx>)
                          (<~> (<uimm32> #x02) <u32>      <expr>        <elemkind> <vec-funcidx>)
                          (<~> (<uimm32> #x03) <elemkind> <vec-funcidx>)
                          (<~> (<uimm32> #x04) <expr>     <vec-expr>)
                          (<~> (<uimm32> #x05) <reftype>  <vec-expr>)
                          (<~> (<uimm32> #x06) <u32>      <expr>        <reftype>  <vec-expr>)
                          (<~> (<uimm32> #x07) <reftype>  <vec-expr>)))

      ;; TODO show <expr>
      (define <func> (<~n> 0 (<vec> (<~> <u32> <valtype>)) <expr>))
      (define <code> (<~> <u32> <func>))

      (define <bchar> (<map> (lambda (x) x) #;integer->char <u8>
                             ))
      (define <vec-byte> (<vec> <bchar>))
      (define <data> (</> (<~> (<uimm32> #x00) <expr> <vec-byte>)
                          (<~> (<uimm32> #x01) <vec-byte>)
                          (<~> (<uimm32> #x02) <u32> <expr> <vec-byte>)))

      (define (mksec who) (lambda (val) (println who) (cons who val)))

      ;; https://github.com/WebAssembly/extended-name-section/blob/main/proposals/extended-name-section/Overview.md
      (define (<namesubsection> n B) (<~> (<uimm8> n) <u32> B))
      (define <namemap>          (<vec> (<~> <u32> <name>)))
      (define <indirectnamemap>  (<vec> (<~> <u32> <namemap>)))
      (define <modulenamesubsec> (<namesubsection> 0 <name>))
      (define <funcnamesubsec>   (<namesubsection> 1 <namemap>))
      (define <localnamesubsec>  (<namesubsection> 2 <indirectnamemap>))
      (define <labelnamesubsec>  (<namesubsection> 3 <indirectnamemap>))
      (define <typenamesubsec>   (<namesubsection> 4 <namemap>))
      (define <tablenamesubsec>  (<namesubsection> 5 <namemap>))
      (define <memorynamesubsec> (<namesubsection> 6 <namemap>))
      (define <globalnamesubsec> (<namesubsection> 7 <namemap>))
      (define <elemnamesubsec>   (<namesubsection> 8 <namemap>))
      (define <datanamesubsec>   (<namesubsection> 9 <namemap>))
      (define <tagnamesubsec>    (<namesubsection> 10 <namemap>))
      (define-who <namesubsecs> (<~> (<optional> <modulenamesubsec>)
                                     (<optional> <funcnamesubsec>)
                                     (<optional> <localnamesubsec>)
                                     (<optional> <labelnamesubsec>)
                                     (<optional> <typenamesubsec>)
                                     (<optional> <tablenamesubsec>)
                                     (<optional> <memorynamesubsec>)
                                     (<optional> <globalnamesubsec>)
                                     (<optional> <elemnamesubsec>)
                                     (<optional> <datanamesubsec>)
                                     (<optional> <tagnamesubsec>)))
      (define-who <customsec>
        (<map> (mksec who)
               (<~> (<uimm8> 0)
                    (<bind> (<~> <u32> ;; section size
                                 (<bind> (<~> <pos> <u32> <pos>) ;; name
                                         (lambda (val)
                                           (<map> (lambda (b*)
                                                    (cons (- (list-ref val 2) (list-ref val 0))
                                                          (list->vector b*)))
                                                  (<rep> <u8> (list-ref val 1))))))
                            ;; section size contains the size of the name, which needs to be subtracted
                            (lambda (val)
                              (let* ([size (list-ref val 0)] [rest (cadr val)]
                                     [b* (cdr rest)] [len-u32 (car rest)]
                                     [name (utf8->string (apply bytevector (vector->list b*)))])
                                (if (string=? name "name")
                                    <namesubsecs>
                                    (<~ (<result> name)
                                        (<rep> <u8> (- size (vector-length b*) len-u32))))))))))
      (define-who <typesec>
        (<map> (mksec who) (<~> (<uimm8> 1) <u32> (<vec> <functype>))))
      (define-who <importsec>
        (<map> (mksec who) (<~> (<uimm8> 2) <u32> (<vec> <import>))))
      (define-who <funcsec>
        (<map> (mksec who) (<~> (<uimm8> 3) <u32> (<vec> <u32>))))
      (define-who <tablesec>
        (<map> (mksec who) (<~> (<uimm8> 4) <u32> (<vec> <table>))))
      (define-who <memsec>
        (<map> (mksec who) (<~> (<uimm8> 5) <u32> (<vec> <mem>))))
      (define-who <globalsec>
        (<map> (mksec who) (<~> (<uimm8> 6) <u32> (<vec> <global>))))
      (define-who <exportsec>
        (<map> (mksec who) (<~> (<uimm8> 7) <u32> (<vec> <export>))))
      (define-who <startsec>
        (<map> (mksec who) (<~> (<uimm8> 8) <u32> <u32>)))
      (define-who <elemsec>
        (<map> (mksec who) (<~> (<uimm8> 9) <u32> (<vec> <elem>))))
      (define-who <codesec>
        (<map> (mksec who) (<~> (<uimm8> 10) <u32> (<vec> <code>))))
      (define-who <datasec>
        (<map> (mksec who) (<~> (<uimm8> 11) <u32> (<vec> <data>))))
      (define-who <datacountsec>
        (<map> (mksec who) (<~> (<uimm8> 12) <u32> <u32>)))

      (define <module> (<~> <magic> <version>
                            (<many>     <customsec>)
                            (<optional> <typesec>)
                            (<many>     <customsec>)
                            (<optional> <importsec>)
                            (<many>     <customsec>)
                            (<optional> <funcsec>)
                            (<many>     <customsec>)
                            (<optional> <tablesec>)
                            (<many>     <customsec>)
                            (<optional> <memsec>)
                            (<many>     <customsec>)
                            (<optional> <globalsec>)
                            (<many>     <customsec>)
                            (<optional> <exportsec>)
                            (<many>     <customsec>)
                            (<optional> <startsec>)
                            (<many>     <customsec>)
                            (<optional> <elemsec>)
                            (<many>     <customsec>)
                            (<optional> <datacountsec>)
                            (<many>     <customsec>)
                            (<optional> <codesec>)
                            (<many>     <customsec>)
                            (<optional> <datasec>)
                            (<many>     <customsec>)
                            <eof>))

      <module>))

  (define parse-wasm-file
    (lambda (path)
      (pcheck ([file-regular? path])
              (parse-binary-file parser-wasm path (make-eq-hashtable)))))

  )
