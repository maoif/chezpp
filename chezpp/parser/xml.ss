(library (chezpp parser xml)
  (export parse-xml parse-xml-file)
  (import (chezpp chez)
          (chezpp parser combinator)
          (chezpp internal)
          (chezpp list)
          (chezpp io)
          (chezpp vector)
          (chezpp utils))

  ;; This xml parser only parses simple xml documents without document type declarations (DTD).

  ;; https://www.w3.org/TR/xml/
  (define parser-xml
    (let ()
      (define <ws> (<one-of> "\t\r\n "))
      (define <S> (<some> <ws>))
      (define <S?> (<optional> <S>))
      (define (<token> p)  (<~ p (<many> <ws>)))
      (define (<token1> p) (<~ p (<some> <ws>)))
      (define <xml-char> (<satisfy-char> (lambda (c)
                                           (or (char=? c #\x9) (char=? c #\xA) (char<=? #\x20 #\xD7FF)
                                               (char<=? #\xE000 #\xFFFD) (char<=? #\x10000 #\x10FFFF)))))
      (define <name-start-char> (</> <letter> (<one-of> ":_")
                                     (<satisfy-char> (lambda (c)
                                                       (or (char<=? #\xC0 c #\xD6)     (char<=? #\xD8 c #\xF6)
                                                           (char<=? #\xF8 c #\x2FF)    (char<=? #\x370 c #\x37D)
                                                           (char<=? #\x37F c #\x1FFF)  (char<=? #\x200C c #\x200D)
                                                           (char<=? #\x2070 c #\x218F) (char<=? #\x2C00 c #\x2FEF)
                                                           (char<=? #\x3001 c #\xD7FF) (char<=? #\xF900 c #\xFDCF)
                                                           (char<=? #\xFDF0 c #\xFFFD) (char<=? #\x10000 c #\xEFFFF))))))
      (define <name-char> (</> <name-start-char> <digit> (<one-of> "-.\xB7;")
                               (<satisfy-char> (lambda (c) (or (char<=? #\x0300 c #\x036F) (char<=? #\x203F c #\x2040))))))
      (define <name> (<map> (lambda (val) (apply string (cons (car val) (cadr val))))
                            (<~> <name-start-char> (<many> <name-char>))))

      ;; TODO refine
      (define <comment> (<map> (lambda (val) `(comment ,(cadr val)))
                               (<~> (<string> "<!--")
                                    (<many> (</> (<none-of> "-") (<~> (<char> #\-) (<none-of> "-"))))
                                    (<string> "-->"))))
      (define <misc> (</> <comment> <S>))

      (define <predefined-char-ref> (</> (<as> #\< (<string> "&lt;"))   (<as> #\> (<string> "&gt;"))
                                         (<as> #\' (<string> "&apos;")) (<as> #\" (<string> "&quot;"))
                                         (<as> #\& (<string> "&amp;"))))
      (define <char-ref> (</> (<~1> (<string> "&#")
                                    (<map> (lambda (d*) (integer->char (hexdigits->num d*))) (<some> <digit10>))
                                    (<char> #\;))
                              (<~1> (<string> "&#x")
                                    (<map> (lambda (d*) (integer->char (digits->num d*))) (<some> <digit16>))
                                    (<char> #\;))
                              <predefined-char-ref>))

      (define <attr-value> (<token> (</> (<~1> (<char> #\")
                                               (<as-string> (<many> (</> (<none-of> "<&\"") <char-ref>)))
                                               (<char> #\"))
                                         (<~1> (<char> #\')
                                               (<as-string> (<many> (</> (<none-of> "<&'") <char-ref>)))
                                               (<char> #\')))))

      (define <doctype-decl> (<~> (<token> (<string> "<!DOCTYPE"))
                                  (<fail-with> "XML with DOCTYPE not supported")))

      (define <version-num> (<map> (lambda (val) (string-append (car val) (cadr val)))
                                   (<~> (<string> "1.") (<as-string> (<some> <digit>)))))
      (define <version-info> (<map> (lambda (val) (cons (car val) (caddr val)))
                                    (<~> (<token> (<string> "version"))
                                         (<token> (<string> "="))
                                         (<token> (</> (<~n> 1 (<char> #\") <version-num> (<char> #\"))
                                                       (<~n> 1 (<char> #\') <version-num> (<char> #\')))))))
      ;; utf-8 only
      (define <enc-name> (</> (<string> "UTF-8") (<string> "utf-8")
                              (<fail-with> "XML with encodings other than utf-8 not supported")))
      (define <encoding-decl> (<map> (lambda (val) (cons (car val) (caddr val)))
                                     (<~> (<token> (<string> "encoding"))
                                          (<token> (<string> "="))
                                          (<token> (</> (<~n> 1 (<char> #\") <enc-name> (<char> #\"))
                                                        (<~n> 1 (<char> #\') <enc-name> (<char> #\')))))))
      (define <yes/no> (</> (<string> "yes") (<string> "no")))
      (define <standalone-decl> (<map> (lambda (val) (cons (car val) (caddr val)))
                                       (<~> (<token> (<string> "standalone"))
                                            (<token> (<string> "="))
                                            (<token> (</> (<~n> 1 (<char> #\") <yes/no> (<char> #\"))
                                                          (<~n> 1 (<char> #\') <yes/no> (<char> #\')))))))
      (define <xml-decl> (<map> (lambda (val)
                                  `(xml-declaration ,(list-ref val 1) ,(list-ref val 2) ,(list-ref val 3)))
                                (<~> (<token> (<string> "<?xml"))
                                     <version-info>
                                     (<optional> <encoding-decl>)
                                     (<optional> <standalone-decl>)
                                     (<token> (<string> "?>")))))
      (define <prolog> (<~0> (<optional> <xml-decl>)
                             (<many> <misc>)
                             (<optional> (<~> <doctype-decl> (<many> <misc>)))))

      (define <attr> (<map> (lambda (val) (cons (car val) (list-ref val 2)))
                            (<~> (<token> <name>) (<token> (<char> #\=)) <attr-value>)))

      (define <CDATA> (<map> (lambda (val)
                               `(CDATA ,(apply string `(,@(list-ref val 0) ,(list-ref val 1)))))
                             (<~1> (<string> "<![CDATA[")
                                   ;; TODO refine, exclude ]]>, maybe use a new PC
                                   (<~> (<many> (<not-followed-by> <xml-char> (<string> "]]>")))
                                        ;; the last char before ]]>
                                        <item>)
                                   (<string> "]]>"))))
      ;; TODO refine, exclude ]]>
      (define <char-data> (<many> (</> (<none-of> "<&") <char-ref>)))
      (define all-ws? (lambda (val) (andmap char-whitespace? val)))

      (define <element>
        (<bind> (~> (<char> #\<) (<token> <name>))
                (lambda (tag)
                  ;;(printf "read tag: ~a~n" tag)
                  (<map> (lambda (val) `(element ,tag ,(list-ref val 0) ,(list-ref val 1)))
                         (<~> (<map> (lambda (val) (cons 'attr val)) (<many> <attr>))
                              (<map> (lambda (val) (cons 'content val))
                                     (</> (<~1> (<token> (<string> ">"))
                                                (<map> (lambda (val)
                                                         ;; merge CDATA, char-data
                                                         (let* ([char-data (car val)] [rest (cadr val)]
                                                                [char-data* (map (lambda (x) (cadr x)) rest)]
                                                                [other*     (map (lambda (x) (car x))  rest)]
                                                                [cdata* (filter (lambda (x) (eq? (car x) 'CDATA))   other*)]
                                                                [elem*  (filter (lambda (x) (eq? (car x) 'element)) other*)]
                                                                [text (let ([t (apply append char-data char-data*)])
                                                                        (if (all-ws? t) '() (list (apply string t))))])
                                                           `((text ,@text)
                                                             (CDATA ,@(map (lambda (x) (cadr x)) cdata*))
                                                             ,@elem*)))
                                                       (<~> ;; content
                                                        (<optional> <char-data>)
                                                        ;; char-ref not needed below
                                                        (<many> (<~> (</> <CDATA>    (<msg-f> "2")  ;; <![CDATA[
                                                                          <comment>  (<msg-f> "3")  ;; <!--
                                                                          <element>  (<msg-f> "4")) ;; <
                                                                     ;; serves as tokenizer
                                                                     (<optional> <char-data>)))))
                                                (<string> (format "</~a>" tag)))
                                          (<as> '() (<string> "/>")))))))))
      (define <document> (<fully> (<~ (<~> <prolog> <element>) (<many> <misc>))))

      <document>))


  #|doc
  `str` must be a string representing a valid xml document.

  `parse-xml` tries to parse the xml document represented as `str`.
  If successful, the parsed xml document is returned;
  otherwise, an error with condition type &parser-error is raised.

  Note: this parser does not support parsing DTDs.
  |#
  (define parse-xml
    (lambda (str)
      (pcheck ([string? str])
              (run-textual-parser parser-xml str))))


  #|doc
  `path` must be a path string that points to a valid xml document file.

  `parse-xml-file` tries to parse the xml file at `path`.
  If successful, the parsed xml document is returned;
  otherwise, an error with condition type &parser-error is raised.

  Note: this parser does not support parsing DTDs.
  |#
  (define parse-xml-file
    (lambda (path)
      (pcheck ([string? path])
              (parse-textual-file parser-xml path))))

  )
