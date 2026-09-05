(import (chezpp)
        (chezpp parser xml))

(define bytevector-concatenate
  (lambda bytevectors
    (let* ([length (fold-left
                    (lambda (length bytevector)
                      (+ length (bytevector-length bytevector)))
                    0
                    bytevectors)]
           [result (make-bytevector length 0)])
      (let loop ([bytevectors bytevectors] [offset 0])
        (if (null? bytevectors)
            result
            (let* ([bytevector (car bytevectors)]
                   [length (bytevector-length bytevector)])
              (bytevector-copy! bytevector 0 result offset length)
              (loop (cdr bytevectors) (+ offset length))))))))

(define bytevector-drop
  (lambda (bytevector count)
    (let* ([length (- (bytevector-length bytevector) count)]
           [result (make-bytevector length 0)])
      (bytevector-copy! bytevector count result 0 length)
      result)))

(define with-temporary-xml-bytes
  (lambda (bytes procedure)
    (let ([path (format "parser-xml-~a.xml" (random 999999))]
          [port #f])
      (dynamic-wind
        (lambda ()
          (set! port
                (open-file-output-port path
                                       (file-options no-fail replace)
                                       (buffer-mode block)
                                       #f))
          (put-bytevector port bytes)
          (flush-output-port port))
        (lambda () (procedure path))
        (lambda ()
          (when port (close-port port))
          (when (file-exists? path) (delete-file path)))))))

(mat parse-xml-records

     (string=? "#[xml-attribute name: \"id\" value: \"1\"]"
               (format "~s"
                       (vector-ref
                        (xml-element-attributes
                         (xml-document-root (parse-xml "<x id='1'/>")))
                        0)))

     (let* ([document (parse-xml "<root a=\"1\">x<![CDATA[y]]><child/>z</root>")]
            [root (xml-document-root document)]
            [children (vector->list (xml-element-children root))])
       (and (xml-document? document)
            (xml-element? root)
            (string=? "root" (xml-element-name root))
            (= 1 (vector-length (xml-element-attributes root)))
            (xml-text? (list-ref children 0))
            (xml-cdata? (list-ref children 1))
            (xml-element? (list-ref children 2))
            (xml-text? (list-ref children 3))))

     )

(mat parse-xml-lexical

     (let ([root (xml-document-root
                  (parse-xml "<\x3B1;_1>\x41;&#65;&#x41;</\x3B1;_1>"))])
       (and (string=? "\x3B1;_1" (xml-element-name root))
            (string=? "AAA"
                      (xml-text-value
                       (vector-ref (xml-element-children root) 0)))))

     ;; error: a name cannot start with an ASCII digit.
     (error? (parse-xml "<1root/>"))

     ;; error: a numeric reference must denote an XML Char.
     (error? (parse-xml "<root>&#0;</root>"))

     ;; error: undeclared general entities are unavailable when DTDs are unsupported.
     (error? (parse-xml "<root>&custom;</root>"))

     (let ([document
            (parse-xml "<?build release?><root><!--ok--><![CDATA[a<b&c]]></root>")])
       (and (xml-processing-instruction?
             (vector-ref (xml-document-before-root document) 0))
            (xml-comment?
             (vector-ref (xml-element-children (xml-document-root document)) 0))
            (xml-cdata?
             (vector-ref (xml-element-children (xml-document-root document)) 1))))

     ;; error: XML comments cannot contain a double hyphen.
     (error? (parse-xml "<root><!-- bad -- comment --></root>"))

     ;; error: CDATA closing delimiters cannot occur in character data.
     (error? (parse-xml "<root><![CDATA[a]]>b]]></root>"))

     ;; error: processing-instruction targets cannot case-insensitively equal xml.
     (error? (parse-xml "<?XML data?><root/>"))

     )

(mat parse-xml-structure

     (let ([declaration
            (xml-document-declaration
             (parse-xml
              "<?xml version='1.0' encoding='UTF-8' standalone='yes'?><root/>"))])
       (and (xml-declaration? declaration)
            (string=? "1.0" (xml-declaration-version declaration))
            (string-ci=? "UTF-8" (xml-declaration-encoding declaration))
            (eq? 'yes (xml-declaration-standalone declaration))))

     ;; error: an end tag must match the open element name.
     (error? (parse-xml "<a></b>"))

     ;; error: an element cannot repeat an attribute name.
     (error? (parse-xml "<a x='1' x='2'/>"))

     ;; error: an XML document must contain exactly one root element.
     (error? (parse-xml "<a/><b/>"))

     ;; error: document type declarations are not supported.
     (error? (parse-xml "<!DOCTYPE root><root/>"))

     (string=? "a\nb\nc"
               (xml-text-value
                (vector-ref
                 (xml-element-children
                  (xml-document-root (parse-xml "<r>a\r\nb\rc</r>")))
                 0)))

     )

(mat parse-xml-files

     (xml-document? (parse-xml-file "data/xml-basic.xml"))

     (xml-document? (parse-xml-file "data/large-dataset.xml"))

     (xml-document?
      (with-temporary-xml-bytes
       (bytevector-concatenate (bytevector #xef #xbb #xbf)
                               (string->utf8 "<root/>"))
       parse-xml-file))

     (and
      (xml-document?
       (with-temporary-xml-bytes
        (string->bytevector
         "<?xml version='1.0' encoding='UTF-16'?><root/>"
         (make-transcoder (utf-16-codec (endianness little))
                          (eol-style none)
                          (error-handling-mode raise)))
        parse-xml-file))
      (xml-document?
       (with-temporary-xml-bytes
        (string->bytevector
         "<?xml version='1.0' encoding='UTF-16'?><root/>"
         (make-transcoder (utf-16-codec (endianness big))
                          (eol-style none)
                          (error-handling-mode raise)))
        parse-xml-file)))

     (and
      (xml-document?
       (with-temporary-xml-bytes
        (bytevector-drop
         (string->bytevector
          "<?xml version='1.0' encoding='UTF-16'?><root/>"
          (make-transcoder (utf-16-codec (endianness little))
                           (eol-style none)
                           (error-handling-mode raise)))
         2)
        parse-xml-file))
      (xml-document?
       (with-temporary-xml-bytes
        (bytevector-drop
         (string->bytevector
          "<?xml version='1.0' encoding='UTF-16'?><root/>"
          (make-transcoder (utf-16-codec (endianness big))
                           (eol-style none)
                           (error-handling-mode raise)))
         2)
        parse-xml-file)))

     ;; error: malformed UTF-8 must not be replaced during decoding.
     (error?
      (with-temporary-xml-bytes (bytevector #xc3 #x28) parse-xml-file))

     ;; error: UTF-16 input must contain complete code units.
     (error?
      (with-temporary-xml-bytes (bytevector #xff #xfe #x3c) parse-xml-file))

     ;; error: the declaration encoding must agree with the detected bytes.
     (error?
      (with-temporary-xml-bytes
       (string->utf8 "<?xml version='1.0' encoding='UTF-16'?><root/>")
       parse-xml-file))

     ;; error: UTF-16 input cannot contain an unpaired high surrogate.
     (error?
      (with-temporary-xml-bytes
       (bytevector #xff #xfe
                   #x3c #x00 #x72 #x00 #x3e #x00
                   #x00 #xd8
                   #x3c #x00 #x2f #x00 #x72 #x00 #x3e #x00)
       parse-xml-file))

     )
