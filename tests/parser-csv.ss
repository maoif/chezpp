(import (chezpp)
        (chezpp parser csv))

(define csv-fields
  (lambda (document)
    (map (lambda (record) (vector->list (csv-record-fields record)))
         (vector->list (csv-document-records document)))))

(mat parse-csv-records

     (string=? "#[csv-record fields: #(\"a\" \"b\")]"
               (format "~s"
                       (vector-ref (csv-document-records (parse-csv "a,b")) 0)))

     (let ([document (parse-csv "")])
       (and (csv-document? document)
            (char=? #\, (csv-document-delimiter document))
            (equal? '() (csv-fields document))))

     (equal? '(("a" "b") ("1" "2"))
             (csv-fields (parse-csv "a,b\r\n1,2")))

     (equal? '(("aaa" "bbb" "ccc") ("xxx" "yyy" "zzz"))
             (csv-fields (parse-csv "aaa,bbb,ccc\nxxx,yyy,zzz\n")))

     (equal? '(("aaa " "  bbb " " ccc") (" xxx" " yyy  " "zzz "))
             (csv-fields (parse-csv "aaa ,  bbb , ccc\r xxx, yyy  ,zzz \r")))

     (equal? '(("aaa" "b\r\nbb" "ccc") ("xxx" "y, yy" "zzz"))
             (csv-fields (parse-csv "aaa,\"b\r\nbb\",ccc\r\nxxx,\"y, yy\",zzz")))

     (equal? '(("aaa" "b\"bb" "ccc"))
             (csv-fields (parse-csv "aaa,\"b\"\"bb\",ccc")))

     (equal? '(("" "" "" ""))
             (csv-fields (parse-csv ",,,")))

     (equal? '(("a" "b") ("1" "2"))
             (csv-fields (parse-csv "a;b\n1;2" #\;)))

     ;; error: a quoted field must end with a double quote.
     (error? (parse-csv "a,\"unterminated"))

     ;; error: a double quote cannot occur inside an unquoted field.
     (error? (parse-csv "a,b\"c"))

     ;; error: non-space data cannot follow a closing quote before the delimiter.
     (error? (parse-csv "a,\"b\"x,c"))

     ;; error: a line break cannot be used as the field delimiter.
     (error? (parse-csv "a\nb" #\newline))

     ;; error: every record must contain the same number of fields.
     (error? (parse-csv "a,b\n1,2,3"))

     (let ([document (parse-csv "a,  \"b,c\"  ,d")])
       (and (equal? '(("a" "b,c" "d")) (csv-fields document))
            (= 1 (vector-length (csv-document-warnings document)))
            (csv-warning? (vector-ref (csv-document-warnings document) 0))))

     (equal? '(("name" "value") ("alpha" "1"))
             (csv-fields (parse-csv-file "data/csv-basic.csv")))

     (csv-document?
      (parse-csv-file "data/New-Zealand-period-life-tables-2017-2019-CSV.csv"))

     )
