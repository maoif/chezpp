(import (chezpp string))


(mat string-search

     (= 0 (string-search "" ""))
     (not (string-search "" "1"))
     (not (string-search "" "13"))
     (= 0 (string-search "1" "1"))
     (= 1 (string-search "21" "1"))

     )


(mat string-search-all

     (not (string-search-all "" "1"))
     (equal? '(0) (string-search-all "" ""))
     (equal? '(0 1 2 3) (string-search-all "1111" "1"))
     (equal? '(0 1 2) (string-search-all "1111" "11"))
     (equal? '(0 2) (string-search-all "1212" "12"))

     )


(mat string-contains?

     (string-contains? "" "")
     (string-contains? "1" "")
     (string-contains? "hfjdhfkjsdh" "")
     (not (string-contains? "1" "2"))
     (not (string-contains? "" "2"))

     )


(mat string-startswith?

     (string-startswith? "" "")
     (string-startswith? "qwe" "")
     (string-startswith? "qwe" "q")
     (string-startswith? "qwe" "qw")
     (string-startswith? "qwe" "qwe")
     (not (string-startswith? "qwe" "Q"))
     (not (string-startswith? "qwe" "qW"))

     )


(mat string-endswith?

     (string-endswith? "" "")
     (string-endswith? "qwe" "")
     (string-endswith? "qwe" "e")
     (string-endswith? "qwe" "we")
     (string-endswith? "qwe" "qwe")
     (not (string-endswith? "qwe" "E"))
     (not (string-endswith? "qwe" "wE"))

     )


(mat string-split

     ;; type error
     (error? (string-split #\a #\a))
     (error? (string-split "string" 1))

     ;; empty string
     (equal? '("") (string-split "" "foo"))
     (equal? '("") (string-split "" ""))
     (equal? '("") (string-split "" #\?))

     ;; delimiter on the side of the string
     (equal? '("" "quick" "fat" "fox" "") (string-split "@quick@fat@fox@" #\@))
     (equal? '("quick" "fat" "fox" "") (string-split "quick@fat@fox@" #\@))
     (equal? '("" "quick" "fat" "fox") (string-split "@quick@fat@fox" #\@))

     (equal? '("" "quick" "fat" "fox" "") (string-split "@@quick@@fat@@fox@@" "@@"))
     (equal? '("quick" "fat" "fox" "") (string-split "quick@@fat@@fox@@" "@@"))
     (equal? '("" "quick" "fat" "fox") (string-split "@@quick@@fat@@fox" "@@"))


     (equal? '("Lorem" "Ipsum" "is" "simply" "dummy" "text" "of" "the" "printing" "and" "typesetting" "industry")
             (string-split "Lorem Ipsum is simply dummy text of the printing and typesetting industry" #\space))

     )


(mat string-trim

     (error? (string-trim 123))
     (error? (string-trim "  123  " 123))

     (equal? "" (string-trim ""))
     (equal? "" (string-trim "   "))

     (equal? "x" (string-trim "x"))
     (equal? "x" (string-trim "  x"))
     (equal? "x" (string-trim "x  "))

     (equal? "xyz" (string-trim "xyz"))
     (equal? "xyz" (string-trim "  xyz"))
     (equal? "xyz" (string-trim "xyz  "))

     )
