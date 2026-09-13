(import (chezpp))

(mat regex-basic
     (let* ([rx (string->regex "(ab)+")]
            [m (regex-search rx "xxababyy")])
       (and (regex? rx)
            (regex-match? m)
            (= 2 (regex-match-start-index m 0))
            (= 6 (regex-match-end-index m 0))
            (string=? "ab" (regex-match-substring m 1)))))

;; Error case: regex APIs reject values of the wrong type.
(mat regex-type-errors
     (guard (c [(error? c) #t] [else #f])
       (string->regex 1)
       #f)
     (guard (c [(error? c) #t] [else #f])
       (regex-search 1 "x")
       #f))
