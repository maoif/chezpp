(import (chezpp))

(mat net-uri-raw-components
     (let ([u (string->uri "http://example.test/a%2Fb?q=%2F#frag%2F")])
       (and (string=? "/a%2Fb" (uri-raw-path u))
            (string=? "q=%2F" (uri-raw-query u))
            (string=? "http://example.test/a%2Fb?q=%2F#frag%2F" (uri->string u))
            (string=? "/a%2Fc" (uri-path (uri-update u 'path "/a%2Fc"))))))

(mat net-uri-constructor
     (string=? "https://example.test/a%2Fb"
               (uri->string (make-uri "https" #f "example.test" #f "/a%2Fb" #f #f))))
