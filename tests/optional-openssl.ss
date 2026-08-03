(import (chezpp))

(mat optional-openssl-fixture
     ;; The fixture supplies an incompatible OpenSSL runtime and expects lazy-load errors.
     (= 0 (system "./optional-openssl.sh")))
