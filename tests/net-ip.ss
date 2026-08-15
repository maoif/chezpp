(import (chezpp))

(mat net-ip-special
     (let ([mapped (string->ip-address "::ffff:192.0.2.1")])
       (and (ip-address-mapped-ipv4? mapped)
            (string=? "192.0.2.1"
                      (ip-address->string (ip-address-unmap-ipv4 mapped)))
            (ip-address-link-local? (string->ip-address "fe80::1"))
            (ip-address-documentation? (string->ip-address "192.0.2.1")))))

(mat net-ip-cidr-algebra
     (let* ([parent (cidr-parse "192.0.2.0/24")]
            [children (cidr-split parent)]
            [left (car children)]
            [right (cadr children)])
       (and (= 256 (cidr-address-count parent))
            (cidr-contains? parent (string->ip-address "192.0.2.200"))
            (cidr-overlaps? left parent)
            (not (cidr-overlaps? left (cidr-parse "192.0.3.0/24")))
            (cidr-merge left right)
            (= 24 (cidr-prefix-length (cidr-merge left right))))))
