(library (chezpp net dns)
  (export dns-resolve
          dns-resolve/ipv4
          dns-resolve/ipv6
          dns-reverse-resolve
          dns-result?
          dns-result-addresses
          dns-result-canonname)
  (import (chezpp chez)
          (chezpp utils)
          (chezpp net errors)
          (chezpp net ffi)
          (chezpp net private)
          (chezpp net address))

  (define ensure-success
    (lambda (who x)
      (when (ffi-error? x)
        (raise-net-error who 'dns (ffi-error-message x) x))
      x))

  (define dns-resolve*
    (lambda (who host family)
      (pcheck ([string? host])
              (%dns-result-from-ffi
               (ensure-success
                who
                (ffi-net-resolve-addresses host -1
                                           (family-symbol->int who family)
                                           0))))))

  #|proc:dns-resolve
The `dns-resolve` procedure resolves a host name and returns a DNS result record.
|#
  (define-who dns-resolve
    (lambda (host)
      (dns-resolve* who host #f)))

  #|proc:dns-resolve/ipv4
The `dns-resolve/ipv4` procedure resolves a host name and restricts results to IPv4 addresses.
|#
  (define-who dns-resolve/ipv4
    (lambda (host)
      (dns-resolve* who host 'inet)))

  #|proc:dns-resolve/ipv6
The `dns-resolve/ipv6` procedure resolves a host name and restricts results to IPv6 addresses.
|#
  (define-who dns-resolve/ipv6
    (lambda (host)
      (dns-resolve* who host 'inet6)))

  #|proc:dns-reverse-resolve
The `dns-reverse-resolve` procedure performs a reverse DNS lookup for a socket address.
|#
  (define-who dns-reverse-resolve
    (lambda (address)
      (pcheck ([socket-address? address])
              (address->name address))))
  )
