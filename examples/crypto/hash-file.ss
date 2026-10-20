(import (cspp)
        (cspp digest)
        (rename (cspp crypto hash) (hash-file crypto-hash-file)))
(load "examples/common.ss")

(define file-hash
  (lambda (method path)
    (case method
      [(sha224 sha256 sha384 sha512 sha3-224 sha3-256 sha3-384 sha3-512
        blake2b-512 blake2s-256)
       ;; Hash the file with the modern streaming crypto hash API.
       (crypto-hash-file method path)]
      [(md5 sha512-224 sha512-256 blake3)
       ;; Hash the file with a supported digest implementation.
       (case method
         [(md5) (md5-file path)]
         [(sha512-224) (sha512-224-file path)]
         [(sha512-256) (sha512-256-file path)]
         [(blake3) (blake3-file path)])]
      [(xxhash32)
       ;; Compute the non-cryptographic 32-bit checksum.
       (xxhash32-file path)]
      [(xxhash64)
       ;; Compute the non-cryptographic 64-bit checksum.
       (xxhash64-file path)]
      [else
       (errorf 'hash-file "unsupported method ~s" method)])))

(example-run!
 'hash-file
 "Print a file hash or checksum using a selected digest method."
 '((file . "Required: path to the file to hash.")
   (method . "Digest: sha224, sha256 (default), sha384, sha512, sha3-224/256/384/512, blake2b-512, blake2s-256, md5, sha512-224, sha512-256, blake3, xxhash32, or xxhash64. Use full names such as sha3-256; some require optional libraries."))
 (lambda (ref)
   (let* ([path (example-required 'hash-file ref 'file)]
          [method (string->symbol (or (ref 'method) "sha256"))]
          [digest (file-hash method path)])
     (example-print
      (list method path
            (if (bytevector? digest)
                (bytevector->hex digest)
                (number->string digest 16))))
     0))
 (command-line-arguments))
