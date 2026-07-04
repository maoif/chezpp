(library (chezpp system filesystem)
  (export
          ;; filesystem capacity records
          filesystem-info?
          filesystem-info
          filesystem-info-path
          filesystem-info-device
          filesystem-info-type
          filesystem-info-block-size
          filesystem-info-blocks
          filesystem-info-blocks-free
          filesystem-info-blocks-available
          filesystem-info-files
          filesystem-info-files-free
          filesystem-info-read-only?
          filesystem-total-bytes filesystem-free-bytes filesystem-available-bytes

          ;; mounted filesystem records
          mounted-filesystem?
          mounted-filesystems
          mounted-filesystem-source
          mounted-filesystem-target
          mounted-filesystem-type
          mounted-filesystem-options)
  (import (chezpp chez)
          (chezpp string)
          (chezpp system common)
          (chezpp utils))

;;;;===----------------------------------------------------------------------===
;;;; filesystem information
;;;;===----------------------------------------------------------------------===

  #|proc:filesystem-info?
The `filesystem-info?` procedure returns `#t` when its argument is a filesystem information record, otherwise `#f`.
The `object` parameter is the object to test.
|#
  #|proc:filesystem-info-path
The `filesystem-info-path` procedure returns the queried path stored in a filesystem information record.
The `info` parameter is a filesystem information record returned by `filesystem-info`.
|#
  #|proc:filesystem-info-device
The `filesystem-info-device` procedure returns the device number for the queried path, or `#f` when unavailable.
The `info` parameter is a filesystem information record returned by `filesystem-info`.
|#
  #|proc:filesystem-info-type
The `filesystem-info-type` procedure returns the filesystem type symbol for the queried path, or `#f` when unavailable.
The `info` parameter is a filesystem information record returned by `filesystem-info`.
|#
  #|proc:filesystem-info-block-size
The `filesystem-info-block-size` procedure returns the filesystem block size in bytes.
The `info` parameter is a filesystem information record returned by `filesystem-info`.
|#
  #|proc:filesystem-info-blocks
The `filesystem-info-blocks` procedure returns the total number of filesystem blocks.
The `info` parameter is a filesystem information record returned by `filesystem-info`.
|#
  #|proc:filesystem-info-blocks-free
The `filesystem-info-blocks-free` procedure returns the number of free filesystem blocks.
The `info` parameter is a filesystem information record returned by `filesystem-info`.
|#
  #|proc:filesystem-info-blocks-available
The `filesystem-info-blocks-available` procedure returns the number of filesystem blocks available to unprivileged users.
The `info` parameter is a filesystem information record returned by `filesystem-info`.
|#
  #|proc:filesystem-info-files
The `filesystem-info-files` procedure returns the total number of file nodes in the filesystem.
The `info` parameter is a filesystem information record returned by `filesystem-info`.
|#
  #|proc:filesystem-info-files-free
The `filesystem-info-files-free` procedure returns the number of free file nodes in the filesystem.
The `info` parameter is a filesystem information record returned by `filesystem-info`.
|#
  #|proc:filesystem-info-read-only?
The `filesystem-info-read-only?` procedure returns `#t` when the filesystem is mounted read-only, otherwise `#f`.
The `info` parameter is a filesystem information record returned by `filesystem-info`.
|#
  (define-record-type ($filesystem-info make-filesystem-info filesystem-info?)
    (nongenerative)
    (fields (immutable path filesystem-info-path)
            (immutable device filesystem-info-device)
            (immutable type filesystem-info-type)
            (immutable block-size filesystem-info-block-size)
            (immutable blocks filesystem-info-blocks)
            (immutable blocks-free filesystem-info-blocks-free)
            (immutable blocks-available filesystem-info-blocks-available)
            (immutable files filesystem-info-files)
            (immutable files-free filesystem-info-files-free)
            (immutable read-only? filesystem-info-read-only?)))

  (define $filesystem-info-ffi
    (foreign-procedure "chezpp_filesystem_info" (string) ptr))

  (define $vector->filesystem-info
    (lambda (v)
      (make-filesystem-info (vector-ref v 0)
                            (vector-ref v 1)
                            (vector-ref v 2)
                            (vector-ref v 3)
                            (vector-ref v 4)
                            (vector-ref v 5)
                            (vector-ref v 6)
                            (vector-ref v 7)
                            (vector-ref v 8)
                            (vector-ref v 9))))

  #|proc:filesystem-info
The `filesystem-info` procedure returns filesystem capacity and identity information for `path`.
The `path` parameter is a string naming a path on the filesystem to query.
|#
  (define filesystem-info
    (lambda (path)
      (pcheck ([string? path])
              ($vector->filesystem-info (ffi-result-ref ($filesystem-info-ffi path))))))

  (define $filesystem-bytes
    (lambda (path block-selector)
      (let ([info (filesystem-info path)])
        (* (filesystem-info-block-size info) (block-selector info)))))

  #|proc:filesystem-total-bytes
The `filesystem-total-bytes` procedure returns the total filesystem size in bytes for `path`.
The `path` parameter is a string naming a path on the filesystem to query.
|#
  (define filesystem-total-bytes
    (lambda (path)
      (pcheck ([string? path])
              ($filesystem-bytes path filesystem-info-blocks))))

  #|proc:filesystem-free-bytes
The `filesystem-free-bytes` procedure returns the free filesystem size in bytes for `path`.
The `path` parameter is a string naming a path on the filesystem to query.
|#
  (define filesystem-free-bytes
    (lambda (path)
      (pcheck ([string? path])
              ($filesystem-bytes path filesystem-info-blocks-free))))

  #|proc:filesystem-available-bytes
The `filesystem-available-bytes` procedure returns the filesystem size in bytes available to unprivileged users for `path`.
The `path` parameter is a string naming a path on the filesystem to query.
|#
  (define filesystem-available-bytes
    (lambda (path)
      (pcheck ([string? path])
              ($filesystem-bytes path filesystem-info-blocks-available))))

;;;;===----------------------------------------------------------------------===
;;;; mounted filesystems
;;;;===----------------------------------------------------------------------===

  #|proc:mounted-filesystem?
The `mounted-filesystem?` procedure returns `#t` when its argument is a mounted filesystem record, otherwise `#f`.
The `object` parameter is the object to test.
|#
  #|proc:mounted-filesystem-source
The `mounted-filesystem-source` procedure returns the source device or pseudo-device of a mounted filesystem record.
The `mount` parameter is a mounted filesystem record returned by `mounted-filesystems`.
|#
  #|proc:mounted-filesystem-target
The `mounted-filesystem-target` procedure returns the mount target path of a mounted filesystem record.
The `mount` parameter is a mounted filesystem record returned by `mounted-filesystems`.
|#
  #|proc:mounted-filesystem-type
The `mounted-filesystem-type` procedure returns the filesystem type string of a mounted filesystem record.
The `mount` parameter is a mounted filesystem record returned by `mounted-filesystems`.
|#
  #|proc:mounted-filesystem-options
The `mounted-filesystem-options` procedure returns the raw mount options string of a mounted filesystem record.
The `mount` parameter is a mounted filesystem record returned by `mounted-filesystems`.
|#
  (define-record-type ($mounted-filesystem make-mounted-filesystem mounted-filesystem?)
    (nongenerative)
    (fields (immutable source mounted-filesystem-source)
            (immutable target mounted-filesystem-target)
            (immutable type mounted-filesystem-type)
            (immutable options mounted-filesystem-options)))

  (define $list-ref/default
    (lambda (items index default)
      (let loop ([items items] [index index])
        (cond [(null? items) default]
              [(fx= index 0) (car items)]
              [else (loop (cdr items) (fx- index 1))]))))

  (define $list-index-string
    (lambda (items needle)
      (let loop ([items items] [index 0])
        (cond [(null? items) #f]
              [(string=? (car items) needle) index]
              [else (loop (cdr items) (fx+ index 1))]))))

  (define $mountinfo-unescape
    (lambda (field)
      (list->string
       (let ([n (string-length field)])
         (let loop ([i 0] [chars '()])
           (if (fx= i n)
               (reverse chars)
               (if (and (char=? (string-ref field i) #\\)
                        (fx<= (fx+ i 3) (fx- n 1)))
                   (let ([escape (substring field (fx+ i 1) (fx+ i 4))])
                     (cond [(string=? escape "040") (loop (fx+ i 4) (cons #\space chars))]
                           [(string=? escape "011") (loop (fx+ i 4) (cons #\tab chars))]
                           [(string=? escape "012") (loop (fx+ i 4) (cons #\newline chars))]
                           [(string=? escape "134") (loop (fx+ i 4) (cons #\\ chars))]
                           [else (loop (fx+ i 1) (cons (string-ref field i) chars))]))
                   (loop (fx+ i 1) (cons (string-ref field i) chars)))))))))

  (define $parse-mountinfo-line
    (lambda (line)
      (let* ([fields (string-split line #\space)]
             [field-count (length fields)]
             [separator-index ($list-index-string fields "-")])
        (and separator-index
             (fx<= 6 separator-index)
             (fx<= (fx+ separator-index 4) field-count)
             (let* ([source ($list-ref/default fields (fx+ separator-index 2) "")]
                    [target ($list-ref/default fields 4 "")]
                    [type ($list-ref/default fields (fx+ separator-index 1) "")]
                    [mount-options ($list-ref/default fields 5 "")]
                    [super-options ($list-ref/default fields (fx+ separator-index 3) "")]
                    [options (if (string=? super-options "")
                                 mount-options
                                 (string-append mount-options " " super-options))])
               (make-mounted-filesystem ($mountinfo-unescape source)
                                        ($mountinfo-unescape target)
                                        type
                                        options))))))

  (define $read-mounted-filesystems
    (lambda (path)
      (call-with-input-file path
        (lambda (port)
          (let loop ([mounts '()])
            (let ([line (get-line port)])
              (if (eof-object? line)
                  (reverse mounts)
                  (let ([mount ($parse-mountinfo-line line)])
                    (loop (if mount (cons mount mounts) mounts))))))))))

  #|proc:mounted-filesystems
The `mounted-filesystems` procedure returns a list of mounted filesystem records for the current process mount namespace.
|#
  (define mounted-filesystems
    (lambda ()
      (let ([mountinfo "/proc/self/mountinfo"])
        (if (file-exists? mountinfo)
            ($read-mounted-filesystems mountinfo)
            '()))))

  )
