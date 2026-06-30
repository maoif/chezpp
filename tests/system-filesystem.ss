(import (chezpp))

(mat filesystem-info-basic

     (let ([info (filesystem-info ".")])
       (and (filesystem-info? info)
            (string? (filesystem-info-path info))
            (integer? (filesystem-info-block-size info))
            (integer? (filesystem-info-blocks info))
            (integer? (filesystem-info-blocks-free info))
            (integer? (filesystem-info-blocks-available info))
            (or (not (filesystem-info-device info))
                (integer? (filesystem-info-device info)))
            (or (not (filesystem-info-inode info))
                (integer? (filesystem-info-inode info)))))

     (<= 0 (filesystem-total-bytes "."))
     (<= 0 (filesystem-free-bytes "."))
     (<= 0 (filesystem-available-bytes ".")))

(mat mounted-filesystems-basic

     (let ([mounts (mounted-filesystems)])
       (and (list? mounts)
            (exists (lambda (m)
                      (and (mounted-filesystem? m)
                           (string? (mounted-filesystem-target m))
                           (string=? "/" (mounted-filesystem-target m))))
                    mounts))))
