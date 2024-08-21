#define _GNU_SOURCE

#include "common.h"

#include <sys/stat.h>
#include <fcntl.h>



// Various checks (e.g., file existence) should be performed on Scheme side.

// Error handling is context-dependent:
// If the function returns a string, it returns a vector with the error string as the sole element.
// If the function returns boolean, it can return the error string when error occurs.
// If the function returns other Scheme objects, ditto.

ptr chezpp_statx(const char *path, int follow_link);



ptr chezpp_statx(const char *path, int follow_link) {
  char *p = expand_pathname(path);

  struct statx res;
  int op = follow_link ? 0 : AT_SYMLINK_NOFOLLOW;
  if (statx(AT_FDCWD, p, op, STATX_BASIC_STATS | STATX_BTIME, &res) == -1) {
    return errno_str();
  }

  free(p);

  ptr v = Smake_vector(14 + 4, Sfalse);
  __u32 mask = res.stx_mask;
  if (mask & STATX_TYPE) {
    Svector_set(v, 0, Sfixnum(res.stx_mode & S_IFMT));
  }
  if (mask & STATX_MODE) {
    Svector_set(v, 1, Sfixnum(res.stx_mode & ~S_IFMT));
  }
  if (mask & STATX_NLINK) {
    Svector_set(v, 2, Sfixnum(res.stx_nlink));
  }
  if (mask & STATX_UID) {
    Svector_set(v, 3, Sfixnum(res.stx_uid));
  }
  if (mask & STATX_GID) {
    Svector_set(v, 4, Sfixnum(res.stx_gid));
  }
  if (mask & STATX_ATIME) {
    struct statx_timestamp t = res.stx_atime;
    Svector_set(v, 5, Sinteger(t.tv_sec));
    Svector_set(v, 6, Sinteger(t.tv_nsec));
  }
  if (mask & STATX_CTIME) {
    struct statx_timestamp t = res.stx_ctime;
    Svector_set(v, 7, Sinteger(t.tv_sec));
    Svector_set(v, 8, Sinteger(t.tv_nsec));
  }
  if (mask & STATX_MTIME) {
    struct statx_timestamp t = res.stx_mtime;
    Svector_set(v, 9, Sinteger(t.tv_sec));
    Svector_set(v, 10, Sinteger(t.tv_nsec));
  }
  if (mask & STATX_BTIME) {
    struct statx_timestamp t = res.stx_btime;
    Svector_set(v, 11, Sinteger(t.tv_sec));
    Svector_set(v, 12, Sinteger(t.tv_nsec));
  }
  if (mask & STATX_INO) {
    Svector_set(v, 13, Sfixnum(res.stx_ino));
  }
  if (mask & STATX_SIZE) {
    Svector_set(v, 14, Sfixnum(res.stx_size));
  }
  if (mask & STATX_BLOCKS) {
    Svector_set(v, 15, Sfixnum(res.stx_blocks));
  }
  Svector_set(v, 16, Sfixnum(res.stx_dev_major));
  Svector_set(v, 17, Sfixnum(res.stx_dev_minor));

  return v;
}

