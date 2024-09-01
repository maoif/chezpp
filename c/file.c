#define _GNU_SOURCE

#include "common.h"

#include <sys/stat.h>
#include <sys/inotify.h>
#include <fcntl.h>



// Various checks (e.g., file existence) should be performed on Scheme side.

// Error handling is context-dependent:
// If the function returns a string, it returns a vector with the error string as the sole element.
// If the function returns boolean, it can return the error string when error occurs.
// If the function returns other Scheme objects, ditto.

ptr chezpp_statx(const char *path, int follow_link);
ptr chezpp_readlink(const char *path);
ptr chezpp_link(const char *src, const char *dest);
ptr chezpp_symlink(const char *src, const char *dest);
ptr chezpp_touch(const char *path, ptr atime, ptr mtime, int follow_link);

ptr chezpp_fswatcher_init(int block);
ptr chezpp_fswatcher_close(int fsw);
ptr chezpp_fswatcher_add(int fsw, const char *path, int mask);
ptr chezpp_fswatcher_remove(int fsw, int id);
ptr chezpp_fswatcher_next(int fsw);


ptr chezpp_statx(const char *path, int follow_link) {
  char *p = expand_pathname(path);

  struct statx res;
  int op = follow_link ? 0 : AT_SYMLINK_NOFOLLOW;
  if (statx(AT_FDCWD, p, op, STATX_BASIC_STATS | STATX_BTIME, &res) == -1) {
    free(p);
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

ptr chezpp_readlink(const char *path) {
  char *p = expand_pathname(path);
  char link[PATH_MAX];
  memset(link, 0, sizeof(link));
  if (readlink(p, link, sizeof(link)) == -1) {
    free(p);
    return errno_str_vector();
  }

  free(p);


  return Sstring(link);
}

ptr chezpp_link(const char *src, const char *dest) {
  char *s = expand_pathname(src);
  char *d = expand_pathname(dest);
  if (link(s, d) == -1) {
    free(s);
    free(d);
    return errno_str();
  }

  free(s);
  free(d);

  return Strue;
}

ptr chezpp_symlink(const char *src, const char *dest) {
  char *s = expand_pathname(src);
  char *d = expand_pathname(dest);
  if (symlink(s, d) == -1) {
    free(s);
    free(d);
    return errno_str();
  }

  free(s);
  free(d);

  return Strue;
}

ptr chezpp_touch(const char *path, ptr atime, ptr mtime, int follow_link) {
  char *p = expand_pathname(path);
  struct timespec ts[2];

  // times are #f if left unchanged
  if (atime == Sfalse) {
    ts[0].tv_nsec = UTIME_OMIT;
  } else {
    ts[0].tv_nsec = Sinteger_value(Svector_ref(atime, 0));
    ts[0].tv_sec = Sinteger_value(Svector_ref(atime, 1));
  }

  if (mtime == Sfalse) {
    ts[1].tv_nsec = UTIME_OMIT;
  } else {
    ts[1].tv_nsec = Sinteger_value(Svector_ref(mtime, 0));
    ts[1].tv_sec = Sinteger_value(Svector_ref(mtime, 1));
  }

  if (utimensat(AT_FDCWD, p, ts, follow_link ? 0 : AT_SYMLINK_NOFOLLOW)) {
    free(p);
    return errno_str();
  }

  free(p);

  return Strue;
}

ptr chezpp_fswatcher_init(int block) {
  int res = inotify_init1(IN_CLOEXEC | (block ? 0 : IN_NONBLOCK));
  if (res == -1) {
    return errno_str();
  }

  return Sfixnum(res);
}

ptr chezpp_fswatcher_close(int fsw) {
  if (close(fsw) == -1) {
    return errno_str();
  }

  return Strue;
}

ptr chezpp_fswatcher_add(int fsw, const char *path, int mask) {
  char *p = expand_pathname(path);
  int fid = inotify_add_watch(fsw, p, mask);
  if (fid == -1) {
    free(p);
    return errno_str();
  }

  free(p);

  return Sfixnum(fid);
}

ptr chezpp_fswatcher_remove(int fsw, int fid) {
  if (inotify_rm_watch(fsw, fid) == -1) {
    return errno_str();
  }

  return Strue;
}

ptr chezpp_fswatcher_next(int fsw) {
  int len;
  const struct inotify_event *event;
  // just enough to read one event
  char buf[sizeof(struct inotify_event) + NAME_MAX + 1]
    __attribute__ ((aligned(__alignof__(struct inotify_event))));
  memset(buf, 0, sizeof(buf));

  len = read(fsw, buf, sizeof(buf));
  if (len == -1 && errno != EAGAIN) {
    return errno_str();
  }

  // non-blocking with no events
  if (len <= 0) return Sfalse;

  // TODO check for event queue overflow
  event = (const struct inotify_event *)buf;
  ptr v = Smake_vector(3, Sfalse);
  Svector_set(v, 0, Sfixnum(event->wd));
  Svector_set(v, 1, Sfixnum(event->mask));
  if (event->len > 0) {
    Svector_set(v, 2, Sstring(event->name));
  }

  return v;
}
