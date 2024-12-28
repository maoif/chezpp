#include "common.h"


ptr errno_str() {
  if (errno == 0) {
    // e.g., getpwnam("bad_user")
    return Sstring("internal error");
  }
  return Sstring(strerror(errno));
}

ptr errno_str_vector() {
  ptr v = Smake_vector(1, Sfalse);
  Svector_set(v, 0, errno_str());
  return v;
}

// from ChezScheme: io.c
// The caller has to free the returned pointer.
#define DIRMARKERP(c) ((c) == '/')
char *expand_pathname(const char *inpath) {
  char *outpath;
  const char *ip;

  if (*inpath == '~') {
    const char *dir;
    size_t n1, n2;
    struct passwd *pwent;
    if (*(ip = inpath + 1) == 0 || DIRMARKERP(*ip)) {
      if ((dir = getenv("HOME")) == NULL)
        if ((pwent = getpwuid(getuid())) != NULL)
          dir = pwent->pw_dir;
    } else {
      char *userbuf;
      const char *user_start = ip;
      do {
        ip += 1;
      } while (*ip != 0 && !DIRMARKERP(*ip));
      if ((userbuf = malloc(ip - user_start + 1)) == NULL)
        return NULL;
      memcpy(userbuf, user_start, ip - user_start);
      userbuf[ip - user_start] = 0;
      dir = (pwent = getpwnam(userbuf)) != NULL ? pwent->pw_dir : NULL;
      free(userbuf);
    }
    if (dir != NULL) {
      n1 = strlen(dir);
      n2 = strlen(ip) + 1;
      if ((outpath = malloc(n1 + n2)) == NULL)
        return NULL;
      memcpy(outpath, dir, n1);
      memcpy(outpath + n1, ip, n2);
      return outpath;
    }
  }

  /* if no ~ or tilde dir can't be found, copy inpath */
  {
    size_t n = strlen(inpath) + 1;
    if ((outpath = (char *)malloc(n)) == NULL)
      return NULL;
    memcpy(outpath, inpath, n);
    return outpath;
  }
}