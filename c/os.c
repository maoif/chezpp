#define _GNU_SOURCE

#include "common.h"

#include <grp.h>
#include <pwd.h>
#include <link.h>


ptr chezpp_getpwnam(const char *name);
ptr chezpp_getpwuid(int uid);
ptr chezpp_getgrnam(const char *name);
ptr chezpp_getgrgid(int gid);

int chezpp_getuid();
int chezpp_getgid();
int chezpp_geteuid();
int chezpp_getegid();

ptr chezpp_fork();
ptr chezpp_vfork();
int chezpp_getppid();
ptr chezpp_shared_object_list();

int chezpp_get_ncores();


//=======================================================================
//
// credentials
//
//=======================================================================

static ptr _getpw(struct passwd *p) {
  if (p == NULL) {
    return errno_str();
  }

  ptr v = Smake_vector(7, Sfalse);
  Svector_set(v, 0, Sstring(p->pw_name));
  Svector_set(v, 1, Sstring(p->pw_passwd));
  Svector_set(v, 2, Sfixnum(p->pw_uid));
  Svector_set(v, 3, Sfixnum(p->pw_gid));
  Svector_set(v, 4, Sstring(p->pw_gecos));
  Svector_set(v, 5, Sstring(p->pw_dir));
  Svector_set(v, 6, Sstring(p->pw_shell));

  return v;
}

ptr chezpp_getpwnam(const char *name) {
  struct passwd *p = getpwnam(name);
  return _getpw(p);
}

ptr chezpp_getpwuid(int uid) {
  struct passwd *p = getpwuid(uid);
  return _getpw(p);
}

static ptr _getgr(struct group *p) {
  if (p == NULL) {
    return errno_str();
  }

  ptr v = Smake_vector(4, Sfalse);
  Svector_set(v, 0, Sstring(p->gr_name));
  Svector_set(v, 1, Sstring(p->gr_passwd));
  Svector_set(v, 2, Sfixnum(p->gr_gid));

  char **gmems = p->gr_mem;
  int len = 0;
  while (gmems != NULL) {
    len++;
    gmems++;
  }

  if (len == 0) {
    Svector_set(v, 3, Sfalse);

    return v;
  }

  ptr vmem = Smake_vector(len, Sfalse);

  len = 0;
  gmems = p->gr_mem;
  while (gmems != NULL) {
    Svector_set(vmem, len, Sstring(*gmems));
    len++;
    gmems++;
  }

  Svector_set(v, 3, vmem);

  return v;
}

ptr chezpp_getgrnam(const char *name) {
  struct group *p = getgrnam(name);
  return _getgr(p);
}

ptr chezpp_getgrgid(int gid) {
  struct group *p = getgrgid(gid);
  return _getgr(p);
}

int chezpp_getuid() { return getuid(); }

int chezpp_getgid() { return getgid(); }

int chezpp_geteuid() { return geteuid(); }

int chezpp_getegid() { return geteuid(); }




//=======================================================================
//
// processes
//
//=======================================================================

int chezpp_getppid() { return getppid(); }

ptr chezpp_fork() {
  int res = fork();
  if (res == -1) {
    return errno_str();
  }

  return Sfixnum(res);
}

ptr chezpp_vfork() {
  int res = vfork();
  if (res == -1) {
    return errno_str();
  }

  return Sfixnum(res);
}

static int shared_object_list_callback(struct dl_phdr_info *info, size_t size, void *data) {
  ptr *objs_addr = (ptr *)data;
  ptr objects = *objs_addr;
  *objs_addr = Scons(Sstring(info->dlpi_name), objects);

  return 0;
}

ptr chezpp_shared_object_list() {
  ptr objs = Snil;
  dl_iterate_phdr(shared_object_list_callback, &objs);
  
  return objs;
}


//=======================================================================
//
// system info
//
//=======================================================================


int chezpp_get_ncores() {
  long num_cores = sysconf(_SC_NPROCESSORS_ONLN); 
  if (num_cores == -1) {
    // just return a fallback value
    return 1;
  }

  return (int) num_cores;
}