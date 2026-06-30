#define _GNU_SOURCE

#include "common.h"

#include <grp.h>
#include <pwd.h>
#include <link.h>
#if defined(__unix__) || defined(__APPLE__)
#include <signal.h>
#endif
#include <string.h>
#if defined(__unix__) || defined(__APPLE__)
#include <sys/stat.h>
#include <sys/statvfs.h>
#endif
#include <sys/utsname.h>



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
ptr chezpp_send_signal(int pid, int sig);

ptr chezpp_hostname();
ptr chezpp_cpu_arch();
int chezpp_cpu_count();
ptr chezpp_filesystem_info(const char *path);


//=======================================================================
//
// credentials
//
//=======================================================================

static ptr _getpw(const char *operation, ptr context, struct passwd *p) {
  if (p == NULL) {
    if (errno == 0) {
      return chezpp_not_found_result(operation, context);
    }
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
  errno = 0;
  struct passwd *p = getpwnam(name);
  ptr context = Scons(Scons(Sstring("name"), Sstring(name)), Snil);
  if (p == NULL) {
    return chezpp_not_found_result("getpwnam", context);
  }
  return _getpw("getpwnam", context, p);
}

ptr chezpp_getpwuid(int uid) {
  errno = 0;
  struct passwd *p = getpwuid(uid);
  ptr context = Scons(Scons(Sstring("uid"), Sfixnum(uid)), Snil);
  return _getpw("getpwuid", context, p);
}

static ptr _getgr(const char *operation, ptr context, struct group *p) {
  if (p == NULL) {
    if (errno == 0) {
      return chezpp_not_found_result(operation, context);
    }
    return errno_str();
  }

  ptr v = Smake_vector(4, Sfalse);
  Svector_set(v, 0, Sstring(p->gr_name));
  Svector_set(v, 1, Sstring(p->gr_passwd));
  Svector_set(v, 2, Sfixnum(p->gr_gid));

  char **gmems = p->gr_mem;
  int len = 0;
  while (gmems != NULL && *gmems != NULL) {
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
  while (gmems != NULL && *gmems != NULL) {
    Svector_set(vmem, len, Sstring(*gmems));
    len++;
    gmems++;
  }

  Svector_set(v, 3, vmem);

  return v;
}

ptr chezpp_getgrnam(const char *name) {
  errno = 0;
  struct group *p = getgrnam(name);
  ptr context = Scons(Scons(Sstring("name"), Sstring(name)), Snil);
  if (p == NULL) {
    return chezpp_not_found_result("getgrnam", context);
  }
  return _getgr("getgrnam", context, p);
}

ptr chezpp_getgrgid(int gid) {
  errno = 0;
  struct group *p = getgrgid(gid);
  ptr context = Scons(Scons(Sstring("gid"), Sfixnum(gid)), Snil);
  return _getgr("getgrgid", context, p);
}

int chezpp_getuid() { return getuid(); }

int chezpp_getgid() { return getgid(); }

int chezpp_geteuid() { return geteuid(); }

int chezpp_getegid() { return getegid(); }




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
  (void)size;
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

ptr chezpp_send_signal(int pid, int sig) {
#if defined(__unix__) || defined(__APPLE__)
  if (kill(pid, sig) != 0) {
    return chezpp_errno_result("send-signal", Snil);
  }

  return chezpp_ok(Strue);
#else
  (void)pid;
  (void)sig;
  return chezpp_unsupported_result("send-signal");
#endif
}


//=======================================================================
//
// system info
//
//=======================================================================


ptr chezpp_hostname() {
  struct utsname sysinfo;
  if (uname(&sysinfo) == 0) {
    return Sstring(sysinfo.nodename);
  }
  return errno_str_vector();
}


ptr chezpp_cpu_arch() {
  struct utsname sysinfo;
  if (uname(&sysinfo) == 0) {
    return Sstring(sysinfo.machine);
  }
  return errno_str_vector();
}


int chezpp_cpu_count() {
  long num_cores = sysconf(_SC_NPROCESSORS_ONLN); 
  if (num_cores == -1) {
    // just return a fallback value
    return 1;
  }

  return (int) num_cores;
}


//=======================================================================
//
// filesystem info
//
//=======================================================================


ptr chezpp_filesystem_info(const char *path) {
#if defined(__unix__) || defined(__APPLE__)
  struct statvfs vfs;
  if (statvfs(path, &vfs) != 0) {
    return chezpp_errno_result("filesystem-info", Snil);
  }

  struct stat st;
  if (stat(path, &st) != 0) {
    return chezpp_errno_result("filesystem-info", Snil);
  }

  ptr v = Smake_vector(11, Sfalse);
  Svector_set(v, 0, Sstring(path));
  Svector_set(v, 1, Sunsigned64((Suint64_t)st.st_dev));
  Svector_set(v, 2, Sunsigned64((Suint64_t)st.st_ino));
  Svector_set(v, 3, Sfalse);
  Svector_set(v, 4, Sunsigned64((Suint64_t)vfs.f_frsize));
  Svector_set(v, 5, Sunsigned64((Suint64_t)vfs.f_blocks));
  Svector_set(v, 6, Sunsigned64((Suint64_t)vfs.f_bfree));
  Svector_set(v, 7, Sunsigned64((Suint64_t)vfs.f_bavail));
  Svector_set(v, 8, Sunsigned64((Suint64_t)vfs.f_files));
  Svector_set(v, 9, Sunsigned64((Suint64_t)vfs.f_ffree));
#ifdef ST_RDONLY
  Svector_set(v, 10, (vfs.f_flag & ST_RDONLY) ? Strue : Sfalse);
#else
  Svector_set(v, 10, Sfalse);
#endif

  return chezpp_ok(v);
#else
  (void)path;
  return chezpp_unsupported_result("filesystem-info");
#endif
}
