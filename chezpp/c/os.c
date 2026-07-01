#define _GNU_SOURCE

#include "common.h"

#include <grp.h>
#include <pwd.h>
#include <link.h>
#include <poll.h>
#include <spawn.h>
#if defined(__unix__) || defined(__APPLE__)
#include <signal.h>
#endif
#include <string.h>
#if defined(__unix__) || defined(__APPLE__)
#include <sys/stat.h>
#include <sys/statvfs.h>
#endif
#include <time.h>
#include <sys/utsname.h>

extern char **environ;


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
ptr chezpp_spawn_capture(ptr argv, ptr env, const char *cwd,
                         ptr stdin_payload, int capture_stdout, int capture_stderr,
                         int stdout_null, int stderr_null, int stderr_to_stdout,
                         int timeout_ms);
ptr chezpp_spawn_process(ptr argv, ptr env, const char *cwd,
                         int stdin_null, int stdout_null, int stderr_null);
ptr chezpp_waitpid(int pid, int nohang);
ptr chezpp_make_pipe();
ptr chezpp_spawn_pipeline(ptr specs);
ptr chezpp_spawn_pipeline_capture(ptr specs, int timeout_ms);

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

typedef struct {
  char *data;
  size_t len;
  size_t cap;
} byte_buffer;

static void buffer_free(byte_buffer *buf) {
  free(buf->data);
  buf->data = NULL;
  buf->len = 0;
  buf->cap = 0;
}

static int buffer_append(byte_buffer *buf, const char *data, size_t len) {
  if (len == 0) return 1;
  if (buf->len + len > buf->cap) {
    size_t next = buf->cap == 0 ? 4096 : buf->cap;
    while (next < buf->len + len) next *= 2;
    char *new_data = (char *)realloc(buf->data, next);
    if (new_data == NULL) return 0;
    buf->data = new_data;
    buf->cap = next;
  }
  memcpy(buf->data + buf->len, data, len);
  buf->len += len;
  return 1;
}

static ptr buffer_to_string(byte_buffer *buf) {
  if (buf->len == 0) return Sstring("");
  return Sstring_utf8(buf->data, (iptr)buf->len);
}

static char *copy_scheme_string(ptr str) {
  if (!Sstringp(str)) return NULL;
  iptr n = Sstring_length(str);
  char *out = (char *)malloc((size_t)n + 1);
  if (out == NULL) return NULL;
  for (iptr i = 0; i < n; i += 1) {
    uint32_t ch = (uint32_t)Sstring_ref(str, i);
    if (ch > 255) {
      free(out);
      return NULL;
    }
    out[i] = (char)ch;
  }
  out[n] = '\0';
  return out;
}

static int list_length(ptr ls) {
  int n = 0;
  while (ls != Snil) {
    if (!Spairp(ls)) return -1;
    n += 1;
    ls = Scdr(ls);
  }
  return n;
}

static void free_string_array(char **items) {
  if (items == NULL) return;
  for (int i = 0; items[i] != NULL; i += 1) free(items[i]);
  free(items);
}

static char **argv_from_list(ptr argv) {
  int argc = list_length(argv);
  if (argc <= 0) return NULL;
  char **out = (char **)calloc((size_t)argc + 1, sizeof(char *));
  if (out == NULL) return NULL;
  ptr ls = argv;
  for (int i = 0; i < argc; i += 1) {
    out[i] = copy_scheme_string(Scar(ls));
    if (out[i] == NULL) {
      free_string_array(out);
      return NULL;
    }
    ls = Scdr(ls);
  }
  out[argc] = NULL;
  return out;
}

static char **env_from_alist(ptr env) {
  if (env == Sfalse || env == Snil) return environ;
  int count = list_length(env);
  if (count < 0) return NULL;
  char **out = (char **)calloc((size_t)count + 1, sizeof(char *));
  if (out == NULL) return NULL;
  ptr ls = env;
  for (int i = 0; i < count; i += 1) {
    ptr entry = Scar(ls);
    if (!Spairp(entry) || !Sstringp(Scar(entry)) || !Sstringp(Scdr(entry))) {
      free_string_array(out);
      return NULL;
    }
    char *key = copy_scheme_string(Scar(entry));
    char *value = copy_scheme_string(Scdr(entry));
    if (key == NULL || value == NULL) {
      free(key);
      free(value);
      free_string_array(out);
      return NULL;
    }
    size_t key_len = strlen(key);
    size_t value_len = strlen(value);
    out[i] = (char *)malloc(key_len + value_len + 2);
    if (out[i] == NULL) {
      free(key);
      free(value);
      free_string_array(out);
      return NULL;
    }
    memcpy(out[i], key, key_len);
    out[i][key_len] = '=';
    memcpy(out[i] + key_len + 1, value, value_len + 1);
    free(key);
    free(value);
    ls = Scdr(ls);
  }
  out[count] = NULL;
  return out;
}

static void close_fd(int *fd) {
  if (*fd >= 0) {
    close(*fd);
    *fd = -1;
  }
}

static int set_nonblock(int fd) {
  int flags = fcntl(fd, F_GETFL, 0);
  if (flags < 0) return -1;
  return fcntl(fd, F_SETFL, flags | O_NONBLOCK);
}

static int make_pipe(int fds[2]) {
  if (pipe(fds) != 0) return -1;
  return 0;
}

static int add_devnull_action(posix_spawn_file_actions_t *actions, int target_fd) {
  int rc = posix_spawn_file_actions_addopen(actions, target_fd, "/dev/null", O_RDWR, 0);
  return rc;
}

static int add_dup_action(posix_spawn_file_actions_t *actions, int from_fd, int to_fd) {
  int rc = posix_spawn_file_actions_adddup2(actions, from_fd, to_fd);
  if (rc != 0) return rc;
  return posix_spawn_file_actions_addclose(actions, from_fd);
}

static int add_chdir_action(posix_spawn_file_actions_t *actions, const char *cwd) {
  if (cwd == NULL || cwd[0] == '\0') return 0;
#if defined(__GLIBC__) || defined(__linux__)
  return posix_spawn_file_actions_addchdir_np(actions, cwd);
#else
  (void)actions;
  (void)cwd;
  errno = ENOTSUP;
  return ENOTSUP;
#endif
}

static int write_all_fd(int fd, const unsigned char *data, size_t len) {
  size_t done = 0;
  while (done < len) {
    ssize_t n = write(fd, data + done, len - done);
    if (n < 0) {
      if (errno == EINTR) continue;
      return -1;
    }
    done += (size_t)n;
  }
  return 0;
}

static int write_stdin_payload(int fd, ptr payload) {
  if (payload == Sfalse) return 0;
  if (!Sbytevectorp(payload)) {
    errno = EINVAL;
    return -1;
  }
  return write_all_fd(fd, Sbytevector_data(payload), (size_t)Sbytevector_length(payload));
}

static int64_t monotonic_ms(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ((int64_t)ts.tv_sec * 1000) + ((int64_t)ts.tv_nsec / 1000000);
}

static void sleep_ms(int ms) {
  struct timespec ts;
  ts.tv_sec = ms / 1000;
  ts.tv_nsec = (long)(ms % 1000) * 1000000L;
  while (nanosleep(&ts, &ts) != 0 && errno == EINTR) {
  }
}

static ptr pid_context(pid_t pid) {
  return Scons(Scons(Sstring("pid"), Sinteger((iptr)pid)), Snil);
}

static int wait_pid_block(pid_t pid, int *status) {
  for (;;) {
    pid_t r = waitpid(pid, status, 0);
    if (r == pid) return 0;
    if (r < 0 && errno == EINTR) continue;
    return -1;
  }
}

static int wait_pid_timeout(pid_t pid, int *status, int timeout_ms, int64_t start_ms) {
  for (;;) {
    pid_t r = waitpid(pid, status, WNOHANG);
    if (r == pid) return 0;
    if (r < 0) {
      if (errno == EINTR) continue;
      return -1;
    }
    if (timeout_ms >= 0 && monotonic_ms() - start_ms >= timeout_ms) {
      return 1;
    }
    sleep_ms(5);
  }
}

static ptr reap_timeout(pid_t pid, const char *operation) {
  int status;
  kill(pid, SIGTERM);
  for (int i = 0; i < 20; i += 1) {
    pid_t r = waitpid(pid, &status, WNOHANG);
    if (r == pid) return chezpp_timeout_result(operation, pid_context(pid));
    if (r < 0 && errno != EINTR) return chezpp_errno_result(operation, pid_context(pid));
    sleep_ms(5);
  }
  kill(pid, SIGKILL);
  (void)wait_pid_block(pid, &status);
  return chezpp_timeout_result(operation, pid_context(pid));
}

static ptr capture_child(pid_t pid, int stdout_fd, int stderr_fd, int timeout_ms,
                         const char *operation) {
  byte_buffer out = {0};
  byte_buffer err = {0};
  int status = 0;
  int reaped = 0;
  int64_t start = monotonic_ms();
  char tmp[4096];

  if (stdout_fd >= 0) set_nonblock(stdout_fd);
  if (stderr_fd >= 0) set_nonblock(stderr_fd);

  while (stdout_fd >= 0 || stderr_fd >= 0) {
    if (timeout_ms >= 0 && monotonic_ms() - start >= timeout_ms) {
      close_fd(&stdout_fd);
      close_fd(&stderr_fd);
      buffer_free(&out);
      buffer_free(&err);
      return reap_timeout(pid, operation);
    }

    if (!reaped) {
      pid_t r = waitpid(pid, &status, WNOHANG);
      if (r == pid) reaped = 1;
      else if (r < 0 && errno != EINTR) {
        close_fd(&stdout_fd);
        close_fd(&stderr_fd);
        buffer_free(&out);
        buffer_free(&err);
        return chezpp_errno_result(operation, pid_context(pid));
      }
    }

    struct pollfd pfds[2];
    int nfds = 0;
    int out_index = -1;
    int err_index = -1;
    if (stdout_fd >= 0) {
      out_index = nfds;
      pfds[nfds].fd = stdout_fd;
      pfds[nfds].events = POLLIN | POLLHUP;
      pfds[nfds].revents = 0;
      nfds += 1;
    }
    if (stderr_fd >= 0) {
      err_index = nfds;
      pfds[nfds].fd = stderr_fd;
      pfds[nfds].events = POLLIN | POLLHUP;
      pfds[nfds].revents = 0;
      nfds += 1;
    }

    int poll_timeout = 50;
    if (timeout_ms >= 0) {
      int64_t elapsed = monotonic_ms() - start;
      int64_t remaining = timeout_ms - elapsed;
      if (remaining < poll_timeout) poll_timeout = remaining < 0 ? 0 : (int)remaining;
    }

    int pr = poll(pfds, (nfds_t)nfds, poll_timeout);
    if (pr < 0) {
      if (errno == EINTR) continue;
      close_fd(&stdout_fd);
      close_fd(&stderr_fd);
      buffer_free(&out);
      buffer_free(&err);
      return chezpp_errno_result(operation, pid_context(pid));
    }

    if (out_index >= 0 && (pfds[out_index].revents & (POLLIN | POLLHUP))) {
      for (;;) {
        ssize_t n = read(stdout_fd, tmp, sizeof(tmp));
        if (n > 0) {
          if (!buffer_append(&out, tmp, (size_t)n)) {
            errno = ENOMEM;
            close_fd(&stdout_fd);
            close_fd(&stderr_fd);
            buffer_free(&out);
            buffer_free(&err);
            return chezpp_errno_result(operation, pid_context(pid));
          }
        } else if (n == 0) {
          close_fd(&stdout_fd);
          break;
        } else if (errno == EINTR) {
          continue;
        } else if (errno == EAGAIN || errno == EWOULDBLOCK) {
          break;
        } else {
          close_fd(&stdout_fd);
          close_fd(&stderr_fd);
          buffer_free(&out);
          buffer_free(&err);
          return chezpp_errno_result(operation, pid_context(pid));
        }
      }
    }

    if (err_index >= 0 && (pfds[err_index].revents & (POLLIN | POLLHUP))) {
      for (;;) {
        ssize_t n = read(stderr_fd, tmp, sizeof(tmp));
        if (n > 0) {
          if (!buffer_append(&err, tmp, (size_t)n)) {
            errno = ENOMEM;
            close_fd(&stdout_fd);
            close_fd(&stderr_fd);
            buffer_free(&out);
            buffer_free(&err);
            return chezpp_errno_result(operation, pid_context(pid));
          }
        } else if (n == 0) {
          close_fd(&stderr_fd);
          break;
        } else if (errno == EINTR) {
          continue;
        } else if (errno == EAGAIN || errno == EWOULDBLOCK) {
          break;
        } else {
          close_fd(&stdout_fd);
          close_fd(&stderr_fd);
          buffer_free(&out);
          buffer_free(&err);
          return chezpp_errno_result(operation, pid_context(pid));
        }
      }
    }
  }

  if (!reaped) {
    int wr = timeout_ms >= 0
      ? wait_pid_timeout(pid, &status, timeout_ms, start)
      : wait_pid_block(pid, &status);
    if (wr == 1) {
      buffer_free(&out);
      buffer_free(&err);
      return reap_timeout(pid, operation);
    }
    if (wr < 0) {
      buffer_free(&out);
      buffer_free(&err);
      return chezpp_errno_result(operation, pid_context(pid));
    }
  }

  ptr v = Smake_vector(4, Sfalse);
  Svector_set(v, 0, Sinteger((iptr)pid));
  Svector_set(v, 1, Sinteger(status));
  Svector_set(v, 2, buffer_to_string(&out));
  Svector_set(v, 3, buffer_to_string(&err));
  buffer_free(&out);
  buffer_free(&err);
  return chezpp_ok(v);
}

ptr chezpp_spawn_capture(ptr argv, ptr env, const char *cwd,
                         ptr stdin_payload, int capture_stdout, int capture_stderr,
                         int stdout_null, int stderr_null, int stderr_to_stdout,
                         int timeout_ms) {
#if defined(__unix__) || defined(__APPLE__)
  char **cargv = argv_from_list(argv);
  char **cenv = env_from_alist(env);
  if (cargv == NULL || cenv == NULL) {
    free_string_array(cargv);
    if (cenv != environ) free_string_array(cenv);
    errno = EINVAL;
    return chezpp_errno_result("spawn-capture", Snil);
  }

  int stdin_pipe[2] = {-1, -1};
  int stdout_pipe[2] = {-1, -1};
  int stderr_pipe[2] = {-1, -1};
  posix_spawn_file_actions_t actions;
  int rc = posix_spawn_file_actions_init(&actions);
  if (rc != 0) {
    errno = rc;
    free_string_array(cargv);
    if (cenv != environ) free_string_array(cenv);
    return chezpp_errno_result("spawn-capture", Snil);
  }

  rc = add_chdir_action(&actions, cwd);
  if (rc == 0 && stdin_payload != Sfalse) {
    if (make_pipe(stdin_pipe) != 0) rc = errno;
    else {
      rc = add_dup_action(&actions, stdin_pipe[0], STDIN_FILENO);
      if (rc == 0) rc = posix_spawn_file_actions_addclose(&actions, stdin_pipe[1]);
    }
  } else if (rc == 0) {
    rc = add_devnull_action(&actions, STDIN_FILENO);
  }

  if (rc == 0 && capture_stdout) {
    if (make_pipe(stdout_pipe) != 0) rc = errno;
    else {
      rc = add_dup_action(&actions, stdout_pipe[1], STDOUT_FILENO);
      if (rc == 0) rc = posix_spawn_file_actions_addclose(&actions, stdout_pipe[0]);
    }
  } else if (rc == 0 && stdout_null) {
    rc = add_devnull_action(&actions, STDOUT_FILENO);
  }

  if (rc == 0 && stderr_to_stdout) {
    rc = posix_spawn_file_actions_adddup2(&actions, STDOUT_FILENO, STDERR_FILENO);
  } else if (rc == 0 && capture_stderr) {
    if (make_pipe(stderr_pipe) != 0) rc = errno;
    else {
      rc = add_dup_action(&actions, stderr_pipe[1], STDERR_FILENO);
      if (rc == 0) rc = posix_spawn_file_actions_addclose(&actions, stderr_pipe[0]);
    }
  } else if (rc == 0 && stderr_null) {
    rc = add_devnull_action(&actions, STDERR_FILENO);
  }

  if (rc != 0) {
    errno = rc;
    posix_spawn_file_actions_destroy(&actions);
    close_fd(&stdin_pipe[0]);
    close_fd(&stdin_pipe[1]);
    close_fd(&stdout_pipe[0]);
    close_fd(&stdout_pipe[1]);
    close_fd(&stderr_pipe[0]);
    close_fd(&stderr_pipe[1]);
    free_string_array(cargv);
    if (cenv != environ) free_string_array(cenv);
    return chezpp_errno_result("spawn-capture", Snil);
  }

  pid_t pid;
  rc = posix_spawnp(&pid, cargv[0], &actions, NULL, cargv, cenv);
  posix_spawn_file_actions_destroy(&actions);
  close_fd(&stdin_pipe[0]);
  close_fd(&stdout_pipe[1]);
  close_fd(&stderr_pipe[1]);
  free_string_array(cargv);
  if (cenv != environ) free_string_array(cenv);

  if (rc != 0) {
    errno = rc;
    close_fd(&stdin_pipe[1]);
    close_fd(&stdout_pipe[0]);
    close_fd(&stderr_pipe[0]);
    return chezpp_errno_result("spawn-capture", Snil);
  }

  if (stdin_pipe[1] >= 0) {
    if (write_stdin_payload(stdin_pipe[1], stdin_payload) != 0) {
      close_fd(&stdin_pipe[1]);
      close_fd(&stdout_pipe[0]);
      close_fd(&stderr_pipe[0]);
      return reap_timeout(pid, "spawn-capture");
    }
    close_fd(&stdin_pipe[1]);
  }

  return capture_child(pid,
                       capture_stdout ? stdout_pipe[0] : -1,
                       capture_stderr ? stderr_pipe[0] : -1,
                       timeout_ms,
                       "spawn-capture");
#else
  (void)argv; (void)env; (void)cwd; (void)stdin_payload;
  (void)capture_stdout; (void)capture_stderr; (void)stdout_null;
  (void)stderr_null; (void)stderr_to_stdout; (void)timeout_ms;
  return chezpp_unsupported_result("spawn-capture");
#endif
}

ptr chezpp_spawn_process(ptr argv, ptr env, const char *cwd,
                         int stdin_null, int stdout_null, int stderr_null) {
#if defined(__unix__) || defined(__APPLE__)
  char **cargv = argv_from_list(argv);
  char **cenv = env_from_alist(env);
  if (cargv == NULL || cenv == NULL) {
    free_string_array(cargv);
    if (cenv != environ) free_string_array(cenv);
    errno = EINVAL;
    return chezpp_errno_result("spawn-process", Snil);
  }

  posix_spawn_file_actions_t actions;
  int rc = posix_spawn_file_actions_init(&actions);
  if (rc == 0) rc = add_chdir_action(&actions, cwd);
  if (rc == 0 && stdin_null) rc = add_devnull_action(&actions, STDIN_FILENO);
  if (rc == 0 && stdout_null) rc = add_devnull_action(&actions, STDOUT_FILENO);
  if (rc == 0 && stderr_null) rc = add_devnull_action(&actions, STDERR_FILENO);
  if (rc != 0) {
    errno = rc;
    posix_spawn_file_actions_destroy(&actions);
    free_string_array(cargv);
    if (cenv != environ) free_string_array(cenv);
    return chezpp_errno_result("spawn-process", Snil);
  }

  pid_t pid;
  rc = posix_spawnp(&pid, cargv[0], &actions, NULL, cargv, cenv);
  posix_spawn_file_actions_destroy(&actions);
  free_string_array(cargv);
  if (cenv != environ) free_string_array(cenv);
  if (rc != 0) {
    errno = rc;
    return chezpp_errno_result("spawn-process", Snil);
  }

  ptr v = Smake_vector(1, Sfalse);
  Svector_set(v, 0, Sinteger((iptr)pid));
  return chezpp_ok(v);
#else
  (void)argv; (void)env; (void)cwd; (void)stdin_null; (void)stdout_null; (void)stderr_null;
  return chezpp_unsupported_result("spawn-process");
#endif
}

ptr chezpp_waitpid(int pid, int nohang) {
#if defined(__unix__) || defined(__APPLE__)
  int status;
  pid_t r;
  do {
    r = waitpid((pid_t)pid, &status, nohang ? WNOHANG : 0);
  } while (r < 0 && errno == EINTR);
  if (r == 0) return chezpp_ok(Sfalse);
  if (r < 0) return chezpp_errno_result("waitpid", pid_context((pid_t)pid));
  ptr v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, Sinteger((iptr)r));
  Svector_set(v, 1, Sinteger(status));
  return chezpp_ok(v);
#else
  (void)pid; (void)nohang;
  return chezpp_unsupported_result("waitpid");
#endif
}

ptr chezpp_make_pipe() {
#if defined(__unix__) || defined(__APPLE__)
  int fds[2];
  if (make_pipe(fds) != 0) {
    return chezpp_errno_result("make-pipe", Snil);
  }
  ptr v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, Sinteger(fds[0]));
  Svector_set(v, 1, Sinteger(fds[1]));
  return chezpp_ok(v);
#else
  return chezpp_unsupported_result("make-pipe");
#endif
}

ptr chezpp_spawn_pipeline(ptr specs) {
#if defined(__unix__) || defined(__APPLE__)
  int count = list_length(specs);
  if (count <= 0) {
    errno = EINVAL;
    return chezpp_errno_result("spawn-pipeline", Snil);
  }

  char ***argvs = (char ***)calloc((size_t)count, sizeof(char **));
  pid_t *pids = (pid_t *)calloc((size_t)count, sizeof(pid_t));
  int (*pipes)[2] = NULL;
  if (argvs == NULL || pids == NULL) {
    free(argvs);
    free(pids);
    errno = ENOMEM;
    return chezpp_errno_result("spawn-pipeline", Snil);
  }
  if (count > 1) {
    pipes = (int (*)[2])calloc((size_t)(count - 1), sizeof(int[2]));
    if (pipes == NULL) {
      free(argvs);
      free(pids);
      errno = ENOMEM;
      return chezpp_errno_result("spawn-pipeline", Snil);
    }
    for (int i = 0; i < count - 1; i += 1) {
      pipes[i][0] = -1;
      pipes[i][1] = -1;
      if (make_pipe(pipes[i]) != 0) {
        for (int j = 0; j <= i; j += 1) {
          close_fd(&pipes[j][0]);
          close_fd(&pipes[j][1]);
        }
        free(argvs);
        free(pids);
        free(pipes);
        return chezpp_errno_result("spawn-pipeline", Snil);
      }
    }
  }

  ptr ls = specs;
  for (int i = 0; i < count; i += 1) {
    argvs[i] = argv_from_list(Scar(ls));
    if (argvs[i] == NULL) {
      for (int j = 0; j < count; j += 1) free_string_array(argvs[j]);
      if (pipes != NULL) {
        for (int j = 0; j < count - 1; j += 1) {
          close_fd(&pipes[j][0]);
          close_fd(&pipes[j][1]);
        }
      }
      free(argvs);
      free(pids);
      free(pipes);
      errno = EINVAL;
      return chezpp_errno_result("spawn-pipeline", Snil);
    }
    ls = Scdr(ls);
  }

  int spawned = 0;
  for (int i = 0; i < count; i += 1) {
    posix_spawn_file_actions_t actions;
    int rc = posix_spawn_file_actions_init(&actions);
    if (rc == 0 && i > 0) {
      rc = posix_spawn_file_actions_adddup2(&actions, pipes[i - 1][0], STDIN_FILENO);
    }
    if (rc == 0 && i < count - 1) {
      rc = posix_spawn_file_actions_adddup2(&actions, pipes[i][1], STDOUT_FILENO);
    }
    if (rc == 0) {
      for (int j = 0; j < count - 1; j += 1) {
        posix_spawn_file_actions_addclose(&actions, pipes[j][0]);
        posix_spawn_file_actions_addclose(&actions, pipes[j][1]);
      }
    }
    if (rc == 0) rc = posix_spawnp(&pids[i], argvs[i][0], &actions, NULL, argvs[i], environ);
    posix_spawn_file_actions_destroy(&actions);
    if (rc != 0) {
      errno = rc;
      for (int j = 0; j < spawned; j += 1) kill(pids[j], SIGKILL);
      for (int j = 0; j < spawned; j += 1) {
        int status;
        (void)wait_pid_block(pids[j], &status);
      }
      for (int j = 0; j < count; j += 1) free_string_array(argvs[j]);
      if (pipes != NULL) {
        for (int j = 0; j < count - 1; j += 1) {
          close_fd(&pipes[j][0]);
          close_fd(&pipes[j][1]);
        }
      }
      free(argvs);
      free(pids);
      free(pipes);
      return chezpp_errno_result("spawn-pipeline", Snil);
    }
    spawned += 1;
  }

  if (pipes != NULL) {
    for (int j = 0; j < count - 1; j += 1) {
      close_fd(&pipes[j][0]);
      close_fd(&pipes[j][1]);
    }
  }

  ptr v = Smake_vector(count, Sfalse);
  for (int i = 0; i < count; i += 1) {
    Svector_set(v, i, Sinteger((iptr)pids[i]));
  }
  for (int j = 0; j < count; j += 1) free_string_array(argvs[j]);
  free(argvs);
  free(pids);
  free(pipes);
  return chezpp_ok(v);
#else
  (void)specs;
  return chezpp_unsupported_result("spawn-pipeline");
#endif
}

ptr chezpp_spawn_pipeline_capture(ptr specs, int timeout_ms) {
#if defined(__unix__) || defined(__APPLE__)
  int count = list_length(specs);
  if (count <= 0) {
    errno = EINVAL;
    return chezpp_errno_result("spawn-pipeline", Snil);
  }

  char ***argvs = (char ***)calloc((size_t)count, sizeof(char **));
  pid_t *pids = (pid_t *)calloc((size_t)count, sizeof(pid_t));
  int (*pipes)[2] = NULL;
  if (argvs == NULL || pids == NULL) {
    free(argvs);
    free(pids);
    errno = ENOMEM;
    return chezpp_errno_result("spawn-pipeline", Snil);
  }
  if (count > 1) {
    pipes = (int (*)[2])calloc((size_t)(count - 1), sizeof(int[2]));
    if (pipes == NULL) {
      free(argvs);
      free(pids);
      errno = ENOMEM;
      return chezpp_errno_result("spawn-pipeline", Snil);
    }
    for (int i = 0; i < count - 1; i += 1) {
      pipes[i][0] = -1;
      pipes[i][1] = -1;
      if (make_pipe(pipes[i]) != 0) {
        for (int j = 0; j <= i; j += 1) {
          close_fd(&pipes[j][0]);
          close_fd(&pipes[j][1]);
        }
        free(argvs);
        free(pids);
        free(pipes);
        return chezpp_errno_result("spawn-pipeline", Snil);
      }
    }
  }

  ptr ls = specs;
  for (int i = 0; i < count; i += 1) {
    argvs[i] = argv_from_list(Scar(ls));
    if (argvs[i] == NULL) {
      for (int j = 0; j < count; j += 1) free_string_array(argvs[j]);
      if (pipes != NULL) {
        for (int j = 0; j < count - 1; j += 1) {
          close_fd(&pipes[j][0]);
          close_fd(&pipes[j][1]);
        }
      }
      free(argvs);
      free(pids);
      free(pipes);
      errno = EINVAL;
      return chezpp_errno_result("spawn-pipeline", Snil);
    }
    ls = Scdr(ls);
  }

  int capture_pipe[2] = {-1, -1};
  if (make_pipe(capture_pipe) != 0) {
    for (int j = 0; j < count; j += 1) free_string_array(argvs[j]);
    if (pipes != NULL) {
      for (int j = 0; j < count - 1; j += 1) {
        close_fd(&pipes[j][0]);
        close_fd(&pipes[j][1]);
      }
    }
    free(argvs);
    free(pids);
    free(pipes);
    return chezpp_errno_result("spawn-pipeline", Snil);
  }

  int spawned = 0;
  for (int i = 0; i < count; i += 1) {
    posix_spawn_file_actions_t actions;
    int rc = posix_spawn_file_actions_init(&actions);
    if (rc == 0 && i > 0) rc = posix_spawn_file_actions_adddup2(&actions, pipes[i - 1][0], STDIN_FILENO);
    if (rc == 0 && i < count - 1) rc = posix_spawn_file_actions_adddup2(&actions, pipes[i][1], STDOUT_FILENO);
    if (rc == 0 && i == count - 1) rc = posix_spawn_file_actions_adddup2(&actions, capture_pipe[1], STDOUT_FILENO);
    if (rc == 0) {
      for (int j = 0; j < count - 1; j += 1) {
        posix_spawn_file_actions_addclose(&actions, pipes[j][0]);
        posix_spawn_file_actions_addclose(&actions, pipes[j][1]);
      }
      posix_spawn_file_actions_addclose(&actions, capture_pipe[0]);
      posix_spawn_file_actions_addclose(&actions, capture_pipe[1]);
    }
    if (rc == 0) rc = posix_spawnp(&pids[i], argvs[i][0], &actions, NULL, argvs[i], environ);
    posix_spawn_file_actions_destroy(&actions);
    if (rc != 0) {
      errno = rc;
      for (int j = 0; j < spawned; j += 1) kill(pids[j], SIGKILL);
      for (int j = 0; j < spawned; j += 1) {
        int status;
        (void)wait_pid_block(pids[j], &status);
      }
      for (int j = 0; j < count; j += 1) free_string_array(argvs[j]);
      for (int j = 0; j < count - 1; j += 1) {
        close_fd(&pipes[j][0]);
        close_fd(&pipes[j][1]);
      }
      close_fd(&capture_pipe[0]);
      close_fd(&capture_pipe[1]);
      free(argvs);
      free(pids);
      free(pipes);
      return chezpp_errno_result("spawn-pipeline", Snil);
    }
    spawned += 1;
  }

  for (int j = 0; j < count - 1; j += 1) {
    close_fd(&pipes[j][0]);
    close_fd(&pipes[j][1]);
  }
  close_fd(&capture_pipe[1]);

  ptr result = capture_child(pids[count - 1], capture_pipe[0], -1, timeout_ms, "spawn-pipeline");
  for (int i = 0; i < count - 1; i += 1) {
    int status;
    (void)wait_pid_block(pids[i], &status);
  }
  for (int j = 0; j < count; j += 1) free_string_array(argvs[j]);
  free(argvs);
  free(pids);
  free(pipes);
  return result;
#else
  (void)specs; (void)timeout_ms;
  return chezpp_unsupported_result("spawn-pipeline");
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
