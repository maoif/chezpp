#include "../common.h"

#include <dlfcn.h>
#include <fcntl.h>
#include <libssh/libssh.h>
#include <libssh/sftp.h>

typedef struct {
  ssh_session session;
} chezpp_ssh_session;

typedef struct {
  ssh_channel channel;
  chezpp_ssh_session *owner;
} chezpp_ssh_channel;

typedef struct {
  sftp_session sftp;
  chezpp_ssh_session *owner;
} chezpp_sftp_session;

typedef struct {
  sftp_file file;
  chezpp_sftp_session *owner;
  sftp_aio pending_read;
  size_t pending_read_len;
  sftp_aio pending_write;
  size_t pending_write_len;
} chezpp_sftp_file;

typedef ssh_session (*ssh_new_fn)(void);
typedef void (*ssh_free_fn)(ssh_session);
typedef int (*ssh_options_set_fn)(ssh_session, enum ssh_options_e, const void *);
typedef int (*ssh_options_parse_config_fn)(ssh_session, const char *);
typedef int (*ssh_connect_fn)(ssh_session);
typedef void (*ssh_disconnect_fn)(ssh_session);
typedef const char *(*ssh_get_error_fn)(void *);
typedef int (*ssh_userauth_password_fn)(ssh_session, const char *, const char *);
typedef int (*ssh_userauth_publickey_auto_fn)(ssh_session, const char *, const char *);
typedef int (*ssh_userauth_agent_fn)(ssh_session, const char *);
typedef ssh_channel (*ssh_channel_new_fn)(ssh_session);
typedef void (*ssh_channel_free_fn)(ssh_channel);
typedef ssh_session (*ssh_channel_get_session_fn)(ssh_channel);
typedef int (*ssh_channel_open_session_fn)(ssh_channel);
typedef int (*ssh_channel_request_exec_fn)(ssh_channel, const char *);
typedef int (*ssh_channel_request_shell_fn)(ssh_channel);
typedef int (*ssh_channel_request_pty_fn)(ssh_channel);
typedef int (*ssh_channel_read_fn)(ssh_channel, void *, uint32_t, int);
typedef int (*ssh_channel_write_fn)(ssh_channel, const void *, uint32_t);
typedef int (*ssh_channel_send_eof_fn)(ssh_channel);
typedef int (*ssh_channel_close_fn)(ssh_channel);
typedef int (*ssh_channel_get_exit_status_fn)(ssh_channel);
typedef int (*ssh_channel_is_eof_fn)(ssh_channel);
typedef socket_t (*ssh_get_fd_fn)(ssh_session);
typedef void (*ssh_set_blocking_fn)(ssh_session, int);
typedef sftp_session (*sftp_new_fn)(ssh_session);
typedef int (*sftp_init_fn)(sftp_session);
typedef void (*sftp_free_fn)(sftp_session);
typedef int (*sftp_get_error_fn)(sftp_session);
typedef sftp_dir (*sftp_opendir_fn)(sftp_session, const char *);
typedef sftp_attributes (*sftp_readdir_fn)(sftp_session, sftp_dir);
typedef int (*sftp_dir_eof_fn)(sftp_dir);
typedef int (*sftp_closedir_fn)(sftp_dir);
typedef sftp_attributes (*sftp_stat_fn)(sftp_session, const char *);
typedef void (*sftp_attributes_free_fn)(sftp_attributes);
typedef sftp_file (*sftp_open_fn)(sftp_session, const char *, int, mode_t);
typedef int (*sftp_close_fn)(sftp_file);
typedef ssize_t (*sftp_read_fn)(sftp_file, void *, size_t);
typedef ssize_t (*sftp_write_fn)(sftp_file, const void *, size_t);
typedef int (*sftp_unlink_fn)(sftp_session, const char *);
typedef int (*sftp_mkdir_fn)(sftp_session, const char *, mode_t);
typedef int (*sftp_rmdir_fn)(sftp_session, const char *);
typedef int (*sftp_rename_fn)(sftp_session, const char *, const char *);
typedef void (*sftp_file_set_nonblocking_fn)(sftp_file);
typedef void (*sftp_file_set_blocking_fn)(sftp_file);
typedef void (*sftp_aio_free_fn)(sftp_aio);
typedef ssize_t (*sftp_aio_begin_read_fn)(sftp_file, size_t, sftp_aio *);
typedef ssize_t (*sftp_aio_wait_read_fn)(sftp_aio *, void *, size_t);
typedef ssize_t (*sftp_aio_begin_write_fn)(sftp_file, const void *, size_t, sftp_aio *);
typedef ssize_t (*sftp_aio_wait_write_fn)(sftp_aio *);

static void *ssh_handle = NULL;
static ssh_new_fn p_ssh_new = NULL;
static ssh_free_fn p_ssh_free = NULL;
static ssh_options_set_fn p_ssh_options_set = NULL;
static ssh_options_parse_config_fn p_ssh_options_parse_config = NULL;
static ssh_connect_fn p_ssh_connect = NULL;
static ssh_disconnect_fn p_ssh_disconnect = NULL;
static ssh_get_error_fn p_ssh_get_error = NULL;
static ssh_userauth_password_fn p_ssh_userauth_password = NULL;
static ssh_userauth_publickey_auto_fn p_ssh_userauth_publickey_auto = NULL;
static ssh_userauth_agent_fn p_ssh_userauth_agent = NULL;
static ssh_channel_new_fn p_ssh_channel_new = NULL;
static ssh_channel_free_fn p_ssh_channel_free = NULL;
static ssh_channel_get_session_fn p_ssh_channel_get_session = NULL;
static ssh_channel_open_session_fn p_ssh_channel_open_session = NULL;
static ssh_channel_request_exec_fn p_ssh_channel_request_exec = NULL;
static ssh_channel_request_shell_fn p_ssh_channel_request_shell = NULL;
static ssh_channel_request_pty_fn p_ssh_channel_request_pty = NULL;
static ssh_channel_read_fn p_ssh_channel_read = NULL;
static ssh_channel_write_fn p_ssh_channel_write = NULL;
static ssh_channel_send_eof_fn p_ssh_channel_send_eof = NULL;
static ssh_channel_close_fn p_ssh_channel_close = NULL;
static ssh_channel_get_exit_status_fn p_ssh_channel_get_exit_status = NULL;
static ssh_channel_is_eof_fn p_ssh_channel_is_eof = NULL;
static ssh_get_fd_fn p_ssh_get_fd = NULL;
static ssh_set_blocking_fn p_ssh_set_blocking = NULL;
static sftp_new_fn p_sftp_new = NULL;
static sftp_init_fn p_sftp_init = NULL;
static sftp_free_fn p_sftp_free = NULL;
static sftp_get_error_fn p_sftp_get_error = NULL;
static sftp_opendir_fn p_sftp_opendir = NULL;
static sftp_readdir_fn p_sftp_readdir = NULL;
static sftp_dir_eof_fn p_sftp_dir_eof = NULL;
static sftp_closedir_fn p_sftp_closedir = NULL;
static sftp_stat_fn p_sftp_stat = NULL;
static sftp_attributes_free_fn p_sftp_attributes_free = NULL;
static sftp_open_fn p_sftp_open = NULL;
static sftp_close_fn p_sftp_close = NULL;
static sftp_read_fn p_sftp_read = NULL;
static sftp_write_fn p_sftp_write = NULL;
static sftp_unlink_fn p_sftp_unlink = NULL;
static sftp_mkdir_fn p_sftp_mkdir = NULL;
static sftp_rmdir_fn p_sftp_rmdir = NULL;
static sftp_rename_fn p_sftp_rename = NULL;
static sftp_file_set_nonblocking_fn p_sftp_file_set_nonblocking = NULL;
static sftp_file_set_blocking_fn p_sftp_file_set_blocking = NULL;
static sftp_aio_free_fn p_sftp_aio_free = NULL;
static sftp_aio_begin_read_fn p_sftp_aio_begin_read = NULL;
static sftp_aio_wait_read_fn p_sftp_aio_wait_read = NULL;
static sftp_aio_begin_write_fn p_sftp_aio_begin_write = NULL;
static sftp_aio_wait_write_fn p_sftp_aio_wait_write = NULL;

static ptr make_status(const char *tag, ptr value) {
  ptr v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, Sstring_to_symbol(tag));
  Svector_set(v, 1, value);
  return v;
}

static ptr make_error_status_message(const char *msg) {
  return make_status("error", msg == NULL ? Sstring("SSH error") : Sstring(msg));
}

static ptr make_errno_status(void) { return make_status("error", errno_str()); }

static int add_default_identities(ssh_session session) {
  static const char *names[] = {"id_ed25519", "id_ecdsa", "id_rsa", "id_dsa", NULL};
  const char *home = getenv("HOME");
  char path[PATH_MAX];
  int i;

  if (home == NULL || *home == 0) return 1;

  for (i = 0; names[i] != NULL; ++i) {
    int n = snprintf(path, sizeof(path), "%s/.ssh/%s", home, names[i]);
    if (n <= 0 || (size_t)n >= sizeof(path)) continue;
    if (access(path, R_OK) != 0) continue;
    if (p_ssh_options_set(session, SSH_OPTIONS_ADD_IDENTITY, path) != SSH_OK) return 0;
  }

  return 1;
}

static int load_symbol(void **out, const char *name) {
  *out = dlsym(ssh_handle, name);
  return *out != NULL;
}

static int ensure_ssh_loaded(void) {
  const char *names[] = {"libssh.so.4", "libssh.so", NULL};
  int i;

  if (ssh_handle != NULL) return 1;

  for (i = 0; names[i] != NULL; ++i) {
    ssh_handle = dlopen(names[i], RTLD_NOW | RTLD_LOCAL);
    if (ssh_handle != NULL) break;
  }
  if (ssh_handle == NULL) return 0;

  if (!load_symbol((void **)&p_ssh_new, "ssh_new") ||
      !load_symbol((void **)&p_ssh_free, "ssh_free") ||
      !load_symbol((void **)&p_ssh_options_set, "ssh_options_set") ||
      !load_symbol((void **)&p_ssh_options_parse_config, "ssh_options_parse_config") ||
      !load_symbol((void **)&p_ssh_connect, "ssh_connect") ||
      !load_symbol((void **)&p_ssh_disconnect, "ssh_disconnect") ||
      !load_symbol((void **)&p_ssh_get_error, "ssh_get_error") ||
      !load_symbol((void **)&p_ssh_userauth_password, "ssh_userauth_password") ||
      !load_symbol((void **)&p_ssh_userauth_publickey_auto, "ssh_userauth_publickey_auto") ||
      !load_symbol((void **)&p_ssh_userauth_agent, "ssh_userauth_agent") ||
      !load_symbol((void **)&p_ssh_channel_new, "ssh_channel_new") ||
      !load_symbol((void **)&p_ssh_channel_free, "ssh_channel_free") ||
      !load_symbol((void **)&p_ssh_channel_get_session, "ssh_channel_get_session") ||
      !load_symbol((void **)&p_ssh_channel_open_session, "ssh_channel_open_session") ||
      !load_symbol((void **)&p_ssh_channel_request_exec, "ssh_channel_request_exec") ||
      !load_symbol((void **)&p_ssh_channel_request_shell, "ssh_channel_request_shell") ||
      !load_symbol((void **)&p_ssh_channel_request_pty, "ssh_channel_request_pty") ||
      !load_symbol((void **)&p_ssh_channel_read, "ssh_channel_read") ||
      !load_symbol((void **)&p_ssh_channel_write, "ssh_channel_write") ||
      !load_symbol((void **)&p_ssh_channel_send_eof, "ssh_channel_send_eof") ||
      !load_symbol((void **)&p_ssh_channel_close, "ssh_channel_close") ||
      !load_symbol((void **)&p_ssh_channel_get_exit_status, "ssh_channel_get_exit_status") ||
      !load_symbol((void **)&p_ssh_channel_is_eof, "ssh_channel_is_eof") ||
      !load_symbol((void **)&p_ssh_get_fd, "ssh_get_fd") ||
      !load_symbol((void **)&p_ssh_set_blocking, "ssh_set_blocking") ||
      !load_symbol((void **)&p_sftp_new, "sftp_new") ||
      !load_symbol((void **)&p_sftp_init, "sftp_init") ||
      !load_symbol((void **)&p_sftp_free, "sftp_free") ||
      !load_symbol((void **)&p_sftp_get_error, "sftp_get_error") ||
      !load_symbol((void **)&p_sftp_opendir, "sftp_opendir") ||
      !load_symbol((void **)&p_sftp_readdir, "sftp_readdir") ||
      !load_symbol((void **)&p_sftp_dir_eof, "sftp_dir_eof") ||
      !load_symbol((void **)&p_sftp_closedir, "sftp_closedir") ||
      !load_symbol((void **)&p_sftp_stat, "sftp_stat") ||
      !load_symbol((void **)&p_sftp_attributes_free, "sftp_attributes_free") ||
      !load_symbol((void **)&p_sftp_open, "sftp_open") ||
      !load_symbol((void **)&p_sftp_close, "sftp_close") ||
      !load_symbol((void **)&p_sftp_read, "sftp_read") ||
      !load_symbol((void **)&p_sftp_write, "sftp_write") ||
      !load_symbol((void **)&p_sftp_unlink, "sftp_unlink") ||
      !load_symbol((void **)&p_sftp_mkdir, "sftp_mkdir") ||
      !load_symbol((void **)&p_sftp_rmdir, "sftp_rmdir") ||
      !load_symbol((void **)&p_sftp_rename, "sftp_rename") ||
      !load_symbol((void **)&p_sftp_file_set_nonblocking, "sftp_file_set_nonblocking") ||
      !load_symbol((void **)&p_sftp_file_set_blocking, "sftp_file_set_blocking") ||
      !load_symbol((void **)&p_sftp_aio_free, "sftp_aio_free") ||
      !load_symbol((void **)&p_sftp_aio_begin_read, "sftp_aio_begin_read") ||
      !load_symbol((void **)&p_sftp_aio_wait_read, "sftp_aio_wait_read") ||
      !load_symbol((void **)&p_sftp_aio_begin_write, "sftp_aio_begin_write") ||
      !load_symbol((void **)&p_sftp_aio_wait_write, "sftp_aio_wait_write")) {
    dlclose(ssh_handle);
    ssh_handle = NULL;
    return 0;
  }

  return 1;
}

static ptr ssh_error_status(ssh_session session, const char *fallback) {
  const char *msg = NULL;
  if (session != NULL && p_ssh_get_error != NULL) msg = p_ssh_get_error(session);
  return make_error_status_message(msg == NULL ? fallback : msg);
}

static ptr ssh_error_status_from_wrapper(chezpp_ssh_session *wrapper, const char *fallback) {
  return ssh_error_status(wrapper == NULL ? NULL : wrapper->session, fallback);
}

static ptr ssh_channel_error_status(chezpp_ssh_channel *wrapper, const char *fallback) {
  if (wrapper == NULL || wrapper->owner == NULL) return make_error_status_message(fallback);
  return ssh_error_status(wrapper->owner->session, fallback);
}

static ptr sftp_error_status(chezpp_sftp_session *wrapper, const char *fallback) {
  char buffer[128];
  if (wrapper != NULL && wrapper->owner != NULL) {
    const char *msg = p_ssh_get_error(wrapper->owner->session);
    if (msg != NULL && *msg != 0) return make_error_status_message(msg);
  }
  if (wrapper != NULL && wrapper->sftp != NULL && p_sftp_get_error != NULL) {
    snprintf(buffer, sizeof(buffer), "%s (sftp code %d)", fallback, p_sftp_get_error(wrapper->sftp));
    return make_error_status_message(buffer);
  }
  return make_error_status_message(fallback);
}

static ptr sftp_file_error_status(chezpp_sftp_file *wrapper, const char *fallback) {
  if (wrapper == NULL || wrapper->owner == NULL) return make_error_status_message(fallback);
  return sftp_error_status(wrapper->owner, fallback);
}

static ptr make_ssh_handle(uptr handle) { return Sunsigned(handle); }

static int64_t monotonic_ms(void) {
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) return -1;
  return (int64_t)ts.tv_sec * 1000 + (int64_t)(ts.tv_nsec / 1000000);
}

static ptr wait_ssh_session_until(ssh_session session, short events, int64_t deadline,
                                  const char *timeout_msg) {
  struct pollfd pfd;

  if (session == NULL || p_ssh_get_fd == NULL) return make_error_status_message("invalid ssh session");
  pfd.fd = (int)p_ssh_get_fd(session);
  if (pfd.fd < 0) return make_error_status_message("failed to query ssh session socket");
  pfd.events = events;
  pfd.revents = 0;

  for (;;) {
    int wait_ms = -1;
    int rc;
    if (deadline >= 0) {
      int64_t now = monotonic_ms();
      int64_t remaining;
      if (now < 0) return make_errno_status();
      remaining = deadline - now;
      if (remaining <= 0) return make_error_status_message(timeout_msg);
      wait_ms = remaining > INT_MAX ? INT_MAX : (int)remaining;
    }
    rc = poll(&pfd, 1, wait_ms);
    if (rc > 0) {
      if ((pfd.revents & (POLLERR | POLLHUP | POLLNVAL)) != 0)
        return make_error_status_message("ssh session socket failed while waiting for readiness");
      if ((pfd.revents & events) != 0) return Strue;
      continue;
    }
    if (rc == 0) return make_error_status_message(timeout_msg);
    if (errno == EINTR) continue;
    return make_errno_status();
  }
}

static ptr sftp_attr_to_vector(sftp_attributes attr) {
  ptr v = Smake_vector(8, Sfalse);
  Svector_set(v, 0, attr->name == NULL ? Sfalse : Sstring(attr->name));
  Svector_set(v, 1, Sfixnum((iptr)attr->type));
  Svector_set(v, 2, Sunsigned64(attr->size));
  Svector_set(v, 3, Sunsigned((uptr)attr->permissions));
  Svector_set(v, 4, Sunsigned((uptr)attr->uid));
  Svector_set(v, 5, Sunsigned((uptr)attr->gid));
  Svector_set(v, 6, Sunsigned64(attr->atime64));
  Svector_set(v, 7, Sunsigned64(attr->mtime64));
  return v;
}

ptr chezpp_net_ssh_open(const char *host, int port, const char *user, int timeout_ms) {
  ssh_session session;
  chezpp_ssh_session *wrapper;
  unsigned int uport;
  long timeout_sec;
  long timeout_usec;

  if (!ensure_ssh_loaded()) return make_error_status_message("failed to load libssh");

  session = p_ssh_new();
  if (session == NULL) return make_error_status_message("failed to allocate ssh session");

  uport = port <= 0 ? 22u : (unsigned int)port;
  timeout_sec = (long)(timeout_ms / 1000);
  timeout_usec = (long)((timeout_ms % 1000) * 1000);

  if (p_ssh_options_set(session, SSH_OPTIONS_HOST, host) != SSH_OK ||
      p_ssh_options_set(session, SSH_OPTIONS_PORT, &uport) != SSH_OK ||
      (user != NULL && *user != 0 && p_ssh_options_set(session, SSH_OPTIONS_USER, user) != SSH_OK) ||
      p_ssh_options_set(session, SSH_OPTIONS_TIMEOUT, &timeout_sec) != SSH_OK ||
      p_ssh_options_set(session, SSH_OPTIONS_TIMEOUT_USEC, &timeout_usec) != SSH_OK) {
    ptr status = ssh_error_status(session, "failed to configure ssh session");
    p_ssh_free(session);
    return status;
  }

  if (p_ssh_options_parse_config(session, NULL) != SSH_OK) {
    ptr status = ssh_error_status(session, "failed to parse ssh config");
    p_ssh_free(session);
    return status;
  }

  if (!add_default_identities(session)) {
    ptr status = ssh_error_status(session, "failed to configure ssh identities");
    p_ssh_free(session);
    return status;
  }

  if (p_ssh_connect(session) != SSH_OK) {
    ptr status = ssh_error_status(session, "failed to connect ssh session");
    p_ssh_disconnect(session);
    p_ssh_free(session);
    return status;
  }

  wrapper = (chezpp_ssh_session *)calloc(1, sizeof(chezpp_ssh_session));
  if (wrapper == NULL) {
    p_ssh_disconnect(session);
    p_ssh_free(session);
    return make_errno_status();
  }
  wrapper->session = session;
  return make_ssh_handle((uptr)wrapper);
}

ptr chezpp_net_ssh_close(uptr handle) {
  chezpp_ssh_session *wrapper = (chezpp_ssh_session *)TO_VOIDP(handle);
  if (wrapper == NULL) return Strue;
  if (wrapper->session != NULL) {
    p_ssh_disconnect(wrapper->session);
    p_ssh_free(wrapper->session);
  }
  free(wrapper);
  return Strue;
}

ptr chezpp_net_ssh_auth_password(uptr handle, const char *user, const char *password) {
  chezpp_ssh_session *wrapper = (chezpp_ssh_session *)TO_VOIDP(handle);
  int rc;
  if (wrapper == NULL || wrapper->session == NULL) return make_error_status_message("invalid ssh session");
  rc = p_ssh_userauth_password(wrapper->session, user != NULL && *user != 0 ? user : NULL, password);
  if (rc == SSH_AUTH_SUCCESS) return Strue;
  return ssh_error_status_from_wrapper(wrapper, "ssh password authentication failed");
}

ptr chezpp_net_ssh_auth_publickey_auto(uptr handle, const char *user, const char *passphrase) {
  chezpp_ssh_session *wrapper = (chezpp_ssh_session *)TO_VOIDP(handle);
  int rc;
  if (wrapper == NULL || wrapper->session == NULL) return make_error_status_message("invalid ssh session");
  rc = p_ssh_userauth_publickey_auto(wrapper->session,
                                     user != NULL && *user != 0 ? user : NULL,
                                     passphrase != NULL && *passphrase != 0 ? passphrase : NULL);
  if (rc == SSH_AUTH_SUCCESS) return Strue;
  return ssh_error_status_from_wrapper(wrapper, "ssh publickey authentication failed");
}

ptr chezpp_net_ssh_auth_agent(uptr handle, const char *user) {
  chezpp_ssh_session *wrapper = (chezpp_ssh_session *)TO_VOIDP(handle);
  int rc;
  if (wrapper == NULL || wrapper->session == NULL) return make_error_status_message("invalid ssh session");
  rc = p_ssh_userauth_agent(wrapper->session, user != NULL && *user != 0 ? user : NULL);
  if (rc == SSH_AUTH_SUCCESS) return Strue;
  return ssh_error_status_from_wrapper(wrapper, "ssh agent authentication failed");
}

ptr chezpp_net_ssh_channel_open(uptr handle) {
  chezpp_ssh_session *wrapper = (chezpp_ssh_session *)TO_VOIDP(handle);
  chezpp_ssh_channel *channel_wrapper;
  ssh_channel channel;

  if (wrapper == NULL || wrapper->session == NULL) return make_error_status_message("invalid ssh session");

  channel = p_ssh_channel_new(wrapper->session);
  if (channel == NULL) return ssh_error_status_from_wrapper(wrapper, "failed to allocate ssh channel");
  if (p_ssh_channel_open_session(channel) != SSH_OK) {
    ptr status = ssh_error_status_from_wrapper(wrapper, "failed to open ssh channel");
    p_ssh_channel_free(channel);
    return status;
  }

  channel_wrapper = (chezpp_ssh_channel *)calloc(1, sizeof(chezpp_ssh_channel));
  if (channel_wrapper == NULL) {
    p_ssh_channel_close(channel);
    p_ssh_channel_free(channel);
    return make_errno_status();
  }
  channel_wrapper->channel = channel;
  channel_wrapper->owner = wrapper;
  return make_ssh_handle((uptr)channel_wrapper);
}

ptr chezpp_net_ssh_channel_close(uptr handle) {
  chezpp_ssh_channel *wrapper = (chezpp_ssh_channel *)TO_VOIDP(handle);
  if (wrapper == NULL) return Strue;
  if (wrapper->channel != NULL) {
    p_ssh_channel_send_eof(wrapper->channel);
    p_ssh_channel_close(wrapper->channel);
    p_ssh_channel_free(wrapper->channel);
  }
  free(wrapper);
  return Strue;
}

ptr chezpp_net_ssh_channel_request_exec(uptr handle, const char *cmd) {
  chezpp_ssh_channel *wrapper = (chezpp_ssh_channel *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->channel == NULL) return make_error_status_message("invalid ssh channel");
  if (p_ssh_channel_request_exec(wrapper->channel, cmd) != SSH_OK)
    return ssh_channel_error_status(wrapper, "failed to request ssh exec");
  return Strue;
}

ptr chezpp_net_ssh_channel_request_shell(uptr handle) {
  chezpp_ssh_channel *wrapper = (chezpp_ssh_channel *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->channel == NULL) return make_error_status_message("invalid ssh channel");
  if (p_ssh_channel_request_shell(wrapper->channel) != SSH_OK)
    return ssh_channel_error_status(wrapper, "failed to request ssh shell");
  return Strue;
}

ptr chezpp_net_ssh_channel_request_pty(uptr handle) {
  chezpp_ssh_channel *wrapper = (chezpp_ssh_channel *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->channel == NULL) return make_error_status_message("invalid ssh channel");
  if (p_ssh_channel_request_pty(wrapper->channel) != SSH_OK)
    return ssh_channel_error_status(wrapper, "failed to request ssh pty");
  return Strue;
}

ptr chezpp_net_ssh_channel_read(uptr handle, int size, int is_stderr, int nonblocking, int timeout_ms) {
  chezpp_ssh_channel *wrapper = (chezpp_ssh_channel *)TO_VOIDP(handle);
  int rc;
  ptr out;
  int use_nonblocking;
  int64_t deadline = -1;

  if (wrapper == NULL || wrapper->channel == NULL || wrapper->owner == NULL)
    return make_error_status_message("invalid ssh channel");

  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }
  use_nonblocking = nonblocking || timeout_ms >= 0;
  if (use_nonblocking) p_ssh_set_blocking(wrapper->owner->session, 0);
  out = Smake_bytevector((iptr)size, 0);
  for (;;) {
    rc = p_ssh_channel_read(wrapper->channel, Sbytevector_data(out), (uint32_t)size, is_stderr);
    if (rc != SSH_AGAIN || nonblocking || timeout_ms < 0) break;
    {
      ptr wait_status = wait_ssh_session_until(wrapper->owner->session, POLLIN, deadline,
                                               "ssh read timed out");
      if (wait_status != Strue) {
        if (use_nonblocking) p_ssh_set_blocking(wrapper->owner->session, 1);
        return wait_status;
      }
    }
  }
  if (use_nonblocking) p_ssh_set_blocking(wrapper->owner->session, 1);

  if (rc == SSH_AGAIN) return make_status("would-block", Sfalse);
  if (rc == SSH_ERROR) return ssh_channel_error_status(wrapper, "ssh read failed");
  if (rc == 0 && p_ssh_channel_is_eof(wrapper->channel)) return Seof_object;
  if (rc < 0) return ssh_channel_error_status(wrapper, "ssh read failed");
  if (rc == size) return out;

  {
    ptr clipped = Smake_bytevector((iptr)rc, 0);
    memcpy(Sbytevector_data(clipped), Sbytevector_data(out), (size_t)rc);
    return clipped;
  }
}

ptr chezpp_net_ssh_channel_read_into(uptr handle, ptr bv, int start, int stop, int is_stderr,
                                     int nonblocking, int timeout_ms) {
  chezpp_ssh_channel *wrapper = (chezpp_ssh_channel *)TO_VOIDP(handle);
  int rc;
  int use_nonblocking;
  int64_t deadline = -1;

  if (wrapper == NULL || wrapper->channel == NULL || wrapper->owner == NULL)
    return make_error_status_message("invalid ssh channel");

  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }
  use_nonblocking = nonblocking || timeout_ms >= 0;
  if (use_nonblocking) p_ssh_set_blocking(wrapper->owner->session, 0);
  for (;;) {
    rc = p_ssh_channel_read(wrapper->channel, Sbytevector_data(bv) + start,
                            (uint32_t)(stop - start), is_stderr);
    if (rc != SSH_AGAIN || nonblocking || timeout_ms < 0) break;
    {
      ptr wait_status = wait_ssh_session_until(wrapper->owner->session, POLLIN, deadline,
                                               "ssh read timed out");
      if (wait_status != Strue) {
        if (use_nonblocking) p_ssh_set_blocking(wrapper->owner->session, 1);
        return wait_status;
      }
    }
  }
  if (use_nonblocking) p_ssh_set_blocking(wrapper->owner->session, 1);

  if (rc == SSH_AGAIN) return make_status("would-block", Sfalse);
  if (rc == SSH_ERROR) return ssh_channel_error_status(wrapper, "ssh read failed");
  if (rc == 0 && p_ssh_channel_is_eof(wrapper->channel)) return Seof_object;
  if (rc < 0) return ssh_channel_error_status(wrapper, "ssh read failed");
  return Sfixnum((iptr)rc);
}

ptr chezpp_net_ssh_channel_write(uptr handle, ptr bv, int start, int stop, int nonblocking,
                                 int timeout_ms) {
  chezpp_ssh_channel *wrapper = (chezpp_ssh_channel *)TO_VOIDP(handle);
  int rc;
  int use_nonblocking;
  int64_t deadline = -1;

  if (wrapper == NULL || wrapper->channel == NULL || wrapper->owner == NULL)
    return make_error_status_message("invalid ssh channel");

  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }
  use_nonblocking = nonblocking || timeout_ms >= 0;
  if (use_nonblocking) p_ssh_set_blocking(wrapper->owner->session, 0);
  for (;;) {
    rc = p_ssh_channel_write(wrapper->channel, Sbytevector_data(bv) + start,
                             (uint32_t)(stop - start));
    if (rc != SSH_AGAIN || nonblocking || timeout_ms < 0) break;
    {
      ptr wait_status = wait_ssh_session_until(wrapper->owner->session, POLLOUT, deadline,
                                               "ssh write timed out");
      if (wait_status != Strue) {
        if (use_nonblocking) p_ssh_set_blocking(wrapper->owner->session, 1);
        return wait_status;
      }
    }
  }
  if (use_nonblocking) p_ssh_set_blocking(wrapper->owner->session, 1);

  if (rc == SSH_AGAIN) return make_status("would-block", Sfalse);
  if (rc == SSH_ERROR || rc < 0) return ssh_channel_error_status(wrapper, "ssh write failed");
  return Sfixnum((iptr)rc);
}

ptr chezpp_net_ssh_channel_exit_status(uptr handle) {
  chezpp_ssh_channel *wrapper = (chezpp_ssh_channel *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->channel == NULL) return make_error_status_message("invalid ssh channel");
  return Sfixnum((iptr)p_ssh_channel_get_exit_status(wrapper->channel));
}

ptr chezpp_net_sftp_open(uptr handle) {
  chezpp_ssh_session *wrapper = (chezpp_ssh_session *)TO_VOIDP(handle);
  chezpp_sftp_session *sftp_wrapper;
  sftp_session sftp;

  if (wrapper == NULL || wrapper->session == NULL) return make_error_status_message("invalid ssh session");

  sftp = p_sftp_new(wrapper->session);
  if (sftp == NULL) return ssh_error_status_from_wrapper(wrapper, "failed to allocate sftp session");
  if (p_sftp_init(sftp) != SSH_OK) {
    ptr status = ssh_error_status_from_wrapper(wrapper, "failed to initialize sftp session");
    p_sftp_free(sftp);
    return status;
  }

  sftp_wrapper = (chezpp_sftp_session *)calloc(1, sizeof(chezpp_sftp_session));
  if (sftp_wrapper == NULL) {
    p_sftp_free(sftp);
    return make_errno_status();
  }
  sftp_wrapper->sftp = sftp;
  sftp_wrapper->owner = wrapper;
  return make_ssh_handle((uptr)sftp_wrapper);
}

ptr chezpp_net_sftp_close(uptr handle) {
  chezpp_sftp_session *wrapper = (chezpp_sftp_session *)TO_VOIDP(handle);
  if (wrapper == NULL) return Strue;
  if (wrapper->sftp != NULL) p_sftp_free(wrapper->sftp);
  free(wrapper);
  return Strue;
}

ptr chezpp_net_sftp_list(uptr handle, const char *path) {
  chezpp_sftp_session *wrapper = (chezpp_sftp_session *)TO_VOIDP(handle);
  sftp_dir dir;
  ptr out = Snil;

  if (wrapper == NULL || wrapper->sftp == NULL) return make_error_status_message("invalid sftp session");

  dir = p_sftp_opendir(wrapper->sftp, path);
  if (dir == NULL) return sftp_error_status(wrapper, "failed to open sftp directory");

  while (!p_sftp_dir_eof(dir)) {
    sftp_attributes attr = p_sftp_readdir(wrapper->sftp, dir);
    if (attr == NULL) break;
    out = Scons(attr->name == NULL ? Sfalse : Sstring(attr->name), out);
    p_sftp_attributes_free(attr);
  }

  if (p_sftp_closedir(dir) != SSH_OK) return sftp_error_status(wrapper, "failed to close sftp directory");
  return out;
}

ptr chezpp_net_sftp_stat(uptr handle, const char *path) {
  chezpp_sftp_session *wrapper = (chezpp_sftp_session *)TO_VOIDP(handle);
  sftp_attributes attr;
  ptr out;

  if (wrapper == NULL || wrapper->sftp == NULL) return make_error_status_message("invalid sftp session");

  attr = p_sftp_stat(wrapper->sftp, path);
  if (attr == NULL) return sftp_error_status(wrapper, "failed to stat sftp path");
  out = sftp_attr_to_vector(attr);
  p_sftp_attributes_free(attr);
  return out;
}

ptr chezpp_net_sftp_delete(uptr handle, const char *path) {
  chezpp_sftp_session *wrapper = (chezpp_sftp_session *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->sftp == NULL) return make_error_status_message("invalid sftp session");
  if (p_sftp_unlink(wrapper->sftp, path) != SSH_OK) return sftp_error_status(wrapper, "failed to delete sftp file");
  return Strue;
}

ptr chezpp_net_sftp_mkdir(uptr handle, const char *path, int mode) {
  chezpp_sftp_session *wrapper = (chezpp_sftp_session *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->sftp == NULL) return make_error_status_message("invalid sftp session");
  if (p_sftp_mkdir(wrapper->sftp, path, (mode_t)mode) != SSH_OK)
    return sftp_error_status(wrapper, "failed to create sftp directory");
  return Strue;
}

ptr chezpp_net_sftp_rmdir(uptr handle, const char *path) {
  chezpp_sftp_session *wrapper = (chezpp_sftp_session *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->sftp == NULL) return make_error_status_message("invalid sftp session");
  if (p_sftp_rmdir(wrapper->sftp, path) != SSH_OK)
    return sftp_error_status(wrapper, "failed to remove sftp directory");
  return Strue;
}

ptr chezpp_net_sftp_rename(uptr handle, const char *from_path, const char *to_path) {
  chezpp_sftp_session *wrapper = (chezpp_sftp_session *)TO_VOIDP(handle);
  if (wrapper == NULL || wrapper->sftp == NULL) return make_error_status_message("invalid sftp session");
  if (p_sftp_rename(wrapper->sftp, from_path, to_path) != SSH_OK)
    return sftp_error_status(wrapper, "failed to rename sftp path");
  return Strue;
}

ptr chezpp_net_sftp_open_file(uptr handle, const char *path, int flags, int mode) {
  chezpp_sftp_session *wrapper = (chezpp_sftp_session *)TO_VOIDP(handle);
  chezpp_sftp_file *file_wrapper;
  sftp_file file;

  if (wrapper == NULL || wrapper->sftp == NULL) return make_error_status_message("invalid sftp session");

  file = p_sftp_open(wrapper->sftp, path, flags, (mode_t)mode);
  if (file == NULL) return sftp_error_status(wrapper, "failed to open sftp file");

  file_wrapper = (chezpp_sftp_file *)calloc(1, sizeof(chezpp_sftp_file));
  if (file_wrapper == NULL) {
    p_sftp_close(file);
    return make_errno_status();
  }
  file_wrapper->file = file;
  file_wrapper->owner = wrapper;
  return make_ssh_handle((uptr)file_wrapper);
}

ptr chezpp_net_sftp_close_file(uptr handle) {
  chezpp_sftp_file *wrapper = (chezpp_sftp_file *)TO_VOIDP(handle);
  if (wrapper == NULL) return Strue;
  if (wrapper->pending_read != NULL) p_sftp_aio_free(wrapper->pending_read);
  if (wrapper->pending_write != NULL) p_sftp_aio_free(wrapper->pending_write);
  if (wrapper->file != NULL) p_sftp_close(wrapper->file);
  free(wrapper);
  return Strue;
}

ptr chezpp_net_sftp_read(uptr handle, int size, int nonblocking, int timeout_ms) {
  chezpp_sftp_file *wrapper = (chezpp_sftp_file *)TO_VOIDP(handle);
  ssize_t rc;
  ptr out;
  int use_nonblocking;
  int64_t deadline = -1;

  if (wrapper == NULL || wrapper->file == NULL) return make_error_status_message("invalid sftp file");

  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }
  use_nonblocking = nonblocking || timeout_ms >= 0;

  if (!use_nonblocking) {
    out = Smake_bytevector((iptr)size, 0);
    rc = p_sftp_read(wrapper->file, Sbytevector_data(out), (size_t)size);

    if (rc == SSH_AGAIN) return make_status("would-block", Sfalse);
    if (rc == SSH_ERROR || rc < 0) return sftp_file_error_status(wrapper, "sftp read failed");
    if (rc == 0) return Seof_object;
    if (rc == size) return out;

    {
      ptr clipped = Smake_bytevector((iptr)rc, 0);
      memcpy(Sbytevector_data(clipped), Sbytevector_data(out), (size_t)rc);
      return clipped;
    }
  }

  p_ssh_set_blocking(wrapper->owner->owner->session, 0);
  p_sftp_file_set_nonblocking(wrapper->file);
  for (;;) {
    if (wrapper->pending_read == NULL) {
      rc = p_sftp_aio_begin_read(wrapper->file, (size_t)size, &wrapper->pending_read);
      if (rc == SSH_ERROR || rc < 0) {
        p_sftp_file_set_blocking(wrapper->file);
        p_ssh_set_blocking(wrapper->owner->owner->session, 1);
        return sftp_file_error_status(wrapper, "sftp read failed");
      }
      wrapper->pending_read_len = (size_t)rc;
    }
    out = Smake_bytevector((iptr)wrapper->pending_read_len, 0);
    rc = p_sftp_aio_wait_read(&wrapper->pending_read, Sbytevector_data(out), wrapper->pending_read_len);
    if (rc != SSH_AGAIN || nonblocking || timeout_ms < 0) break;
    {
      ptr wait_status = wait_ssh_session_until(wrapper->owner->owner->session, POLLIN, deadline,
                                               "sftp read timed out");
      if (wait_status != Strue) {
        p_sftp_file_set_blocking(wrapper->file);
        p_ssh_set_blocking(wrapper->owner->owner->session, 1);
        return wait_status;
      }
    }
  }
  p_sftp_file_set_blocking(wrapper->file);
  p_ssh_set_blocking(wrapper->owner->owner->session, 1);

  if (rc == SSH_AGAIN) return make_status("would-block", Sfalse);
  wrapper->pending_read_len = 0;
  if (rc == SSH_ERROR || rc < 0) return sftp_file_error_status(wrapper, "sftp read failed");
  if (rc == 0) return Seof_object;
  if (rc == (ssize_t)Sbytevector_length(out)) return out;

  {
    ptr clipped = Smake_bytevector((iptr)rc, 0);
    memcpy(Sbytevector_data(clipped), Sbytevector_data(out), (size_t)rc);
    return clipped;
  }
}

ptr chezpp_net_sftp_read_into(uptr handle, ptr bv, int start, int stop, int nonblocking,
                              int timeout_ms) {
  chezpp_sftp_file *wrapper = (chezpp_sftp_file *)TO_VOIDP(handle);
  ssize_t rc;
  int use_nonblocking;
  int64_t deadline = -1;

  if (wrapper == NULL || wrapper->file == NULL) return make_error_status_message("invalid sftp file");

  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }
  use_nonblocking = nonblocking || timeout_ms >= 0;

  if (!use_nonblocking) {
    rc = p_sftp_read(wrapper->file, Sbytevector_data(bv) + start, (size_t)(stop - start));

    if (rc == SSH_AGAIN) return make_status("would-block", Sfalse);
    if (rc == SSH_ERROR || rc < 0) return sftp_file_error_status(wrapper, "sftp read failed");
    if (rc == 0) return Seof_object;
    return Sfixnum((iptr)rc);
  }

  p_ssh_set_blocking(wrapper->owner->owner->session, 0);
  p_sftp_file_set_nonblocking(wrapper->file);
  for (;;) {
    if (wrapper->pending_read == NULL) {
      rc = p_sftp_aio_begin_read(wrapper->file, (size_t)(stop - start), &wrapper->pending_read);
      if (rc == SSH_ERROR || rc < 0) {
        p_sftp_file_set_blocking(wrapper->file);
        p_ssh_set_blocking(wrapper->owner->owner->session, 1);
        return sftp_file_error_status(wrapper, "sftp read failed");
      }
      wrapper->pending_read_len = (size_t)rc;
    }
    rc = p_sftp_aio_wait_read(&wrapper->pending_read,
                              Sbytevector_data(bv) + start,
                              (size_t)(stop - start));
    if (rc != SSH_AGAIN || nonblocking || timeout_ms < 0) break;
    {
      ptr wait_status = wait_ssh_session_until(wrapper->owner->owner->session, POLLIN, deadline,
                                               "sftp read timed out");
      if (wait_status != Strue) {
        p_sftp_file_set_blocking(wrapper->file);
        p_ssh_set_blocking(wrapper->owner->owner->session, 1);
        return wait_status;
      }
    }
  }
  p_sftp_file_set_blocking(wrapper->file);
  p_ssh_set_blocking(wrapper->owner->owner->session, 1);

  if (rc == SSH_AGAIN) return make_status("would-block", Sfalse);
  wrapper->pending_read_len = 0;
  if (rc == SSH_ERROR || rc < 0) return sftp_file_error_status(wrapper, "sftp read failed");
  if (rc == 0) return Seof_object;
  return Sfixnum((iptr)rc);
}

ptr chezpp_net_sftp_write(uptr handle, ptr bv, int start, int stop, int nonblocking,
                          int timeout_ms) {
  chezpp_sftp_file *wrapper = (chezpp_sftp_file *)TO_VOIDP(handle);
  ssize_t rc;
  int use_nonblocking;
  int64_t deadline = -1;

  if (wrapper == NULL || wrapper->file == NULL) return make_error_status_message("invalid sftp file");

  if (timeout_ms >= 0) {
    deadline = monotonic_ms();
    if (deadline < 0) return make_errno_status();
    deadline += timeout_ms;
  }
  use_nonblocking = nonblocking || timeout_ms >= 0;

  if (!use_nonblocking) {
    rc = p_sftp_write(wrapper->file, Sbytevector_data(bv) + start, (size_t)(stop - start));

    if (rc == SSH_AGAIN) return make_status("would-block", Sfalse);
    if (rc == SSH_ERROR || rc < 0) return sftp_file_error_status(wrapper, "sftp write failed");
    return Sfixnum((iptr)rc);
  }

  p_ssh_set_blocking(wrapper->owner->owner->session, 0);
  p_sftp_file_set_nonblocking(wrapper->file);
  for (;;) {
    if (wrapper->pending_write == NULL) {
      rc = p_sftp_aio_begin_write(wrapper->file,
                                  Sbytevector_data(bv) + start,
                                  (size_t)(stop - start),
                                  &wrapper->pending_write);
      if (rc == SSH_ERROR || rc < 0) {
        p_sftp_file_set_blocking(wrapper->file);
        p_ssh_set_blocking(wrapper->owner->owner->session, 1);
        return sftp_file_error_status(wrapper, "sftp write failed");
      }
      wrapper->pending_write_len = (size_t)rc;
    }
    rc = p_sftp_aio_wait_write(&wrapper->pending_write);
    if (rc != SSH_AGAIN || nonblocking || timeout_ms < 0) break;
    {
      ptr wait_status = wait_ssh_session_until(wrapper->owner->owner->session, POLLOUT, deadline,
                                               "sftp write timed out");
      if (wait_status != Strue) {
        p_sftp_file_set_blocking(wrapper->file);
        p_ssh_set_blocking(wrapper->owner->owner->session, 1);
        return wait_status;
      }
    }
  }
  p_sftp_file_set_blocking(wrapper->file);
  p_ssh_set_blocking(wrapper->owner->owner->session, 1);

  if (rc == SSH_AGAIN) return make_status("would-block", Sfalse);
  wrapper->pending_write_len = 0;
  if (rc == SSH_ERROR || rc < 0) return sftp_file_error_status(wrapper, "sftp write failed");
  return Sfixnum((iptr)rc);
}

int chezpp_net_open_rdonly(void) { return O_RDONLY; }
int chezpp_net_open_wronly(void) { return O_WRONLY; }
int chezpp_net_open_rdwr(void) { return O_RDWR; }
int chezpp_net_open_append(void) { return O_APPEND; }
int chezpp_net_open_creat(void) { return O_CREAT; }
int chezpp_net_open_trunc(void) { return O_TRUNC; }
int chezpp_net_open_excl(void) { return O_EXCL; }
#ifdef O_TEXT
int chezpp_net_open_text(void) { return O_TEXT; }
#else
int chezpp_net_open_text(void) { return 0; }
#endif
