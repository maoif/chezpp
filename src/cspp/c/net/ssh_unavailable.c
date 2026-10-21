#include "unavailable.h"

ptr cspp_net_ssh_open(const char *host, int port, const char *user, int timeout_ms,
                        int hostkey_policy) {
  (void)host;
  (void)port;
  (void)user;
  (void)timeout_ms;
  (void)hostkey_policy;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_close(uptr handle) {
  (void)handle;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_session_fd(uptr handle) {
  (void)handle;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_auth_password(uptr handle, const char *user, const char *password) {
  (void)handle;
  (void)user;
  (void)password;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_auth_publickey_auto(uptr handle, const char *user, const char *passphrase) {
  (void)handle;
  (void)user;
  (void)passphrase;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_auth_publickey(uptr handle, const char *user, const char *public_path,
                                 const char *private_path, const char *passphrase) {
  (void)handle;
  (void)user;
  (void)public_path;
  (void)private_path;
  (void)passphrase;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_auth_keyboard_interactive_step(uptr handle, const char *user) {
  (void)handle;
  (void)user;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_auth_keyboard_interactive_answer(uptr handle, int index,
                                                    const char *answer) {
  (void)handle;
  (void)index;
  (void)answer;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_auth_agent(uptr handle, const char *user) {
  (void)handle;
  (void)user;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_auth_agent_identity(uptr handle, const char *user, const char *identity) {
  (void)handle;
  (void)user;
  (void)identity;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_known_host_check(uptr handle, const char *path) {
  (void)handle;
  (void)path;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_known_host_update(uptr handle, const char *path) {
  (void)handle;
  (void)path;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_known_host_export(uptr handle) {
  (void)handle;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_channel_open(uptr handle, int timeout_ms) {
  (void)handle;
  (void)timeout_ms;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_channel_open_forward(uptr handle, const char *remote_host, int remote_port,
                                        const char *source_host, int source_port,
                                        int timeout_ms) {
  (void)handle;
  (void)remote_host;
  (void)remote_port;
  (void)source_host;
  (void)source_port;
  (void)timeout_ms;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_channel_forward_start(uptr handle) {
  (void)handle;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_channel_forward_step(uptr handle, const char *remote_host, int remote_port,
                                       const char *source_host, int source_port) {
  (void)handle; (void)remote_host; (void)remote_port;
  (void)source_host; (void)source_port;
  return cspp_unavailable_status("ssh: disabled at build time");
}

void cspp_net_ssh_channel_forward_discard(uptr handle) { (void)handle; }

ptr cspp_net_ssh_remote_forward_listen(uptr handle, const char *address, int port) {
  (void)handle;
  (void)address;
  (void)port;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_remote_forward_accept(uptr handle) {
  (void)handle;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_remote_forward_cancel(uptr handle, const char *address, int port) {
  (void)handle;
  (void)address;
  (void)port;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_channel_send_eof(uptr handle) {
  (void)handle;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_channel_close(uptr handle) {
  (void)handle;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_channel_request_exec(uptr handle, const char *cmd, int timeout_ms) {
  (void)handle;
  (void)cmd;
  (void)timeout_ms;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_channel_request_shell(uptr handle, int timeout_ms) {
  (void)handle;
  (void)timeout_ms;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_channel_request_pty(uptr handle, int timeout_ms) {
  (void)handle;
  (void)timeout_ms;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_channel_request_pty_size(uptr handle, const char *terminal,
                                          int columns, int rows, int resize, int timeout_ms) {
  (void)handle; (void)terminal; (void)columns; (void)rows; (void)resize; (void)timeout_ms;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_channel_request_environment(uptr handle, const char *name,
                                                const char *value) {
  (void)handle;
  (void)name;
  (void)value;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_channel_request_subsystem(uptr handle, const char *subsystem) {
  (void)handle;
  (void)subsystem;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_channel_read(uptr handle, int size, int is_stderr, int nonblocking, int timeout_ms) {
  (void)handle;
  (void)size;
  (void)is_stderr;
  (void)nonblocking;
  (void)timeout_ms;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_channel_read_into(uptr handle, ptr bv, int start, int stop, int is_stderr,
                                     int nonblocking, int timeout_ms) {
  (void)handle;
  (void)bv;
  (void)start;
  (void)stop;
  (void)is_stderr;
  (void)nonblocking;
  (void)timeout_ms;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_channel_write(uptr handle, ptr bv, int start, int stop, int nonblocking,
                                 int timeout_ms) {
  (void)handle;
  (void)bv;
  (void)start;
  (void)stop;
  (void)nonblocking;
  (void)timeout_ms;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_ssh_channel_exit_status(uptr handle) {
  (void)handle;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_open(uptr handle) {
  (void)handle;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_close(uptr handle) {
  (void)handle;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_scp_download_file(uptr handle, const char *remote_path, const char *local_path,
                                 int timeout_ms) {
  (void)handle;
  (void)remote_path;
  (void)local_path;
  (void)timeout_ms;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_scp_upload_file(uptr handle, const char *local_path, const char *remote_path,
                               int timeout_ms) {
  (void)handle;
  (void)local_path;
  (void)remote_path;
  (void)timeout_ms;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_scp_stat(uptr handle, const char *path) {
  (void)handle;
  (void)path;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_scp_download_directory(uptr handle, const char *remote_path, const char *local_path,
                                      int timeout_ms) {
  (void)handle;
  (void)remote_path;
  (void)local_path;
  (void)timeout_ms;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_scp_upload_directory(uptr handle, const char *local_path, const char *remote_path,
                                    int timeout_ms) {
  (void)handle;
  (void)local_path;
  (void)remote_path;
  (void)timeout_ms;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_scp_transfer_start(uptr handle, int direction, const char *source,
                                  const char *target) {
  (void)handle;
  (void)direction;
  (void)source;
  (void)target;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_scp_transfer_start_many(uptr handle, int direction,
                                      const char *local_directory,
                                      const char *remote_directory, ptr encoded_names) {
  (void)handle; (void)direction; (void)local_directory; (void)remote_directory;
  (void)encoded_names;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_scp_transfer_step(uptr handle) {
  (void)handle;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_scp_transfer_cancel(uptr handle) {
  (void)handle;
  return cspp_unavailable_status("ssh: disabled at build time");
}

void cspp_net_scp_transfer_close(uptr handle) {
  (void)handle;
}

ptr cspp_net_sftp_list(uptr handle, const char *path) {
  (void)handle;
  (void)path;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_stat(uptr handle, const char *path) {
  (void)handle;
  (void)path;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_open_directory(uptr handle, const char *path) {
  (void)handle;
  (void)path;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_read_directory(uptr handle) {
  (void)handle;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_close_directory(uptr handle) {
  (void)handle;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_chmod(uptr handle, const char *path, unsigned mode) {
  (void)handle;
  (void)path;
  (void)mode;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_chown(uptr handle, const char *path, unsigned uid, unsigned gid) {
  (void)handle;
  (void)path;
  (void)uid;
  (void)gid;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_utimes(uptr handle, const char *path, int64_t atime, int64_t mtime) {
  (void)handle;
  (void)path;
  (void)atime;
  (void)mtime;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_symlink(uptr handle, const char *target, const char *dest) {
  (void)handle;
  (void)target;
  (void)dest;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_readlink(uptr handle, const char *path) {
  (void)handle;
  (void)path;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_seek(uptr handle, uint64_t offset) {
  (void)handle;
  (void)offset;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_delete(uptr handle, const char *path) {
  (void)handle;
  (void)path;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_mkdir(uptr handle, const char *path, int mode) {
  (void)handle;
  (void)path;
  (void)mode;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_rmdir(uptr handle, const char *path) {
  (void)handle;
  (void)path;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_rename(uptr handle, const char *from_path, const char *to_path) {
  (void)handle;
  (void)from_path;
  (void)to_path;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_open_file(uptr handle, const char *path, int flags, int mode) {
  (void)handle;
  (void)path;
  (void)flags;
  (void)mode;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_close_file(uptr handle) {
  (void)handle;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_read(uptr handle, int size, int nonblocking, int timeout_ms) {
  (void)handle;
  (void)size;
  (void)nonblocking;
  (void)timeout_ms;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_read_into(uptr handle, ptr bv, int start, int stop, int nonblocking,
                              int timeout_ms) {
  (void)handle;
  (void)bv;
  (void)start;
  (void)stop;
  (void)nonblocking;
  (void)timeout_ms;
  return cspp_unavailable_status("ssh: disabled at build time");
}

ptr cspp_net_sftp_write(uptr handle, ptr bv, int start, int stop, int nonblocking,
                          int timeout_ms) {
  (void)handle;
  (void)bv;
  (void)start;
  (void)stop;
  (void)nonblocking;
  (void)timeout_ms;
  return cspp_unavailable_status("ssh: disabled at build time");
}

int cspp_net_sftp_flag_read(void) {
  return 0;
}

int cspp_net_sftp_flag_write(void) {
  return 0;
}

int cspp_net_sftp_flag_read_write(void) {
  return 0;
}

int cspp_net_sftp_flag_append(void) {
  return 0;
}

int cspp_net_sftp_flag_create(void) {
  return 0;
}

int cspp_net_sftp_flag_truncate(void) {
  return 0;
}

int cspp_net_sftp_flag_exclusive(void) {
  return 0;
}

int cspp_net_sftp_flag_text(void) {
  return 0;
}
