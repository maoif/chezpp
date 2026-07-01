// #define _GNU_SOURCE

#include <sys/types.h>
#include <sys/wait.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <limits.h>
#include <assert.h>
#include <fcntl.h>
#include <poll.h>
#include <time.h>
#include <grp.h>
#include <pwd.h>


#include "scheme.h"

ptr errno_str();
ptr errno_str_vector();

ptr chezpp_ok(ptr value);
ptr chezpp_errno_result(const char *operation, ptr context);
ptr chezpp_not_found_result(const char *operation, ptr context);
ptr chezpp_unsupported_result(const char *operation);
ptr chezpp_timeout_result(const char *operation, ptr context);

ptr chezpp_filesystem_info(const char *path);
ptr chezpp_send_signal(int pid, int sig);
ptr chezpp_spawn_capture(ptr argv, ptr env, const char *cwd,
                         ptr stdin_payload, int capture_stdout, int capture_stderr,
                         int stdout_null, int stderr_null, int stderr_to_stdout,
                         int timeout_ms);
ptr chezpp_spawn_process(ptr argv, ptr env, const char *cwd,
                         int stdin_null, int stdout_null, int stderr_null);
ptr chezpp_waitpid(int pid, int nohang);
ptr chezpp_make_pipe();
ptr chezpp_spawn_pipeline_capture(ptr specs, int timeout_ms);

char *expand_pathname(const char *inpath);
