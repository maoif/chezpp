/* Local terminal support for the SSH shell example. State belongs to one caller. */
#include "common.h"
#include <sys/ioctl.h>
#include <termios.h>

typedef struct {
  int fd;
  struct termios saved;
} cspp_ssh_terminal;

static ptr terminal_error(void) {
  int saved_errno = errno;
  ptr result = Smake_vector(2, Sfalse);
  Svector_set(result, 0, Sstring_to_symbol("error"));
  Svector_set(result, 1, Sstring(strerror(saved_errno)));
  return result;
}

ptr cspp_ssh_terminal_size(int fd) {
  struct winsize size;
  ptr result;
  if (!isatty(fd)) return Sfalse;
  if (ioctl(fd, TIOCGWINSZ, &size) != 0) return terminal_error();
  result = Smake_vector(2, Sfalse);
  Svector_set(result, 0, Sfixnum(size.ws_col > 0 ? size.ws_col : 80));
  Svector_set(result, 1, Sfixnum(size.ws_row > 0 ? size.ws_row : 24));
  return result;
}

ptr cspp_ssh_terminal_raw_start(int fd) {
  cspp_ssh_terminal *terminal = malloc(sizeof(*terminal));
  struct termios raw;
  ptr result;
  if (terminal == NULL) return terminal_error();
  terminal->fd = dup(fd);
  if (terminal->fd < 0) { free(terminal); return terminal_error(); }
  if (tcgetattr(terminal->fd, &terminal->saved) != 0) goto fail;
  raw = terminal->saved;
  cfmakeraw(&raw);
  /* Do not discard input typed while the connection is starting. */
  if (tcsetattr(terminal->fd, TCSANOW, &raw) != 0) goto fail;
  result = Smake_vector(2, Sfalse);
  Svector_set(result, 0, Sstring_to_symbol("ok"));
  Svector_set(result, 1, Sunsigned((uptr)terminal));
  return result;
fail:
  {
    int saved_errno = errno;
    close(terminal->fd);
    free(terminal);
    errno = saved_errno;
    return terminal_error();
  }
}

int cspp_ssh_terminal_raw_close(uptr handle) {
  cspp_ssh_terminal *terminal = (cspp_ssh_terminal *)handle;
  int result, saved_errno;
  if (terminal == NULL) return 0;
  do {
    result = tcsetattr(terminal->fd, TCSANOW, &terminal->saved);
  } while (result < 0 && errno == EINTR);
  saved_errno = result < 0 ? errno : 0;
  close(terminal->fd);
  free(terminal);
  return saved_errno;
}
