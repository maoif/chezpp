#include "../common.h"

#include <poll.h>

static ptr make_status(const char *tag, ptr value) {
  ptr v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, Sstring_to_symbol(tag));
  Svector_set(v, 1, value);
  return v;
}

static ptr make_errno_status(const char *tag) { return make_status(tag, errno_str()); }

int chezpp_net_pollin() { return POLLIN; }
int chezpp_net_pollout() { return POLLOUT; }
int chezpp_net_pollpri() { return POLLPRI; }
int chezpp_net_pollerr() { return POLLERR; }
int chezpp_net_pollhup() { return POLLHUP; }
int chezpp_net_pollnval() { return POLLNVAL; }

ptr chezpp_net_poll(ptr specs, int timeout_ms) {
  struct pollfd *pfds = NULL;
  ptr ls = specs;
  ptr out_head = Snil;
  ptr out_tail = Snil;
  int count = 0;
  int rc;
  int i;

  while (!Snullp(ls)) {
    if (!Spairp(ls)) {
      errno = EINVAL;
      return make_errno_status("error");
    }
    count += 1;
    ls = Scdr(ls);
  }

  pfds = (struct pollfd *)calloc((size_t)count, sizeof(struct pollfd));
  if (pfds == NULL) return make_errno_status("error");

  ls = specs;
  i = 0;
  while (!Snullp(ls)) {
    ptr spec = Scar(ls);
    if (!Svectorp(spec) || Svector_length(spec) != 2 || !Sfixnump(Svector_ref(spec, 0)) ||
        !Sfixnump(Svector_ref(spec, 1))) {
      free(pfds);
      errno = EINVAL;
      return make_errno_status("error");
    }
    pfds[i].fd = (int)Sfixnum_value(Svector_ref(spec, 0));
    pfds[i].events = (short)Sfixnum_value(Svector_ref(spec, 1));
    pfds[i].revents = 0;
    ls = Scdr(ls);
    i += 1;
  }

  rc = poll(pfds, (nfds_t)count, timeout_ms);
  if (rc < 0) {
    free(pfds);
    return make_errno_status("error");
  }

  for (i = 0; i < count; i += 1) {
    ptr item = Smake_vector(3, Sfalse);
    ptr cell;
    Svector_set(item, 0, Sfixnum(pfds[i].fd));
    Svector_set(item, 1, Sfixnum((iptr)pfds[i].events));
    Svector_set(item, 2, Sfixnum((iptr)pfds[i].revents));
    cell = Scons(item, Snil);
    if (Snullp(out_head)) {
      out_head = cell;
      out_tail = cell;
    } else {
      Scdr(out_tail) = cell;
      out_tail = cell;
    }
  }

  free(pfds);
  return out_head;
}
