#include "../cares_loader.h"
#include "../common.h"

#include <ares.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <poll.h>

typedef int (*ares_init_options_fn)(ares_channel_t **, const struct ares_options *, int);
typedef void (*ares_destroy_fn)(ares_channel_t *);
typedef void (*ares_cancel_fn)(ares_channel_t *);
typedef void (*ares_getaddrinfo_fn)(ares_channel_t *, const char *, const char *,
                                    const struct ares_addrinfo_hints *,
                                    ares_addrinfo_callback, void *);
typedef void (*ares_freeaddrinfo_fn)(struct ares_addrinfo *);
typedef int (*ares_getsock_fn)(const ares_channel_t *, ares_socket_t *, int);
typedef struct timeval *(*ares_timeout_fn)(const ares_channel_t *, struct timeval *,
                                           struct timeval *);
typedef void (*ares_process_fd_fn)(ares_channel_t *, ares_socket_t, ares_socket_t);
typedef const char *(*ares_strerror_fn)(int);

typedef struct {
  ares_channel_t *channel;
  struct ares_addrinfo *result;
  char *query_name;
  int status;
  int timeouts;
  int done;
  int deferred_read_fd;
  int deferred_write_fd;
  int deferred_reported;
} chezpp_dns_operation;

static ares_init_options_fn p_ares_init_options;
static ares_destroy_fn p_ares_destroy;
static ares_cancel_fn p_ares_cancel;
static ares_getaddrinfo_fn p_ares_getaddrinfo;
static ares_freeaddrinfo_fn p_ares_freeaddrinfo;
static ares_getsock_fn p_ares_getsock;
static ares_timeout_fn p_ares_timeout;
static ares_process_fd_fn p_ares_process_fd;
static ares_strerror_fn p_ares_strerror;

static ptr make_status(const char *tag, ptr value) {
  ptr out = Smake_vector(2, Sfalse);
  Svector_set(out, 0, Sstring_to_symbol(tag));
  Svector_set(out, 1, value);
  return out;
}

static int load_cares_functions(void) {
#define LOAD(name)                                                                    \
  do {                                                                                \
    p_##name = (name##_fn)chezpp_cares_symbol(#name);                                 \
    if (p_##name == NULL) return 0;                                                    \
  } while (0)
  LOAD(ares_init_options);
  LOAD(ares_destroy);
  LOAD(ares_cancel);
  LOAD(ares_getaddrinfo);
  LOAD(ares_freeaddrinfo);
  LOAD(ares_getsock);
  LOAD(ares_timeout);
  LOAD(ares_process_fd);
  LOAD(ares_strerror);
#undef LOAD
  return 1;
}

static void dns_callback(void *data, int status, int timeouts,
                         struct ares_addrinfo *result) {
  chezpp_dns_operation *operation = (chezpp_dns_operation *)data;
  operation->status = status;
  operation->timeouts = timeouts;
  operation->result = result;
  operation->done = 1;
}

static ptr make_socket_address(const struct sockaddr *address) {
  char host[INET6_ADDRSTRLEN + 1];
  ptr out = Smake_vector(4, Sfalse);
  memset(host, 0, sizeof(host));
  if (address->sa_family == AF_INET) {
    const struct sockaddr_in *v4 = (const struct sockaddr_in *)address;
    if (inet_ntop(AF_INET, &v4->sin_addr, host, sizeof(host)) == NULL) return Sfalse;
    Svector_set(out, 0, Sstring_to_symbol("inet"));
    Svector_set(out, 1, Sstring(host));
    Svector_set(out, 2, Sfixnum(ntohs(v4->sin_port)));
  } else if (address->sa_family == AF_INET6) {
    const struct sockaddr_in6 *v6 = (const struct sockaddr_in6 *)address;
    if (inet_ntop(AF_INET6, &v6->sin6_addr, host, sizeof(host)) == NULL) return Sfalse;
    Svector_set(out, 0, Sstring_to_symbol("inet6"));
    Svector_set(out, 1, Sstring(host));
    Svector_set(out, 2, Sfixnum(ntohs(v6->sin6_port)));
  } else {
    return Sfalse;
  }
  return out;
}

static void append_item(ptr *head, ptr *tail, ptr value) {
  ptr cell = Scons(value, Snil);
  if (*head == Snil) {
    *head = cell;
    *tail = cell;
  } else {
    Scdr(*tail) = cell;
    *tail = cell;
  }
}

static ptr dns_result(chezpp_dns_operation *operation) {
  ptr out;
  ptr addresses = Snil;
  ptr address_tail = Snil;
  ptr aliases = Snil;
  ptr alias_tail = Snil;
  ptr ttls = Snil;
  ptr ttl_tail = Snil;
  const char *canonical_name = NULL;
  struct ares_addrinfo_node *node;
  struct ares_addrinfo_cname *cname;

  if (operation->status != ARES_SUCCESS) {
    out = Smake_vector(3, Sfalse);
    Svector_set(out, 0, Sstring_to_symbol("dns-error"));
    Svector_set(out, 1, Sfixnum(operation->status));
    Svector_set(out, 2, Sstring(p_ares_strerror(operation->status)));
    return out;
  }
  if (operation->result == NULL) return make_status("error", Sstring("empty c-ares result"));

  canonical_name = operation->result->name;
  for (cname = operation->result->cnames; cname != NULL; cname = cname->next) {
    if (cname->alias != NULL) append_item(&aliases, &alias_tail, Sstring(cname->alias));
    if (cname->name != NULL) canonical_name = cname->name;
  }
  for (node = operation->result->nodes; node != NULL; node = node->ai_next) {
    ptr address = make_socket_address(node->ai_addr);
    if (address != Sfalse) {
      append_item(&addresses, &address_tail, address);
      append_item(&ttls, &ttl_tail, Sfixnum(node->ai_ttl));
    }
  }
  out = Smake_vector(6, Sfalse);
  Svector_set(out, 0, Sstring_to_symbol("dns-ok"));
  Svector_set(out, 1, Sstring(operation->query_name));
  Svector_set(out, 2, canonical_name == NULL ? Sfalse : Sstring(canonical_name));
  Svector_set(out, 3, addresses);
  Svector_set(out, 4, aliases);
  Svector_set(out, 5, ttls);
  return out;
}

static ptr pending_result(chezpp_dns_operation *operation) {
  ares_socket_t sockets[ARES_GETSOCK_MAXNUM];
  int bits = p_ares_getsock(operation->channel, sockets, ARES_GETSOCK_MAXNUM);
  struct timeval tv;
  struct timeval *timeout;
  ptr specs = Snil;
  ptr tail = Snil;
  ptr out = Smake_vector(3, Sfalse);
  int index;
  long timeout_ms;

  for (index = 0; index < ARES_GETSOCK_MAXNUM; index++) {
    int events = 0;
    ptr spec;
    if (ARES_GETSOCK_READABLE(bits, index)) events |= 1;
    if (ARES_GETSOCK_WRITABLE(bits, index)) events |= 2;
    if (events == 0) continue;
    spec = Smake_vector(2, Sfalse);
    Svector_set(spec, 0, Sfixnum(sockets[index]));
    Svector_set(spec, 1, Sfixnum(events));
    append_item(&specs, &tail, spec);
  }
  timeout = p_ares_timeout(operation->channel, NULL, &tv);
  timeout_ms = timeout == NULL ? 1000 : timeout->tv_sec * 1000L +
                                        (timeout->tv_usec + 999L) / 1000L;
  if (timeout_ms < 0) timeout_ms = 0;
  Svector_set(out, 0, Sstring_to_symbol("dns-pending"));
  Svector_set(out, 1, specs);
  Svector_set(out, 2, Sfixnum((iptr)timeout_ms));
  return out;
}

static ptr deferred_pending_result(chezpp_dns_operation *operation) {
  ptr spec = Smake_vector(2, Sfalse);
  ptr specs = Scons(spec, Snil);
  ptr out = Smake_vector(3, Sfalse);
  Svector_set(spec, 0, Sfixnum(operation->deferred_read_fd));
  Svector_set(spec, 1, Sfixnum(1));
  Svector_set(out, 0, Sstring_to_symbol("dns-pending"));
  Svector_set(out, 1, specs);
  Svector_set(out, 2, Sfixnum(0));
  return out;
}

uptr chezpp_net_dns_start(const char *name, int family, int timeout_ms) {
  chezpp_dns_operation *operation;
  struct ares_options options;
  struct ares_addrinfo_hints hints;
  int status;
  if (!load_cares_functions()) return 0;
  operation = (chezpp_dns_operation *)calloc(1, sizeof(*operation));
  if (operation == NULL) return 0;
  operation->query_name = strdup(name);
  operation->deferred_read_fd = -1;
  operation->deferred_write_fd = -1;
  if (operation->query_name == NULL) {
    free(operation);
    return 0;
  }
  memset(&options, 0, sizeof(options));
  options.timeout = timeout_ms;
  status = p_ares_init_options(&operation->channel, &options, ARES_OPT_TIMEOUTMS);
  if (status != ARES_SUCCESS) {
    free(operation->query_name);
    free(operation);
    return 0;
  }
  memset(&hints, 0, sizeof(hints));
  hints.ai_flags = ARES_AI_CANONNAME;
  hints.ai_family = family;
  p_ares_getaddrinfo(operation->channel, name, NULL, &hints, dns_callback, operation);
  if (operation->done) {
    int pipe_fd[2];
    unsigned char ready = 1;
    if (pipe(pipe_fd) == 0) {
      operation->deferred_read_fd = pipe_fd[0];
      operation->deferred_write_fd = pipe_fd[1];
      (void)fcntl(pipe_fd[0], F_SETFL, fcntl(pipe_fd[0], F_GETFL, 0) | O_NONBLOCK);
      (void)fcntl(pipe_fd[1], F_SETFL, fcntl(pipe_fd[1], F_GETFL, 0) | O_NONBLOCK);
      (void)write(pipe_fd[1], &ready, 1);
    }
  }
  return (uptr)operation;
}

ptr chezpp_net_dns_advance(uptr handle) {
  chezpp_dns_operation *operation = (chezpp_dns_operation *)handle;
  ares_socket_t sockets[ARES_GETSOCK_MAXNUM];
  struct pollfd pollfds[ARES_GETSOCK_MAXNUM];
  int bits;
  int count = 0;
  int index;
  int ready;
  if (operation == NULL || operation->channel == NULL)
    return make_status("error", Sstring("invalid c-ares operation"));
  if (operation->done && operation->deferred_read_fd >= 0) {
    if (!operation->deferred_reported) {
      operation->deferred_reported = 1;
      return deferred_pending_result(operation);
    }
    close(operation->deferred_read_fd);
    close(operation->deferred_write_fd);
    operation->deferred_read_fd = -1;
    operation->deferred_write_fd = -1;
  }
  if (operation->done) return dns_result(operation);

  bits = p_ares_getsock(operation->channel, sockets, ARES_GETSOCK_MAXNUM);
  memset(pollfds, 0, sizeof(pollfds));
  for (index = 0; index < ARES_GETSOCK_MAXNUM; index++) {
    if (!ARES_GETSOCK_READABLE(bits, index) && !ARES_GETSOCK_WRITABLE(bits, index)) continue;
    pollfds[count].fd = sockets[index];
    if (ARES_GETSOCK_READABLE(bits, index)) pollfds[count].events |= POLLIN;
    if (ARES_GETSOCK_WRITABLE(bits, index)) pollfds[count].events |= POLLOUT;
    count++;
  }
  ready = poll(pollfds, (nfds_t)count, 0);
  if (ready < 0) return make_status("error", Sstring(strerror(errno)));
  if (ready == 0) {
    p_ares_process_fd(operation->channel, ARES_SOCKET_BAD, ARES_SOCKET_BAD);
  } else {
    for (index = 0; index < count; index++) {
      ares_socket_t read_fd = (pollfds[index].revents & (POLLIN | POLLERR | POLLHUP))
                                  ? pollfds[index].fd
                                  : ARES_SOCKET_BAD;
      ares_socket_t write_fd = (pollfds[index].revents & POLLOUT)
                                   ? pollfds[index].fd
                                   : ARES_SOCKET_BAD;
      if (read_fd != ARES_SOCKET_BAD || write_fd != ARES_SOCKET_BAD)
        p_ares_process_fd(operation->channel, read_fd, write_fd);
    }
  }
  return operation->done ? dns_result(operation) : pending_result(operation);
}

ptr chezpp_net_dns_cancel(uptr handle) {
  chezpp_dns_operation *operation = (chezpp_dns_operation *)handle;
  if (operation != NULL && operation->channel != NULL) p_ares_cancel(operation->channel);
  return Strue;
}

void chezpp_net_dns_close(uptr handle) {
  chezpp_dns_operation *operation = (chezpp_dns_operation *)handle;
  if (operation == NULL) return;
  if (operation->channel != NULL) p_ares_destroy(operation->channel);
  if (operation->result != NULL) p_ares_freeaddrinfo(operation->result);
  if (operation->deferred_read_fd >= 0) close(operation->deferred_read_fd);
  if (operation->deferred_write_fd >= 0) close(operation->deferred_write_fd);
  free(operation->query_name);
  free(operation);
}
