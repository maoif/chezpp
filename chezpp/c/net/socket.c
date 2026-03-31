#include "../common.h"

#include <arpa/inet.h>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <sys/socket.h>
#include <sys/un.h>

#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0
#endif

#ifndef MSG_DONTWAIT
#define MSG_DONTWAIT 0
#endif

static ptr make_status(const char *tag, ptr value) {
  ptr v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, Sstring_to_symbol(tag));
  Svector_set(v, 1, value);
  return v;
}

static ptr make_errno_status(const char *tag) { return make_status(tag, errno_str()); }

static ptr make_addr(int family, const char *host, int port, const char *path) {
  ptr v = Smake_vector(4, Sfalse);
  switch (family) {
  case AF_INET:
    Svector_set(v, 0, Sstring_to_symbol("inet"));
    break;
  case AF_INET6:
    Svector_set(v, 0, Sstring_to_symbol("inet6"));
    break;
  case AF_UNIX:
    Svector_set(v, 0, Sstring_to_symbol("unix"));
    break;
  default:
    Svector_set(v, 0, Sfalse);
    break;
  }
  Svector_set(v, 1, host == NULL ? Sfalse : Sstring(host));
  Svector_set(v, 2, port < 0 ? Sfalse : Sfixnum(port));
  Svector_set(v, 3, path == NULL ? Sfalse : Sstring(path));
  return v;
}

static ptr make_addr_from_sockaddr(const struct sockaddr *addr, socklen_t addrlen) {
  char host[INET6_ADDRSTRLEN + 1];
  memset(host, 0, sizeof(host));

  if (addr->sa_family == AF_INET) {
    const struct sockaddr_in *sa = (const struct sockaddr_in *)addr;
    if (inet_ntop(AF_INET, &sa->sin_addr, host, sizeof(host)) == NULL) {
      return Sfalse;
    }
    return make_addr(AF_INET, host, ntohs(sa->sin_port), NULL);
  }

  if (addr->sa_family == AF_INET6) {
    const struct sockaddr_in6 *sa = (const struct sockaddr_in6 *)addr;
    if (inet_ntop(AF_INET6, &sa->sin6_addr, host, sizeof(host)) == NULL) {
      return Sfalse;
    }
    return make_addr(AF_INET6, host, ntohs(sa->sin6_port), NULL);
  }

  if (addr->sa_family == AF_UNIX) {
    const struct sockaddr_un *sa = (const struct sockaddr_un *)addr;
    if (addrlen <= offsetof(struct sockaddr_un, sun_path)) {
      return make_addr(AF_UNIX, NULL, -1, "");
    }
    return make_addr(AF_UNIX, NULL, -1, sa->sun_path);
  }

  return Sfalse;
}

static int fill_sockaddr(int family, const char *host, int port, const char *path,
                         int passive, struct sockaddr_storage *storage,
                         socklen_t *out_len) {
  if (family == AF_UNIX) {
    struct sockaddr_un *sa = (struct sockaddr_un *)storage;
    size_t n;
    if (path == NULL || path[0] == '\0') {
      errno = EINVAL;
      return -1;
    }
    memset(sa, 0, sizeof(*sa));
    sa->sun_family = AF_UNIX;
    n = strlen(path);
    if (n >= sizeof(sa->sun_path)) {
      errno = ENAMETOOLONG;
      return -1;
    }
    memcpy(sa->sun_path, path, n + 1);
    *out_len = (socklen_t)offsetof(struct sockaddr_un, sun_path) + (socklen_t)n + 1;
    return 0;
  }

  {
    struct addrinfo hints;
    struct addrinfo *result = NULL;
    struct addrinfo *ai = NULL;
    char service[32];
    int rc;

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = family;
    hints.ai_socktype = 0;
    hints.ai_flags = passive ? AI_PASSIVE : 0;

    if (port >= 0) {
      snprintf(service, sizeof(service), "%d", port);
    }

    rc = getaddrinfo((host != NULL && host[0] != '\0') ? host : NULL,
                     port >= 0 ? service : NULL, &hints, &result);
    if (rc != 0) {
      errno = EINVAL;
      return -1;
    }

    ai = result;
    memcpy(storage, ai->ai_addr, ai->ai_addrlen);
    *out_len = (socklen_t)ai->ai_addrlen;
    freeaddrinfo(result);
    return 0;
  }
}

static int would_block_errno(int e) { return e == EAGAIN || e == EWOULDBLOCK; }

int chezpp_net_af_inet() { return AF_INET; }
int chezpp_net_af_inet6() { return AF_INET6; }
int chezpp_net_af_unix() { return AF_UNIX; }

int chezpp_net_sock_stream() { return SOCK_STREAM; }
int chezpp_net_sock_datagram() { return SOCK_DGRAM; }
#ifdef SOCK_SEQPACKET
int chezpp_net_sock_seqpacket() { return SOCK_SEQPACKET; }
#else
int chezpp_net_sock_seqpacket() { return -1; }
#endif

int chezpp_net_shut_rd() { return SHUT_RD; }
int chezpp_net_shut_wr() { return SHUT_WR; }
int chezpp_net_shut_rdwr() { return SHUT_RDWR; }

ptr chezpp_net_socket_open(int family, int type, int proto) {
  int fd = socket(family, type, proto);
  if (fd < 0) return make_errno_status("error");
  return Sfixnum(fd);
}

ptr chezpp_net_socket_close(int fd) {
  if (close(fd) < 0) return make_errno_status("error");
  return Strue;
}

ptr chezpp_net_socket_dup(int fd) {
  int dupfd = dup(fd);
  if (dupfd < 0) return make_errno_status("error");
  return Sfixnum(dupfd);
}

ptr chezpp_net_socket_bind(int fd, int family, const char *host, int port, const char *path) {
  struct sockaddr_storage storage;
  socklen_t len;
  if (fill_sockaddr(family, host, port, path, 1, &storage, &len) != 0)
    return make_errno_status("error");
  if (bind(fd, (struct sockaddr *)&storage, len) < 0) return make_errno_status("error");
  return Strue;
}

ptr chezpp_net_socket_listen(int fd, int backlog) {
  if (listen(fd, backlog) < 0) return make_errno_status("error");
  return Strue;
}

ptr chezpp_net_socket_connect(int fd, int family, const char *host, int port, const char *path) {
  struct sockaddr_storage storage;
  socklen_t len;
  if (fill_sockaddr(family, host, port, path, 0, &storage, &len) != 0)
    return make_errno_status("error");
  if (connect(fd, (struct sockaddr *)&storage, len) < 0) {
    if (would_block_errno(errno)) return make_status("would-block", Sfalse);
    return make_errno_status("error");
  }
  return Strue;
}

ptr chezpp_net_socket_accept(int fd, int nonblocking) {
  struct sockaddr_storage storage;
  socklen_t len = sizeof(storage);
  int client_fd;
  ptr v;
  struct pollfd pfd;

  if (nonblocking) {
    memset(&pfd, 0, sizeof(pfd));
    pfd.fd = fd;
    pfd.events = POLLIN;
    client_fd = poll(&pfd, 1, 0);
    if (client_fd < 0) return make_errno_status("error");
    if (client_fd == 0) return make_status("would-block", Sfalse);
  }

  client_fd = accept(fd, (struct sockaddr *)&storage, &len);
  if (client_fd < 0) {
    if (would_block_errno(errno)) return make_status("would-block", Sfalse);
    return make_errno_status("error");
  }

  if (nonblocking) {
    int state = fcntl(client_fd, F_GETFL, 0);
    if (state >= 0) (void)fcntl(client_fd, F_SETFL, state | O_NONBLOCK);
  }

  v = Smake_vector(2, Sfalse);
  Svector_set(v, 0, Sfixnum(client_fd));
  Svector_set(v, 1, make_addr_from_sockaddr((struct sockaddr *)&storage, len));
  return v;
}

ptr chezpp_net_socket_shutdown(int fd, int how) {
  if (shutdown(fd, how) < 0) return make_errno_status("error");
  return Strue;
}

ptr chezpp_net_socket_send(int fd, ptr bv, int start, int stop, int nonblocking) {
  int flags = MSG_NOSIGNAL | (nonblocking ? MSG_DONTWAIT : 0);
  ssize_t n = send(fd, Sbytevector_data(bv) + start, (size_t)(stop - start), flags);
  if (n < 0) {
    if (would_block_errno(errno)) return make_status("would-block", Sfalse);
    return make_errno_status("error");
  }
  return Sfixnum((iptr)n);
}

ptr chezpp_net_socket_recv(int fd, int size, int nonblocking) {
  ptr bv;
  ssize_t n;
  if (size < 0) {
    errno = EINVAL;
    return make_errno_status("error");
  }
  bv = Smake_bytevector(size, 0);
  n = recv(fd, Sbytevector_data(bv), (size_t)size, nonblocking ? MSG_DONTWAIT : 0);
  if (n < 0) {
    if (would_block_errno(errno)) return make_status("would-block", Sfalse);
    return make_errno_status("error");
  }
  if (n == 0) return Seof_object;
  if (n == size) return bv;

  {
    ptr out = Smake_bytevector((iptr)n, 0);
    memcpy(Sbytevector_data(out), Sbytevector_data(bv), (size_t)n);
    return out;
  }
}

ptr chezpp_net_socket_recv_into(int fd, ptr bv, int start, int stop, int nonblocking) {
  ssize_t n = recv(fd, Sbytevector_data(bv) + start, (size_t)(stop - start),
                   nonblocking ? MSG_DONTWAIT : 0);
  if (n < 0) {
    if (would_block_errno(errno)) return make_status("would-block", Sfalse);
    return make_errno_status("error");
  }
  if (n == 0) return Seof_object;
  return Sfixnum((iptr)n);
}

ptr chezpp_net_socket_local_address(int fd) {
  struct sockaddr_storage storage;
  socklen_t len = sizeof(storage);
  if (getsockname(fd, (struct sockaddr *)&storage, &len) < 0) return make_errno_status("error");
  return make_addr_from_sockaddr((struct sockaddr *)&storage, len);
}

ptr chezpp_net_socket_peer_address(int fd) {
  struct sockaddr_storage storage;
  socklen_t len = sizeof(storage);
  if (getpeername(fd, (struct sockaddr *)&storage, &len) < 0) return make_errno_status("error");
  return make_addr_from_sockaddr((struct sockaddr *)&storage, len);
}

ptr chezpp_net_socket_set_blocking(int fd, int blocking) {
  int state = fcntl(fd, F_GETFL, 0);
  if (state < 0) return make_errno_status("error");
  if (blocking)
    state &= ~O_NONBLOCK;
  else
    state |= O_NONBLOCK;
  if (fcntl(fd, F_SETFL, state) < 0) return make_errno_status("error");
  return Sboolean(blocking);
}

ptr chezpp_net_socket_get_blocking(int fd) {
  int state = fcntl(fd, F_GETFL, 0);
  if (state < 0) return make_errno_status("error");
  return Sboolean((state & O_NONBLOCK) == 0);
}

ptr chezpp_net_socket_wait(int fd, int events, int timeout_ms) {
  struct pollfd pfd;
  int rc;
  memset(&pfd, 0, sizeof(pfd));
  pfd.fd = fd;
  pfd.events = (short)events;
  rc = poll(&pfd, 1, timeout_ms);
  if (rc < 0) return make_errno_status("error");
  if (rc == 0) return Sfalse;
  return Sfixnum((iptr)pfd.revents);
}

ptr chezpp_net_socket_set_option(int fd, const char *name, ptr value) {
  int level = SOL_SOCKET;
  int optname = 0;
  int ivalue = 0;
  socklen_t len = sizeof(ivalue);

  if (strcmp(name, "reuse-address") == 0) {
    optname = SO_REUSEADDR;
    ivalue = Sboolean_value(value) ? 1 : 0;
  } else if (strcmp(name, "keepalive") == 0) {
    optname = SO_KEEPALIVE;
    ivalue = Sboolean_value(value) ? 1 : 0;
  } else if (strcmp(name, "broadcast") == 0) {
    optname = SO_BROADCAST;
    ivalue = Sboolean_value(value) ? 1 : 0;
  } else if (strcmp(name, "tcp-nodelay") == 0) {
    level = IPPROTO_TCP;
    optname = TCP_NODELAY;
    ivalue = Sboolean_value(value) ? 1 : 0;
  } else if (strcmp(name, "recv-buffer") == 0) {
    optname = SO_RCVBUF;
    ivalue = Sfixnum_value(value);
  } else if (strcmp(name, "send-buffer") == 0) {
    optname = SO_SNDBUF;
    ivalue = Sfixnum_value(value);
  } else {
    errno = EINVAL;
    return make_errno_status("error");
  }

  if (setsockopt(fd, level, optname, &ivalue, len) < 0) return make_errno_status("error");
  return Strue;
}

ptr chezpp_net_socket_get_option(int fd, const char *name) {
  int level = SOL_SOCKET;
  int optname = 0;
  int ivalue = 0;
  socklen_t len = sizeof(ivalue);

  if (strcmp(name, "reuse-address") == 0) {
    optname = SO_REUSEADDR;
  } else if (strcmp(name, "keepalive") == 0) {
    optname = SO_KEEPALIVE;
  } else if (strcmp(name, "broadcast") == 0) {
    optname = SO_BROADCAST;
  } else if (strcmp(name, "tcp-nodelay") == 0) {
    level = IPPROTO_TCP;
    optname = TCP_NODELAY;
  } else if (strcmp(name, "recv-buffer") == 0) {
    optname = SO_RCVBUF;
  } else if (strcmp(name, "send-buffer") == 0) {
    optname = SO_SNDBUF;
  } else {
    errno = EINVAL;
    return make_errno_status("error");
  }

  if (getsockopt(fd, level, optname, &ivalue, &len) < 0) return make_errno_status("error");

  if (strcmp(name, "recv-buffer") == 0 || strcmp(name, "send-buffer") == 0)
    return Sfixnum(ivalue);
  return Sboolean(ivalue != 0);
}

ptr chezpp_net_resolve_addresses(const char *host, int port, int family, int type) {
  struct addrinfo hints;
  struct addrinfo *result = NULL;
  struct addrinfo *ai = NULL;
  ptr head = Snil;
  ptr tail = Snil;
  ptr out;
  char service[32];
  int rc;

  memset(&hints, 0, sizeof(hints));
  hints.ai_family = family;
  hints.ai_socktype = type;
  hints.ai_flags = AI_CANONNAME;
  if (port >= 0) snprintf(service, sizeof(service), "%d", port);

  rc = getaddrinfo((host != NULL && host[0] != '\0') ? host : NULL, port >= 0 ? service : NULL,
                   &hints, &result);
  if (rc != 0) return make_status("error", Sstring(gai_strerror(rc)));

  for (ai = result; ai != NULL; ai = ai->ai_next) {
    ptr addr = make_addr_from_sockaddr(ai->ai_addr, (socklen_t)ai->ai_addrlen);
    ptr cell = Scons(addr, Snil);
    if (head == Snil) {
      head = cell;
      tail = cell;
    } else {
      Scdr(tail) = cell;
      tail = cell;
    }
  }

  out = Smake_vector(2, Sfalse);
  Svector_set(out, 0, (result != NULL && result->ai_canonname != NULL) ? Sstring(result->ai_canonname) : Sfalse);
  Svector_set(out, 1, head);
  freeaddrinfo(result);
  return out;
}

ptr chezpp_net_address_to_name(int family, const char *host, int port, const char *path) {
  struct sockaddr_storage storage;
  socklen_t len;
  char name[NI_MAXHOST + 1];
  int rc;
  if (fill_sockaddr(family, host, port, path, 0, &storage, &len) != 0)
    return make_errno_status("error");
  memset(name, 0, sizeof(name));
  rc = getnameinfo((struct sockaddr *)&storage, len, name, sizeof(name), NULL, 0, NI_NAMEREQD);
  if (rc != 0) return make_status("error", Sstring(gai_strerror(rc)));
  return Sstring(name);
}
