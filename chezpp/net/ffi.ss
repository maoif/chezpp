(library (chezpp net ffi)
  (export net-af-inet
          net-af-inet6
          net-af-unix
          net-sock-stream
          net-sock-datagram
          net-sock-seqpacket
          net-shut-read
          net-shut-write
          net-shut-read/write
          ffi-net-socket-open
          ffi-net-socket-close
          ffi-net-socket-dup
          ffi-net-socket-bind
          ffi-net-socket-listen
          ffi-net-socket-connect
          ffi-net-socket-accept
          ffi-net-socket-shutdown
          ffi-net-socket-send
          ffi-net-socket-recv
          ffi-net-socket-recv-into
          ffi-net-socket-local-address
          ffi-net-socket-peer-address
          ffi-net-socket-set-blocking
          ffi-net-socket-get-blocking
          ffi-net-socket-wait
          ffi-net-socket-set-option
          ffi-net-socket-get-option
          ffi-net-resolve-addresses
          ffi-net-address->name)
  (import (chezpp chez)
          (chezpp internal))

  (define net-af-inet (foreign-procedure "chezpp_net_af_inet" () int))
  (define net-af-inet6 (foreign-procedure "chezpp_net_af_inet6" () int))
  (define net-af-unix (foreign-procedure "chezpp_net_af_unix" () int))

  (define net-sock-stream (foreign-procedure "chezpp_net_sock_stream" () int))
  (define net-sock-datagram (foreign-procedure "chezpp_net_sock_datagram" () int))
  (define net-sock-seqpacket (foreign-procedure "chezpp_net_sock_seqpacket" () int))

  (define net-shut-read (foreign-procedure "chezpp_net_shut_rd" () int))
  (define net-shut-write (foreign-procedure "chezpp_net_shut_wr" () int))
  (define net-shut-read/write (foreign-procedure "chezpp_net_shut_rdwr" () int))

  (define ffi-net-socket-open
    (foreign-procedure "chezpp_net_socket_open" (int int int) scheme-object))
  (define ffi-net-socket-close
    (foreign-procedure "chezpp_net_socket_close" (int) scheme-object))
  (define ffi-net-socket-dup
    (foreign-procedure "chezpp_net_socket_dup" (int) scheme-object))
  (define ffi-net-socket-bind
    (foreign-procedure "chezpp_net_socket_bind" (int int string int string) scheme-object))
  (define ffi-net-socket-listen
    (foreign-procedure "chezpp_net_socket_listen" (int int) scheme-object))
  (define ffi-net-socket-connect
    (foreign-procedure "chezpp_net_socket_connect" (int int string int string) scheme-object))
  (define ffi-net-socket-accept
    (foreign-procedure "chezpp_net_socket_accept" (int int) scheme-object))
  (define ffi-net-socket-shutdown
    (foreign-procedure "chezpp_net_socket_shutdown" (int int) scheme-object))
  (define ffi-net-socket-send
    (foreign-procedure "chezpp_net_socket_send" (int ptr int int int) scheme-object))
  (define ffi-net-socket-recv
    (foreign-procedure "chezpp_net_socket_recv" (int int int) scheme-object))
  (define ffi-net-socket-recv-into
    (foreign-procedure "chezpp_net_socket_recv_into" (int ptr int int int) scheme-object))
  (define ffi-net-socket-local-address
    (foreign-procedure "chezpp_net_socket_local_address" (int) scheme-object))
  (define ffi-net-socket-peer-address
    (foreign-procedure "chezpp_net_socket_peer_address" (int) scheme-object))
  (define ffi-net-socket-set-blocking
    (foreign-procedure "chezpp_net_socket_set_blocking" (int int) scheme-object))
  (define ffi-net-socket-get-blocking
    (foreign-procedure "chezpp_net_socket_get_blocking" (int) scheme-object))
  (define ffi-net-socket-wait
    (foreign-procedure "chezpp_net_socket_wait" (int int int) scheme-object))
  (define ffi-net-socket-set-option
    (foreign-procedure "chezpp_net_socket_set_option" (int string scheme-object) scheme-object))
  (define ffi-net-socket-get-option
    (foreign-procedure "chezpp_net_socket_get_option" (int string) scheme-object))
  (define ffi-net-resolve-addresses
    (foreign-procedure "chezpp_net_resolve_addresses" (string int int int) scheme-object))
  (define ffi-net-address->name
    (foreign-procedure "chezpp_net_address_to_name" (int string int string) scheme-object))
  )
