(library (chezpp net ffi)
  (export net-af-inet
          net-af-inet6
          net-af-unix
          net-pollin
          net-pollout
          net-pollpri
          net-pollerr
          net-pollhup
          net-pollnval
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
          ffi-net-poll
          ffi-net-resolve-addresses
          ffi-net-address->name
          ffi-net-ftp-list
          ffi-net-ftp-download
          ffi-net-ftp-upload
          ffi-net-ftp-command
          ffi-net-ftp-rename
          ffi-net-ssh-open
          ffi-net-ssh-close
          ffi-net-ssh-auth-password
          ffi-net-ssh-auth-publickey-auto
          ffi-net-ssh-auth-agent
          ffi-net-ssh-channel-open
          ffi-net-ssh-channel-close
          ffi-net-ssh-channel-request-exec
          ffi-net-ssh-channel-request-shell
          ffi-net-ssh-channel-request-pty
          ffi-net-ssh-channel-read
          ffi-net-ssh-channel-read-into
          ffi-net-ssh-channel-write
          ffi-net-ssh-channel-exit-status
          ffi-net-sftp-open
          ffi-net-sftp-close
          ffi-net-sftp-list
          ffi-net-sftp-stat
          ffi-net-sftp-delete
          ffi-net-sftp-mkdir
          ffi-net-sftp-rmdir
          ffi-net-sftp-rename
          ffi-net-sftp-open-file
          ffi-net-sftp-close-file
          ffi-net-sftp-read
          ffi-net-sftp-read-into
          ffi-net-sftp-write
          ffi-net-websocket-listen
          ffi-net-websocket-server-close
          ffi-net-websocket-accept
          ffi-net-websocket-connect
          ffi-net-websocket-close
          ffi-net-websocket-send
          ffi-net-websocket-recv
          ffi-net-grpc-channel-open
          ffi-net-grpc-channel-close
          ffi-net-grpc-server-open
          ffi-net-grpc-server-close
          ffi-net-grpc-unary-call
          ffi-net-grpc-unary-start
          ffi-net-grpc-unary-poll
          ffi-net-grpc-unary-close
          ffi-net-grpc-server-request
          ffi-net-grpc-server-respond
          ffi-net-open-rdonly
          ffi-net-open-wronly
          ffi-net-open-rdwr
          ffi-net-open-append
          ffi-net-open-creat
          ffi-net-open-trunc
          ffi-net-open-excl
          ffi-net-open-text
          ffi-net-tls-context-create
          ffi-net-tls-context-free
          ffi-net-tls-context-load-ca-file
          ffi-net-tls-context-load-ca-path
          ffi-net-tls-context-load-cert-file
          ffi-net-tls-context-load-cert-bytes
          ffi-net-tls-context-load-key-file
          ffi-net-tls-context-load-key-bytes
          ffi-net-tls-context-check-key
          ffi-net-tls-context-set-verify
          ffi-net-tls-context-set-alpn
          ffi-net-tls-connect
          ffi-net-tls-accept
          ffi-net-tls-close
          ffi-net-tls-read
          ffi-net-tls-read-into
          ffi-net-tls-write
          ffi-net-tls-shutdown
          ffi-net-tls-protocol-version
          ffi-net-tls-cipher-name
          ffi-net-tls-verified
          ffi-net-tls-peer-certificate-der
          ffi-net-tls-peer-certificate-chain-der)
  (import (chezpp chez)
          (chezpp internal))

  (define net-af-inet (foreign-procedure "chezpp_net_af_inet" () int))
  (define net-af-inet6 (foreign-procedure "chezpp_net_af_inet6" () int))
  (define net-af-unix (foreign-procedure "chezpp_net_af_unix" () int))
  (define net-pollin (foreign-procedure "chezpp_net_pollin" () int))
  (define net-pollout (foreign-procedure "chezpp_net_pollout" () int))
  (define net-pollpri (foreign-procedure "chezpp_net_pollpri" () int))
  (define net-pollerr (foreign-procedure "chezpp_net_pollerr" () int))
  (define net-pollhup (foreign-procedure "chezpp_net_pollhup" () int))
  (define net-pollnval (foreign-procedure "chezpp_net_pollnval" () int))

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
  (define ffi-net-poll
    (foreign-procedure "chezpp_net_poll" (scheme-object int) scheme-object))
  (define ffi-net-resolve-addresses
    (foreign-procedure "chezpp_net_resolve_addresses" (string int int int) scheme-object))
  (define ffi-net-address->name
    (foreign-procedure "chezpp_net_address_to_name" (int string int string) scheme-object))
  (define ffi-net-ftp-list
    (foreign-procedure "chezpp_net_ftp_list"
                       (string string string int int int int int)
                       scheme-object))
  (define ffi-net-ftp-download
    (foreign-procedure "chezpp_net_ftp_download"
                       (string string string string int int int int int)
                       scheme-object))
  (define ffi-net-ftp-upload
    (foreign-procedure "chezpp_net_ftp_upload"
                       (string string string string int int int int int)
                       scheme-object))
  (define ffi-net-ftp-command
    (foreign-procedure "chezpp_net_ftp_command"
                       (string string string int int int int int string)
                       scheme-object))
  (define ffi-net-ftp-rename
    (foreign-procedure "chezpp_net_ftp_rename"
                       (string string string int int int int int string string)
                       scheme-object))
  (define ffi-net-ssh-open
    (foreign-procedure "chezpp_net_ssh_open" (string int string int) scheme-object))
  (define ffi-net-ssh-close
    (foreign-procedure "chezpp_net_ssh_close" (uptr) scheme-object))
  (define ffi-net-ssh-auth-password
    (foreign-procedure "chezpp_net_ssh_auth_password" (uptr string string) scheme-object))
  (define ffi-net-ssh-auth-publickey-auto
    (foreign-procedure "chezpp_net_ssh_auth_publickey_auto" (uptr string string) scheme-object))
  (define ffi-net-ssh-auth-agent
    (foreign-procedure "chezpp_net_ssh_auth_agent" (uptr string) scheme-object))
  (define ffi-net-ssh-channel-open
    (foreign-procedure "chezpp_net_ssh_channel_open" (uptr) scheme-object))
  (define ffi-net-ssh-channel-close
    (foreign-procedure "chezpp_net_ssh_channel_close" (uptr) scheme-object))
  (define ffi-net-ssh-channel-request-exec
    (foreign-procedure "chezpp_net_ssh_channel_request_exec" (uptr string) scheme-object))
  (define ffi-net-ssh-channel-request-shell
    (foreign-procedure "chezpp_net_ssh_channel_request_shell" (uptr) scheme-object))
  (define ffi-net-ssh-channel-request-pty
    (foreign-procedure "chezpp_net_ssh_channel_request_pty" (uptr) scheme-object))
  (define ffi-net-ssh-channel-read
    (foreign-procedure "chezpp_net_ssh_channel_read" (uptr int int int) scheme-object))
  (define ffi-net-ssh-channel-read-into
    (foreign-procedure "chezpp_net_ssh_channel_read_into" (uptr ptr int int int int) scheme-object))
  (define ffi-net-ssh-channel-write
    (foreign-procedure "chezpp_net_ssh_channel_write" (uptr ptr int int int) scheme-object))
  (define ffi-net-ssh-channel-exit-status
    (foreign-procedure "chezpp_net_ssh_channel_exit_status" (uptr) scheme-object))
  (define ffi-net-sftp-open
    (foreign-procedure "chezpp_net_sftp_open" (uptr) scheme-object))
  (define ffi-net-sftp-close
    (foreign-procedure "chezpp_net_sftp_close" (uptr) scheme-object))
  (define ffi-net-sftp-list
    (foreign-procedure "chezpp_net_sftp_list" (uptr string) scheme-object))
  (define ffi-net-sftp-stat
    (foreign-procedure "chezpp_net_sftp_stat" (uptr string) scheme-object))
  (define ffi-net-sftp-delete
    (foreign-procedure "chezpp_net_sftp_delete" (uptr string) scheme-object))
  (define ffi-net-sftp-mkdir
    (foreign-procedure "chezpp_net_sftp_mkdir" (uptr string int) scheme-object))
  (define ffi-net-sftp-rmdir
    (foreign-procedure "chezpp_net_sftp_rmdir" (uptr string) scheme-object))
  (define ffi-net-sftp-rename
    (foreign-procedure "chezpp_net_sftp_rename" (uptr string string) scheme-object))
  (define ffi-net-sftp-open-file
    (foreign-procedure "chezpp_net_sftp_open_file" (uptr string int int) scheme-object))
  (define ffi-net-sftp-close-file
    (foreign-procedure "chezpp_net_sftp_close_file" (uptr) scheme-object))
  (define ffi-net-sftp-read
    (foreign-procedure "chezpp_net_sftp_read" (uptr int int) scheme-object))
  (define ffi-net-sftp-read-into
    (foreign-procedure "chezpp_net_sftp_read_into" (uptr ptr int int int) scheme-object))
  (define ffi-net-sftp-write
    (foreign-procedure "chezpp_net_sftp_write" (uptr ptr int int int) scheme-object))
  (define ffi-net-websocket-listen
    (foreign-procedure "chezpp_net_websocket_listen" (string int string) scheme-object))
  (define ffi-net-websocket-server-close
    (foreign-procedure "chezpp_net_websocket_server_close" (uptr) scheme-object))
  (define ffi-net-websocket-accept
    (foreign-procedure "chezpp_net_websocket_accept" (uptr int) scheme-object))
  (define ffi-net-websocket-connect
    (foreign-procedure "chezpp_net_websocket_connect" (string int string string int) scheme-object))
  (define ffi-net-websocket-close
    (foreign-procedure "chezpp_net_websocket_close" (uptr) scheme-object))
  (define ffi-net-websocket-send
    (foreign-procedure "chezpp_net_websocket_send" (uptr int ptr int int int) scheme-object))
  (define ffi-net-websocket-recv
    (foreign-procedure "chezpp_net_websocket_recv" (uptr int) scheme-object))
  (define ffi-net-grpc-channel-open
    (foreign-procedure "chezpp_net_grpc_channel_open" (string) scheme-object))
  (define ffi-net-grpc-channel-close
    (foreign-procedure "chezpp_net_grpc_channel_close" (uptr) scheme-object))
  (define ffi-net-grpc-server-open
    (foreign-procedure "chezpp_net_grpc_server_open" (string int) scheme-object))
  (define ffi-net-grpc-server-close
    (foreign-procedure "chezpp_net_grpc_server_close" (uptr) scheme-object))
  (define ffi-net-grpc-unary-call
    (foreign-procedure "chezpp_net_grpc_unary_call"
                       (uptr string ptr int int scheme-object int)
                       scheme-object))
  (define ffi-net-grpc-unary-start
    (foreign-procedure "chezpp_net_grpc_unary_start"
                       (uptr string ptr int int scheme-object int)
                       scheme-object))
  (define ffi-net-grpc-unary-poll
    (foreign-procedure "chezpp_net_grpc_unary_poll" (uptr) scheme-object))
  (define ffi-net-grpc-unary-close
    (foreign-procedure "chezpp_net_grpc_unary_close" (uptr) scheme-object))
  (define ffi-net-grpc-server-request
    (foreign-procedure "chezpp_net_grpc_server_request" (uptr) scheme-object))
  (define ffi-net-grpc-server-respond
    (foreign-procedure "chezpp_net_grpc_server_respond"
                       (uptr ptr int int int string scheme-object)
                       scheme-object))
  (define ffi-net-open-rdonly
    (foreign-procedure "chezpp_net_open_rdonly" () int))
  (define ffi-net-open-wronly
    (foreign-procedure "chezpp_net_open_wronly" () int))
  (define ffi-net-open-rdwr
    (foreign-procedure "chezpp_net_open_rdwr" () int))
  (define ffi-net-open-append
    (foreign-procedure "chezpp_net_open_append" () int))
  (define ffi-net-open-creat
    (foreign-procedure "chezpp_net_open_creat" () int))
  (define ffi-net-open-trunc
    (foreign-procedure "chezpp_net_open_trunc" () int))
  (define ffi-net-open-excl
    (foreign-procedure "chezpp_net_open_excl" () int))
  (define ffi-net-open-text
    (foreign-procedure "chezpp_net_open_text" () int))
  (define ffi-net-tls-context-create
    (foreign-procedure "chezpp_net_tls_context_create" (int) uptr))
  (define ffi-net-tls-context-free
    (foreign-procedure "chezpp_net_tls_context_free" (uptr) void))
  (define ffi-net-tls-context-load-ca-file
    (foreign-procedure "chezpp_net_tls_context_load_ca_file" (uptr string) scheme-object))
  (define ffi-net-tls-context-load-ca-path
    (foreign-procedure "chezpp_net_tls_context_load_ca_path" (uptr string) scheme-object))
  (define ffi-net-tls-context-load-cert-file
    (foreign-procedure "chezpp_net_tls_context_load_cert_file" (uptr string int) scheme-object))
  (define ffi-net-tls-context-load-cert-bytes
    (foreign-procedure "chezpp_net_tls_context_load_cert_bytes" (uptr ptr int int int) scheme-object))
  (define ffi-net-tls-context-load-key-file
    (foreign-procedure "chezpp_net_tls_context_load_key_file" (uptr string int) scheme-object))
  (define ffi-net-tls-context-load-key-bytes
    (foreign-procedure "chezpp_net_tls_context_load_key_bytes" (uptr ptr int int int) scheme-object))
  (define ffi-net-tls-context-check-key
    (foreign-procedure "chezpp_net_tls_context_check_key" (uptr) scheme-object))
  (define ffi-net-tls-context-set-verify
    (foreign-procedure "chezpp_net_tls_context_set_verify" (uptr int) scheme-object))
  (define ffi-net-tls-context-set-alpn
    (foreign-procedure "chezpp_net_tls_context_set_alpn" (uptr ptr int int) scheme-object))
  (define ffi-net-tls-connect
    (foreign-procedure "chezpp_net_tls_connect" (uptr int string) scheme-object))
  (define ffi-net-tls-accept
    (foreign-procedure "chezpp_net_tls_accept" (uptr int) scheme-object))
  (define ffi-net-tls-close
    (foreign-procedure "chezpp_net_tls_close" (uptr) scheme-object))
  (define ffi-net-tls-read
    (foreign-procedure "chezpp_net_tls_read" (uptr int int) scheme-object))
  (define ffi-net-tls-read-into
    (foreign-procedure "chezpp_net_tls_read_into" (uptr ptr int int int) scheme-object))
  (define ffi-net-tls-write
    (foreign-procedure "chezpp_net_tls_write" (uptr ptr int int int) scheme-object))
  (define ffi-net-tls-shutdown
    (foreign-procedure "chezpp_net_tls_shutdown" (uptr) scheme-object))
  (define ffi-net-tls-protocol-version
    (foreign-procedure "chezpp_net_tls_protocol_version" (uptr) scheme-object))
  (define ffi-net-tls-cipher-name
    (foreign-procedure "chezpp_net_tls_cipher_name" (uptr) scheme-object))
  (define ffi-net-tls-verified
    (foreign-procedure "chezpp_net_tls_verified" (uptr) scheme-object))
  (define ffi-net-tls-peer-certificate-der
    (foreign-procedure "chezpp_net_tls_peer_certificate_der" (uptr) scheme-object))
  (define ffi-net-tls-peer-certificate-chain-der
    (foreign-procedure "chezpp_net_tls_peer_certificate_chain_der" (uptr) scheme-object))
  )
