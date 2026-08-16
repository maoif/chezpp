(library (chezpp net ffi)
  (export net-af-inet
          ffi-optional-library-info
          ffi-zlib-stream-open
          ffi-zlib-stream-process
          ffi-zlib-stream-close
          ffi-net-http2-open
          ffi-net-http2-close
          ffi-net-http2-submit-request
          ffi-net-http2-submit-response
          ffi-net-http2-mem-send
          ffi-net-http2-mem-recv
          ffi-net-http2-next-event
          ffi-net-http2-consume
          ffi-net-http2-rst
          ffi-net-http2-goaway
          ffi-net-http2-want-read
          ffi-net-http2-want-write
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
          ffi-net-socket-connect-status
          ffi-net-socket-accept
          ffi-net-socket-shutdown
          ffi-net-socket-send
          ffi-net-socket-recv
          ffi-net-socket-recv-into
          ffi-net-socket-send-to
          ffi-net-socket-recv-from
          ffi-net-socket-recv-from-into
          ffi-net-socket-local-address
          ffi-net-socket-peer-address
          ffi-net-socket-set-blocking
          ffi-net-socket-get-blocking
          ffi-net-socket-wait
          ffi-net-socket-set-option
          ffi-net-socket-get-option
          ffi-net-poll
          ffi-net-resolve-addresses
          ffi-net-service->port
          ffi-net-dns-start
          ffi-net-dns-advance
          ffi-net-dns-cancel
          ffi-net-dns-close
          ffi-net-address->name
          ffi-net-ftp-list
          ffi-net-ftp-stat
          ffi-net-ftp-download
          ffi-net-ftp-upload
          ffi-net-ftp-command
          ffi-net-ftp-rename
          ffi-net-ftp-transfer-start
          ffi-net-ftp-transfer-step
          ffi-net-ftp-transfer-cancel
          ffi-net-ftp-transfer-close
          ffi-net-ftp-session-open
          ffi-net-ftp-session-close
          ffi-net-ftp-file-open
          ffi-net-ftp-file-step
          ffi-net-ftp-file-read
          ffi-net-ftp-file-read-into
          ffi-net-ftp-file-write
          ffi-net-ftp-file-finish
          ffi-net-ftp-file-cancel
          ffi-net-ftp-file-close
          ffi-net-ssh-open
          ffi-net-ssh-close
          ffi-net-ssh-session-fd
          ffi-net-ssh-auth-password
          ffi-net-ssh-auth-publickey-auto
          ffi-net-ssh-auth-publickey
          ffi-net-ssh-auth-keyboard-interactive-step
          ffi-net-ssh-auth-keyboard-interactive-answer
          ffi-net-ssh-auth-agent
          ffi-net-ssh-auth-agent-identity
          ffi-net-ssh-known-host-check
          ffi-net-ssh-known-host-update
          ffi-net-ssh-known-host-export
          ffi-net-ssh-channel-open
          ffi-net-ssh-channel-open-forward
          ffi-net-ssh-remote-forward-listen
          ffi-net-ssh-remote-forward-accept
          ffi-net-ssh-remote-forward-cancel
          ffi-net-ssh-channel-close
          ffi-net-ssh-channel-request-exec
          ffi-net-ssh-channel-request-shell
          ffi-net-ssh-channel-request-pty
          ffi-net-ssh-channel-request-environment
          ffi-net-ssh-channel-request-subsystem
          ffi-net-ssh-channel-read
          ffi-net-ssh-channel-read-into
          ffi-net-ssh-channel-write
          ffi-net-ssh-channel-exit-status
          ffi-net-sftp-open
          ffi-net-sftp-close
          ffi-net-scp-download-file
          ffi-net-scp-upload-file
          ffi-net-scp-stat
          ffi-net-scp-download-directory
          ffi-net-scp-upload-directory
          ffi-net-scp-transfer-start
          ffi-net-scp-transfer-step
          ffi-net-scp-transfer-cancel
          ffi-net-scp-transfer-close
          ffi-net-sftp-list
          ffi-net-sftp-stat
          ffi-net-sftp-open-directory
          ffi-net-sftp-read-directory
          ffi-net-sftp-close-directory
          ffi-net-sftp-chmod
          ffi-net-sftp-chown
          ffi-net-sftp-utimes
          ffi-net-sftp-symlink
          ffi-net-sftp-readlink
          ffi-net-sftp-seek
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
          ffi-net-websocket-connect-step
          ffi-net-websocket-close
          ffi-net-websocket-close-with-reason
          ffi-net-websocket-state
          ffi-net-websocket-cancel-send
          ffi-net-websocket-send
          ffi-net-websocket-send-fragment
          ffi-net-websocket-recv
          ffi-net-grpc-channel-open
          ffi-net-grpc-channel-open-tls
          ffi-net-grpc-channel-close
          ffi-net-grpc-server-open
          ffi-net-grpc-server-open-tls
          ffi-net-grpc-server-close
          ffi-net-grpc-unary-call
          ffi-net-grpc-unary-start
          ffi-net-grpc-unary-poll
          ffi-net-grpc-unary-close
          ffi-net-grpc-stream-open
          ffi-net-grpc-stream-open-start
          ffi-net-grpc-stream-open-poll
          ffi-net-grpc-stream-send
          ffi-net-grpc-stream-recv
          ffi-net-grpc-stream-close-send
          ffi-net-grpc-stream-finish
          ffi-net-grpc-stream-close
          ffi-net-grpc-server-request
          ffi-net-grpc-server-request-stream
          ffi-net-grpc-server-respond
          ffi-net-grpc-driver-fd
          ffi-net-grpc-driver-drain
          ffi-net-sftp-flag-read
          ffi-net-sftp-flag-write
          ffi-net-sftp-flag-read/write
          ffi-net-sftp-flag-append
          ffi-net-sftp-flag-create
          ffi-net-sftp-flag-truncate
          ffi-net-sftp-flag-exclusive
          ffi-net-sftp-flag-text
          ffi-net-tls-load-error
          ffi-net-tls-context-create
          ffi-net-tls-context-free
          ffi-net-tls-context-load-ca-file
          ffi-net-tls-context-load-ca-path
          ffi-net-tls-context-load-default-ca
          ffi-net-tls-context-load-cert-file
          ffi-net-tls-context-load-cert-bytes
          ffi-net-tls-context-load-key-file
          ffi-net-tls-context-load-key-bytes
          ffi-net-tls-context-check-key
          ffi-net-tls-context-set-verify
          ffi-net-tls-context-set-alpn
          ffi-net-tls-connect
          ffi-net-tls-accept
          ffi-net-tls-handshake-step
          ffi-net-tls-close
          ffi-net-tls-read
          ffi-net-tls-read-into
          ffi-net-tls-write
          ffi-net-tls-shutdown
          ffi-net-tls-protocol-version
          ffi-net-tls-negotiated-alpn
          ffi-net-tls-cipher-name
          ffi-net-tls-verified
          ffi-net-tls-peer-certificate-der
          ffi-net-tls-peer-certificate-chain-der)
  (import (chezpp chez)
          (chezpp internal))

  (define ffi-optional-library-info
    (foreign-procedure "chezpp_optional_library_info" (string) scheme-object))
  (define ffi-zlib-stream-open
    (foreign-procedure "chezpp_zlib_stream_open" (int int) uptr))
  (define ffi-zlib-stream-process
    (foreign-procedure "chezpp_zlib_stream_process"
                       (uptr scheme-object int int int int) scheme-object))
  (define ffi-zlib-stream-close
    (foreign-procedure "chezpp_zlib_stream_close" (uptr) void))
  (define ffi-net-http2-open
    (foreign-procedure "chezpp_net_http2_open" (int) scheme-object))
  (define ffi-net-http2-close
    (foreign-procedure "chezpp_net_http2_close" (uptr) scheme-object))
  (define ffi-net-http2-submit-request
    (foreign-procedure "chezpp_net_http2_submit_request" (uptr scheme-object scheme-object)
                       scheme-object))
  (define ffi-net-http2-submit-response
    (foreign-procedure "chezpp_net_http2_submit_response"
                       (uptr int scheme-object scheme-object) scheme-object))
  (define ffi-net-http2-mem-send
    (foreign-procedure "chezpp_net_http2_mem_send" (uptr) scheme-object))
  (define ffi-net-http2-mem-recv
    (foreign-procedure "chezpp_net_http2_mem_recv"
                       (uptr scheme-object int int) scheme-object))
  (define ffi-net-http2-next-event
    (foreign-procedure "chezpp_net_http2_next_event" (uptr) scheme-object))
  (define ffi-net-http2-consume
    (foreign-procedure "chezpp_net_http2_consume" (uptr int int) scheme-object))
  (define ffi-net-http2-rst
    (foreign-procedure "chezpp_net_http2_rst" (uptr int int) scheme-object))
  (define ffi-net-http2-goaway
    (foreign-procedure "chezpp_net_http2_goaway" (uptr int int) scheme-object))
  (define ffi-net-http2-want-read
    (foreign-procedure "chezpp_net_http2_want_read" (uptr) scheme-object))
  (define ffi-net-http2-want-write
    (foreign-procedure "chezpp_net_http2_want_write" (uptr) scheme-object))

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
  (define ffi-net-socket-connect-status
    (foreign-procedure "chezpp_net_socket_connect_status" (int) scheme-object))
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
  (define ffi-net-socket-send-to
    (foreign-procedure "chezpp_net_socket_send_to"
                       (int ptr int int int string int string int) scheme-object))
  (define ffi-net-socket-recv-from
    (foreign-procedure "chezpp_net_socket_recv_from" (int int int) scheme-object))
  (define ffi-net-socket-recv-from-into
    (foreign-procedure "chezpp_net_socket_recv_from_into"
                       (int ptr int int int) scheme-object))
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
  (define ffi-net-service->port
    (foreign-procedure "chezpp_net_service_to_port" (string int) scheme-object))
  (define ffi-net-dns-start
    (foreign-procedure "chezpp_net_dns_start" (string int int) uptr))
  (define ffi-net-dns-advance
    (foreign-procedure "chezpp_net_dns_advance" (uptr) scheme-object))
  (define ffi-net-dns-cancel
    (foreign-procedure "chezpp_net_dns_cancel" (uptr) scheme-object))
  (define ffi-net-dns-close
    (foreign-procedure "chezpp_net_dns_close" (uptr) void))
  (define ffi-net-address->name
    (foreign-procedure "chezpp_net_address_to_name" (int string int string) scheme-object))
  (define ffi-net-ftp-list
    (foreign-procedure "chezpp_net_ftp_list"
                       (string string string int int int int int)
                       scheme-object))
  (define ffi-net-ftp-stat
    (foreign-procedure "chezpp_net_ftp_stat"
                       (string string string int int int int int string)
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
  (define ffi-net-ftp-transfer-start
    (foreign-procedure "chezpp_net_ftp_transfer_start"
                       (int string string string string int int int int int)
                       scheme-object))
  (define ffi-net-ftp-transfer-step
    (foreign-procedure "chezpp_net_ftp_transfer_step" (uptr scheme-object int) scheme-object))
  (define ffi-net-ftp-transfer-cancel
    (foreign-procedure "chezpp_net_ftp_transfer_cancel" (uptr) scheme-object))
  (define ffi-net-ftp-transfer-close
    (foreign-procedure "chezpp_net_ftp_transfer_close" (uptr) void))
  (define ffi-net-ftp-session-open
    (foreign-procedure "chezpp_net_ftp_session_open" () scheme-object))
  (define ffi-net-ftp-session-close
    (foreign-procedure "chezpp_net_ftp_session_close" (uptr) scheme-object))
  (define ffi-net-ftp-file-open
    (foreign-procedure "chezpp_net_ftp_file_open"
                       (uptr int string string string int int int int int iptr)
                       scheme-object))
  (define ffi-net-ftp-file-step
    (foreign-procedure "chezpp_net_ftp_file_step" (uptr scheme-object int) scheme-object))
  (define ffi-net-ftp-file-read
    (foreign-procedure "chezpp_net_ftp_file_read" (uptr int) scheme-object))
  (define ffi-net-ftp-file-read-into
    (foreign-procedure "chezpp_net_ftp_file_read_into"
                       (uptr scheme-object int int int)
                       scheme-object))
  (define ffi-net-ftp-file-write
    (foreign-procedure "chezpp_net_ftp_file_write"
                       (uptr scheme-object int int int)
                       scheme-object))
  (define ffi-net-ftp-file-finish
    (foreign-procedure "chezpp_net_ftp_file_finish" (uptr) scheme-object))
  (define ffi-net-ftp-file-cancel
    (foreign-procedure "chezpp_net_ftp_file_cancel" (uptr) scheme-object))
  (define ffi-net-ftp-file-close
    (foreign-procedure "chezpp_net_ftp_file_close" (uptr) void))
  (define ffi-net-ssh-open
    (foreign-procedure "chezpp_net_ssh_open" (string int string int int) scheme-object))
  (define ffi-net-ssh-close
    (foreign-procedure "chezpp_net_ssh_close" (uptr) scheme-object))
  (define ffi-net-ssh-session-fd
    (foreign-procedure "chezpp_net_ssh_session_fd" (uptr) scheme-object))
  (define ffi-net-ssh-auth-password
    (foreign-procedure "chezpp_net_ssh_auth_password" (uptr string string) scheme-object))
  (define ffi-net-ssh-auth-publickey-auto
    (foreign-procedure "chezpp_net_ssh_auth_publickey_auto" (uptr string string) scheme-object))
  (define ffi-net-ssh-auth-publickey
    (foreign-procedure "chezpp_net_ssh_auth_publickey"
                       (uptr string string string string) scheme-object))
  (define ffi-net-ssh-auth-keyboard-interactive-step
    (foreign-procedure "chezpp_net_ssh_auth_keyboard_interactive_step"
                       (uptr string) scheme-object))
  (define ffi-net-ssh-auth-keyboard-interactive-answer
    (foreign-procedure "chezpp_net_ssh_auth_keyboard_interactive_answer"
                       (uptr int string) scheme-object))
  (define ffi-net-ssh-auth-agent
    (foreign-procedure "chezpp_net_ssh_auth_agent" (uptr string) scheme-object))
  (define ffi-net-ssh-auth-agent-identity
    (foreign-procedure "chezpp_net_ssh_auth_agent_identity"
                       (uptr string string) scheme-object))
  (define ffi-net-ssh-known-host-check
    (foreign-procedure "chezpp_net_ssh_known_host_check" (uptr string) scheme-object))
  (define ffi-net-ssh-known-host-update
    (foreign-procedure "chezpp_net_ssh_known_host_update" (uptr string) scheme-object))
  (define ffi-net-ssh-known-host-export
    (foreign-procedure "chezpp_net_ssh_known_host_export" (uptr) scheme-object))
  (define ffi-net-ssh-channel-open
    (foreign-procedure "chezpp_net_ssh_channel_open" (uptr int) scheme-object))
  (define ffi-net-ssh-channel-open-forward
    (foreign-procedure "chezpp_net_ssh_channel_open_forward"
                       (uptr string int string int int) scheme-object))
  (define ffi-net-ssh-remote-forward-listen
    (foreign-procedure "chezpp_net_ssh_remote_forward_listen"
                       (uptr string int) scheme-object))
  (define ffi-net-ssh-remote-forward-accept
    (foreign-procedure "chezpp_net_ssh_remote_forward_accept" (uptr) scheme-object))
  (define ffi-net-ssh-remote-forward-cancel
    (foreign-procedure "chezpp_net_ssh_remote_forward_cancel"
                       (uptr string int) scheme-object))
  (define ffi-net-ssh-channel-close
    (foreign-procedure "chezpp_net_ssh_channel_close" (uptr) scheme-object))
  (define ffi-net-ssh-channel-request-exec
    (foreign-procedure "chezpp_net_ssh_channel_request_exec" (uptr string int) scheme-object))
  (define ffi-net-ssh-channel-request-shell
    (foreign-procedure "chezpp_net_ssh_channel_request_shell" (uptr int) scheme-object))
  (define ffi-net-ssh-channel-request-pty
    (foreign-procedure "chezpp_net_ssh_channel_request_pty" (uptr int) scheme-object))
  (define ffi-net-ssh-channel-request-environment
    (foreign-procedure "chezpp_net_ssh_channel_request_environment" (uptr string string)
                       scheme-object))
  (define ffi-net-ssh-channel-request-subsystem
    (foreign-procedure "chezpp_net_ssh_channel_request_subsystem" (uptr string) scheme-object))
  (define ffi-net-ssh-channel-read
    (foreign-procedure "chezpp_net_ssh_channel_read" (uptr int int int int) scheme-object))
  (define ffi-net-ssh-channel-read-into
    (foreign-procedure "chezpp_net_ssh_channel_read_into" (uptr ptr int int int int int) scheme-object))
  (define ffi-net-ssh-channel-write
    (foreign-procedure "chezpp_net_ssh_channel_write" (uptr ptr int int int int) scheme-object))
  (define ffi-net-ssh-channel-exit-status
    (foreign-procedure "chezpp_net_ssh_channel_exit_status" (uptr) scheme-object))
  (define ffi-net-sftp-open
    (foreign-procedure "chezpp_net_sftp_open" (uptr) scheme-object))
  (define ffi-net-sftp-close
    (foreign-procedure "chezpp_net_sftp_close" (uptr) scheme-object))
  (define ffi-net-scp-download-file
    (foreign-procedure "chezpp_net_scp_download_file" (uptr string string int) scheme-object))
  (define ffi-net-scp-upload-file
    (foreign-procedure "chezpp_net_scp_upload_file" (uptr string string int) scheme-object))
  (define ffi-net-scp-stat
    (foreign-procedure "chezpp_net_scp_stat" (uptr string) scheme-object))
  (define ffi-net-scp-download-directory
    (foreign-procedure "chezpp_net_scp_download_directory" (uptr string string int) scheme-object))
  (define ffi-net-scp-upload-directory
    (foreign-procedure "chezpp_net_scp_upload_directory" (uptr string string int) scheme-object))
  (define ffi-net-scp-transfer-start
    (foreign-procedure "chezpp_net_scp_transfer_start" (uptr int string string) scheme-object))
  (define ffi-net-scp-transfer-step
    (foreign-procedure "chezpp_net_scp_transfer_step" (uptr) scheme-object))
  (define ffi-net-scp-transfer-cancel
    (foreign-procedure "chezpp_net_scp_transfer_cancel" (uptr) scheme-object))
  (define ffi-net-scp-transfer-close
    (foreign-procedure "chezpp_net_scp_transfer_close" (uptr) void))
  (define ffi-net-sftp-list
    (foreign-procedure "chezpp_net_sftp_list" (uptr string) scheme-object))
  (define ffi-net-sftp-stat
    (foreign-procedure "chezpp_net_sftp_stat" (uptr string) scheme-object))
  (define ffi-net-sftp-open-directory
    (foreign-procedure "chezpp_net_sftp_open_directory" (uptr string) scheme-object))
  (define ffi-net-sftp-read-directory
    (foreign-procedure "chezpp_net_sftp_read_directory" (uptr) scheme-object))
  (define ffi-net-sftp-close-directory
    (foreign-procedure "chezpp_net_sftp_close_directory" (uptr) scheme-object))
  (define ffi-net-sftp-chmod
    (foreign-procedure "chezpp_net_sftp_chmod" (uptr string unsigned) scheme-object))
  (define ffi-net-sftp-chown
    (foreign-procedure "chezpp_net_sftp_chown" (uptr string unsigned unsigned) scheme-object))
  (define ffi-net-sftp-utimes
    (foreign-procedure "chezpp_net_sftp_utimes" (uptr string integer-64 integer-64)
                       scheme-object))
  (define ffi-net-sftp-symlink
    (foreign-procedure "chezpp_net_sftp_symlink" (uptr string string) scheme-object))
  (define ffi-net-sftp-readlink
    (foreign-procedure "chezpp_net_sftp_readlink" (uptr string) scheme-object))
  (define ffi-net-sftp-seek
    (foreign-procedure "chezpp_net_sftp_seek" (uptr unsigned-64) scheme-object))
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
    (foreign-procedure "chezpp_net_sftp_read" (uptr int int int) scheme-object))
  (define ffi-net-sftp-read-into
    (foreign-procedure "chezpp_net_sftp_read_into" (uptr ptr int int int int) scheme-object))
  (define ffi-net-sftp-write
    (foreign-procedure "chezpp_net_sftp_write" (uptr ptr int int int int) scheme-object))
  (define ffi-net-websocket-listen
    (foreign-procedure "chezpp_net_websocket_listen"
                       (string int string string int uptr) scheme-object))
  (define ffi-net-websocket-server-close
    (foreign-procedure "chezpp_net_websocket_server_close" (uptr) scheme-object))
  (define ffi-net-websocket-accept
    (foreign-procedure "chezpp_net_websocket_accept" (uptr int int) scheme-object))
  (define ffi-net-websocket-connect
    (foreign-procedure "chezpp_net_websocket_connect"
                       (string int string string string int int uptr int) scheme-object))
  (define ffi-net-websocket-connect-step
    (foreign-procedure "chezpp_net_websocket_connect_step" (uptr) scheme-object))
  (define ffi-net-websocket-close
    (foreign-procedure "chezpp_net_websocket_close" (uptr) scheme-object))
  (define ffi-net-websocket-close-with-reason
    (foreign-procedure "chezpp_net_websocket_close_with_reason"
                       (uptr int scheme-object) scheme-object))
  (define ffi-net-websocket-state
    (foreign-procedure "chezpp_net_websocket_state" (uptr) scheme-object))
  (define ffi-net-websocket-cancel-send
    (foreign-procedure "chezpp_net_websocket_cancel_send" (uptr) scheme-object))
  (define ffi-net-websocket-send
    (foreign-procedure "chezpp_net_websocket_send" (uptr int ptr int int int int) scheme-object))
  (define ffi-net-websocket-send-fragment
    (foreign-procedure "chezpp_net_websocket_send_fragment"
                       (uptr int ptr int int int int int) scheme-object))
  (define ffi-net-websocket-recv
    (foreign-procedure "chezpp_net_websocket_recv" (uptr int int) scheme-object))
  (define ffi-net-grpc-channel-open
    (foreign-procedure "chezpp_net_grpc_channel_open" (string) scheme-object))
  (define ffi-net-grpc-channel-open-tls
    (foreign-procedure "chezpp_net_grpc_channel_open_tls"
                       (string string string string) scheme-object))
  (define ffi-net-grpc-channel-close
    (foreign-procedure "chezpp_net_grpc_channel_close" (uptr) scheme-object))
  (define ffi-net-grpc-server-open
    (foreign-procedure "chezpp_net_grpc_server_open" (string int) scheme-object))
  (define ffi-net-grpc-server-open-tls
    (foreign-procedure "chezpp_net_grpc_server_open_tls"
                       (string int string string string) scheme-object))
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
  (define ffi-net-grpc-stream-open
    (foreign-procedure "chezpp_net_grpc_stream_open"
                       (uptr string int ptr int int scheme-object int)
                       scheme-object))
  (define ffi-net-grpc-stream-open-start
    (foreign-procedure "chezpp_net_grpc_stream_open_start"
                       (uptr string int ptr int int scheme-object int)
                       scheme-object))
  (define ffi-net-grpc-stream-open-poll
    (foreign-procedure "chezpp_net_grpc_stream_open_poll" (uptr) scheme-object))
  (define ffi-net-grpc-stream-send
    (foreign-procedure "chezpp_net_grpc_stream_send" (uptr ptr int int) scheme-object))
  (define ffi-net-grpc-stream-recv
    (foreign-procedure "chezpp_net_grpc_stream_recv" (uptr) scheme-object))
  (define ffi-net-grpc-stream-close-send
    (foreign-procedure "chezpp_net_grpc_stream_close_send" (uptr) scheme-object))
  (define ffi-net-grpc-stream-finish
    (foreign-procedure "chezpp_net_grpc_stream_finish"
                       (uptr ptr int int int string scheme-object)
                       scheme-object))
  (define ffi-net-grpc-stream-close
    (foreign-procedure "chezpp_net_grpc_stream_close" (uptr) scheme-object))
  (define ffi-net-grpc-server-request
    (foreign-procedure "chezpp_net_grpc_server_request" (uptr) scheme-object))
  (define ffi-net-grpc-server-request-stream
    (foreign-procedure "chezpp_net_grpc_server_request_stream" (uptr) scheme-object))
  (define ffi-net-grpc-server-respond
    (foreign-procedure "chezpp_net_grpc_server_respond"
                       (uptr ptr int int int string scheme-object)
                       scheme-object))
  (define ffi-net-grpc-driver-fd
    (foreign-procedure "chezpp_net_grpc_driver_fd" () int))
  (define ffi-net-grpc-driver-drain
    (foreign-procedure "chezpp_net_grpc_driver_drain" () scheme-object))
  (define ffi-net-sftp-flag-read
    (foreign-procedure "chezpp_net_sftp_flag_read" () int))
  (define ffi-net-sftp-flag-write
    (foreign-procedure "chezpp_net_sftp_flag_write" () int))
  (define ffi-net-sftp-flag-read/write
    (foreign-procedure "chezpp_net_sftp_flag_read_write" () int))
  (define ffi-net-sftp-flag-append
    (foreign-procedure "chezpp_net_sftp_flag_append" () int))
  (define ffi-net-sftp-flag-create
    (foreign-procedure "chezpp_net_sftp_flag_create" () int))
  (define ffi-net-sftp-flag-truncate
    (foreign-procedure "chezpp_net_sftp_flag_truncate" () int))
  (define ffi-net-sftp-flag-exclusive
    (foreign-procedure "chezpp_net_sftp_flag_exclusive" () int))
  (define ffi-net-sftp-flag-text
    (foreign-procedure "chezpp_net_sftp_flag_text" () int))
  (define ffi-net-tls-load-error
    (foreign-procedure "chezpp_net_tls_load_error" () ptr))
  (define ffi-net-tls-context-create
    (foreign-procedure "chezpp_net_tls_context_create" (int) uptr))
  (define ffi-net-tls-context-free
    (foreign-procedure "chezpp_net_tls_context_free" (uptr) void))
  (define ffi-net-tls-context-load-ca-file
    (foreign-procedure "chezpp_net_tls_context_load_ca_file" (uptr string) scheme-object))
  (define ffi-net-tls-context-load-ca-path
    (foreign-procedure "chezpp_net_tls_context_load_ca_path" (uptr string) scheme-object))
  (define ffi-net-tls-context-load-default-ca
    (foreign-procedure "chezpp_net_tls_context_load_default_ca" (uptr) scheme-object))
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
    (foreign-procedure "chezpp_net_tls_connect" (uptr int string int) scheme-object))
  (define ffi-net-tls-accept
    (foreign-procedure "chezpp_net_tls_accept" (uptr int int) scheme-object))
  (define ffi-net-tls-handshake-step
    (foreign-procedure "chezpp_net_tls_handshake_step" (uptr) scheme-object))
  (define ffi-net-tls-close
    (foreign-procedure "chezpp_net_tls_close" (uptr) scheme-object))
  (define ffi-net-tls-read
    (foreign-procedure "chezpp_net_tls_read" (uptr int int int) scheme-object))
  (define ffi-net-tls-read-into
    (foreign-procedure "chezpp_net_tls_read_into" (uptr ptr int int int int) scheme-object))
  (define ffi-net-tls-write
    (foreign-procedure "chezpp_net_tls_write" (uptr ptr int int int int) scheme-object))
  (define ffi-net-tls-shutdown
    (foreign-procedure "chezpp_net_tls_shutdown" (uptr) scheme-object))
  (define ffi-net-tls-protocol-version
    (foreign-procedure "chezpp_net_tls_protocol_version" (uptr) scheme-object))
  (define ffi-net-tls-negotiated-alpn
    (foreign-procedure "chezpp_net_tls_negotiated_alpn" (uptr) scheme-object))
  (define ffi-net-tls-cipher-name
    (foreign-procedure "chezpp_net_tls_cipher_name" (uptr) scheme-object))
  (define ffi-net-tls-verified
    (foreign-procedure "chezpp_net_tls_verified" (uptr) scheme-object))
  (define ffi-net-tls-peer-certificate-der
    (foreign-procedure "chezpp_net_tls_peer_certificate_der" (uptr) scheme-object))
  (define ffi-net-tls-peer-certificate-chain-der
    (foreign-procedure "chezpp_net_tls_peer_certificate_chain_der" (uptr) scheme-object))
  )
