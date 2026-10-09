NET_TESTS := net-operation.ss net-transfer.ss net-core.ss net-http-contract.ss net-lws-reactor.ss \
             net-lws-http2.ss \
             net-lws-server.ss \
             net-http-fiber.ss net-lws-stress.ss net-http-stress.ss \
             net-http.ss net-ftp.ss \
             net-ssh.ss net-sftp.ss \
             net-scp.ss net-websocket.ss net-grpc.ss net-examples.ss

SRCS_TEST := test.ss record.ss datatype.ss match.ss for.ss control.ss system.ss system-user.ss system-filesystem.ss system-signal.ss system-process.ss iter.ss transducer.ss file.ss path.ss glob.ss navigator.ss \
             list.ss string.ss vector.ss array.ss dlist.ss stack.ss queue.ss heap.ss \
             hashset.ss treemap.ss treeset.ss rbtree-specialized.ss \
             bittree.ss bitvec.ss dset.ss \
             concurrency.ss \
             cli.ss rich.ss logging.ss benchmark.ss hash.ss crypto.ss optional-openssl.ss \
             optional-library.ss net-loader.ss uuid.ss \
             protobuf.ss protobuf-codegen.ss \
             $(NET_TESTS) \
             parser.ss net-errors.ss net-address.ss net-dns.ss net-ip.ss net-docs.ss \
             net-socket-datagram.ss net-uri.ss
