.ONESHELL:

include ../build-options.mk

ROOT_DIR := ..
COVIN_FILES := $(shell find ../chezpp -type f -name '*.covin' -print)

.PHONY: print-build-options prepare-chezpp

print-build-options:
	$(print-build-options)

prepare-chezpp:
	@$(MAKE) --no-print-directory -C $(ROOT_DIR) chez++ \
		VARIANT='$(VARIANT)' GENCOV='$(GENCOV)' $(BUILD_OPTION_FORWARD_VARS)

NET_TESTS := net-operation.ss net-transfer.ss net-core.ss net-http-contract.ss net-lws-reactor.ss \
             net-lws-http2.ss \
             net-lws-server.ss \
             net-http-fiber.ss net-lws-stress.ss net-http-stress.ss \
             net-http.ss net-ftp.ss \
             net-ssh.ss net-sftp.ss \
             net-scp.ss net-websocket.ss net-grpc.ss net-examples.ss
NET_TEST_SUPPORT := net-common.ss net-ftp-common.ss net-ftp-server-process.ss net-ssh-common.ss \
                    net-lws-fixture.ss

SRCS_TEST := test.ss record.ss datatype.ss match.ss for.ss control.ss system.ss system-user.ss system-filesystem.ss system-signal.ss system-process.ss iter.ss transducer.ss file.ss path.ss navigator.ss \
             list.ss string.ss vector.ss array.ss dlist.ss stack.ss queue.ss heap.ss \
             hashset.ss treemap.ss treeset.ss rbtree-specialized.ss \
             bittree.ss bitvec.ss dset.ss \
             concurrency.ss \
             cli.ss rich.ss logging.ss benchmark.ss hash.ss crypto.ss optional-openssl.ss net-loader.ss uuid.ss \
             protobuf.ss protobuf-codegen.ss \
             $(NET_TESTS) \
             parser.ss net-errors.ss net-address.ss net-dns.ss net-ip.ss net-docs.ss \
             net-socket-datagram.ss net-uri.ss
OBJS_TEST := $(SRCS_TEST:.ss=.so)
TEST      ?= $(SRCS_TEST)
TEST_OBJS := $(addsuffix .so,$(basename $(TEST)))

HTTP_ERROR_SOAK_SECONDS ?= 300
HTTP_ERROR_SOAK_TESTS := net-http net-lws-http2 net-lws-server
HTTP_ERROR_SOAK_OBJS := $(addsuffix .so,$(HTTP_ERROR_SOAK_TESTS))

lws-http2-fixture: ../chezpp/c/net/lws_http2_fixture.c
	$(CC) -Wall -Wextra -O2 $< -o $@ -ldl

net-lws-http2.so net-http-fiber.so: lws-http2-fixture

.PHONY: test
test: test-all

mat.so: mat.sls | prepare-chezpp
	@stdout=$@.build.stdout; stderr=$@.build.stderr; \
	rm -f "$$stdout" "$$stderr"; \
	printf '%s\n' "== compiling mat.sls =="; \
	printf '%s\n' '$(CHEZ_TEST_COMPILER_FORMS) (compile-file "mat.sls")' | ../chez++ --script /dev/stdin >"$$stdout" 2>"$$stderr"; \
	status=$$?; \
	if [ -s "$$stdout" ]; then cat "$$stdout"; fi; \
	if [ -s "$$stderr" ]; then cat "$$stderr" >&2; fi; \
	if [ $$status -ne 0 ]; then exit $$status; fi

$(OBJS_TEST): %.so: %.ss $(NET_TEST_SUPPORT) ../chezpp.lib ../chez++ | prepare-chezpp
	@stdout=$@.build.stdout; stderr=$@.build.stderr; \
	rm -f "$$stdout" "$$stderr"; \
	printf '%s\n' '$(CHEZ_TEST_COMPILER_FORMS) (load "mat.so")(compile-file "$(basename $@).ss")' | ../chez++ --script /dev/stdin >"$$stdout" 2>"$$stderr"; \
	status=$$?; \
	if [ -s "$$stdout" ]; then cat "$$stdout"; fi; \
	if [ -s "$$stderr" ]; then cat "$$stderr" >&2; fi; \
	if [ $$status -ne 0 ]; then exit $$status; fi

.PHONY: test-all
test-all: prepare-chezpp mat.so $(OBJS_TEST)
	@cleanup() { \
	  rm -f *.out *.stdout *.stderr *.build.stdout *.build.stderr *.covout coverage.covout; \
	  rm -f /tmp/chezpp-net-test-cert.pem /tmp/chezpp-net-test-key.pem \
	    /tmp/chezpp-net-test-san-cert.pem /tmp/chezpp-net-test-san-key.pem \
	    /tmp/chezpp-net.sock /tmp/chezpp-net-ftp-*.txt /tmp/chezpp-net-ftp-*.bin \
	    /tmp/chezpp-net-sftp-*.bin /tmp/chezpp-net-scp-*.txt /tmp/chezpp-net-ssh-*.txt; \
	  rm -rf /tmp/chezpp-net-ftp-root /tmp/chezpp-net-ftp-link-source \
	    /tmp/chezpp-net-ftp-tree-source /tmp/chezpp-net-ftp-tree-dest \
	    /tmp/chezpp-example-sftp /tmp/explicit-chezpp-sftp-state \
	    /tmp/upload-root; \
	}; \
	trap 'cleanup; rm -f lws-http2-fixture' EXIT HUP INT TERM; cleanup
	@status=0; \
	for item in $(OBJS_TEST); do \
	  stdout="$$item.stdout"; stderr="$$item.stderr"; \
	  rm -f "$$stdout" "$$stderr"; \
	  printf '%s\n' "== running $$item =="; \
	  if [ -n "$(GENCOV)" ]; then \
	    covin_files=$$(find ../chezpp -type f -name '*.covin' -print); \
	    ../chez++ --script coverage-init.ss "$$item" "$${item%.so}.covout" $$covin_files >"$$stdout" 2>"$$stderr"; \
	  else \
	    echo '(load "mat.so")(load "'"$$item"'")' | ../chez++ --script /dev/stdin >"$$stdout" 2>"$$stderr"; \
	  fi; \
	  code=$$?; \
	  if [ -s "$$stdout" ]; then cat "$$stdout"; fi; \
	  if [ -s "$$stderr" ]; then cat "$$stderr" >&2; fi; \
	  if [ $$code -ne 0 ]; then status=1; fi; \
	  if grep -Eq 'Bug|Error' "$$stdout" "$$stderr"; then status=1; fi; \
	 done; \
	 if [ -n "$(GENCOV)" ]; then \
	   covin_files=$$(find ../chezpp -type f -name '*.covin' -print); \
	   covout_files=$$(find . -maxdepth 1 -type f -name '*.covout' -print); \
	   set -- $$covin_files; covin_count=$$#; \
	   ../chez++ --script coverage-report.ss "$$covin_count" coverage.covout $$covin_files $$covout_files; \
	   report_code=$$?; if [ $$report_code -ne 0 ]; then status=1; fi; \
	 fi; \
	 exit $$status

.PHONY: test-some
test-some: prepare-chezpp mat.so $(TEST_OBJS)
	@cleanup() { \
	  rm -f *.out *.stdout *.stderr *.build.stdout *.build.stderr *.covout coverage.covout; \
	  rm -f /tmp/chezpp-net-test-cert.pem /tmp/chezpp-net-test-key.pem \
	    /tmp/chezpp-net-test-san-cert.pem /tmp/chezpp-net-test-san-key.pem \
	    /tmp/chezpp-net.sock /tmp/chezpp-net-ftp-*.txt /tmp/chezpp-net-ftp-*.bin \
	    /tmp/chezpp-net-sftp-*.bin /tmp/chezpp-net-scp-*.txt /tmp/chezpp-net-ssh-*.txt; \
	  rm -rf /tmp/chezpp-net-ftp-root /tmp/chezpp-net-ftp-link-source \
	    /tmp/chezpp-net-ftp-tree-source /tmp/chezpp-net-ftp-tree-dest \
	    /tmp/chezpp-example-sftp /tmp/explicit-chezpp-sftp-state \
	    /tmp/upload-root; \
	}; \
	trap 'cleanup; rm -f lws-http2-fixture' EXIT HUP INT TERM; cleanup
	@status=0; \
	for item in $(TEST); do \
	  case "$$item" in \
	    *.ss) base="$${item%.ss}" ;; \
	    *.so) base="$${item%.so}" ;; \
	    *) base="$$item" ;; \
	  esac; \
	  stdout="$$base.stdout"; stderr="$$base.stderr"; \
	  rm -f "$$stdout" "$$stderr"; \
	  printf '%s\n' "== running $$base.so =="; \
	  if [ -n "$(GENCOV)" ]; then \
	    covin_files=$$(find ../chezpp -type f -name '*.covin' -print); \
	    ../chez++ --script coverage-init.ss "$$base.so" "$$base.covout" $$covin_files >"$$stdout" 2>"$$stderr"; \
	  else \
	    echo '(load "mat.so")(load "'"$$base"'.so")' | ../chez++ --script /dev/stdin >"$$stdout" 2>"$$stderr"; \
	  fi; \
	  code=$$?; \
	  if [ -s "$$stdout" ]; then cat "$$stdout"; fi; \
	  if [ -s "$$stderr" ]; then cat "$$stderr" >&2; fi; \
	  if [ $$code -ne 0 ]; then status=1; fi; \
	  if grep -Eq 'Bug|Error' "$$stdout" "$$stderr"; then status=1; fi; \
	 done; \
	 if [ -n "$(GENCOV)" ]; then \
	   covin_files=$$(find ../chezpp -type f -name '*.covin' -print); \
	   covout_files=$$(find . -maxdepth 1 -type f -name '*.covout' -print); \
	   set -- $$covin_files; covin_count=$$#; \
	   ../chez++ --script coverage-report.ss "$$covin_count" coverage.covout $$covin_files $$covout_files; \
	   report_code=$$?; if [ $$report_code -ne 0 ]; then status=1; fi; \
	 fi; \
	 exit $$status

.PHONY: test-http-error-soak
test-http-error-soak: prepare-chezpp mat.so $(HTTP_ERROR_SOAK_OBJS)
	@case "$(HTTP_ERROR_SOAK_SECONDS)" in \
	  ''|*[!0-9]*) \
	    printf '%s\n' "HTTP_ERROR_SOAK_SECONDS must be a positive integer" >&2; exit 2 ;; \
	esac
	@if [ "$(HTTP_ERROR_SOAK_SECONDS)" -le 0 ]; then \
	  printf '%s\n' "HTTP_ERROR_SOAK_SECONDS must be a positive integer" >&2; exit 2; \
	fi
	@cleanup() { \
	  rm -f *.out *.stdout *.stderr *.build.stdout *.build.stderr; \
	  rm -f /tmp/chezpp-net-test-cert.pem /tmp/chezpp-net-test-key.pem \
	    /tmp/chezpp-net-test-san-cert.pem /tmp/chezpp-net-test-san-key.pem \
	    /tmp/chezpp-net.sock /tmp/chezpp-net-ftp-*.txt /tmp/chezpp-net-ftp-*.bin \
	    /tmp/chezpp-net-sftp-*.bin /tmp/chezpp-net-scp-*.txt /tmp/chezpp-net-ssh-*.txt; \
	  rm -rf /tmp/chezpp-net-ftp-root /tmp/chezpp-net-ftp-link-source \
	    /tmp/chezpp-net-ftp-tree-source /tmp/chezpp-net-ftp-tree-dest \
	    /tmp/chezpp-example-sftp /tmp/explicit-chezpp-sftp-state \
	    /tmp/upload-root; \
	}; \
	trap 'cleanup; rm -f lws-http2-fixture' EXIT HUP INT TERM; cleanup
	@start=$$(date +%s); deadline=$$((start + $(HTTP_ERROR_SOAK_SECONDS))); round=0; \
	while :; do \
	  round=$$((round + 1)); \
	  for base in $(HTTP_ERROR_SOAK_TESTS); do \
	    stdout="$$base.stdout"; stderr="$$base.stderr"; \
	    rm -f "$$stdout" "$$stderr"; \
	    printf '%s\n' "== soak round $$round: $$base.so =="; \
	    echo '(load "mat.so")(load "'"$$base"'.so")' | \
	      ../chez++ --script /dev/stdin >"$$stdout" 2>"$$stderr"; \
	    code=$$?; \
	    if [ -s "$$stdout" ]; then cat "$$stdout"; fi; \
	    if [ -s "$$stderr" ]; then cat "$$stderr" >&2; fi; \
	    if [ $$code -ne 0 ] || grep -Eq 'Bug|Error' "$$stdout" "$$stderr"; then \
	      exit 1; \
	    fi; \
	  done; \
	  now=$$(date +%s); \
	  if [ $$now -ge $$deadline ]; then \
	    printf '%s\n' "completed $$round soak rounds in $$((now - start)) seconds"; \
	    break; \
	  fi; \
	done

.PHONY: test-cov
test-cov: $(SRCS_TESTS)
	echo todo

.PHONY: test-optional-hash-linkage
test-optional-hash-linkage:
	@./optional-hash-linkage.sh

.PHONY: test-optional-library-loader
test-optional-library-loader:
	@./optional-library-loader.sh

.PHONY: test-optional-library-linkage
test-optional-library-linkage:
	@./optional-library-linkage.sh

.PHONY: clean
clean:
	@rm -f *.so *.out *.stdout *.stderr *.build.stdout *.build.stderr lws-http2-fixture
	@rm -f /tmp/chezpp-net-test-cert.pem /tmp/chezpp-net-test-key.pem \
	  /tmp/chezpp-net-test-san-cert.pem /tmp/chezpp-net-test-san-key.pem \
	  /tmp/chezpp-net.sock /tmp/chezpp-net-ftp-*.txt /tmp/chezpp-net-ftp-*.bin \
	  /tmp/chezpp-net-sftp-*.bin /tmp/chezpp-net-scp-*.txt /tmp/chezpp-net-ssh-*.txt
	@rm -rf /tmp/chezpp-net-ftp-root /tmp/chezpp-net-ftp-link-source \
	  /tmp/chezpp-net-ftp-tree-source /tmp/chezpp-net-ftp-tree-dest \
	  /tmp/chezpp-example-sftp /tmp/explicit-chezpp-sftp-state /tmp/upload-root
