NET_TESTS := net-core.ss net-http.ss net-ftp.ss net-ssh.ss net-sftp.ss net-rpc.ss net-websocket.ss net-grpc.ss
NET_TEST_SUPPORT := net-common.ss net-ftp-common.ss net-ssh-common.ss

SRCS_TEST := record.ss datatype.ss match.ss for.ss control.ss os.ss iter.ss file.ss \
             list.ss string.ss vector.ss array.ss dlist.ss stack.ss queue.ss heap.ss \
             hashset.ss treemap.ss treeset.ss \
             bittree.ss bitvec.ss dset.ss \
             concurrency.ss \
             cli.ss hash.ss crypto.ss $(NET_TESTS)
OBJS_TEST := $(SRCS_TEST:.ss=.so)
TEST      := $(SRCS_TEST)

.PHONY: test
test: test-all

mat.so: mat.sls
	@tmp=$@.build.out; \
	rm -f $$tmp; \
	if ! echo '(optimize-level 3)(compile-file "mat.sls")' | ../chez++ -q > $$tmp 2>&1; then \
	  cat $$tmp; \
	  rm -f $$tmp; \
	  exit 1; \
	fi; \
	rm -f $$tmp

$(OBJS_TEST): $(SRCS_TEST) $(NET_TEST_SUPPORT) ../chezpp.lib ../chez++
	@tmp=$@.build.out; \
	rm -f $$tmp; \
	if ! echo '(optimize-level 2)(load "mat.so")(compile-file "$(basename $@).ss")' | ../chez++ -q > $$tmp 2>&1; then \
	  cat $$tmp; \
	  rm -f $$tmp; \
	  exit 1; \
	fi; \
	rm -f $$tmp

.PHONY: test-all
test-all: mat.so $(OBJS_TEST)
	@rm -f *.out
	@status=0; \
	$(foreach item, $(OBJS_TEST), \
	     echo '(load "mat.so")(load "$(item)")' | ../chez++ -q > $(item).out 2>&1 || status=$$?; \
	     if [ -s $(item).out ]; then cat $(item).out; status=1; fi; \
	) \
	exit $$status

.PHONY: test-some
test-some: mat.so $(OBJS_TEST)
	@rm -f *.out
	@status=0; \
	$(foreach item, $(TEST), \
	     echo '(load "mat.so")(load "$(item).so")' | ../chez++ -q > $(item).out 2>&1 || status=$$?; \
	     if [ -s $(item).out ]; then cat $(item).out; status=1; fi; \
	) \
	exit $$status

.PHONY: test-cov
test-cov: $(SRCS_TESTS)
	echo todo

.PHONY: clean
clean:
	@rm -f *.so *.out
