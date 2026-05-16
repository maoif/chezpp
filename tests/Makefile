NET_TESTS := net-core.ss net-http.ss net-ftp.ss net-ssh.ss net-sftp.ss net-scp.ss net-websocket.ss net-grpc.ss
NET_TEST_SUPPORT := net-common.ss net-ftp-common.ss net-ftp-server-process.ss net-ssh-common.ss

SRCS_TEST := record.ss datatype.ss match.ss for.ss control.ss os.ss iter.ss file.ss \
             list.ss string.ss vector.ss array.ss dlist.ss stack.ss queue.ss heap.ss \
             hashset.ss treemap.ss treeset.ss \
             bittree.ss bitvec.ss dset.ss \
             concurrency.ss \
             cli.ss rich.ss hash.ss crypto.ss uuid.ss $(NET_TESTS)
OBJS_TEST := $(SRCS_TEST:.ss=.so)
TEST      ?= $(SRCS_TEST)

.PHONY: test
test: test-all

mat.so: mat.sls
	@stdout=$@.build.stdout; stderr=$@.build.stderr; \
	rm -f "$$stdout" "$$stderr"; \
	printf '%s\n' "== compiling mat.sls =="; \
	echo '(optimize-level 3)(compile-file "mat.sls")' | ../chez++ -q >"$$stdout" 2>"$$stderr"; \
	status=$$?; \
	if [ -s "$$stdout" ]; then cat "$$stdout"; fi; \
	if [ -s "$$stderr" ]; then cat "$$stderr" >&2; fi; \
	if [ $$status -ne 0 ]; then exit $$status; fi

$(OBJS_TEST): $(SRCS_TEST) $(NET_TEST_SUPPORT) ../chezpp.lib ../chez++
	@stdout=$@.build.stdout; stderr=$@.build.stderr; \
	rm -f "$$stdout" "$$stderr"; \
	echo '(optimize-level 2)(load "mat.so")(compile-file "$(basename $@).ss")' | ../chez++ -q >"$$stdout" 2>"$$stderr"; \
	status=$$?; \
	if [ -s "$$stdout" ]; then cat "$$stdout"; fi; \
	if [ -s "$$stderr" ]; then cat "$$stderr" >&2; fi; \
	if [ $$status -ne 0 ]; then exit $$status; fi

.PHONY: test-all
test-all: mat.so $(OBJS_TEST)
	@rm -f *.out *.stdout *.stderr
	@status=0; \
	for item in $(OBJS_TEST); do \
	  stdout="$$item.stdout"; stderr="$$item.stderr"; \
	  rm -f "$$stdout" "$$stderr"; \
	  printf '%s\n' "== running $$item =="; \
	  echo '(load "mat.so")(load "'"$$item"'")' | ../chez++ -q >"$$stdout" 2>"$$stderr"; \
	  code=$$?; \
	  if [ -s "$$stdout" ]; then cat "$$stdout"; fi; \
	  if [ -s "$$stderr" ]; then cat "$$stderr" >&2; fi; \
	  if [ $$code -ne 0 ]; then status=1; fi; \
	  if grep -Eq 'Bug|Error' "$$stdout" "$$stderr"; then status=1; fi; \
	done; \
	exit $$status

.PHONY: test-some
test-some: mat.so $(OBJS_TEST)
	@rm -f *.out *.stdout *.stderr
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
	  echo '(load "mat.so")(load "'"$$base"'.so")' | ../chez++ -q >"$$stdout" 2>"$$stderr"; \
	  code=$$?; \
	  if [ -s "$$stdout" ]; then cat "$$stdout"; fi; \
	  if [ -s "$$stderr" ]; then cat "$$stderr" >&2; fi; \
	  if [ $$code -ne 0 ]; then status=1; fi; \
	  if grep -Eq 'Bug|Error' "$$stdout" "$$stderr"; then status=1; fi; \
	done; \
	exit $$status

.PHONY: test-cov
test-cov: $(SRCS_TESTS)
	echo todo

.PHONY: clean
clean:
	@rm -f *.so *.out *.stdout *.stderr *.build.stdout *.build.stderr
