SRCS_TEST := record.ss datatype.ss match.ss comprehension.ss control.ss os.ss iter.ss file.ss \
             list.ss string.ss vector.ss array.ss dlist.ss stack.ss queue.ss heap.ss \
             hashset.ss treemap.ss treeset.ss \
             bittree.ss bitvec.ss dset.ss \
             concurrency.ss
OBJS_TEST := $(SRCS_TEST:.ss=.so)
TEST      := $(SRCS_TEST)

.PHONY: test
test: test-all

mat.so: mat.sls
	@echo '(optimize-level 3)(compile-file "mat.sls")' | ../chez++ -q

$(OBJS_TEST): $(SRCS_TEST) ../chezpp.lib ../chez++
	@echo '(optimize-level 2)(load "mat.so")(compile-file "$(basename $@).ss")' | ../chez++ -q

.PHONY: test-all
test-all: mat.so $(OBJS_TEST)
	@rm -f *.out
	@$(foreach item, $(OBJS_TEST), \
	     echo testing $(item) ;\
	     echo '(load "mat.so")(load "$(item)")' | ../chez++ -q &> $(item).out; \
	)
	@grep -E 'Bug|Error' *.out || true

.PHONY: test-some
test-some: mat.so $(OBJS_TEST)
	@rm -f *.out
	@$(foreach item, $(TEST), \
	     echo testing $(item) ;\
	     echo '(load "mat.so")(load "$(item).so")' | ../chez++ -q  &> $(item).out; \
	)
	@grep -E 'Bug|Error' *.out || true

.PHONY: test-cov
test-cov: $(SRCS_TESTS)
	echo todo

.PHONY: clean
clean:
	@rm -f *.so *.out
