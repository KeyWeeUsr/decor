EMACS := emacs

.PHONY: all
all: byte-compile test

.PHONY: clean
clean:
	@-rm decor*.elc 2>/dev/null
	@-rm *.ok 2>/dev/null

%.elc: %.el
	@-rm "$@" 2>/dev/null
	@$(EMACS) --batch --quick \
		--directory . \
		--load compile-setup \
		--eval '(byte-compile-file "$(subst .elc,.el,$@)")' \
		&& test -f "$@"

byte-compile: \
	decor.elc \
	decor-tests.elc

.PHONY: test
test: byte-compile main-tests

decor-tests.ok: \
	decor.elc decor-tests.elc
	$(EMACS) --batch --quick \
		--directory . \
		--load decor-tests.el \
		--funcall ert-run-tests-batch \
	&& touch decor-tests.ok
main-tests: decor-tests.ok

Makefile.ok: Makefile
	@make -n all
	@docker run \
		--network=none \
		--volume "$(PWD)"/Makefile:/Makefile \
		backplane/checkmake /Makefile
lint-makefile: Makefile.ok

.PHONY: tag
tag:
	$(MAKE) all
	git add -f . && git stash
	@grep ";; Version:" decor.el \
		| tee /dev/stderr | grep "$(TAG)"
	@git tag "$(TAG)" --sign
