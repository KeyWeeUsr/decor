EMACS := emacs

all:
	$(EMACS) --batch --quick \
		--directory . \
		--load decor-tests.el \
		--funcall ert-run-tests-batch
