EMACS ?= emacs

LISP_SUBDIRS := $(wildcard lisp/*)
ELFILES := emagent.el $(shell find lisp -name '*.el')

.PHONY: all compile test clean

all: compile

compile:
	$(EMACS) --batch -L . $(addprefix -L ,$(LISP_SUBDIRS)) -f batch-byte-compile $(ELFILES)

test: compile
	$(EMACS) --batch -L . $(addprefix -L ,$(LISP_SUBDIRS)) -l tests/emagent-test-runner.el

clean:
	rm -f emagent.elc
	find lisp -name '*.elc' -delete
