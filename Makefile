EMACS ?= emacs

ELFILES := $(wildcard emagent*.el)

.PHONY: all compile test clean

all: compile

compile:
	$(EMACS) --batch -L . -f batch-byte-compile $(ELFILES)

test: compile
	$(EMACS) --batch -L . -l tests/emagent-test-runner.el

clean:
	rm -f $(ELFILES:.el=.elc)
