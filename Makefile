EMACS ?= emacs
ELPACA_DIR ?= $(CURDIR)/.elpaca-vendor/elpaca

LISP_SUBDIRS := $(wildcard lisp/*)
ELFILES := emagent.el $(shell find lisp -name '*.el')

.PHONY: all compile compile-strict test ci elpaca-vendor clean

all: compile

compile:
	$(EMACS) --batch -L . $(addprefix -L ,$(LISP_SUBDIRS)) -f batch-byte-compile $(ELFILES)

# Byte-compile from a clean tree with warnings promoted to errors. Cleaning
# first avoids stale .elc being loaded via `require`, which produces spurious
# "not known to be defined" warnings during incremental builds.
compile-strict:
	rm -f emagent.elc
	find lisp -name '*.elc' -delete
	$(EMACS) --batch -L . $(addprefix -L ,$(LISP_SUBDIRS)) \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(ELFILES)
	rm -f emagent.elc
	find lisp -name '*.elc' -delete

test: compile
	$(EMACS) --batch -L . $(addprefix -L ,$(LISP_SUBDIRS)) -l tests/emagent-test-runner.el

elpaca-vendor:
	@mkdir -p "$(dir $(ELPACA_DIR))"
	@if [ ! -d "$(ELPACA_DIR)/.git" ]; then \
		git clone --depth 1 https://github.com/progfolio/elpaca.git "$(ELPACA_DIR)"; \
	fi

ci: compile-strict elpaca-vendor
	EMAGENT_ROOT="$(CURDIR)" ELPACA_DIR="$(ELPACA_DIR)" \
	$(EMACS) --batch -l ci/build-and-test.el

clean:
	rm -f emagent.elc
	find lisp -name '*.elc' -delete
	rm -rf .elpaca-ci
