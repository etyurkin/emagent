EMACS ?= emacs
ELPACA_DIR ?= $(CURDIR)/.elpaca-vendor/elpaca

LISP_SUBDIRS := $(wildcard lisp/*)
ELFILES := emagent.el $(shell find lisp -name '*.el')

.PHONY: all compile test ci elpaca-vendor clean

all: compile

compile:
	$(EMACS) --batch -L . $(addprefix -L ,$(LISP_SUBDIRS)) -f batch-byte-compile $(ELFILES)

test: compile
	$(EMACS) --batch -L . $(addprefix -L ,$(LISP_SUBDIRS)) -l tests/emagent-test-runner.el

elpaca-vendor:
	@mkdir -p "$(dir $(ELPACA_DIR))"
	@if [ ! -d "$(ELPACA_DIR)/.git" ]; then \
		git clone --depth 1 https://github.com/progfolio/elpaca.git "$(ELPACA_DIR)"; \
	fi

ci: elpaca-vendor
	EMAGENT_ROOT="$(CURDIR)" ELPACA_DIR="$(ELPACA_DIR)" \
	$(EMACS) --batch -l ci/build-and-test.el

clean:
	rm -f emagent.elc
	find lisp -name '*.elc' -delete
	rm -rf .elpaca-ci
