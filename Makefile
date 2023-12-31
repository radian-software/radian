SHELL := bash

VERSION ?=
CMD ?=

longlines_files := $(shell find . -name .git -prune -o -print)
for_checkindent := $(shell find . -name .git -prune -o -name '*.el' -print)

.PHONY: help
help: ## Show this message
	@echo "usage:" >&2
	@grep -h "[#]# " $(MAKEFILE_LIST)	| \
		sed 's/^/  make /'		| \
		sed 's/:[^#]*[#]# /|/'		| \
		sed 's/%/LANG/'			| \
		column -t -s'|' >&2

.PHONY: link
link: ## Symlink dotfiles into home directory
	@scripts/symlink-dotfiles.bash

.PHONY: build
build: ## Make sure straight.el dependencies are built
	@scripts/build.bash

.PHONY: compile
compile: ## Byte-compile radian.el
	@scripts/byte-compile.bash

.PHONY: checkdoc
checkdoc: ## Check docstring style in radian.el
	@scripts/checkdoc.bash

.PHONY: longlines
longlines: ## Check for long lines
	@scripts/check-line-length.bash

.PHONY: checkindent
checkindent: ## Ensure that indentation is correct
	@tmpdir="$$(mktemp -d)"; for file in $(for_checkindent); do \
	    emacs -Q --batch \
	        --eval "(setq inhibit-message t)" \
                --eval "(progn \
                         (setq straight-safe-mode t) \
                         (load (expand-file-name \"init.el\" \
                               user-emacs-directory) nil t))" \
	        --eval "(find-file \"$$file\")" \
	        --eval "(indent-region (point-min) (point-max))" \
	        --eval "(write-file \"$$tmpdir/indented.el\")"; \
	    (diff <(cat          "$$file" | nl -v1 -ba | \
                           sed "s/\t/: /" | sed "s|^ *|$$file:|") \
	          <(cat "$$tmpdir/indented.el" | nl -v1 -ba | \
                           sed "s/\t/: /" | sed "s|^ *|$$file:|") ) \
	        | grep -F ">" | grep -o "[a-z].*" | grep . && exit 1 || true; \
	done

.PHONY: validate
validate: ## Validate el-patches
	@scripts/validate-patches.bash

.PHONY: lint
lint: build compile validate checkdoc longlines checkindent ## Run all linters

.PHONY: clean
clean: ## Remove build artifacts
	@rm -f emacs/radian.elc

.PHONY: docker
docker: ## Start a Docker shell; e.g. make docker VERSION=27
	@scripts/docker.bash "$(VERSION)" "$(CMD)"
