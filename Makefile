VERSION ?=
CMD ?=

longlines_files := $(shell find . -name .git -prune -o -print)

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

.PHONY: validate
validate: ## Validate el-patches
	@scripts/validate-patches.bash

.PHONY: lint
lint: build compile validate checkdoc longlines ## Run all linters

.PHONY: clean
clean: ## Remove build artifacts
	@rm -f emacs/radian.elc

.PHONY: docker
docker: ## Start a Docker shell; e.g. make docker VERSION=26
	@scripts/docker.bash "$(VERSION)" "$(CMD)"
