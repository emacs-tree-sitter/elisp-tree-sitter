.PHONY: help
help: ## Prints target and a help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) |  \
	awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: build
build: ## Build the bindings
	@./bin/build
	@echo "[!!] in Emacs add $(PWD) to load-path:\n\n\t(add-to-list 'load-path \"$(PWD)/lisp\")\n\n\t(add-to-list 'load-path \"$(PWD)/langs\")"

.PHONY: ensure/%
ensure/%: ## Download grammar for a given language
	@./bin/ensure-lang $*
	@echo "[!!] load it in Emacs: \n\n\t(require 'tree-sitter)\n\t(tree-sitter-require '$*)\n"

.PHONY: test
test: ## Run tests
	@./bin/test

.PHONY: watch
watch: ## Continuous testing (requires cargo-watch)
	@./bin/test watch
