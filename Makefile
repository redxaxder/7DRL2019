.PHONY: list
list: ## Show available targets.
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##\s*\(.*\)/\n\t\1\n/'

.PHONY: watch
watch: ## Compile files on change with PSCID (useful for interactive development).
	@npx pscid -I src/

.PHONY: clean
clean: ## Remove all generated project files (keeps standard library).
	@find src -type f | cut -d/ -f2- | cut -d. -f1 | sed 's/\//./g' | sed 's/^/output\//' | xargs -L1 rm -rf
	@rm -rf target/

.PHONY: build
build: ## Build source files and package artifacts.
	@psc-package build
	@pulp browserify --no-check-main --to target/game.js
	@cp html/index.html target/index.html
	@cp data/* target/

.PHONY: tags
tags: build ## Create machine-readable project documentation.
	@psc-package sources | xargs purs docs --format ctags src/*.purs src/**/*.purs > tags

.PHONY: package
package: build
