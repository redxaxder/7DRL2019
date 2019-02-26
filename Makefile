.PHONY: list
list: ## Show available targets.
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##\s*\(.*\)/\n\t\1\n/'

.PHONY: watch
watch: ## Compile files on change with PSCID (useful for interactive development).
	@npx pscid -I src/

.PHONY: compile
compile: output/Main/index.js ## Compile all project source files.

.PHONY: package
package: target/game.js target/index.html ## Package project build artifacts for distribution.

tags: output/Main/index.js ## Create machine-readable project documentation.
	@psc-package sources | xargs purs docs --format ctags src/*.purs src/**/*.purs > tags

.PHONY: build
build: compile package tags ## Full project build.

.PHONY: clean
clean: ## Remove all generated project files (keeps standard library).
	@find src -type f | cut -d/ -f2- | cut -d. -f1 | sed 's/\//./g' | sed 's/^/output\//' | xargs -L1 rm -rf
	@rm -rf target/

output/Main/index.js: $(shell find src/ -type f)
	@psc-package build

target/game.js: output/Main/index.js
	@purs bundle output/*/{index,foreign}.js --module Main --main Main -o target/game.js

target/index.html: html/index.html
	@cp html/index.html target/index.html
