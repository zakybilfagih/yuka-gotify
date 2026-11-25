PORT ?= 8080

OPAM = opam exec --switch=$(CURDIR) --
DUNE = $(OPAM) dune
SERVER = ./_build/default/server/main.exe --port $(PORT)

.PHONY: help
help: ## Print this help message
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-10s\033[0m %s\n", $$1, $$2}'

.PHONY: build
build: ## Build the server and the client
	$(DUNE) build

.PHONY: test
test: ## Run the API tests
	$(DUNE) test

# The bundle goes in dist/, outside _build, so that a dune build does not delete
# the files of a running server. The server serves it while dist/meta.json exists.
.PHONY: bundle
bundle: build ## Bundle the client with esbuild in dist/, served instead of the module tree
	rm -rf dist && mkdir -p dist/client
	cd dist/client && $(CURDIR)/node_modules/.bin/esbuild $(CURDIR)/_build/default/client/out/client/main.js \
	  --bundle --minify --format=esm --splitting --outdir=bundle --metafile=../meta.json \
	  --define:__YUKA_DEV__=false --log-level=warning

.PHONY: serve
serve: bundle ## Serve the bundle on PORT (YUKA_SECRET must be set)
	$(SERVER)

.PHONY: serve-modules
serve-modules: build ## Serve the module tree on PORT, without esbuild
	rm -rf dist
	$(SERVER)

.PHONY: dev
dev: ## Rebuild, restart and reload on change, with the module tree (PORT, YUKA_SECRET must be set)
	$(DUNE) build ./server/main.exe && \
	$(OPAM) yuka dev --build 'dune build --watch @all' --assets _build/default/client/out --assets static \
	  -- $(SERVER)

.PHONY: clean
clean: ## Remove the build and the bundle
	$(DUNE) clean
	rm -rf dist
