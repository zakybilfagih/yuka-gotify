PORT ?= 8080
YUKA ?= git+https://github.com/davesnx/yuka.git\#main
OCAML_VERSION ?= 5.5.2

OPAM = opam exec --switch=$(CURDIR) --
DUNE = $(OPAM) dune
SERVER = ./_build/default/server/main.exe --port $(PORT)

.PHONY: help
help: ## Print this help message
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-10s\033[0m %s\n", $$1, $$2}'

.PHONY: init
init: ## Create the local switch, pin yuka and yuka-server to $(YUKA), install the opam and npm packages
	opam switch create . ocaml-base-compiler.$(OCAML_VERSION) --no-install -y
	opam pin add --switch=$(CURDIR) -y --no-action dune.3.25.0 "git+https://github.com/davesnx/dune.git#1e6f26d4d6615e0d2770977a00d2f654fcc4dba3"
	opam pin add --switch=$(CURDIR) -y --no-action melange.7.0.1-55 "git+https://github.com/davesnx/melange.git#16162e70268bf63cab1e12fd14f49f8d06983cf6"
	opam pin add --switch=$(CURDIR) -y --no-action ache "git+https://github.com/davesnx/ache.git#main"
	opam pin add --switch=$(CURDIR) -y --no-action ache-lwt "git+https://github.com/davesnx/ache.git#main"
	OPAMEDITOR="sh $(CURDIR)/scripts/patch-yuka-opam.sh --public" opam pin add --switch=$(CURDIR) -y --no-action --edit yuka.dev "$(YUKA)"
	OPAMEDITOR="sh $(CURDIR)/scripts/patch-yuka-opam.sh" opam pin add --switch=$(CURDIR) -y --no-action --edit yuka-server.dev "$(YUKA)"
	opam install --switch=$(CURDIR) -y yuka yuka-server caqti caqti-lwt caqti-driver-sqlite3 cmarkit safepass alcotest yojson \
	  ocaml-lsp-server ocamlmerlin-mlx
	npm install

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
