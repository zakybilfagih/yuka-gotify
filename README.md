# yuka-gotify

A clone of [Gotify](https://gotify.net) 3.1.1 in OCaml, on
[Yuka](https://github.com/davesnx/yuka) and its built-in server, yuka-server.
One binary serves:

- **The Gotify REST API and `/stream`**, compatible with Gotify's clients:
  users, applications, clients, messages with paging, elevation, and the
  live WebSocket. Data is in SQLite.
- **A web UI with Gotify's Material look**, server-rendered with Yuka
  islands. Every page and form also works without JavaScript.

Web UI features: a live message list with infinite scroll, Markdown messages,
relative times, Read more, delete with Undo, desktop notifications, a
reconnect banner, Push message, drag to reorder applications, edit/delete/new
token/elevate dialogs as overlays, token reveal with Copy, light and dark
themes, and registration when enabled.

Not implemented: plugins and OIDC.

## Requirements

- [opam](https://opam.ocaml.org) 2.x
- Node.js and npm (for esbuild, which bundles the client)

## Setup

```bash
git clone https://github.com/zakybilfagih/yuka-gotify
cd yuka-gotify
make init
```

`make init` creates a local opam switch in `_opam` with OCaml 5.5.2, pins
`yuka` and `yuka-server` to Yuka's `main` on GitHub, installs the dependencies
of `yuka-gotify.opam`, and runs `npm install`. Its `pin-depends`, from
`yuka-gotify.opam.template`, pin Dave's patched dune and Melange and the ache
HTTP library. The opam file is generated from
`dune-project`: after changing the dependencies there, run
`dune build @opam --auto-promote`. The first run builds the compiler and every dependency, so it
takes a while.

Yuka is not on opam yet, and its `main` needs two changes to build as a pinned
package: [scripts/patch-yuka-opam.sh](scripts/patch-yuka-opam.sh) edits the
opam files during the pin to drop `dune subst`, which needs a project name
that Yuka's `dune-project` lacks, and to make `Yuka.Async`, `Yuka.Browser` and
`Yuka.State` public. To build against a local Yuka checkout instead, run
`make init YUKA=/path/to/yuka`.

## Build and run

The server needs a secret of at least 32 bytes, which signs yuka-server's
session and CSRF cookies:

```bash
export YUKA_SECRET="$(openssl rand -hex 32)"
make serve PORT=8080
```

Open http://localhost:8080 and sign in as `admin` / `admin`.

`make serve` builds the client bundle with esbuild into `dist/` and runs the
server. Other targets (`make help` lists them):

| Target | What it does |
|---|---|
| `make init` | Create the switch and install the dependencies |
| `make build` | Build the server and the client |
| `make test` | Run the API test suite |
| `make serve-modules` | Serve the unbundled module tree |
| `make dev` | Rebuild, restart and reload on change |
| `make clean` | Remove the build and the bundle |

### Server options

```
_build/default/server/main.exe [--port N] [--interface IP] [--db FILE] [--images DIR] [--root DIR] [--secure]
```

- `--port`: default `8080`
- `--interface`: default `127.0.0.1`; use `0.0.0.0` to listen on every interface
- `--db`: the SQLite file, default `gotify.db`
- `--images`: the directory of uploaded application images, default `images` (created if missing)
- `--secure`: secure cookies, for a site behind HTTPS. Browsers accept those
  over plain HTTP only on `localhost`.

Environment:

- `YUKA_SECRET`: required, at least 32 bytes. It signs yuka-server's
  session and CSRF cookies, not the login: as in Gotify, the web UI is
  signed in by its client's token, in the `gotify-client-token` cookie.
- `GOTIFY_DEFAULTUSER_NAME`, `GOTIFY_DEFAULTUSER_PASS`: the first admin,
  created with an empty database (default `admin` / `admin`).
- `GOTIFY_REGISTRATION=true`: lets anyone create a (non-admin) user, from the
  login page or `POST /user`.

### Push a message

Create an application in the web UI, then use its token as with Gotify:

```bash
curl -H "X-Gotify-Key: <app token>" -d title=Hello -d message=World http://localhost:8080/message
```

## Layout

- `core/`: the Gotify REST API, `/stream`, auth and the SQLite store
- `server/`: the web UI pages, form handlers and live feeds
- `shared/`: islands and code that runs on both the server and in the browser
- `client/`: the browser entry point
- `Routes.ml`: the web UI routes
- `test/`: the API tests

## License

MIT, see [LICENSE](LICENSE). `static/defaultapp.png` is Gotify's default
application image, from [gotify/server](https://github.com/gotify/server)
(MIT).
