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

Not implemented: plugins, OIDC, application image upload, Gotify's
`gotify-client-token` cookie and signed four-part tokens. Passwords are hashed
with bcrypt, not Argon2.

## Requirements

- [opam](https://opam.ocaml.org) 2.x
- Node.js and npm (for esbuild, which bundles the client)
- A checkout of Yuka next to this repository

## Setup

Yuka is not on opam yet, so it is pinned from a checkout. Its `main` needs two
small local edits to build as an opam package: a `(name yuka)` in
`dune-project`, and `Async`, `Browser` and `State` as public modules.

```bash
git clone https://github.com/zakybilfagih/yuka-gotify
git clone https://github.com/davesnx/yuka
cd yuka
sed -i 's/^(lang dune 3.25)$/&\n\n(name yuka)/' dune-project
sed -i '/^  \(async\|browser\|state\)$/d' src/dune
cd ../yuka-gotify

# A local switch in this directory, with Yuka's pinned dependencies
opam switch create . ocaml-base-compiler.5.5.2 --no-install
eval "$(opam env)"
make -C ../yuka -s pin-list | while read -r name target; do
  opam pin add "$name" "$target" --yes --no-action
done
opam pin add yuka "$PWD/../yuka" --kind=rsync --yes --no-action
opam pin add yuka-server "$PWD/../yuka" --kind=rsync --yes --no-action
opam install -y yuka yuka-server caqti caqti-lwt caqti-driver-sqlite3 cmarkit safepass alcotest yojson

npm install
```

## Build and run

The server needs a secret of at least 32 bytes, for its signed cookies:

```bash
export YUKA_SECRET="$(openssl rand -hex 32)"
make serve PORT=8080
```

Open http://localhost:8080 and sign in as `admin` / `admin`.

`make serve` builds the client bundle with esbuild into `dist/` and runs the
server. Other targets (`make help` lists them):

| Target | What it does |
|---|---|
| `make build` | Build the server and the client |
| `make test` | Run the API test suite |
| `make serve-modules` | Serve the unbundled module tree |
| `make dev` | Rebuild, restart and reload on change |
| `make clean` | Remove the build and the bundle |

### Server options

```
_build/default/server/main.exe [--port N] [--interface IP] [--db FILE] [--root DIR] [--secure]
```

- `--port`: default `8080`
- `--interface`: default `127.0.0.1`; use `0.0.0.0` to listen on every interface
- `--db`: the SQLite file, default `gotify.db`
- `--secure`: secure cookies, for a site behind HTTPS. Browsers accept those
  over plain HTTP only on `localhost`.

Environment:

- `YUKA_SECRET`: required, at least 32 bytes. Keep it the same across
  restarts, or every browser is signed out.
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
