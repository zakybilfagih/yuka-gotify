#!/bin/sh
# OPAMEDITOR for `opam pin add --edit` of yuka and yuka-server (see `make init`).
# Usage: patch-yuka-opam.sh [--public] OPAM_FILE
#   Drops `dune subst`, which fails because yuka's dune-project has no name.
#   --public also makes Yuka.Async, Yuka.State and Yuka.Browser public before
#   the build, since src/dune lists them in private_modules.
public=
[ "$1" = --public ] && { public=1; shift; }
f="$1"
sed -i '/\["dune" "subst"\]/d' "$f"
if [ -n "$public" ]; then
  sed -i 's|^build: \[$|build: [\n  ["sed" "-i" "-e" "/^  async$/d" "-e" "/^  browser$/d" "-e" "/^  state$/d" "src/dune"]|' "$f"
fi
