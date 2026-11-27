#!/bin/sh
# The database and images live on the Railway volume at $DATA (default /data).
# The first start copies the demo data into it; later starts keep what is there.
set -e
DATA="${DATA:-/data}"
mkdir -p "$DATA"
if [ ! -f "$DATA/gotify.db" ]; then
  echo "seeding $DATA with the demo data"
  cp /seed/gotify.db "$DATA/gotify.db"
  mkdir -p "$DATA/images" && cp /seed/images/* "$DATA/images/"
fi
exec /app/_build/default/server/main.exe --root /app --interface 0.0.0.0 --port "${PORT:-8080}" \
  --db "$DATA/gotify.db" --images "$DATA/images" --secure
