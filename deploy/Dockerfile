# The prebuilt server (glibc 2.42+), its client bundle and static files, and the demo data.
# Build it from a directory that holds this file, entrypoint.sh, app/ and seed/ (see the README).
# The server links libsqlite3 and libgmp (for Zarith): check with `ldd main.exe` after adding a C library.
FROM debian:sid-slim
RUN apt-get update && apt-get install -y --no-install-recommends libsqlite3-0 libgmp10 ca-certificates && rm -rf /var/lib/apt/lists/*
COPY app /app
COPY seed /seed
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh /app/_build/default/server/main.exe
CMD ["/entrypoint.sh"]
