(lang dune 3.25)

(name yuka-gotify)

(using melange 1.0)

(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader mlx)
  (preprocess
   (run mlx-pp %{input-file}))))

(generate_opam_files true)

(source
 (github zakybilfagih/yuka-gotify))

(license MIT)

(authors "Zaky Bilfagih")

(maintainers "Zaky Bilfagih")

; yuka and yuka-server are pinned by `make init`, which patches their opam
; files; Dave's dune and Melange and ache are in yuka-gotify.opam.template.

(package
 (name yuka-gotify)
 (allow_empty)
 (synopsis "Gotify on Yuka: the Gotify REST API and a web UI")
 (depends
  (ocaml (>= 5.5))
  (dune (>= 3.25))
  melange
  mlx
  yuka
  yuka-server
  ache
  ache-lwt
  lwt
  caqti
  caqti-lwt
  caqti-driver-sqlite3
  cmarkit
  yojson
  uri
  ptime
  base64
  magic-mime
  mirage-crypto-ec
  mirage-crypto-rng
  safepass
  (alcotest :with-test)))
