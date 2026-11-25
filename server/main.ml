(* Gotify on Yuka: the Gotify REST API and /stream (Gotify_core.Api), and
   the web UI (Routes.ml) for every other path, on yuka-server. *)

let read file = In_channel.with_open_bin file In_channel.input_all

let usage =
  "gotify-yuka [--port N] [--interface IP] [--db FILE] [--secure]\n\
   Environment: YUKA_SECRET (at least 32 bytes, required), GOTIFY_DEFAULTUSER_NAME,\n\
   GOTIFY_DEFAULTUSER_PASS, GOTIFY_REGISTRATION=true"

let () =
  let port = ref 8080 and interface = ref "127.0.0.1" and db = ref "gotify.db" and root = ref "." in
  Arg.parse
    [
      ("--port", Arg.Set_int port, "PORT to listen on (default 8080)");
      ("--interface", Arg.Set_string interface, "IP to listen on (default 127.0.0.1)");
      ("--db", Arg.Set_string db, "FILE of the SQLite database (default gotify.db)");
      ("--root", Arg.Set_string root, "DIR of the project, with _build and static (default .)");
      ("--secure", Arg.Set Ui.secure, " the site is on HTTPS: secure cookies");
    ]
    (fun _ -> raise (Arg.Bad "no positional arguments"))
    usage;
  let secret =
    match Sys.getenv_opt "YUKA_SECRET" with
    | Some s when String.length s >= 32 -> s
    | _ ->
        prerr_endline "set YUKA_SECRET (at least 32 bytes), for example: export YUKA_SECRET=\"$(openssl rand -hex 32)\"";
        exit 1
  in
  Mirage_crypto_rng_unix.use_default ();
  Gotify_core.Api.registration := Sys.getenv_opt "GOTIFY_REGISTRATION" = Some "true";
  let file path = Filename.concat !root path in
  let dev = Sys.getenv_opt "YUKA_DEV" = Some "1" in
  let dir = file "_build/default/client/out" in
  let manifest = read (file "_build/default/client/yuka_islands.json") in
  let metafile = file "dist/meta.json" in
  let assets =
    match
      (* `make bundle` writes the esbuild bundle and its metafile in dist/, and its
         hashed chunks get an immutable cache-control; without them, and in dev, serve the module tree. *)
      if (not dev) && Sys.file_exists metafile then
        Yuka.Server.bundle ~dir:(file "dist/client") ~entry:"bundle/main.js" ~manifest
          ~metafile:(read metafile)
      else
        Yuka.Server.modules ~dir ~entry:"client/main.js" ~manifest
          ~packages:(Array.to_list (Sys.readdir (Filename.concat dir "node_modules")))
          ~digest:(Yuka.Server.tree_digest dir)
    with
    | Ok assets -> assets
    | Error message ->
        prerr_endline message;
        exit 1
  in
  Static.load ~dev (file "static");
  let config =
    Yuka.Server.config ~assets
      ~public:[ ("/static", file "static") ]
      ~styles:[ Static.url "style.css" ]
      ~dev ()
  in
  let sessions = Yuka_server.sessions ~secure:!Ui.secure ~secret () in
  Ui.sessions := Some sessions;
  Feed.register ();
  Handlers.register ();
  let yuka = Yuka_server.handler ~sessions config Yuka_routes.app in
  let handler connection reqd = if not (Gotify_core.Api.dispatch reqd || Static.serve reqd) then yuka connection reqd in
  let name = Option.value (Sys.getenv_opt "GOTIFY_DEFAULTUSER_NAME") ~default:"admin" in
  let pass = Option.value (Sys.getenv_opt "GOTIFY_DEFAULTUSER_PASS") ~default:"admin" in
  Lwt_main.run
    (Lwt.bind (Gotify_core.Db.connect !db) (fun () ->
         Lwt.bind (Gotify_core.Api.create_default_user ~name ~pass) (fun () ->
             Lwt.bind (Ui.load_known ()) @@ fun () ->
             Lwt.async Gotify_core.Api.sweep;
             Yuka_server.serve ~interface:!interface ~port:!port
               ~listening:(fun p -> Printf.eprintf "gotify-yuka on http://%s:%d\n%!" !interface p)
               handler)))
