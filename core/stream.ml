(* The sockets of GET /stream, by user, and the one place a new message is
   published from: every socket of the user, then every hook (the live topic
   of the web UI). *)

type socket = { token : string; ws : Ache_lwt.Websocket.t }

let sockets : (int, socket list) Hashtbl.t = Hashtbl.create 16
let hooks : (int -> unit) list ref = ref []

(** [f uid] runs after the messages of user [uid] change (created or deleted). *)
let on_change f = hooks := f :: !hooks

let changed uid = List.iter (fun f -> try f uid with _ -> ()) !hooks
let of_user uid = Option.value (Hashtbl.find_opt sockets uid) ~default:[]
let add uid s = Hashtbl.replace sockets uid (s :: of_user uid)
let remove uid s = Hashtbl.replace sockets uid (List.filter (fun x -> x != s) (of_user uid))

let close s = Lwt.async (fun () -> Lwt.catch (fun () -> Ache_lwt.Websocket.close s.ws 1000) (fun _ -> Lwt.return_unit))

let publish uid (m : Model.message) =
  let text = Yojson.Safe.to_string (Model.message_json m) in
  List.iter
    (fun s ->
      Lwt.async (fun () ->
          Lwt.catch (fun () -> Ache_lwt.Websocket.send_text s.ws text) (fun _ -> remove uid s; Lwt.return_unit)))
    (of_user uid);
  changed uid

let deleted_hooks : (string -> unit) list ref = ref []

(** [f token] runs after the client with this (stored) token was deleted. *)
let on_client_deleted f = deleted_hooks := f :: !deleted_hooks

(** Closes the sockets of a deleted client. *)
let client_deleted uid token =
  List.iter (fun f -> try f token with _ -> ()) !deleted_hooks;
  let gone, kept = List.partition (fun s -> s.token = token) (of_user uid) in
  Hashtbl.replace sockets uid kept;
  List.iter close gone

let user_deleted uid =
  List.iter close (of_user uid);
  Hashtbl.remove sockets uid

(* The server pings every socket, as Gotify (server.stream.pingperiodseconds):
   ache closes a socket that receives no frame for 60 s, and a client that
   only listens sends none but the pong. *)
let ping_period = 45.

(** server.stream.allowedorigins: regular expressions, matched anywhere in the
    lowercased host name of the Origin. *)
let allowed_origins : Re.re list ref = ref []

(* Gotify's isAllowedOrigin: no Origin, the request's own host, or an allowed
   host name. *)
let origin_allowed reqd =
  match Http.header reqd "origin" with
  | None | Some "" -> true
  | Some origin ->
      let u = Uri.of_string origin in
      let name = Option.value (Uri.host u) ~default:"" in
      let host = match Uri.port u with Some p -> Printf.sprintf "%s:%d" name p | None -> name in
      let own = Option.value (Http.header reqd "host") ~default:"" in
      String.lowercase_ascii host = String.lowercase_ascii own
      || List.exists (fun r -> Re.execp r (String.lowercase_ascii name)) !allowed_origins

(** Answers the upgrade and keeps the socket until the client goes: ache runs
    the session and closes the socket when it ends. A socket from another
    site is refused, as gorilla/websocket refuses it. *)
let serve reqd ~user_id ~token =
  if not (origin_allowed reqd) then
    Http.respond reqd
      ~headers:
        [
          ("content-type", "text/plain; charset=utf-8");
          ("sec-websocket-version", "13");
          ("x-content-type-options", "nosniff");
        ]
      403 "Forbidden\n"
  else
  let session ws =
    let s = { token; ws } in
    add user_id s;
    let rec drain () =
      Lwt.bind (Ache_lwt.Websocket.receive ws) (function Some _ -> drain () | None -> Lwt.return_unit)
    in
    let rec ping () =
      Lwt.bind (Lwt_unix.sleep ping_period) (fun () -> Lwt.bind (Ache_lwt.Websocket.ping ws "") ping)
    in
    Lwt.finalize
      (fun () -> Lwt.catch (fun () -> Lwt.pick [ drain (); ping () ]) (fun _ -> Lwt.return_unit))
      (fun () -> remove user_id s; Lwt.return_unit)
  in
  match Ache_lwt.Websocket.upgrade ~max_message_size:64 reqd session with
  | Ok () -> ()
  | Error `Unsupported_version ->
      Http.respond reqd ~headers:[ ("sec-websocket-version", "13") ] 426 "Upgrade Required"
  | Error _ -> Http.error reqd 400 "Bad Request"
