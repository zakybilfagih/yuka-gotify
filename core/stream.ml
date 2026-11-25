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
          Lwt.catch (fun () -> Ache_lwt.Websocket.send s.ws text) (fun _ -> remove uid s; Lwt.return_unit)))
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

(** Answers the upgrade and keeps the socket until the client goes. *)
let serve reqd ~user_id ~token =
  match Ache_lwt.Websocket.upgrade ~max_message_size:64 reqd with
  | Error `Unsupported_version ->
      Http.respond reqd ~headers:[ ("sec-websocket-version", "13") ] 426 "Upgrade Required"
  | Error _ -> Http.error reqd 400 "Bad Request"
  | Ok ws ->
      let s = { token; ws } in
      add user_id s;
      let rec drain () =
        Lwt.bind (Ache_lwt.Websocket.receive ws) (function Some _ -> drain () | None -> Lwt.return_unit)
      in
      Lwt.async (fun () ->
          Lwt.finalize
            (fun () -> Lwt.catch drain (fun _ -> Lwt.return_unit))
            (fun () -> remove user_id s; Lwt.return_unit))
