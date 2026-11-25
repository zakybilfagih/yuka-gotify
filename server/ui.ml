(* The web UI session: a Gotify client made at login, whose id is in a
   cookie signed with the Yuka_server secret. Deleting that client (here,
   from the REST API, or by expiry) logs the browser out. *)

open Gotify_core
module M = Gotify_core.Model

let ( let* ) = Lwt.bind
let sessions : Yuka_server.sessions option ref = ref None
let secure = ref false
let cookie_name = "gotify-ui"
let theme_cookie = "gotify-theme"
let the_sessions () = match !sessions with Some s -> s | None -> failwith "Ui.sessions not set"

type session = { user : M.user; client : M.client; apps : M.application list; theme : string }

let client_id req =
  Option.bind (Yuka_server.Signed_cookie.get (the_sessions ()) req cookie_name) int_of_string_opt

(* Signed-in client id -> (user id, stored token), for the synchronous live
   [allow]. Filled by [find], emptied when the client is deleted. *)
let known : (int, int * string) Hashtbl.t = Hashtbl.create 16

let forget_token token =
  let ids = Hashtbl.fold (fun id (_, t) acc -> if t = token then id :: acc else acc) known [] in
  List.iter
    (fun id ->
      Hashtbl.remove known id;
      Yuka.Server.Live.revoke (fun r -> client_id r = Some id))
    ids

let () = Stream.on_client_deleted forget_token

(* Fills [known] with every client at startup, so that the live sockets of
   pages opened before a restart are allowed again when they reconnect. *)
let load_known () =
  let* users = Db.users () in
  Lwt_list.iter_s
    (fun (u : M.user) ->
      let* clients = Db.clients_of_user u.id in
      List.iter (fun (c : M.client) -> Hashtbl.replace known c.id (u.id, c.token)) clients;
      Lwt.return_unit)
    users

let user_of_request req =
  match client_id req with Some id -> Option.map fst (Hashtbl.find_opt known id) | None -> None

let theme req =
  match Yuka.Server.Request.cookie req theme_cookie with Some ("light" | "dark" as t) -> t | _ -> "system"

(** The session of the request, or [None]. *)
let find req =
  match client_id req with
  | None -> Lwt.return None
  | Some id -> (
      let* c = Db.client_by_id id in
      match c with
      | None -> Hashtbl.remove known id; Lwt.return None
      | Some client -> (
          let* () = if Auth.stale client.last_used then Db.client_used client.id else Lwt.return_unit in
          let* u = Db.user_by_id client.user_id in
          match u with
          | None -> Lwt.return None
          | Some user ->
              Hashtbl.replace known client.id (user.id, client.token);
              let* apps = Db.apps_of_user user.id in
              Lwt.return (Some { user; client; apps; theme = theme req })))

(* How long a password confirmation elevates a session, as Gotify 3.1.1. *)
let elevation = 3600.
let elevated s = M.elevated ~now:(Unix.gettimeofday ()) s.client

(* Answers *)

let redirect ?(headers = []) location =
  Yuka.Server.response ~status:303 ~headers:(("location", location) :: headers) (Yuka.Server.String "")

let with_alert path message = Yuka.Router.with_query path [ ("alert", message) ]

(** A local path to go back to, never another site. *)
let safe_next = function
  | Some n when String.length n > 0 && n.[0] = '/' && not (String.length n > 1 && (n.[1] = '/' || n.[1] = '\\')) -> n
  | _ -> "/"

let form req =
  Yuka.Async.map
    (function Ok body -> Http.parse_urlencoded body | Error `Too_large -> [])
    (Yuka.Server.Request.body req ~limit:65536)

let field fields name = Option.value (List.assoc_opt name fields) ~default:""

(** Runs [f] with the session and the posted fields; without a session,
    back to the login page. *)
let posted req f =
  Yuka.Async.bind (Yuka_server.to_async (find req)) (function
    | None -> redirect "/login"
    | Some s -> Yuka.Async.bind (form req) (fun fields -> f s fields))

let lwt p = Yuka.Async.bind (Yuka_server.to_async p) Fun.id

(** As [posted], for an action that Gotify only allows to an elevated
    session. A session that is not elevated is elevated for an hour by the
    password in the [elevate_pass] field of the form (Pages.elevation_field);
    with a wrong one, back to [back], the form, with an alert; a form with
    no such field goes through /elevate. *)
let posted_elevated req ~back f =
  posted req (fun s fields ->
      if elevated s then f s fields
      else
        match List.assoc_opt "elevate_pass" fields with
        | None -> redirect (Yuka.Router.with_query "/elevate" [ ("next", back) ])
        | Some pass when Auth.check_password s.user pass ->
            let client = { s.client with elevated_until = Some (Unix.gettimeofday () +. elevation) } in
            lwt (Lwt.map (fun () -> f { s with client } fields) (Db.update_client client))
        | Some _ -> redirect (with_alert back "Wrong password"))

(* Tokens shown once, after a create or a rotation: kept two minutes for
   the user who made them, under a random key in the redirect URL. *)
let reveals : (string, int * string * string * float) Hashtbl.t = Hashtbl.create 8

let reveal ~user ~name token =
  let now = Unix.gettimeofday () in
  Hashtbl.filter_map_inplace (fun _ ((_, _, _, t) as v) -> if t > now then Some v else None) reveals;
  let key = Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet (Mirage_crypto_rng.generate 12) in
  Hashtbl.replace reveals key (user, name, token, now +. 120.);
  key

let revealed (s : session) = function
  | None -> None
  | Some key -> (
      match Hashtbl.find_opt reveals key with
      | Some (user, name, token, until) when user = s.user.id && until > Unix.gettimeofday () -> Some (name, token)
      | _ -> None)

let date t =
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d UTC" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min

let date_opt = function Some t -> date t | None -> "never"
