(* Authentication as Gotify 3.1.1 does it (auth/authentication.go): a token
   from ?token=, X-Gotify-Key or "Authorization: Bearer", or basic auth. Each
   check gives Skip (try the next), a refusal, or the identity. *)

open Model

type identity =
  | User of user  (** basic auth *)
  | Client of client * user
  | App of application * user

let user_of = function User u | Client (_, u) | App (_, u) -> u
let client_of = function Client (c, _) -> Some c | _ -> None

type state = Skip | Ok of identity | Refuse of int * string

let not_allowed = Refuse (403, "you are not allowed to access this api")
let not_elevated = Refuse (403, "session not elevated, use basic auth or call /client:elevate")
let unauthorized = (401, "you need to provide a valid access token or user credentials to access this api")

let token reqd =
  match List.assoc_opt "token" (Http.query reqd) with
  | Some t when t <> "" -> Some t
  | _ -> (
      match Http.header reqd "x-gotify-key" with
      | Some t when t <> "" -> Some t
      | _ -> (
          match Http.header reqd "authorization" with
          | Some h when String.length h > 7 && String.lowercase_ascii (String.sub h 0 7) = "bearer " ->
              Some (String.sub h 7 (String.length h - 7))
          | _ -> None))

let basic reqd =
  match Http.header reqd "authorization" with
  | Some h when String.length h > 6 && String.lowercase_ascii (String.sub h 0 6) = "basic " -> (
      match Base64.decode (String.trim (String.sub h 6 (String.length h - 6))) with
      | Error _ -> None
      | Ok pair -> (
          match String.index_opt pair ':' with
          | Some i -> Some (String.sub pair 0 i, String.sub pair (i + 1) (String.length pair - i - 1))
          | None -> None))
  | _ -> None

let check_password (u : user) pass = try Bcrypt.verify pass (Bcrypt.hash_of_string u.pass) with _ -> false
let hash_password pass = Bcrypt.string_of_hash (Bcrypt.hash pass)

(* bcrypt reads at most 72 bytes; Gotify refuses longer passwords. *)
let validate_password pass =
  if pass = "" then Error "password must not be empty"
  else if String.length pass > 72 then Error "bcrypt: password length exceeds 72 bytes"
  else Result.Ok ()

let ( let* ) = Lwt.bind

let login name pass =
  let* u = Db.user_by_name name in
  Lwt.return (match u with Some u when check_password u pass -> Some u | _ -> None)

let by_user ?(check = fun _ -> true) reqd =
  match basic reqd with
  | None -> Lwt.return Skip
  | Some (name, pass) -> (
      let* u = login name pass in
      match u with
      | None -> Lwt.return Skip
      | Some u -> Lwt.return (if check u then Ok (User u) else not_allowed))

(* last_used is written at most every five minutes, as Gotify does. *)
let stale last = match last with None -> true | Some t -> t +. 300. < Unix.gettimeofday ()

let canonical_token reqd =
  match token reqd with
  | None -> Result.Ok None
  | Some t -> ( match Token.canonical t with Result.Ok c -> Result.Ok (Some c) | Error () -> Error ())

let by_client ?(admin = false) ?(elevated = false) reqd =
  match canonical_token reqd with
  | Error () -> Lwt.return (Refuse (401, "cannot parse token"))
  | Result.Ok None -> Lwt.return Skip
  | Result.Ok (Some t) -> (
      let* c = Db.client_by_token t in
      match c with
      | None -> Lwt.return Skip
      | Some c -> (
          let* () = if stale c.last_used then Db.client_used c.id else Lwt.return_unit in
          let* u = Db.user_by_id c.user_id in
          match u with
          | None -> Lwt.return not_allowed
          | Some u ->
              Lwt.return
                (if admin && not u.admin then not_allowed
                 else if elevated && not (Model.elevated ~now:(Unix.gettimeofday ()) c) then not_elevated
                 else Ok (Client (c, u)))))

let by_app reqd =
  match canonical_token reqd with
  | Error () -> Lwt.return (Refuse (401, "cannot parse token"))
  | Result.Ok None -> Lwt.return Skip
  | Result.Ok (Some t) -> (
      let* a = Db.app_by_token t in
      match a with
      | None -> Lwt.return Skip
      | Some a -> (
          let* () = if stale a.last_used then Db.app_used a.id else Lwt.return_unit in
          let* u = Db.user_by_id a.user_id in
          match u with None -> Lwt.return not_allowed | Some u -> Lwt.return (Ok (App (a, u)))))

let rec first = function
  | [] -> Lwt.return Skip
  | f :: rest -> ( let* s = f () in match s with Skip -> first rest | s -> Lwt.return s)

let finish s =
  Lwt.return
    (match s with
    | Ok i -> Result.Ok i
    | Refuse (code, m) -> Error (code, m)
    | Skip -> Error unauthorized)

let client reqd = Lwt.bind (first [ (fun () -> by_user reqd); (fun () -> by_client reqd) ]) finish

let elevated_client reqd =
  Lwt.bind (first [ (fun () -> by_user reqd); (fun () -> by_client ~elevated:true reqd) ]) finish

let admin_handlers reqd =
  first [ (fun () -> by_user ~check:(fun u -> u.admin) reqd); (fun () -> by_client ~admin:true ~elevated:true reqd) ]

let admin reqd = Lwt.bind (admin_handlers reqd) finish

(** POST /user: no credentials is allowed (registration), wrong ones are not. *)
let optional_admin reqd =
  Lwt.map
    (function Skip -> Result.Ok None | Ok i -> Result.Ok (Some i) | Refuse (c, m) -> Error (c, m))
    (admin_handlers reqd)

let app_or_client reqd =
  Lwt.bind (first [ (fun () -> by_app reqd); (fun () -> by_client reqd); (fun () -> by_user reqd) ]) finish
