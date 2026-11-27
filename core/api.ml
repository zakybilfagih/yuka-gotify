(* The REST API of Gotify 3.1.1 (router/router.go), minus plugins (GET /plugin
   answers an empty list) and OIDC. [dispatch] answers a request whose path is one of Gotify's and
   returns true; any other path is left to the Yuka app. *)

open Model
module H = Http

let ( let* ) = Lwt.bind
let version = "3.1.1-yuka"
let registration = ref false
let elevation = 3600.

type ctx = { reqd : Ache.Request_descriptor.t; segs : string list }

let ok_json c v = H.json c.reqd v; Lwt.return_unit
let fail c code m = H.error c.reqd code m; Lwt.return_unit
let ok_empty c = H.empty c.reqd 200; Lwt.return_unit

let with_auth check c f =
  let* r = check c.reqd in
  match r with Ok i -> f i | Error (code, m) -> fail c code m

let with_body c f =
  let* b = H.body c.reqd in
  match b with
  | Ok b -> f b
  | Error `Too_large -> fail c 413 "request body too large"
  | Error (`Json m) -> fail c 400 m
  | Error `Bad -> fail c 400 "invalid request body"

let with_id c s f = match int_of_string_opt s with Some id when id >= 0 -> f id | _ -> fail c 400 "invalid id"
let required name = Printf.sprintf "Field '%s' is required" name

(* Paging *)

let paged c ~path fetch =
  let q = H.query c.reqd in
  let num name default =
    match List.assoc_opt name q with None -> Ok default | Some s -> Option.to_result ~none:name (int_of_string_opt s)
  in
  match (num "limit" 100, num "since" 0) with
  | Error n, _ | _, Error n -> fail c 400 (Printf.sprintf "Field '%s' is not valid" n)
  | Ok limit, _ when limit < 1 -> fail c 400 "Field 'limit' must be more or equal to 1"
  | Ok limit, _ when limit > 200 -> fail c 400 "Field 'limit' must be less or equal to 200"
  | _, Ok since when since < 0 -> fail c 400 "Field 'since' must be more or equal to 0"
  | Ok limit, Ok since ->
      let* ms = fetch ~limit:(limit + 1) ~since in
      let ms, next =
        if List.length ms > limit then
          let ms = List.filteri (fun i _ -> i < limit) ms in
          let last = (List.nth ms (limit - 1)).id in
          (ms, Some last)
        else (ms, None)
      in
      ok_json c
        (`Assoc
          [
            ( "paging",
              `Assoc
                ((match next with
                 | Some s -> [ ("next", `String (Printf.sprintf "%s?limit=%d&since=%d" path limit s)) ]
                 | None -> [])
                @ [
                    ("size", `Int (List.length ms));
                    ("since", `Int (Option.value next ~default:0));
                    ("limit", `Int limit);
                  ]) );
            ("messages", `List (List.map message_json ms));
          ])

(* Messages *)

let create_message c =
  with_auth Auth.app_or_client c (fun id ->
      with_body c (fun b ->
          let user = Auth.user_of id in
          match H.string_field b "message" with
          | None | Some "" -> fail c 400 (required "message")
          | Some message -> (
              let priority = H.int_field b "priority" in
              let* app =
                match id with
                | Auth.App (a, _) -> Lwt.return (Ok a)
                | _ -> (
                    match H.int_field b "appid" with
                    | None | Some (Ok 0) ->
                        Lwt.return (Error "appid is required when not authenticating with an application token")
                    | Some (Error ()) -> Lwt.return (Error "Field 'appid' is not valid")
                    | Some (Ok appid) ->
                        let* a = Db.app_by_id appid in
                        Lwt.return
                          (match a with Some a when a.user_id = user.id -> Ok a | _ -> Error "appid not found"))
              in
              match (app, priority) with
              | Error m, _ -> fail c 400 m
              | _, Some (Error ()) -> fail c 400 "Field 'priority' is not valid"
              | Ok app, priority ->
                  let title =
                    match H.string_field b "title" with Some t when String.trim t <> "" -> t | _ -> app.name
                  in
                  let priority = match priority with Some (Ok p) -> p | _ -> app.default_priority in
                  let extras = match H.field b "extras" with Some (`Assoc _ as e) -> Some e | _ -> None in
                  let* m = Db.create_message ~app_id:app.id ~message ~title ~priority ~extras in
                  Stream.publish user.id m;
                  ok_json c (message_json m))))

let own_app c (u : user) id (f : application -> unit Lwt.t) =
  let* a = Db.app_by_id id in
  match a with Some a when a.user_id = u.id -> f a | _ -> fail c 404 (Printf.sprintf "app with id %d doesn't exists" id)

let own_client c (u : user) id (f : client -> unit Lwt.t) =
  let* cl = Db.client_by_id id in
  match cl with
  | Some cl when cl.user_id = u.id -> f cl
  | _ -> fail c 404 (Printf.sprintf "client with id %d doesn't exists" id)

(* Applications *)

let app_params b =
  match H.string_field b "name" with
  | None | Some "" -> Error (required "name")
  | Some name -> (
      match H.int_field b "defaultPriority" with
      | Some (Error ()) -> Error "Field 'defaultPriority' is not valid"
      | p ->
          Ok
            ( name,
              Option.value (H.string_field b "description") ~default:"",
              (match p with Some (Ok p) -> p | _ -> 0),
              H.string_field b "sortKey" ))

let create_app c (u : user) =
  with_body c (fun b ->
      match app_params b with
      | Error m -> fail c 400 m
      | Ok (name, description, default_priority, sort_key) ->
          let public, secret = Token.application () in
          Lwt.catch
            (fun () ->
              let* a = Db.create_app ~user_id:u.id ~name ~description ~default_priority ?sort_key public in
              ok_json c (application_json ~token:secret a))
            (fun _ -> fail c 400 "sort key is not unique"))

let update_app c u id =
  own_app c u id (fun a ->
      with_body c (fun b ->
          match app_params b with
          | Error m -> fail c 400 m
          | Ok (name, description, default_priority, sort_key) ->
              let a =
                {
                  a with
                  name;
                  description;
                  default_priority;
                  sort_key = (match sort_key with Some k when k <> "" -> k | _ -> a.sort_key);
                }
              in
              Lwt.catch
                (fun () ->
                  let* () = Db.update_app a in
                  ok_json c (application_json a))
                (fun _ -> fail c 400 "sort key is not unique")))

let app_security c u id =
  own_app c u id (fun a ->
      with_body c (fun b ->
          if H.bool_field b "regenerateToken" = Some true then
            let public, secret = Token.application () in
            let* () = Db.update_app { a with token = public } in
            ok_json c (`Assoc [ ("regenerateToken", `Assoc [ ("token", `String secret) ]) ])
          else ok_json c (`Assoc [])))

(* POST /application/:id/image: the file of the multipart field "file"
   becomes the image, and the old file is removed. *)
let upload_image c (a : application) =
  let* body = H.read_body ~limit:Images.max_size c.reqd in
  match body with
  | Error `Too_large -> fail c 413 "request body too large"
  | Error `Bad -> fail c 400 "invalid request body"
  | Ok body -> (
      let file =
        Option.bind (Option.bind (H.header c.reqd "content-type") H.boundary_of) (fun boundary ->
            List.find_map (fun (field, filename, data) -> if field = "file" then Some (filename, data) else None) (H.files ~boundary body))
      in
      match file with
      | None -> fail c 400 "file with key 'file' must be present"
      | Some (filename, data) -> (
          match Images.save ~filename data with
          | Error m -> fail c 400 m
          | Ok name ->
              let* () = Db.set_app_image a.id name in
              Images.remove a.image;
              Stream.changed a.user_id;
              ok_json c (application_json { a with image = name })))

(* Clients *)

let client_params b =
  match H.string_field b "name" with
  | None | Some "" -> Error (required "name")
  | Some name -> (
      match H.int_field b "expiresAfterInactivitySeconds" with
      | Some (Error ()) -> Error "Field 'expiresAfterInactivitySeconds' is not valid"
      | Some (Ok n) when n < 0 -> Error "Field 'expiresAfterInactivitySeconds' is not valid"
      | e -> Ok (name, match e with Some (Ok n) -> Some n | _ -> None))

let now = Unix.gettimeofday

let delete_client (cl : client) =
  Stream.client_deleted cl.user_id cl.token;
  Db.delete_client cl.id

let current_user_json ?(client : client option) (u : user) =
  let base = match user_json u with `Assoc kv -> kv | _ -> [] in
  `Assoc
    (base
    @
    match client with
    | Some cl ->
        (("clientId", `Int cl.id) :: (if Model.elevated ~now:(now ()) cl then [ ("elevatedUntil", time_opt cl.elevated_until) ] else []))
    | None -> [])

(* Users *)

let user_params ~pass_required b =
  match (H.string_field b "name", H.string_field b "pass") with
  | (None | Some ""), _ -> Error (required "name")
  | _, (None | Some "") when pass_required -> Error (required "pass")
  | Some name, pass -> Ok (name, Option.value (H.bool_field b "admin") ~default:false, pass)

let hash_new pass = Result.map (fun () -> Auth.hash_password pass) (Auth.validate_password pass)

let create_user c who =
  with_body c (fun b ->
      match user_params ~pass_required:true b with
      | Error m -> fail c 400 m
      | Ok (name, admin, pass) -> (
          match hash_new (Option.get pass) with
          | Error m -> fail c 400 m
          | Ok pass -> (
              if who = None && not !registration then fail c 401 "you are not allowed to access this api"
              else if who = None && admin then fail c 401 "you are not allowed to create an admin user"
              else
                let* existing = Db.user_by_name name in
                match existing with
                | Some _ -> fail c 400 "username already exists"
                | None ->
                    let* u = Db.create_user ~name ~pass ~admin in
                    ok_json c (user_json u))))

let update_user c id =
  with_body c (fun b ->
      match user_params ~pass_required:false b with
      | Error m -> fail c 400 m
      | Ok (name, admin, pass) -> (
          let* u = Db.user_by_id id in
          match u with
          | None -> fail c 404 "user does not exist"
          | Some u -> (
              let* admins = Db.count_admins () in
              if (not admin) && u.admin && admins = 1 then fail c 400 "cannot delete last admin"
              else
                let pass = match pass with Some p when p <> "" -> Some (hash_new p) | _ -> None in
                match pass with
                | Some (Error m) -> fail c 400 m
                | pass ->
                    let u = { u with name; admin; pass = (match pass with Some (Ok p) -> p | _ -> u.pass) } in
                    Lwt.catch
                      (fun () ->
                        let* () = Db.update_user u in
                        ok_json c (user_json u))
                      (fun _ -> fail c 400 "username already exists"))))

let delete_user c id =
  let* u = Db.user_by_id id in
  match u with
  | None -> fail c 404 "user does not exist"
  | Some u ->
      let* admins = Db.count_admins () in
      if u.admin && admins = 1 then fail c 400 "cannot delete last admin"
      else (
        let* clients = Db.clients_of_user id in
        let* () = Lwt_list.iter_s delete_client clients in
        Stream.user_deleted id;
        let* () = Db.delete_user id in
        ok_empty c)

(* Sessions *)

let login c =
  match Auth.basic c.reqd with
  | None -> fail c 401 "basic auth required"
  | Some (name, pass) -> (
      let* u = Auth.login name pass in
      match u with
      | None -> fail c 401 "invalid credentials"
      | Some u ->
          with_body c (fun b ->
              match H.string_field b "name" with
              | None | Some "" -> fail c 400 (required "name")
              | Some cname ->
                  let public, secret = Token.client () in
                  let* cl =
                    Db.create_client ~user_id:u.id ~name:cname ~elevated_until:(now () +. elevation)
                      ~expires_after:(7 * 24 * 3600) public
                  in
                  (* As Gotify, the token is in the cookie only. *)
                  Auth.set_cookie c.reqd secret;
                  ok_json c (current_user_json ~client:cl u)))

let route c =
  let m = (H.request c.reqd).meth in
  let path, _ = H.path_and_query c.reqd in
  let path = "/" ^ String.concat "/" (H.segments path) in
  match (m, c.segs) with
  | (`GET | `HEAD), [ "health" ] ->
      Lwt.catch
        (fun () ->
          let* _ = Db.ping () in
          ok_json c (`Assoc [ ("health", `String "green"); ("database", `String "green") ]))
        (fun _ -> H.json c.reqd ~code:500 (`Assoc [ ("health", `String "orange"); ("database", `String "red") ]); Lwt.return_unit)
  | `GET, [ "version" ] ->
      ok_json c (`Assoc [ ("version", `String version); ("commit", `String "yuka"); ("buildDate", `String "") ])
  | `GET, [ "gotifyinfo" ] ->
      ok_json c
        (`Assoc
          [
            ("version", `String version);
            ("register", `Bool !registration);
            ("localAuth", `Bool true);
            ("oidc", `Bool false);
            ("oidcIdpName", `String "");
            ("oidcAutoRedirect", `Bool false);
          ])
  | `POST, [ "auth"; "local"; "login" ] -> login c
  | `POST, [ "auth"; "logout" ] ->
      with_auth Auth.client c (fun id ->
          Auth.set_cookie c.reqd ~max_age:(-1) "";
          match Auth.client_of id with
          | None -> fail c 403 "no client auth provided"
          | Some cl ->
              let* () = delete_client cl in
              ok_empty c)
  | `POST, [ "message" ] -> create_message c
  | `GET, [ "message" ] ->
      with_auth Auth.client c (fun id ->
          paged c ~path (fun ~limit ~since -> Db.messages_of_user ~limit ~since (Auth.user_of id).id))
  | `DELETE, [ "message" ] ->
      with_auth Auth.client c (fun id ->
          let uid = (Auth.user_of id).id in
          let* () = Db.delete_user_messages uid in
          Stream.changed uid;
          ok_empty c)
  | `DELETE, [ "message"; mid ] ->
      with_auth Auth.client c (fun id ->
          with_id c mid (fun mid ->
              let u = Auth.user_of id in
              let* msg = Db.message_by_id mid in
              let* app = match msg with Some m -> Db.app_by_id m.app_id | None -> Lwt.return None in
              match app with
              | Some a when a.user_id = u.id ->
                  let* () = Db.delete_message mid in
                  Stream.changed u.id;
                  ok_empty c
              | _ -> fail c 404 "message does not exist"))
  | `GET, [ "application" ] ->
      with_auth Auth.client c (fun id ->
          let* apps = Db.apps_of_user (Auth.user_of id).id in
          ok_json c (`List (List.map (fun a -> application_json a) apps)))
  | `POST, [ "application" ] -> with_auth Auth.client c (fun id -> create_app c (Auth.user_of id))
  | `PUT, [ "application"; aid ] ->
      with_auth Auth.client c (fun id -> with_id c aid (update_app c (Auth.user_of id)))
  | `DELETE, [ "application"; aid ] ->
      with_auth Auth.elevated_client c (fun id ->
          with_id c aid (fun aid ->
              own_app c (Auth.user_of id) aid (fun a ->
                  if a.internal then fail c 400 "cannot delete internal application"
                  else
                    let* () = Db.delete_app a in
                    Stream.changed a.user_id;
                    ok_empty c)))
  | `POST, [ "application"; aid; "image" ] ->
      with_auth Auth.client c (fun id -> with_id c aid (fun aid -> own_app c (Auth.user_of id) aid (upload_image c)))
  | `DELETE, [ "application"; aid; "image" ] ->
      with_auth Auth.client c (fun id ->
          with_id c aid (fun aid ->
              own_app c (Auth.user_of id) aid (fun a ->
                  if a.image = "" then fail c 400 (Printf.sprintf "app with id %d does not have a customized image" aid)
                  else
                    let* () = Db.set_app_image a.id "" in
                    Images.remove a.image;
                    Stream.changed a.user_id;
                    ok_json c (application_json { a with image = "" }))))
  | (`GET | `HEAD), [ "image"; name ] ->
      Images.serve c.reqd name;
      Lwt.return_unit
  | `PUT, [ "application"; aid; "security" ] ->
      with_auth Auth.elevated_client c (fun id -> with_id c aid (app_security c (Auth.user_of id)))
  | `GET, [ "application"; aid; "message" ] ->
      with_auth Auth.client c (fun id ->
          with_id c aid (fun aid ->
              let* a = Db.app_by_id aid in
              match a with
              | Some a when a.user_id = (Auth.user_of id).id ->
                  paged c ~path (fun ~limit ~since -> Db.messages_of_app ~limit ~since aid)
              | _ -> fail c 404 "application does not exist"))
  | `DELETE, [ "application"; aid; "message" ] ->
      with_auth Auth.client c (fun id ->
          with_id c aid (fun aid ->
              let* a = Db.app_by_id aid in
              match a with
              | Some a when a.user_id = (Auth.user_of id).id ->
                  let* () = Db.delete_app_messages aid in
                  Stream.changed a.user_id;
                  ok_empty c
              | _ -> fail c 404 "application does not exists"))
  | `GET, [ "client" ] ->
      with_auth Auth.client c (fun id ->
          let* cs = Db.clients_of_user (Auth.user_of id).id in
          let now = now () in
          ok_json c (`List (List.map (client_json ~now) cs)))
  | `POST, [ "client" ] ->
      with_auth Auth.client c (fun id ->
          with_body c (fun b ->
              match client_params b with
              | Error m -> fail c 400 m
              | Ok (name, expires_after) ->
                  let public, secret = Token.client () in
                  let* cl = Db.create_client ~user_id:(Auth.user_of id).id ~name ?expires_after public in
                  ok_json c (client_json ~token:secret ~now:(now ()) cl)))
  | `PUT, [ "client"; cid ] ->
      with_auth Auth.client c (fun id ->
          with_id c cid (fun cid ->
              own_client c (Auth.user_of id) cid (fun cl ->
                  with_body c (fun b ->
                      match client_params b with
                      | Error m -> fail c 400 m
                      | Ok (name, expires_after) ->
                          let cl = { cl with name; expires_after = Option.value expires_after ~default:cl.expires_after } in
                          let* () = Db.update_client cl in
                          ok_json c (client_json ~now:(now ()) cl)))))
  | `DELETE, [ "client"; cid ] ->
      with_auth Auth.elevated_client c (fun id ->
          with_id c cid (fun cid ->
              own_client c (Auth.user_of id) cid (fun cl ->
                  let* () = delete_client cl in
                  ok_empty c)))
  | `POST, [ "client"; cid; "elevate" ] ->
      with_auth Auth.elevated_client c (fun id ->
          with_id c cid (fun cid ->
              with_body c (fun b ->
                  match H.int_field b "durationSeconds" with
                  | None | Some (Ok 0) -> fail c 400 (required "durationSeconds")
                  | Some (Error ()) -> fail c 400 "Field 'durationSeconds' is not valid"
                  | Some (Ok secs) -> (
                      let* cl = Db.client_by_id cid in
                      match cl with
                      | Some cl when cl.user_id = (Auth.user_of id).id ->
                          let* () = Db.update_client { cl with elevated_until = Some (now () +. float_of_int secs) } in
                          H.empty c.reqd 204;
                          Lwt.return_unit
                      | _ -> fail c 404 "client not found"))))
  | `GET, [ "stream" ] ->
      with_auth Auth.client c (fun id ->
          let token = match Auth.client_of id with Some cl -> cl.token | None -> "" in
          Stream.serve c.reqd ~user_id:(Auth.user_of id).id ~token;
          Lwt.return_unit)
  (* No plugins: the list Gotify answers for a user who has none. *)
  | `GET, [ "plugin" ] -> with_auth Auth.client c (fun _ -> ok_json c (`List []))
  | `GET, [ "current"; "user" ] ->
      with_auth Auth.client c (fun id -> ok_json c (current_user_json ?client:(Auth.client_of id) (Auth.user_of id)))
  | `POST, [ "current"; "user"; "password" ] ->
      with_auth Auth.elevated_client c (fun id ->
          with_body c (fun b ->
              match H.string_field b "pass" with
              | None | Some "" -> fail c 400 (required "pass")
              | Some p -> (
                  match hash_new p with
                  | Error m -> fail c 400 m
                  | Ok pass ->
                      let* () = Db.update_user { (Auth.user_of id) with pass } in
                      ok_empty c)))
  | `GET, [ "user" ] ->
      with_auth Auth.admin c (fun _ ->
          let* us = Db.users () in
          ok_json c (`List (List.map user_json us)))
  | `POST, [ "user" ] -> (
      let* who = Auth.optional_admin c.reqd in
      match who with Error (code, m) -> fail c code m | Ok who -> create_user c who)
  | `GET, [ "user"; uid ] ->
      with_auth Auth.admin c (fun _ ->
          with_id c uid (fun uid ->
              let* u = Db.user_by_id uid in
              match u with Some u -> ok_json c (user_json u) | None -> fail c 404 "user does not exist"))
  | `POST, [ "user"; uid ] -> with_auth Auth.admin c (fun _ -> with_id c uid (update_user c))
  | `DELETE, [ "user"; uid ] -> with_auth Auth.admin c (fun _ -> with_id c uid (delete_user c))
  | `OPTIONS, _ -> ok_empty c
  | _ -> fail c 404 "page not found"

let api_roots =
  [ "health"; "version"; "gotifyinfo"; "auth"; "message"; "application"; "client"; "stream"; "current"; "user"; "image"; "plugin" ]

let dispatch reqd =
  let path, _ = H.path_and_query reqd in
  let segs = H.segments path in
  match segs with
  | root :: _ when List.mem root api_roots ->
      (* Gotify registers /health and /image before its header and CORS
         middleware. *)
      let cors = root <> "health" && root <> "image" in
      Lwt.async (fun () ->
          Lwt.finalize
            (fun () ->
              Lwt.catch
                (fun () -> if (not cors) || Cors.apply reqd then route { reqd; segs } else Lwt.return_unit)
                (fun e ->
                  prerr_endline ("gotify api: " ^ Printexc.to_string e);
                  (try H.error reqd 500 "internal error" with _ -> ());
                  Lwt.return_unit))
            (fun () -> H.forget reqd; Lwt.return_unit));
      true
  | _ -> false

(** Removes clients that have been idle longer than their limit. *)
let rec sweep () =
  let* () = Lwt_unix.sleep 300. in
  let* expired = Lwt.catch Db.expired_clients (fun _ -> Lwt.return []) in
  let* () = Lwt_list.iter_s delete_client expired in
  sweep ()

let create_default_user ~name ~pass =
  let* users = Db.users () in
  if users <> [] then Lwt.return_unit
  else
    let* _ = Db.create_user ~name ~pass:(Auth.hash_password pass) ~admin:true in
    Lwt.return_unit
