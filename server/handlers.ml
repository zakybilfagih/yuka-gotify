(* The POST forms of the web UI. Each answers 303: back to its page, with
   ?alert= on bad input, so every form works without JavaScript. *)

open Gotify_core
module Routes = App_shared.Routes
module M = Gotify_core.Model

let ( let* ) = Lwt.bind
let redirect = Ui.redirect
let back path message = redirect (Ui.with_alert path message)
let elevation = Ui.elevation

let login req =
  Yuka.Async.bind (Ui.form req) (fun fields ->
      Ui.lwt
        (let* u = Auth.login (Ui.field fields "name") (Ui.field fields "pass") in
         match u with
         | None -> Lwt.return (redirect "/login?failed=true")
         | Some u ->
             let public, _secret = Token.client () in
             let* c =
               Db.create_client ~user_id:u.id ~name:"Gotify web (Yuka)"
                 ~elevated_until:(Unix.gettimeofday () +. elevation) ~expires_after:(30 * 24 * 3600) public
             in
             let cookie =
               Yuka_server.Signed_cookie.set (Ui.the_sessions ()) ~secure:!Ui.secure ~same_site:`Lax
                 ~max_age:(30 * 24 * 3600) Ui.cookie_name (string_of_int c.id)
             in
             Lwt.return (match cookie with Ok h -> redirect ~headers:[ h ] "/" | Error m -> Yuka.Server.text ~status:500 m)))

let delete_client (c : M.client) = Stream.client_deleted c.user_id c.token; Db.delete_client c.id

let logout req =
  Ui.posted req (fun s _ ->
      Ui.lwt
        (let* () = delete_client s.client in
         let headers = match Yuka.Server.Cookie.drop Ui.cookie_name with Ok h -> [ h ] | Error _ -> [] in
         Lwt.return (redirect ~headers "/login")))

let elevate req =
  Ui.posted req (fun s fields ->
      let next = Ui.safe_next (List.assoc_opt "next" fields) in
      if not (Auth.check_password s.user (Ui.field fields "pass")) then
        redirect (Yuka.Router.with_query "/elevate" [ ("next", next); ("alert", "Wrong password") ])
      else
        Ui.lwt
          (let* () = Db.update_client { s.client with elevated_until = Some (Unix.gettimeofday () +. elevation) } in
           Lwt.return (redirect next)))

(* A new user, from the login page, when GOTIFY_REGISTRATION=true, as POST
   /user with no credentials: never an admin. *)
let register_post req =
  Yuka.Async.bind (Ui.form req) (fun fields ->
      let name = String.trim (Ui.field fields "name") and pass = Ui.field fields "pass" in
      if not !Api.registration then redirect "/login"
      else
        match (name, Auth.validate_password pass) with
        | "", _ -> back "/register" "The name is required"
        | _, Error m -> back "/register" m
        | _, Ok () ->
            Ui.lwt
              (let* existing = Db.user_by_name name in
               match existing with
               | Some _ -> Lwt.return (back "/register" "username already exists")
               | None ->
                   let* _ = Db.create_user ~name ~pass:(Auth.hash_password pass) ~admin:false in
                   Lwt.return (redirect "/login?registered=true")))

let theme req =
  Yuka.Async.bind (Ui.form req) (fun fields ->
      let t = match Ui.field fields "theme" with ("light" | "dark") as t -> t | _ -> "system" in
      let headers =
        match Yuka.Server.Cookie.set ~max_age:(365 * 24 * 3600) ~http_only:false Ui.theme_cookie t with
        | Ok h -> [ h ]
        | Error _ -> []
      in
      redirect ~headers (Ui.safe_next (List.assoc_opt "next" fields)))

let password req =
  Ui.posted_elevated req ~back:"/settings" (fun s fields ->
      let pass = Ui.field fields "pass" in
      match Auth.validate_password pass with
      | Error m -> back "/settings" m
      | Ok () ->
          Ui.lwt
            (let* () = Db.update_user { s.user with pass = Auth.hash_password pass } in
             Lwt.return (back "/settings" "Password changed")))

let priority fields = match int_of_string_opt (String.trim (Ui.field fields "defaultPriority")) with Some p -> p | None -> 0

(* Applications *)

let own_app (s : Ui.session) id = List.find_opt (fun (a : M.application) -> a.id = id) s.apps

let app_create req =
  Ui.posted req (fun s fields ->
      match String.trim (Ui.field fields "name") with
      | "" -> back "/applications" "The name is required"
      | name ->
          Ui.lwt
            (let public, secret = Token.application () in
             let* _ =
               Db.create_app ~user_id:s.user.id ~name ~description:(Ui.field fields "description")
                 ~default_priority:(priority fields) public
             in
             let key = Ui.reveal ~user:s.user.id ~name secret in
             Lwt.return (redirect (Yuka.Router.with_query "/applications" [ ("reveal", key) ]))))

let app_update req ~id =
  Ui.posted req (fun s fields ->
      match (own_app s id, String.trim (Ui.field fields "name")) with
      | None, _ -> back "/applications" "No such application"
      | _, "" -> back "/applications" "The name is required"
      | Some a, name ->
          Ui.lwt
            (let* () =
               Db.update_app { a with name; description = Ui.field fields "description"; default_priority = priority fields }
             in
             Lwt.return (redirect "/applications")))

(* Clients *)

let expires fields =
  match int_of_string_opt (String.trim (Ui.field fields "expiresAfterInactivitySeconds")) with
  | Some n when n >= 0 -> Some n
  | _ -> None

let own_client (s : Ui.session) id =
  Lwt.map (function Some (c : M.client) when c.user_id = s.user.id -> Some c | _ -> None) (Db.client_by_id id)

let client_create req =
  Ui.posted req (fun s fields ->
      match String.trim (Ui.field fields "name") with
      | "" -> back "/clients" "The name is required"
      | name ->
          Ui.lwt
            (let public, secret = Token.client () in
             let* _ = Db.create_client ~user_id:s.user.id ~name ?expires_after:(expires fields) public in
             let key = Ui.reveal ~user:s.user.id ~name secret in
             Lwt.return (redirect (Yuka.Router.with_query "/clients" [ ("reveal", key) ]))))

let client_update req ~id =
  Ui.posted req (fun s fields ->
      Ui.lwt
        (let* c = own_client s id in
         match (c, String.trim (Ui.field fields "name")) with
         | None, _ -> Lwt.return (back "/clients" "No such client")
         | _, "" -> Lwt.return (back "/clients" "The name is required")
         | Some c, name ->
             let* () = Db.update_client { c with name; expires_after = Option.value (expires fields) ~default:c.expires_after } in
             Lwt.return (redirect "/clients")))

(* Users: admins only, and elevated to change anything, as Gotify 3.1.1. *)

let admin req ~back:path f =
  Ui.posted_elevated req ~back:path (fun s fields -> if s.user.admin then f s fields else back "/" "Admins only")

let user_create req =
  admin req ~back:"/users" (fun _ fields ->
      let name = String.trim (Ui.field fields "name") and pass = Ui.field fields "pass" in
      match (name, Auth.validate_password pass) with
      | "", _ -> back "/users" "The name is required"
      | _, Error m -> back "/users" m
      | _, Ok () ->
          Ui.lwt
            (let* existing = Db.user_by_name name in
             match existing with
             | Some _ -> Lwt.return (back "/users" "username already exists")
             | None ->
                 let* _ =
                   Db.create_user ~name ~pass:(Auth.hash_password pass) ~admin:(List.mem_assoc "admin" fields)
                 in
                 Lwt.return (redirect "/users")))

(* The forms of the dialogs that need an elevated session (Actions.confirm and
   Actions.user_edit). Each answers the URL to go to, or the errors to show in
   the dialog: a wrong password stays in it. *)

let alert path message = Lwt.return (Ok (Ui.with_alert path message))
let go path = Lwt.return (Ok path)

let app_rotate (s : Ui.session) id =
  match own_app s id with
  | None -> alert "/applications" "No such application"
  | Some a ->
      let public, secret = Token.application () in
      let* () = Db.update_app { a with token = public } in
      let key = Ui.reveal ~user:s.user.id ~name:a.name secret in
      go (Yuka.Router.with_query "/applications" [ ("reveal", key) ])

let app_delete (s : Ui.session) id =
  match own_app s id with
  | None -> alert "/applications" "No such application"
  | Some a when a.internal -> alert "/applications" "Cannot delete an internal application"
  | Some a ->
      let* () = Db.delete_app a.id in
      Stream.changed s.user.id;
      go "/applications"

let client_delete (s : Ui.session) id =
  let* c = own_client s id in
  match c with
  | None -> alert "/clients" "No such client"
  | Some c ->
      let* () = delete_client c in
      (* Deleting the client of this browser signs it out. *)
      go (if c.id = s.client.id then "/login" else "/clients")

let user_delete (s : Ui.session) id =
  let* u = Db.user_by_id id in
  let* admins = Db.count_admins () in
  match u with
  | None -> alert "/users" "user does not exist"
  | Some u when u.admin && admins = 1 -> alert "/users" "cannot delete last admin"
  | Some u ->
      let* clients = Db.clients_of_user u.id in
      let* () = Lwt_list.iter_s delete_client clients in
      Stream.user_deleted u.id;
      let* () = Db.delete_user u.id in
      go (if u.id = s.user.id then "/login" else "/users")

let user_update ({ id; name; pass; admin; elevate_pass = _ } : App_shared.Actions.user_edit) =
  let name = String.trim name in
  let fail field message = Lwt.return (Error [ (field, message) ]) in
  let* u = Db.user_by_id id in
  let* admins = Db.count_admins () in
  match u with
  | None -> alert "/users" "user does not exist"
  | Some _ when name = "" -> fail (Some `Name) "The name is required"
  | Some u when u.admin && (not admin) && admins = 1 -> fail None "cannot delete last admin"
  | Some u -> (
      match if pass = "" then Ok u.pass else Result.map (fun () -> Auth.hash_password pass) (Auth.validate_password pass) with
      | Error m -> fail (Some `Pass) m
      | Ok hash ->
          Lwt.catch
            (fun () ->
              let* () = Db.update_user { u with name; admin; pass = hash } in
              go "/users")
            (fun _ -> fail (Some `Name) "username already exists"))

(* The session, elevated by [pass], for [f]; without a session, the login page. *)
let elevated req pass f =
  Yuka_server.to_async
    (let* s = Ui.find req in
     match s with
     | None -> go "/login"
     | Some s -> (
         let* s = Ui.elevate s pass in
         match s with Some s -> f s | None -> Lwt.return (Error [ (Some `Elevate_pass, "Wrong password") ])))

let admin_only (s : Ui.session) f = if s.user.admin then f s else alert "/" "Admins only"

let push req ({ app; title; message; priority } : App_shared.Actions.push) =
  Yuka_server.to_async
    (let* s = Ui.find req in
     match s with
     | None -> go "/login"
     | Some s -> (
         match own_app s app with
         | None -> alert "/" "No such application"
         | Some _ when String.trim message = "" -> Lwt.return (Error [ (Some `Message, "The message is required") ])
         | Some a ->
             let title = if String.trim title = "" then a.name else title in
             let* m = Db.create_message ~app_id:a.id ~message ~title ~priority ~extras:None in
             Stream.publish s.user.id m;
             go (Routes.app_messages ~app ())))

let elevate_client (s : Ui.session) id seconds =
  let* c = own_client s id in
  match c with
  | None -> alert "/clients" "No such client"
  | Some _ when seconds < 0 -> Lwt.return (Error [ (Some `Seconds, "Not a valid duration") ])
  | Some c ->
      let elevated_until = if seconds = 0 then None else Some (Unix.gettimeofday () +. float_of_int seconds) in
      let* () = Db.update_client { c with elevated_until } in
      go "/clients"

let register () =
  let module A = App_shared.Actions in
  Yuka.Server.Form.implement A.confirm ~redirect:Fun.id (fun req { target; elevate_pass } ->
      elevated req elevate_pass (fun s ->
          match target with
          | App_token id -> app_rotate s id
          | App_delete id -> app_delete s id
          | Client_delete id -> client_delete s id
          | User_delete id -> admin_only s (fun s -> user_delete s id)));
  Yuka.Server.Form.implement A.user_edit ~redirect:Fun.id (fun req (form : A.user_edit) ->
      elevated req form.elevate_pass (fun s -> admin_only s (fun _ -> user_update form)));
  Yuka.Server.Form.implement A.push ~redirect:Fun.id push;
  Yuka.Server.Form.implement A.elevate ~redirect:Fun.id (fun req { id; seconds; elevate_pass } ->
      elevated req elevate_pass (fun s -> elevate_client s id seconds))
