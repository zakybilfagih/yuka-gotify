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
             (* As Gotify, the client expires after 7 days without use, and
                each use moves that on (Ui.find). The cookie keeps as long as
                a browser keeps one (400 days), so the client's expiry decides. *)
             let* c =
               Db.create_client ~user_id:u.id ~name:"Gotify web (Yuka)"
                 ~elevated_until:(Unix.gettimeofday () +. elevation) ~expires_after:(7 * 24 * 3600) public
             in
             let cookie =
               Yuka_server.Signed_cookie.set (Ui.the_sessions ()) ~secure:!Ui.secure ~same_site:`Lax
                 ~max_age:(400 * 24 * 3600) Ui.cookie_name (string_of_int c.id)
             in
             Lwt.return (match cookie with Ok h -> redirect ~headers:[ h ] "/" | Error m -> Yuka.Server.text ~status:500 m)))

let delete_client (c : M.client) = Stream.client_deleted c.user_id c.token; Db.delete_client c.id

let logout req =
  Ui.posted req (fun s _ ->
      Ui.lwt
        (let* () = delete_client s.client in
         let headers = match Yuka.Server.Cookie.drop Ui.cookie_name with Ok h -> [ h ] | Error _ -> [] in
         Lwt.return (redirect ~headers "/login")))

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

(* The path and query of the Referer, the page a form was posted from; the
   origin check of the POST already refused other sites. *)
let referer req =
  match Yuka.Server.Request.header req "referer" with
  | None -> None
  | Some url -> (
      match String.index_opt url ':' with
      | Some i when i + 2 < String.length url && String.sub url i 3 = "://" -> (
          let rest = String.sub url (i + 3) (String.length url - i - 3) in
          match String.index_opt rest '/' with
          | Some j -> Some (String.sub rest j (String.length rest - j))
          | None -> Some "/")
      | _ -> None)

let theme req =
  Yuka.Async.bind (Ui.form req) (fun fields ->
      let t = match Ui.field fields "theme" with ("light" | "dark") as t -> t | _ -> "system" in
      let headers =
        match Yuka.Server.Cookie.set ~max_age:(365 * 24 * 3600) ~http_only:false Ui.theme_cookie t with
        | Ok h -> [ h ]
        | Error _ -> []
      in
      redirect ~headers (Ui.safe_next (match List.assoc_opt "next" fields with Some _ as n -> n | None -> referer req)))

(* Applications *)

let own_app (s : Ui.session) id = List.find_opt (fun (a : M.application) -> a.id = id) s.apps

(* The new order of the user's applications, when [ids] are all of them. *)
let reorder (s : Ui.session) ids =
  let own = List.sort compare (List.map (fun (a : M.application) -> a.id) s.apps) in
  if List.sort compare ids = own then Lwt.map (fun () -> true) (Db.reorder_apps s.user.id ids) else Lwt.return false

(* The image of an application, the upload form of its row. Without
   JavaScript it is a multipart post that answers 303; the island posts the
   same form with fetch and the header Yuka-Upload, and gets the URL to go to
   as text, which it navigates to. *)
let app_image req ~id =
  let answer path =
    if Yuka.Server.Request.header req "yuka-upload" = Some "1" then Yuka.Server.text path else redirect path
  in
  let alert message = answer (Ui.with_alert "/applications" message) in
  Yuka.Async.bind (Yuka_server.to_async (Ui.find req)) (function
    | None -> redirect "/login"
    | Some s -> (
        match own_app s id with
        | None -> alert "No such application"
        | Some a ->
            Yuka.Async.bind (Yuka.Server.Request.body req ~limit:Images.max_size) (function
              | Error `Too_large -> alert "The image is too large: at most 10 MB"
              | Ok body -> (
                  let file =
                    Option.bind (Option.bind (Yuka.Server.Request.header req "content-type") Http.boundary_of)
                      (fun boundary ->
                        List.find_map
                          (fun (field, filename, data) -> if field = "file" then Some (filename, data) else None)
                          (Http.files ~boundary body))
                  in
                  match file with
                  | None | Some ("", _) -> alert "Choose an image to upload"
                  | Some (filename, data) -> (
                      match Images.save ~filename data with
                      | Error "file must be an image" -> alert "That file is not an image"
                      | Error _ -> alert "Upload a .png, .jpg, .jpeg or .gif image"
                      | Ok name ->
                          Ui.lwt
                            (let* () = Db.set_app_image a.id name in
                             Images.remove a.image;
                             Stream.changed s.user.id;
                             Lwt.return (answer "/applications")))))))

(* Up or Down of an application, the form of the Applications page without
   JavaScript. *)
(* The Delete of a message without JavaScript, from its confirm dialog: back to
   the list it was opened from ([next]). *)
let message_delete req ~id =
  Ui.posted req (fun s fields ->
      Ui.lwt
        (let* m = Db.message_by_id id in
         let* () =
           match m with
           | Some m when List.exists (fun (a : M.application) -> a.id = m.app_id) s.apps ->
               let* () = Db.delete_message id in
               Lwt.return (Stream.changed s.user.id)
           | _ -> Lwt.return_unit
         in
         Lwt.return (redirect (Ui.safe_next (List.assoc_opt "next" fields)))))

let app_move req ~id =
  Ui.posted req (fun s fields ->
      let ids = List.map (fun (a : M.application) -> a.id) s.apps in
      let rec up = function a :: b :: rest when b = id -> b :: a :: rest | x :: rest -> x :: up rest | [] -> [] in
      let moved = match Ui.field fields "dir" with "up" -> up ids | _ -> List.rev (up (List.rev ids)) in
      Ui.lwt (Lwt.map (fun _ -> redirect "/applications") (reorder s moved)))

(* Clients *)

let own_client (s : Ui.session) id =
  Lwt.map (function Some (c : M.client) when c.user_id = s.user.id -> Some c | _ -> None) (Db.client_by_id id)

(* Users: admins only, and elevated to change anything, as Gotify 3.1.1. *)

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
      let* () = Db.delete_app a in
      Stream.changed s.user.id;
      go "/applications"

let app_image_delete (s : Ui.session) id =
  match own_app s id with
  | None -> alert "/applications" "No such application"
  | Some a ->
      let* () = Db.set_app_image a.id "" in
      Images.remove a.image;
      Stream.changed s.user.id;
      go "/applications"

let messages_delete (s : Ui.session) app =
  match app with
  | None ->
      let* () = Db.delete_user_messages s.user.id in
      Stream.changed s.user.id;
      go (Routes.messages ())
  | Some id -> (
      match own_app s id with
      | None -> alert "/" "No such application"
      | Some a ->
          let* () = Db.delete_app_messages a.id in
          Stream.changed s.user.id;
          go (Routes.app_messages ~app:a.id ()))

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

(* The create and edit forms. *)

let field_error field message = Lwt.return (Error [ (Some field, message) ])

(* The session for [f]; without one, the login page. *)
let signed req f =
  Yuka_server.to_async
    (let* s = Ui.find req in
     match s with None -> go "/login" | Some s -> f s)

let app_save (s : Ui.session) ({ id; name; description; priority } : App_shared.Actions.app_save) =
  let name = String.trim name in
  if name = "" then field_error `Name "The name is required"
  else if priority < 0 || priority > 10 then field_error `Priority "From 0 to 10"
  else
    match id with
    | None ->
        let public, secret = Token.application () in
        let* _ = Db.create_app ~user_id:s.user.id ~name ~description ~default_priority:priority public in
        let key = Ui.reveal ~user:s.user.id ~name secret in
        go (Yuka.Router.with_query "/applications" [ ("reveal", key) ])
    | Some id -> (
        match own_app s id with
        | None -> alert "/applications" "No such application"
        | Some a ->
            let* () = Db.update_app { a with name; description; default_priority = priority } in
            go "/applications")

let client_save (s : Ui.session) ({ id; name; expires } : App_shared.Actions.client_save) =
  let name = String.trim name in
  if name = "" then field_error `Name "The name is required"
  else if expires < 0 then field_error `Expires "Not a valid number of seconds"
  else
    match id with
    | None ->
        let public, secret = Token.client () in
        let* _ = Db.create_client ~user_id:s.user.id ~name ~expires_after:expires public in
        let key = Ui.reveal ~user:s.user.id ~name secret in
        go (Yuka.Router.with_query "/clients" [ ("reveal", key) ])
    | Some id -> (
        let* c = own_client s id in
        match c with
        | None -> alert "/clients" "No such client"
        | Some c ->
            let* () = Db.update_client { c with name; expires_after = expires } in
            go "/clients")

let user_create ({ name; pass; admin; elevate_pass = _ } : App_shared.Actions.user_create) =
  let name = String.trim name in
  match (name, Auth.validate_password pass) with
  | "", _ -> field_error `Name "The name is required"
  | _, Error m -> field_error `Pass m
  | _, Ok () -> (
      let* existing = Db.user_by_name name in
      match existing with
      | Some _ -> field_error `Name "username already exists"
      | None ->
          let* _ = Db.create_user ~name ~pass:(Auth.hash_password pass) ~admin in
          go "/users")

let password (s : Ui.session) ({ pass; elevate_pass = _ } : App_shared.Actions.password) =
  match Auth.validate_password pass with
  | Error m -> field_error `Pass m
  | Ok () ->
      let* () = Db.update_user { s.user with pass = Auth.hash_password pass } in
      go (Ui.with_alert "/settings" "Password changed")

let register () =
  let module A = App_shared.Actions in
  Yuka.Server.Action.implement A.apps (fun req _island ->
      match Ui.By_request.find_opt Ui.page_apps req with
      | Some apps -> Yuka.Async.return (List.map Ui.app_row apps)
      | None -> (
          Yuka.Async.bind (Yuka_server.to_async (Ui.find req)) (function
            | None -> Yuka.Server.Action.reject "You are not signed in"
            | Some s -> Yuka.Async.return (List.map Ui.app_row s.apps))));
  Yuka.Server.Action.implement A.reorder_apps (fun req ids ->
      Yuka.Async.bind (Yuka_server.to_async (Ui.find req)) (function
        | None -> Yuka.Server.Action.reject "You are not signed in"
        | Some s ->
            Yuka.Async.bind (Yuka_server.to_async (reorder s ids)) (fun ok ->
                if ok then Yuka.Async.return () else Yuka.Server.Action.reject "The applications changed: reload the page")));
  Yuka.Server.Form.implement A.confirm ~redirect:Fun.id (fun req { target; elevate_pass } ->
      (* Gotify lets any client change an image; the others need an elevated session. *)
      let session f =
        match target with App_image_delete _ | Messages_delete _ -> signed req f | _ -> elevated req elevate_pass f
      in
      session (fun s ->
          match target with
          | App_token id -> app_rotate s id
          | App_delete id -> app_delete s id
          | App_image_delete id -> app_image_delete s id
          | Messages_delete app -> messages_delete s app
          | Client_delete id -> client_delete s id
          | User_delete id -> admin_only s (fun s -> user_delete s id)));
  Yuka.Server.Form.implement A.user_edit ~redirect:Fun.id (fun req (form : A.user_edit) ->
      elevated req form.elevate_pass (fun s -> admin_only s (fun _ -> user_update form)));
  Yuka.Server.Form.implement A.push ~redirect:Fun.id push;
  Yuka.Server.Form.implement A.elevate ~redirect:Fun.id (fun req { id; seconds; elevate_pass } ->
      elevated req elevate_pass (fun s -> elevate_client s id seconds));
  Yuka.Server.Form.implement A.app_save ~redirect:Fun.id (fun req form -> signed req (fun s -> app_save s form));
  Yuka.Server.Form.implement A.client_save ~redirect:Fun.id (fun req form -> signed req (fun s -> client_save s form));
  Yuka.Server.Form.implement A.user_create ~redirect:Fun.id (fun req (form : A.user_create) ->
      elevated req form.elevate_pass (fun s -> admin_only s (fun _ -> user_create form)));
  Yuka.Server.Form.implement A.password ~redirect:Fun.id (fun req (form : A.password) ->
      elevated req form.elevate_pass (fun s -> password s form))
