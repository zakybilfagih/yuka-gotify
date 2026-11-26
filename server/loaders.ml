(* The session of every signed-in page, else the login page. *)
let session req =
  Yuka.Async.map
    (function
      | Some s -> Ok s
      | None -> Error (Yuka.Server.Redirect { location = "/login"; kind = `See_other }))
    (Yuka_server.to_async (Ui.find req))

let clients req =
  Yuka.Async.bind (session req) (function
    | Error _ as e -> Yuka.Async.return e
    | Ok (s : Ui.session) -> Yuka.Async.map Result.ok (Yuka_server.to_async (Gotify_core.Db.clients_of_user s.user.id)))

let users (_ : Yuka.Server.request) =
  Yuka.Async.map Result.ok (Yuka_server.to_async (Gotify_core.Db.users ()))

(* The client of a dialog route, when it is one of the session's user. *)
let client req ~id =
  Yuka.Async.bind (session req) (function
    | Error _ as e -> Yuka.Async.return e
    | Ok (s : Ui.session) ->
        Yuka.Async.map
          (function Some (c : Gotify_core.Model.client) when c.user_id = s.user.id -> Ok (Some c) | _ -> Ok None)
          (Yuka_server.to_async (Gotify_core.Db.client_by_id id)))

(* The message of a dialog route, when it is one of the session's user. *)
let message req ~id =
  Yuka.Async.bind (session req) (function
    | Error _ as e -> Yuka.Async.return e
    | Ok (s : Ui.session) ->
        Yuka.Async.map
          (function
            | Some (m : Gotify_core.Model.message)
              when List.exists (fun (a : Gotify_core.Model.application) -> a.id = m.app_id) s.apps ->
                Ok (Some m)
            | _ -> Ok None)
          (Yuka_server.to_async (Gotify_core.Db.message_by_id id)))

(* The user of a dialog route; the page shows it to admins only. *)
let user (_ : Yuka.Server.request) ~id = Yuka.Async.map Result.ok (Yuka_server.to_async (Gotify_core.Db.user_by_id id))
