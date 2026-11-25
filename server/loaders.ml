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
