(* The live message feeds of the web UI. Every change of a user's messages
   (REST API or UI) goes through Stream.changed, which reloads the feeds of
   that user here. *)

open Gotify_core
module S = App_shared

let ( let* ) = Lwt.bind

let view (apps : Model.application list) (m : Model.message) : S.Model.message =
  let markdown = Markdown.is_markdown m.extras in
  let app = List.find_opt (fun (a : Model.application) -> a.id = m.app_id) apps in
  {
    id = m.id;
    app_id = m.app_id;
    app_name = (match app with Some a -> a.name | None -> "");
    image = (match app with Some a -> Static.app_image a | None -> Static.url "defaultapp.png");
    title = m.title;
    body = Markdown.body ~markdown m.message;
    markdown;
    priority = m.priority;
    time = m.date;
    date = Ui.date m.date;
  }

(* The feeds loaded for each user, so a change reloads each of them; the key of
   a feed has its number of pages, which is up to the client. *)
let loaded : (int, S.Model.feed list) Hashtbl.t = Hashtbl.create 16

let load (f : S.Model.feed) =
  let feeds = Option.value (Hashtbl.find_opt loaded f.user) ~default:[] in
  if not (List.mem f feeds) then Hashtbl.replace loaded f.user (f :: feeds);
  let pages = Int.max 1 (Int.min S.Topics.max_pages f.pages) in
  (* One more than the pages show, to know whether there is a next page. *)
  let limit = (pages * S.Topics.per_page) + 1 in
  let* apps = Db.apps_of_user f.user in
  let* ms =
    match f.app with
    | None -> Db.messages_of_user ~limit ~since:0 f.user
    | Some app when List.exists (fun (a : Model.application) -> a.id = app) apps -> Db.messages_of_app ~limit ~since:0 app
    | Some _ -> Lwt.return []
  in
  let more = List.length ms = limit && pages < S.Topics.max_pages in
  Lwt.return { S.Model.messages = List.map (view apps) (List.filteri (fun i _ -> i < limit - 1) ms); more }

let changed uid =
  Lwt.async (fun () ->
      let* apps = Lwt.catch (fun () -> Db.apps_of_user uid) (fun _ -> Lwt.return []) in
      let current = function { S.Model.app = None; _ } -> true | { app = Some a; _ } -> List.exists (fun (b : Model.application) -> b.id = a) apps in
      let feeds = List.filter current (Option.value (Hashtbl.find_opt loaded uid) ~default:[]) in
      Hashtbl.replace loaded uid feeds;
      List.iter (Yuka.Server.Live.changed S.Topics.messages) feeds;
      Lwt.return_unit)

let session req =
  Yuka.Async.bind (Yuka_server.to_async (Ui.find req)) (function
    | Some s -> Yuka.Async.return s
    | None -> Yuka.Server.Action.reject "You are not signed in")

let register () =
  Stream.on_change changed;
  Yuka.Server.Live.implement S.Topics.messages
    ~allow:(fun req (f : S.Model.feed) -> Ui.user_of_request req = Some f.user)
    (fun f -> Yuka_server.to_async (load f));
  Yuka.Server.Action.implement S.Actions.ping (fun _ () -> Yuka.Async.return ());
  Yuka.Server.Action.implement S.Actions.delete_message (fun req id ->
      Yuka.Async.bind (session req) (fun (s : Ui.session) ->
          Yuka_server.to_async
            (let* m = Db.message_by_id id in
             let* app = match m with Some m -> Db.app_by_id m.app_id | None -> Lwt.return None in
             match app with
             | Some a when a.user_id = s.user.id ->
                 let* () = Db.delete_message id in
                 Stream.changed s.user.id;
                 Lwt.return_unit
             | _ -> Lwt.return_unit)))
