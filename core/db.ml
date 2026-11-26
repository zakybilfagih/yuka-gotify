(* SQLite through Caqti on Lwt. Times are Unix seconds (REAL). *)

open Model
open Caqti_request.Infix

module T = struct
  include Caqti_type.Std

  let user =
    product (fun id name pass admin created_at : user -> { id; name; pass; admin; created_at })
    @@ proj int (fun (u : user) -> u.id)
    @@ proj string (fun (u : user) -> u.name)
    @@ proj string (fun (u : user) -> u.pass)
    @@ proj bool (fun (u : user) -> u.admin)
    @@ proj float (fun (u : user) -> u.created_at)
    @@ proj_end

  let application =
    product
      (fun id user_id token name description default_priority internal sort_key created_at last_used image : application ->
        { id; user_id; token; name; description; default_priority; internal; sort_key; created_at; last_used; image })
    @@ proj int (fun (a : application) -> a.id)
    @@ proj int (fun (a : application) -> a.user_id)
    @@ proj string (fun (a : application) -> a.token)
    @@ proj string (fun (a : application) -> a.name)
    @@ proj string (fun (a : application) -> a.description)
    @@ proj int (fun (a : application) -> a.default_priority)
    @@ proj bool (fun (a : application) -> a.internal)
    @@ proj string (fun (a : application) -> a.sort_key)
    @@ proj float (fun (a : application) -> a.created_at)
    @@ proj (option float) (fun (a : application) -> a.last_used)
    @@ proj string (fun (a : application) -> a.image)
    @@ proj_end

  let client =
    product
      (fun id user_id token name created_at last_used elevated_until expires_after : client ->
        { id; user_id; token; name; created_at; last_used; elevated_until; expires_after })
    @@ proj int (fun (c : client) -> c.id)
    @@ proj int (fun (c : client) -> c.user_id)
    @@ proj string (fun (c : client) -> c.token)
    @@ proj string (fun (c : client) -> c.name)
    @@ proj float (fun (c : client) -> c.created_at)
    @@ proj (option float) (fun (c : client) -> c.last_used)
    @@ proj (option float) (fun (c : client) -> c.elevated_until)
    @@ proj int (fun (c : client) -> c.expires_after)
    @@ proj_end

  let extras_of = function None -> None | Some s -> ( try Some (Yojson.Safe.from_string s) with _ -> None)
  let extras_to = Option.map Yojson.Safe.to_string

  let message =
    product (fun id app_id message title priority extras date : message ->
        { id; app_id; message; title; priority; extras = extras_of extras; date })
    @@ proj int (fun (m : message) -> m.id)
    @@ proj int (fun (m : message) -> m.app_id)
    @@ proj string (fun (m : message) -> m.message)
    @@ proj string (fun (m : message) -> m.title)
    @@ proj int (fun (m : message) -> m.priority)
    @@ proj (option string) (fun (m : message) -> extras_to m.extras)
    @@ proj float (fun (m : message) -> m.date)
    @@ proj_end
end

let schema =
  [
    {|CREATE TABLE IF NOT EXISTS users (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL UNIQUE,
        pass TEXT NOT NULL,
        admin INTEGER NOT NULL,
        created_at REAL NOT NULL)|};
    {|CREATE TABLE IF NOT EXISTS applications (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
        token TEXT NOT NULL UNIQUE,
        name TEXT NOT NULL,
        description TEXT NOT NULL,
        default_priority INTEGER NOT NULL,
        internal INTEGER NOT NULL,
        sort_key TEXT NOT NULL,
        created_at REAL NOT NULL,
        last_used REAL,
        image TEXT NOT NULL DEFAULT '',
        UNIQUE (user_id, sort_key))|};
    {|CREATE TABLE IF NOT EXISTS clients (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
        token TEXT NOT NULL UNIQUE,
        name TEXT NOT NULL,
        created_at REAL NOT NULL,
        last_used REAL,
        elevated_until REAL,
        expires_after INTEGER NOT NULL)|};
    {|CREATE TABLE IF NOT EXISTS messages (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        app_id INTEGER NOT NULL REFERENCES applications(id) ON DELETE CASCADE,
        message TEXT NOT NULL,
        title TEXT NOT NULL,
        priority INTEGER NOT NULL,
        extras TEXT,
        date REAL NOT NULL)|};
    "CREATE INDEX IF NOT EXISTS messages_app ON messages (app_id, id)";
  ]

type pool = (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t

let pool : pool option ref = ref None

let use f =
  match !pool with
  | None -> Lwt.fail_with "Db.use: not connected"
  | Some p ->
      Lwt.bind (Caqti_lwt_unix.Pool.use f p) (function
        | Ok v -> Lwt.return v
        | Error e -> Lwt.fail_with (Caqti_error.show e))

let foreign_keys = (T.unit ->. T.unit) "PRAGMA foreign_keys = ON"

let q_has_image =
  (T.unit ->! T.int) "SELECT COUNT(*) FROM pragma_table_info('applications') WHERE name = 'image'"

let q_add_image = (T.unit ->. T.unit) "ALTER TABLE applications ADD COLUMN image TEXT NOT NULL DEFAULT ''"

(* Databases made before application images have no image column. *)
let migrate () =
  use (fun (module C : Caqti_lwt.CONNECTION) ->
      Lwt_result.bind (C.find q_has_image ()) (fun n -> if n = 0 then C.exec q_add_image () else Lwt_result.return ()))

let connect path =
  let uri = Uri.make ~scheme:"sqlite3" ~path () in
  let post_connect (module C : Caqti_lwt.CONNECTION) = C.exec foreign_keys () in
  match Caqti_lwt_unix.connect_pool ~post_connect uri with
  | Error e -> Lwt.fail_with (Caqti_error.show e)
  | Ok p ->
      pool := Some p;
      Lwt.bind
        (Lwt_list.iter_s
           (fun sql -> use (fun (module C : Caqti_lwt.CONNECTION) -> C.exec ((T.unit ->. T.unit) sql) ()))
           schema)
        migrate

let now () = Unix.gettimeofday ()

(* Users *)

let user_cols = "id, name, pass, admin, created_at"
let q_user_by_name = (T.string ->? T.user) ("SELECT " ^ user_cols ^ " FROM users WHERE name = ?")
let q_user_by_id = (T.int ->? T.user) ("SELECT " ^ user_cols ^ " FROM users WHERE id = ?")
let q_users = (T.unit ->* T.user) ("SELECT " ^ user_cols ^ " FROM users ORDER BY id")
let q_count_admins = (T.unit ->! T.int) "SELECT COUNT(*) FROM users WHERE admin = 1"

let q_insert_user =
  (T.(t4 string string bool float) ->! T.int)
    "INSERT INTO users (name, pass, admin, created_at) VALUES (?, ?, ?, ?) RETURNING id"

let q_update_user = (T.(t4 string string bool int) ->. T.unit) "UPDATE users SET name = ?, pass = ?, admin = ? WHERE id = ?"
let q_delete_user = (T.int ->. T.unit) "DELETE FROM users WHERE id = ?"

let user_by_name name = use (fun (module C : Caqti_lwt.CONNECTION) -> C.find_opt q_user_by_name name)
let user_by_id id = use (fun (module C : Caqti_lwt.CONNECTION) -> C.find_opt q_user_by_id id)
let users () = use (fun (module C : Caqti_lwt.CONNECTION) -> C.collect_list q_users ())
let count_admins () = use (fun (module C : Caqti_lwt.CONNECTION) -> C.find q_count_admins ())

let create_user ~name ~pass ~admin =
  let created_at = now () in
  Lwt.map
    (fun id -> { id; name; pass; admin; created_at })
    (use (fun (module C : Caqti_lwt.CONNECTION) -> C.find q_insert_user (name, pass, admin, created_at)))

let update_user (u : user) =
  use (fun (module C : Caqti_lwt.CONNECTION) -> C.exec q_update_user (u.name, u.pass, u.admin, u.id))


(* Applications *)

let app_cols =
  "id, user_id, token, name, description, default_priority, internal, sort_key, created_at, last_used, image"

let q_app_by_token = (T.string ->? T.application) ("SELECT " ^ app_cols ^ " FROM applications WHERE token = ?")
let q_app_by_id = (T.int ->? T.application) ("SELECT " ^ app_cols ^ " FROM applications WHERE id = ?")

let q_apps_of_user =
  (T.int ->* T.application) ("SELECT " ^ app_cols ^ " FROM applications WHERE user_id = ? ORDER BY sort_key, id")

let q_last_sort_key =
  (T.int ->? T.string) "SELECT sort_key FROM applications WHERE user_id = ? ORDER BY sort_key DESC LIMIT 1"

let q_insert_app =
  (T.(t2 (t4 int string string string) (t4 int string float unit)) ->! T.int)
    {|INSERT INTO applications (user_id, token, name, description, default_priority, internal, sort_key, created_at)
      VALUES (?, ?, ?, ?, ?, 0, ?, ?) RETURNING id|}

let q_update_app =
  (T.(t2 (t4 string string string int) (t2 string int)) ->. T.unit)
    "UPDATE applications SET token = ?, name = ?, description = ?, default_priority = ?, sort_key = ? WHERE id = ?"

let q_set_app_image = (T.(t2 string int) ->. T.unit) "UPDATE applications SET image = ? WHERE id = ?"
let q_app_used = (T.(t2 float int) ->. T.unit) "UPDATE applications SET last_used = ? WHERE id = ?"
let q_delete_app = (T.int ->. T.unit) "DELETE FROM applications WHERE id = ?"

let set_app_image id image = use (fun (module C : Caqti_lwt.CONNECTION) -> C.exec q_set_app_image (image, id))
let app_by_token t = use (fun (module C : Caqti_lwt.CONNECTION) -> C.find_opt q_app_by_token t)
let app_by_id id = use (fun (module C : Caqti_lwt.CONNECTION) -> C.find_opt q_app_by_id id)
let apps_of_user uid = use (fun (module C : Caqti_lwt.CONNECTION) -> C.collect_list q_apps_of_user uid)

(* A sort key after every key of the user. Gotify uses fractional indexing;
   any string that sorts after the last one keeps the order. *)
let next_sort_key last = match last with None -> "a0" | Some k -> k ^ "V"

(* The sort key of the [i]th application in a new order: a valid key of
   fractional indexing ("a0" to "az", then "b00"), so Gotify's clients can
   still put a key between two of them. *)
let sort_key_at i =
  let digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" in
  let d n = String.make 1 digits.[n] in
  if i < 62 then "a" ^ d i else
    let j = i - 62 in
    if j < 62 * 62 then "b" ^ d (j / 62) ^ d (j mod 62) else "c" ^ d (j / 3844 mod 62) ^ d (j / 62 mod 62) ^ d (j mod 62)

let q_set_sort_key = (T.(t3 string int int) ->. T.unit) "UPDATE applications SET sort_key = ? WHERE id = ? AND user_id = ?"

(** Gives the applications [ids] of the user, all of them, this order, in one
    transaction; the keys go through temporary ones first, as they are unique. *)
let reorder_apps uid ids =
  use (fun (module C : Caqti_lwt.CONNECTION) ->
      let ( let* ) = Lwt_result.bind in
      let set key id = C.exec q_set_sort_key (key, id, uid) in
      let rec each f i = function [] -> Lwt_result.return () | id :: rest -> let* () = f i id in each f (i + 1) rest in
      let* () = C.start () in
      let run =
        let* () = each (fun _ id -> set ("~" ^ string_of_int id) id) 0 ids in
        each (fun i id -> set (sort_key_at i) id) 0 ids
      in
      Lwt.bind run (function
        | Ok () -> C.commit ()
        | Error _ as e -> Lwt.bind (C.rollback ()) (fun _ -> Lwt.return e)))

let create_app ~user_id ~name ~description ~default_priority ?sort_key token =
  let created_at = now () in
  use (fun (module C : Caqti_lwt.CONNECTION) ->
      let ( let* ) = Lwt_result.bind in
      let* sort_key =
        match sort_key with
        | Some k when k <> "" -> Lwt_result.return k
        | _ -> Lwt_result.map next_sort_key (C.find_opt q_last_sort_key user_id)
      in
      let* id =
        C.find q_insert_app ((user_id, token, name, description), (default_priority, sort_key, created_at, ()))
      in
      Lwt_result.return
        {
          id;
          user_id;
          token;
          name;
          description;
          default_priority;
          internal = false;
          sort_key;
          created_at;
          last_used = None;
          image = "";
        })

let update_app (a : application) =
  use (fun (module C : Caqti_lwt.CONNECTION) ->
      C.exec q_update_app ((a.token, a.name, a.description, a.default_priority), (a.sort_key, a.id)))

let app_used id = use (fun (module C : Caqti_lwt.CONNECTION) -> C.exec q_app_used (now (), id))
(** Deletes the application, its messages and its image file. *)
let delete_app (a : application) =
  Lwt.map (fun () -> Images.remove a.image) (use (fun (module C : Caqti_lwt.CONNECTION) -> C.exec q_delete_app a.id))

(** Deletes the user, with their applications, clients and messages, and the image files of the applications. *)
let delete_user id =
  Lwt.bind (apps_of_user id) (fun apps ->
      Lwt.map
        (fun () -> List.iter (fun (a : application) -> Images.remove a.image) apps)
        (use (fun (module C : Caqti_lwt.CONNECTION) -> C.exec q_delete_user id)))

(* Clients *)

let client_cols = "id, user_id, token, name, created_at, last_used, elevated_until, expires_after"
let q_client_by_token = (T.string ->? T.client) ("SELECT " ^ client_cols ^ " FROM clients WHERE token = ?")
let q_client_by_id = (T.int ->? T.client) ("SELECT " ^ client_cols ^ " FROM clients WHERE id = ?")
let q_clients_of_user = (T.int ->* T.client) ("SELECT " ^ client_cols ^ " FROM clients WHERE user_id = ? ORDER BY id")

let q_insert_client =
  (T.(t2 (t4 int string string float) (t2 (option float) int)) ->! T.int)
    {|INSERT INTO clients (user_id, token, name, created_at, elevated_until, expires_after)
      VALUES (?, ?, ?, ?, ?, ?) RETURNING id|}

let q_update_client =
  (T.(t2 (t4 string string (option float) int) int) ->. T.unit)
    "UPDATE clients SET token = ?, name = ?, elevated_until = ?, expires_after = ? WHERE id = ?"

let q_client_used = (T.(t2 float int) ->. T.unit) "UPDATE clients SET last_used = ? WHERE id = ?"
let q_delete_client = (T.int ->. T.unit) "DELETE FROM clients WHERE id = ?"

let q_expired_clients =
  (T.float ->* T.client)
    ("SELECT " ^ client_cols
   ^ " FROM clients WHERE expires_after > 0 AND COALESCE(last_used, created_at) + expires_after < ?")

let client_by_token t = use (fun (module C : Caqti_lwt.CONNECTION) -> C.find_opt q_client_by_token t)
let client_by_id id = use (fun (module C : Caqti_lwt.CONNECTION) -> C.find_opt q_client_by_id id)
let clients_of_user uid = use (fun (module C : Caqti_lwt.CONNECTION) -> C.collect_list q_clients_of_user uid)

let create_client ~user_id ~name ?elevated_until ?(expires_after = 0) token =
  let created_at = now () in
  Lwt.map
    (fun id -> { id; user_id; token; name; created_at; last_used = None; elevated_until; expires_after })
    (use (fun (module C : Caqti_lwt.CONNECTION) ->
         C.find q_insert_client ((user_id, token, name, created_at), (elevated_until, expires_after))))

let update_client (c : client) =
  use (fun (module C : Caqti_lwt.CONNECTION) ->
      C.exec q_update_client ((c.token, c.name, c.elevated_until, c.expires_after), c.id))

let client_used id = use (fun (module C : Caqti_lwt.CONNECTION) -> C.exec q_client_used (now (), id))
let delete_client id = use (fun (module C : Caqti_lwt.CONNECTION) -> C.exec q_delete_client id)
let expired_clients () = use (fun (module C : Caqti_lwt.CONNECTION) -> C.collect_list q_expired_clients (now ()))

(* Messages. Newest first; [since] is an exclusive upper bound on the id
   (0: no bound), as in Gotify. *)

let msg_cols = "m.id, m.app_id, m.message, m.title, m.priority, m.extras, m.date"

let q_messages_of_user =
  (T.(t4 int int int int) ->* T.message)
    ("SELECT " ^ msg_cols
   ^ " FROM messages m JOIN applications a ON a.id = m.app_id WHERE a.user_id = ? AND (? = 0 OR m.id < ?) ORDER BY m.id DESC LIMIT ?"
    )

let q_messages_of_app =
  (T.(t4 int int int int) ->* T.message)
    ("SELECT " ^ msg_cols ^ " FROM messages m WHERE m.app_id = ? AND (? = 0 OR m.id < ?) ORDER BY m.id DESC LIMIT ?")

let q_message_by_id = (T.int ->? T.message) ("SELECT " ^ msg_cols ^ " FROM messages m WHERE m.id = ?")

let q_insert_message =
  (T.(t2 (t4 int string string int) (t2 (option string) float)) ->! T.int)
    "INSERT INTO messages (app_id, message, title, priority, extras, date) VALUES (?, ?, ?, ?, ?, ?) RETURNING id"

let q_delete_message = (T.int ->. T.unit) "DELETE FROM messages WHERE id = ?"
let q_delete_app_messages = (T.int ->. T.unit) "DELETE FROM messages WHERE app_id = ?"

let q_delete_user_messages =
  (T.int ->. T.unit) "DELETE FROM messages WHERE app_id IN (SELECT id FROM applications WHERE user_id = ?)"

let messages_of_user ~limit ~since uid =
  use (fun (module C : Caqti_lwt.CONNECTION) -> C.collect_list q_messages_of_user (uid, since, since, limit))

let messages_of_app ~limit ~since app_id =
  use (fun (module C : Caqti_lwt.CONNECTION) -> C.collect_list q_messages_of_app (app_id, since, since, limit))

let message_by_id id = use (fun (module C : Caqti_lwt.CONNECTION) -> C.find_opt q_message_by_id id)

let create_message ~app_id ~message ~title ~priority ~extras =
  let date = now () in
  Lwt.map
    (fun id -> { id; app_id; message; title; priority; extras; date })
    (use (fun (module C : Caqti_lwt.CONNECTION) ->
         C.find q_insert_message ((app_id, message, title, priority), (T.extras_to extras, date))))

let delete_message id = use (fun (module C : Caqti_lwt.CONNECTION) -> C.exec q_delete_message id)
let delete_app_messages id = use (fun (module C : Caqti_lwt.CONNECTION) -> C.exec q_delete_app_messages id)
let delete_user_messages uid = use (fun (module C : Caqti_lwt.CONNECTION) -> C.exec q_delete_user_messages uid)

let ping () = use (fun (module C : Caqti_lwt.CONNECTION) -> C.find ((T.unit ->! T.int) "SELECT 1") ())
