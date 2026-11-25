(* The rows of the database and their JSON in the shape of the Gotify 3.1.1 API. *)

type user = { id : int; name : string; pass : string; admin : bool; created_at : float }

type application = {
  id : int;
  user_id : int;
  token : string;
  name : string;
  description : string;
  default_priority : int;
  internal : bool;
  sort_key : string;
  created_at : float;
  last_used : float option;
}

type client = {
  id : int;
  user_id : int;
  token : string;
  name : string;
  created_at : float;
  last_used : float option;
  elevated_until : float option;
  expires_after : int;  (** seconds of inactivity; 0 = never *)
}

type message = {
  id : int;
  app_id : int;
  message : string;
  title : string;
  priority : int;
  extras : Yojson.Safe.t option;
  date : float;
}

let time t =
  match Ptime.of_float_s t with
  | Some p -> `String (Ptime.to_rfc3339 ~frac_s:3 ~tz_offset_s:0 p)
  | None -> `Null

let time_opt = function Some t -> time t | None -> `Null

let user_json (u : user) : Yojson.Safe.t =
  `Assoc [ ("id", `Int u.id); ("name", `String u.name); ("admin", `Bool u.admin); ("createdAt", time u.created_at) ]

let application_json ?token (a : application) : Yojson.Safe.t =
  `Assoc
    ([
       ("id", `Int a.id);
       ("name", `String a.name);
       ("description", `String a.description);
       ("internal", `Bool a.internal);
       ("image", `String "static/defaultapp.png");
       ("defaultPriority", `Int a.default_priority);
       ("sortKey", `String a.sort_key);
       ("createdAt", time a.created_at);
       ("lastUsed", time_opt a.last_used);
     ]
    @ match token with Some t -> [ ("token", `String t) ] | None -> [])

let expires_at (c : client) =
  if c.expires_after = 0 then None
  else Some (Option.value c.last_used ~default:c.created_at +. float_of_int c.expires_after)

let elevated ~now (c : client) = match c.elevated_until with Some t -> now < t | None -> false

let client_json ?token ~now (c : client) : Yojson.Safe.t =
  `Assoc
    ([
       ("id", `Int c.id);
       ("name", `String c.name);
       ("createdAt", time c.created_at);
       ("lastUsed", time_opt c.last_used);
       ("expiresAfterInactivitySeconds", `Int c.expires_after);
     ]
    @ (match token with Some t -> [ ("token", `String t) ] | None -> [])
    @ (if elevated ~now c then [ ("elevatedUntil", time_opt c.elevated_until) ] else [])
    @ match expires_at c with Some t -> [ ("expiresAt", time t) ] | None -> [])

let message_json (m : message) : Yojson.Safe.t =
  `Assoc
    ([
       ("id", `Int m.id);
       ("appid", `Int m.app_id);
       ("message", `String m.message);
       ("title", `String m.title);
       ("priority", `Int m.priority);
       ("date", time m.date);
     ]
    @ match m.extras with Some (`Assoc (_ :: _) as e) -> [ ("extras", e) ] | _ -> [])
