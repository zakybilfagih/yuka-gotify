let delete_message = Yuka.Action.mutation ~name:"delete_message" ~input:Yuka.Codec.int ~output:Yuka.Codec.unit

(** Whether the server answers, before the message list loads the page again. *)
let ping = Yuka.Action.query ~name:"ping" ~input:Yuka.Codec.unit ~output:Yuka.Codec.unit

(** What a confirm dialog does; the hidden [target] field of its form. *)
module Target = struct
  (** [Messages_delete]: every message of the user ([None]) or of one app. *)
  type t =
    | App_delete of int
    | App_token of int
    | App_image_delete of int
    | Client_delete of int
    | User_delete of int
    | Messages_delete of int option

  let to_string = function
    | App_delete id -> "app-delete:" ^ Int.to_string id
    | App_token id -> "app-token:" ^ Int.to_string id
    | App_image_delete id -> "app-image-delete:" ^ Int.to_string id
    | Client_delete id -> "client-delete:" ^ Int.to_string id
    | User_delete id -> "user-delete:" ^ Int.to_string id
    | Messages_delete None -> "messages-delete:all"
    | Messages_delete (Some app) -> "messages-delete:" ^ Int.to_string app

  let of_string s =
    match String.split_on_char ':' s with
    | [ kind; id ] -> (
        match (kind, int_of_string_opt id) with
        | "app-delete", Some id -> Some (App_delete id)
        | "app-token", Some id -> Some (App_token id)
        | "app-image-delete", Some id -> Some (App_image_delete id)
        | "client-delete", Some id -> Some (Client_delete id)
        | "user-delete", Some id -> Some (User_delete id)
        | "messages-delete", Some app -> Some (Messages_delete (Some app))
        | "messages-delete", None when id = "all" -> Some (Messages_delete None)
        | _ -> None)
    | _ -> None
end

(* The forms of the dialogs that Gotify allows only to an elevated session:
   [elevate_pass] is the password of a session that is not. Each answers the
   URL to go to. *)

type confirm = { target : Target.t; elevate_pass : string option } [@@deriving form]

let confirm = Yuka.Form.action ~name:"confirm" ~fields:confirm_form ~output:Yuka.Codec.string

type user_edit = { id : int; name : string; pass : string; admin : bool; elevate_pass : string option }
[@@deriving form]

let user_edit = Yuka.Form.action ~name:"user_edit" ~fields:user_edit_form ~output:Yuka.Codec.string

(** A message pushed from the web UI to one of the user's applications, as
    POST /message. An empty title is the application's name. *)
type push = { app : int; title : string; message : string; priority : int } [@@deriving form]

let push = Yuka.Form.action ~name:"push" ~fields:push_form ~output:Yuka.Codec.string

(** Elevates a client of the user for [seconds], as POST /client/:id/elevate;
    0 ends its elevation. *)
type elevate = { id : int; seconds : int; elevate_pass : string option } [@@deriving form]

let elevate = Yuka.Form.action ~name:"elevate" ~fields:elevate_form ~output:Yuka.Codec.string

(** The choices of the Elevate dialog, as Gotify's web UI. *)
let durations = [ (3600, "1 hour"); (86400, "1 day"); (30 * 86400, "30 days"); (365 * 86400, "1 year"); (0, "End the elevation") ]

(** The ids of all the user's applications, in their new order. *)
let reorder_apps = Yuka.Action.mutation ~name:"reorder_apps" ~input:(Yuka.Codec.list Yuka.Codec.int) ~output:Yuka.Codec.unit

(* The create and edit forms. [id]: [None] to create, [Some] to edit. Each
   answers the URL to go to. *)

type app_save = { id : int option; name : string; description : string; priority : int } [@@deriving form]

let app_save = Yuka.Form.action ~name:"app_save" ~fields:app_save_form ~output:Yuka.Codec.string

type client_save = { id : int option; name : string; expires : int } [@@deriving form]

let client_save = Yuka.Form.action ~name:"client_save" ~fields:client_save_form ~output:Yuka.Codec.string

type user_create = { name : string; pass : string; admin : bool; elevate_pass : string option } [@@deriving form]

let user_create = Yuka.Form.action ~name:"user_create" ~fields:user_create_form ~output:Yuka.Codec.string

type password = { pass : string; elevate_pass : string option } [@@deriving form]

let password = Yuka.Form.action ~name:"password" ~fields:password_form ~output:Yuka.Codec.string
