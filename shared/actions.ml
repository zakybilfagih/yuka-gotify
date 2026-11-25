let delete_message = Yuka.Action.mutation ~name:"delete_message" ~input:Yuka.Codec.int ~output:Yuka.Codec.unit

(** [None]: every message of the user; [Some app]: those of one app. *)
let delete_all = Yuka.Action.mutation ~name:"delete_all" ~input:(Yuka.Codec.option Yuka.Codec.int) ~output:Yuka.Codec.unit

(** Whether the server answers, before the message list loads the page again. *)
let ping = Yuka.Action.query ~name:"ping" ~input:Yuka.Codec.unit ~output:Yuka.Codec.unit

(** What a confirm dialog does; the hidden [target] field of its form. *)
module Target = struct
  type t = App_delete of int | App_token of int | Client_delete of int | User_delete of int

  let to_string = function
    | App_delete id -> "app-delete:" ^ Int.to_string id
    | App_token id -> "app-token:" ^ Int.to_string id
    | Client_delete id -> "client-delete:" ^ Int.to_string id
    | User_delete id -> "user-delete:" ^ Int.to_string id

  let of_string s =
    match String.split_on_char ':' s with
    | [ kind; id ] -> (
        match (kind, int_of_string_opt id) with
        | "app-delete", Some id -> Some (App_delete id)
        | "app-token", Some id -> Some (App_token id)
        | "client-delete", Some id -> Some (Client_delete id)
        | "user-delete", Some id -> Some (User_delete id)
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
