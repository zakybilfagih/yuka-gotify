(* What the web UI shows of a message, sent to the island. *)
type message = {
  id : int;
  app_id : int;
  app_name : string;
  title : string;
  body : Markup.node list;
  markdown : bool;
  priority : int;
  date : string;  (** formatted on the server, so both renders agree *)
}
[@@deriving codec]

(** The messages of a user, of one app or of all of them. *)
type feed = { user : int; app : int option } [@@deriving codec]
