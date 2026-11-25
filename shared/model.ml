(* What the web UI shows of a message, sent to the island. *)
type message = {
  id : int;
  app_id : int;
  app_name : string;
  title : string;
  body : Markup.node list;
  markdown : bool;
  priority : int;
  time : float;  (** seconds since the epoch, for the relative time *)
  date : string;  (** the full date, formatted on the server, so both renders agree *)
}
[@@deriving codec]

(** The messages of a user, of one app or of all of them: the newest
    [pages] pages of {!Topics.per_page}. *)
type feed = { user : int; app : int option; pages : int } [@@deriving codec]

(** The messages of a feed, newest first, and whether older ones remain. *)
type page = { messages : message list; more : bool } [@@deriving codec]

(** An application in the drawer. *)
type app_link = { id : int; name : string } [@@deriving codec]
