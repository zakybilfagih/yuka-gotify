(* What the islands need from the platform and Yuka does not give. The
   browser sets these in client/main.ml with JavaScript; the server sets
   [now] in server/main.ml, so both render the same relative times. *)

(** Seconds since the epoch. *)
let now : (unit -> float) ref = ref (fun () -> 0.)

(** [every ms f] calls [f] every [ms] milliseconds and gives the function
    that stops it. *)
let every : (int -> (unit -> unit) -> unit -> unit) ref = ref (fun _ _ () -> ())

(** Copies the text to the clipboard; the promise of the browser decides,
    so the callback gets whether it worked. *)
let copy : (string -> (bool -> unit) -> unit) ref = ref (fun _ k -> k false)

(** The permission of the Notification API: ["default"], ["granted"],
    ["denied"], or ["unsupported"] when the browser has none. *)
let permission : (unit -> string) ref = ref (fun () -> "unsupported")

(** Asks for the permission and gives the answer. *)
let request_permission : ((string -> unit) -> unit) ref = ref (fun k -> k "unsupported")

(** [notify ~title ~body ~icon] shows a desktop notification; a click on it
    brings the tab to the front. *)
let notify : (title:string -> body:string -> icon:string -> unit) ref = ref (fun ~title:_ ~body:_ ~icon:_ -> ())

(** [after ms f] calls [f] once, in [ms] milliseconds, and gives the
    function that cancels it. *)
let after : (int -> (unit -> unit) -> unit -> unit) ref = ref (fun _ _ () -> ())

(** Whether the element with this id has more content than it shows. *)
let overflows : (string -> bool) ref = ref (fun _ -> false)

(** [on_image_load f] calls [f] each time an image in the page finishes
    loading, and gives the function that stops it. *)
let on_image_load : ((unit -> unit) -> unit -> unit) ref = ref (fun _ () -> ())

(** How far the window is scrolled down, in CSS pixels. *)
let scroll_y : (unit -> float) ref = ref (fun () -> 0.)

(** Scrolls the window to the top, smoothly. *)
let scroll_to_top : (unit -> unit) ref = ref (fun () -> ())

(** In a dragstart: sets the data of the drag, which Firefox needs to start
    it, and makes it a move. *)
let start_drag : (Yuka.Event.t -> unit) ref = ref (fun _ -> ())
