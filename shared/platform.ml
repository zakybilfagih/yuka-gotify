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
