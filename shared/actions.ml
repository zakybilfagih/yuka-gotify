let delete_message = Yuka.Action.mutation ~name:"delete_message" ~input:Yuka.Codec.int ~output:Yuka.Codec.unit

(** [None]: every message of the user; [Some app]: those of one app. *)
let delete_all = Yuka.Action.mutation ~name:"delete_all" ~input:(Yuka.Codec.option Yuka.Codec.int) ~output:Yuka.Codec.unit
