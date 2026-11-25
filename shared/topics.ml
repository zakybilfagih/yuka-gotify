(* The newest messages of a feed, newest first. *)
let messages = Yuka.Topic.declare ~name:"messages" ~key:Model.feed_codec ~state:(Yuka.Codec.list Model.message_codec)

let limit = 100
