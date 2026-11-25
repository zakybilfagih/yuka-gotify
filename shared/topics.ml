let messages = Yuka.Topic.declare ~name:"messages" ~key:Model.feed_codec ~state:Model.page_codec

let per_page = 100

(* The most pages a feed loads; a feed of this many has no next page, so the
   island does not load the same page again and again. *)
let max_pages = 10_000
