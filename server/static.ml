(* The files of static/. Outside dev, pages link them with ?v= and a digest
   of the directory ([url]), so a change to a file gives new URLs, and a
   request with the current digest gets the file from memory with an
   immutable cache-control ([serve]). Other /static requests, such as the
   static/defaultapp.png of the API's application images, and all of them
   in dev, go to the ~public of Yuka, which reads the disk and sets no
   cache-control. *)

module H = Gotify_core.Http

let files = ref []
let version = ref None

let load ~dev dir =
  if not dev then begin
    files :=
      List.filter_map
        (fun name ->
          let path = Filename.concat dir name in
          if Sys.is_directory path then None else Some (name, In_channel.with_open_bin path In_channel.input_all))
        (Array.to_list (Sys.readdir dir));
    version := Some (String.sub (Yuka.Server.tree_digest dir) 0 12)
  end

(** The URL of the file [name] of static/. *)
let url name =
  let path = "/static/" ^ name in
  match !version with Some v -> path ^ "?v=" ^ v | None -> path

(** Answers a GET or HEAD of a versioned URL and returns true; false for any other request. *)
let serve reqd =
  match !version with
  | None -> false
  | Some v -> (
      let path, query = H.path_and_query reqd in
      match (String.split_on_char '/' path, (H.request reqd).meth) with
      | [ ""; "static"; name ], (`GET | `HEAD) when List.mem ("v", v) (H.parse_urlencoded query) -> (
          match List.assoc_opt name !files with
          | Some body ->
              H.respond reqd
                ~headers:
                  [
                    ("content-type", Magic_mime.lookup name);
                    ("cache-control", "public, max-age=31536000, immutable");
                    ("x-content-type-options", "nosniff");
                  ]
                200 body;
              true
          | None -> false)
      | _ -> false)
