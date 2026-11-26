(* Application images, as Gotify 3.1.1 keeps them: a file in [dir] with a
   random name and the extension of the upload, served at /image/<name>
   without authentication. The database keeps the name; "" is the default
   image, static/defaultapp.png. *)

let dir = ref "images"

(** The largest upload. Gotify has none; this keeps a request in memory. *)
let max_size = 10 lsl 20

let valid_ext ext = List.mem (String.lowercase_ascii ext) [ ".gif"; ".png"; ".jpg"; ".jpeg" ]

(* Gotify asks filetype.IsImage of the first bytes: these are the formats a
   browser shows in an <img>. *)
let is_image data =
  let has prefix = String.starts_with ~prefix data in
  has "\x89PNG\r\n\x1a\n" || has "\xff\xd8\xff" || has "GIF87a" || has "GIF89a" || has "BM"
  || (String.length data >= 12 && String.sub data 0 4 = "RIFF" && String.sub data 8 4 = "WEBP")

(** The API's image of an application: its path, relative to the server. *)
let path_of name = if name = "" then "static/defaultapp.png" else "image/" ^ name

let file name = Filename.concat !dir name

(** Checks an upload as Gotify does and writes it under a new name, which it
    gives; [Error] has Gotify's message. *)
let save ~filename data =
  let ext = Filename.extension filename in
  if not (is_image data) then Error "file must be an image"
  else if not (valid_ext ext) then Error "invalid file extension"
  else begin
    let rec fresh () =
      let name =
        Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet (Mirage_crypto_rng.generate 18)
        ^ String.lowercase_ascii ext
      in
      if Sys.file_exists (file name) then fresh () else name
    in
    let name = fresh () in
    Out_channel.with_open_bin (file name) (fun oc -> Out_channel.output_string oc data);
    Ok name
  end

let remove name = if name <> "" then try Sys.remove (file name) with Sys_error _ -> ()

let content_type name =
  match String.lowercase_ascii (Filename.extension name) with
  | ".png" -> "image/png"
  | ".gif" -> "image/gif"
  | _ -> "image/jpeg"

(** GET or HEAD /image/<name>: the file, under a name that is never reused. *)
let serve reqd name =
  let not_found () = Http.error reqd 404 "image not found" in
  if name = "" || name.[0] = '.' || String.contains name '\\' || not (valid_ext (Filename.extension name)) then not_found ()
  else
    match In_channel.with_open_bin (file name) In_channel.input_all with
    | exception Sys_error _ -> not_found ()
    | data ->
      Http.respond reqd
        ~headers:
          [
            ("content-type", content_type name);
            ("cache-control", "public, max-age=31536000, immutable");
            ("x-content-type-options", "nosniff");
          ]
        200 data
