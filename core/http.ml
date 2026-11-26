(* Small helpers over ache for the Gotify REST API. *)

module Reqd = Ache.Request_descriptor

let request reqd = Reqd.request reqd
let header reqd name = Ache.Headers.get (request reqd).headers name

let path_and_query reqd =
  let target = (request reqd).target in
  match String.index_opt target '?' with
  | Some i -> (String.sub target 0 i, String.sub target (i + 1) (String.length target - i - 1))
  | None -> (target, "")

(* Gotify (gin) drops repeated and trailing slashes. *)
let segments path = List.filter (( <> ) "") (String.split_on_char '/' path)

let decode s = Uri.pct_decode (String.map (function '+' -> ' ' | c -> c) s)

let parse_urlencoded s =
  if s = "" then []
  else
    List.filter_map
      (fun pair ->
        if pair = "" then None
        else
          match String.index_opt pair '=' with
          | Some i -> Some (decode (String.sub pair 0 i), decode (String.sub pair (i + 1) (String.length pair - i - 1)))
          | None -> Some (decode pair, ""))
      (String.split_on_char '&' s)

let query reqd = parse_urlencoded (snd (path_and_query reqd))
let limit = 1 lsl 20

let read_body ?(limit = limit) reqd =
  let reader = Reqd.request_body reqd in
  match Ache.Request.body_length (request reqd) with
  | `Fixed 0L -> Ache.Body.Reader.close reader; Lwt.return (Ok "")
  | `Error `Bad_request -> Lwt.return (Error `Bad)
  | `Fixed n when Int64.compare n (Int64.of_int limit) > 0 -> Ache.Body.Reader.close reader; Lwt.return (Error `Too_large)
  | `Fixed _ | `Chunked ->
      let p, w = Lwt.wait () in
      let buffer = Buffer.create 256 in
      let rec next () =
        Ache.Body.Reader.schedule_read reader
          ~on_eof:(fun () -> Lwt.wakeup_later w (Ok (Buffer.contents buffer)))
          ~on_error:(fun () -> Lwt.wakeup_later w (Error `Bad))
          ~on_read:(fun data ~off ~len ->
            if Buffer.length buffer + len > limit then begin
              Ache.Body.Reader.close reader;
              Lwt.wakeup_later w (Error `Too_large)
            end
            else begin
              Buffer.add_string buffer (Ache.Bigstring.substring data ~off ~len);
              next ()
            end)
      in
      next ();
      p

(* multipart/form-data: each part as its name, the filename of a file part,
   and its value. *)
let parse_multipart ~boundary body =
  let delimiter = "--" ^ boundary in
  let find s sub from =
    let n = String.length s and m = String.length sub in
    let rec matches i k = k = m || (s.[i + k] = sub.[k] && matches i (k + 1)) in
    let rec go i = if i + m > n then None else if matches i 0 then Some i else go (i + 1) in
    go from
  in
  (* The parameters of the Content-Disposition line of a part's head, by exact key. *)
  let disposition head =
    List.concat_map
      (fun line ->
        let line = String.trim line in
        if String.starts_with ~prefix:"content-disposition:" (String.lowercase_ascii line) then
          List.filter_map
            (fun p ->
              match String.index_opt p '=' with
              | Some i ->
                  let v = String.trim (String.sub p (i + 1) (String.length p - i - 1)) in
                  let v = if String.length v >= 2 && v.[0] = '"' then String.sub v 1 (String.length v - 2) else v in
                  Some (String.lowercase_ascii (String.trim (String.sub p 0 i)), v)
              | None -> None)
            (String.split_on_char ';' line)
        else [])
      (String.split_on_char '\n' head)
  in
  let rec parts from acc =
    match find body delimiter from with
    | None -> List.rev acc
    | Some i -> (
        let start = i + String.length delimiter in
        if start + 2 <= String.length body && String.sub body start 2 = "--" then List.rev acc
        else
          match find body delimiter start with
          | None -> List.rev acc
          | Some j ->
              let acc =
                match find body "\r\n\r\n" start with
                | Some h when h < j -> (
                    let head = String.sub body start (h - start) in
                    let stop = if j >= 2 && String.sub body (j - 2) 2 = "\r\n" then j - 2 else j in
                    let value = String.sub body (h + 4) (max 0 (stop - h - 4)) in
                    let params = disposition head in
                    match List.assoc_opt "name" params with
                    | Some name -> (name, List.assoc_opt "filename" params, value) :: acc
                    | None -> acc)
                | _ -> acc
              in
              parts j acc)
  in
  parts 0 []

(** The boundary of a multipart/form-data Content-Type. *)
let boundary_of content_type =
  if not (String.starts_with ~prefix:"multipart/form-data" (String.lowercase_ascii content_type)) then None
  else
    List.find_map
      (fun p ->
        let p = String.trim p in
        if String.starts_with ~prefix:"boundary=" (String.lowercase_ascii p) then
          let b = String.sub p 9 (String.length p - 9) in
          Some (if String.length b >= 2 && b.[0] = '"' then String.sub b 1 (String.length b - 2) else b)
        else None)
      (List.tl (String.split_on_char ';' content_type))

(** The file parts of a multipart body, as [(field, filename, data)]. *)
let files ~boundary body =
  List.filter_map (fun (name, filename, value) -> Option.map (fun f -> (name, f, value)) filename) (parse_multipart ~boundary body)

type body = Params of (string * string) list | Json of Yojson.Safe.t

let content_type reqd = Option.map String.lowercase_ascii (header reqd "content-type")

(** The query and the body, as gin's [Bind] reads them. *)
let body reqd =
  Lwt.map
    (function
      | Error _ as e -> e
      | Ok text -> (
          let q = query reqd in
          match content_type reqd with
          | Some ct when String.starts_with ~prefix:"application/json" ct -> (
              try Ok (Json (Yojson.Safe.from_string (if text = "" then "{}" else text)))
              with Yojson.Json_error m -> Error (`Json m))
          | Some ct when String.starts_with ~prefix:"multipart/form-data" ct ->
              let fields =
                match Option.bind (header reqd "content-type") boundary_of with
                | Some b ->
                    List.filter_map
                      (function name, None, value -> Some (name, value) | _, Some _, _ -> None)
                      (parse_multipart ~boundary:b text)
                | None -> []
              in
              Ok (Params (q @ fields))
          | _ -> Ok (Params (q @ parse_urlencoded text))))
    (read_body reqd)

(* Field access over either form of the body. *)
let field b name =
  match b with
  | Params ps -> Option.map (fun v -> `String v) (List.assoc_opt name ps)
  | Json (`Assoc kv) -> ( match List.assoc_opt name kv with Some `Null | None -> None | Some v -> Some v)
  | Json _ -> None

let string_field b name = match field b name with Some (`String s) -> Some s | Some (`Int i) -> Some (string_of_int i) | _ -> None

let int_field b name =
  match field b name with
  | Some (`Int i) -> Some (Ok i)
  | Some (`Float f) when Float.is_integer f -> Some (Ok (int_of_float f))
  | Some (`String s) -> ( match int_of_string_opt (String.trim s) with Some i -> Some (Ok i) | None -> Some (Error ()))
  | Some _ -> Some (Error ())
  | None -> None

let bool_field b name =
  match field b name with
  | Some (`Bool v) -> Some v
  | Some (`String s) -> Some (List.mem (String.lowercase_ascii s) [ "true"; "1"; "on" ])
  | _ -> None

(* Answers *)

let status_text code =
  match code with
  | 400 -> "Bad Request"
  | 401 -> "Unauthorized"
  | 403 -> "Forbidden"
  | 404 -> "Not Found"
  | 405 -> "Method Not Allowed"
  | 413 -> "Request Entity Too Large"
  | 500 -> "Internal Server Error"
  | _ -> "Error"

let respond reqd ?(headers = []) code text =
  let headers =
    if code = 204 || code = 304 then headers else ("content-length", string_of_int (String.length text)) :: headers
  in
  let headers = Ache.Headers.of_list headers in
  let text = if (request reqd).meth = `HEAD then "" else text in
  Reqd.respond_with_string reqd (Ache.Response.create ~headers (Ache.Status.of_code code)) text

let json reqd ?(code = 200) value =
  respond reqd ~headers:[ ("content-type", "application/json") ] code (Yojson.Safe.to_string value)

let empty reqd code = respond reqd ~headers:[ ("content-type", "application/json") ] code ""

let error reqd code description =
  json reqd ~code
    (`Assoc
      [
        ("error", `String (status_text code)); ("errorCode", `Int code); ("errorDescription", `String description);
      ])
