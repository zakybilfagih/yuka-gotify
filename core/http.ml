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

let read_body reqd =
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

(* multipart/form-data: text fields only (Gotify's image upload is out of scope). *)
let parse_multipart ~boundary body =
  let delimiter = "--" ^ boundary in
  let find s sub from =
    let n = String.length s and m = String.length sub in
    let rec go i = if i + m > n then None else if String.sub s i m = sub then Some i else go (i + 1) in
    go from
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
              let part = String.sub body start (j - start) in
              let acc =
                match find part "\r\n\r\n" 0 with
                | None -> acc
                | Some h ->
                    let head = String.lowercase_ascii (String.sub part 0 h) in
                    let value = String.sub part (h + 4) (String.length part - h - 4) in
                    let value =
                      if String.ends_with ~suffix:"\r\n" value then String.sub value 0 (String.length value - 2)
                      else value
                    in
                    let name =
                      match find head "name=\"" 0 with
                      | None -> None
                      | Some k -> (
                          let k = k + 6 in
                          match String.index_from_opt head k '"' with
                          | Some e ->
                              (* the original case of the name *)
                              Some (String.sub (String.sub part 0 h) k (e - k))
                          | None -> None)
                    in
                    let is_file = Option.is_some (find head "filename=" 0) in
                    (match name with Some n when not is_file -> (n, value) :: acc | _ -> acc)
              in
              parts j acc)
  in
  parts 0 []

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
              let boundary =
                match String.split_on_char ';' (Option.get (header reqd "content-type")) with
                | _ :: params ->
                    List.find_map
                      (fun p ->
                        let p = String.trim p in
                        if String.starts_with ~prefix:"boundary=" (String.lowercase_ascii p) then
                          let b = String.sub p 9 (String.length p - 9) in
                          Some
                            (if String.length b >= 2 && b.[0] = '"' then String.sub b 1 (String.length b - 2) else b)
                        else None)
                      params
                | [] -> None
              in
              Ok (Params (q @ match boundary with Some b -> parse_multipart ~boundary:b text | None -> []))
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
