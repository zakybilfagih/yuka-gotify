(* The response headers and CORS of the API, as Gotify 3.1.1 sets them
   (router/router.go and auth/cors.go, on gin-contrib/cors 1.7.7): every API
   path but /health and /image gets server.responseheaders, and a request from
   another origin is refused with 403 unless one of server.cors.alloworigins
   matches it. *)

let response_headers : (string * string) list ref = ref []

(** server.cors.alloworigins: regular expressions, matched anywhere in the
    lowercased origin. *)
let allow_origins : Re.re list ref = ref []

let allow_methods : string list ref = ref []
let allow_headers : string list ref = ref []

(** A Perl-style regular expression, as Go's [regexp.MustCompile] takes it. *)
let regex s = try Re.compile (Re.Perl.re s) with _ -> failwith (Printf.sprintf "invalid regular expression: %s" s)

(* Without alloworigins, an Access-Control-Allow-Origin of the response
   headers is the one origin allowed, and "*" allows every origin. *)
let fixed_origins () =
  if !allow_origins <> [] then []
  else
    List.filter_map
      (fun (k, v) -> if String.lowercase_ascii k = "access-control-allow-origin" then Some (String.lowercase_ascii (String.trim v)) else None)
      !response_headers

let allow_all () = List.mem "*" (fixed_origins ())

let valid origin =
  allow_all ()
  || List.mem origin (fixed_origins ())
  || List.exists (fun r -> Re.execp r (String.lowercase_ascii origin)) !allow_origins

(* gin-contrib's normalize: trimmed, lowercased, without repeats. *)
let normalize values =
  List.fold_left
    (fun acc v ->
      let v = String.lowercase_ascii (String.trim v) in
      if List.mem v acc then acc else acc @ [ v ])
    [] values

(* Go's CanonicalHeaderKey: x-gotify-key is X-Gotify-Key. *)
let canonical key =
  String.mapi (fun i c -> if i = 0 || key.[i - 1] = '-' then Char.uppercase_ascii c else c) key

let vary = [ "Origin"; "Access-Control-Request-Method"; "Access-Control-Request-Headers" ]

let preflight_headers () =
  let list name values = if values = [] then [] else [ (name, String.concat "," values) ] in
  list "access-control-allow-methods" (List.map String.uppercase_ascii (normalize !allow_methods))
  @ list "access-control-allow-headers" (List.map canonical (normalize !allow_headers))
  @ [ ("access-control-max-age", "43200") ]
  @ if allow_all () then [ ("access-control-allow-origin", "*") ] else List.map (fun v -> ("vary", v)) vary

let normal_headers () =
  if allow_all () then [ ("access-control-allow-origin", "*") ] else [ ("vary", "Origin") ]

(** Sets the response headers and CORS headers of an API request, and answers
    it when CORS ends it: a preflight or a refused origin. [true] when the
    request goes on to its handler. *)
let apply reqd =
  Http.add_headers reqd !response_headers;
  match Http.header reqd "origin" with
  | None | Some "" -> true
  | Some origin ->
      let host = Option.value (Http.header reqd "host") ~default:"" in
      if origin = "http://" ^ host || origin = "https://" ^ host then true
      else if not (valid origin) then begin
        Http.empty reqd 403;
        false
      end
      else
        let preflight = (Http.request reqd).meth = `OPTIONS in
        Http.add_headers reqd (if preflight then preflight_headers () else normal_headers ());
        if not (allow_all ()) then Http.add_headers reqd [ ("access-control-allow-origin", origin) ];
        if preflight then begin
          Http.empty reqd 204;
          false
        end
        else true
