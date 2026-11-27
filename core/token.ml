(* Gotify 3 "enhanced" tokens: gtfy<ident>.<base64url key>. The private form
   (an Ed25519 seed) goes to the user once; the database keeps the public form
   (the public key), so a leaked database holds no usable token. Tokens of
   another form (Gotify 2 style random strings) are looked up as they are.

   A client may also send a signed form, gtfy<ident>.<pub>.<timestamp>.<sig>:
   the Ed25519ph signature of "iat=<timestamp>" by its private key, good for
   15 minutes either side of now, so the private form never goes over the
   wire. This follows Gotify's auth/token.go, error messages included. *)

let prefix = "gtfy"
let b64 s = Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet s

let public_of_seed ident seed =
  match Mirage_crypto_ec.Ed25519.priv_of_octets seed with
  | Ok priv ->
      Some (prefix ^ ident ^ "." ^ b64 (Mirage_crypto_ec.Ed25519.pub_to_octets (Mirage_crypto_ec.Ed25519.pub_of_priv priv)))
  | Error _ -> None

(** [generate ident] is [(public, private)]. *)
let generate ident =
  let seed = Mirage_crypto_rng.generate 32 in
  match public_of_seed ident seed with
  | Some public -> (public, prefix ^ ident ^ "." ^ b64 seed)
  | None -> assert false

let application () = generate "a"
let client () = generate "c"

(* Go's base64.RawURLEncoding.DecodeString: CR and LF are skipped, an
   unused low bit may be set, and an error names the offset of the first bad
   byte, or of a lone last character. *)
let go_unb64 s =
  let value c =
    match c with
    | 'A' .. 'Z' -> Some (Char.code c - 65)
    | 'a' .. 'z' -> Some (Char.code c - 71)
    | '0' .. '9' -> Some (Char.code c + 4)
    | '-' -> Some 62
    | '_' -> Some 63
    | _ -> None
  in
  let out = Buffer.create (String.length s) in
  let rec go i acc bits last =
    if i = String.length s then
      if bits = 6 then Error last else Ok (Buffer.contents out)
    else
      match s.[i] with
      | '\r' | '\n' -> go (i + 1) acc bits last
      | c -> (
          match value c with
          | None -> Error i
          | Some v ->
              let acc = (acc lsl 6) lor v and bits = bits + 6 in
              if bits >= 8 then begin
                Buffer.add_char out (Char.chr ((acc lsr (bits - 8)) land 0xff));
                go (i + 1) (acc land ((1 lsl (bits - 8)) - 1)) (bits - 8) i
              end
              else go (i + 1) acc bits i)
  in
  go 0 0 0 0

(* base64.RawURLEncoding.DecodedLen *)
let decoded_len n = n * 6 / 8

(* strconv.ParseInt(s, 10, 64) *)
let parse_int s =
  let fail why = Error (Printf.sprintf "strconv.ParseInt: parsing %S: %s" s why) in
  let neg, digits =
    if s <> "" && (s.[0] = '+' || s.[0] = '-') then (s.[0] = '-', String.sub s 1 (String.length s - 1)) else (false, s)
  in
  if digits = "" || not (String.for_all (function '0' .. '9' -> true | _ -> false) digits) then fail "invalid syntax"
  else
    match Int64.of_string_opt ((if neg then "-" else "") ^ digits) with
    | Some n -> Ok n
    | None -> fail "value out of range"

type checked =
  | Valid of string  (** the form the database keeps *)
  | Stale  (** signed too long ago or ahead: no token, as if none was given *)
  | Refused of int * string

let max_skew = 900L (* 15 minutes *)

(** What a token stands for at time [now] (Unix seconds). A token without
    the gtfy prefix is kept as it is. *)
let check ~now token =
  if not (String.starts_with ~prefix token) then Valid token
  else
    let unparsable why = Refused (401, "cannot parse token: " ^ why) in
    let decode s f = match go_unb64 s with Ok b -> f b | Error i -> unparsable (Printf.sprintf "base64 decode failed: illegal base64 data at input byte %d" i) in
    let rest = String.sub token 4 (String.length token - 4) in
    let fields =
      (* strings.SplitN(rest, ".", 4) *)
      let rec split acc n s =
        match String.index_opt s '.' with
        | Some i when n > 1 -> split (String.sub s 0 i :: acc) (n - 1) (String.sub s (i + 1) (String.length s - i - 1))
        | _ -> List.rev (s :: acc)
      in
      split [] 4 rest
    in
    match fields with
    | [ ident; key ] ->
        decode key (fun seed ->
            if decoded_len (String.length key) <> 32 then unparsable "private key must be 32 bytes"
            else match public_of_seed ident seed with Some p -> Valid p | None -> unparsable "private key must be 32 bytes")
    | [ ident; key; timestamp; signature ] ->
        decode key (fun public ->
            if decoded_len (String.length key) <> 32 then unparsable "public key must be 32 bytes"
            else
              match parse_int timestamp with
              | Error m -> unparsable ("timestamp must be an integer: " ^ m)
              | Ok ts ->
                  if decoded_len (String.length signature) <> 64 then unparsable "signature must be 64 bytes"
                  else
                    decode signature (fun sig_ ->
                        if not (Ed25519ph.verify ~key:public ~signature:sig_ (Printf.sprintf "iat=%Ld" ts)) then
                          Refused (500, "invalid token")
                        else if Int64.compare ts (Int64.sub now max_skew) < 0 || Int64.compare ts (Int64.add now max_skew) > 0
                        then Stale
                        else Valid (prefix ^ ident ^ "." ^ b64 public)))
    | _ -> unparsable "token must have 2 or 4 fields separated by dots"
