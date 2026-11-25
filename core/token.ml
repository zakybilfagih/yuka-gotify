(* Gotify 3 "enhanced" tokens: gtfy<ident>.<base64url key>. The private form
   (an Ed25519 seed) goes to the user once; the database keeps the public form
   (the public key), so a leaked database holds no usable token. Tokens of
   another form (Gotify 2 style random strings) are looked up as they are.

   The signed form (gtfy<ident>.<pub>.<timestamp>.<signature>) uses Ed25519ph,
   which mirage-crypto-ec does not offer: such a token is refused. *)

let prefix = "gtfy"
let b64 s = Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet s
let unb64 s = Base64.decode ~pad:false ~alphabet:Base64.uri_safe_alphabet s

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

(** The form the database keeps, or [Error ()] when the token has the gtfy
    prefix and does not parse. *)
let canonical token =
  if not (String.starts_with ~prefix token) then Ok token
  else
    let rest = String.sub token 4 (String.length token - 4) in
    match String.split_on_char '.' rest with
    | [ ident; key ] -> (
        match unb64 key with
        | Ok seed when String.length seed = 32 -> (
            match public_of_seed ident seed with Some p -> Ok p | None -> Error ())
        | _ -> Error ())
    | _ -> Error ()
