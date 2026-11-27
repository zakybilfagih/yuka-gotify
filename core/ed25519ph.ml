(* Verification of Ed25519ph signatures (RFC 8032, 5.1), which Gotify's
   signed tokens use and mirage-crypto-ec does not offer. It checks as Go's
   crypto/ed25519 does: S must be below the group order, the public key may
   be any encoding of a point, and R must be the exact encoding of
   [S]B - [k]A. Arithmetic is on Zarith and not constant time, which
   verification does not need: every input is public. *)

let p = Z.(shift_left one 255 - of_int 19)
let order = Z.(shift_left one 252 + of_string "27742317777372353535851937790883648493")
let ( %% ) a b = Z.erem a b
let ( +: ) a b = Z.(a + b) %% p
let ( -: ) a b = Z.(a - b) %% p
let ( *: ) a b = Z.(a * b) %% p
let inv a = Z.powm a Z.(p - of_int 2) p
let d = Z.of_int (-121665) *: inv (Z.of_int 121666)
let d2 = Z.of_int 2 *: d
let sqrt_m1 = Z.powm (Z.of_int 2) Z.((p - one) / of_int 4) p

(* Little-endian bytes and integers. *)
let of_le s = Z.of_bits s

let to_le32 z =
  let b = Z.to_bits z in
  String.sub (b ^ String.make 32 '\000') 0 32

(* A point in extended coordinates (X:Y:Z:T), with x = X/Z, y = Y/Z, xy = T/Z. *)
type point = { x : Z.t; y : Z.t; z : Z.t; t : Z.t }

let identity = { x = Z.zero; y = Z.one; z = Z.one; t = Z.zero }

(* RFC 8032's addition, which is complete: it also doubles. *)
let add a b =
  let a' = (a.y -: a.x) *: (b.y -: b.x) and b' = (a.y +: a.x) *: (b.y +: b.x) in
  let c = a.t *: d2 *: b.t and d' = Z.of_int 2 *: a.z *: b.z in
  let e = b' -: a' and f = d' -: c and g = d' +: c and h = b' +: a' in
  { x = e *: f; y = g *: h; z = f *: g; t = e *: h }

let neg a = { a with x = Z.zero -: a.x; t = Z.zero -: a.t }

let mul k pt =
  let rec go k acc pt = if Z.equal k Z.zero then acc else go (Z.shift_right k 1) (if Z.testbit k 0 then add acc pt else acc) (add pt pt) in
  go k identity pt

let encode pt =
  let zi = inv pt.z in
  let x = pt.x *: zi and y = pt.y *: zi in
  let b = Bytes.of_string (to_le32 y) in
  if Z.testbit x 0 then Bytes.set b 31 (Char.chr (Char.code (Bytes.get b 31) lor 0x80));
  Bytes.to_string b

(* RFC 8032 5.1.3, but y may be p or more (Go accepts those encodings). *)
let decode s =
  if String.length s <> 32 then None
  else
    let sign = Char.code s.[31] lsr 7 = 1 in
    let y = of_le (String.mapi (fun i c -> if i = 31 then Char.chr (Char.code c land 0x7f) else c) s) %% p in
    let u = (y *: y) -: Z.one and v = (d *: y *: y) +: Z.one in
    let x2 = u *: inv v in
    let x = Z.powm x2 Z.((p + of_int 3) / of_int 8) p in
    let x = if Z.equal (x *: x) x2 then Some x else if Z.equal (x *: x) (Z.zero -: x2) then Some (x *: sqrt_m1) else None in
    match x with
    | None -> None
    | Some x when Z.equal x Z.zero && sign -> None
    | Some x ->
        let x = if Z.testbit x 0 <> sign then Z.zero -: x else x in
        Some { x; y; z = Z.one; t = x *: y }

let base =
  let y = Z.of_int 4 *: inv (Z.of_int 5) in
  match decode (to_le32 y) with Some b -> b | None -> assert false

let sha512 s = Digestif.SHA512.(to_raw_string (digest_string s))

(* dom2(1, ""): Ed25519ph without a context. *)
let dom2 = "SigEd25519 no Ed25519 collisions\001\000"

(** [verify ~key ~signature message]: Ed25519ph of SHA-512 of [message]. *)
let verify ~key ~signature message =
  String.length signature = 64
  &&
  let r = String.sub signature 0 32 and s = of_le (String.sub signature 32 32) in
  Z.lt s order
  &&
  match decode key with
  | None -> false
  | Some a ->
      let k = of_le (sha512 (dom2 ^ r ^ key ^ sha512 message)) %% order in
      String.equal (encode (add (mul s base) (neg (mul k a)))) r
