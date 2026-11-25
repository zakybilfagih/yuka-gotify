(* End-to-end tests of the Gotify REST API and /stream. The API runs on
   yuka-server on port 0 in this process, and the tests talk to it over
   sockets, as gotify/cli and the Android app do. Tests share one database
   and run in order. *)

open Gotify_core
module J = Yojson.Safe.Util

let ( >>= ) = Lwt.bind
let ( >|= ) p f = Lwt.map f p
let port = ref 0
let addr () = Unix.ADDR_INET (Unix.inet_addr_loopback, !port)
let run p = Lwt_main.run (Lwt_unix.with_timeout 5. (fun () -> p))

type response = { status : int; body : string }

let read_exactly ic n =
  let b = Bytes.create n in
  Lwt_io.read_into_exactly ic b 0 n >|= fun () -> Bytes.to_string b

let read_response ic =
  Lwt_io.read_line ic >>= fun status_line ->
  let rec headers acc =
    Lwt_io.read_line ic >>= fun l ->
    let l = String.trim l in
    if l = "" then Lwt.return acc
    else
      match String.index_opt l ':' with
      | Some i ->
          let name = String.lowercase_ascii (String.sub l 0 i) in
          headers ((name, String.trim (String.sub l (i + 1) (String.length l - i - 1))) :: acc)
      | None -> headers acc
  in
  headers [] >>= fun hs ->
  let status = match String.split_on_char ' ' status_line with _ :: c :: _ -> int_of_string c | _ -> 0 in
  let len = match List.assoc_opt "content-length" hs with Some n -> int_of_string n | None -> 0 in
  read_exactly ic len >|= fun body -> { status; body }

let request ?(headers = []) ?(body = "") meth path =
  Lwt_io.with_connection (addr ()) (fun (ic, oc) ->
      let headers = ("host", "127.0.0.1") :: ("content-length", string_of_int (String.length body)) :: headers in
      let head = String.concat "" (List.map (fun (k, v) -> k ^ ": " ^ v ^ "\r\n") headers) in
      Lwt_io.write oc (Printf.sprintf "%s %s HTTP/1.1\r\n%s\r\n%s" meth path head body) >>= fun () -> read_response ic)

let basic user pass = ("authorization", "Basic " ^ Base64.encode_string (user ^ ":" ^ pass))
let admin = basic "admin" "admin"
let key t = ("x-gotify-key", t)
let bearer t = ("authorization", "Bearer " ^ t)

let call ?auth ?json ?(headers = []) meth path =
  let headers = (match auth with Some h -> [ h ] | None -> []) @ headers in
  let headers, body =
    match json with
    | Some j -> (("content-type", "application/json") :: headers, Yojson.Safe.to_string j)
    | None -> (headers, "")
  in
  run (request ~headers ~body meth path)

let json r = Yojson.Safe.from_string r.body
let status what expected r = Alcotest.(check int) (Printf.sprintf "%s (%s)" what r.body) expected r.status
let str j name = J.(member name j |> to_string)
let int j name = J.(member name j |> to_int)
let has j name = List.mem_assoc name (J.to_assoc j)
let json_t = Alcotest.testable Yojson.Safe.pp Yojson.Safe.equal

let create_app ?(auth = admin) ?(priority = 0) name =
  let r = call ~auth ~json:(`Assoc [ ("name", `String name); ("defaultPriority", `Int priority) ]) "POST" "/application" in
  status "create app" 200 r;
  let j = json r in
  (int j "id", str j "token")

let create_client ?(auth = admin) name =
  let r = call ~auth ~json:(`Assoc [ ("name", `String name) ]) "POST" "/client" in
  status "create client" 200 r;
  let j = json r in
  (int j "id", str j "token")

let create_user name pass =
  let r =
    call ~auth:admin ~json:(`Assoc [ ("name", `String name); ("pass", `String pass); ("admin", `Bool false) ]) "POST" "/user"
  in
  status "create user" 200 r

let push ?(fields = []) token message =
  call ~auth:(key token) ~json:(`Assoc (("message", `String message) :: fields)) "POST" "/message"

let ids r = List.map (fun m -> int m "id") J.(member "messages" (json r) |> to_list)

(* Tests *)

let open_endpoints () =
  let r = call "GET" "/health" in
  status "health" 200 r;
  Alcotest.(check string) "health" "green" (str (json r) "health");
  status "version" 200 (call "GET" "/version")

let authentication () =
  status "no credentials" 401 (call "GET" "/current/user");
  status "wrong password" 401 (call ~auth:(basic "admin" "nope") "GET" "/current/user");
  let r = call "GET" "/current/user?token=gtfyc.zzz" in
  status "malformed token" 401 r;
  Alcotest.(check string) "description" "cannot parse token" (str (json r) "errorDescription");
  let _, token = create_client "auth" in
  List.iter
    (fun (what, auth, path) ->
      let r = call ?auth "GET" path in
      status what 200 r;
      Alcotest.(check string) what "admin" (str (json r) "name"))
    [
      ("?token=", None, "/current/user?token=" ^ token);
      ("X-Gotify-Key", Some (key token), "/current/user");
      ("Bearer", Some (bearer token), "/current/user");
      ("bearer, any case", Some ("authorization", "bearer " ^ token), "/current/user");
    ];
  let _, app_token = create_app "auth app" in
  status "an app token on a client endpoint" 401 (call ~auth:(key app_token) "GET" "/current/user")

let tokens_shown_once () =
  let id, token = create_app "once" in
  Alcotest.(check bool) "enhanced app token" true (String.starts_with ~prefix:"gtfya." token);
  let listed = List.find (fun a -> int a "id" = id) J.(to_list (json (call ~auth:admin "GET" "/application"))) in
  Alcotest.(check bool) "GET /application has no token" false (has listed "token");
  let cid, ctoken = create_client "once" in
  Alcotest.(check bool) "enhanced client token" true (String.starts_with ~prefix:"gtfyc." ctoken);
  let listed = List.find (fun c -> int c "id" = cid) J.(to_list (json (call ~auth:admin "GET" "/client"))) in
  Alcotest.(check bool) "GET /client has no token" false (has listed "token");
  let r = call ~auth:admin ~json:(`Assoc [ ("regenerateToken", `Bool true) ]) "PUT" (Printf.sprintf "/application/%d/security" id) in
  status "rotate" 200 r;
  let fresh = J.(json r |> member "regenerateToken" |> member "token" |> to_string) in
  Alcotest.(check bool) "new token differs" true (fresh <> token);
  status "old token after rotation" 401 (push token "old");
  status "new token after rotation" 200 (push fresh "new")

let message_defaults () =
  let app, token = create_app ~priority:4 "Defaults" in
  let m = json (push token "no title, no priority") in
  Alcotest.(check string) "title defaults to the app name" "Defaults" (str m "title");
  Alcotest.(check int) "priority defaults to the app's" 4 (int m "priority");
  let m = json (push ~fields:[ ("priority", `Int 0); ("title", `String "  ") ] token "zero") in
  Alcotest.(check int) "an explicit priority 0 stays 0" 0 (int m "priority");
  Alcotest.(check string) "a blank title is the app name" "Defaults" (str m "title");
  let r =
    run
      (request
         ~headers:[ ("content-type", "application/x-www-form-urlencoded") ]
         ~body:"title=Form+post&message=from%20a%20form&priority=7" "POST" ("/message?token=" ^ token))
  in
  status "form post" 200 r;
  let m = json r in
  Alcotest.(check (list string)) "form fields" [ "Form post"; "from a form" ] [ str m "title"; str m "message" ];
  Alcotest.(check int) "form priority" 7 (int m "priority");
  let extras = `Assoc [ ("client::display", `Assoc [ ("contentType", `String "text/markdown") ]) ] in
  let m = json (push ~fields:[ ("extras", extras) ] token "**md**") in
  Alcotest.check json_t "extras round-trip" extras (J.member "extras" m);
  status "no message" 400 (call ~auth:(key token) ~json:(`Assoc [ ("title", `String "x") ]) "POST" "/message");
  let _, ctoken = create_client "sender" in
  status "client token without appid" 400 (push ctoken "where to?");
  let r = push ~fields:[ ("appid", `Int app) ] ctoken "with appid" in
  status "client token with appid" 200 r;
  Alcotest.(check int) "appid" app (int (json r) "appid")

let paging () =
  let app, token = create_app "pages" in
  let pushed = List.map (fun i -> int (json (push token (string_of_int i))) "id") [ 1; 2; 3; 4; 5 ] in
  let id n = List.nth pushed (n - 1) in
  let page path =
    let r = call ~auth:admin "GET" path in
    status path 200 r;
    (ids r, J.member "paging" (json r))
  in
  let first, paging = page (Printf.sprintf "/application/%d/message?limit=2" app) in
  Alcotest.(check (list int)) "first page, newest first" [ id 5; id 4 ] first;
  Alcotest.(check int) "since" (id 4) (int paging "since");
  Alcotest.(check int) "size" 2 (int paging "size");
  let next = str paging "next" in
  Alcotest.(check string) "next is relative" (Printf.sprintf "/application/%d/message?limit=2&since=%d" app (id 4)) next;
  let second, paging = page next in
  Alcotest.(check (list int)) "second page" [ id 3; id 2 ] second;
  let last, paging = page (str paging "next") in
  Alcotest.(check (list int)) "last page" [ id 1 ] last;
  Alcotest.(check bool) "no next on the last page" false (has paging "next");
  Alcotest.(check int) "since 0 on the last page" 0 (int paging "since");
  List.iter
    (fun q -> status ("limit " ^ q) 400 (call ~auth:admin "GET" ("/message?limit=" ^ q)))
    [ "0"; "201"; "abc" ];
  status "limit 200" 200 (call ~auth:admin "GET" "/message?limit=200")

let ownership () =
  status "registration is off" 401
    (call ~json:(`Assoc [ ("name", `String "eve"); ("pass", `String "x") ]) "POST" "/user");
  create_user "bob" "bobpass";
  let bob = basic "bob" "bobpass" in
  let admin_app, _ = create_app "admin's" in
  let _, bob_token = create_app ~auth:bob "bob's" in
  let bob_msg = int (json (push bob_token "secret")) "id" in
  Alcotest.(check bool) "admin does not see bob's message" false
    (List.mem bob_msg (ids (call ~auth:admin "GET" "/message?limit=200")));
  status "admin deletes bob's message" 404 (call ~auth:admin "DELETE" (Printf.sprintf "/message/%d" bob_msg));
  status "bob reads admin's app" 404 (call ~auth:bob "GET" (Printf.sprintf "/application/%d/message" admin_app));
  status "bob lists users" 403 (call ~auth:bob "GET" "/user");
  status "bob deletes his message" 200 (call ~auth:bob "DELETE" (Printf.sprintf "/message/%d" bob_msg));
  Alcotest.(check bool) "gone" false (List.mem bob_msg (ids (call ~auth:bob "GET" "/message")))

let elevation () =
  let r = call ~auth:admin ~json:(`Assoc [ ("name", `String "web") ]) "POST" "/auth/local/login" in
  status "login" 200 r;
  let j = json r in
  let token = str j "token" and client = int j "clientId" in
  Alcotest.(check bool) "a login is elevated" true (has j "elevatedUntil");
  let app, _ = create_app "to delete" in
  status "end the elevation" 204
    (call ~auth:(bearer token) ~json:(`Assoc [ ("durationSeconds", `Int (-10)) ]) "POST" (Printf.sprintf "/client/%d/elevate" client));
  Alcotest.(check bool) "no longer elevated" false (has (json (call ~auth:(bearer token) "GET" "/current/user")) "elevatedUntil");
  let r = call ~auth:(bearer token) "DELETE" (Printf.sprintf "/application/%d" app) in
  status "delete without elevation" 403 r;
  Alcotest.(check string) "description" "session not elevated, use basic auth or call /client:elevate"
    (str (json r) "errorDescription");
  status "elevate without elevation" 403
    (call ~auth:(bearer token) ~json:(`Assoc [ ("durationSeconds", `Int 60) ]) "POST" (Printf.sprintf "/client/%d/elevate" client));
  status "basic auth needs no elevation" 200 (call ~auth:admin "DELETE" (Printf.sprintf "/application/%d" app));
  status "logout" 200 (call ~auth:(bearer token) "POST" "/auth/logout");
  status "token after logout" 401 (call ~auth:(bearer token) "GET" "/current/user")

(* A WebSocket client that only reads: server frames are not masked. *)
let ws_open path =
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.connect fd (addr ()) >>= fun () ->
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd and oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt_io.write oc
    (Printf.sprintf
       "GET %s HTTP/1.1\r\nHost: 127.0.0.1\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n\
        Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n\r\n"
       path)
  >>= fun () ->
  read_response ic >|= fun r -> (r.status, ic, fd)

let rec next_text ic =
  Lwt.catch
    (fun () ->
      read_exactly ic 2 >>= fun h ->
      let op = Char.code h.[0] land 0x0f and len = Char.code h.[1] land 0x7f in
      (if len = 126 then read_exactly ic 2 >|= fun s -> (Char.code s.[0] lsl 8) lor Char.code s.[1]
       else if len = 127 then
         read_exactly ic 8 >|= fun s ->
         let n = ref 0 in
         String.iter (fun c -> n := (!n lsl 8) lor Char.code c) s;
         !n
       else Lwt.return len)
      >>= fun len ->
      read_exactly ic len >>= fun payload ->
      match op with 1 -> Lwt.return (`Text payload) | 8 -> Lwt.return `Closed | _ -> next_text ic)
    (function End_of_file | Unix.Unix_error _ -> Lwt.return `Closed | e -> Lwt.fail e)

let stream () =
  let refused, _, fd = run (ws_open "/stream?token=gtfyc.zzz") in
  run (Lwt_unix.close fd);
  Alcotest.(check int) "a bad token gets no socket" 401 refused;
  create_user "carol" "carolpass";
  let _, carol_token = create_app ~auth:(basic "carol" "carolpass") "carol's" in
  let client, ctoken = create_client "phone" in
  let _, app_token = create_app "streamed" in
  let code, ic, fd = run (ws_open ("/stream?token=" ^ ctoken)) in
  Alcotest.(check int) "upgrade" 101 code;
  status "another user's push" 200 (push carol_token "not for admin");
  let sent = push ~fields:[ ("title", `String "To the phone"); ("priority", `Int 8) ] app_token "hello phone" in
  status "push" 200 sent;
  (match run (next_text ic) with
  | `Text text -> Alcotest.check json_t "the stream gets the pushed message, and only admin's" (json sent) (Yojson.Safe.from_string text)
  | `Closed -> Alcotest.fail "the stream closed before the message");
  status "delete the client" 200 (call ~auth:admin "DELETE" (Printf.sprintf "/client/%d" client));
  (match run (next_text ic) with
  | `Closed -> ()
  | `Text t -> Alcotest.fail ("expected the socket of a deleted client to close, got " ^ t));
  run (Lwt_unix.close fd)

let last_admin () =
  let me = int (json (call ~auth:admin "GET" "/current/user")) "id" in
  let r = call ~auth:admin "DELETE" (Printf.sprintf "/user/%d" me) in
  status "delete the last admin" 400 r;
  Alcotest.(check string) "description" "cannot delete last admin" (str (json r) "errorDescription");
  status "demote the last admin" 400
    (call ~auth:admin ~json:(`Assoc [ ("name", `String "admin"); ("admin", `Bool false) ]) "POST" (Printf.sprintf "/user/%d" me));
  status "still an admin" 200 (call ~auth:admin "GET" "/user")

let () =
  Mirage_crypto_rng_unix.use_default ();
  let db = Filename.temp_file "gotify-test" ".db" in
  Lwt_main.run (Db.connect db >>= fun () -> Api.create_default_user ~name:"admin" ~pass:"admin");
  let ready, wake = Lwt.wait () in
  Lwt.async (fun () ->
      Yuka_server.serve ~port:0
        ~listening:(fun p ->
          port := p;
          Lwt.wakeup_later wake ())
        (fun _ reqd -> if not (Api.dispatch reqd) then Http.error reqd 404 "page not found"));
  Lwt_main.run ready;
  let test name f = Alcotest.test_case name `Quick f in
  Alcotest.run "gotify-api"
    [
      ( "api",
        [
          test "health and version need no auth" open_endpoints;
          test "token and basic auth, in Gotify's order" authentication;
          test "tokens are shown once and rotate" tokens_shown_once;
          test "message title, priority and extras" message_defaults;
          test "paging with a relative next" paging;
          test "users see only their own data" ownership;
          test "elevated sessions" elevation;
          test "stream delivers and closes" stream;
          test "the last admin stays" last_admin;
        ] );
    ]
