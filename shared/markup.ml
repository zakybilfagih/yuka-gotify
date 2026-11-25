(* A message body as a small typed tree that the server makes (from Markdown
   or plain text) and the server and the island render the same way. There
   is no raw HTML: the server only makes the tags of Markdown_render. *)

type node = Text of string | Element of { tag : string; attributes : (string * string) list; children : node list }

let rec to_json = function
  | Text text -> `String text
  | Element { tag; attributes; children } ->
      `List (`String tag :: `Assoc (List.map (fun (name, value) -> (name, `String value)) attributes) :: List.map to_json children)

let rec all = function [] -> Some [] | Some v :: rest -> Option.map (fun vs -> v :: vs) (all rest) | None :: _ -> None

let rec of_json = function
  | `String text -> Some (Text text)
  | `List (`String tag :: `Assoc attributes :: children) -> (
      let attribute = function name, `String value -> Some (name, value) | _ -> None in
      match (all (List.map attribute attributes), all (List.map of_json children)) with
      | Some attributes, Some children -> Some (Element { tag; attributes; children })
      | _ -> None)
  | _ -> None

let node_codec =
  Yuka.Codec.make ~encode:to_json ~decode:(fun json ->
      Option.to_result ~none:"markup: expected a text or [tag, attributes, ...children]" (of_json json))

let rec view : node -> _ Yuka.element = function
  | Text text -> Yuka.text text
  | Element { tag; attributes; children } ->
      Yuka.element tag (List.map (fun (name, value) -> Yuka.attr name value) attributes) (List.map view children)
