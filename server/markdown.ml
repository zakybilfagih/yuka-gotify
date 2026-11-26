(* Message bodies: Markdown (when the extras say client::display text/markdown)
   or plain text, into App_shared.Markup. Links and images keep only http(s)
   URLs; raw HTML stays text. *)

open Cmarkit
module Markup = App_shared.Markup

let element ?(attributes = []) tag children = Markup.Element { tag; attributes; children }

let safe_url url =
  let url = String.trim url in
  let lower = String.lowercase_ascii url in
  if String.starts_with ~prefix:"http://" lower || String.starts_with ~prefix:"https://" lower then Some url else None

let plain_text inline =
  String.concat "\n" (List.map (String.concat "") (Inline.to_plain_text ~break_on_soft:false inline))

let destination defs target =
  match Inline.Link.reference_definition defs target with
  | Some (Link_definition.Def (d, _)) -> Option.map fst (Link_definition.dest d)
  | _ -> None

let rec inline defs (node : Inline.t) : Markup.node list =
  match node with
  | Inline.Text (text, _) -> [ Text text ]
  | Inline.Inlines (inlines, _) -> List.concat_map (inline defs) inlines
  | Inline.Emphasis (e, _) -> [ element "em" (inline defs (Inline.Emphasis.inline e)) ]
  | Inline.Strong_emphasis (e, _) -> [ element "strong" (inline defs (Inline.Emphasis.inline e)) ]
  | Inline.Ext_strikethrough (s, _) -> [ element "del" (inline defs (Inline.Strikethrough.inline s)) ]
  | Inline.Code_span (span, _) -> [ element "code" [ Text (Inline.Code_span.code span) ] ]
  | Inline.Break (b, _) -> ( match Inline.Break.type' b with `Hard -> [ element "br" [] ] | `Soft -> [ Text "\n" ])
  | Inline.Autolink (a, _) -> (
      let text, _ = Inline.Autolink.link a in
      match safe_url text with
      | Some href -> [ element "a" ~attributes:[ ("href", href); ("rel", "nofollow noopener noreferrer"); ("target", "_blank") ] [ Text text ] ]
      | None -> [ Text text ])
  | Inline.Link (target, _) -> (
      let children = inline defs (Inline.Link.text target) in
      match Option.bind (destination defs target) safe_url with
      | Some href -> [ element "a" ~attributes:[ ("href", href); ("rel", "nofollow noopener noreferrer"); ("target", "_blank") ] children ]
      | None -> children)
  | Inline.Image (target, _) -> (
      let alt = plain_text (Inline.Link.text target) in
      match Option.bind (destination defs target) safe_url with
      | Some src -> [ element "img" ~attributes:[ ("src", src); ("alt", alt); ("loading", "lazy") ] [] ]
      | None -> [ Text alt ])
  | Inline.Raw_html (lines, _) -> [ Text (String.concat "\n" (List.map Block_line.tight_to_string lines)) ]
  | _ -> []

let rec block defs ~tight (node : Block.t) : Markup.node list =
  match node with
  | Block.Blocks (blocks, _) -> List.concat_map (block defs ~tight) blocks
  | Block.Paragraph (p, _) ->
      let content = inline defs (Block.Paragraph.inline p) in
      if tight then content else [ element "p" content ]
  | Block.Heading (h, _) ->
      (* Two levels down, under the h2 of the message title; the class keeps
         the Markdown level, which the style sizes as Gotify shows it. *)
      let level = Block.Heading.level h in
      [
        element ~attributes:[ ("class", "md-h" ^ Int.to_string level) ] ("h" ^ Int.to_string (min 6 (level + 2)))
          (inline defs (Block.Heading.inline h));
      ]
  | Block.Block_quote (q, _) -> [ element "blockquote" (block defs ~tight:false (Block.Block_quote.block q)) ]
  | Block.List (l, _) ->
      let tight = Block.List'.tight l in
      let items = List.map (fun (item, _) -> element "li" (block defs ~tight (Block.List_item.block item))) (Block.List'.items l) in
      [
        (match Block.List'.type' l with
        | `Unordered _ -> element "ul" items
        | `Ordered (1, _) -> element "ol" items
        | `Ordered (start, _) -> element "ol" ~attributes:[ ("start", Int.to_string start) ] items);
      ]
  | Block.Code_block (code, _) ->
      let text = String.concat "\n" (List.map Block_line.to_string (Block.Code_block.code code)) in
      [ element "pre" [ element "code" [ Text text ] ] ]
  | Block.Html_block (lines, _) -> [ element "p" [ Text (String.concat "\n" (List.map Block_line.to_string lines)) ] ]
  | Block.Thematic_break _ -> [ element "hr" [] ]
  | Block.Ext_table (table, _) -> [ element "div" ~attributes:[ ("class", "table-scroll") ] [ table_element defs table ] ]
  | _ -> []

(* A GFM table: its header rows in thead, its data rows in tbody, each row padded to the
   column count, and each cell aligned by the separator row. *)
and table_element defs table =
  let rows = List.map (fun ((row, _), _) -> row) (Block.Table.rows table) in
  let aligns = List.find_map (function `Sep seps -> Some (List.map (fun ((align, _), _) -> align) seps) | _ -> None) rows in
  let align i = match Option.bind aligns (fun a -> List.nth_opt a i) with Some (Some a) -> Some a | _ -> None in
  let row tag cells =
    let cells = List.map (fun (cell, _) -> inline defs cell) cells in
    let cells = cells @ List.init (max 0 (Block.Table.col_count table - List.length cells)) (fun _ -> []) in
    let cell i children =
      let attributes =
        match align i with
        | Some `Left -> [ ("style", "text-align: left") ]
        | Some `Center -> [ ("style", "text-align: center") ]
        | Some `Right -> [ ("style", "text-align: right") ]
        | None -> []
      in
      element tag ~attributes children
    in
    element "tr" (List.mapi cell cells)
  in
  let head = List.filter_map (function `Header cells -> Some (row "th" cells) | _ -> None) rows in
  let body = List.filter_map (function `Data cells -> Some (row "td" cells) | _ -> None) rows in
  element "table" ((if head = [] then [] else [ element "thead" head ]) @ if body = [] then [] else [ element "tbody" body ])

let rec join = function
  | Markup.Text "" :: rest -> join rest
  | Markup.Text a :: Markup.Text b :: rest -> join (Markup.Text (a ^ b) :: rest)
  | Markup.Element e :: rest -> Markup.Element { e with children = join e.children } :: join rest
  | n :: rest -> n :: join rest
  | [] -> []

let render source =
  let doc = Doc.of_string ~strict:false source in
  join (block (Doc.defs doc) ~tight:false (Doc.block doc))

let is_markdown (extras : Yojson.Safe.t option) =
  match extras with
  | Some (`Assoc kv) -> (
      match List.assoc_opt "client::display" kv with
      | Some (`Assoc d) -> List.assoc_opt "contentType" d = Some (`String "text/markdown")
      | _ -> false)
  | _ -> false

let body ~markdown text = if markdown then render text else [ Markup.Text text ]
