(* Relative times as the react-timeago of Gotify's web UI writes them, with
   Intl.RelativeTimeFormat in English and numeric "auto": [`Narrow] ("6m ago",
   "yesterday", "2d ago") on message cards, [`Long] ("6 minutes ago") in
   tables. As react-timeago, the unit is the largest under 60 s, 60 min, 24 h,
   7 days, 30 days, 365 days, and the count is rounded. *)

let span seconds =
  let say n unit = Printf.sprintf "%d %s%s" n unit (if n = 1 then "" else "s") in
  if seconds < 60 then say seconds "second"
  else if seconds < 3600 then say (seconds / 60) "minute"
  else if seconds < 86400 then say (seconds / 3600) "hour"
  else if seconds < 7 * 86400 then say (seconds / 86400) "day"
  else if seconds < 30 * 86400 then say (seconds / (7 * 86400)) "week"
  else if seconds < 365 * 86400 then say (seconds / (30 * 86400)) "month"
  else say (seconds / (365 * 86400)) "year"

let text ?(style = `Long) ~now time =
  let ago = now -. time in
  let seconds = Float.abs ago in
  let count by = Float.to_int (Float.round (seconds /. by)) in
  let n, unit =
    if seconds < 60. then (count 1., `Second)
    else if seconds < 3600. then (count 60., `Minute)
    else if seconds < 86400. then (count 3600., `Hour)
    else if seconds < 7. *. 86400. then (count 86400., `Day)
    else if seconds < 30. *. 86400. then (count (7. *. 86400.), `Week)
    else if seconds < 365. *. 86400. then (count (30. *. 86400.), `Month)
    else (count (365. *. 86400.), `Year)
  in
  let past = ago >= 0. in
  (* The words of numeric "auto" for 0 and 1. *)
  let named =
    match (unit, n, style) with
    | `Second, 0, _ -> Some "now"
    | `Day, 1, _ -> Some (if past then "yesterday" else "tomorrow")
    | (`Week | `Month | `Year), 1, _ ->
        let word =
          match (unit, style) with
          | `Week, `Narrow -> "wk."
          | `Month, `Narrow -> "mo."
          | `Year, `Narrow -> "yr."
          | `Week, `Long -> "week"
          | `Month, `Long -> "month"
          | _ -> "year"
        in
        Some ((if past then "last " else "next ") ^ word)
    | _ -> None
  in
  match named with
  | Some s -> s
  | None ->
      let amount =
        match style with
        | `Narrow ->
            let suffix =
              match unit with
              | `Second -> "s"
              | `Minute -> "m"
              | `Hour -> "h"
              | `Day -> "d"
              | `Week -> "w"
              | `Month -> "mo"
              | `Year -> "y"
            in
            Int.to_string n ^ suffix
        | `Long ->
            let word =
              match unit with
              | `Second -> "second"
              | `Minute -> "minute"
              | `Hour -> "hour"
              | `Day -> "day"
              | `Week -> "week"
              | `Month -> "month"
              | `Year -> "year"
            in
            Printf.sprintf "%d %s%s" n word (if n = 1 then "" else "s")
      in
      if past then amount ^ " ago" else "in " ^ amount

(** [None] once [time] has passed. *)
let until ~now time =
  let seconds = Float.to_int (Float.ceil (time -. now)) in
  if seconds <= 0 then None else Some ("in " ^ span seconds)
