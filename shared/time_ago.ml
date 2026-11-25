(* "5 minutes ago" and "in 5 minutes", as the react-timeago of Gotify's web UI. *)

let span seconds =
  let say n unit = Printf.sprintf "%d %s%s" n unit (if n = 1 then "" else "s") in
  if seconds < 60 then say seconds "second"
  else if seconds < 3600 then say (seconds / 60) "minute"
  else if seconds < 86400 then say (seconds / 3600) "hour"
  else if seconds < 7 * 86400 then say (seconds / 86400) "day"
  else if seconds < 30 * 86400 then say (seconds / (7 * 86400)) "week"
  else if seconds < 365 * 86400 then say (seconds / (30 * 86400)) "month"
  else say (seconds / (365 * 86400)) "year"

let text ~now time =
  let seconds = Float.to_int (now -. time) in
  if seconds < 0 then "just now" else span seconds ^ " ago"

(** [None] once [time] has passed. *)
let until ~now time =
  let seconds = Float.to_int (Float.ceil (time -. now)) in
  if seconds <= 0 then None else Some ("in " ^ span seconds)
