(* "5 minutes ago", as the react-timeago of Gotify's web UI. *)

let text ~now time =
  let seconds = Float.to_int (now -. time) in
  let say n unit = Printf.sprintf "%d %s%s ago" n unit (if n = 1 then "" else "s") in
  if seconds < 0 then "just now"
  else if seconds < 60 then say seconds "second"
  else if seconds < 3600 then say (seconds / 60) "minute"
  else if seconds < 86400 then say (seconds / 3600) "hour"
  else if seconds < 7 * 86400 then say (seconds / 86400) "day"
  else if seconds < 30 * 86400 then say (seconds / (7 * 86400)) "week"
  else if seconds < 365 * 86400 then say (seconds / (30 * 86400)) "month"
  else say (seconds / (365 * 86400)) "year"
