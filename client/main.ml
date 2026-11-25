open App_shared

let now : unit -> float = [%mel.raw {|function () { return Date.now() / 1000 }|}]

let every : int -> (unit -> unit) -> unit -> unit =
  [%mel.raw {|function (ms, f) { var id = setInterval(function () { f() }, ms); return function () { clearInterval(id) } }|}]

let copy : string -> (bool -> unit) -> unit =
  [%mel.raw
    {|function (text, k) { if (!navigator.clipboard) { k(false); return } navigator.clipboard.writeText(text).then(function () { k(true) }, function () { k(false) }) }|}]

let () =
  Platform.now := now;
  Platform.every := every;
  Platform.copy := copy;
  Yuka.render ~islands:Yuka_islands.registry ()
