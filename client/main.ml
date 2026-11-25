open App_shared

let now : unit -> float = [%mel.raw {|function () { return Date.now() / 1000 }|}]

let every : int -> (unit -> unit) -> unit -> unit =
  [%mel.raw {|function (ms, f) { var id = setInterval(function () { f() }, ms); return function () { clearInterval(id) } }|}]

let after : int -> (unit -> unit) -> unit -> unit =
  [%mel.raw {|function (ms, f) { var id = setTimeout(function () { f() }, ms); return function () { clearTimeout(id) } }|}]

let overflows : string -> bool =
  [%mel.raw {|function (id) { var e = document.getElementById(id); return !!e && e.scrollHeight > e.clientHeight + 1 }|}]

let copy : string -> (bool -> unit) -> unit =
  [%mel.raw
    {|function (text, k) { if (!navigator.clipboard) { k(false); return } navigator.clipboard.writeText(text).then(function () { k(true) }, function () { k(false) }) }|}]

let permission : unit -> string =
  [%mel.raw {|function () { return typeof Notification === "undefined" ? "unsupported" : Notification.permission }|}]

let request_permission : (string -> unit) -> unit =
  [%mel.raw
    {|function (k) { if (typeof Notification === "undefined") { k("unsupported"); return } Notification.requestPermission().then(function (p) { k(p) }, function () { k(Notification.permission) }) }|}]

let notify : string -> string -> string -> unit =
  [%mel.raw
    {|function (title, body, icon) { if (typeof Notification === "undefined" || Notification.permission !== "granted") return; var n = new Notification(title, { body: body, icon: icon }); n.onclick = function () { window.focus(); n.close() } }|}]

let () =
  Platform.permission := permission;
  Platform.request_permission := request_permission;
  (Platform.notify := fun ~title ~body ~icon -> notify title body icon);
  Platform.now := now;
  Platform.every := every;
  Platform.after := after;
  Platform.overflows := overflows;
  Platform.copy := copy;
  Yuka.render ~islands:Yuka_islands.registry ()
