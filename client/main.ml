open App_shared

let now : unit -> float = [%mel.raw {|function () { return Date.now() / 1000 }|}]

let every : int -> (unit -> unit) -> unit -> unit =
  [%mel.raw {|function (ms, f) { var id = setInterval(function () { f() }, ms); return function () { clearInterval(id) } }|}]

let after : int -> (unit -> unit) -> unit -> unit =
  [%mel.raw {|function (ms, f) { var id = setTimeout(function () { f() }, ms); return function () { clearTimeout(id) } }|}]

let overflows : string -> bool =
  [%mel.raw {|function (id) { var e = document.getElementById(id); return !!e && e.scrollHeight > e.clientHeight + 1 }|}]

(* Image loads do not bubble, so this listens in the capture phase. *)
let on_image_load : (unit -> unit) -> unit -> unit =
  [%mel.raw
    {|function (f) { var h = function (e) { if (e.target && e.target.tagName === "IMG") f() }; document.addEventListener("load", h, true); return function () { document.removeEventListener("load", h, true) } }|}]

let set_theme : string -> unit =
  [%mel.raw
    {|function (t) { document.querySelectorAll(".app").forEach(function (e) { e.className = e.className.replace(/\btheme-\w+/, "theme-" + t) }) }|}]

let scroll_y : unit -> float = [%mel.raw {|function () { return window.scrollY }|}]
let scroll_to_top : unit -> unit = [%mel.raw {|function () { window.scrollTo({ top: 0, behavior: "smooth" }) }|}]

let start_drag : Yuka.Event.t -> unit =
  [%mel.raw {|function (e) { if (e.dataTransfer) { e.dataTransfer.setData("text/plain", ""); e.dataTransfer.effectAllowed = "move" } }|}]

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

(* Yuka runs every client navigation in a view transition (~transitions:true). This skips the
   animation, though not the update, when a dialog opens or closes (an overlay route: .overlay
   is in the page before or after) and when the path stays the same: a refresh (theme change,
   reordered apps) or a change of query only (infinite scroll's ?pages=N). Yuka has changed the
   URL before it starts the transition, so [shown] keeps the path of the page on screen: set
   after an update that leaves no dialog, and left as it is when a dialog closes (Yuka hides it
   without a transition) or opens over the page. *)
let skip_some_transitions : unit -> unit =
  [%mel.raw
    {|function () {
  if (typeof document.startViewTransition !== "function") return;
  var start = document.startViewTransition.bind(document);
  var shown = location.pathname;
  var overlay = function () { return !!document.querySelector(".overlay") };
  document.startViewTransition = function (update) {
    var from = shown, before = overlay();
    var t = start(update);
    t.updateCallbackDone.then(function () {
      var after = overlay(), path = location.pathname;
      if (!after) shown = path;
      if (before || after || from === path) t.skipTransition();
    }, function () {});
    return t;
  };
}|}]

let () =
  skip_some_transitions ();
  Platform.permission := permission;
  Platform.request_permission := request_permission;
  (Platform.notify := fun ~title ~body ~icon -> notify title body icon);
  Platform.now := now;
  Platform.every := every;
  Platform.after := after;
  Platform.overflows := overflows;
  Platform.on_image_load := on_image_load;
  Platform.set_theme := set_theme;
  Platform.scroll_y := scroll_y;
  Platform.scroll_to_top := scroll_to_top;
  Platform.start_drag := start_drag;
  Platform.copy := copy;
  Yuka.render ~islands:Yuka_islands.registry ~transitions:true ()
