(* The parts of the dialog forms of Confirm_form and User_edit_form. *)

(* The password of a session that is not elevated ([user]: its user name), with
   the error of a wrong one under it. *)
let password form ~user =
  match user with
  | None -> Yuka.empty
  | Some name ->
      Yuka.element "label" [ Yuka.attr "class" "field" ]
        [
          Yuka.text ("Password of " ^ name ^ ", for a recent sign-in");
          Yuka.element "input"
            [ Yuka.attr "type" "password"; Yuka.attr "name" "elevate_pass"; Yuka.attr "required" "";
              Yuka.attr "autocomplete" "current-password" ]
            [];
          Yuka.Form.field_error_view form `Elevate_pass (fun ~id message ->
              Yuka.element "span" [ Yuka.attr "id" id; Yuka.attr "class" "field-error" ] [ Yuka.text message ]);
        ]

let errors form = Yuka.Form.form_errors_view form (fun message -> Yuka.element "p" [ Yuka.attr "class" "alert" ] [ Yuka.text message ])

(** The value sent after a failed post (422), else [default]. *)
let sent form field default = match Yuka.Form.value form field with "" -> default | v -> v

(** The error of a field, under it. *)
let error form field =
  Yuka.Form.field_error_view form field (fun ~id message ->
      Yuka.element "span" [ Yuka.attr "id" id; Yuka.attr "class" "field-error" ] [ Yuka.text message ])

(** The submit button, disabled while the form waits. *)
let submit ?(class_ = "button contained") form label =
  Yuka.hole (fun () ->
      Yuka.element "button"
        ([ Yuka.attr "type" "submit"; Yuka.attr "class" class_ ]
        @ if Yuka.State.get (Yuka.Form.pending form) then [ Yuka.attr "disabled" "" ] else [])
        [ Yuka.text label ])

(* The [on_ok] of a form that changes the apps: the drawer and the
   Applications table keep their apps query through the redirect's client
   navigation, so it asks them again, then follows the redirect. *)
let apps_changed redirect =
  Yuka.Remote.invalidate Actions.apps;
  Yuka.Browser.navigate redirect
