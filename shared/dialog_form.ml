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
