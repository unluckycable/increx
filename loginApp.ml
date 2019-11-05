open Core_kernel
open Incr_dom

module Model = struct
  type t =
    { username : string
    ; password : string
    } [@@deriving sexp, fields, compare]
  let set_default_input username password =
    { username
    ; password }
  let init () =
    set_default_input "" ""
  let update_input_username t username =
    { t with username }
  let update_input_password t password =
    { t with password }
  let submit_input t =
    t
  let cutoff t1 t2 =
    compare t1 t2 = 0
end

module Action = struct
  type t =
    | Update_input_username of string
    | Update_input_password of string
    | Submit_input
  [@@deriving sexp]
  let should_log _ = true
end

module State = struct
  type t = unit
end

let apply_action model action _ ~schedule_action:_ =
  match (action : Action.t) with
  | Update_input_username text ->
    Model.update_input_username model text
  | Update_input_password text ->
    Model.update_input_password model text
  | Submit_input ->
    Model.submit_input model

let on_startup ~schedule_action:_ _ =
  Async_kernel.return ()

let view (m : Model.t Incr.t) ~inject =
  let open Incr.Let_syntax in
  let open Vdom in
  let b = `RGBA (Css_gen.Color.RGBA.create ~r:127 ~g:127 ~b:127 ()) in
  let f = `RGBA (Css_gen.Color.RGBA.create ~r:255 ~g:255 ~b:255 ()) in
  let in_style = Css_gen.((background_color b) @> (color f) @> (border ~width:(`Px 0) ~color:b ~style:`None ()) ) in
  let%map input_username =
    let%map input_text = m >>| Model.username in
    Node.input
      [ Attr.type_ "text"
      ; Attr.string_property "value" input_text
      ; Attr.on_input (fun _ev text -> inject (Action.Update_input_username text))
      ; Attr.style in_style
      ]
      []
  and input_password =
    let%map input_text = m >>| Model.password in
    Node.input
      [ Attr.type_ "password"
      ; Attr.string_property "value" input_text
      ; Attr.on_input (fun _ev text -> inject (Action.Update_input_password text))
      ; Attr.style in_style
      ]
      []
  in
  let button label action =
    Node.button [
      Attr.on_click (fun _ev -> inject action)
    ; Attr.style in_style ] [ Node.text label ]
  in
  let submit_button =
    button "Submit" Action.Submit_input
  in
  let label text =
    Node.text text
  in
  let li1 = Node.li [ ] [ ( label "user: " ) ; input_username ] in
  let li2 = Node.li [ ] [ ( label "pass: " ) ; input_password ] in
  let li3 = Node.li [ ] [ submit_button ] in
  let ul  = Node.ul [ ] [ li1 ; li2 ; li3 ] in
  Node.body [ Attr.style ( Css_gen.margin_left ( `Em 1 ) ) ] [ ul ]

let create model ~old_model:_ ~inject =
  let open Incr.Let_syntax in
  let%map apply_action =
    let%map model = model in
    apply_action model
  and view = view model ~inject
  and model = model in
  Component.create ~apply_action model view
