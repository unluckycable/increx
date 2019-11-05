open Core_kernel
open Incr_dom

module Status = struct
  type t = Online | Offline      
  [@@deriving sexp, compare]

  let to_string = function
    | Online -> "Online"
    | Offline -> "Offline"

end

module Model = struct
  type t =
    { status     : Status.t
    ; lastActive : string
    } [@@deriving sexp, fields, compare]

  let init () =
    { status = Offline
    ; lastActive = "" }

  let setOnline t =
    { t with status = Online }

  let setOffline t =
    { t with status = Offline }

  let setLastActive t msg =
    { t with lastActive = msg }

  let cutoff t1 t2 =
    compare t1 t2 = 0
end

module Action = struct
  type t =
    | SetOnlineAction
    | SetOfflineAction
    | SetLastActiveAction of string
  [@@deriving sexp]
  let should_log = true
end

module State = struct
  type t = unit
end

let apply_action model action _ ~schedule_action:_ =
  match ( action: Action.t ) with
  | SetOnlineAction ->
    Model.setOnline model
  | SetOfflineAction ->
    Model.setOffline model
  | SetLastActiveAction text ->
    Model.setLastActive model text

let on_startup ~schedule_action:_ _ =
  Async_kernel.return ()

let view ( m : Model.t Incr.t ) ~inject =
  let open Incr.Let_syntax in
  let open Vdom in
  let%map status =
    let%map current_status = m >>| Model.status in
    Node.li [
      Attr.on_click (
        fun _ev -> inject Action.SetOnlineAction
      ) ]
      [ ( Node.text (Status.to_string current_status) ) ]

  and lastactive =
    let%map current_msg = m >>| Model.lastActive in
    Node.li [ ] [ ( Node.text current_msg ) ]
  in
  
  let ul  = Node.ul [ ] [ status ; lastactive ] in
  Node.body [ ] [ ul ]

let create model ~old_model:_ ~inject =
  let open Incr.Let_syntax in
  let%map apply_action =
    let%map model = model in
    apply_action model
  and view = view model ~inject
  and model = model in
  Component.create ~apply_action model view
