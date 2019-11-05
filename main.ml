open! Core_kernel
open! Incr_dom
open! Js_of_ocaml

let () =
  Start_app.start (module LoginApp)  ~bind_to_element_with_id: "LoginApp"  ~initial_model: (LoginApp.Model.init ());
  Start_app.start (module StatusApp) ~bind_to_element_with_id: "StatusApp" ~initial_model: (StatusApp.Model.init ())
    
