open Lwt.Infix
open Lwt.Syntax
open Ffrlib

let main =
  let* str = Store.master () in
  let user =
    User.t
      ~email:"patrik@pkel.dev"
      ~name:"Patrik Keller"
      "FZMV9Kgha69fN3sAbiK2"
  and author = "ffr-opium/set_user"
  in
  Store.set_user ~author str "pkel" user

let () =
  Lwt_main.run (main >|= ignore)
