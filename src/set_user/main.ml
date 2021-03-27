open Lwt.Syntax
open Ffrlib
open Cmdliner

let arg_handle =
  let doc = "The user's login name."
  in Arg.(required & pos 0 (some string) None & info [] ~docv:"HANDLE" ~doc)

let arg_email =
  let doc = "The user's email address. Will be used in commit messages."
  in Arg.(required & pos 1 (some string) None & info [] ~docv:"EMAIL" ~doc)

let arg_name =
  let doc = "The user's full name. Will be used in commit messages."
  in Arg.(required & pos 2 (some string) None & info [] ~docv:"NAME" ~doc)

let set_user_lwt ~handle ~email ~name ~password =
  let* str = Store.master () in
  let user =
    User.t ~email ~name ~password
  and author = "ffr-opium/set_user"
  in
  Store.set_user ~author str handle user

let term_set_user =
  let main handle email name =
    let lwt =
      let* password = Lwt_io.(read_line stdin) in
      assert (String.length password > 10);
      let* _ = set_user_lwt ~handle ~email ~name ~password in
      Lwt_io.printf
        "User:\t\t%s\nEmail:\t\t%s\nName:\t\t%s\nPassword:\t%s\n"
        handle email name password
    in
    Lwt_main.run lwt
  in
  Term.(const main $ arg_handle $ arg_email $ arg_name)

let info_set_user =
  let doc = "Set ffr-opium user" in
  let man =
    [ `P "Adds given user to the configured ffr-opium repository. Overwrites existing users with the same handle."
    ; `P "Reads user password from STDIN."
    ; `S "USE FRESH PASSWORDS"
    ; `P "We store an Argon2 hash of your password in a (public) git repository. We do not take security seriously. If you provide a shared password here, all your other accounts are at risk."
    ]
  in
  Term.info "set-user" ~doc ~man

let () = Term.exit @@ Term.eval (term_set_user, info_set_user)
