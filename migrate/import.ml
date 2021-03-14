(* Usage: import.exe post0.md post1.md ... *)

open Lwt.Syntax
open Lwt.Infix
open Ffrlib

let file fpath =
  let* exists = Lwt_unix.file_exists fpath in
  if exists then
    Lwt_io.with_file ~flags:[O_RDONLY] ~mode:Input fpath Lwt_io.read
  else
    let msg = Printf.sprintf "cannot read non-existing file %s" fpath in
    failwith msg

let import str fpath =
  let* post = file fpath >|= Post.of_string in
  let* jpegs =
    let open Post in
    Lwt_list.map_s (fun img ->
        let* data = file ("migrate/_kats/media/" ^ img.filename) in
        Lwt.return (img.filename, data)
      ) post.head.gallery
  in
  let () =
    Printf.printf "import %s: %i images\n%!" fpath (List.length jpegs)
  in
  let author = "ffr-opium/migrate/import.exe" in
  Store.save_post str ~author ~jpegs post
  >|= ignore

let args =
  let i = ref 1
  and n = Array.length Sys.argv
  in
  let rec seq () =
    let open Seq in
    if !i < n then
      let el = Sys.argv.(!i) in
      let () = incr i in
      Cons (el, seq)
    else Nil
  in seq

let main =
  let* str = Store.master () in
  Lwt_stream.of_seq args
  |> Lwt_stream.iter_s (import str)

let () = Lwt_main.run main
