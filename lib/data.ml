let string_of_date = ISO8601.Permissive.string_of_date
let date_of_string = ISO8601.Permissive.date

let date_of_string_opt s =
  try Some (date_of_string s) with _ -> None

module Post = struct
  type meta =
    { title: string option
    ; lead: string option
    ; date: float option
    ; place: string option
    }

  type t =
    { head: meta
    ; body: string
    }

  let meta_to_toml m =
    let open Toml in
    let open Types in
    let key = Table.Key.bare_key_of_string in
    let string k = Option.map (fun x -> key k, TString x)
    and date k = Option.map (fun x -> key k, TDate x)
    in
    [ string "title" m.title
    ; string "lead" m.lead
    ; string "place" m.place
    ; date "date" m.date
    ]
    |> List.filter_map (fun x -> x)
    |> Table.of_key_values

  let meta_of_toml m =
    let open Toml in
    let open Types in
    let key = Table.Key.bare_key_of_string in
    let get f k =
      Table.find_opt (key k) m
      |> Option.map f
      |> Option.join
    in
    let string = get (function TString x -> Some x | _ -> None)
    and date = get (function TDate x -> Some x | _ -> None)
    in
    { title = string "title"
    ; lead = string "lead"
    ; date = date "date"
    ; place = string "place"
    }

  let to_string t =
    Format.asprintf "---\n%a---\n%s"
      Toml.Printer.table
      (meta_to_toml t.head)
      t.body

  let empty_meta : meta =
    { title = None
    ; lead = None
    ; date = None
    ; place = None
    }

  let empty : t = { head = empty_meta; body= "" }

  let of_string x =
    let open Astring.String in
    if not (is_prefix ~affix:"---" x) then
      { head = empty_meta; body = x }
    else
      match cut ~sep:"\n---" (drop ~max:3 x) with
      | None -> { head = empty_meta; body = x }
      | Some (head, body) ->
        match Toml.Parser.from_string head with
        | `Ok m -> { head = meta_of_toml m; body = trim body }
        | `Error _ -> { head = empty_meta; body = x }

  let%test_module _ = (module struct
    let dummy : t =
      { head =
          { title = Some "Hello World"
          ; lead = Some "This is my very first post"
          ; date = Some (date_of_string "2021-02-22T20:59:26+00:00")
          ; place = Some "Innsbruck"
          }
      ; body = {|I'm too lazy to produce a long text here.
Good news is, I don't have to.

But did you see OCaml's multi-line string feature?

The End.|} }

    let%expect_test "post_to_string" =
      print_endline (to_string dummy);
      [%expect {|
        ---
        date = 2021-02-22T00:00:00+00:00
        lead = "This is my very first post"
        place = "Innsbruck"
        title = "Hello World"
        ---
        I'm too lazy to produce a long text here.
        Good news is, I don't have to.

        But did you see OCaml's multi-line string feature?

        The End. |}]

    let%test "post_roundtrip" =
      dummy = (to_string dummy |> of_string)
  end)
end

open Lwt.Infix

module Git_store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)

let git_config = Irmin_git.config "./_db"

let posts () =
  let open Git_store in
  Repo.v git_config >>=
  master >>= fun t ->
  list t [] >>=
  Lwt_list.map_p (fun (step, tree) ->
      let%lwt content = Tree.get tree [] in
      Lwt.return (step, Post.of_string content)
    )

let post key =
  let open Git_store in
  Repo.v git_config >>=
  master >>= fun t ->
  get t [key] >|= Post.of_string

let info msg =
  let date = Unix.gettimeofday () |> Int64.of_float in
  let author = "brainstorm@app" in
  fun () -> Irmin.Info.v ~date ~author msg

let save_post key post =
  let open Git_store in
  Repo.v git_config >>=
  master >>= fun t ->
  set_exn ~info:("save post " ^ key |> info) t [key] (Post.to_string post)
