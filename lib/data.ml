module Post = struct
  type meta =
    { title: string option
    ; lead: string option
    ; date: string option
    ; place: string option
    }

  type t =
    { head: meta
    ; body: string
    }

  let meta_to_yaml m : Yaml.value =
    let string k = Option.map (fun x -> k, `String x) in
    [ string "title" m.title
    ; string "lead" m.lead
    ; string "date" m.date
    ; string "place" m.place
    ]
    |> List.filter_map (fun x -> x)
    |> fun l -> `O l

  let meta_of_yaml m =
    match m with
    | `O l ->
      let get f k =
        List.assoc_opt k l
        |> Option.map f
        |> Option.join
      in
      let string = get (function `String x -> Some x | _ -> None) in
      Some { title = string "title"
           ; lead = string "lead"
           ; date = string "date"
           ; place = string "place"
           }
    | _ -> None

  let to_string t =
    Format.asprintf "---\n%a---\n%s"
      Yaml.pp
      (meta_to_yaml t.head)
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
    let default = { head = empty_meta; body = x } in
    if not (is_prefix ~affix:"---" x) then default
    else
      match cut ~sep:"\n---" (drop ~max:3 x) with
      | None -> default
      | Some (head, body) ->
        match Yaml.of_string head with
        | Error _ -> default
        | Ok m -> match meta_of_yaml m with
          | None -> default
          | Some head -> { head; body = trim body }

  let%test_module _ = (module struct
    let dummy : t =
      { head =
          { title = Some "Hello World"
          ; lead = Some "This is my very first post"
          ; date = Some "2021-02-22"
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
        title: Hello World
        lead: This is my very first post
        date: 2021-02-22
        place: Innsbruck
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
