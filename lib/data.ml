module Post = struct
  type meta =
    { title: string option
    ; lead: string option
    ; date: string option
    ; place: string option
    ; source: Yaml.value option (* used to preserve unknown yaml *)
    }

  type t =
    { head: meta
    ; body: string
    }

  let update ~was is =
    let head = { is.head with source = was.head.source } in
    { is with head }

  let meta_to_yaml m : Yaml.value =
    let set k v kv =
      List.remove_assoc k kv
      |> fun kv -> match v with
      | Some s -> (k, `String s) :: kv
      | None -> kv
    in
    ( match m.source with
      | Some (`O x) -> x
      | _ -> []
    )
    (* reverse order *)
    |> set "place" m.place
    |> set "date" m.date
    |> set "lead" m.lead
    |> set "title" m.title
    |> fun l -> `O l

  let meta_of_yaml m =
    match m with
    | `O l ->
      let get k =
        List.assoc_opt k l
        |> Option.map (function `String x -> Some x | _ -> None)
        |> Option.join
      in
      Some { title = get "title"
           ; lead = get "lead"
           ; date = get "date"
           ; place = get "place"
           ; source = Some m
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
    ; source = None
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
          ; source = None
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
      dummy = (to_string dummy |> of_string |> update ~was:empty)
  end)
end

open Lwt.Infix

module Git_store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)

let git_config = Irmin_git.config "./_db"

let get_posts () =
  let open Git_store in
  Repo.v git_config >>=
  master >>= fun t ->
  list t [] >>=
  Lwt_list.map_p (fun (step, tree) ->
      let%lwt content = Tree.get tree [] in
      Lwt.return (step, Post.of_string content)
    )

let get_post key =
  let open Git_store in
  Repo.v git_config >>=
  master >>= fun t ->
  get t [key] >|= Post.of_string

let info ~author msg =
  let date = Unix.gettimeofday () |> Int64.of_float in
  fun () -> Irmin.Info.v ~date ~author msg

let save_post ~author key post =
  let open Git_store in
  Repo.v git_config >>=
  master >>= fun t ->
  get_post key >>= fun was ->
  set_exn ~info:("save post " ^ key |> info ~author) t [key]
    Post.(update ~was post |> to_string)
