open Lwt.Infix
open Lwt.Syntax

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

module Git_store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)

let git_config = Irmin_git.config "./_db"

let get_post_years () =
  let open Git_store in
  Repo.v git_config >>=
  master >>= fun t ->
  list t ["posts"] >>=
  Lwt_list.filter_map_p (fun (year, tree) ->
      Tree.kind tree [] >|= function
      | Some `Node -> Some year
      | _ -> None
    )

let get_posts ~year =
  let open Git_store in
  Repo.v git_config >>=
  master >>= fun t ->
  list t ("posts" :: year :: []) >>=
  Lwt_list.filter_map_p (fun (id, tree) ->
      Tree.kind tree [] >>= function
      | Some `Node -> (
          Tree.find tree ["index.md"] >|= function
          | None -> None
          | Some content ->
            Some ((year, id), Post.of_string content)
        )
      | _ -> Lwt.return None
    )

let get_post (year, id) =
  let open Git_store in
  Repo.v git_config >>=
  master >>= fun t ->
  get t ["posts"; year; id; "index.md"] >|= Post.of_string

let info ~author msg =
  let date = Unix.gettimeofday () |> Int64.of_float in
  fun () -> Irmin.Info.v ~date ~author msg

let post_key =
  let regex = Str.regexp "[^0-9a-zA-ZöÖüÜäÄß-]+" in
  let replace pat rpl str =
    Astring.String.(cuts ~sep:pat str |> concat ~sep:rpl)
  in
  fun (post : Post.t) ->
    let title =
      Option.value ~default:"" post.head.title
      |> Str.global_replace regex "_"
      |> replace "ö" "o"
      |> replace "Ö" "O"
      |> replace "ü" "u"
      |> replace "Ü" "U"
      |> replace "ä" "a"
      |> replace "Ä" "A"
      |> replace "ß" "ss"
    and year, month =
      let date = Option.value ~default:"0000-00-00" post.head.date in
      (* TODO: Properly parse this date. input validation *)
      match Astring.String.cuts ~sep:"-" date with
      | a :: b :: c :: _ -> a, b ^ "-" ^ c
      | _ -> "0", "0"
    in
    year, month ^ "_" ^ title

let save_post ~author (oyear, oid) post =
  let (nyear, nid) as nkey = post_key post in
  let info =
    "Post speichern: " ^ nyear ^ "/" ^ nid
    |> info ~author
  in
  let open Git_store in
  Repo.v git_config >>=
  master >>= fun t ->
  with_tree ~info t ["posts"]
    ( let open Tree in function
          | Some t ->
            let* was = get t [oyear; oid; "index.md"] >|= Post.of_string in
            remove t [oyear; oid] >>= fun t -> (* the remove deletes all files *)
            (* TODO: what if new_key != old key, but new_key exists? *)
            add t [nyear; nid; "index.md"] Post.(update ~was post |> to_string)
            >|= Option.some
          | None ->
            add empty [nyear; nid; "index.md"] Post.(to_string post)
            >|= Option.some
    )
  >|= Result.map (fun () -> nkey)
