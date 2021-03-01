open Lwt.Infix
open Lwt.Syntax

module Post = struct
  type image =
    { filename: string
    ; caption: string option
    ; source: string option
    }

  let image ?caption ?source filename = { caption; source; filename }

  let image_to_yaml x : Yaml.value =
    let set k v kv =
      List.remove_assoc k kv
      |> fun kv -> match v with
      | Some s -> (k, `String s) :: kv
      | None -> kv
    in
    []
    (* reverse order *)
    |> set "source" x.source
    |> set "caption" x.caption
    |> set "filename" (Some x.filename)
    |> fun l -> `O l

  let image_of_yaml x =
    match x with
    | `O l ->
      let get k =
        List.assoc_opt k l
        |> Option.map (function `String x -> Some x | _ -> None)
        |> Option.join
      in
      ( match get "filename" with
        | None -> None
        | Some filename ->
          Some { caption = get "caption"
               ; source = get "source"
               ; filename
               }
      )
    | _ -> None

  type meta =
    { title: string option
    ; lead: string option
    ; date: string option
    ; place: string option
    ; gallery: image list
    ; foreign: Yaml.value option (* used to preserve unknown yaml *)
    }

  let meta_to_yaml m : Yaml.value =
    let set k v kv =
      List.remove_assoc k kv
      |> fun kv -> match v with
      | Some s -> (k, `String s) :: kv
      | None -> kv
    in
    ( match m.foreign with
      | Some (`O x) -> x
      | _ -> []
    )
    (* reverse order *)
    |> List.remove_assoc "gallery"
    |> (fun kv ->
        match m.gallery with
        | [] -> kv
        | l -> ("gallery", `A (List.map image_to_yaml l)) :: kv
      )
    |> set "place" m.place
    |> set "date" m.date
    |> set "lead" m.lead
    |> set "title" m.title
    |> fun l -> `O l

  let meta_of_yaml m =
    (* TODO: investigate: `String "1" roundtrips to `Float 1. *)
    match m with
    | `O l ->
      let get k =
        List.assoc_opt k l
        |> Option.map (function `String x -> Some x | _ -> None)
        |> Option.join
      and gallery =
        match List.assoc_opt "gallery" l with
        | Some (`A l) -> List.filter_map image_of_yaml l
        | _ -> []
      in
      Some { title = get "title"
           ; lead = get "lead"
           ; date = get "date"
           ; place = get "place"
           ; gallery
           ; foreign = Some m
           }
    | _ -> None

  type t =
    { head: meta
    ; body: string
    }

  let update ~was is =
    let head = { is.head with foreign = was.head.foreign } in
    { is with head }

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
    ; gallery = []
    ; foreign = None
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
          ; gallery =
              [ image ~caption:"Bild 1" ~source:"CC0" "image1.jpg"
              ; image ~caption:"Bild 2" ~source:"CC-BY" "image2.jpg"
              ]
          ; foreign = None
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
        gallery:
        - filename: image1.jpg
          caption: Bild 1
          source: CC0
        - filename: image2.jpg
          caption: Bild 2
          source: CC-BY
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
  let* t = Repo.v git_config >>= master in
  (* TODO: get -> find ; return post option *)
  let* post = get t ["posts"; year; id; "index.md"] >|= Post.of_string in
  let+ files =
    list t ["posts"; year; id] >>=
    Lwt_list.filter_map_s (fun (step, tree) ->
        if step <> "index.md" then
          Tree.mem tree [] >|= function
          | true -> Some step
          | false -> None
        else Lwt.return None
      )
  in
  let open Post in
  (* gallery 1:1 files *)
  let gallery =
    List.filter
      (fun x -> List.mem x.filename files)
      post.head.gallery
  in
  let missing =
    List.filter
      (fun f -> List.exists (fun x -> x.filename = f) gallery |> not)
      files
    |> List.map image
  in
  { post with head = { post.head with gallery = gallery @ missing } }

let get_attachment (year, id) filename =
  let open Git_store in
  let* t = Repo.v git_config >>= master in
  find t ["posts"; year; id; filename]

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

let save_post ~author ~files (oyear, oid) post =
  (* TODO: compress images on first save *)
  let (year, id) as nkey = post_key post in
  let info =
    "Post speichern: " ^ year ^ "/" ^ id
    |> info ~author
  in
  let open Git_store in
  Repo.v git_config >>=
  master >>= fun t ->
  with_tree ~info t ["posts"] (fun t ->
      let open Tree in
      let t =
        (* posts dir might not exists *)
        Option.value ~default:empty t
      in
      let* t =
        (* move to new location *)
        let* old = get_tree t [oyear; oid] in
        remove t [oyear; oid] >>= fun t ->
        (* TODO: what if new_key != old key, but new_key exists? *)
        add_tree t [year; id] old
      in
      let* t =
        (* write/update index.md *)
        let* post =
          find t [year; id; "index.md"]
          >|= Option.map Post.of_string
          >|= function
          | None -> post
          | Some was -> Post.update ~was post
        in
        add t [year; id; "index.md"] (Post.to_string post)
      in
      let* t =
        (* remove files not mentioned in gallery *)
        let keep =
          let open Post in
          "index.md" :: List.map (fun x -> x.filename) post.head.gallery
        in
        list t [year; id] >>=
        Lwt_list.fold_left_s (fun acc (step, _) ->
            if List.mem step keep then
              Lwt.return acc
            else
              remove acc [year; id; step]
          ) t
      in
      let* t =
        (* add new files *)
        Lwt_list.fold_left_s (fun t (filename, data) ->
            if filename = "index.md" then
              Lwt.return t
            else
              add t [year; id; filename] data
          ) t files
      in
      Lwt.return (Some t)
    )
  >|= Result.map (fun () -> nkey)
