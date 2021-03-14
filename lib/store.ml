open Lwt.Infix
open Lwt.Syntax

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

let compressed_jpeg data =
  let command = Lwt_process.shell
      {|convert - \
          -filter Triangle \
          -define filter:support=2 \
          -thumbnail 1920 \
          -unsharp 0.25x0.25+8+0.065 \
          -dither None \
          -posterize 136 \
          -quality 82 \
          -define jpeg:fancy-upsampling=off \
          -interlace none \
          -colorspace sRGB \
          -strip \
          jpg:-|}
  in
  let+ data' = Lwt_process.pmap command data in
  let n = String.length data |> float_of_int
  and n' = String.length data' |> float_of_int
  in
  if n' /. n < 0.8 then data' else data

let save_post ~author ~jpegs (oyear, oid) post =
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
        Lwt_list.map_p (fun (filename, data) ->
          let+ data = compressed_jpeg data in
          (filename, data)
          ) jpegs
        >>=
        Lwt_list.fold_left_s (fun t (filename, data) ->
            if filename = "index.md" then
              Lwt.return t
            else
              add t [year; id; filename] data
          ) t
      in
      Lwt.return (Some t)
    )
  >|= Result.map (fun () -> nkey)
