open Lwt.Infix
open Lwt.Syntax

module Git_store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)

let git_config = Irmin_git.config Config.repo

let absolute =
  let rev_root = List.rev Config.root in
  fun path -> List.fold_left (fun acc el -> el :: acc) path rev_root

let master () =
  let open Git_store in
  Repo.v git_config >>= master

(* Directory structure:
 * root / category / year-month / day_title /
 *   index.md
 *   img0.jpg
 *   ...
 * *)

let get_years str category =
  let open Git_store in
  list str (absolute [category]) >>=
  Lwt_list.filter_map_p (fun (year, tree) ->
      Tree.kind tree [] >|= function
      | Some `Node -> Some year
      | _ -> None
    )

let get_posts str (category, year) =
  let open Git_store in
  list str (absolute [category; year]) >>=
  Lwt_list.filter_map_p (fun (id, tree) ->
      Tree.kind tree [] >>= function
      | Some `Node -> (
          Tree.find tree ["index.md"] >|= function
          | None -> None
          | Some content ->
            Some ((category, year, id), Post.of_string content)
        )
      | _ -> Lwt.return None
    )

let get_post str (category, year, id) =
  let open Git_store in
  let* opt = find str (absolute [category; year; id; "index.md"]) in
  match opt with
  | Some x ->
    let post = Post.of_string x in
    let+ files =
      list str (absolute [category; year; id]) >>=
      Lwt_list.filter_map_s (fun (step, tree) ->
          if step <> "index.md" then
            Tree.mem tree [] >|= function
            | true -> Some step
            | false -> None
          else Lwt.return None
        )
    in
    let open Post in
    (* use path category if post does not have category in header *)
    let category = Some (Option.value ~default:category post.head.category) in
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
    Some { post with head = { post.head with gallery = gallery @ missing
                                           ; category } }
  | None -> Lwt.return None

let get_attachment str (category, year, id) filename =
  let open Git_store in
  find str (absolute [category; year; id; filename])

let info ~author msg =
  let date = Unix.gettimeofday () |> Int64.of_float in
  fun () -> Irmin.Info.v ~date ~author msg

let escape =
  let regex = Str.regexp "[^0-9a-zA-ZöÖüÜäÄß-]+" in
  let replace pat rpl str =
    Astring.String.(cuts ~sep:pat str |> concat ~sep:rpl)
  in
  fun str ->
    Str.global_replace regex "_" str
    |> replace "ö" "oe"
    |> replace "Ö" "Oe"
    |> replace "ü" "ue"
    |> replace "Ü" "Ue"
    |> replace "ä" "ae"
    |> replace "Ä" "Ae"
    |> replace "ß" "ss"

let post_key (post : Post.t) =
    let title = Option.value ~default:"" post.head.title
    and category =
      let default = Config.category_default in
      Option.value ~default post.head.category
    and year, month =
      let date = Option.value ~default:"0000-00-00" post.head.date in
      (* TODO: Properly parse this date / input validation *)
      match Astring.String.cuts ~sep:"-" date with
      | a :: b :: c :: _ -> a, b ^ "-" ^ c
      | _ -> "0", "0"
    in
    escape category, year, month ^ "_" ^ (escape title)

let compressed_jpeg data =
  let command = Lwt_process.shell
      {|convert - \
          -filter Triangle \
          -define filter:support=2 \
          -thumbnail 720 \
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

let save_post str ~author ~jpegs (ocategory, oyear, oid) post =
  let (category, year, id) as nkey = post_key post in
  let info =
    "Post speichern: " ^ year ^ "/" ^ id
    |> info ~author
  in
  let open Git_store in
  with_tree ~info str Config.root (fun t ->
      let open Tree in
      let t =
        (* posts dir might not exists *)
        Option.value ~default:empty t
      in
      let* t =
        (* move to new location *)
        let* old = get_tree t [ocategory; oyear; oid] in
        remove t [ocategory; oyear; oid] >>= fun t ->
        (* TODO: what if new_key != old key, but new_key exists? *)
        add_tree t [category; year; id] old
      in
      let* t =
        (* write/update index.md *)
        let* post =
          find t [category; year; id; "index.md"]
          >|= Option.map Post.of_string
          >|= function
          | None -> post
          | Some was -> Post.update ~was post
        in
        add t [category; year; id; "index.md"] (Post.to_string post)
      in
      let* t =
        (* remove files not mentioned in gallery *)
        let keep =
          let open Post in
          "index.md" :: List.map (fun x -> x.filename) post.head.gallery
        in
        list t [category; year; id] >>=
        Lwt_list.fold_left_s (fun acc (step, _) ->
            if List.mem step keep then
              Lwt.return acc
            else
              remove acc [category; year; id; step]
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
              add t [category; year; id; filename] data
          ) t
      in
      Lwt.return (Some t)
    )
  >|= Result.map (fun () -> nkey)
