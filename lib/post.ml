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
      |> Option.map String.trim
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
  { category: string option
  ; title: string option
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
  |> set "category" m.category
  |> fun l -> `O l

let meta_of_yaml m =
  (* TODO: investigate: `String "1" roundtrips to `Float 1.
   * This seems to be a fundamental problem in YAML ?! *)
  match m with
  | `O l ->
    let get k =
      List.assoc_opt k l
      |> Option.map (function `String x -> Some x | _ -> None)
      |> Option.join
      |> Option.map String.trim
    and gallery =
      match List.assoc_opt "gallery" l with
      | Some (`A l) -> List.filter_map image_of_yaml l
      | _ -> []
    in
    Some { category = get "category"
         ; title = get "title"
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
  { category = None
  ; title = None
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
        { category = Some "einsaetze"
        ; title = Some "Hello World"
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
        category: einsaetze
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
