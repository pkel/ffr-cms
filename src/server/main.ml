open Ffrlib

let trim_opt s =
  match String.trim s with
  | "" -> None
  | s -> Some s

module Location = struct
  let root = "/"
  let category c = root ^ c ^ "/"
  let year (c, y) = category c ^ y ^ "/"
  let post (c, y, id) = year (c, y) ^ id ^ "/"
  let attachment key file = post key ^ file
  let create_post = root ^ "create"
  let delete_post (c, y, id) = root ^ "delete/" ^ (String.concat "/" [c;y;id])
  let preview_post x = "/vorschau" ^ post x
end

module Whitelist : sig
  val whitelist : string -> string
  val whitelisted : string -> bool
end = struct
  module S = Set.Make(String)

  let set = ref S.empty

  let whitelist uri = set := S.add uri !set ; uri

  let whitelisted uri = S.mem uri !set
end

open Whitelist

let _ = whitelist "/favicon.ico"

module View = struct
  open Tyxml.Html

  module BS = struct
    let select ~id ~name ~lbl options value =
      let id = id name in
      let a = [ a_id id
              ; a_name name
              ; a_class ["form-control"]
              ]
      in
      div ~a:[a_class ["form-group"]]
        [ label ~a:[a_label_for id] [txt lbl]
        ; select ~a
            ( List.map (fun (k, v) ->
                  let a =
                    if Some k = value
                    then [a_selected (); a_value k]
                    else [a_value k]
                  in
                  option ~a (txt v)
                ) options
            )
        ]

    let input ~id ~name ~lbl typ value =
      let id = id name in
      let a = [ a_id id
              ; a_name name
              ; a_input_type typ
              ; a_class ["form-control"]
              ]
      in
      let a = match value with
        | None -> a
        | Some x -> a_value x :: a
      in
      match typ with
      | `Hidden -> input ~a ()
      | _ ->
        div ~a:[a_class ["form-group"]]
          [ label ~a:[a_label_for id] [txt lbl]
          ; input ~a ()
          ]

    let textarea ~id ~name ~lbl ~rows content =
      let id = id name
      and content = Option.value ~default:"" content in
      div ~a:[a_class ["form-group"]]
        [ label ~a:[a_label_for id ] [txt lbl]
        ; textarea ~a:[ a_id id
                      ; a_rows rows
                      ; a_name name
                      ; a_class ["form-control"]
                      ]
            (txt content)
        ]
  end

  let page content =
    (html
       (head (title (txt Config.t.title))
          [ meta ~a:[ a_charset "UTF-8" ] ()
          ; meta ~a:[ a_name "viewport"
                    ; a_content "width=device-width, initial-scale=1.0"
                    ] ()
          ; link ~href:(whitelist "/assets/bootstrap.min.css") ~rel:[`Stylesheet] ()
          ; link ~href:(whitelist "/assets/app.css") ~rel:[`Stylesheet] ()
          ])
       (body ([ div ~a:[a_class ["container"; "mb-3"; "mt-3"]] content
              ; script ~a:[ a_src (whitelist "/assets/jquery-3.5.1.slim.min.js")] (txt "")
              ; script ~a:[ a_src (whitelist "/assets/bootstrap.bundle.min.js")] (txt "")
              ])))

  let posts ~categories ~category ~years ~year posts =
    let create =
      a ~a:[ a_href Location.create_post
           ; a_class ["btn"; "btn-primary"; "float-right"]]
        [txt "Neuer Eintrag"]
    and years =
      let cls = ["badge"; "badge-pill"] in
      List.sort compare years
      |> List.rev_map (fun y ->
          let cls = if y = year then "badge-secondary" :: cls else "badge-light" :: cls in
          li ~a:[a_class ["list-inline-item"; "h4"]]
            [a ~a:[ a_class cls; a_href (Location.year (category, y)) ] [ txt y ]]
        )
      |> ul ~a:[a_class ["list-inline"]]
    and categories =
      let cls = ["badge"; "badge-pill"] in
      List.map (fun Config.{id; label} ->
          let cls = if id = category then "badge-secondary" :: cls else "badge-light" :: cls in
          li ~a:[a_class ["list-inline-item"; "h4"]]
            [a ~a:[ a_class cls; a_href (Location.year (id, year)) ] [ txt label ]]
        ) categories
      |> ul ~a:[a_class ["list-inline"]]
    in
    page [ create
         ; categories
         ; years
         ; div ~a:[a_class ["list-group"]]
             ( List.rev_map (fun (key, post) ->
                   let open Post in
                   let loc = Location.post key in
                   let date_place =
                     Printf.sprintf "%s – %s"
                       (Option.value ~default:"" post.head.date) (* TODO pp date *)
                       (Option.value ~default:"" post.head.place)
                   and title =
                     Option.value ~default:"Kein Titel" post.head.title
                   in
                   let entry =
                     [ Some (p ~a:[a_class ["h5"]] [ txt title ])
                     ; Option.map (fun l ->
                           p ~a:[a_class ["h6"]] [txt l]
                         ) post.head.lead
                     ; Some (txt date_place)
                     ]
                     |> List.filter_map (fun x -> x)
                   in
                   a ~a:[ a_href loc
                        ; a_class [ "list-group-item"
                                  ; "list-group-item-action"
                                  ]
                        ] entry ) posts)
         ]

  let hex_hash (type a) (x: a) : string =
    Hashtbl.hash x |> Printf.sprintf "%x"

  let modal ~id ~title ~actions ~body =
    div ~a:[ a_class ["modal"]
           ; a_id id
           ; a_tabindex (-1)
           ; a_aria "hidden" ["true"]
           ]
      [ div ~a:[a_class ["modal-dialog"; "modal-dialog-centered"]]
          [ div ~a:[a_class ["modal-content"]]
              [ div ~a:[a_class ["modal-header"]]
                  [ h5 ~a:[a_class ["modal-title"]] [txt title ]
                  ; button ~a:[ a_button_type `Button
                              ; a_class ["close"]
                              ; a_user_data "dismiss" "modal"
                              ; a_aria "label" ["Close"]
                              ]
                      [ span ~a:[a_aria "hidden" ["true"]] [txt "⨉"] ]
                  ]
              ; div ~a:[a_class ["modal-body"]] body
              ; div ~a:[a_class ["modal-footer"]] actions
              ]
          ]
      ]

  let post ?(link_preview=true) key post =
    let open Post in
    let id =
      let prefix = hex_hash key in
      fun x -> "post-" ^ prefix ^ "-" ^ x
    in
    let input' name lbl typ v = BS.input ~id ~name ~lbl typ v in
    page [ a ~a:[ a_class ["float-right"; "btn"; "btn-secondary"]
                ; a_href ".."
                ]
             [ txt "Zurück zur Übersicht" ]
         ; h1 ~a:[a_class ["h3"]] [txt "Bearbeiten"]
         ; hr ()
         ; form ~a:[ a_method `Post
                   ; a_enctype "multipart/form-data"
                   ]
             [ BS.select ~id ~name:"draft" ~lbl:"Status"
                 ["false", "öffentlich"; "true", "nicht öffentlicher Entwurf (nur Vorschau)"]
                 (match post.head.draft with Some () -> Some "true" | None -> Some "false")
             ; BS.select ~id ~name:"category" ~lbl:"Rubrik"
                 Config.(category_assoc t)
                 post.head.category
             ; input' "title" "Titel"      `Text post.head.title
             ; input' "lead"  "Untertitel" `Text post.head.lead
             ; input' "place" "Ort"        `Text post.head.place
             ; input' "date"  "Datum"      `Date post.head.date
             ; BS.textarea ~id ~name:"body" ~lbl:"Beitrag" ~rows:20
                 ( Some post.body )
             ; hr ()
             ; List.mapi ( fun i x ->
                   let i = Int.to_string i in
                   let open Post in
                   let id_prefix = hex_hash x.filename in
                   let name' x = ("img" ^ "-" ^ id_prefix ^ "-" ^ x) in
                   let input' name lbl typ v =
                     BS.input ~id ~name:(name' name) ~lbl typ v
                   and action ?(c="btn-light") name lbl =
                     li ~a:[a_class ["list-inline-item"]]
                       [ button ~a:[ a_formaction ("?" ^ name ^ "=" ^ i)
                                   ; a_class ["btn"; c]
                                   ] [txt lbl]
                       ]
                   in
                   div ~a:[a_class ["row"]]
                     [ figure
                         ~a:[a_class ["col-md-6"; "figure"; "post-image"]]
                         ~figcaption:(
                           `Bottom (figcaption ~a:[a_class ["figure-caption"]]
                                      [txt x.filename]))
                         [ img
                             ~a:[ a_style "width: 100%"
                                ; a_title x.filename
                                ]
                             ~src:(Location.attachment key x.filename)
                             ~alt:(Option.value ~default:"" x.caption)
                             ()
                         ]
                     ; div ~a:[a_class ["col-md-6"]]
                         [ BS.textarea ~id ~name:(name' "caption") ~rows:3
                             ~lbl:"Bildunterschrift" x.caption
                         ; input' "source" "Quelle" `Text x.source
                         ; input' "filename" "" `Hidden (Some x.filename)
                         ; input' "position" "" `Hidden (Some i)
                         ; ul ~a:[a_class ["list-inline"; "float-left"]]
                             [ action ~c:"btn-danger" "delete" "✖" ]
                         ; ul ~a:[a_class ["list-inline"; "float-right"]]
                             [ action "up" "🢁"
                             ; action "down" "🡻"
                             ]
                         ]
                     ]
                 ) post.head.gallery
               |> List.concat_map (fun fig -> [fig; hr ()])
               |> div
             ; (let id = id "upload" in
                div ~a:[a_class ["form-group"]]
                  [ label ~a:[ a_label_for id ] [txt "Bilder hinzufügen"]
                  ; input ~a:[ a_input_type `File
                             ; a_name "upload"
                             ; a_id id
                             ; a_multiple ()
                             ; a_accept ["image/jpeg"]
                             ; a_class ["form-control-file"]
                             ] ()
                  ])
             ; hr ()
             ; div ~a:[a_class ["clearfix"]]
                 [ button ~a:[ a_button_type `Button
                             ; a_class ["btn"; "btn-danger"; "float-left"]
                             ; a_user_data "toggle" "modal"
                             ; a_user_data "target" "#modal-delete-post"
                             ; a_formaction (Location.delete_post key)
                               (* nojs fallback *)
                             ]
                     [ txt "Eintrag löschen" ]
                 ; modal
                     ~id:"modal-delete-post"
                     ~title:"Eintrag löschen"
                     ~body:
                       [ p [txt "Wollen Sie diesen Eintrag wirklich löschen?"] ]
                     ~actions:
                       [ button ~a:[ a_button_type `Button
                                   ; a_class ["btn"; "btn-primary"; "float-right"]
                                   ; a_user_data "dismiss" "modal"
                                   ]
                           [ txt "Abbrechen" ]
                       ; button ~a:[ a_button_type `Submit
                                   ; a_formaction (Location.delete_post key)
                                   ; a_class ["btn"; "btn-primary"; "float-left"]
                                   ]
                           [ txt "Löschen" ]
                       ]
                 ; button ~a:[ a_button_type `Submit
                             ; a_class ["btn"; "btn-primary"; "float-right"]
                             ]
                     [ txt "Speichern" ]
                 ; if link_preview then
                     a ~a:[ a_class ["btn"; "btn-secondary"; "float-right"; "mr-2"]
                          ; a_href (Location.preview_post key)
                          ; a_title "Nach dem Speichern kann es mehrere Sekunden \
                                     dauern, bis die Vorschau verfügbar beziehungs\
                                     weise aktualisiert wurde. Gegebenenfalls \
                                     muss die Vorschau-Seite neu geladen werden."
                          ]
                       [ txt "Vorschau" ]
                   else
                     div []
                 ]
             ]
         ]

  let login ?message () =
    let id = fun x -> "login-" ^ x in
    let input' name lbl typ v = BS.input ~id ~name ~lbl typ v in
    [ Some (h1 [txt "Anmeldung"])
    ; Option.map (fun m ->
          div ~a:[a_class ["alert"; "alert-danger"]] [ txt m ]
        ) message
    ; Some (form ~a:[ a_method `Post; a_class ["clearfix"] ]
              [ input' "user" "Nutzername" `Text None
              ; input' "password" "Passwort" `Password None
              ; button ~a:[ a_button_type `Submit
                          ; a_class ["btn"; "btn-primary"; "float-right"]
                          ]
                  [ txt "Los!" ]
              ])
    ]
    |> List.filter_map (fun x -> x)
    |> page

end

open Lwt.Infix
open Lwt.Syntax
open Opium

module Auth = struct

  let sessions = Hashtbl.create 7

  let auth_cookie_name = "session"

  let user_key =
    Rock.Context.Key.create ("auth_user", User.sexp_of_t)

  let user req =
    let open Request in
    Context.find user_key req.env

  let post_login req =
    (* handle post from login form *)
    let+ handle = Request.urlencoded "user" req >|= Option.get
    and+ password = Request.urlencoded "password" req >|= Option.get
    and+ users = Store.master () >>= Store.get_users
    in try
      let registered, user =
        (* TODO: Do constant time lookup here ?! *)
        match List.assoc_opt handle users with
        | Some x -> true, x
        | None -> false, User.nobody
      in
      assert (User.valid_password user password);
      (* Intentionally check registered after hashing.
       * Otherwise, we'd provide a username oracle. *)
      assert registered;
      let id =
        Nocrypto.Rng.generate 32
        |> Nocrypto.Base64.encode
        |> Cstruct.to_string
      in
      Hashtbl.replace sessions id user; (* store session *)
      Response.redirect_to req.target (* redirect get *)
      |> Response.add_cookie
        ~http_only:true
        ~same_site:`Strict
        ~expires:`Session
        ~scope:(Uri.of_string "/")
        (auth_cookie_name, id)
    with _ ->
      View.login ~message:"Login fehlgeschlagen." ()
      |> Response.of_html

  let get_login () = View.login () |> Response.of_html |> Lwt.return

  let middleware =
    let auth handler req =
      match Request.cookie auth_cookie_name req with
      | Some cookie when Hashtbl.mem sessions cookie ->
        (* Authenticated (memory) *)
        let user = Hashtbl.find sessions cookie in
        let env = Context.add user_key user req.env in
        handler { req with env }
      | _ ->
        (* Not authenticated *)
        match req.meth with
        | `POST ->
          (* handle post from login form *)
          post_login req
        | `GET ->
          (* show login form *)
          get_login ()
        | _ ->
          (* redirect get *)
          Response.redirect_to req.target |> Lwt.return
    in
    let whitelist_or_auth handler (req: Request.t) =
      if whitelisted req.target
      then handler req
      else auth handler req
    in
    Rock.Middleware.create ~filter:whitelist_or_auth ~name:"Authentication"
end

let app_name = "ffr-opium"

let author req =
  let user = Auth.user req |> Option.get in
  Printf.sprintf "%s via %s <%s>" user.name app_name user.email

let crlf_regex = Str.regexp "\r\n"

let save_post ?key req =
  let files = Hashtbl.create ~random:true 5 in
  let callback ~name:_ ~filename data =
    if filename <> "" then (
      let l = Hashtbl.find_opt files filename |> Option.value ~default:[] in
      Hashtbl.replace files filename (data :: l)
    );
    Lwt.return ()
  in
  let* fields = Request.to_multipart_form_data_exn ~callback req in
  let* jpegs =
    Lwt_list.map_p (fun (fname, parts) ->
        Lwt.return (fname, List.rev parts |> Astring.String.concat)
      ) (Hashtbl.to_seq files |> List.of_seq)
  in
  let field name =
    List.assoc_opt name fields
    |> Option.map trim_opt
    |> Option.join
    |> Option.map (Str.global_replace crlf_regex "\n")
  and draft =
    List.assoc_opt "draft" fields
    |> Option.map (function "false" -> None | _ -> Some ())
    |> Option.join
  in
  let category =
    Option.map (fun c ->
        if List.mem_assoc c Config.(category_assoc t)
        then c else Config.t.default_category
      ) (field "category")
  and title = field "title"
  and lead = field "lead"
  (* TODO: Properly parse this date. input validation. see RFC 3339 *)
  and date = field "date"
  and place = field "place"
  and body = field "body" |> Option.value ~default:""
  and gallery =
    (* get filenames *)
    List.filter_map
      (fun (k, v) ->
         match Astring.String.cuts ~sep:"-" k with
         | ["img"; id; "filename"] -> Some (id, v)
         | _ -> None
      )
      fields
    |> (* read other fields *)
    List.map (fun (id, filename) ->
        let field k = field ("img-" ^ id ^ "-" ^ k) in
        let open Post in
        let image =
          { caption = field "caption"
          ; source = field "source"
          ; filename
          }
        and position =
          field "position" |> Option.map int_of_string_opt
          |> Option.join |> Option.value ~default:0
        in
        position, image
      )
    |> (* move gallery item *)
    ( Request.query "up" req
      |> Option.map int_of_string_opt
      |> Option.join
      |> function
      | None -> fun x -> x
      | Some i -> List.map (fun (j, x) ->
          let j' =
            if j = i then i - 1
            else if j = i - 1  then i
            else j
          in j', x
        )
    )
    |> (* move gallery item *)
    ( Request.query "down" req
      |> Option.map int_of_string_opt
      |> Option.join
      |> function
      | None -> fun x -> x
      | Some i -> List.map (fun (j, x) ->
          let j' =
            if j = i then i + 1
            else if j = i + 1  then i
            else j
          in j', x
        )
    )
    |> (* delete gallery entry *)
    ( Request.query "delete" req
      |> Option.map int_of_string_opt
      |> Option.join
      |> function
      | None -> fun x -> x
      | Some i -> List.filter (fun (j, _) -> j <> i)
    )
    |> (* sort gallery by position *)
    List.sort (fun (a,_) (b,_) -> Int.compare a b)
    |> (* drop position *)
    List.map snd
  in
  let post : Post.t =
    { head = { category
             ; title
             ; lead
             ; date
             ; place
             ; gallery
             ; draft
             ; foreign = None
             }
    ; body }
  and author = author req
  in
  let* str = Store.master () in
  Store.save_post str ~author ~jpegs ?key post

let etag = Unix.time () |> Hashtbl.hash |> Printf.sprintf "%x"

let () =
  let foreach lst f app = List.fold_left (fun app el -> f el app) app lst in
  App.empty |> (* AUTH *)
  App.middleware Auth.middleware
  |> (* SERVE static files *)
  App.middleware (
    let local_path = Config.t.static_dir
    and etag_of_fname _fname =
      (* TODO: Open issue/PR regarding Lwt.t return type.
       * Derive etag from file modification date
       * https://github.com/rgrinberg/opium/issues/265
       *)
      Some etag (* Restarting the server leads to cache-invalidation *)
    and headers =
      (* Cache for 1 day. Then revalidate with Etag. *)
      Opium.Headers.of_list [("Cache-Control", "max-age=62400")]
    in Middleware.static_unix ~local_path ~etag_of_fname ~headers ())
  |> (* GET / -> REDIRECT category/year *)
  App.get Location.root (fun _req ->
      let* str = Store.master () in
      let category = Config.t.default_category in
      let+ year =
        Store.get_years str category
        >|= List.fold_left (fun a b -> if a > b then a else b) "2021"
      in
      Response.redirect_to (Location.year (category, year))
    )
  |> (* GET /create -> SHOW post form *)
  App.get Location.create_post (fun _req ->
      let p = Post.empty in
      View.post ~link_preview:false (Store.post_key p) p
      |> Response.of_html |> Lwt.return
    )
  |> (* POST /create -> SAVE post form *)
  App.post Location.create_post (fun req ->
      save_post req
      >|= function
      | Ok key' ->
        Response.redirect_to (Location.post key')
      | _ -> (* TODO communicate error *)
        Response.redirect_to Location.create_post
    )
  |> foreach Config.(category_assoc t) (fun (category, _) app ->
      app |> (* GET /category -> REDIRECT category/year *)
      App.get (Location.category category) (fun _req ->
          let* str = Store.master () in
          let+ year =
            Store.get_years str category
            >|= List.fold_left (fun a b -> if a > b then a else b) "2021"
          in
          Response.redirect_to (Location.year (category, year))
        )
      |> (* GET /category/year -> SHOW overview *)
      App.get (Location.year (category, ":a")) (fun req ->
          let year = Router.param req "a" in
          let* str = Store.master () in
          let+ years = Store.get_years str category
          and+ posts = Store.get_posts str (category, year)
          in
          let categories = Config.t.categories in
          View.posts ~categories ~category ~years ~year posts |> Response.of_html
        )
      |> (* GET /category/year/post -> SHOW post form *)
      App.get (Location.post (category, ":a", ":b")) (fun req ->
          let key = Router.(category, param req "a", param req "b") in
          Store.master ()
          >>= fun str -> Store.get_post str key
          >|= function
          | None -> Response.of_plain_text ~status:(`Code 404) "not found"
          | Some post -> View.post key post |> Response.of_html
        )
      |> (* POST /category/year/post -> SAVE post form *)
      App.post (Location.post (category, ":a", ":b")) (fun req ->
          let key = Router.(category, param req "a", param req "b") in
          save_post ~key req
          >|= function
          | Ok key' ->
            Response.redirect_to (Location.post key')
          | _ -> (* TODO communicate error *)
            Response.redirect_to (Location.post key)
        )
      |> (* POST /delete/category/year/post -> SAVE post form *)
      App.post (Location.delete_post (category, ":a", ":b")) (fun req ->
          let key = Router.(category, param req "a", param req "b") in
          let* str = Store.master () in
          Store.delete_post ~author:(author req) str key
          >|= fun _ ->
          Response.redirect_to (Location.category category)
        )
      |> (* GET /category/year/post/file -> STATIC w/ cache *)
      App.middleware (
        let parse fname =
          match Astring.String.cuts ~sep:"/" fname with
          | [ year; post; file ] -> Some ((category, year, post), file)
          | _ -> None
        in
        let read fname =
          match parse fname with
          | None -> Lwt.return_error `Not_found
          | Some (key, file) ->
            let* str = Store.master () in
            let+ o = Store.get_attachment str key file in
            match o with
            | None -> Error `Not_found
            | Some data -> Ok (Body.of_string data)
        and uri_prefix = Location.category category
        and etag_of_fname _fname =
          (* TODO: open Issue/PR to for [string option Lwt.t] return value
           * https://github.com/rgrinberg/opium/issues/265
             match parse fname with
             | None -> Lwt.return_none
             | Some (key, file) ->
             let* str = Store.master () in
             Store.get_attachment_etag str key file
          *)
          Some etag (* Restarting the server leads to cache-invalidation *)
        and headers =
          (* Cache for 1 day. Then revalidate with ETag. *)
          Opium.Headers.of_list [("Cache-Control", "max-age=62400")]
        in Middleware.static ~read ~uri_prefix ~headers ~etag_of_fname ()
      ))
  |> App.run_command
  |> ignore
