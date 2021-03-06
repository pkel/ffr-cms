open Lwt.Infix
open Lwt.Syntax
open Bslib

let trim_opt s =
  match String.trim s with
  | "" -> None
  | s -> Some s

module Location = struct
  let root = "/"
  let year y = root ^ "posts/" ^ y ^ "/"
  let post (y, id) = year y ^ id ^ "/"
  let attachment key file = post key ^ file
end

module View = struct
  open Tyxml.Html

  module BS = struct
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
       (head (title (txt "Brainstorm"))
          [ meta ~a:[ a_charset "UTF-8" ] ()
          ; meta ~a:[ a_name "viewport"
                    ; a_content "width=device-width, initial-scale=1.0"
                    ] ()
          ; link ~href:"/bootstrap.min.css" ~rel:[`Stylesheet] ()
          ; link ~href:"/app.css" ~rel:[`Stylesheet] ()
          ])
       (body ([ script (txt "0")
              ; div ~a:[a_class ["container"; "mb-3"; "mt-3"]] content
              ])))

  let post_years () =
    let+ years = Data.get_post_years () in
    page [ h1 [txt "Posts nach Jahr"]
         ; ul ( List.map (fun year ->
               let loc = Location.year year in
               li [a ~a:[a_href loc] [txt loc]]) years)
         ]

  let posts ~year =
    let+ years =
      let cls = ["badge"; "badge-pill"] in
      Data.get_post_years ()
      >|= List.sort compare
      >|= List.map (fun y ->
          let cls = if y = year then "badge-secondary" :: cls else "badge-light" :: cls in
          li ~a:[a_class ["list-inline-item"; "h4"]]
            [a ~a:[ a_class cls; a_href (Location.year y) ] [ txt y ]]
        )
      >|= ul ~a:[a_class ["list-inline"]]
    and+ posts = Data.get_posts ~year in
    page [ years
         ; ul ( List.map (fun (key, _) ->
               let loc = Location.post key in
               li [a ~a:[a_href loc] [txt loc]]) posts)
         ]

  let hex_hash (type a) (x: a) : string =
    Hashtbl.hash x |> Printf.sprintf "%x"

  let post key =
    let id =
      let prefix = hex_hash key in
      fun x -> "post-" ^ prefix ^ "-" ^ x
    in
    let input' name lbl typ v = BS.input ~id ~name ~lbl typ v in
    let+ post = Data.get_post key in
    let name = fst key ^ "/" ^ snd key in
    page [ h1 ~a:[a_class ["h3"]] [txt ("Post " ^ name)]
         ; form ~a:[ a_method `Post
                   ; a_enctype "multipart/form-data"
                   ]
             [ input' "title" "Titel"      `Text post.head.title
             ; input' "lead"  "Untertitel" `Text post.head.lead
             ; input' "place" "Ort"        `Text post.head.place
             ; input' "date"  "Datum"      `Date post.head.date
             ; BS.textarea ~id ~name:"body" ~lbl:"Beitrag" ~rows:20
                 ( Some post.body )
             ; List.mapi ( fun i x ->
                   let i = Int.to_string i in
                   let open Data.Post in
                   let id_prefix = hex_hash x.filename in
                   let name' x = ("img" ^ "-" ^ id_prefix ^ "-" ^ x) in
                   let input' name lbl typ v =
                     BS.input ~id ~name:(name' name) ~lbl typ v
                   and action name lbl =
                     li ~a:[a_class ["list-inline-item"]]
                       [ button ~a:[ a_formaction ("?" ^ name ^ "=" ^ i)
                                   ; a_class ["btn"; "btn-light"]
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
                         ; ul ~a:[a_class ["list-inline"; "float-right"]]
                             [ action "up" "🢁"
                             ; action "down" "🡻"
                             ; action "delete" "✖"
                             ]
                         ]
                     ]

                   ) post.head.gallery
               |> List.concat_map (fun f -> [f; hr ()])
               |> (fun l -> div ( hr () :: l ))
             ; (let id = id "upload" in
                div ~a:[a_class ["form-group"]]
                  [ label ~a:[a_label_for id ] [txt "Bilder hinzufügen"]
                  ; input ~a:[ a_input_type `File; a_name "upload"
                             ; a_id id
                             ; a_multiple () ; a_accept ["image/*"]
                             ; a_class ["form-control-file"]
                             ] ()
                  ])
             ; div ~a:[a_class ["clearfix"; "pt-3"]]
                 [ a ~a:[ a_href ".."
                        ; a_class ["btn"; "btn-secondary"; "float-left"]
                        ]
                     [ txt "Zurück zur Übersicht" ]
                 ; button ~a:[ a_button_type `Submit
                             ; a_class ["btn"; "btn-primary"; "float-right"]
                             ]
                     [ txt "Speichern" ]
                 ]
             ]
         ]

  let login ?message () =
    page ( [ h1 [txt "Login"]
           ; form ~a:[a_method `Post]
               [ fieldset
                   [ label [txt "login: "]
                   ; input ~a:[a_name "user"; a_input_type `Text] ()
                   ; br ()
                   ; label [txt "password: "]
                   ; input ~a:[a_name "password"; a_input_type `Password] ()
                   ; br ()
                   ; input ~a:[a_value "Login"; a_input_type `Submit] ()
                   ]
               ]
           ]
           @ match message with
           | None -> []
           | Some m -> [p [ txt m ]]
         )
    |> Lwt.return
end

open Opium

module Auth = struct
  open Sexplib.Std

  type user_data =
    { name: string (* goes into commit message *)
    ; email: string (* goes into commit message *)
    } [@@deriving sexp_of]

  type user_auth =
    { argon2encoded: string }

  type user =
    { data: user_data
    ; auth: user_auth
    }

  let _init =
    (* Seed Rng and reseed it regularly *)
    Nocrypto_entropy_lwt.initialize ()

  let salt len =
    Nocrypto.Rng.generate len |> Cstruct.to_string

  let user ~email ~name pwd : user =
    let salt_len = 16 in
    let salt = salt salt_len
    and t_cost = 1
    and m_cost = 1 lsl 20
    and parallelism = 4
    and hash_len = 16
    and kind = Argon2.ID
    and version = Argon2.VERSION_13
    in
    let encoded_len =
      Argon2.encoded_len ~salt_len ~t_cost ~m_cost ~kind ~hash_len ~parallelism
    in
    let argon2encoded =
      Argon2.hash ~t_cost ~m_cost ~parallelism ~pwd ~salt ~hash_len
        ~encoded_len ~version ~kind
      |> Result.map snd
      |> Result.get_ok
    in
    { data = {name ; email}
    ; auth = {argon2encoded}
    }

  (* TODO: read these entries from the git repository *)
  let users =
    [ "pkel", user ~email:"patrik@pkel.dev" ~name:"Patrik Keller"
        "FZMV9Kgha69fN3sAbiK2" ]

  let nobody = user ~email:"" ~name:"" (salt 16)

  let sessions_dbm = Dbm.opendbm "_sessions" [Dbm_rdwr; Dbm_create] 0o600
  let sessions = Hashtbl.create 7

  let auth_cookie_name = "session"

  let user_key =
    Rock.Context.Key.create ("auth_user", sexp_of_user_data)

  let user req =
    let open Request in
    Context.find user_key req.env

  let post_login req =
    (* handle post from login form *)
    try
      let* user_id = Request.urlencoded "user" req >|= Option.get
      and* pwd = Request.urlencoded "password" req >|= Option.get
      in
      let registered, user =
        match List.assoc_opt user_id users with
        | Some x -> true, x
        | None -> false, nobody
      in
      let encoded = user.auth.argon2encoded in
      assert (Argon2.verify ~encoded ~pwd ~kind:ID |> Result.get_ok);
      (* Intentionally check registered after hashing.
       * Otherwise, we provide a username oracle. *)
      assert registered;
      let id =
        Nocrypto.Rng.generate 32
        |> Nocrypto.Base64.encode
        |> Cstruct.to_string
      in
      Dbm.replace sessions_dbm id user_id; (* store session persistently *)
      Hashtbl.replace sessions id user.data; (* store session *)
      Response.redirect_to req.target (* redirect get *)
      |> Response.add_cookie
        ~http_only:true
        ~same_site:`Strict
        ~expires:`Session
        ~scope:(Uri.of_string "/")
        (auth_cookie_name, id)
      |> Lwt.return
    with _ ->
      View.login ~message:"login failed" ()
      >|= Response.of_html

  let dbm_mem dbm key =
    match Dbm.find dbm key with
    | _ -> true
    | exception _ -> false

  let middleware =
    let filter handler req =
      match Request.cookie auth_cookie_name req with
      | Some cookie when Hashtbl.mem sessions cookie ->
        (* Authenticated (memory) *)
        let user = Hashtbl.find sessions cookie in
        let env = Context.add user_key user req.env in
        handler { req with env }
      | Some cookie when dbm_mem sessions_dbm cookie ->
        (* Authenticated (persistent store) *)
        let user = Dbm.find sessions_dbm cookie in
        begin
          match List.assoc_opt user users with
          | Some user ->
            let env = Context.add user_key user.data req.env in
            Hashtbl.replace sessions cookie user.data;
            handler { req with env }
          | None ->
            (* user has a valid cookie but we cannot find his data *)
            Response.redirect_to req.target |> Lwt.return
        end
      | _ ->
        (* Not authenticated *)
        match req.meth with
        | `POST ->
          (* handle post from login form *)
          post_login req
        | `GET ->
          (* show login form *)
          View.login () >|= Response.of_html
        | _ ->
          (* redirect get *)
          Response.redirect_to req.target |> Lwt.return
    in
    Rock.Middleware.create ~filter ~name:"Authentication"
end

let app_name = "brainstorm"

let author req =
  let user = Auth.user req |> Option.get in
  Printf.sprintf "%s via %s <%s>" user.name app_name user.email

let () =
  App.empty
  |> App.middleware Auth.middleware
  |> App.middleware (Middleware.static_unix ~local_path:"static" ())
  |> App.get Location.root (fun _req ->
      Data.get_post_years ()
      >|= List.fold_left (fun a b -> if a > b then a else b) "2020"
      >|= fun year -> Response.redirect_to (Location.year year)
    )
  |> App.get (Location.year ":a") (fun req ->
      let year = Router.param req "a" in
      View.posts ~year >|= Response.of_html
    )
  |> App.get (Location.post (":a", ":b")) (fun req ->
      let key = Router.(param req "a", param req "b") in
      View.post key >|= Response.of_html
    )
  |> App.post (Location.post (":a", ":b")) (fun req ->
      let key = Router.(param req "a", param req "b") in
      let files = Hashtbl.create ~random:true 5 in
      let callback ~name:_ ~filename data =
        if filename <> "" then (
          let l = Hashtbl.find_opt files filename |> Option.value ~default:[] in
          Hashtbl.replace files filename (data :: l)
        );
        Lwt.return ()
      in
      let* fields = Request.to_multipart_form_data_exn ~callback req in
      let* files =
        Lwt_list.map_p (fun (fname, parts) ->
            let () = Logs.info
                (fun m -> m "File upload: %s" (Location.attachment key fname))
            in
            Lwt.return (fname, List.rev parts |> Astring.String.concat)
          ) (Hashtbl.to_seq files |> List.of_seq)
      in
      let field name =
        List.assoc_opt name fields |> fun x -> Option.bind x trim_opt
      in
      let title = field "title"
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
            let open Data.Post in
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
      let post : Data.Post.t =
        { head = { title
                 ; lead
                 ; date
                 ; place
                 ; gallery
                 ; foreign = None
                 }
        ; body }
      and author = author req
      in
      Data.save_post ~author ~files key post >|= function
      | Ok key' ->
        Response.redirect_to (Location.post key')
      | _ -> (* TODO communicate error *)
        Response.redirect_to (Location.post key)
    )
  |> App.get (Location.attachment (":a", ":b") ":c") (fun req ->
      let key = Router.(param req "a", param req "b")
      and fname = Router.param req "c"
      in
      Data.get_attachment key fname >|= function
      | None -> Response.of_plain_text ~status:(`Code 404) "not found"
      | Some data ->
        let headers =
          Headers.of_list
            [ "Content-Type", Magic_mime.lookup fname ]
        in
        Response.of_plain_text ~headers data
    )
  |> App.run_command
  |> ignore
