open Lwt.Infix
open Lwt.Syntax
open Bslib

let trim_opt s =
  match String.trim s with
  | "" -> None
  | s -> Some s

module Location = struct
  let root = "/"
  let year y = root ^ "posts/" ^ y
  let post (y, id) = year y ^ "/" ^ id
  let attachment key file = post key ^ "/" ^ file
end

module View = struct
  open Tyxml.Html

  let page content =
    (html (head (title (txt "Brainstorm")) [])
       (body content))

  let post_years () =
    let+ years = Data.get_post_years () in
    page [ h1 [txt "Posts nach Jahr"]
         ; ul ( List.map (fun year ->
               let loc = Location.year year in
               li [a ~a:[a_href loc] [txt loc]]) years)
         ]

  let posts ~year =
    let+ posts = Data.get_posts ~year in
    page [ h1 [txt ("Posts " ^ year)]
         ; ul ( List.map (fun (key, _) ->
               let loc = Location.post key in
               li [a ~a:[a_href loc] [txt loc]]) posts)
         ; p [a ~a:[a_href Location.root] [txt "Home"]]
         ]

  let input' id var lbl typ v =
    let a = [a_id id; a_name var; a_input_type typ] in
    let a = match v with
      | None -> a
      | Some x -> a_value x :: a
    in
    div
      [ label ~a:[a_label_for id] [txt lbl]
      ; input ~a ()
      ]

  let post key =
    let+ post = Data.get_post key in
    let name = fst key ^ "/" ^ snd key in
    page [ h1 [txt ("Post " ^ name)]
         ; form ~a:[a_method `Post]
             [ fieldset
                 [ input' "ptitle" "title" "Titel"      `Text post.head.title
                 ; input' "plead"  "lead"  "Untertitel" `Text post.head.lead
                 ; input' "pplace" "place" "Ort"        `Text post.head.place
                 ; input' "pdate"  "date"  "Datum"      `Date post.head.date
                 ]
             ; textarea ~a:[a_id "pbody"; a_name "body"] (txt post.body)
             ; br ()
             ; button ~a:[ a_button_type `Submit ] [ txt "Speichern" ]
             ]
         ; form ~a:[ a_method `Post
                   ; a_action (Location.post key ^ "?action=upload")
                   ; a_enctype "multipart/form-data"
                   ]
             [ input ~a:[ a_input_type `File; a_name "photo_upload"
                        ; a_multiple () ; a_accept ["image/*"]; a_required ()
                        ] ()
             ; button ~a:[ a_button_type `Submit ] [ txt "Bilder Hochladen" ]
             ]
         ; p [a ~a:[a_href (Location.year (fst key))] [txt (fst key)]]
         ; p [a ~a:[a_href Location.root] [txt "Home"]]
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

  let _ = Nocrypto_entropy_lwt.initialize ()

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
      let* user = Request.urlencoded "user" req >|= Option.get
      and* pwd = Request.urlencoded "password" req >|= Option.get
      in
      let registered, user =
        match List.assoc_opt user users with
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

  let middleware =
    let filter handler req =
      match Request.cookie auth_cookie_name req with
      | Some value when Hashtbl.mem sessions value ->
        (* Authenticated *)
        let user = Hashtbl.find sessions value in
        let env =
          Context.add user_key user req.env
        in
        handler { req with env }
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

let post_save key req =
  let str name =
    Request.urlencoded name req >|= fun x ->
    Option.bind x trim_opt
  in
  let* title = str "title"
  and* lead = str "lead"
  (* TODO: Properly parse this date. input validation. see RFC 3339 *)
  and* date = str "date"
  and* place = str "place"
  and* body = str "body" >|= Option.value ~default:""
  in
  let post : Data.Post.t =
    { head = { title; lead; date; place; source=None }
    ; body }
  and author = author req
  in
  Data.save_post ~author key post >|= function
  | Ok key' ->
    Response.redirect_to (Location.post key')
  | _ -> (* TODO communicate error *)
    Response.redirect_to Location.root

let post_upload key req =
  let files = Hashtbl.create ~random:true 5 in
  let callback ~name:_ ~filename data =
    if filename <> "" then (
      let l = Hashtbl.find_opt files filename |> Option.value ~default:[] in
      Hashtbl.replace files filename (data :: l)
    );
    Lwt.return ()
  in
  let* _ = Request.to_multipart_form_data_exn ~callback req in
  let* files =
    Lwt_list.map_p (fun (fname, parts) ->
        let () = Logs.info
            (fun m -> m "File upload: %s" (Location.attachment key fname))
        in
        Lwt.return (fname, List.rev parts |> Astring.String.concat)
      ) (Hashtbl.to_seq files |> List.of_seq)
  in
  Data.add_attachments ~author:(author req)
    key files >|= function
  | Ok () ->
    Response.redirect_to (Location.post key)
  | _ -> (* TODO communicate error *)
    Response.redirect_to Location.root

let () =
  App.empty
  |> App.middleware Auth.middleware
  |> App.get Location.root (fun _req ->
      View.post_years () >|= Response.of_html
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
      (* I think both actions could go into the same multipart/form-data request *)
      let key = Router.(param req "a", param req "b") in
      match Request.query "action" req with
      | Some "upload" -> post_upload key req
      | _ -> post_save key req
    )
  |> App.run_command
  |> ignore
