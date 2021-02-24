open Lwt.Infix
open Brainstorm

let trim_opt s =
  match String.trim s with
  | "" -> None
  | s -> Some s

module View = struct
  open Tyxml.Html

  let page content =
    (html (head (title (txt "Brainstorm")) [])
       (body content))
    |> Lwt.return

  let posts () =
    let%lwt posts = Data.posts () in
    page [ h1 [txt "Posts"]
         ; ul ( List.map (fun (id, _) ->
               li [a ~a:[a_href ("/post/" ^ id)] [txt id]]) posts)
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

  let post id =
    let%lwt post = Data.post id in
    page [ h1 [txt ("Post #" ^ id)]
         ; form ~a:[a_method `Post]
             [ fieldset
                 [ input' "ptitle" "title" "Titel"      `Text post.head.title
                 ; input' "plead"  "lead"  "Untertitel" `Text post.head.lead
                 ; input' "pplace" "place" "Ort"        `Text post.head.place
                 ; input' "pdate"  "date"  "Datum"      `Date post.head.date
                 ]
             ; textarea ~a:[a_id "pbody"; a_name "body"] (txt post.body)
             ; br ()
             ; input ~a:[a_value "Speichern"; a_input_type `Submit] ()
             ]
         ; p [a ~a:[a_href "/"] [txt "Home"]]
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
end

open Opium

module Auth = struct
  let users = [ ("pkel", "FZMV9Kgha69fN3sAbiK2") ]

  let sessions = Hashtbl.create 7

  let auth_cookie_name = "session"

  let _ = Nocrypto_entropy_lwt.initialize ()

  let middleware =
    let filter handler req =
      match Request.cookie auth_cookie_name req with
      | Some value when Hashtbl.mem sessions value ->
        (* Authenticated *)
        handler req
      | _ ->
        (* Not authenticated *)
        match req.meth with
        | `POST -> (
            (* handle post from login form *)
            try
              let%lwt user = Request.urlencoded "user" req >|= Option.get
              and password = Request.urlencoded "password" req >|= Option.get
              in
              assert (List.mem (user, password) users);
              let id = Nocrypto.Rng.Z.gen_bits 256 |> Z.format "%064x" in
              Hashtbl.replace sessions id (); (* store session *)
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
          )
        | `GET ->
          (* show login form *)
          View.login () >|= Response.of_html
        | _ ->
          (* redirect get *)
          Response.redirect_to req.target |> Lwt.return
    in
    Rock.Middleware.create ~filter ~name:"Authentication"
end

let () =
  App.empty
  |> App.middleware Auth.middleware
  |> App.get "/" (fun _req ->
      View.posts () >|= Response.of_html
    )
  |> App.get "/post/:id" (fun req ->
      let id = Router.param req "id" in
      View.post id >|= Response.of_html
    )
  |> App.post "/post/:id" (fun req ->
      let id = Router.param req "id" in
      let str name =
        Request.urlencoded name req >|= fun x ->
        Option.bind x trim_opt
      in
      let%lwt title = str "title"
      and lead = str "lead"
      and date = str "date"
      and place = str "place"
      and body = str "body" >|= Option.value ~default:""
      in
      let post : Data.Post.t =
        { head = { title; lead; date; place }
        ; body }
      in
      let%lwt _ = Data.save_post id post in
      Response.redirect_to req.target |> Lwt.return
    )
  |> App.run_command
  |> ignore
