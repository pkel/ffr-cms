module Data = struct
  let posts = ref [ ("id42", "hello world")
                  ; ("id99", "This is the second test post")
                  ]
end

module View = struct
  open Tyxml.Html

  let page content =
    (html (head (title (txt "Brainstorm")) [])
       (body content))

  let posts () =
    page [ h1 [txt "Posts"]
         ; ul (List.map (fun (id, _) ->
               li [a ~a:[a_href ("/post/" ^ id)] [txt id]]) !Data.posts)
         ]

  let post id =
    page [ h1 [txt ("Post ID: " ^ id)]
         ; p [txt (List.assoc id !Data.posts)]
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
              let open Lwt.Infix in
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
              |> Response.of_html |> Lwt.return
          )
        | `GET ->
          (* show login form *)
          View.login () |> Response.of_html |> Lwt.return
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
      View.posts () |> Response.of_html |> Lwt.return
    )
  |> App.get "/post/:id" (fun req ->
      let id = Router.param req "id" in
      View.post id |> Response.of_html |> Lwt.return
    )
  |> App.run_command
  |> ignore
