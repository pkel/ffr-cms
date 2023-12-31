open Sexplib.Std

type category =
  { id : string (* also used as folder under content_path *)
  ; label : string (* human readable *)
  } [@@deriving sexp]

type t =
  { repo : string (* unix path to git repository backing the store *)
  ; bare : bool (* whether the store is backed by a bare git repository *)
  ; static_dir : string (* unix path to statically served content *)
  ; user_file : string list (* path to user file within repository *)
  ; domain : string (* domain to use in git commit messages *)
  ; content_path : string list (* path to managed content within repository *)
  ; categories : category list (* managed categories *)
  ; default_category : string (* default category (folder) *)
  ; title : string (* website title *)
  ; preview_url : string (* base url of preview website *)
  } [@@deriving sexp]

let default_s =
  {|;; Example configuration
((repo ./_db.git) ; unix path to local git repository to-be-managed
 (bare true) ; whether the git repository is bare
 (static_dir ./static) ; unix path to statically served content
 (user_file (editors)) ; path to user file within repository
 (domain localhost) ; domain to use in git commit messages
 (content_path (content)) ; path to managed content within git repository
 ; managed categories ; id is used as folder under content_path
 (categories (((id einsaetze) (label Einsätze))
              ((id neues) (label Neuigkeiten))
             ))
 (default_category einsaetze) ; default category (id)
 (title "ffr-cms") ; website title
 (preview_url "/vorschau") ; base url of preview website
 )
|}

let t : t =
  let path = "./config.sexp" in
  let rec h first_try =
    match Sexplib.Sexp.load_sexp_conv_exn path t_of_sexp with
    | a ->
      (* TODO: use logs library *)
      Printf.eprintf "Use configuration: %s\n%!" path;
      a
    | exception (Sys_error _ as e) ->
      if first_try then (
        (* TODO: use logs library *)
        Printf.eprintf "Write example configuration: %s\n%!" path;
        let oc = open_out path in
        begin
          try
            Printf.fprintf oc "%s" default_s;
            close_out oc
          with e ->
            close_out oc;
            raise e
        end;
        h false
      ) else raise e
  in h true

let category_assoc t =
  List.map (fun c -> c.id, c.label) t.categories
