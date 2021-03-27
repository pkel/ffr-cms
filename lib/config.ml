open Sexplib.Std

type category =
  { id : string (* also used as folder under content_path *)
  ; label : string (* human readable *)
  } [@@deriving sexp]

type t =
  { repo : string (* unix path to git repository backing the store *)
  ; static_dir : string (* unix path to statically served content *)
  ; user_file : string list (* path to user file within repository *)
  ; content_path : string list (* path to managed content within repository *)
  ; categories : category list (* managed categories *)
  ; default_category : string (* default category (folder) *)
  } [@@deriving sexp]

let t : t =
  Sexplib.Sexp.load_sexp_conv_exn "./config.sexp" t_of_sexp

let category_assoc t =
  List.map (fun c -> c.id, c.label) t.categories
