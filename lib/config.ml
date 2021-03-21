(* path to local git repository backing the store *)
let repo = "./_db"

(* path to user file in git repository *)
(* w/o leading/trailing slashes *)
let user_file = "opium-users"

(* path to managed content within git repository *)
(* w/o leading/trailing slashes *)
let content_path = "content"

(* managed categories (directory/id, label) *)
let categories = [ "einsaetze", "Einsätze"
                 ; "neues", "Neuigkeiten"
                 ]

(* default category *)
let category_default = "einsaetze"
