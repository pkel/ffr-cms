(* path to local git repository backing the store *)
let repo = "./_db"

(* path to managed content within git repository *)
let root = []

(* managed categories (directory/id, label) *)
let categories = [ "einsaetze", "Einsätze"
                 ; "neues", "Neuigkeiten"
                 ]

(* default category *)
let category_default = "einsaetze"
