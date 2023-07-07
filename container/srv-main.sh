#!/usr/bin/env bash

cat > config.sexp << EOF
((repo /website.git) ; unix path to local git repository to-be-managed
 (bare true) ; whether the git repository is bare
 (static_dir /static) ; unix path to statically served content
 (user_file (opium-users)) ; path to user file within repository
 (content_path (content)) ; path to managed content within git repository
 ; managed categories ; id is used as folder under content_path
 (categories (((id einsaetze) (label Einsätze))
              ((id neues) (label Neuigkeiten))
             ))
 (default_category einsaetze) ; default category (id)
 (title "ffr-cms") ; website title
 )
EOF

exec ffr-cms --port=3000 --verbose
