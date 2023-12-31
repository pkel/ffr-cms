#!/usr/bin/env bash

cat > config.sexp << EOF
((repo /website.git) ; unix path to local git repository to-be-managed
 (bare true) ; whether the git repository is bare
 (static_dir /static) ; unix path to statically served content
 (user_file (editors)) ; path to user file within repository
 (domain "$DOMAIN") ; domain to use in git commit messages
 (content_path (content)) ; path to managed content within git repository
 ; managed categories ; id is used as folder under content_path
 (categories (((id einsaetze) (label Einsätze))
              ((id neues) (label Neuigkeiten))
             ))
 (default_category einsaetze) ; default category (id)
 (title "Editor") ; website title
 (preview_url "$PREVIEW_URL") ; location of site preview
 )
EOF

exec ffr-cms --port=3000 --verbose "$@"
