(lang dune 2.8)
(name ffr-cms)
(authors "Patrik Keller")
(maintainers "patrik@pkel.dev")

(generate_opam_files true)

(package
  (name ffr-cms)
  (synopsis "FFR CMS based on OCaml Opium")
  (description ".")
  (depends
    (ocaml (and (>= 5.5.0) (< 5.6)))
    (dune (and :build (>= 3.7)))
    (irmin-git (and (>= 3.11.0) (< 3.12)))
    (opium (and (>= 0.20.0) (< 0.21)))
    (sexplib (and (>= v0.17.0) (< v0.18)))
    (ppx_sexp_conv (and (>= v0.17.0) (< v0.18)))
    (mirage-crypto-rng (>= 0.11.1))
    (argon2 (>= 1.0.2))
    (yaml (and (>= 3.2.0) (< 3.3)))
    ppx_expect
    ppx_inline_test
    (merlin (and :with-dev-setup))
    (ocp-indent (and :with-dev-setup))
    (odig (and :with-dev-setup))
    ))
