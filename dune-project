(lang dune 2.8)
(name ffr-opium)
(authors "Patrik Keller")
(maintainers "patrik@pkel.dev")

(generate_opam_files true)

(package
  (name ffr-opium)
  (synopsis "FFR CMS based on OCaml Opium")
  (description ".")
  (depends
    (ocaml (and (>= 4.14.1) (< 4.15)))
    (dune (and :build (>= 3.7)))
    (irmin-git (and (>= 3.7.1) (< 3.8)))
    (opium (and (>= 0.20.0) (< 0.21)))
    (ppx_sexp_conv (and (>= v0.16.0) (< v0.17)))
    (nocrypto (and (>= 0.5.4-2) (< 0.6)))
    (argon2 (and (>= 1.0.1) (< 1.1)))
    (yaml (and (>= 3.1.0) (< 3.2)))
    ppx_expect
    ppx_inline_test
    (merlin (and :with-dev-setup))
    (ocp-indent (and :with-dev-setup))
    (odig (and :with-dev-setup))
    ))
