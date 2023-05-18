(lang dune 2.8)
(name ffr-opium)
(authors "Patrik Keler")
(maintainers "patrik@pkel.dev")

(generate_opam_files true)

(package
  (name ffr-opium)
  (synopsis "FFR CMS based on OCaml Opium")
  (description ".")
  (depends
    (ocaml (< 4.12.0))
    (argon2 (>= 1.0.1))
    (dbm (>= 1.2))
    (dune (and :build (>= 2.8)))
    (irmin-unix (< 2.6.0))
    (opium (>= 0.20.0))
    (nocrypto (>= 0.5.4-2))
    (ppx_expect (>= v0.14.1))
    (ppx_inline_test (and :build (>= v0.14.1)))
    (ppx_sexp_conv (>= v0.14.3))
    (yaml (>= 2.1.0))
    ))

(package
  (name devtools)
  (synopsis ".")
  (description ".")
  (depends
    (merlin :dev)
    (ocp-indent :dev)
    (odig :dev)
    (odoc :dev)
    ))
