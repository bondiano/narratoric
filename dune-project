(lang dune 3.17)

(formatting
 (enabled_for dune ocaml))

(name narratoric)

(generate_opam_files true)

(source
 (github bondiano/narratoric))

(authors "Vassiliy Kuzenkov <batbondik0@gmail.com>")

(maintainers "Vassiliy Kuzenkov <batbondik0@gmail.com>")

(license MIT)

(package
 (name narratoric)
 (synopsis "A visual novel and narrative game engine")
 (description "A visual novel and narrative game engine written in OCaml with support for interactive narratives, branching dialogue, and multimedia content.")

 (depends
  ocaml
  (dune (>= 3.17))
  (base (>= v0.16.0))
  (stdio (>= v0.16.0))
  (ppx_deriving (>= 5.2.1))
  (cmdliner (>= 1.3.0))
  (logs (>= 0.7.0))
  (fmt (>= 0.9.0))
  (ocamlformat (and :with-dev-setup (>= 0.27.0))))

 (tags
  ("visual novel" "game engine" "narrative" "ocaml" "interactive fiction")))
