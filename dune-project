(lang dune 3.0)

(name slang)

(generate_opam_files true)

(source
 (github yallop/cc_cl_cam_ac_uk))

(authors "Timothy G. Griffin <tgg22@cam.ac.uk>")

(maintainers "Timothy G. Griffin <tgg22@cam.ac.uk>")

(license MIT)

(package
 (name slang)
 (synopsis "Slang interpreter and compiler for Cambridge Compiler Construction")
 (description
  "High-level interpreters and a stack-based VM compiler for the Slang language.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (ppx_deriving (>= 5.0))
  ppx_deriving_yojson)
 (tags
  (compiler interpreter "compiler construction" cambridge ocaml)))
