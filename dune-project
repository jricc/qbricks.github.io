(lang dune 3.17)

(using menhir 3.0)

(name sQbricks)

(version 0.1.0)

(generate_opam_files true)

(source
 (github jricc/SQbricks))

(authors "Jerome Ricciardi")

(maintainers "Jerome Ricciardi")

(license LGPL-2.1-only)

(documentation https://github.com/jricc/SQbricks/tree/main/doc)

(package
 (name sQbricks)
 (synopsis "Automatic equivalence checker for hybrid quantum circuits")
 (description
  "SQbricks is a tool for verifying the equivalence of hybrid quantum circuits.\nIt combines:\n- SQL: Implementation of our lifting method enabling hybrid circuit verification\n- SQV: A unitary verification tool for quantum circuits\n\nThe tool automatically checks circuit equivalence using formal methods.")
 (depends
  ocaml
  dune
  zarith
  landmarks
  benchmark
  lwt
  batteries
  logs
  menhir
  (alcotest :with-test))
 (tags
  (topics
   "quantum computing"
   "circuit equivalence"
   "formal verification"
   "quantum circuits"
   "hybrid circuits")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
