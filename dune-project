(lang dune 3.16)

(name jay)

(using menhir 2.0)

(generate_opam_files true)

(source
 (github JHU-PL-Lab/jaylang))

(license Apache-2.0)

(authors "JHU PL Lab <pl.cs@jhu.edu>")

(maintainers "JHU PL Lab <pl.cs@jhu.edu>")

(package
 (name jay)
 (description "Higher-Order Demand-Driven Symbolic Execution")
 (depends
  core
  core_unix
  (alcotest :with-test)
  menhir
  lwt
  lwt_ppx
  landmarks
  landmarks-ppx
  alcotest
  alcotest-lwt
  psq
  preface
  moonpool
  z3
  (ocaml-monadic
   (>= 0.4.1))
  (ppx_deriving
   (>= 3.2))))


