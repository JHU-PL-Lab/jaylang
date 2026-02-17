(lang dune 3.20)

(name jay)

(using menhir 3.0)

(generate_opam_files true)

(source
 (github JHU-PL-Lab/jaylang))

(license Apache-2.0)

(authors "JHU PL Lab <pl.cs@jhu.edu>")

(maintainers "JHU PL Lab <pl.cs@jhu.edu>")

(package
 (name jay)
 (description "Semantic typing via program translation and symbolic execution")
 (depends
  core
  core_unix
  (alcotest :with-test)
  menhir
  lwt
  landmarks
  landmarks-ppx
  alcotest
  alcotest-lwt
  bisect_ppx
  baby
  psq
  preface
  mtime
  moonpool
  vector
  z3
  (ocaml-monadic
   (>= 0.4.1))
  (ppx_deriving
   (>= 3.2))))


