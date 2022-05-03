(lang dune 3.0)

(name odefa)

(using menhir 2.0)

(generate_opam_files true)

(source
 (github JHU-PL-Lab/odefa))

(license Apache)

(authors "JHU PL Lab <pl.cs@jhu.edu>")

(maintainers "JHU PL Lab <pl.cs@jhu.edu>")

(package
 (name odefa)
 (description "Higher-Order Demand-Driven Symbolic Execution")
 (depends
  core
  base-threads
  batteries
  (alcotest :with-test)
  ounit
  (jhupllib
   (>= 0.1.1))
  (ocaml-monadic
   (>= 0.4.1))
  (ppx_deriving
   (>= 3.2))
  (ppx_deriving_yojson
   (>= 2.1))
  yojson))

(package
 (name sudu)
 (description "API built upon official ones for easy operations")
 (depends z3))
