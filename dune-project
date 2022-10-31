(lang dune 3.0)

(name jay)

(using menhir 2.0)

(generate_opam_files true)

(source
 (github JHU-PL-Lab/jaylang))

(license Apache)

(authors "JHU PL Lab <pl.cs@jhu.edu>")

(maintainers "JHU PL Lab <pl.cs@jhu.edu>")

(package
 (name jay)
 (description "Higher-Order Demand-Driven Symbolic Execution")
 (depends
  core
  core_unix
  batteries
  (alcotest :with-test)
  shexp
  hashcons
  ocamlgraph
  menhir
  fmt
  logs
  lwt
  lwt_ppx
  shexp
  pds-reachability
  landmarks
  landmarks-ppx
  ounit
  alcotest
  alcotest-lwt
  sudu
  unroll
  (jhupllib
   (>= 0.1.1))
  (ocaml-monadic
   (>= 0.4.1))
  (ppx_deriving
   (>= 3.2))
  (ppx_deriving_yojson
   (>= 2.1))
  yojson
  pds-reachability))

(package
 (name sudu)
 (description "Z3 API built upon official one for easy operations")
 (depends core z3 logs fmt))

(package
 (name unroll)
 (description "Unroll is a library to search unbounded cases")
 (depends
  core
  core_unix.nano_mutex
  core_kernel.pairing_heap
  hashcons
  fmt
  lwt
  lwt.unix
  logs
  logs.lwt
  logs.fmt))

(package
 (name langdk)
 (description "Lang Develop Toolkit")
 (depends core core_unix.sys_unix lwt lambda-term react))
