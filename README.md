opam update
opam upgrade
opam install oasis batteries menhir ounit ppx_deriving ocaml-monadic monadlib
oasis setup -setup-update dynamic
./configure
ocaml setup.ml -configure --enable-tests
make
