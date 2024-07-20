FROM ocaml/opam:ubuntu-23.04-ocaml-4.14

RUN sudo apt-get update && sudo apt-get install -y libgmp-dev python3 vim emacs graphviz time
WORKDIR /home/opam
RUN sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam && opam init --reinit -ni
RUN echo "eval $(opam env)" >> .bashrc

RUN eval $(opam env) && opam update
RUN eval $(opam env) && opam install utop ocaml-lsp-server ocamlformat
RUN eval $(opam env) && opam install core core_unix psq hashcons ocamlgraph z3 fmt ppx_deriving yojson=2.1.2 ppx_deriving_yojson=3.7.0 lwt_ppx landmarks-ppx jhupllib monadlib pds-reachability alcotest-lwt
RUN eval $(opam env) && opam install csv core_bench shexp

WORKDIR /home/opam/jaylang
COPY --chown=opam . .
RUN git checkout oopsla-24

RUN eval $(opam env) && opam exec -- dune build

ENTRYPOINT [ "/bin/bash" ]
