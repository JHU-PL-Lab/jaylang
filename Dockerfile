FROM ocaml/opam:ubuntu-23.04-ocaml-5.2

RUN sudo apt-get update && sudo apt-get install -y libgmp-dev python3 vim emacs graphviz time
WORKDIR /home/opam
RUN sudo ln -f /usr/bin/opam-2.1 /usr/bin/opam && opam init --reinit -ni
RUN echo "eval $(opam env)" >> .bashrc

RUN eval $(opam env) && opam update
RUN eval $(opam env) && opam install utop ocaml-lsp-server ocamlformat
RUN eval $(opam env) && opam pin add monadlib https://github.com/besport/monadlib/archive/076858f1a9ce700b89f6e0a9651b9dd32c71fc29.zip
RUN eval $(opam env) && opam install core core_unix psq hashcons ocamlgraph z3 fmt ppx_deriving yojson ppx_deriving_yojson lwt_ppx landmarks-ppx jhupllib pds-reachability alcotest-lwt
RUN eval $(opam env) && opam install csv core_bench shexp

WORKDIR /home/opam/jaylang
COPY --chown=opam . .
RUN git checkout ocaml-5.2.0

RUN eval $(opam env) && opam exec -- dune build

ENTRYPOINT [ "/bin/bash" ]
