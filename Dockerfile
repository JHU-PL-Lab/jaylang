FROM ocaml/opam:ubuntu-22.04-ocaml-5.2
WORKDIR /home/opam/jaylang

# Z3 needs python3, and something else needs libgmp-dev and pkg-config
RUN sudo apt-get update && sudo apt-get install -y python3 libgmp-dev pkg-config

# Force at most four processes with -j 4 to reduce memory usage
ADD jay.opam .
RUN opam exec -- opam pin add -y --no-action jay .
RUN opam exec -- opam depext jay
RUN opam exec -- opam install -j 4 --deps-only jay

COPY --chown=opam . .
RUN opam exec -- dune build
