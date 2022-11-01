Jay Lang
=====

(Update: Oct 30, 2022)

This the the codebase for languages BlueJay, Jay and Jay Intermediate Language (JayIL), developped by JHU Programming Languages Lab. It's a pipeline of functional languages that fits for research at each layers.

This monorepo contains tools built upon these languages.

This was for the artifact for the paper **Higher-Order Demand-Driven Symbolic Evaluation**.

Install
-------

The repo is tested under MacOS, Ubuntu, and WSL on Windows.

Prepare and upgrade `apt` and the ocaml environment
```
sudo apt upgrade opam
opam update
```

Install local opam switch. Answer `yes` to questions. It can take a while.

```
opam switch create ./ 4.14.0

opam user-setup install
opam install ocaml-lsp-server ocamlformat
```


Here are the libraries needed:
```
opam install core_kernel core_unix fmt logs lwt_ppx hashcons ocamlgraph alcotest alcotest-lwt landmarks landmarks-ppx
opam install react lambda-term pds-reachability


```
<!-- opam install shexp core batteries gmap jhupllib monadlib ocaml-monadic pds-reachability ppx_deriving ppx_deriving_yojson -y -->

Run
---

```
make
make test
make benchmark
```
