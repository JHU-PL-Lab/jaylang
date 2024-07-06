Jay Lang
=====

(Update: July 5th, 2024)

This the the codebase for languages BlueJay, Jay and Jay Intermediate Language (JayIL), as well as the language's semantic-type-guided type checker. This code is developped by JHU Programming Languages Lab. It's a pipeline of functional languages that fits for research at each layers.

This monorepo contains all tools built upon these languages.

This snapshot is for the artifact for the paper **Semantic-Type-Guided Bug Finding**.

Install
-------

The repo is tested under MacOS, Ubuntu, and WSL on Windows.

Prepare and upgrade `apt` and the ocaml environment
```
sudo apt upgrade opam
opam update
```

Install local opam switch. Answer `yes` to questions. It can take a while.
This command installs the dependencies of this project to opam. You are supposed to develop in this directory.


```
opam switch create 4.14.0
```

After that, you can install the develop tools
```
opam user-setup install
opam install utop ocaml-lsp-server ocamlformat
opam install core core_unix psq hashcons ocamlgraph z3 fmt ppx_deriving yojson=2.1.2 ppx_deriving_yojson=3.7.0 lwt_ppx landmarks-ppx jhupllib monadlib pds-reachability alcotest-lwt
```

Now you should be able to run the project.


Run
---

```
make sc
```
This makes the executable for the semantic type checker itself. The resulting 
executable, `sato_concolic.exe`, can be used directly by running the command, 
`./sato_concolic.exe SOURCE_FILE`.

```
make sctest
```
This makes the full test suite for `sato_concolic.exe` (the type checker). The 
tests are found in the folder, `test/sato`. (Note that only test folders without 
`_` prefix are run in the testing process, and you can find out what errors are 
expected in the `.expect.s` file for each corresponding test).
