Jay Lang
=====

(Update: July 5th, 2024)

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
This command installs the dependencies of this project to opam. You are supposed to develop in this directory.

If you also want to install this project to opam and develop external project, remove `--deps-only` from the command.

```
opam switch create ./ 4.14.1 --deps-only --description=jay-dev

# Sometimes opam fails due to parallel installation of packages. To fix it
opam install . --deps-only
```

After that, you can install the develop tools
```
opam user-setup install
opam install utop ocaml-lsp-server ocamlformat
```

Now you should be able to run the project.


Run
---

```
make dj
make dbmc
make sato
```
