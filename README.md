Jay Lang
=====

Please cite this software as:

```bibtex
@software{Artifact24Zenodo,
  author       = {Kelvin Qian and Brandon Stride and Scott Smith and Shiwei Weng and Ke Wu},
  doi          = {10.5281/zenodo.13388361},
  organization = {Zenodo},
  month        = aug,
  title        = {Software Artifact for Semantic-Type-Guided Bug Finding},
  url          = {https://doi.org/10.5281/zenodo.13388361},
  version      = {1.0.0},
  year         = {2024}
}
```

This is the codebase for languages BlueJay, Jay and Jay Intermediate Language 
(JayIL), as well as the language's semantic-type-guided type checker. This code 
is developed by JHU Programming Languages Lab. It's a pipeline of functional 
languages that fits for research at each layer.

This monorepo contains all tools built upon these languages.

This snapshot is for the artifact for the paper **Semantic-Type-Guided Bug Finding**.

## Getting Started Guide

The repo is tested under MacOS, Ubuntu, and WSL on Windows.

After cloning the repository, make sure that the branch is switched to `oopsla-24`.

```
# git clone https://github.com/JHU-PL-Lab/jaylang.git
git checkout oopsla-24
```

### Install from docker

```
docker build -t jaylang .
docker run -it jaylang

# go to Section run
make sc

```

### Install from source

Prepare and upgrade `apt` and the ocaml environment
```
sudo apt upgrade opam
opam update
```

Install local opam switch. Answer `yes` to questions. It can take a while.
This command installs the dependencies of this project to opam. You are supposed 
to develop in this directory.


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

### Run

```
make sc
```
This makes the executable for the semantic type checker itself. The resulting 
executable, `sato_concolic.exe`, can be used directly by running the command, 
`./sato_concolic.exe SOURCE_FILE`.

```
make test-concolic
```
This makes the full test suite for `sato_concolic.exe` (the type checker). The 
tests are found in the folder, `test/concolic/bjy/`.

```
make cbenchmark
```
This makes the benchmark for `sato_concolic.exe`. The results are printed to stdout
in a LaTeX table format as is seen in Table 2 in Section 6.6 of the paper.

To run the benchmarks in other tables, go to the file `benchmark/concolic/cbenchmark.ml`
and change the directories run at the bottom of the file (several directories that might
be run are currently commented out).

All benchmarks are run in the test suite. Success of each run is not confirmed during
benchmarking. Instead, use `make test-concolic` to see the results.
