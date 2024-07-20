Jay Lang
=====

(Update: July 5th, 2024)

This is the codebase for languages BlueJay, Jay and Jay Intermediate Language 
(JayIL), as well as the language's semantic-type-guided type checker. This code 
is developed by JHU Programming Languages Lab. It's a pipeline of functional 
languages that fits for research at each layers.

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
make sctest
```
This makes the full test suite for `sato_concolic.exe` (the type checker). The 
tests are found in the folder, `test/sato`. (Note that only test folders without 
`_` prefix are run in the testing process, and you can find out what errors are 
expected in the `.expect.s` file for each corresponding test).

Please note that some tests might appear to be failing. This is not a cause for
concern, because these failures are results of multiple type errors being present
in the code, and our artifact currently is not good at reporting all such errors. 
Since the runs can be non-deterministic, sometimes the reported error is different
from the one specified in the `.expect.s` file, and manual inspection of the code
should verify that the error reported indeed is correct.

```
make cbenchmark
```
This makes the benchmark for `sato_concolic.exe`. The results can be found in 
`benchmark/concolic/result/<specific_benchmark_run>/0table.txt`, and they correspond
to the Bluejay entries seen in Table 1 in Section 5.5 of the paper.

To run the benchmarks in Table 2, go to the file, `benchmark/concolic/cbenchmark.ml`, 
and change the `config_path` to `ref "benchmark/concolic/config.s"`, then run 
`make cbenchmark` again. The newly generated `0table.txt` in the `result` folder 
will contain the corresponding results.
