[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13393058.svg)](https://doi.org/10.5281/zenodo.13393058)

Jay Lang
=====

This is the codebase for the BlueJay language with its semantic-type-guided bug finder.
This code is developed by the JHU Programming Languages Lab.

This monorepo contains all tools built upon this language, which has recently be pruned
down to contain less code that is better-supported.

There is a snapshot that is the artifact for the paper **Semantic-Type-Guided Bug Finding** at SPLASH-OOPSLA 2024. Find it at the appropriate tag and release. Alternatively, checkout to the `oopsla-24` branch and follow the instructions there to recreate the results.

# Getting Started Guide

The repo is tested under MacOS, Ubuntu, and WSL on Windows.

After cloning the repository, make sure that the branch is switched to `main`.

```
git clone https://github.com/JHU-PL-Lab/jaylang.git
git checkout main
```

## Install from docker

```
docker build -t jaylang .
docker run -it jaylang
```

This may be very slow. To limit memory usage, the dependencies are installed using only four processes. It may be faster to install from source.

After installation, go to section `Run`.

## Install from source

Prepare and upgrade `apt` and the OCaml environment:

```
sudo apt upgrade opam
opam update
```

Install the local opam switch with the following command. Answer `yes` to all questions. It can take a while.
This command installs the dependencies of this project to opam. You are supposed 
to develop in this directory, as this switch is only active when you are in this directory.

```
opam switch create ./ 5.3.0
```

Note that this installs all of the necessary packages to build and run the project. However, you may want a few developer tools. We suggest you run the following to get developer tools:

```
opam user-setup install
opam install utop ocaml-lsp-server ocamlformat
```

Now you are ready to develop in the project.

## Run

### Concolic evaluator

```
make ceval
```

This makes the executable to run the concolic evaluator on a BlueJay file.
The resulting executable, `ceval.exe`, can be used directly by running the 
command

```
./ceval.exe <source_file>.bjy
```

Try the `--help` flag to learn more about arguments to the executable.

### Tests

```
make test-all
```

This makes the full tests suite for the concolic evaluator. The tests are found
in the directory `test/bjy`. With this, the well-typed tests are run, which are
expected to take a long time.

To run only the interesting, ill-typed tests, where the concolic evaluator is
expected to find an error in the test program, run only the fast tests:

```
make test-fast
```

Note this also runs any well-typed test that has no recursion and is expected to be
proven well-typed quickly, and those for which the incompleteness of type-splaying
and stubbing recursive types is not a problem and can be proven well-typed.

To check that the interpreter can run in all modes on all tests:

```
make test-interp
```

### Benchmarks

```
make cbenchmark
```

This makes the benchmarks for the concolic evaluator. The results are printed to stdout
in a LaTeX table format as is seen in Table 2 in Section 6.6 of the paper.

To run the benchmarks in other tables, go to the file `benchmark/concolic/cbenchmark.ml`
and change the directories run at the bottom of the file (several directories that might
be run are currently commented out).

All benchmarks are run in the test suite. Success of each run is not confirmed during
benchmarking. Instead, use `make test-fast` to see the results or run the file individually
with `ceval.exe`. Results are deterministic by default (modulo small variations in timeout)
and are therefore replicable.

## Coding in Bluejay

Write Bluejay code in `.bjy ` files. It's recommended to use the syntax highlighter found
in `bluejay-language/`. See the instructions there for how to install the highlighter as
a VS Code extension.

The features and syntax of Bluejay are informally but thoroughly documented in `docs/language/bluejay`.
It is recommended to look there to learn the language, and then refer to the many example programs
in the test suite.
