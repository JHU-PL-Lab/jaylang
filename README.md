[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13393058.svg)](https://doi.org/10.5281/zenodo.13393058)

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

This is the codebase for the BlueJay language with its semantic-type-guided bug finder.
This code is developed by the JHU Programming Languages Lab.

This monorepo contains all tools built upon this language, which has recently be pruned
down to contain less code that is better-supported.

There is a snapshot that is the artifact for the paper **Semantic-Type-Guided Bug Finding** at SPLASH-OOPSLA 2024. Find it at the appropriate tag and release.

# Getting Started Guide

The repo is tested under MacOS, Ubuntu, and WSL on Windows.

After cloning the repository, make sure that the branch is switch to `main`, as it is
the most well-supported.

```
git clone https://github.com/JHU-PL-Lab/jaylang.git
git checkout main
```

## Install from docker

```
docker build -t jaylang .
docker run -it jaylang
```

Then go to section `Run`.

## Install from source

Prepare and upgrade `apt` and the OCaml environment

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
The resulting executable `ceval.exe`, can be used directly by running the 
command

```
./ceval.exe <source_file>.bjy
```

Optional arguments for this executable can be found in the source code at
`src/bin/ceval.ml`.


### Tests

```
make test-concolic
```

This makes the full tests suite for the concolic evaluator. The tests are found
in the directory `test/bjy`. By default, the well-typed tests are skipped because
they are expected to run for a long time.

### Benchmarks

```
make cbenchmark
```

This makes the benchmark for the concolic evaluator. The results are printed to stdout
in a LaTeX table format as is seen in Table 2 in Section 6.6 of the paper.

To run the benchmarks in other tables, go to the file `benchmark/concolic/cbenchmark.ml`
and change the directories run at the bottom of the file (several directories that might
be run are currently commented out).

All benchmarks are run in the test suite. Success of each run is not confirmed during
benchmarking. Instead, use `make test-concolic` to see the results.
