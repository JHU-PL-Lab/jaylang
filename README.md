Odefa
=====

This directory contains an implementation of the language discussed in the paper
"Implementing Higher-Order Demand-Driven Program Analysis".  This document contains
information about compiling and running the Odefa toploop as well as information
about the contents of this directory.

Building
--------

There are three major steps to set up a build environment from Odefa:

  1. Install OPAM and required libraries.
  2. Download and pin development dependencies.
  3. Build Odefa itself.

The subsections below walk through these processes.

### OPAM

1. Make sure you have [OCaml][ocaml] and [OPAM][opam] installed on the latest
   version:

        opam init               # necessary for freshly-installed OPAM instances
        eval `opam config env`  # if you do not have OPAM's environment configured
        opam update
        opam upgrade
        opam switch 4.02.3  # this may take a while

2. Install the dependencies:

        opam install oasis batteries menhir ounit ppx_deriving ocaml-monadic monadlib

   If your shell hashes binary locations, you may need to clear your hashes now.
   (In bash, `hash -r` does this.)

### Development Dependencies

Odefa depends upon libraries which tend to develop at the same time as it does
(but which are functionally independent and are designed to be used by other
projects).  To configure this environment, you must first clone the repository
for the dependency and then pin that repository as an OPAM package.

1. Install `jhupllib`:

        git clone https://github.com/JHU-PL-Lab/jhu-pl-lib.git ../jhu-pl-lib
        opam pin add jhupllib ../jhu-pl-lib

2. Install `pds-reachability`:

        git clone https://github.com/JHU-PL-Lab/pds-reachability.git ../pds-reachability
        opam pin add pds-reachability ../pds-reachability

You will need to re-run an appropriate `opam pin` command each time one of these
libraries is changed.

### Building Odefa

With the above configuration, it is now possible to build Odefa.

1. Generate configuration:

        oasis setup -setup-update dynamic

2. Configure:

        ./configure

3. Enable tests:

        ocaml setup.ml -configure --enable-tests

4. Build:

        make

5. Interact with the toploop (sample programs can be found at `test-sources/`):

        ./toploop.native

6. Run the tests:

        make test

Execution
---------

The Odefa toploop accepts command-line arguments.  Brief help for these
arguments may be obtained by passing `--help`.  Notable options are:

#### `--log=trace`

Enables quite verbose logging.

#### `--disable-inconsistency-check`

By default, the toploop checks programs for a form of inconsistency: lookup on
call sites should return only functions.  This causes several variable lookups
and is not suitable for benchmarking.  This flag disables the inconsistency
check.

#### `--select-context-stack=0ddpa`

Uses DDPA with a 0-level context stack (which is a monovariant analysis).  Any positive integer value is admitted here (e.g. `7ddpa`).

Other options exist, including a setting to produce diagrams of the incremental PDR graphs.

Benchmark
---------

To reproduce the benchmark, start by asserting that the code builds and runs as
expected:

    make && ./benchmark.native

Then, use `benchmark/generate-big-example.rb` to create big programs by copying
a benchmark over and over, renaming variables accordingly.

    ruby benchmark/generate-big-example.rb <odefa|scheme> <file> <times>

Where `odefa` is used for Odefa code and `scheme` is used for Scheme code from
the P4F benchmarks.

Example:

    ruby benchmark/generate-big-example.rb odefa benchmark-sources/sat.code 5

The experiments reported on the paper are documented as scripts named
`benchmark/benchmark-*.rb`.

Authors
-------

- Leandro Facchinetti <lfacchi2@jhu.edu>.
- Zachary Palmer <zachary.palmer@swarthmore.edu>.
- Scott F. Smith <scott@jhu.edu>.
- Clare Hanlon <chanlon1@swarthmore.edu>

The Johns Hopkins University
Swarthmore College


[ocaml]: https://ocaml.org/
[opam]: https://opam.ocaml.org/
[docker]: https://www.docker.com/
[docker-compose]: https://docs.docker.com/compose/
