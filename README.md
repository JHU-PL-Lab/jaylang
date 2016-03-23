Odefa
=====

This directory contains an implementation of the language discussed in the paper
"Implementing Higher-Order Demand-Driven Program Analysis".  This document contains
information about compiling and running the Odefa toploop as well as information
about the contents of this directory.

Compilation
-----------

There are three different ways to setup and run Odefa.

### OPAM

1. Make sure you have [OCaml][ocaml] and [OPAM][opam] installed on the latest
   version:

    $ opam init  # necessary for freshly-installed OPAM instances
    $ eval `opam config env`  # if you do not have OPAM's environment configured
    $ opam update
    $ opam upgrade
    $ opam switch 4.02.2  # this may take a while

2. Install the dependencies:

    $ opam install oasis batteries menhir ounit ppx_deriving ocaml-monadic monadlib
   
   If your shell hashes binary locations, you may need to clear your hashes now.
   (In bash, `hash -r` does this.)

3. Generate configuration:

    $ oasis setup -setup-update dynamic

4. Configure:

    $ ./configure

5. Enable tests:

    $ ocaml setup.ml -configure --enable-tests

6. Build:

    $ make

7. Interact with the toploop (sample programs can be found at `test-sources/`):

    $ ./toploop.native

8. Run the tests:

    $ make test

### Docker

Having [Docker][docker] and [Docker Compose][docker-compose] installed, run:

    $ docker-compose run --rm odefa

This builds and runs the tests.

In order to interact with the toploop (sample programs can be found at
`test-sources/`):

    $ docker-compose run --rm odefa './toploop.native'
    
### Vagrant

Having [VirtualBox][virtual-box] and [Vagrant][vagrant] installed, run:

    $ vagrant up && vagrant exec docker-compose run --rm odefa

This builds and runs the tests.

In order to interact with the toploop (sample programs can be found at
`test-sources/`):

    $ vagrant exec docker-compose run --rm odefa 'toploop.native'
    
Execution
---------

The Odefa toploop accepts command-line arguments.  Brief help for these
arguments may be obtained by passing `--help`.  Notable options are:

### `--log=trace`

Enables quite verbose logging.

### `--disable-inconsistency-check`

By default, the toploop checks programs for a form of inconsistency: lookup on
call sites should return only functions.  This causes several variable lookups
and is not suitable for benchmarking.  This flag disables the inconsistency
check.

### `--select-context-stack=0ddpa`

Uses DDPA with a 0-level context stack (which is a monovariant analysis).  Any positive integer value is admitted here (e.g. `7ddpa`).

### `--ddpa-logging=result`

Generates a Graphviz DOT file of the DDPA CFG at the end of analysis.  These files may be processed by Graphviz as follows:

    dot -Tpdf <_toploop_DDPA_final.dot >_toploop_DDPA_final.pdf

Other options exist, including a setting to produce diagrams of the incremental PDR graphs.
                    
Authors
-------

- Leandro Facchinetti <lfacchi2@jhu.edu>.
- Zachary Palmer <zachary.palmer@jhu.edu>.
- Scott F. Smith <scott@jhu.edu>.

The Johns Hopkins University


[ocaml]: https://ocaml.org/
[opam]: https://opam.ocaml.org/
[docker]: https://www.docker.com/
[docker-compose]: https://docs.docker.com/compose/
