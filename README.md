Odefa
=====

Artifact for the paper **Higher-Order Demand-Driven Program Analysis**.

Build
-----

1. Install [OCaml](https://ocaml.org/) and [OPAM](https://opam.ocaml.org/).

   <details><summary>Windows Instructions</summary>
   Install [OCaml for Windows](http://fdopen.github.io/opam-repository-mingw/installation/), which includes the Cygwin shell with OCaml and OPAM preinstalled.
   </details>

2. installed on the latest version:

   ```console
   $ opam init               # necessary for freshly-installed OPAM instances
   $ eval `opam config env`  # if you do not have OPAM's environment configured
   $ opam update
   $ opam upgrade
   $ opam switch 4.02.3      # this may take a while
   ```

   On Windows,  The `opam switch` line will need to be changed to either

   ```console
   $ opam switch 4.02.3+mingw64
   ```

   or

   ```console
   $ opam switch 4.02.3+mingw32
   ```

   depending on the system.

Building
--------

There are three major steps to set up a build environment from Odefa:

1. Install OPAM and required libraries.
2. Download and pin development dependencies.
3. Build Odefa itself.

The subsections below walk through these processes.

### OPAM



2. Install the dependencies:

   ```console
   $ opam install oasis batteries menhir ounit ppx_deriving ppx_deriving_yojson ocaml-monadic monadlib jhupllib pds-reachability
   ```

   If your shell hashes binary locations, you may need to clear your hashes. (In bash, `hash -r` does this.)

### Development Dependencies

Odefa depends on libraries which tend to develop at the same time as it does (but which are functionally independent and are designed to be used by other projects). To configure this environment, you must first clone the repository for the dependency and then pin that repository as an OPAM package.

1. Install `jhupllib`:

   ```console
   $ git clone https://github.com/JHU-PL-Lab/jhu-pl-lib.git ../jhu-pl-lib
   $ opam pin add jhupllib ../jhu-pl-lib
   ```

2. Install `pds-reachability`:

   ```console
   $ git clone https://github.com/JHU-PL-Lab/pds-reachability.git ../pds-reachability
   $ opam pin add pds-reachability ../pds-reachability
   ```

You will need to re-run an appropriate `opam pin` command each time one of these libraries is changed.

### Building Odefa

With the above configuration, it is now possible to build Odefa.

1. Generate configuration:

   ```console
   $ oasis setup -setup-update dynamic
   ```

2. Configure:

   ```console
   $ ./configure
   ```

3. Enable tests:

   ```console
   $ ocaml setup.ml -configure --enable-tests
   ```

4. Build:

   ```console
   $ make
   ```

5. Interact with the toploop (sample programs can be found at `test-sources/`):

   ```console
   $ ./toploop.native
   ```

6. Run the tests:

   ```console
   $ make test
   ```

Execution
---------

The Odefa toploop accepts command-line arguments.  Brief help for these arguments may be obtained by passing `--help`.  Notable options are:

#### `--log=trace`

Enables quite verbose logging.

#### `--disable-inconsistency-check`

By default, the toploop checks programs for a form of inconsistency: lookup on call sites should return only functions.  This causes several variable lookups and is not suitable for benchmarking.  This flag disables the inconsistency check.

#### `--select-context-stack=0ddpa`

Uses DDPA with a 0-level context stack (which is a monovariant analysis). Any positive integer value is admitted here (e.g. `7ddpa`).

Other options exist, including a setting to produce diagrams of the incremental PDR graphs.

Authors
-------

| | | |
|-|-|-|
| Leandro Facchinetti | <lfacchi2@jhu.edu> | The Johns Hopkins University |
| Zachary Palmer | <zachary.palmer@swarthmore.edu> | Swarthmore College |
| Scott F. Smith | <scott@jhu.edu> | The Johns Hopkins University |
