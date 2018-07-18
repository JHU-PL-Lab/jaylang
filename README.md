Odefa
=====

Artifact for the paper **Higher-Order Demand-Driven Program Analysis**.

| | | |
|-|-|-|
| Leandro Facchinetti | <lfacchi2@jhu.edu> | The Johns Hopkins University |
| Zachary Palmer | <zachary.palmer@swarthmore.edu> | Swarthmore College |
| Scott F. Smith | <scott@jhu.edu> | The Johns Hopkins University |

Build
-----

1. Install [OCaml](https://ocaml.org/) and [OPAM](https://opam.ocaml.org/).

   <details>
   <summary>Windows Instructions</summary>

   Install [OCaml for Windows](http://fdopen.github.io/opam-repository-mingw/installation/), which includes the Cygwin shell with OCaml and OPAM preinstalled.

   </details>

2. Initialize OPAM:

   ```console
   $ opam init
   $ eval `opam config env`
   ```

3. Update & upgrade:

   ```console
   $ opam update
   $ opam upgrade
   ```

4. Switch to the appropriate compiler version:

   ```console
   $ opam switch 4.06.1
   $ eval `opam config env`
   ```

   <details>
   <summary>Windows Instructions</summary>

   Either

   ```console
   $ opam switch 4.06.1+mingw64
   $ eval `opam config env`
   ```

   or

   ```console
   $ opam switch 4.06.1+mingw32
   $ eval `opam config env`
   ```

   depending on the system.

   </details>

5. Install the dependencies:

   ```console
   $ opam install oasis \
                  batteries \
                  menhir \
                  ounit \
                  ppx_deriving \
                  ppx_deriving_yojson \
                  ocaml-monadic \
                  monadlib \
                  jhupllib \
                  pds-reachability
   ```

6. If your shell hashes binary locations, you may need to clear your hashes, for example (in Bash):

   ```console
   $ hash -r
   ```

7. Generate configuration:

   ```console
   $ oasis setup -setup-update dynamic
   ```

8. Configure:

   ```console
   $ ./configure
   ```

9. Enable tests:

   ```console
   $ ocaml setup.ml -configure --enable-tests
   ```

10. Build:

    ```console
    $ make
    ```

11. Interact with the toploop (find sample programs at `test-sources/`):

    ```console
    $ ./toploop.native
    ```

12. Run the tests:

    ```console
    $ make test
    ```

`toploop.native` Command-Line Arguments
---------------------------------------

- `--log=trace`: Enable verbose logging.
- `--disable-inconsistency-check`: By default, the toploop checks programs for inconsistencies. For example, it checks that only functions appear in the operator position of a function call, and that only records appear in the subject position of a record projection. This inconsistency check forces variable lookups that interfere with benchmarking, and this flag disables it.
- `--select-context-stack=0ddpa`: Uses DDPA with a 0-level context stack (which is a monovariant analysis). Any positive integer value is admitted here (e.g. `7ddpa`).

Run the following for extended help (including options to produce diagrams of the incremental PDR graphs):

```console
$ ./toploop.native --help
```

Developer Setup
---------------

Odefa depends on libraries which tend to develop at the same time as it does, but which are functionally independent and are designed to be used by other projects. Configure these libraries for local development by pinning them:

1. `jhupllib`:

   ```console
   $ git clone https://github.com/JHU-PL-Lab/jhu-pl-lib.git ../jhu-pl-lib
   $ opam pin add jhupllib ../jhu-pl-lib
   ```

2. `pds-reachability`:

   ```console
   $ git clone https://github.com/JHU-PL-Lab/pds-reachability.git ../pds-reachability
   $ opam pin add pds-reachability ../pds-reachability
   ```

Re-run `opam pin` when these libraries change.
