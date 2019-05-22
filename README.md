Odefa
=====

Artifact for the paper **Higher-Order Demand-Driven Symbolic Evaluation**.

| | | |
|-|-|-|
| Leandro Facchinetti | <lfacchi2@jhu.edu> | The Johns Hopkins University |
| Zachary Palmer | <zachary.palmer@swarthmore.edu> | Swarthmore College |
| Scott F. Smith | <scott@jhu.edu> | The Johns Hopkins University |

**TODO: update this document with OPAM 2.0 build instructions, etc.**

Developer Setup
---------------

Odefa depends on libraries which tend to develop at the same time as it does, but which are functionally independent and are designed to be used by other projects. Configure these libraries for local development by pinning them:

1. `jhupllib`:

   ```console
   $ git clone https://github.com/JHU-PL-Lab/jhu-pl-lib.git ../jhu-pl-lib
   $ opam install ../jhu-pl-lib
   ```

2. `pds-reachability`:

   ```console
   $ git clone https://github.com/JHU-PL-Lab/pds-reachability.git ../pds-reachability
   $ opam install ../pds-reachability
   ```

When these libraries change, run

   ```console
   $ opam upgrade jhupllib pds-reachability
   ```
