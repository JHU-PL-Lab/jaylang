## Concolic Evaluator TODOs

### Eventually (after April deadline...)

* Solve for all targets along a path to not have to load the solver so many times.
* Scale max depth of tree with largest non-rec path
* Prune irrelevant expressions
  * Similar to value numbering, constant folding etc
  * Also very similar to (or frankly just the same as) checking "hittability" from each line of code

### Out of scope (for April deadline)

* Use mutable solver
  * Write interface that uses mutation but isn't smart. Then make new implementation over same interface that is smart, and that way we know what broke.
* Fuzz with answers and try for as small of inputs as possible (which would require many runs of the solver) only when the solver finishes sufficiently quickly
  * Therefore can get better runs, but we don't waste time on an overloaded solver
* Value numbering and dead store elimination on jil programs (and hence liveness?)

