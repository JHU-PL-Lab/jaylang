## Concolic Evaluator TODOs

### Eventually (after April deadline...)

* Solve for all targets along a path to not have to load the solver so many times.
* Prune irrelevant expressions
  * Similar to value numbering, constant folding etc
  * Also very similar to (or frankly just the same as) checking "hittability" from each line of code

### Out of scope (for April deadline)

* Use mutable solver
  * Write interface that uses mutation but isn't smart. Then make new implementation over same interface that is smart, and that way we know what broke.
* Fuzz with answers and try for as small of inputs as possible (which would require many runs of the solver) only when the solver finishes sufficiently quickly
  * Therefore can get better runs, but we don't waste time on an overloaded solver
* Value numbering and dead store elimination on jil programs (and hence liveness?)




## Ignore all above

### 25 June 2024

Tests I added:
* Polymorphism:
  * Simple higher order polymorphic function application
  * Calling with record supertype
  * Subtyping of intersection types
  * Subtyping of functions


Need to add:
* Duck typing records (I'm unsure how to do this any better than just regular use of records)
* type casing leads to violations of parametricity
* more improper use of higher order functions
* Longer test cases (working on ngrams right now)

Other things to add:
* Discussion on test cases (e.g. clear up what tests like `polymorphic_map` cover)
  * Better: loc in tests, features in tests, etc. added to table

Other things to do:
* Tweak heuristics
  * Let's target branches closer to uncovered code in *any* path. e.g. simply track number of times each line has been hit, and prioritize lesser-hit lines, or distance to lesser-hit lines (using number of conditionals as distance metric). (this should roughly handle prioritizing non-recursions).
  * Could also track number of solves on each AST branch and try to even that out.

Questions I have:
* Why is it ill-typed to pattern match on 'a?
* What does it really mean to have a subtyping test?
  * It is not noteworthy to show that we fail to find an error in a well-typed program with subtyping
  * It is therefore more noteworthy to find errors in an ill-typed program, where maybe subtyping seems closely related, but "not a subtype" is very much like "ill-typed". See my examples for attempts at this.