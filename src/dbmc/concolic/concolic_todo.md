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

TODO:
* turn off generators for all but one function, and test just that function (so wrap can still catch errors in uses of other functions (i.e. we don't just turn off types on other functions)), and maybe test all functions this way in parallel (requires removing mutation from riddler)
* trim nodes where there are no unsolved targets underneath to save memory
  * can recursively send child to something like `Trimmed`, and trim only if neither child is unsolved.
* Remove option to not quit on abort, then can save memory by storing fewer input lists
  * Can then possibly discard branch_info, but we want to adapt branch_info to help with targeting unhit lines

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
* e.g. colored point is used in function and passed in, so it all works if dynamically typed (duck typing), but parameter has less specific type

Other things to add:
* Discussion on test cases (e.g. clear up what tests like `polymorphic_map` cover)
  * Better: loc in tests, features in tests, etc. added to table

Other things to do:
* Tweak heuristics
  * Let's target branches closer to uncovered code in *any* path. e.g. simply track number of times each line has been hit, and prioritize lesser-hit lines, or distance to lesser-hit lines (using number of conditionals as distance metric). (this should roughly handle prioritizing non-recursions).
  * Could also track number of solves on each AST branch and try to even that out.

Questions I have:
* What does it really mean to have a subtyping test?
  * It is not noteworthy to show that we fail to find an error in a well-typed program with subtyping
  * It is therefore more noteworthy to find errors in an ill-typed program, where maybe subtyping seems closely related, but "not a subtype" is very much like "ill-typed". See my examples for attempts at this.



# 2 July 2024

Summary: I'm going crazy trying to get complex well-typed programs because of small bugs in translation.

Case in point:
* subtyping5_well_typed
* dependent_type0
* ngrams
* parametric1
* parametric2
* subtyping2

Other items:
* I thought I had the "number of times hit" heuristic working, and then it seems like it's not working
  * I have an indication that sometimes (rarely) I lose a target, and I haven't checked why yet

Items I know I need to do:
* More big programs
* Add filler to some simple programs (e.g. type_casing1) to hide the error a little deeper
* Fully integrate Earl's variant/union change from a while ago (i.e. just change union types in current tests)
* Benchmark without parsing

I'd like advice on this:
* OOP-style tests are very easy and feel covered by any tests using records, so I don't have many.
  * Do we want these easy tests singled out, or is an annotation on all the more complex tests enough to indicate we have this covered?
* A reviewer asked for tests where "type casing leads to violations of parametricity"
  * Is this sufficiently covered by cases where a variable should be polymorphic, but it is locked in by its usage. e.g. type_casing1, polymorphism2
* They also asked for more focus on higher order functions.
  * Do my tests cover it well enough?
    * polymorphism2 -- higher order function used with wrong arguments
    * polymorphism3 -- higher order function passed through a recursive call where the polymorphic types don't line up
    * parametric2/3 -- continuation monad with bind
    * subtyping5 -- higher order function looks like we have subtypes, until you think about it, and it's really not
    * ... and other natural uses with no deliberate focus on erroring with the higher order functions deliberately