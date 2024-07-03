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

**Summary: I'm going crazy trying to get complex well-typed programs because of small bugs in translation (or somewhere else, idk).**

Case in point:
* subtyping5_well_typed
* dependent_type0
* ngrams
* parametric1
* parametric2
* subtyping2

**Note:**
* I thought I had the "number of times hit" heuristic working, and then it seems like it's not working
  * I have an indication that sometimes (rarely) I lose a target, and I haven't checked why yet

**My to-do list:**
* More big programs
  * This is hard when I can't be sure anything is well-typed due to the errors I've brought up
* Add filler to some simple programs (e.g. type_casing1) to hide the error a little deeper
* Benchmark without parsing?
* For ease of mind: get well-typed versions of **every** test so that we are sure they are correct

**Discussion topics for today:**
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
  * How about I get a standard--a number of nearly non-redundant test cases that should be tagged with such a feature
* Why is polymorphism4 an error? Is this a bug? It seems fine to me from a type inference perspective
* Structure question: does it make sense to move concolic out of dbmc directory?
* I would like to see what happens if we turn off all but one generator.
  * Currently, we can get caught up on earlier parts of code. If we have no generator, then this won't happen. Right?
  * If I can remove mutation, we can run in parallel easily
    * The current mutation is in `Riddler` and in setting a `Random` seed, and maybe the exceptions will be a problem?
* Should benchmarking include translation from bjy? It may be a little deceptive to not include this fundamental step in our type-checking.
* Anything else?


* Visitors on trees / adapters / google design patterns
* communicate max step vs tree depth differently
* benchmark with/without frontend (frontend = parse + translation, backend = concolic)

### Meeting summary

Immediate items to do:
* Determine tags for test programs (e.g. higher order functions, polymorphism, subtyping, etc.) and have at least 3-5 programs sufficiently covering each
* Copy classic OOP style programs, including, but not limited to, visitors on trees, adapter design pattern, flyweight pattern, etc.
* Write more big programs, archiving the type errors I make along the way as 
  * Note: my progress on these feels quite dependent on Earl fixing the wrap bugs
* Inflate the super simple programs with more code
* Add type casing tests (NOT type casting)
* Narrow down and share all the wrap bugs I've found as minimal examples
* Nudge Earl and ask for help on the bugs

Once those have been done:
* Benchmark the programs with and without the parse and translation included in the runtime
* Mock up several versions of tables to communicate benchmarking results -- how best do we show what the tests contain without oversharing?

Improvements that are even lower priority:
* Let the concolic evaluator run for at least 5 (or similar cutoff) seconds even if the path tree gets exhausted sooner than that
* Communicate path tree pruning vs interpreter step cutoff differently