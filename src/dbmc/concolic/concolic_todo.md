## Concolic Evaluator TODOs

### Urgent

* Disallow inputs that have already been used
  * I think we can just add global formulas because even if this makes some branch appear unsatisfiable, that branch has already been hit
* Fix the case where I sometimes have to press ^C to stop the evaluation and continue to the next one
  * My guess is this happens when the solver stuck, and any exception quits the solver as "unsatisfiable", but I have no good evidence for this--it's just a hunch.
* Customize unsatisfiability with a reason (e.g. unreachable/unsatisfiable because of abort, max step, both, or neither)
  * Note: Will need to store the formulas separately and add them to the solver carefully
* Use `Unkown` result from solver and add to branch statuses
  * I think this is in case of timeout, which I need to be ready to handle.

### Eventually

* Convert Earl's jay files to jayil and make test cases
* Quit solving after missing too many times (e.g. depth_dependent2_tail_rec tried a ton of inputs that got nowhere but didn't hit max step) and say "unknown"
* Analyze AST to determine dependencies
  * This is important so that some later branch that always gives abort doesn't prevent earlier branches from being hit
  * SIMILAR: selectively add formulas that are encountered on the path to the target, and discard other formulas
    * Could give each run a new formula tracker and then save a snapshot of the tracker (exiting up to global) when hitting each target. Then merge all of these before solving.
* Allow multiple expected test results
* Throw exception if we ever try to solve for the same branch with the same formulas, i.e. continue with misses until we reach a steady state
* Logging
* Use optional input AST branches to customize output
* Efficiency optimizations!
  * Regarding pick formulas, especially. Can I make them shorter or not duplicate? Maybe mark off some runtime branch as already having a pick formula? Could use a hashtbl and mutability for extra speed
  * Shorten lookup keys to hash, if possible. Might not be beneficial though if the cost of hashing is greater than the cost of comparing a few times later. I think that since I have maps and sets with these lookup keys, I do want them shorter

### Random thoughts

None right now
