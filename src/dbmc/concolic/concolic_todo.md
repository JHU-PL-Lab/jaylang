## Concolic Evaluator TODOs

### Urgent

* Reduce max step to reduce load on solver
* Target all possible runtime instances of a branch under a large "or" to see if any are satisfiable.
  * Note: I will need to pass up the pick formulas to the session when I hit a branch
* Handle max step
  * Note: This might require the "analyze AST to determine dependencies" bullet in the "eventually" section
  * If I choose to bubble the max step, then I need to make sure the max step tracker doesn't recount the branches due to recursion
  * And fix the case where I sometimes have to press ^C to stop the evaluation and continue to the next one
    * My guess is this happens when the solver stuck, and any exception quits the solver as "unsatisfiable", but I have no good evidence for this--it's just a hunch.
* Customize unsatisfiability with a reason (e.g. unreachable/unsatisfiable because of abort, max step, both, or neither)
  * Note: Will need to store the formulas separately and add them to the solver carefully
* Make persistent formulas true only under their parents (I think this should fix the "double abort" test)

### Eventually

* Payloads on branch statuses
  * Track inputs and solvers that lead to hits/misses
* Analyze AST to determine dependencies
  * This is important so that some later branch that always gives abort doesn't prevent earlier branches from being hit
  * Also to disallow some parent from being hit if all children result in max step, or similar
* Allow multiple possible branch outcomes to account for nondeterminism
* Throw exception if we ever try to solve for the same branch with the same formulas, i.e. continue with misses until we reach a steady state
* Logging
* Use optional input AST branches to customize output
* Use variants instead of exceptions to make tracking a little nicer (it's currently quite hacky)

### Questions

* Can both sides of a branch immediately hit an abort? Is this allowed? e.g. `branch = cond ? ( t = abort ) : ( f = abort )`
* Do we only have int inputs?
* Should I have a "function is called" pick formula, or is this always implied by entering the branch that calls the function?
  * If I need it, then I can enter a function like any Parent in branch solver.

### Random thoughts

**Branch_tracker**

I think I could benefit from having a branch tracker that just handles all program-wide branch logic and relations.
* The session and concolic session will each have one
* It would hold the status store
* It can be merged with other branch trackers to accumulate the results from the concolic session
* Tracks the number of times max step has been hit in each AST branch
* Maps AST branches to all runtime instances
* Tracks parent dependencies so that formulas are implied correctly (e.g. abort formulas)
* Holds abort and max step formulas separately
* Unorganized thoughts:
  * It would handle some formulas because it needs to mark runtime branches as off-limits
    * Then it might as well take on other solver logic. And let branch_solver just do runtime branch tracking instead of solving logic
      * That's because we need to be adding some formulas sometimes to determine solvability
    * This motivates a name change for branch_solver

I think I need a branch_tracker. 
* It holds the status store
* Maps ast branches to runtime branches found so far
* tracks number of times max step has been hit in each ast branch
* All of the above are between-runs data
  * Should it track data during runtime? 
    * Maybe, yes, because it could be used during run to store up information as if we're currently at end of run (e.g. it could be a better way to track hits of branches)
    * But could also just store less data and use that to determine end-of-run data (e.g. no need to store aborts because that happens only once per run)
* It would handle some formulas because it needs to mark runtime branches as off-limits
  * Then it might as well take on other solver logic. And let branch_solver just do runtime branch tracking instead of solving logic
    * That's because we need to be adding some formulas sometimes to determine solvability
  * This motivates a name change for branch_solver
* A slight alternative is to be tracking hits and solves for runtime branches. Try to hit as many as possible and combine results. (I'm now not sure what I meant by this)
  * Don't let some unsatisfiables to affect other runtime branches
  * Just need to make sure that we can explore deeply into the recursion -- I don't want to be limited and think we have called a branch unsatisfiable when really we've just not recursed deep enough.
    * For this reason, I'm not sure how we prove whether something is actually unsatisfiable when there is a recursive function. If the recursive depth depends on the input, then my intuition tells me we can't be confident about satisfiability.
      * Similarly for if we reach max step. Can we make any conclusions at all? If the function is recursive, then there are circular dependencies between branches, so we can't just mark "lower" branches as unknown. This will require some deep analysis of the ast and program.
* Really I need to talk to scott about this

FAILING TEST CASES:
I need to think about how this evaluator/solver handles some branches that are satisfiable
at some depths of recursion but not at others. I think I am currently marking it off as
unsatisfiable if the branch can't be solved for at some depth. Can I come up with an example
that proves this wrong?
* See depth_dependent.jil

Is it ever possible that we keep increasing the size of recursion to get around the stack-dependent
targets? 
* E.g. we hit abort or max step at some recursive depth, so we mark it off limits, but then the solver
  still thinks it can hit the target with a slightly larger depth, so it does that, and so on...
* I think I can get around this by marking ALL depths found so far to be off limits.
  * I need to map ast branch to all runtime branches found.