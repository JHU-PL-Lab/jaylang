
TODO:
* Try to resolve all of the todos I have left in the code. There are LOTS of them.
  * The following bullet points, though, try to summarize some of them.
* Continue with misses until steady state
* Handle max step
  * And test it
* Track inputs and solvers that lead to hits/misses -- i.e. payloads on branch statuses
* Customize unsatisfiability with a reason (e.g. because of aborts, max steps, or both, or neither)
* Port over logging (and figure out logging in general)
* Use input AST branches to customize output -- this is for use in the type checker
* Use variants instead of exceptions to make tracking easier
* Confirm that we only want int input

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
* A slight alternative is to be tracking hits and solves for runtime branches. Try to hit as many as possible and combine results.
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