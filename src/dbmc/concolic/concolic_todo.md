I need to accept a list of AST branches and try to hit them.
* The best I can do is try randomly at first, so I think I keep the same flow
* If a parent is unsatisfiable, then it is unreachable
  * Instead of analyzing parents, I could just track if either side is reached to determine reachability vs unsatisfiability
* If satisfied, then track the input that got there
* Also track the previous session because I might want to return to previous targets
  * If our target is not satisfiable, then go to previously assigned target
  * We might like a target stack, or maybe a target priority queue
    * Add targets according to time, and if they are a duplicate, then overwrite the time
    * Prioritize by most recent
* Use variants so we can finish the session in case we need to return to it with a different target
  * We need to clean up after exiting a branch, so if an exception is thrown, then we can't clean up
  * It's more annoying for me to catch an exception than to deal with variants
  * It's also a good chance to just make a session monad.

Other:
* Use a logger instead of printing
  * Or set verbosity in session
* Handle reaching max step -- what should happen with formulas?
  * I think we want to be able to revert to the previous session with a new target
  * Can also jsut try again with different input
  * Or can just trust the most recent targets... doesn't feel as safe.

I need to handle exceptions:
* Max step:
* Abort:
* Assert: these are always considered true by the interpreter


I need to be able to back out of an unsatisfiable branch.
* If target is unsatisfiable, then go up to previous target and solve for it
* Recurse until no targets left (so they're unreachable) or hit one
  * If we hit one, then we can try to solve for that, but we had already cleared
    our stores because that made a new session.
  * Is that okay? Let's discuss it. We had tried to hit the deepest possible branches
    for which we had seen the other side, but they were deemed unsatisfiable. Below those
    might be unreachable branches. After deeming a branch unsatisfiable, we have necessarily
    hit the other side, so ALL branches below that condition/clause are exhausted (they are
    either hit, unsatisfiable, unreachable, or reached max step).
    So now we've found a branch that is satisfiable, and it's the next deepest one, so we're
    good to discard the old session and old targets because when we go to hit this new branch,
    we'll encounter all the higher branches above and build new formulas. These formulas seem
    sufficient to solve for all branches because we never "don't have enough information". But
    I'm not sure the proof for that.
    I do need to handle when the max step is reached. When this happens, we need to back up to
    the previous session and try the latest target there. I don't think we can safely assume
    that we just try the next target from the failing session because it might not have gotten as far.