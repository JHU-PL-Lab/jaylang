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
  * I might just like to completely ignore an abort
  * We need to carry on and hit any higher branches. so it does make sense to try to exit the branch
  * However, we need it to stop from hitting any lower branches because that's kind of the point of the exception. It's okay to hit parallel ones
  * So when we hit an abort, we can just jump back to exiting the current branch as an abort and let all of the formulas gather up, and then
    just continue on normally. We do need to throw the abort again though after collecting so that branches above don't get incorrect values
    and try to solve using them as if there was never an abort. But does that matter because we are only solver for higher branches.
  * If we hit an abort in an internal branch and let the interpreter carry on, then it could join up the input and carry on, hitting branches
    further down in the program

       upper0 
       /    \ 
    stuff     upper
      |      /      \
      |    normal  abort
      |    \        /
      |        lower
      |        /     \
      |     normal  normal
      |        \      /
      |          joins up
      \       /
        lower0

    We dont' want to solve for either branch of the "lower" after hitting abort. However, if we carry on after the abort, then
    we would hit either of the "normal" "lower" branches and have the other direction set as a target.
    For this reason, once we hit an abort, we want to exit the branch right then so that the "upper" finishes, and then throw
    abort again so that "upper0" finishes, and we can eventually solve for "upper->normal".
    What happens though if we do that and hit a "lower" branch where the solver then chooses inputs that hit abort again.
    This could maybe be a problem because we need to somehow mark that branch as permanently off limits.
      * TODO: use the branch store to mark "abort" and "reach max step" branches as off limits when solving. This needs to only
        be done when the solver might possible want to hit those branches.
          * Do my pick formulas require that a certain path be taken again to hit the lower? i.e. by my pick formulas, it seems
            like we force "upper normal" whenever trying to hit "lower normal" because we force parents to be true, and if ever
            we can't solve for a branch that way, we can try again when going through the other side of the higher up target.
          * this would grow exponentially, so maybe we don't do that. It's an interesting problem I need to bring up
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

Note the messages I DM'd to Shiwei about changes outside of `conclic/`