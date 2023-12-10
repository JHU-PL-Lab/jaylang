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
* 