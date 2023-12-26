
TODO:
* Docs
* Continue with misses until steady state
* Track inputs and solvers that lead to hits/misses
* Nice output
* Replicable tests
* Port over logging (and figure out logging in general)
* Use input AST branches to customize output
* Use variants instead of exceptions to make tracking easier
* Confirm that we only want int input

## Summary

Things that are working:
* The concolic evaluator continues until it has nothing left to do.
  * It handles aborts (mostly) smoothly
  * It marks status of branches clearly
  * In almost all test cases, it works flawlessly.

Things that aren't working:
* Sometimes it doesn't solve correctly. This is a fundamental logic issue.
* "Max step" is unhandled
* Sometimes aborts lead to infinite loops

Things that I want to do:
* Use a logger instead of printing
* Refactor sessions to make it less "hacky"
* Accept a list of AST branches that are of interest, and return a nice output.
* Use variants instead of exceptions to make the tracking just a little easier.
* Record input that let us hit a branch (this is easy...)

## In depth

### Incorrect solving

* I think this has to do with how parents and dependencies are handled. It seems downstream from too many implications and not enough statements.
  * It might also be dependent on binary operations. See the difference between `hit_abort_target2.jil` and `hit_abort_target.jil`
* Here is the current logic:
  * We assume there is some parent branch. This is the global program scope or the condition of the immediate if-statement.
  * When the parent branch is exited, the formulas are accumulated and "implied" by the parent. Any formulas that depend on the parent in any way are noted as such so that whenever they are used, they are also implied by the parent.
    * Note that when under the immediate parent, there are no formulas implied by that parent yet. It all happens when the parent is exited.
  * Any formula that depends on other formulas gain their parents. Thus, they get implied by their parents.
    * This means that if their parents are not satisfied, then those formulas never get implied. I believe this is a flaw, but it is not the only flaw.

Here is an example of the logic.
Consider this example:
```jayil
x = input;
zero = 0;
first_condition = zero < x;
first_branch = first_condition ? (
  first_branch_false_return = 5
) : (
  first_branch_true_return = 100
);

second_condition = first_branch < x;
second_branch = second_condition ? (
  result_true = 42
) : (
  result_false = 84
)
```

In this program, suppose the input is `4`. Then `first_branch` takes the true direction and gains the value `5`. Then the clause `second_condition` depends on `first_branch`, whose value depends on `first_condition = true`, so the entire formula `second_condition = first_branch < x` is implied by `first_condition = true`. This isn't correct because `second_condition = first_branch < x` is a formula no matter what, but the *values* of each variable in the formula are dependent on `first_condition = true`.

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

    If we found an abort and did NOT hit the target branch, then we didn't make it far enough and need to back up while
    updated the branch store and adding any permanent formulas. The permanent formulas must be of the implied format so that
    they aren't unnecessarily limiting the solver. If we found an abort and DID hit the target branch, then we can just
    continue to the next session but still add permanent formulas.

    I've started to get a little bit (actually quite a lot) hacky by coupling persistent and temporary aspects of the
    sessions/runs, and how I have to back up to previous sessions but carry over some info. It's tough to merge some things
    but overwrite others, and then return some state as well to tell what happened.
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



```
# output when using [x_plus_one], and no implied formulas
Starting concolic execution...
------------------------------
Running program...

Branch Information:
first_branch: True=Unhit; False=Unhit
second_branch: True=Unhit; False=Unhit
zero_branch: True=Unhit; False=Unhit

Target branch: None
Feed -5 to x
ADD GLOBAL FORMULA (= large_ (Int 100))
ADD GLOBAL FORMULA (let ((a!1 (= zero_condition_ (Bool (< (i x_) (i large_))))))
  (and a!1 ((_ is Int) x_) ((_ is Int) large_)))
Hitting: zero_branch: true
ADD GLOBAL FORMULA (=> (= zero_condition_ (Bool true))
    (and (= zero_branch_ |zero_branch_true_-(zero_branch,$tt);|)
         (= |zero_branch_true_-(zero_branch,$tt);| (Bool true))))
ADD PICK FORMULA: (=> P_zero_branch_ and)
ADD GLOBAL FORMULA (=> P_zero_branch_ and)
ADD GLOBAL FORMULA (= first_condition_ zero_branch_)
Hitting: first_branch: true
ADD GLOBAL FORMULA (=> (= first_condition_ (Bool true))
    (and (= first_branch_ |first_branch_false_return_-(first_branch,$tt);|)
         (= |first_branch_false_return_-(first_branch,$tt);| (Int 5))))
ADD PICK FORMULA: (=> P_first_branch_ and)
ADD GLOBAL FORMULA (=> P_first_branch_ and)
ADD GLOBAL FORMULA (= one_ (Int 1))
ADD GLOBAL FORMULA (let ((a!1 (= x_plus_one_ (Int (+ (i x_) (i one_))))))
  (and a!1 ((_ is Int) x_) ((_ is Int) one_)))
ADD GLOBAL FORMULA (let ((a!1 (= second_condition_ (Bool (< (i first_branch_) (i x_plus_one_))))))
  (and a!1 ((_ is Int) first_branch_) ((_ is Int) x_plus_one_)))
Hitting: second_branch: false
ADD GLOBAL FORMULA (=> (= second_condition_ (Bool false))
    (and (= second_branch_ |result_false_-(second_branch,$ff);|)
         (= |result_false_-(second_branch,$ff);| (Int 84))))
ADD PICK FORMULA: (=> P_second_branch_ and)
ADD GLOBAL FORMULA (=> P_second_branch_ and)
Evaluated to: 84
------------------------------
Running program...

Branch Information:
first_branch: True=Hit; False=Unhit
second_branch: True=Unhit; False=Hit
zero_branch: True=Hit; False=Unhit

Target branch: second_branch_; condition: second_condition_ = true
Solving for target branch:
Branch to pick: P_second_branch_
Branch condition: (= second_condition_ (Bool true))
Feed 21338 to x
ADD GLOBAL FORMULA (= large_ (Int 100))
ADD GLOBAL FORMULA (let ((a!1 (= zero_condition_ (Bool (< (i x_) (i large_))))))
  (and a!1 ((_ is Int) x_) ((_ is Int) large_)))
Hitting: zero_branch: false
ADD GLOBAL FORMULA (=> (= zero_condition_ (Bool false))
    (and (= zero_branch_ |zero_branch_false_-(zero_branch,$ff);|)
         (= |zero_branch_false_-(zero_branch,$ff);| (Bool false))))
ADD PICK FORMULA: (=> P_zero_branch_ and)
ADD GLOBAL FORMULA (=> P_zero_branch_ and)
ADD GLOBAL FORMULA (= first_condition_ zero_branch_)
Hitting: first_branch: false
Running next iteration of concolic after abort


# Then it repeats
```

```
# output when using [x_plus_one], and no implied formulas
Starting concolic execution...
------------------------------
Running program...

Branch Information:
first_branch: True=Unhit; False=Unhit
second_branch: True=Unhit; False=Unhit
zero_branch: True=Unhit; False=Unhit

Target branch: None
Feed 0 to x
ADD GLOBAL FORMULA (= large_ (Int 100))
ADD GLOBAL FORMULA (let ((a!1 (= zero_condition_ (Bool (< (i x_) (i large_))))))
  (and a!1 ((_ is Int) x_) ((_ is Int) large_)))
Hitting: zero_branch: true
ADD GLOBAL FORMULA (=> (= zero_condition_ (Bool true))
    (and (= zero_branch_ |zero_branch_true_-(zero_branch,$tt);|)
         (= |zero_branch_true_-(zero_branch,$tt);| (Bool true))))
ADD PICK FORMULA: (=> P_zero_branch_ and)
ADD GLOBAL FORMULA (=> P_zero_branch_ and)
ADD GLOBAL FORMULA (=> (and (= zero_condition_ (Bool true))) (= first_condition_ zero_branch_))
Hitting: first_branch: true
ADD GLOBAL FORMULA (let ((a!1 (=> (= first_condition_ (Bool true))
               (and (= first_branch_
                       |first_branch_false_return_-(first_branch,$tt);|)
                    (= |first_branch_false_return_-(first_branch,$tt);| (Int 5))))))
  (=> (and (= zero_condition_ (Bool true))) a!1))
ADD PICK FORMULA: (=> P_first_branch_ and)
ADD GLOBAL FORMULA (=> P_first_branch_ and)
ADD GLOBAL FORMULA (= one_ (Int 1))
ADD GLOBAL FORMULA (let ((a!1 (= x_plus_one_ (Int (+ (i x_) (i one_))))))
  (and a!1 ((_ is Int) x_) ((_ is Int) one_)))
ADD GLOBAL FORMULA (let ((a!1 (= second_condition_ (Bool (< (i first_branch_) (i x_plus_one_))))))
(let ((a!2 (=> (and (= first_condition_ (Bool true)))
               (and a!1 ((_ is Int) first_branch_) ((_ is Int) x_plus_one_)))))
  (=> (and (= zero_condition_ (Bool true))) a!2)))
Hitting: second_branch: false
ADD GLOBAL FORMULA (let ((a!1 (=> (= second_condition_ (Bool false))
               (and (= second_branch_ |result_false_-(second_branch,$ff);|)
                    (= |result_false_-(second_branch,$ff);| (Int 84))))))
(let ((a!2 (=> (and (= first_condition_ (Bool true))) a!1)))
  (=> (and (= zero_condition_ (Bool true))) a!2)))
ADD PICK FORMULA: (=> P_second_branch_ and)
ADD GLOBAL FORMULA (=> P_second_branch_ and)
Evaluated to: 84
------------------------------
Running program...

Branch Information:
first_branch: True=Hit; False=Unhit
second_branch: True=Unhit; False=Hit
zero_branch: True=Hit; False=Unhit

Target branch: second_branch_; condition: second_condition_ = true
Solving for target branch:
Branch to pick: P_second_branch_
Branch condition: (= second_condition_ (Bool true))
Feed 21338 to x
ADD GLOBAL FORMULA (= large_ (Int 100))
ADD GLOBAL FORMULA (let ((a!1 (= zero_condition_ (Bool (< (i x_) (i large_))))))
  (and a!1 ((_ is Int) x_) ((_ is Int) large_)))
Hitting: zero_branch: false
ADD GLOBAL FORMULA (=> (= zero_condition_ (Bool false))
    (and (= zero_branch_ |zero_branch_false_-(zero_branch,$ff);|)
         (= |zero_branch_false_-(zero_branch,$ff);| (Bool false))))
ADD PICK FORMULA: (=> P_zero_branch_ and)
ADD GLOBAL FORMULA (=> P_zero_branch_ and)
ADD GLOBAL FORMULA (=> (and (= zero_condition_ (Bool false))) (= first_condition_ zero_branch_))
Hitting: first_branch: false
Running next iteration of concolic after abort
```


## Debug `hit_abort_target2.jil`

Feed -2 to x

Global formulas
* large = 100
* zero_condition = x < large
  * and x is int and large is int
* zero_condition = true => (zero_branch = zero_branch_true and zero_branch_true = true) 
  * i.e zero_condition = true => zero_branch = true
* 

## Debug `unreachable_bc_abort.jil`

We miss a target when then first input is 1. I think it may be related to not adding the pick branches properly.

This run doubly prints the formulas, unfortunately.

```
bstride@bstridexps:/mnt/c/Users/brand/Documents/JHU/CS/masters/jaylang-main/jaylang$ ./_build/default/src/bin/jil.exe -i ./test/dbmc/concolic/unreachable_bc_abort.jil -m concolic

Starting concolic execution...
------------------------------
Running program...

Branch Information:
unreachable_branch_because_abort: True=Unhit; False=Unhit
x_gte_one_branch: True=Unhit; False=Unhit
x_is_one_branch_with_abort: True=Unhit; False=Unhit
x_is_one_branch_without_abort: True=Unhit; False=Unhit
Feed 1 to x
ADD GLOBAL FORMULA (= one_ (Int 1))
ADD GLOBAL FORMULA (let ((a!1 (= x_gte_one_cond_ (Bool (<= (i one_) (i x_))))))
  (and a!1 ((_ is Int) one_) ((_ is Int) x_)))
Hitting: x_gte_one_branch: true
Hitting: x_is_one_branch_with_abort: true
ADD GLOBAL FORMULA (let ((a!1 (= |x_is_one_cond_-(x_gte_one_branch,$tt);|
              (Bool (= (i x_) (i one_))))))
  (=> (= x_gte_one_cond_ (Bool true))
      (and a!1 ((_ is Int) x_) ((_ is Int) one_))))
ADD PICK FORMULA: (=> P_x_gte_one_branch_ and)
ADD GLOBAL FORMULA (=> P_x_gte_one_branch_ and)
Running next iteration of concolic after abort
Creating persistent formula for branch x_is_one_branch_with_abort:true
ADD GLOBAL FORMULA (=> P_x_gte_one_branch_ and)
ADD GLOBAL FORMULA (let ((a!1 (= |x_is_one_cond_-(x_gte_one_branch,$tt);|
              (Bool (= (i x_) (i one_))))))
  (=> (= x_gte_one_cond_ (Bool true))
      (and a!1 ((_ is Int) x_) ((_ is Int) one_))))
ADD GLOBAL FORMULA (let ((a!1 (= x_gte_one_cond_ (Bool (<= (i one_) (i x_))))))
  (and a!1 ((_ is Int) one_) ((_ is Int) x_)))
ADD GLOBAL FORMULA (= one_ (Int 1))
ADD GLOBAL FORMULA (= |x_is_one_cond_-(x_gte_one_branch,$tt);| (Bool false))
Solving for target branch:
Branch to pick: |P_x_is_one_branch_with_abort_-(x_gte_one_branch,$tt);|
Branch condition: (= |x_is_one_cond_-(x_gte_one_branch,$tt);| (Bool false))
------------------------------
Running program...

Branch Information:
unreachable_branch_because_abort: True=Unhit; False=Unhit
x_gte_one_branch: True=Hit; False=Unhit
x_is_one_branch_with_abort: True=Found_abort; False=Unhit
x_is_one_branch_without_abort: True=Unhit; False=Unhit
Feed 0 to x
ADD GLOBAL FORMULA (= one_ (Int 1))
ADD GLOBAL FORMULA (let ((a!1 (= x_gte_one_cond_ (Bool (<= (i one_) (i x_))))))
  (and a!1 ((_ is Int) one_) ((_ is Int) x_)))
Hitting: x_gte_one_branch: false
ADD GLOBAL FORMULA (=> (= x_gte_one_cond_ (Bool false))
    (and (= x_gte_one_branch_ |arbitrary_x_lt_one_-(x_gte_one_branch,$ff);|)
         (= |arbitrary_x_lt_one_-(x_gte_one_branch,$ff);| (Int 1000))))
ADD PICK FORMULA: (=> P_x_gte_one_branch_ and)
ADD GLOBAL FORMULA (=> P_x_gte_one_branch_ and)
Evaluated to: 1000
Skipping already-hit target x_gte_one_branch_; condition: x_gte_one_cond_ = false
------------------------------
Finishing...

Branch Information:
unreachable_branch_because_abort: True=Unreachable; False=Unreachable
x_gte_one_branch: True=Hit; False=Hit
x_is_one_branch_with_abort: True=Found_abort; False=Missed
x_is_one_branch_without_abort: True=Unreachable; False=Unreachable
```