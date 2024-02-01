## Concolic Evaluator TODOs

### Urgent

* Figure out why recursive_fun_1_fixed doesn't terminate on positive input
* Solver formulas for record pattern matching.
  * e.g. it can't recognize that { b = {} } ~ { a = {} } is always false
  * and remove the `is_done` label is `Session.t` to check this against the record_11.jil test
  * I believe this is the reason record_7 can't find its target branch binop_j_74 unsatisfiable (I traced it, and correct record matching should make that target branch unsatisfiable)
* If quit due to timeout, then still report if abort is found or not
  * This will necessarily require mutation, or some great Lwt skills
  * ^ This is sort of done because we quit on first abort most of the time
* Scale max step with number of lines
* fuzz with the answers, and check that they still SAT
  * try an OR that checks if any of the inputs can be different and still satisfy
* Don't resolve for target if there have been no new runs because currently we try the same target a few times with no change in formulas
* set unsatisfiables to unknown if using formulas from an exited session
  * can separately keep the formulas from an exit, and if those result in unsatisfiable, then try
    without. Then make some good choice of whether to try the solve or call unknown
  * The above bullet isn't done, but the parent bullet is

### Eventually

* Logging
* Use optional input AST branches to customize output
* Efficiency optimizations!
  * Regarding pick formulas, especially. Can I make them shorter or not duplicate? Maybe mark off some runtime branch as already having a pick formula? Could use a hashtbl and mutability for extra speed
  * ^ similarly, having all implied formulas under a map instead would allow me to use sets properly and not have duplicate formulas.
  * Shorten lookup keys to hash, if possible. Might not be beneficial though if the cost of hashing is greater than the cost of comparing a few times later. I think that since I have maps and sets with these lookup keys, I do want them shorter
* Write up the logic that proves the formula tracker implies stuff is correct
* Benchmark how the implies work to see what is most difficult to solve for.
* Don't re-solve for a branch if branch info hasn't changed.
  * This is tough because we might have gained new formulas by hitting deeper branches, but not new info.
  * I will just let it run a few times, and if the `Missed` is not significantly different after a few times, then set to unknown.
  * ^ This is done by comparing with a status store without payload, but I've moved it to "eventually" because it might want improvements

### Out of scope

* Fuzz with answers and try for as small of inputs as possible (which would require many runs of the solver) only when the solver finishes sufficiently quickly
  * Therefore can get better runs, but we don't waste time on an overloaded solver
* Prune irrelevant expressions
  * Similar to value numbering, constant folding etc
  * Also very similar to (or frankly just the same as) checking "hittability" from each line of code
* Analyze AST to determine hittable vs unreachable, as well as how aborts affect future runs
* Analyze AST to determine which branches are reachable starting from any line of code, and thus which branches depend on which lines. Then only use those formulas to solve, and ignore other formulas that might clog up the solver.
  * Further, we can determine if a solve/branch is affected by an abort (but this will need some more thought)
* Target the branches first that are known to have aborts by first analyzing AST
  * Prioritize any that are seen by pushing them to the front of the stack
* Value numbering and dead store elimination on jil programs (and hence liveness?)
* Any other AST analysis...

### Random thoughts

**Aborts and max steps**

See the recursive_abort test file.

When we hit an abort at some recursive depth, branches later in the clause list at that same depth are never
found and don't get put into the target list. Then, when that branch is solved for, it "correctly" comes out
as unsatisfiable because we won't be able to hit it because of max step or abort. However, it is unsatisfiable
before we add any of the abort or max step formulas--it is unsatisfiable because it has no idea there is a branch
that is satisfiable.

This is an issue because of max step. Further branches after max step are not necessarily unsatisfiable, but they
are unknown. For this reason, it seems the best I can do is just quit on max step and mark everything unhit as unknown.

With abort, a branch really is unsatisfiable. I just don't yet get the output status to be correct and specific.

**Efficiency**

I'd like to make the formulas easier to solve. UPDATE: it seems I've done this, but it introduced other problems where
the solver now quickly thinks it finds a solution, but it misses, so it just continues forever

First, the pick branches for target formulas:
* Currently, we pick and then add OR (AND "all parents" "runtime condition") (AND "all parents" "runtime condition")
* I think this is what is so difficult to solve for, but I think it will be hard to improve because we need to make
  and AND of all parents until the next pick statement that does the AND.
* We really we have a sequence of sets A1 > A2 > A3 > ... > An where A1 contains all the other sets of parents.
  We then use (AND An) OR (AND A(n-1)) OR ... OR (AND A1), which I think is hard on the solver.
  I would like to just show that if we have A > B, and we have (AND A) OR (AND B), this is the same
  as 

  This is tough because we have and, or, union, intersect which are all different.

  (AND (A union B)) is the AND of all formulas in A and B. SO this is (AND (AND A) (AND B))

  A > B when A - B = C, so (AND A) OR (AND B) = (AND (B union C)) OR (AND B) = (AND (AND B) (AND C)) OR (AND B) = (AND X Y) OR X = X = AND B
* This seems like it's wrong because of the depth dependent test. We can't just solve for the first instance.
  The reason it's wrong is because this doesn't include the condition. SO the nestedness is only for the parents, not for the conditions. Really we need to satisfy parents and condition, but a set that contains more parents doesn't have to satisfy the previous condition.

  P1 > P2 are the parent sets, and we have conditions C1 and C2 respectively. The formula to satisfy is
    (AND (AND P1) C1) OR (AND (AND P2) C2)
  where P1 - P2 = P, so P union P2 = P1
    (AND (AND P) (AND P2) C1) OR (AND (AND P2) C2)
    = AND P2 ((AND (AND P) C1) OR C2)
  i.e. we need to satisfy all the additional parents and the new condition, or we can just satisfy the original thing.
  So when building the formulas, we make an AND of condition plus all parents since the last checkpoint, and we OR that
  with the already known formulas. This builds the entire target formula

Second, the abort formulas:
* We have AND (A1 => ... => An => "abort condition n") (A1 => ... => Am => "abort condition m") ...
* This is added when we pick the abort.
* We already have formulas added underneath each Ai. So we could just add the picks underneath.
* i.e. AND pick (A1 => AND (pick => "abort condition1") ... A2 => ... An => (pick => abort condition2") )
* There will then be a bunch of picks in there anyways, but at least they'll all be underneath the same implies, which might make it easier.
* The same would be done for max steps. Can currently just use the same pick because we handle it the same.
* This does leave more work up to the solver to parse through the picks, but it makes for smaller formulas.

## Meetings


### 19 Jan 2024

**Where we left off**
* I used to only solve for one runtime instance of a branch, leading to failing tests
  * e.g. concolic/depth_dependent.jil (recall: a branch was only satisfiable during exactly one instance of the recursive call)
* We had several failing tests, and "max step" and solver timeouts were completely unhandled
* We couldn't solve for some input that determined the recursive depth of the function
  * e.g. what input leads to a desired sum in a simple sum recursive function
* I had only used hand-written jil files for tests

**What's been done**:
* The Bluejay files are copied to Jil, but some (specifically flow_sensitive_1) don't stop
  * It keeps hitting the same abort, but I'm trying to get it to stop giving that same input
  * This might be related to `assume` or `assert` statements.
  * For this reason, I don't yet have them automatically tested
* I now solve for every known instance of a branch  at once (fixing the concolic/depth_dependent.jil test)
* We have an "unknown" status if the solver times out because it is overloaded
  * This handles the inability to solve for the recursive depth
  * The runs can be quite slow though
* Several possible results are accepted by the tests for nondeterminism
* I made some efficiency improvements (I think) to the solver, so we can handle more formulas
  * This is needed because of how difficult solves are for targets in recursive functions

#### Issues

**Aborts**

Aborts and max steps lead to incorrect formulas.

Two facts first:
1. Even if we don't have *all* of the information, after a single run, we don't have any *wrong* or *conflicting* information, so the solver will only miss a target branch instead of incorrectly calling a branch unsatisfiable
2. Quitting because of reaching max step is like hitting an abort, and the program exits with no further information ... so we can simulate max steps by throwing aborts wherever we want, and the two can be handled the same

Here is an example program that shows aborts lead to incorrect formulas:

```
x = input;

zero = 0;
one = 1;

f = fun self -> (
    f0 = fun counter -> (
        is_done = counter == zero;
        r = is_done ? (
          abort_when_done = abort
        ) : (
          do_nothing = 0
        );

        not_is_done = not is_done;
        w = not_is_done ? (
          ss = self self;               
          new_counter = counter - one;  
          res = ss new_counter
        ) : (
          unreachable_bc_abort = r # unreachable because of abort in is_done
        )
    );
);

ff = f f;
void = ff one
```

Let's walk through what we expect to happen:
* Note that the input is never used
* The function has a counter that decrements, and the function aborts when the counter is zero
* The counter starts at one, so there will be two runtime instances of the `r = is_done` branch
  * One is on the first iteration, which is always false
  * One is on the second iteration, which is always true
* The program quits during the second iteration, so there is only one runtime instance of `w = not_is_done`
  * It is on the first iteration, which is always true
  * The branch is never encountered on the second iteration
* The solver tries to solve for `not_is_done = false`, but it only knows about the first instance, which is true, so the false branch is unsatisfiable
  * In reality, it is only "unreachable" because it hits an abort first. It could be satisfied if anything but abort happened first
  * Since aborts are like quitting due to max step, if this interpreter instead hit the max step at the `abort_when_done = abort` line instead, the branch should be incorrectly labeled as unsatisfiable

**Unreachable branches**

When the program quits early, some conditionals are never found, so the branches appear unreachable

* Some branches are unreachable because they are underneath an unsatisfiable branch. This is correct
* Some branches appear unreachable because the program quit early, and it never found either side of the branch
  * Thus, they don't get solved for
* I think I should analyze paths in the AST (which have cycles due to functions)...
  * When there are no targets on our stack of found branches, then pull a target from the reachable branches in the AST
  * Branches that are one of unsatisfiable, abort, max step, or unknown would block paths in the graph
* Similarly, should we limit our solving for branches that contain abort?
  * The only issue is that we wouldn't have any information about these branches if they are unreachable, and thus we'd be solving repeatedly forever

**Repetitive solving**

Sometimes the solver just can't figure out how to hit a branch, and it seems to make no progress. When do we stop trying?

* If the solver gains new formulas with the run, then it might be worth trying again
* I may like to quit when the branch information hasn't changed at all
* I would think that disallowing repeat inputs would fix this, but I must have a bug

#### Questions

* How do you suggest we handle the incorrect formulas?
  * Should we continue the interpreter but have a special formula for anything that depends on the abort statement, and don't dive into new functions?
* What should I do about `assume` statements? The interpreter is incomplete
* Is it out of scope to analyze the AST like this?
  * I worry that if we don't do it, then the evaluator is wrong
* Or is it best to just quit upon any abort or max step? (Max step would mean everything about the program is unknown, abort means we found a type error)

#### Conclusions

* Most programs will time out because they type check and are probably recursive, and we can't solve for that
* Upon abort or max step, if not recursive, then throw away all formulas and just remember that the given sequence of inputs can't be used again
* Assume is like abort, where we have to quit if it fails, but we don't report a failing assume
  * It affects control flow such that we don't continue
* Assert is like an exception, so again like an abort, but we don't report

### 24 Jan 2024

Updates:
* Most tests should be passing because an abort is found, but I try to continue to learn more and end up hitting an infinite loop of solving
  * I think this is because I don't stop trying to solve after several misses
* I have a **very** hacky timeout system, and I don't know Lwt well, but it sort of works
* I still add formulas from exited runs (e.g. max step, abort, etc.) but they can't ever imply "unsatisfiable" branches
  * I would like to selectively add them and be more sophisticated to still let some branches be called unsatisfiable

### 2 Feb 2024 -- Weekly PL Meeting

**Sharing the concolic evaluator**

* What is the concolic evaluator
  * It is an interpreter that collects concrete formulas from runtime variables, and it leaves input variables as abstract
  * It runs over Jayil files converted from Bluejay programs
* Purpose
  * To help with typing of Bluejay, and here's how:
  * The goal is to run the interpreter such that every line of code gets hit, specifically targeting aborts
  * Between runs, it solves for input variables that hit other lines while satisfying the concrete formulas
  * It should be fast to quickly hit as many lines as possible
  * If the concolic evaluator can find an input that leads to an abort in the Jayil program, it refutes that the Bluejay program was well-typed
* Comparison with other approaches
  * It is a forward analysis
  * It uses concrete runs instead of purely abstract formulas
  * It allows for random input variables instead of formulas purely from non-input variables
* Expected behavior
  * Goals:
    * It hits every line of satisfiable code and correctly finds the unsatisfiable lines
    * The next best thing is it always finds an abort if it is possible to hit one
  * In practice, it either quickly finds an abort, or it times out because there are none, and the program is too large to quickly hit all satisfiable lines
* What counts as success
  * If the Bluejay program is well-typed:
    * The evaluator finishes with a set of formulas that show all aborts are unsatisfiable, OR
    * The evaluator times out without hitting an abort
  * If the Bluejay program is not well-typed:
    * It hits an abort
* What is a failure:
  * If the Bluejay program is well-typed:
    * We count everything as a success
  * If the Bluejay program is not well-typed:
    * The evaluator times out (because the program is too large or loops too long)
* Open challenges:
  * Handle reaching max step in the interpretation. The choices are:
    * Quit and forget all formulas (but then we might be likely to hit max step immediately again because we learned nothing), OR
    * Quit but keep the formulas, recognizing that they can lead to incorrect solves (but then the solver is clogged up with formulas that take a long time to satisfy because there are so many after such a long run of the interpreter) OR
