## Concolic Evaluator TODOs

Next steps:
* Split into concrete, symbolic, and concolic sessions.
  * Concrete is for the interpreter to run correctly
  * Symbolic is to track everything during a run
  * Concolic is for between-run to combine the results from the symbolic into what else is known from other runs.
    * Maybe this can have a better name because it's really a "between-run-symbolic session"
* If never hit max depth, then report that all program paths were exhausted.
* Scale max depth of tree with largest non-rec path
* propose tutorial vidoes
  * Not sure if these should be conference videos or some tutorial (e.g. summer school)
* propose how we might use several heuristics and dial them before actually implementing
  * e.g. what ideas can we use to do both DFS and BFS on the tree at the same time
* address top urgent todos

### Urgent

* Research prioritization schemes
  * Currently, BFS and DFS do similarly well
  * Try prioritizing branches that immediately have an abort
  * Prioritize branches that are closest in CFG to uncovered lines
    * I wonder how this works when so many lines are uncovered due to the many aborts
* Prune irrelevant expressions
  * Similar to value numbering, constant folding etc
  * Also very similar to (or frankly just the same as) checking "hittability" from each line of code
* Shorten lookup keys to atoms
* Scale max step with number of lines
* fuzz with the answers, and check that they still SAT
  * try an OR that checks if any of the inputs can be different and still satisfy
* Don't resolve for target if there have been no new runs because currently we try the same target a few times with no change in formulas

### Eventually

* Logging
* Use optional input AST branches to customize output

### Out of scope

* Fuzz with answers and try for as small of inputs as possible (which would require many runs of the solver) only when the solver finishes sufficiently quickly
  * Therefore can get better runs, but we don't waste time on an overloaded solver
* Analyze AST to determine hittable vs unreachable, as well as how aborts affect future runs
* Analyze AST to determine which branches are reachable starting from any line of code, and thus which branches depend on which lines. Then only use those formulas to solve, and ignore other formulas that might clog up the solver.
  * Further, we can determine if a solve/branch is affected by an abort (but this will need some more thought)
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


**Overview**

Concolic = concrete + symbolic

It is an interpreter that uses concrete inputs to run the program, and as it runs, it writes a formula for every line of the program.

It uses these formulas to solve for inputs to hit all conditional branches.

It runs on Jayil programs that are translated from Bluejay programs.

**Purpose**

The concolic evaluator will help with the typing of Bluejay, and here's how:
* It tries to hit every line, hopefully hitting aborts
* It should be fast to quickly hit as many lines as possible
* If it finds an input to hit an abort, it has refuted that the Bluejay program is well-typed

**Comparison with other approaches**

* It is a forward analysis
* It uses concrete runs instead of purely symbolic formulas
* It uses some symbolic formulas to guide the abstract runs (it is not purely random)

**Goals**

What is a successful run when the Bluejay program is well-typed:
* It hits every line of satisfiable code and correctly finds unsatisfiable lines, OR
* It times out before it finds an abort (because an abort is impossible to hit)

What is a successful run when the Bluejay program is not well-typed:
* It quickly hits an abort without timing out

**Challenges**

Record pattern matching: this is not represented completely by formulas in the solver.

Max steps:
* We can't solve the halting program, so we need to stop interpretation short. The choices are:
  * Quit and forget all formulas (but then we might be likely to hit max step immediately again because we learned nothing), OR
  * Quit but keep the formulas, recognizing that they can lead to incorrect solves (but then the solver is clogged up with formulas that take a long time to satisfy because there are so many after such a long run of the interpreter) OR
  * Something else?

The solver gets clogged up, and it begins to run slowly. Similarly, in the worst case it is O(n^2) to track formulas where n is the number of lines in the program.


It's unlikely that we'll hit all satisfiable lines of the program quickly, so we settle for a timeout

Input inside of recursive function until sum of inputs is large enough