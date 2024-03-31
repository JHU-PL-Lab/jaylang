## Concolic Evaluator TODOs

### Eventually (after April deadline...)

* Solve for all targets along a path to not have to load the solver so many times.
* Scale max depth of tree with largest non-rec path
* Prune irrelevant expressions
  * Similar to value numbering, constant folding etc
  * Also very similar to (or frankly just the same as) checking "hittability" from each line of code

### Out of scope (for April deadline)

* Use mutable solver
  * Write interface that uses mutation but isn't smart. Then make new implementation over same interface that is smart, and that way we know what broke.
* Fuzz with answers and try for as small of inputs as possible (which would require many runs of the solver) only when the solver finishes sufficiently quickly
  * Therefore can get better runs, but we don't waste time on an overloaded solver
* Value numbering and dead store elimination on jil programs (and hence liveness?)


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


## 28 Feb 2024

**Last time**

* I brought in the record formula changes (=> all tests passed, and interpretation was exactly modeled by formulas)
* I used int identifiers in the solver instead of strings (=> small improvement)
* We wanted to get benchmarks on more programs
* We wanted to know where the program was slowing

**Since then**

* The new tests are reasonably fast, but we do not yet assert correctness (waiting on Earl to type the programs)
* Regarding where the program slows:
  * It takes about as long to load the solver as it does to solve the formulas in it
    * This motivates a change to use the solver's internal stack and push/pop formulas, but that forces a DFS and nonfunctional code
  * On "average", solving and loading each separately take about 0.005 seconds
    * But we might have to do this a lot to exhaust the tree by proving many branches unsatisfiable!
* We stop tracking any formulas after a certain depth to allow the interpreter to continue further but without slowing
* And some code cleanup/shortening

**Next**

* A simple way to combine DFS and BFS is this:
  * Always do DFS, but if we hit max step, then "run BFS once"
  * i.e. keep a DFS and BFS horizon, always popping from DFS, and if the run hits max step, then dequeue from BFS for next run and continue as normal (by going back to DFS after the run)
  * Alternatively, push all targets from a max step run to the very back, so all other targets get run first
  * With both of these approaches, we try to distance ourselves from the branches that are really deep in the program and thus might be more likely to lead to max step again
  * I don't discard all targets from a max step run because we still want to exhaust the tree eventually, but the above ideas might help us exhaust the expensive parts last
* Instead of finally using the internal stack to push/pop (which won't be natural if we choose the DFS/BFS combination like above) it sounds more interesting to me to selectively add only the formulas necessary
  * We have talked about this before but haven't given it high priority
  * If some value never directly depends on an input (though it might depend on the branches that are affected by the input, which is okay), then I can simply add the concrete value and nothing before it
  * This might reduce formulas, but maybe the programs are complex enough that it actually won't

**Questions**

* When do we combine all these tools into one pipeline?
* What's our technique for benchmarking?
  * Shiwei and robert have used "ocamlbench"
* When do we call our tool "finished" enough for the paper submission? i.e. what is the deadline to stop making improvements?
  * It appears to work now, but we have plenty of ideas for potential improvements
  
TODO: haskell and racket benchmarks converted (in a while... benchmarch bluejay first)


**Other**

* Approximation algorithms


## 13 Mar 2024

**Last time**
* We decided to just benchmark
* I need to choose some concolic tutorials

**What's been done**
* I fixed some bugs that caused slowdown
* It's easy to set a search strategy. Right now I randomly draw from BFS or DFS horizon and always draw from BFS after hitting max step
* I have chosen some programs to benchmark
* I benchmark the entire executable (which includes the parsing of the jil file)
* I found okay videos, but nothing excellent. They all feel slow or focus much more on symbolic execution than I'd expect
  * https://www.youtube.com/watch?v=TlEjgqSXYNE&list=PLBmY8PAxzwIEGtnJiucyGAnwWpxACE633&index=24 

**The benchmarked programs**

### Trees ###

| **Description** | **Filename** | **Notes** |
|-----------------|--------------|-----------|
| Well-typed bst | bst_type_well_typed | Simple small BST | 
| Ill-typed bst | set_type_10 | Left child is greater than right in small tree |
| Wrong record field in recursive tree type | recursive_type_3 | |
| Balanced tree | set_type_5_well_typed | |
| Imbalanced tree | set_type_5 | |

### Recursive programs

| **Description** | **Filename** | **Notes** |
|-----------------|--------------|-----------|
| Simple type error in base case | recursive_fun_1 | |
| No type error at all | recursive_fun_1_well_typed | Tree gets exhausted up to max depth in not too long |

### Dependent types ###

| **Description** | **Filename** | **Notes** |
|-----------------|--------------|-----------|
| Simple type error, no recursion | dependent_type_test_1 | |
| Simple no error, no recursion | dependent_type_test_1_well_typed | Tree should get exhausted quickly without limiting depth |
| Mutually recursive with type error | mutually_recursive_dep_types_1 | Should fail at base case, so it's easy to catch |

### Records and modules ###

| **Description** | **Filename** | **Notes** |
|-----------------|--------------|-----------|
| Constraints on fields, but no nested record | record_7 | |
| Fixed values so that constraints are satisfied | record_7_well_typed | No recursion, so should exhaust the tree |
| Type error in one field of module | module_5_1 | Is a big program, and well-typed version is way too large to include as a test |

### Other ###

| **Description** | **Filename** | **Notes** |
|-----------------|--------------|------------|
| Simple functions on types | let_fun_test_9 | Should get covered by other types because they use anonymous functions |
| Fixed the error | let_fun_test_9_well_typed | Should easily exhaust tree |
| List map | polymorphic_map | Well-typed version times out -- cannot even exhaust the pruned tree |
| Two typed functions | flow_sensitive_1 | No recursion, so type error is easy to catch |
| Two typed functions, well-typed | flow_sensitive_1_well_typed | Should be able to exhaust tree |

### Limitations ###

| **Description** | **Filename** | **Notes** |
|-----------------|--------------|-----------|
| Type error is too deep in the program | expected_timeout | Exhausts the tree but limits depth,and the type error is far past the allowed depth |


**What to do**
* Let's look at the programs I benchmark
* Which programs should we add to this?
  * Specifically: is this one limited case not enough?
* I need to benchmark only the concolic part, not the parsing: use ocamlbench like Robert
* If these are good, then I'll start looking at Haskell and Racket