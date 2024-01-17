## Concolic Evaluator TODOs

### Urgent

* Search AST for abort branches, and solve for those
  * Misses will be common while gaining solve info, but we at least target the most important branches
* Don't resolve for a branch if branch info hasn't changed.
  * This is tough because we might have gained new formulas by hitting deeper branches, but not new info.
  * I will just let it run a few times, and if the `Missed` is not significantly different after a few times,
    then set to unknown.
* Benchmark how the implies work to see what is most difficult to solve for.
* Disallow inputs that have already been used
  * I think we can just add global formulas because even if this makes some branch appear unsatisfiable, that branch has already been hit
  * ^ this is done, but I'm not sure if I properly disallow repeat "second inputs" after the first one is different. I need to disallow some set of inputs, but other sets are fine.
  * ^ It also makes some later branches appear unsatisfiable when they are only so because of aborts. Need to handle this, and with max step as well.
* Convert Earl's jay (or bluejay?) files to jayil and make test cases
  * Use convert and translator. Can see about converting on the spot, or maybe write a script to save the conversions.
  * ^ I should do the latter so that I can read the jil files to debug concolic better

### Eventually

* Quit solving after missing too many times (e.g. depth_dependent2_tail_rec tried a ton of inputs that got nowhere but didn't hit max step) and say "unknown"
* Throw exception if we ever try to solve for the same branch with the same formulas, i.e. continue with misses until we reach a steady state
* Logging
* Use optional input AST branches to customize output
* Efficiency optimizations!
  * Regarding pick formulas, especially. Can I make them shorter or not duplicate? Maybe mark off some runtime branch as already having a pick formula? Could use a hashtbl and mutability for extra speed
  * ^ similarly, having all implied formulas under a map instead would allow me to use sets properly and not have duplicate formulas.
  * Shorten lookup keys to hash, if possible. Might not be beneficial though if the cost of hashing is greater than the cost of comparing a few times later. I think that since I have maps and sets with these lookup keys, I do want them shorter
* Write up the logic that proves the formula tracker implies stuff is correct

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
