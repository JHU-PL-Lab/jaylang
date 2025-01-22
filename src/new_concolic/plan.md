
I call use the word formula to refer to a value of type Expression.t

Issues:
* I want to return a value and a formula, but I have typed formulas for sake of correctness within that module and within uses of them. 
  * I cannot have a typed AST because there is inherent incompatibility with what we call typed vs what OCaml calls typed.
  * Therefore, I must "generate" a type during evaluation, but OCaml doesn't allow that, so I would have to have a packed generated value.
  * It's not a good idea to have this everywhere because it is verbose. However, it's also not a good idea to forego types in many places because it helps with my correctness (because I get to parse instead of validate). However, with the packing, I lose that parsing...
* I now have several kinds of branches: I can branch on ints and bools. It's thus a good idea to use types for this, but I get back to the same problem as before.
* I don't have keys on branches anymore, so it is more difficult to compare branches. I should really just be comparing directions (which to have types on them would be a GADT, so I cannot derive that).
* We have to communicate all of the other cases possible when taking a branch. We can do this naively and just pass in claims and other directions, but this leads to repeated expressions.
  * It makes sense that a claim should be made of a Z3 expr instead of an Expression because we don't want to have to compute the Z3 expr whenever we use the claim.
  * I'm starting to realize we don't even need a 

Some things I can do differently now:
* I don't need a cache of expressions. I just keep an expression on each node in the path tree.
  * When I collect formulas to solve for a target, I just use the formula on the branch, and that's it.
    * Note I may need to be careful about having duplication of formulas because I never reuse a subformula (but this is not a new problem; it existed before and was not a problem)
  * I then cannot avoid formula re-creation, but this might not be a big deal. It might be less costly than storing all formulas (because that is the most costly thing in the old version)
* The path tree should have two different kinds of nodes, one for int case and one for bool branch
* Symbolic session doesn't need to do so much 
  * It should only learn about branch hits (and request the formula for the condition), provide inputs (I shouldn't need to ask for the feeder--just query the symbolic session), handle expressions so that they don't have to be computed after a certain depth (especially because they could get big), so all expression stuff (which is much less now) is property of the symbolic session, watch for pruning (and track other similar results) so that the main session reports results properly.
  * Expression stuff involves:
    * Perform binop
    * Perform not
    * Create const expr
    * Create expressions for keys during input
  * Branch stuff involves creating a stem to be added to the path tree
    * Note that we'll recreate the formulas (because the subexpressions are not stored and might be needed later), but we won't bother pushing anything until after we've found the target, which the symbolic session tracks



Resolutions:
* Because I only have expressions for int and bool values, I can just put the expressions inside the value
  * Previously we never looked up the key unless it was an int or bool, but we always fetched a key for each node. This is much better now
* I've packed the branches alright. We'll see how nice it is inside the path tree
* Quick note: the key for the input doesn't need to be step count. It can be input count. But we might as well just use step count.
* We lose the ability to prioritize by branch hits (because we cannot index the branches easily anymore), but then we also don't need to represent a branch by anything except the path to reach it
  * Therefore, to compare targets, we only use the directions in the path


