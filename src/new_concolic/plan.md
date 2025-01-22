
I call use the word formula to refer to a value of type Expression.t

Issues:
* I want to return a value and a formula, but I have typed formulas for sake of correctness within that module and within uses of them. 
  * I cannot have a typed AST because there is inherent incompatibility with what we call typed vs what OCaml calls typed.
  * Therefore, I must "generate" a type during evaluation, but OCaml doesn't allow that, so I would have to have a packed generated value.
  * It's not a good idea to have this everywhere because it is verbose. However, it's also not a good idea to forego types in many places because it helps with my correctness (because I get to parse instead of validate). However, with the packing, I lose that parsing...
* I now have several kinds of branches: I can branch on ints and bools. It's thus a good idea to use types for this, but I get back to the same problem as before.
* I don't have keys on branches anymore, so it is more difficult to compare branches. I should really just be comparing directions (which to have types on them would be a GADT, so I cannot derive that).

Some things I can do differently now:
* I don't need a cache of expressions. I just keep an expression on each node in the path tree.
  * When I collect formulas to solve for a target, I just use the formula on the branch, and that's it.
    * Note I may need to be careful about having duplication of formulas because I never reuse a subformula (but this is not a new problem; it existed before and was not a problem)
  * I then cannot avoid formula re-creation, but this might not be a big deal. It might be less costly than storing all formulas (because that is the most costly thing in the old version)
* The path tree should have two different kinds of nodes, one for int case and one for bool branch
* Symbolic session doesn't need to do so much 




Resolutions:
* Because I only have expressions for int and bool values, I can just put the expressions inside the value
  * Previously we never looked up the key unless it was an int or bool, but we always fetched a key for each node. This is much better now
* I've packed the branches alright. We'll see how nice it is inside the path tree
* Quick note: the key for the input doesn't need to be step count. It can be input count. But we might as well just use step count.
