
module Sort = Sudu.Simpler_z3_api.Sort

(*
  Maybe here I should have a "depends on".

  I don't think it makes sense to have another type that has the dependencies,
  and then has one of these because there is no need to store dependencies
  when this is const.

  Potentially a JIL value should have a concolic key (and I send key back to just
  id and x). Further, I should rename key's labels to be `clause_id` and `uniq_id`, 
  or something like that.

  The goal is to have an interface that the evaluator can use where the evaluator
  just tells the interface what operation happened, what where the values involved
  (where a value is a dvalue and the keys or whatever this type happens to be). The
  evaluator will be doing the lookups and the math and all that. I guess I hope that
  the evaluator basically doesn't have to change.

  However, it's still to have a third interface with all of the possible operations again.
  This might motivate a change to completely remove Riddler (it is so short, and it
  can be a hidden helper in some other actual interface).

  I may also move away from Dvalue because it is so old and might not be the best
  choice anymore. If I am always passing along other things with it, then I just
  need to join them together. Like of course I need to access the actual value
  separately from the key sometimes and vice versa, but that is easy to do with
  records. So maybe whatever this is needs to be recursive with dvalue, and denv
  maps to this.

  I think it would be nice to have an interface that takes the values and does the math,
  actually, and returns the keys and values and expressions and all that. Then I just
  pass this into the symbolic session. These expressions would be lazy, and they only
  really get loaded in the case that I need them. That's because right now, I am both passing in
  the keys and the operation, and then I'm also doing the operation inside evaluator. Let's
  consolidate and have one thing do these at once so it's harder to mess up.

  But I do think this stuff needs to not be in symbolic session because there are other variables
  there (e.g. max step, current depth, branches we've hit, etc.), and to also handle all math
  and Z3 logic and expression dependencies is just too much. I just want an SMT frontend but also
  joined with a calculator.

  It seems like just a key is the right data structure, and the key continues to hold the value
  or abstraction and sort. Then we can just have the denv map to the key (because a key always
  gets made), and it is recursive with dvalue. The only annoyance is that now we can't just make
  any key for the clause: we must wait to make the key until we know the value. I should still
  think about this trafeoff.
  
  I need to copy all of the possible operations (including not and alias) to have payloads for the
  keys, which has a t here, and that way, when I build expressions, I am only building something
  that can later be put together concisely.
    e.g. For aliases, I just follow a key mapping until I reach an expression that can't be simplified.
  So I have a set of expressions that I need to add, and for each I go look at what they are built of,
  and I add those (I only need to be smart about formulas that share dependencies). Recursively, this
  gets us there. I need to be careful about aliases and make sure I only add the most simple expression.
  Possibly I should do this when creating the expression: make the simplification immediately so that
  there is no tracing down to do. And I should have a map from many keys to the expression they are.
*)

type t = 
  | Const of Dvalue.t
  | Abstract of Sort.t