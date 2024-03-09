
module Test_result :
  sig
    type t =
      | Found_abort of Branch.t (* Found an abort at this branch *)
      | Exhausted               (* Ran all possible tree paths, and no paths were too deep *)
      | Exhausted_pruned_tree   (* Ran all possible tree paths up to the given max depth *)
      | Timeout                 (* total evaluation timeout *)
  end

val eval : (Jayil.Ast.expr -> Branch_info.t) Concolic_options.Fun.t

val test : (Jayil.Ast.expr -> Test_result.t) Concolic_options.Fun.t

(* val find_abort : (Jayil.Ast.expr -> Branch.t option) Concolic_options.Fun.t *)