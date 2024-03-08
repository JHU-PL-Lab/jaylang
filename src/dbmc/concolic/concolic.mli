
val eval : (Jayil.Ast.expr -> Branch_info.t) Concolic_options.Fun.t

val test : (Jayil.Ast.expr -> [ `Found_abort | `Exhausted | `Exhausted_pruned_tree | `Timeout ]) Concolic_options.Fun.t

val find_abort : (Jayil.Ast.expr -> Branch.t option) Concolic_options.Fun.t