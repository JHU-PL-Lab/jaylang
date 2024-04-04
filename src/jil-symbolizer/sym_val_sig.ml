open Core
open Dj_common

module type S = sig
  include module type of struct
    include
      Sudu.Z3_datatype.Make_datatype_ops (Sym_val.Jil_val_z3) (Solver_helper.C)
  end

  include Sudu.Z3_helper.Jil_z3_datatye

  val phi_of_value : Lookup_key.t -> Jayil.Ast.value -> Z3.Expr.expr
  val if_pattern : Z3.Expr.expr -> Jayil.Ast.pattern -> Z3.Expr.expr
end
