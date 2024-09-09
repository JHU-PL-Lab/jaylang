open Core
open Jayil

module Direction =
  struct
    type t =
      | True_direction
      | False_direction
      [@@deriving equal, compare, sexp]

    let to_string = function
      | True_direction -> "true"
      | False_direction -> "false"

    let other_direction = function
      | True_direction -> False_direction
      | False_direction -> True_direction

    let to_value_bool = function
      | True_direction -> Ast.Value_bool true
      | False_direction -> Ast.Value_bool false

    let of_bool b = if b then True_direction else False_direction
  end

module T = 
 struct
    type t =
      { branch_ident : Ast.Ident_new.t
      ; direction    : Direction.t }
      [@@deriving sexp, compare]
  end

include T

module Runtime =
  struct
    type t =
      { branch_key    : Concolic_key.t
      ; condition_key : Concolic_key.t
      ; direction     : Direction.t }
      [@@deriving compare, sexp]

    let to_expr ({condition_key ; direction ; _ } : t) : Z3.Expr.expr =
      Concolic_riddler.eqv condition_key (Direction.to_value_bool direction)

    let to_ast_branch ({ branch_key ; direction ; _ } : t) : T.t =
      T.{ branch_ident = Concolic_key.id branch_key ; direction }

    let to_string ({ branch_key ; condition_key ; direction } : t) : string =
      Concolic_key.to_string branch_key
      ^ "; condition: "
      ^ Concolic_key.to_string condition_key
      ^ " = "
      ^ Direction.to_string direction

    let other_direction (x : t) : t =
      { x with direction = Direction.other_direction x.direction }
  end