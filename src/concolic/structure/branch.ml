open Core
open Jayil

let (@.) = Fn.compose

module Direction =
  struct
    type t =
      | True_direction
      | False_direction
      [@@deriving equal, compare, sexp]

    let to_bool = function
      | True_direction -> true
      | False_direction -> false

    let to_string = Bool.to_string @. to_bool

    let other_direction = function
      | True_direction -> False_direction
      | False_direction -> True_direction

    let to_value_bool d = Ast.Value_bool (to_bool d)

    let to_expression = Expression.const_bool @. to_bool

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
      ; condition_key : Concolic_key.t[@compare.ignore]
      ; direction     : Direction.t }
      [@@deriving compare, sexp]

    let to_claim ({ condition_key ; direction ; _ } : t) : Claim.t =
      Claim.Bool_equality (condition_key, Direction.to_expression direction)

    let to_ast_branch ({ branch_key ; direction ; _ } : t) : T.t =
      T.{ branch_ident = Concolic_key.clause_name branch_key ; direction }

    let to_string ({ branch_key ; condition_key ; direction } : t) : string =
      Concolic_key.to_string branch_key
      ^ "; condition: "
      ^ Concolic_key.to_string condition_key
      ^ " = "
      ^ Direction.to_string direction

    let other_direction (x : t) : t =
      { x with direction = Direction.other_direction x.direction }
  end