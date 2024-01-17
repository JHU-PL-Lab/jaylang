open Core
open Jayil

module Lookup_key = 
  struct
    include Lookup_key
    (* Core.Map.Key expects t_of_sexp, so provide failing implementation *)
    let t_of_sexp _ = failwith "Lookup_key.t_of_sexp needed and not implemented"
  end

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

    let to_string { branch_ident = Ast.Ident s ; direction } : string =
      s ^ ":" ^ Direction.to_string direction

    let of_ident_and_bool (branch_ident : Ast.ident) (dir : bool) : t =
      { branch_ident ; direction = Direction.of_bool dir }
    
    let other_direction { branch_ident ; direction } : t =
      { branch_ident ; direction = Direction.other_direction direction }

    let (^-) (Ast.Ident a) b = Ast.Ident (a ^ "-" ^ b)

    let pick_abort (x : t) : Z3.Expr.expr =
      (* hyphens aren't allowed in a variable name, so this is necessarily unique *)
      let Ident s = (x.branch_ident ^- Direction.to_string x.direction) ^- "abort" in
      Riddler.picked_string s

    let pick_max_step (x : t) : Z3.Expr.expr =
      (* hyphens aren't allowed in a variable name, so this is necessarily unique *)
      let Ident s = (x.branch_ident ^- Direction.to_string x.direction) ^- "max-step" in
      Riddler.picked_string s

  end

include T

module Runtime =
  struct
    type t =
      { branch_key    : Lookup_key.t
      ; condition_key : Lookup_key.t
      ; direction     : Direction.t }
      [@@deriving compare, sexp]

    let to_expr ({condition_key ; direction ; _ } : t) : Z3.Expr.expr =
      Riddler.eqv condition_key (Direction.to_value_bool direction)

    let to_ast_branch ({ branch_key ; direction ; _ } : t) : T.t =
      T.{ branch_ident = branch_key.x ; direction }

    let to_string ({ branch_key ; condition_key ; direction } : t) : string =
      Lookup_key.to_string branch_key
      ^ "; condition: "
      ^ Lookup_key.to_string condition_key
      ^ " = "
      ^ Direction.to_string direction

    let other_direction (x : t) : t =
      { x with direction = Direction.other_direction x.direction }

    let print_target_option (x : t option) : unit =
      let target_branch_str = 
        match x with 
        | None -> "None"
        | Some target -> to_string target
      in 
      Format.printf "\nTarget branch: %s\n" target_branch_str

    let pick_abort (x : t) : Z3.Expr.expr =
      T.pick_abort
      @@ to_ast_branch x

    let pick_max_step (x : t) : Z3.Expr.expr =
      T.pick_max_step
      @@ to_ast_branch x
  end