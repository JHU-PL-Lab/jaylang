open Core
open Dj_common

module Abs_exp = struct
  module T = struct
    type var = Var of Id.t [@@deriving equal, compare, hash, sexp]

    type binop = Jayil.Ast.binary_operator =
      | Binary_operator_plus
      | Binary_operator_minus
      | Binary_operator_times
      | Binary_operator_divide
      | Binary_operator_modulus
      | Binary_operator_less_than
      | Binary_operator_less_than_or_equal_to
      | Binary_operator_equal_to
      | Binary_operator_not_equal_to
      | Binary_operator_and
      | Binary_operator_or
    [@@deriving equal, compare, hash, sexp]

    type t = exp
    and exp = Just of clause | More of clause * exp
    and clause = Clause of Id.t * clause_body

    and clause_body =
      | Value of value
      | CVar of var
      | Appl of var * var
      | Not of var
      | Binop of var * binop * var
      | Cond of var * exp * exp
      | Match of var * pattern
      | Project of var * Id.t
      | Abort
      | Assume of var
      | Assert of var

    and pattern =
      | Fun_pat
      | Int_pat
      | Bool_pat
      | Record_pat of Set.M(Id).t
      | Strict_record_pat of Set.M(Id).t
      | Any_pat

    and value =
      | Int
      | Bool of bool
      | Function of var * exp
      | Record of Id.t Map.M(Id).t
    [@@deriving equal, compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let rec pp ff = pp_exp ff

  and pp_exp ff = function
    | Just c -> pp_clause ff c
    | More (c, e) -> Fmt.pf ff "%a%a" pp_clause c pp e

  and pp_clause ff (Clause (x, cb)) =
    Fmt.pf ff "%a = %a;@\n" Id.pp x pp_clause_body cb

  and pp_clause_body ff = function _ -> Fmt.string ff "body"
end

module Abs_clause = struct
  module T = struct
    type t = Abs_exp.T.clause [@@deriving equal, compare, hash, sexp]
  end

  include T
  include Comparator.Make (T)
end

include Abs_exp

(* Jayil_ast *)
module For_jayil_ast = struct
  let of_var (Jayil.Ast.Var (x, _)) = Var x
  let id_of_jvar (Jayil.Ast.Var (x, _)) = x
end

open For_jayil_ast

let to_id (Var x) = x
let id_of_clause (Clause (x, _)) = x

let rec lift_expr (Jayil.Ast.Expr cls) =
  match cls with
  | [] -> failwith "empty expr"
  | cl :: [] -> Just (lift_clause cl)
  | cl :: cls -> More (lift_clause cl, lift_expr (Jayil.Ast.Expr cls))

and lift_clause (Jayil.Ast.Clause (x, cbody)) =
  Clause (id_of_jvar x, lift_cbody cbody)

and lift_cbody = function
  | Jayil.Ast.Value_body v -> Value (lift_value v)
  | Jayil.Ast.Var_body x -> CVar (of_var x)
  | Jayil.Ast.Input_body -> Value Int
  | Jayil.Ast.Appl_body (x1, x2) -> Appl (of_var x1, of_var x2)
  | Jayil.Ast.Not_body x -> Not (of_var x)
  | Jayil.Ast.Binary_operation_body (x1, bop, x2) ->
      Binop (of_var x1, bop, of_var x2)
  | Jayil.Ast.Conditional_body (x, e1, e2) ->
      Cond (of_var x, lift_expr e1, lift_expr e2)
  | Jayil.Ast.Match_body (x, pat) -> Match (of_var x, lift_pattern pat)
  | Jayil.Ast.Projection_body (x, d) -> Project (of_var x, d)
  | Jayil.Ast.Abort_body -> Abort
  | Jayil.Ast.Assume_body x -> Assume (of_var x)
  | Jayil.Ast.Assert_body x -> Assert (of_var x)

and lift_value = function
  | Jayil.Ast.Value_int _ -> Int
  | Jayil.Ast.Value_bool b -> Bool b
  | Jayil.Ast.Value_function (Function_value (x, e)) ->
      Function (of_var x, lift_expr e)
  | Jayil.Ast.Value_record (Jayil.Ast.Record_value r) ->
      Record (Id_helper.core_map_of_id_map ~f:Jayil.Ast.id_of_var r)

and lift_pattern = function
  | Jayil.Ast.Fun_pattern -> Fun_pat
  | Jayil.Ast.Int_pattern -> Int_pat
  | Jayil.Ast.Bool_pattern -> Bool_pat
  | Jayil.Ast.Rec_pattern set ->
      Record_pat (Id_helper.core_set_of_id_set ~f:Fn.id set)
  | Jayil.Ast.Strict_rec_pattern set ->
      Strict_record_pat (Id_helper.core_set_of_id_set ~f:Fn.id set)
  | Jayil.Ast.Any_pattern -> Any_pat

let clb_to_string clb = Sexp.to_string_hum (sexp_of_clause_body clb)

let clause_of_e_exn = function
  | Just c -> c
  | More _ -> failwith "not just a clause"

let id_of_e_exn e =
  let cl = clause_of_e_exn e in
  let (Clause (x, _)) = cl in
  x
