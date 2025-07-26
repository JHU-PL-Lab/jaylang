
open Core

type int_direction =
  | Case_int of int
  | Case_default

module T = struct
  type 'k t =
    | Bool_direction of bool * (bool, 'k) Smt.Formula.t
    | Int_direction of { dir : int_direction ; not_in : int list ; expr : (int, 'k) Smt.Formula.t }
end

include T

let to_expression (dir : 'k t) : (bool, 'k) Smt.Formula.t =
  match dir with
  | Bool_direction (true, e) -> e
  | Bool_direction (false, e) -> Smt.Formula.not_ e
  | Int_direction { dir = Case_int i ; expr ; not_in = _ } ->
    Smt.Formula.binop Smt.Binop.Equal expr (Smt.Formula.const_int i)
  | Int_direction { dir = Case_default ; expr ; not_in } ->
    Smt.Formula.and_ @@ List.map not_in ~f:(fun i -> Smt.Formula.binop Smt.Binop.Not_equal expr (Smt.Formula.const_int i))

(*
  The list of all expressions that take other directions.
*)
let negations (dir : 'k t) : (bool, 'k) Smt.Formula.t list =
  match dir with
  | Bool_direction (b, e) -> [ to_expression @@ Bool_direction (not b, e) ]
  | Int_direction { dir = Case_int i ; expr ; not_in } ->
    to_expression (Int_direction { dir = Case_default ; not_in = i :: not_in ; expr })
    :: List.map not_in ~f:(fun i -> 
      to_expression @@ Int_direction { dir = Case_int i ; expr ; not_in = [] (* gets ignored *) }
    )
  | Int_direction { dir = Case_default ; expr ; not_in } ->
    List.map not_in ~f:(fun i -> 
      to_expression @@ Int_direction { dir = Case_int i ; expr ; not_in = [] (* gets ignored *) }
    )
