
open Core

type int_direction =
  | Case_int of int
  | Case_default

type 'k t =
  | Bool_direction of bool * (bool, 'k) Smt.Formula.t
  | Int_direction of { dir : int_direction ; not_in : int list ; formula : (int, 'k) Smt.Formula.t }

let to_formula (dir : 'k t) : (bool, 'k) Smt.Formula.t =
  match dir with
  | Bool_direction (true, e) -> e
  | Bool_direction (false, e) -> Smt.Formula.not_ e
  | Int_direction { dir = Case_int i ; formula ; not_in = _ } ->
    Smt.Formula.binop Smt.Binop.Equal formula (Smt.Formula.const_int i)
  | Int_direction { dir = Case_default ; formula ; not_in } ->
    Smt.Formula.and_ @@ List.map not_in ~f:(fun i -> Smt.Formula.binop Smt.Binop.Not_equal formula (Smt.Formula.const_int i))

(*
  The list of all formulas that take other directions.
*)
let negations (dir : 'k t) : (bool, 'k) Smt.Formula.t list =
  match dir with
  | Bool_direction (b, e) -> [ to_formula @@ Bool_direction (not b, e) ]
  | Int_direction { dir = Case_int i ; formula ; not_in } ->
    to_formula (Int_direction { dir = Case_default ; not_in = i :: not_in ; formula })
    :: List.map not_in ~f:(fun i -> 
      to_formula @@ Int_direction { dir = Case_int i ; formula ; not_in = [] (* gets ignored *) }
    )
  | Int_direction { dir = Case_default ; formula ; not_in } ->
    List.map not_in ~f:(fun i -> 
      to_formula @@ Int_direction { dir = Case_int i ; formula ; not_in = [] (* gets ignored *) }
    )
