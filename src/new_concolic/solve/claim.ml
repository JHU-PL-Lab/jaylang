
open Core

type t = 
  | Int_equality of int Expression.t * int
  | Bool_equality of bool Expression.t * bool

let to_formulas (claims : t list) : bool C_sudu.Gexpr.t list =
  List.map claims ~f:(function 
    | Int_equality (expr, i) -> C_sudu.eq (Expression.int_t_to_formula expr) (C_sudu.box_int i)
    | Bool_equality (expr, b) -> C_sudu.eq (Expression.bool_t_to_formula expr) (C_sudu.box_bool b)
  )

