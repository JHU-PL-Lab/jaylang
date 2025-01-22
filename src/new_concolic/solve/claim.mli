
type t = 
  | Int_equality of int Expression.t * int
  | Bool_equality of bool Expression.t * bool

val to_formulas : t list -> bool C_sudu.Gexpr.t list
