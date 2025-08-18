
type ('a, 'k) t = 'a * ('a, 'k) Smt.Formula.t

let to_string f (v, _) = f v

let return_bool b = b, Smt.Formula.const_bool b

let equal eq (a, e_a) (b, e_b) =
  eq a b
  && Smt.Formula.equal e_a e_b

module Make (K : Smt.Symbol.KEY) = struct
  type nonrec 'a t = ('a, K.t) t

  let to_string = to_string
  let return_bool = return_bool
  let equal = equal
end
