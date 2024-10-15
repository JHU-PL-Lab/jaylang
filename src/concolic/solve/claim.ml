
open Core

type t =
  | Int_equality of Concolic_key.t * int Expression.t
  | Bool_equality of Concolic_key.t * bool Expression.t

let[@landmarks] get_formulas (claims : t list) (cache : Expression.Cache.t) : bool C_sudu.Gexpr.t list =
  List.fold claims ~init:[] ~f:(fun acc claim ->
    let accum mk_key lookup key e acc =
      let key_expr = Expression.t_to_formula @@ mk_key key in
      C_sudu.eq key_expr (Expression.t_to_formula e)
      :: C_sudu.eq key_expr (Expression.t_to_formula @@ lookup cache key)
      :: acc
    in
    match claim with
    | Int_equality (key, i_expr) -> accum Expression.int_key Expression.Cache.lookup_int key i_expr acc
    | Bool_equality (key, b_expr) -> accum Expression.bool_key Expression.Cache.lookup_bool key b_expr acc
    )
