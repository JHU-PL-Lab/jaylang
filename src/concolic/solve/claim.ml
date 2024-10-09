
open Core

type t =
  | Int_equality of Concolic_key.t * int Expression.t
  | Bool_equality of Concolic_key.t * bool Expression.t

let get_formulas (claims : t list) (cache : Expression.Cache.t) : bool C_sudu.Gexpr.t list =
  List.fold claims ~init:[] ~f:(fun acc claim ->
    let open Expression in
    match claim with
    | Int_equality (key, i_expr) ->
      let g_expr = int_t_to_formula i_expr in
      let key_expr = int_t_to_formula @@ Expression.int_key key in
      let expr = int_t_to_formula @@ Expression.Cache.lookup cache key in
      C_sudu.eq key_expr g_expr :: C_sudu.eq key_expr expr :: acc
    | Bool_equality (key, b_expr) ->
      let g_expr = bool_t_to_formula b_expr in
      let key_expr = bool_t_to_formula @@ Expression.bool_key key in
      let expr = bool_t_to_formula @@ Expression.Cache.lookup cache key in
      C_sudu.eq key_expr g_expr :: C_sudu.eq key_expr expr :: acc
    )
