
open Core

type 'a t = Equality : 'a Expression.t * 'a Direction.t -> 'a t

let to_formula (type a) (Equality (expr, dir) : a t) : bool C_sudu.Gexpr.t =
  let z3_expr = Expression.t_to_formula expr in
  match dir with
  | True_direction -> C_sudu.eq z3_expr (C_sudu.box_bool true)
  | False_direction -> C_sudu.eq z3_expr (C_sudu.box_bool false)
  | Case_int i -> C_sudu.eq z3_expr (C_sudu.box_int i)
  | Case_default { not_in } ->
    List.fold not_in ~init:(C_sudu.box_bool true) ~f:(fun acc i ->
      C_sudu.and_ acc (C_sudu.neq z3_expr (C_sudu.box_int i))
    )

let flip (Equality (e, dir) : bool t) : bool t =
  match dir with
  | True_direction -> Equality (e, False_direction)
  | False_direction -> Equality (e, True_direction)

let direction (type a) (Equality (_, dir) : a t) : a Direction.t =
  dir
