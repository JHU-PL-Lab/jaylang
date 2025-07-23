
open Core

type 'a t = Equality of ('a Formula.t * 'a Direction.t) [@@unboxed]

let to_expression (type a) (Equality (expr, dir) : a t) : bool Formula.t =
  let eq_int a b = Formula.binop Formula.Binop.Equal a b in
  match dir with
  | True_direction -> expr 
  | False_direction -> Formula.not_ expr
  | Case_int i -> eq_int expr (Formula.const_int i)
  | Case_default { not_in } ->
    Formula.and_
    @@ List.map not_in ~f:(fun i -> Formula.binop Formula.Binop.Not_equal expr (Formula.const_int i))

let flip (Equality (e, dir) : bool t) : bool t =
  match dir with
  | True_direction -> Equality (e, False_direction)
  | False_direction -> Equality (e, True_direction)

let direction (type a) (Equality (_, dir) : a t) : a Direction.t =
  dir
