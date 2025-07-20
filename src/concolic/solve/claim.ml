
open Core

type 'a t = Equality of ('a Formula.t * 'a Direction.t) [@@unboxed]

let to_expression (type a) (Equality (expr, dir) : a t) : bool Formula.t =
  let eq_int a b = Formula.binop a b Formula.Typed_binop.Equal_int in
  match dir with
  | True_direction -> expr 
  | False_direction -> Formula.not_ expr
  | Case_int i -> eq_int expr (Formula.const_int i)
  | Case_default { not_in } ->
    List.fold not_in ~init:(Formula.const_bool true) ~f:(fun acc i ->
      let neq = Formula.binop expr (Formula.const_int i) Formula.Typed_binop.Not_equal in
      Formula.binop acc neq Formula.Typed_binop.And
    )

let flip (Equality (e, dir) : bool t) : bool t =
  match dir with
  | True_direction -> Equality (e, False_direction)
  | False_direction -> Equality (e, True_direction)

let direction (type a) (Equality (_, dir) : a t) : a Direction.t =
  dir
