
(*
  Res is short for "result", but we don't want to conflict
  with Core.Result.

  These are errors or real semantic values.
*)

type 'a t = 
  | V of 'a Value.v
  | E of Err.t

type 'a r =
  | Expr of Lang.Ast.Embedded.t
  | Res of 'a t
