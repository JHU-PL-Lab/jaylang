open Fix

let interp sol e = match e with _ -> failwith "not yet"

module CS = Dj_common.Concrete_stack

module Exp_with_cs_as_key = struct
  type t = Jayil.Ast.expr * CS.t

  let equal (e1, cs1) (e2, cs2) = Jayil.Ast.equal_expr e1 e2 && CS.equal cs1 cs2
  let hash = Hashtbl.hash
end

open Abs_value

module AValue_as_prop = struct
  type property = AVal.t

  let bottom = AVal.Any
  let equal = AVal.equal
  let is_maximal _v = false
end

open Jayil.Ast
module F = Fix.ForHashedType (Exp_with_cs_as_key) (AValue_as_prop)

let eqs ((Expr cls, _) : Exp_with_cs_as_key.t)
    (_aeval : Exp_with_cs_as_key.t -> AValue_as_prop.property) =
  AVal.Any

let f = F.lfp eqs

(* let run source = f (source, CS.empty) *)
let run e = Fixer.analyze e
let _ = 1

(*
   Definitive
   let rev aeval abs_e abs_env : abs_v =
     match abs_e with
     | AInt x -> AInt x
     | Var x -> lookup abs_env x
*)
(*
   The value we need is already in the abs_env,
   Key: mimic the interpreter
   Ainterp is at least more complex than the interp.

   Therefore, e.g. without optmization, think about how 

   a = {T}
   b = not a
   c = a
   d = b & c

   a = {T,F}
   b = not a
   c = a
   d = b & c

   When `a` discovers from `{T}` to `{T,F}`

      *)

(* let eqs2 (_aeval : Exp_with_cs_as_key.t -> AValue_as_prop.property)
       ((Expr cls, _) : Exp_with_cs_as_key.t) =
     AVal.Any

   module M = Memoize.ForHashedType (Exp_with_cs_as_key)

   let run source =
     let f, _table = M.visibly_memoize (M.defensive_fix eqs2) in
     f (source, CS.empty) *)
