
open Core
open Z3

module Sort =
  struct
    type t = 
      | Int_sort 
      | Bool_sort
  end

module type Context = sig
  val ctx : Z3.context
end

module Make_common_builders (C : Context) = struct
  let ctx = C.ctx

  let eq e1 e2 = Boolean.mk_eq ctx e1 e2
  let or_ = Boolean.mk_or ctx
  let not_ = Boolean.mk_not ctx

  (* box to Z3 expression *)
  let box_int i = Arithmetic.Integer.mk_numeral_i ctx i
  let box_bool b = Boolean.mk_val ctx b

  (* unbox from Z3 expression *)
  let unbox_int e =
    e |> Arithmetic.Integer.get_big_int |> Big_int_Z.int_of_big_int
end

module Make_datatype_builders (C : Context) = struct
  (*
     This module is for Z3 formulae working on type
     ```ocaml
     type t = 
     | Int of {i : Z3.int}
     | Bool of {b : Z3.bool}
     ```
  *)
  include Make_common_builders (C)

  (* making sorts *)
  let intS = Arithmetic.Integer.mk_sort ctx
  let boolS = Boolean.mk_sort ctx

  (* building Z3 value expressions with the declarations  *)
  let int_ = box_int
  let bool_ = box_bool

  (* basic builders *)
  let int_var i = Expr.mk_const ctx (Symbol.mk_int ctx i) intS (* used to identify variables with a unique int *)
  let bool_var i = Expr.mk_const ctx (Symbol.mk_int ctx i) boolS

  let var sort i =
    match sort with
    | Sort.Int_sort -> int_var i
    | Bool_sort -> bool_var i

  (* use variable expression to query model for int input *)
  let get_int_expr model e =
    let open Option.Let_syntax in
    Model.get_const_interp_e model e (* check if the expression exists in the model *)
    >>= fun expr -> Model.eval model expr false (* find the value of that int within the model *)
    >>| unbox_int (* get into an ocaml int *)
end

module Make_datatype_builder_helpers (C : Context) = struct
  include Make_datatype_builders (C)

  let fn fop =
    fun y e1 e2 ->
      eq y @@ fop e1 e2

  (* actual operations on expressions *)

  let fn_not y e = eq y @@ not_ e
  let fn_plus = fn @@ fun e1 e2 -> Arithmetic.mk_add ctx [ e1; e2 ]
  let fn_minus = fn @@ fun e1 e2 -> Arithmetic.mk_sub ctx [ e1; e2 ]
  let fn_times = fn @@ fun e1 e2 -> Arithmetic.mk_mul ctx [ e1; e2 ]
  let fn_divide = fn @@ Arithmetic.mk_div ctx
  let fn_modulus = fn @@ Arithmetic.Integer.mk_mod ctx
  let fn_lt = fn @@ Arithmetic.mk_lt ctx
  let fn_le = fn @@ Arithmetic.mk_le ctx
  let fn_eq = fn eq
  let fn_neq = fn @@ fun e1 e2 -> not_ @@ eq e1 e2
  let fn_and = fn @@ fun e1 e2 -> Boolean.mk_and ctx [ e1; e2 ]
  let fn_or = fn @@ fun e1 e2 -> Boolean.mk_or ctx [ e1; e2 ]
end

module Make (C : Context) = struct
  include Make_datatype_builder_helpers (C)
end
