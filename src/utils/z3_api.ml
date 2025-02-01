
open Core
open Z3

module type Context = sig
  val ctx : Z3.context
end

module Make_common_builders (C : Context) = struct
  module E = Separate.Make (struct type t = Z3.Expr.expr end)
  open E

  let ctx = C.ctx

  (* box to Z3 expression *)
  let box_int i = int_ @@ Arithmetic.Integer.mk_numeral_i ctx i
  let box_bool b = bool_ @@ Boolean.mk_val ctx b

  (* making sorts *)
  let intS = Arithmetic.Integer.mk_sort ctx
  let boolS = Boolean.mk_sort ctx

  (* basic builders. Given int is the unique identifier for the variable, which is used for the symbol *)
  let int_var i = int_ @@ Expr.mk_const ctx (Symbol.mk_int ctx i) intS
  let bool_var i = bool_ @@ Expr.mk_const ctx (Symbol.mk_int ctx i) boolS

  (* unbox from Z3 expression *)
  let unbox_int e =
    Big_int_Z.int_of_big_int
    @@ Arithmetic.Integer.get_big_int e

  let unbox_bool e =
    match Boolean.get_bool_value e with
    | L_FALSE -> false
    | L_TRUE -> true
    | L_UNDEF -> failwith "bad unbox bool"

  (* use variable expression to query model for a concrete value associated with the expression *)
  let a_of_expr model expr unbox =
    let open Option.Let_syntax in
    Model.get_const_interp_e model expr (* check if the expression exists in the model *)
    >>= fun expr -> Model.eval model expr false (* find the value of that int within the model *)
    >>| unbox (* unbox into ocaml value *)

  (* use variable expression to query model for input *)
  let value_of_expr (type a) model (expr : a t) : a option =
    match expr with
    | I e -> a_of_expr model e unbox_int
    | B e -> a_of_expr model e unbox_bool

  (* use variable expression to query model for input *)
  let int_of_expr model (I e) = a_of_expr model e unbox_int (* this needs to stick around for old concolic *)
  (* let bool_of_expr model (Bool_expr e) = a_of_expr model e unbox_bool *)
end

module Make_datatype_builders (C : Context) = struct
  include Make_common_builders (C)

  open E

  let op_two_ints ret op =
    fun (e1 : int t) (e2 : int t) ->
      ret @@ op (extract e1) (extract e2)

  let op_two_bools ret op =
    fun (e1 : bool t) (e2 : bool t) ->
      ret @@ op (extract e1) (extract e2)

  let list_curry f x y = f [ x ; y ]

  (* actual operations on expressions. *)

  let eq (type a) (e1 : a t) (e2 : a t) : bool t = bool_ @@ Boolean.mk_eq ctx (extract e1) (extract e2)

  let not_ (e : bool t) = bool_ @@ Boolean.mk_not ctx @@ extract e
  let plus = op_two_ints int_ @@ list_curry @@ Arithmetic.mk_add ctx
  let minus = op_two_ints int_ @@ list_curry @@ Arithmetic.mk_sub ctx
  let times = op_two_ints int_ @@ list_curry @@ Arithmetic.mk_mul ctx
  let divide = op_two_ints int_ @@ Arithmetic.mk_div ctx
  let modulus = op_two_ints int_ @@ Arithmetic.Integer.mk_mod ctx
  let less_than = op_two_ints bool_ @@ Arithmetic.mk_lt ctx
  let less_than_eq = op_two_ints bool_ @@ Arithmetic.mk_le ctx
  let eq_ints = op_two_ints bool_ @@ Boolean.mk_eq ctx
  let eq_bools = op_two_bools bool_ @@ Boolean.mk_eq ctx
  let neq e1 e2 = not_ @@ (op_two_ints bool_ @@ Boolean.mk_eq ctx) e1 e2
  let and_ = op_two_bools bool_ @@ list_curry @@ Boolean.mk_and ctx
  let or_ = op_two_bools bool_ @@ list_curry @@ Boolean.mk_or ctx
end

module Make (C : Context) = struct
  include Make_datatype_builders (C)
end
