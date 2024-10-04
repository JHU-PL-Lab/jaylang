
open Core
open Z3

module type Context = sig
  val ctx : Z3.context
end

module Make_common_builders (C : Context) = struct
  module Gexpr =
    struct
      type _ t =
        | Int_expr : Z3.Expr.expr -> int t 
        | Bool_expr : Z3.Expr.expr -> bool t

      let extract : type a. a t -> Z3.Expr.expr = function
        | Int_expr e -> e
        | Bool_expr e -> e

      let rec extract_list : type a. a t list -> Z3.Expr.expr list = function
        | [] -> []
        | Int_expr hd :: tl -> hd :: extract_list tl
        | Bool_expr hd :: tl -> hd :: extract_list tl

      let int_ e = Int_expr e
      let bool_ e = Bool_expr e
    end

  open Gexpr

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

  (* use variable expression to query model for int input *)
  let int_of_expr model (Int_expr e) =
    let open Option.Let_syntax in
    Model.get_const_interp_e model e (* check if the expression exists in the model *)
    >>= fun expr -> Model.eval model expr false (* find the value of that int within the model *)
    >>| unbox_int (* get into an ocaml int *)
end

module Make_datatype_builders (C : Context) = struct
  include Make_common_builders (C)

  open Gexpr


  (* let fn fop =
    fun y e1 e2 ->
      eq y @@ fop e1 e2

  let fn_two_ints ret op  =
    fn
    @@ fun (e1 : int t) (e2 : int t) ->
      ret @@ op (extract e1) (extract e2)

  let fn_two_bools ret op  =
    fn
    @@ fun (e1 : bool t) (e2 : bool t) ->
      ret @@ op (extract e1) (extract e2) *)

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

  (* let fn_not y (e : bool t) = eq y @@ bool_ (Boolean.mk_not ctx (extract e))
  let fn_plus = fn_two_ints int_ @@ list_curry @@ Arithmetic.mk_add ctx
  let fn_minus = fn_two_ints int_ @@ list_curry @@ Arithmetic.mk_sub ctx
  let fn_times = fn_two_ints int_ @@ list_curry @@ Arithmetic.mk_mul ctx
  let fn_divide = fn_two_ints int_ @@ Arithmetic.mk_div ctx
  let fn_modulus = fn_two_ints int_ @@ Arithmetic.Integer.mk_mod ctx
  let fn_lt = fn_two_ints bool_ @@ Arithmetic.mk_lt ctx
  let fn_le = fn_two_ints bool_ @@ Arithmetic.mk_le ctx
  let fn_eq_ints = fn_two_ints bool_ @@ Boolean.mk_eq ctx
  let fn_eq_bools = fn_two_bools bool_ @@ Boolean.mk_eq ctx
  let fn_neq = fn_two_ints bool_ @@ (fun e1 e2 -> Boolean.mk_not ctx @@ Boolean.mk_eq ctx e1 e2)
  let fn_and = fn_two_bools bool_ @@ list_curry @@ Boolean.mk_and ctx
  let fn_or = fn_two_bools bool_ @@ list_curry @@ Boolean.mk_or ctx *)
end

module Make (C : Context) = struct
  include Make_datatype_builders (C)
end
