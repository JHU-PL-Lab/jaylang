
open Core
open Z3

module type S = sig
  type 'a t
  type model
  val set_timeout : Core.Time_float.Span.t -> unit
  val box_int : int -> int t
  val box_bool : bool -> bool t
  val int_var : int -> int t
  val bool_var : int -> bool t
  val value_of_expr : model -> 'a t -> 'a option
  val constrained_vars : model -> int list
  val not_ : bool t -> bool t
  val plus : int t -> int t -> int t
  val minus : int t -> int t -> int t
  val times : int t -> int t -> int t
  val divide : int t -> int t -> int t
  val modulus : int t -> int t -> int t
  val less_than : int t -> int t -> bool t
  val less_than_eq : int t -> int t -> bool t
  val eq_ints : int t -> int t -> bool t
  val eq_bools : bool t -> bool t -> bool t
  val neq : int t -> int t -> bool t
  val and_ : bool t -> bool t -> bool t
  val or_ : bool t -> bool t -> bool t
  module Solve_status : sig
    type t =
      | Sat of model
      | Unknown
      | Unsat
  end
  val empty_model : model
  val solve : bool t list -> Solve_status.t
end

module type Context = sig
  val ctx : Z3.context
end

module Make_common_builders (C : Context) = struct
  include Utils.Separate.Make (struct type t = Z3.Expr.expr end)

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

  let constrained_vars model = 
    List.map ~f:(fun decl -> FuncDecl.get_name decl |> Symbol.get_int)
    @@ Model.get_decls model
end

module Make_datatype_builders (C : Context) = struct
  include Make_common_builders (C)

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

  let bool_expr_list_to_expr ls = 
    Boolean.mk_and ctx
    @@ extract_list ls
end

module Make_solver (C : Context) = struct
  include Make_datatype_builders (C)  

  type model = Z3.Model.model

  module Solve_status = struct
    type t =
      | Sat of model
      | Unknown
      | Unsat
  end

  let solver = Z3.Solver.mk_simple_solver ctx

  (* 
    Somewhat surprisingly, the empty solver has an empty model, but this could
    eventually be the source of a bug if this we cannot always get the value.
  *)
  let empty_model = 
    Option.value_exn
    @@ Z3.Solver.get_model solver

  let set_timeout time =
    time
    |> Time_float.Span.to_ms
    |> Float.iround_up_exn
    |> Int.to_string
    |> Z3.Params.update_param_value ctx "timeout"

  let () = set_timeout (Time_float.Span.of_sec 2.)

  let solve : bool t list -> Solve_status.t = fun bool_formulas ->
    (* It is a bit faster to `and` all formulas together and only run `check` with that one. *)
    (* That is, instead of adding to the solver, keep the solver empty and check the one formula. *)
    let res = Z3.Solver.check solver [ bool_expr_list_to_expr bool_formulas ] in
    match res with
    | Z3.Solver.SATISFIABLE ->
      let model = Z3.Solver.get_model solver in
      Solve_status.Sat (Option.value_exn model)
    | UNKNOWN -> Unknown
    | UNSATISFIABLE -> Unsat
end

module Make (C : Context) = struct
  include Make_solver (C)
end

module New_context () = Make (struct let ctx = Z3.mk_context [] end)
