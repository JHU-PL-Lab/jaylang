
open Core
open Z3

type plain = Int of int | Bool of bool | Fun of int | Record of int
(* Fun and Record payloads are just int identifiers *)

module type Context = sig
  val ctx : Z3.context
end

module Make_common_builders (C : Context) = struct
  let ctx = C.ctx

  let get_model solver status =
    match status with
    | Z3.Solver.SATISFIABLE -> Z3.Solver.get_model solver
    | Z3.Solver.UNSATISFIABLE -> None
    | Z3.Solver.UNKNOWN -> None

  let simplify e = Expr.simplify e None
  let eq e1 e2 = Boolean.mk_eq ctx e1 e2
  let and2 e1 e2 = Boolean.mk_and ctx [ e1; e2 ]
  let and_ = Boolean.mk_and ctx
  let join = and_
  let or_ = Boolean.mk_or ctx
  let not_ = Boolean.mk_not ctx

  (* box to Z3 expression *)
  let box_int i = Z3.Arithmetic.Integer.mk_numeral_i ctx i
  let box_bool b = Boolean.mk_val ctx b

  (* unbox from Z3 expression *)
  let unbox_bool_exn v =
    match Boolean.get_bool_value v with
    | L_TRUE -> true
    | L_FALSE -> false
    | L_UNDEF -> failwith "pass_if_true"

  let unbox_bool v =
    match Boolean.get_bool_value v with
    | L_TRUE -> true
    | L_FALSE -> false
    | L_UNDEF -> false

  let unbox_int e =
    e |> Z3.Arithmetic.Integer.get_big_int |> Big_int_Z.int_of_big_int
end

module Make_datatype_builders (C : Context) = struct
  (*
     This module is for Z3 formulae working on type
     ```ocaml
     type t = 
     | Int of {i : Z3.int}
     | Bool of {b : Z3.bool}
     | Fun of {fid : Z3.int}
     ```
  *)
  include Make_common_builders (C)

  (* making sorts *)
  let intS = Arithmetic.Integer.mk_sort ctx
  let boolS = Boolean.mk_sort ctx

  (* making constructors, checkers, and selectors *)
  let intC =
    Datatype.mk_constructor_s ctx "Int"
      (Symbol.mk_string ctx "is-Int")
      [ Symbol.mk_string ctx "i" ]
      [ Some intS ] [ 1 ]

  let boolC =
    Datatype.mk_constructor_s ctx "Bool"
      (Symbol.mk_string ctx "is-Bool")
      [ Symbol.mk_string ctx "b" ]
      [ Some boolS ] [ 1 ]

  let funC =
    Datatype.mk_constructor_s ctx "Fun"
      (Symbol.mk_string ctx "is-Fun")
      [ Symbol.mk_string ctx "fid" ]
      [ Some intS ] [ 1 ]

  let recordC =
    Datatype.mk_constructor_s ctx "Record"
      (Symbol.mk_string ctx "is-Record")
      [ Symbol.mk_string ctx "r" ]
      [ Some intS ] [ 1 ]

  (* making *the* sort *)
  let valS = Datatype.mk_sort_s ctx "TypOdefa" [ intC; boolC; funC; recordC ]

  (* making recognizers *)
  let intR, boolR, funR, recordR =
    match Datatype.get_recognizers valS with
    | [ r1; r2; r3; r4 ] -> (r1, r2, r3, r4)
    | _ -> failwith "recogniziers mismatch"

  (* building Z3 bool expressions with the recognizers  *)
  let ifInt e = FuncDecl.apply intR [ e ]
  let ifBool e = FuncDecl.apply boolR [ e ]
  let ifFun e = FuncDecl.apply funR [ e ]
  let ifRecord e = FuncDecl.apply recordR [ e ]

  (* making field getters *)
  let getInt, getBool, getFun, getRecord =
    match Datatype.get_accessors valS with
    | [ [ a1 ]; [ a2 ]; [ a3 ]; [ a4 ] ] -> (a1, a2, a3, a4)
    | _ -> failwith "accessors mismatch"

  (* making declarations from constructors *)
  let intD = Datatype.Constructor.get_constructor_decl intC
  let boolD = Datatype.Constructor.get_constructor_decl boolC
  let funD = Datatype.Constructor.get_constructor_decl funC
  let recordD = Datatype.Constructor.get_constructor_decl recordC

  (* building Z3 value expressions with the declarations  *)
  let int_ i = FuncDecl.apply intD [ box_int i ]
  let bool_ b = FuncDecl.apply boolD [ box_bool b ]
  let fun_ i = FuncDecl.apply funD [ box_int i ]
  let record_ i = FuncDecl.apply recordD [ box_int i ]

  (* basic builders *)
  let inject_int e = FuncDecl.apply intD [ e ]
  let inject_bool e = FuncDecl.apply boolD [ e ]
  let project_int e = FuncDecl.apply getInt [ e ]
  let project_bool e = FuncDecl.apply getBool [ e ]
  let true_ = bool_ true
  let false_ = bool_ false
  let var_i i = Expr.mk_const ctx (Symbol.mk_int ctx i) valS (* used to identify variables with a unique int *)

  (* model *)
  let is_int_from_model model e =
    unbox_bool (Option.value_exn (Model.eval model (ifInt e) false))

  let is_bool_from_model model e =
    unbox_bool (Option.value_exn (Model.eval model (ifBool e) false))

  let is_fun_from_model model e =
    unbox_bool (Option.value_exn (Model.eval model (ifFun e) false))

  let is_record_from_model model e =
    unbox_bool (Option.value_exn (Model.eval model (ifRecord e) false))

  let get_int_expr_exn model e =
    Option.value_exn (Model.eval model (project_int e) false)

  let get_bool_expr_exn model e =
    Option.value_exn (Model.eval model (project_bool e) false)

  let get_fun_expr_exn model e =
    Option.value_exn (Model.eval model (project_int e) false)

  let get_record_expr_exn model e =
    Option.value_exn (Model.eval model (project_int e) false)

  let get_unbox_int_exn model e = unbox_int (get_int_expr_exn model e)
  let get_unbox_bool_exn model e = unbox_bool (get_bool_expr_exn model e)
  let get_unbox_fun_exn model e = unbox_int (get_fun_expr_exn model e)
  let get_unbox_record_exn model e = unbox_int (get_record_expr_exn model e)
  let eval_value model e = Option.value_exn (Model.eval model e false)

  let get_value model e =
    if is_int_from_model model e
    then Some (Int (get_unbox_int_exn model e))
    else if is_bool_from_model model e
    then Some (Bool (get_unbox_bool_exn model e))
    else if is_fun_from_model model e
    then
      let fid = get_unbox_fun_exn model e in
      Some (Fun fid)
    else if is_record_from_model model e
    then
      let i = get_unbox_record_exn model e in
      Some (Record i)
    else None

  (* use variable expression to query model for int input *)
  let get_int_expr model e =
    match get_value model e with
    | Some (Int i) -> Some i
    | Some _ ->
        Logs.warn (fun m -> m "Get non-int for input%s" (Z3.Expr.to_string e)) ;
        Some 0
    | None -> None
end

module Make_datatype_builder_helpers (C : Context) = struct
  include Make_datatype_builders (C)

  let fn_not y e1 =
    let b1 = FuncDecl.apply getBool [ e1 ] in
    let by = Boolean.mk_not ctx b1 in
    let ey = FuncDecl.apply boolD [ by ] in
    join [ eq y ey; ifBool e1 ]

  (* helper builders *)
  let bop case inj fn e1 e2 =
    let p1 = FuncDecl.apply case [ e1 ] in
    let p2 = FuncDecl.apply case [ e2 ] in
    let p3 = fn p1 p2 in
    FuncDecl.apply inj [ p3 ]

  let fn_two_ints fop y e1 e2 =
    let ey = bop getInt intD fop e1 e2 in
    join [ eq y ey; ifInt e1; ifInt e2 ]

  let fn_two_ints_to_bool fop y e1 e2 =
    let ey = bop getInt boolD fop e1 e2 in
    join [ eq y ey; ifInt e1; ifInt e2 ]

  let fn_two_bools fop y e1 e2 =
    let ey = bop getBool boolD fop e1 e2 in
    join [ eq y ey; ifBool e1; ifBool e2 ]

  let fn_plus = fn_two_ints (fun e1 e2 -> Arithmetic.mk_add ctx [ e1; e2 ])
  let fn_minus = fn_two_ints (fun e1 e2 -> Arithmetic.mk_sub ctx [ e1; e2 ])
  let fn_times = fn_two_ints (fun e1 e2 -> Arithmetic.mk_mul ctx [ e1; e2 ])
  let fn_divide = fn_two_ints (Arithmetic.mk_div ctx)
  let fn_modulus = fn_two_ints (Arithmetic.Integer.mk_mod ctx)
  let fn_lt = fn_two_ints_to_bool (Arithmetic.mk_lt ctx)
  let fn_le = fn_two_ints_to_bool (Arithmetic.mk_le ctx)
  let fn_eq = fn_two_ints_to_bool (Boolean.mk_eq ctx)

  let fn_neq =
    fn_two_ints_to_bool (fun e1 e2 ->
        Boolean.mk_not ctx @@ Boolean.mk_eq ctx e1 e2)

  let fn_and = fn_two_bools and2
  let fn_or = fn_two_bools (fun e1 e2 -> Boolean.mk_or ctx [ e1; e2 ])
end

module Make (C : Context) = struct
  include Make_datatype_builder_helpers (C)
end
