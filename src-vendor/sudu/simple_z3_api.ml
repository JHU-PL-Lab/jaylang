
open Core
open Z3

module type Context = sig
  val ctx : Z3.context
end

module Make_common_builders (C : Context) = struct
  let ctx = C.ctx

  let eq e1 e2 = Boolean.mk_eq ctx e1 e2
  let or_ = Boolean.mk_or ctx
  let not_ = Boolean.mk_not ctx

  (* box to Z3 expression *)
  let box_int i = Z3.Arithmetic.Integer.mk_numeral_i ctx i
  let box_bool b = Boolean.mk_val ctx b

  (* unbox from Z3 expression *)
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

  (* making *the* sort *)
  let valS = Datatype.mk_sort_s ctx "TypeJayilConcolic" [ intC; boolC ]

  (* making field getters *)
  let getInt, getBool =
    match Datatype.get_accessors valS with
    | [ [ a1 ]; [ a2 ] ] -> (a1, a2)
    | _ -> failwith "accessors mismatch"

  (* making declarations from constructors *)
  let intD = Datatype.Constructor.get_constructor_decl intC
  let boolD = Datatype.Constructor.get_constructor_decl boolC

  (* building Z3 value expressions with the declarations  *)
  let int_ i = FuncDecl.apply intD [ box_int i ]
  let bool_ b = FuncDecl.apply boolD [ box_bool b ]

  (* basic builders *)
  let project_int e = FuncDecl.apply getInt [ e ]
  let var_i i = Expr.mk_const ctx (Symbol.mk_int ctx i) valS (* used to identify variables with a unique int *)

  (* use variable expression to query model for int input *)
  let get_int_expr model e =
    let open Option.Let_syntax in
    Model.get_const_interp_e model e (* check if the expression exists in the model *)
    >>| project_int (* if it does, then because JIL program is well-typed so far, then get the int *)
    >>= fun expr -> Model.eval model expr false (* find the value of that int within the model *)
    >>| unbox_int (* get into an ocaml int *)
end

module Make_datatype_builder_helpers (C : Context) = struct
  include Make_datatype_builders (C)

  (* helper builders *)
  let bop case inj fn e1 e2 =
    let p1 = FuncDecl.apply case [ e1 ] in
    let p2 = FuncDecl.apply case [ e2 ] in
    let p3 = fn p1 p2 in
    FuncDecl.apply inj [ p3 ]

  let fn_two_ints fop y e1 e2 =
    eq y @@ bop getInt intD fop e1 e2

  let fn_two_ints_to_bool fop y e1 e2 =
    eq y @@ bop getInt boolD fop e1 e2

  let fn_two_bools fop y e1 e2 =
    eq y @@ bop getBool boolD fop e1 e2

  (* actual operations on expressions *)

  let fn_not y e1 = eq y @@ FuncDecl.apply boolD [ not_ @@ FuncDecl.apply getBool [ e1 ] ]
  let fn_plus = fn_two_ints @@ fun e1 e2 -> Arithmetic.mk_add ctx [ e1; e2 ]
  let fn_minus = fn_two_ints @@ fun e1 e2 -> Arithmetic.mk_sub ctx [ e1; e2 ]
  let fn_times = fn_two_ints @@ fun e1 e2 -> Arithmetic.mk_mul ctx [ e1; e2 ]
  let fn_divide = fn_two_ints @@ Arithmetic.mk_div ctx
  let fn_modulus = fn_two_ints @@ Arithmetic.Integer.mk_mod ctx
  let fn_lt = fn_two_ints_to_bool @@ Arithmetic.mk_lt ctx
  let fn_le = fn_two_ints_to_bool @@ Arithmetic.mk_le ctx
  let fn_eq = fn_two_ints_to_bool eq
  let fn_neq = fn_two_ints_to_bool @@ fun e1 e2 -> not_ @@ eq e1 e2
  let fn_and = fn_two_bools @@ fun e1 e2 -> Boolean.mk_and ctx [ e1; e2 ]
  let fn_or = fn_two_bools @@ fun e1 e2 -> Boolean.mk_or ctx [ e1; e2 ]
end

module Make (C : Context) = struct
  include Make_datatype_builder_helpers (C)
end
