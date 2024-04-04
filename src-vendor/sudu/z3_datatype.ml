open Core
open Z3
open Z3_helper

(*
  Note:
  We are going to make two final modules that based on two types.
  The final modules are step-wise constructed via module functor-ing.
  We are going to make two core type-aware modules and the rest module functors are type-agnostic.

  The point is the module functors work like polymorphic functions, therefore we should not inspect
  the type (the tags in the variant) in them. Only the two core modules can inspect type and the rest
  are based on the type of that core module.

  This can simply the signatures of module functors. That is, no `with` will be used.

  One remaining question is I thought `case` is for the user-side selection.

if a module is functor-ing on a type, then it shouldn't read the type's concrete tags *)

type t_v1 = Int of int | Bool of bool | Fun of string | Record of string
type case = Int_case | Bool_case | Fun_case | Record_case

let cases = [ Int_case; Bool_case; Fun_case; Record_case ]

module Make_z3_datatype (C : Context) : Jil_z3_datatye with type t = t_v1 =
struct
  type t = t_v1
  type nonrec case = case

  let cases = cases

  open C
  include Make_basic_to_z3_basic (C)

  (* making constructors, checkers, and selectors *)
  let intC =
    Datatype.mk_constructor_s ctx "Int"
      (Symbol.mk_string ctx "is-Int")
      [ Symbol.mk_string ctx "i" ]
      [ Some int_sort ] [ 1 ]

  let boolC =
    Datatype.mk_constructor_s ctx "Bool"
      (Symbol.mk_string ctx "is-Bool")
      [ Symbol.mk_string ctx "b" ]
      [ Some bool_sort ] [ 1 ]

  let funC =
    Datatype.mk_constructor_s ctx "Fun"
      (Symbol.mk_string ctx "is-Fun")
      [ Symbol.mk_string ctx "fid" ]
      [ Some string_sort ] [ 1 ]

  let recordC =
    Datatype.mk_constructor_s ctx "Record"
      (Symbol.mk_string ctx "is-Record")
      [ Symbol.mk_string ctx "rid" ]
      [ Some string_sort ] [ 1 ]

  let the_sort =
    Datatype.mk_sort_s ctx "Jil_type" [ intC; boolC; funC; recordC ]

  (* making recognizers *)
  let mk_int, mk_bool, mk_fun, mk_record =
    match Datatype.get_recognizers the_sort with
    | [ r1; r2; r3; r4 ] -> (r1, r2, r3, r4)
    | _ -> failwith "recogniziers mismatch"

  (* building Z3 bool expressions with the recognizers  *)
  let is_int e = FuncDecl.apply mk_int [ e ]
  let is_bool e = FuncDecl.apply mk_bool [ e ]
  let is_fun e = FuncDecl.apply mk_fun [ e ]
  let is_record e = FuncDecl.apply mk_record [ e ]

  (* making field getters *)
  let getInt, getBool, getFun, getRecord =
    match Datatype.get_accessors the_sort with
    | [ [ a1 ]; [ a2 ]; [ a3 ]; [ a4 ] ] -> (a1, a2, a3, a4)
    | _ -> failwith "accessors mismatch"

  (* making declarations from constructors *)
  let intD = Datatype.Constructor.get_constructor_decl intC
  let boolD = Datatype.Constructor.get_constructor_decl boolC
  let funD = Datatype.Constructor.get_constructor_decl funC
  let recordD = Datatype.Constructor.get_constructor_decl recordC

  (* basic builders *)
  let inject_int e = FuncDecl.apply intD [ e ]
  let inject_bool e = FuncDecl.apply boolD [ e ]
  let inject_string e = FuncDecl.apply funD [ e ]
  let inject_record e = FuncDecl.apply recordD [ e ]
  let project_int e = FuncDecl.apply getInt [ e ]
  let project_bool e = FuncDecl.apply getBool [ e ]
  let project_string e = FuncDecl.apply getFun [ e ]
  let project_record e = FuncDecl.apply getRecord [ e ]

  let case_to_recognizer = function
    | Int_case -> is_int
    | Bool_case -> is_bool
    | Fun_case -> is_fun
    | Record_case -> is_record

  let case_to_injector = function
    | Int_case -> inject_int
    | Bool_case -> inject_bool
    | Fun_case -> inject_string
    | Record_case -> inject_record

  let case_to_projecter = function
    | Int_case -> project_int
    | Bool_case -> project_bool
    | Fun_case -> project_string
    | Record_case -> project_record

  let case_of_value_exn v =
    List.find_exn cases ~f:(fun case ->
        v |> case_to_recognizer case |> simplify |> unbox_bool)

  let project_value case v =
    match case with
    | Int_case -> v |> project_int |> simplify
    | Bool_case -> v |> project_bool |> simplify
    | Fun_case -> v |> project_string |> simplify
    | Record_case -> v |> project_record |> simplify

  let unbox_value v =
    let case = case_of_value_exn v in
    let pv = project_value case v in
    match case with
    | Int_case -> Int (unbox_int pv)
    | Bool_case -> Bool (unbox_bool pv)
    | Fun_case -> Fun (unbox_string pv)
    | Record_case -> Record (unbox_string pv)

  let box_value = function
    | Int i -> i |> box_int |> inject_int
    | Bool b -> b |> box_bool |> inject_bool
    | Fun fs -> fs |> box_string |> inject_string
    | Record rs -> rs |> box_string |> inject_record

  (* let project_unbox_int v =
     if v |> is_int |> simplify |> unbox_bool
     then Some (v |> project_int |> simplify |> unbox_int)
     else None *)

  let eval_value model e =
    let v = eval_exn_ model e in
    Some (unbox_value v)

  let get_int_expr model e =
    match eval_value model e with
    | Some (Int i) -> Some i
    | Some _ ->
        Logs.warn (fun m -> m "Get non-int for input%s" (Z3.Expr.to_string e)) ;
        Some 0
    | None -> None
end

module Make_datatype_builders (JZ : Jil_z3_datatye) (C : Context) = struct
  open JZ
  open C
  include Make_basic_to_z3_basic (C)

  let get_unbox_fun_exn model e =
    unbox_string (eval_exn_ model (project_string e))

  (* ocaml basic to this datatype *)
  let int_ i = i |> box_int |> inject_int
  let bool_ b = b |> box_bool |> inject_bool
  let string_ s = s |> box_string |> inject_string
  let fun_ s = string_ s
  (* let record_ rs = rs |> box_string |> inject_record *)

  let true_ = bool_ true
  let false_ = bool_ false
  let var_s n = Expr.mk_const_s ctx n the_sort
  let var_sym n = Expr.mk_const ctx n the_sort

  let var_i i =
    Expr.mk_const ctx (Symbol.mk_int ctx i)
      the_sort (* used to identify variables with a unique int *)

  let get_int_s model s = get_int_expr model (var_s s)

  let get_bool model e =
    let r = Option.value_exn (Z3.Model.eval model e false) in
    match Z3.Boolean.get_bool_value r with
    | L_TRUE -> Some true
    | L_FALSE -> Some false
    | L_UNDEF ->
        Logs.warn (fun m -> m "%s L_UNDEF" (Z3.Expr.to_string e)) ;
        None
end

module Make_datatype_ops (JZ : Jil_z3_datatye) (C : Context) = struct
  open JZ
  include Make_datatype_builders (JZ) (C)

  let fn_not y e =
    let not_y = e |> project_bool |> not_ |> inject_bool in
    join [ eq y not_y; is_bool e ]

  let bop prj inj fn e1 e2 = inj (fn (prj e1) (prj e2))

  let typing_two_ints fop y e1 e2 =
    let ey = bop project_int inject_int fop e1 e2 in
    join [ eq y ey; is_int e1; is_int e2 ]

  let typing_two_ints_bool fop y e1 e2 =
    let ey = bop project_int inject_bool fop e1 e2 in
    join [ eq y ey; is_int e1; is_int e2 ]

  let typing_two_bools fop y e1 e2 =
    let ey = bop project_bool inject_bool fop e1 e2 in
    join [ eq y ey; is_bool e1; is_bool e2 ]

  let fn_plus = typing_two_ints add2
  let fn_minus = typing_two_ints sub2
  let fn_times = typing_two_ints mul2
  let fn_divide = typing_two_ints div
  let fn_modulus = typing_two_ints mod_
  let fn_lt = typing_two_ints_bool lt
  let fn_le = typing_two_ints_bool le
  let fn_eq = typing_two_ints_bool eq
  let fn_neq = typing_two_ints_bool neq
  let fn_and = typing_two_bools and2
  let fn_or = typing_two_bools or2
  (* let fn_xor = fn_two_bools (Boolean.mk_xor ctx) *)
end

module Make (C : Context) = struct
  include C
  include Make_helper (C)
  include Contextless_functions
  module JZ = Make_z3_datatype (C)
  include JZ
  include Make_datatype_ops (JZ) (C)
end
