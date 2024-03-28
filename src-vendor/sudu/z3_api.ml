open Core
open Z3_helper
open Z3

type plain =
  | Int of int
  | Bool of bool
  | Fun of string
  | Record of int (* record can have 63 diff labels *)

type case = Int_case | Bool_case | Fun_case | Record_case

(*
     This module is for Z3 formulae working on type
     ```ocaml
     type t = 
     | Int of {i : Z3.int}
     | Bool of {b : Z3.bool}
     | Fun of {fid : Z3.string}
     ```
  *)

(* type value = Sudu.Z3_api.plain =
     | Int of int [@printer Fmt.int]
     | Bool of bool [@printer Fmt.bool]
     | Fun of string [@printer Fmt.string]
     | Record of int [@printer Fmt.int] (* int is bitvector *)
   [@@deriving sexp, compare, equal, show { with_path = false }] *)

module Make_z3_datatype (C : Context) = struct
  open C

  (* making sorts *)
  let intS = Arithmetic.Integer.mk_sort ctx
  let boolS = Boolean.mk_sort ctx
  let strS = Seq.mk_string_sort ctx

  let bvS =
    BitVector.mk_sort ctx 63 (* hardcode 63 bits because we use ocaml int *)

  (* making constructors, checkers, and selectors *)
  let intC =
    Datatype.mk_constructor_s ctx "Int"
      (Symbol.mk_string ctx "is-Int")
      [ Symbol.mk_string ctx "i" ]
      [ Some intS ] [ 1 ]

  let boolC =
    Datatype.mk_constructor_s ctx "J_bool"
      (Symbol.mk_string ctx "is-Bool")
      [ Symbol.mk_string ctx "b" ]
      [ Some boolS ] [ 1 ]

  let funC =
    Datatype.mk_constructor_s ctx "J_fun"
      (Symbol.mk_string ctx "is-Fun")
      [ Symbol.mk_string ctx "fid" ]
      [ Some strS ] [ 1 ]

  let recordC =
    Datatype.mk_constructor_s ctx "J_record"
      (Symbol.mk_string ctx "is-Record")
      [ Symbol.mk_string ctx "r" ]
      [ Some bvS ] [ 1 ]

  (* making *the* sort *)
  let valS = Datatype.mk_sort_s ctx "JIL_Type" [ intC; boolC; funC; recordC ]

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
    (* | [a1]::[a2]::[a3;a4]::[] -> a1, a2, (a3, a4) *)
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
    | Int_case -> ifInt
    | Bool_case -> ifBool
    | Fun_case -> ifFun
    | Record_case -> ifRecord

  let case_to_projecter = function
    | Int_case -> project_int
    | Bool_case -> project_bool
    | Fun_case -> project_string
    | Record_case -> project_record
end

module Make_datatype_builders (C : Context) = struct
  open C
  include Make_helper (C)
  include Make_basic_to_z3_basic (C)
  include Make_z3_datatype (C)

  (* ocaml basic to this datatype *)
  let int_ i = inject_int (box_int i)
  let bool_ b = inject_bool (box_bool b)
  let fun_ s = inject_string (box_string s)

  (* let record_ rid = FuncDecl.apply recordD [ Seq.mk_string ctx rid ] *)
  let record_ bv = inject_record (box_bitvector bv)

  (* z3 basic to this datatype *)

  let true_inj = bool_ true
  let false_inj = bool_ false

  (* let ground_truth = mk_eq true_inj true_inj *)
  let ground_truth = box_bool true
  let ground_false = box_bool false
  let var_s n = Expr.mk_const_s ctx n valS
  let var_sym n = Expr.mk_const ctx n valS
  let var_i i = Expr.mk_const ctx (Symbol.mk_int ctx i) valS

  (* model *)
  let is_case_from_model model e case =
    unbox_bool (eval_exn_ model ((case_to_recognizer case) e))

  let get_unbox_int_exn model e = unbox_int (eval_exn_ model (project_int e))
  let get_unbox_bool_exn model e = unbox_bool (eval_exn_ model (project_bool e))

  let get_unbox_fun_exn model e =
    unbox_string (eval_exn_ model (project_string e))

  let get_unbox_record_exn model e =
    unbox_bitvector (eval_exn_ model (project_record e))

  let get_value model e =
    if is_case_from_model model e Int_case
    then Some (Int (get_unbox_int_exn model e))
    else if is_case_from_model model e Bool_case
    then Some (Bool (get_unbox_bool_exn model e))
    else if is_case_from_model model e Bool_case
    then Some (Fun (get_unbox_fun_exn model e))
    else if is_case_from_model model e Record_case
    then
      let bv = get_unbox_record_exn model e in
      Some (Record bv)
    else None

  (* use variable expression to query model for int input *)
  let get_int_expr model e =
    match get_value model e with
    | Some (Int i) -> Some i
    | Some _ ->
        Logs.warn (fun m -> m "Get non-int for input%s" (Expr.to_string e)) ;
        Some 0
    | None -> None

  let get_int_s model s = get_int_expr model (var_s s)

  let get_bool model e =
    let r = Option.value_exn (Model.eval model e false) in
    match Boolean.get_bool_value r with
    | L_TRUE -> Some true
    | L_FALSE -> Some false
    | L_UNDEF ->
        Logs.warn (fun m -> m "%s L_UNDEF" (Expr.to_string e)) ;
        None
end

module Make_datatype_ops (C : Context) = struct
  include Make_datatype_builders (C)

  let fn_not y e =
    let not_y = e |> project_bool |> mk_not |> inject_bool in
    join [ mk_eq y not_y; ifBool e ]

  let bop prj inj fn e1 e2 = inj (fn (prj e1) (prj e2))

  let typing_two_ints fop y e1 e2 =
    let ey = bop project_int inject_int fop e1 e2 in
    join [ mk_eq y ey; ifInt e1; ifInt e2 ]

  let typing_two_ints_bool fop y e1 e2 =
    let ey = bop project_int inject_bool fop e1 e2 in
    join [ mk_eq y ey; ifInt e1; ifInt e2 ]

  let typing_two_bools fop y e1 e2 =
    let ey = bop project_bool inject_bool fop e1 e2 in
    join [ mk_eq y ey; ifBool e1; ifBool e2 ]

  let fn_plus = typing_two_ints add2
  let fn_minus = typing_two_ints sub2
  let fn_times = typing_two_ints mul2
  let fn_divide = typing_two_ints div
  let fn_modulus = typing_two_ints mod_
  let fn_lt = typing_two_ints_bool lt
  let fn_le = typing_two_ints_bool le
  let fn_eq = typing_two_ints_bool mk_eq
  let fn_neq = typing_two_ints_bool mk_neq
  let fn_and = typing_two_bools and2
  let fn_or = typing_two_bools or2
  (* let fn_xor = typing_two_bools (Boolean.mk_xor ctx) *)
end

module Make (C : Context) = struct
  include C
  include Make_datatype_ops (C)
end
