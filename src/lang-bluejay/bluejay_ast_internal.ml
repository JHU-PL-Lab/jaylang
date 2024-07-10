open Batteries
open Dj_std.Translation_counter

type label = Bluejay_ast.label = Label of string
[@@deriving eq, ord, show, to_yojson]

type ident = Jayil.Ast.ident = Ident of string
[@@deriving eq, ord, show, to_yojson]

module Ident = Jayil.Ast.Ident
module Ident_set = Jayil.Ast.Ident_set
module Ident_map = Jayil.Ast.Ident_map

type variant_label = Bluejay_ast.variant_label = Variant_label of string
[@@deriving eq, ord, show, to_yojson]

type syntactic_only = [ `Syntactic ]
type semantic_only = [ `Semantic ]
type core_only = [ `Core ]
type 'a syntactic_and_semantic = [< `Syntactic | `Semantic ] as 'a
type 'a core_and_semantic = [< `Core | `Semantic ] as 'a

type type_sig =
  | TopType
  | IntType
  | BoolType
  | FunType
  | RecType of Ident_set.t
  | ListType
  | VariantType of variant_label
  | UntouchedType of string
[@@deriving eq, ord, show, to_yojson]

type pattern = Bluejay_ast.pattern =
  | AnyPat
  | IntPat
  | BoolPat
  | FunPat
  | RecPat of ident option Ident_map.t
  | StrictRecPat of ident option Ident_map.t
  | VariantPat of variant_label * ident
  | VarPat of ident
  | EmptyLstPat
  | LstDestructPat of ident * ident
[@@deriving eq, ord, show, to_yojson]

type predicate = syntactic_only expr_desc
and 'a funsig = Funsig of ident * ident list * 'a expr_desc

and 'a typed_funsig =
  | Typed_funsig of
      ident * (ident * 'a expr_desc) list * ('a expr_desc * 'a expr_desc)
  (* TODO: In the future we may want to change this to argument list accomodate easier user experience *)
  | DTyped_funsig of
      ident * (ident * 'a expr_desc) * ('a expr_desc * 'a expr_desc)

and 'a expr_desc = { body : 'a expr; tag : int }
(*
   P1: no internal transformation -> doesn't need to change tag
   P2: no internal transformation
   P3: HAS internal transformation
*)

and 'a expr =
  | Int : int -> 'a expr
  | Bool : bool -> 'a expr
  | Var : ident -> 'a expr
  | Function : (ident list * 'a expr_desc) -> 'a expr
  | Input : 'a expr
  | Appl : ('a expr_desc * 'a expr_desc) -> 'a expr
  | Let : (ident * 'a expr_desc * 'a expr_desc) -> 'a expr
  | LetRecFun : ('a funsig list * 'a expr_desc) -> 'a expr
  | LetFun : ('a funsig * 'a expr_desc) -> 'a expr
  | LetWithType :
      (ident * 'a expr_desc * 'a expr_desc * 'a expr_desc)
      -> 'a syntactic_and_semantic expr
  | LetRecFunWithType :
      ('a typed_funsig list * 'a expr_desc)
      -> 'a syntactic_and_semantic expr
  | LetFunWithType :
      ('a typed_funsig * 'a expr_desc)
      -> 'a syntactic_and_semantic expr
  | Plus : ('a expr_desc * 'a expr_desc) -> 'a expr
  | Minus : ('a expr_desc * 'a expr_desc) -> 'a expr
  | Times : ('a expr_desc * 'a expr_desc) -> 'a expr
  | Divide : ('a expr_desc * 'a expr_desc) -> 'a expr
  | Modulus : ('a expr_desc * 'a expr_desc) -> 'a expr
  | Equal : ('a expr_desc * 'a expr_desc) -> 'a expr
  | Neq : ('a expr_desc * 'a expr_desc) -> 'a expr
  | LessThan : ('a expr_desc * 'a expr_desc) -> 'a expr
  | Leq : ('a expr_desc * 'a expr_desc) -> 'a expr
  | GreaterThan : ('a expr_desc * 'a expr_desc) -> 'a expr
  | Geq : ('a expr_desc * 'a expr_desc) -> 'a expr
  | And : ('a expr_desc * 'a expr_desc) -> 'a expr
  | Or : ('a expr_desc * 'a expr_desc) -> 'a expr
  | Not : 'a expr_desc -> 'a expr
  | If : ('a expr_desc * 'a expr_desc * 'a expr_desc) -> 'a expr
  | Record : 'a expr_desc Ident_map.t -> 'a expr
  | RecordProj : ('a expr_desc * label) -> 'a expr
  | Match : ('a expr_desc * (pattern * 'a expr_desc) list) -> 'a expr
  | VariantExpr : (variant_label * 'a expr_desc) -> 'a expr
  | List : 'a expr_desc list -> 'a expr
  | ListCons : ('a expr_desc * 'a expr_desc) -> 'a expr
  (* TODO: Create a separate class of constructors for type errors? *)
  | TypeError : ident -> 'a expr
  | Assert : 'a expr_desc -> 'a expr
  | Assume : 'a expr_desc -> 'a expr
  (* Type expressions *)
  | TypeVar : ident -> syntactic_only expr
  | TypeInt : syntactic_only expr
  | TypeBool : syntactic_only expr
  | TypeRecord : syntactic_only expr_desc Ident_map.t -> syntactic_only expr
  | TypeList : syntactic_only expr_desc -> syntactic_only expr
  | TypeArrow :
      (syntactic_only expr_desc * syntactic_only expr_desc)
      -> syntactic_only expr
  | TypeArrowD :
      ((ident * syntactic_only expr_desc) * syntactic_only expr_desc)
      -> syntactic_only expr
  | TypeSet : syntactic_only expr_desc * predicate -> syntactic_only expr
  | TypeUnion :
      (syntactic_only expr_desc * syntactic_only expr_desc)
      -> syntactic_only expr
  | TypeIntersect :
      (syntactic_only expr_desc * syntactic_only expr_desc)
      -> syntactic_only expr
  | TypeRecurse : (ident * syntactic_only expr_desc) -> syntactic_only expr
  | TypeUntouched : string -> syntactic_only expr
  | TypeVariant :
      (variant_label * syntactic_only expr_desc) list
      -> syntactic_only expr

let new_expr_desc : type a. a expr -> a expr_desc =
 fun e -> { tag = fresh_n (); body = e }

type syn_type_bluejay = syntactic_only expr
type syn_bluejay_edesc = syntactic_only expr_desc

(* type syn_type_bluejay_desc = syntactic_only expr_desc *)

type sem_type_bluejay = [ `Semantic ] expr
type sem_bluejay_edesc = [ `Semantic ] expr_desc

(* type sem_type_bluejay_desc = [ `Semantic ] expr *)

type core_bluejay = [ `Core ] expr
type core_bluejay_edesc = [ `Core ] expr_desc

let rec equal_funsig : type a. a funsig -> a funsig -> bool =
 fun (Funsig (id1, params1, fe1)) (Funsig (id2, params2, fe2)) ->
  id1 = id2 && List.eq equal_ident params1 params2 && equal_expr_desc fe1 fe2

and equal_typed_funsig : type a. a typed_funsig -> a typed_funsig -> bool =
 fun fsig_1 fsig_2 ->
  match (fsig_1, fsig_2) with
  | ( Typed_funsig (f1, params_with_type_1, (f_body_1, ret_type_1)),
      Typed_funsig (f2, params_with_type_2, (f_body_2, ret_type_2)) ) ->
      equal_ident f1 f2
      && List.equal
           (fun (param1, t1) (param2, t2) ->
             equal_ident param1 param2 && equal_expr_desc t1 t2)
           params_with_type_1 params_with_type_2
      && equal_expr_desc f_body_1 f_body_2
      && equal_expr_desc ret_type_1 ret_type_2
  | ( DTyped_funsig (f1, (param1, t1), (f_body_1, ret_type_1)),
      DTyped_funsig (f2, (param2, t2), (f_body_2, ret_type_2)) ) ->
      equal_ident f1 f2 && equal_ident param1 param2 && equal_expr_desc t1 t2
      && equal_expr_desc f_body_1 f_body_2
      && equal_expr_desc ret_type_1 ret_type_2
  | _ -> false

and equal_expr_desc : type a. a expr_desc -> a expr_desc -> bool =
 fun e1 e2 ->
  equal_expr e1.body e2.body
  && (* Option.eq e1.tag e2.tag *)
  e1.tag = e2.tag

and equal_expr : type a. a expr -> a expr -> bool =
 fun e1 e2 ->
  match (e1, e2) with
  | Int n1, Int n2 -> n1 = n2
  (* | Int _, _ -> false *)
  | Bool b1, Bool b2 -> b1 = b2
  (* | Bool _, _ -> false *)
  | Input, Input -> true
  (* | Input, _ -> false *)
  | Var x1, Var x2 -> x1 = x2
  (* | Var _, _ -> false *)
  | List l1, List l2 -> List.eq equal_expr_desc l1 l2
  (* | List _, _ -> false *)
  | Record r1, Record r2 -> Ident_map.equal equal_expr_desc r1 r2
  (* | Record _, _ -> false *)
  | Function (id_lst1, fun_body1), Function (id_lst2, fun_body2) ->
      List.eq equal_ident id_lst1 id_lst2 && equal_expr_desc fun_body1 fun_body2
  (* | Function _, _ -> false *)
  | Let (x1, xe1, e1), Let (x2, xe2, e2) ->
      x1 = x2 && equal_expr_desc xe1 xe2 && equal_expr_desc e1 e2
  (* | Let _, _ -> false *)
  | LetFun (f1, e1), LetFun (f2, e2) ->
      equal_funsig f1 f2 && equal_expr_desc e1 e2
  (* | LetFun _, _ -> false *)
  | LetRecFun (sig_lst1, e1), LetRecFun (sig_lst2, e2) ->
      List.eq equal_funsig sig_lst1 sig_lst2 && equal_expr_desc e1 e2
  (* | LetRecFun _, _ -> false *)
  | LetWithType (x1, xe1, e1, t1), LetWithType (x2, xe2, e2, t2) ->
      x1 = x2 && equal_expr_desc xe1 xe2 && equal_expr_desc e1 e2
      && equal_expr_desc t1 t2
  (* | LetWithType _, _ -> false *)
  | LetFunWithType (f1, e1), LetFunWithType (f2, e2) ->
      equal_typed_funsig f1 f2 && equal_expr_desc e1 e2
  (* | LetFunWithType _, _ -> false *)
  | LetRecFunWithType (sig_lst1, e1), LetRecFunWithType (sig_lst2, e2) ->
      List.eq equal_typed_funsig sig_lst1 sig_lst2 && equal_expr_desc e1 e2
  (* | LetRecFunWithType _, _ -> false *)
  | Match (me1, pe_lst1), Match (me2, pe_lst2) ->
      let eq_pe (p1, e1) (p2, e2) = p1 = p2 && equal_expr_desc e1 e2 in
      equal_expr_desc me1 me2 && List.eq eq_pe pe_lst1 pe_lst2
  (* | Match _, _ -> false *)
  | If (cond1, tb1, fb1), If (cond2, tb2, fb2) ->
      equal_expr_desc cond1 cond2
      && equal_expr_desc tb1 tb2 && equal_expr_desc fb1 fb2
  (* | If _, _ -> false *)
  | Or (lop1, rop1), Or (lop2, rop2)
  | And (lop1, rop1), And (lop2, rop2)
  | Equal (lop1, rop1), Equal (lop2, rop2)
  | Neq (lop1, rop1), Neq (lop2, rop2)
  | LessThan (lop1, rop1), LessThan (lop2, rop2)
  | Leq (lop1, rop1), Leq (lop2, rop2)
  | GreaterThan (lop1, rop1), GreaterThan (lop2, rop2)
  | Geq (lop1, rop1), Geq (lop2, rop2)
  | Appl (lop1, rop1), Appl (lop2, rop2)
  | Plus (lop1, rop1), Plus (lop2, rop2)
  | Minus (lop1, rop1), Minus (lop2, rop2)
  | Times (lop1, rop1), Times (lop2, rop2)
  | Divide (lop1, rop1), Divide (lop2, rop2)
  | Modulus (lop1, rop1), Modulus (lop2, rop2)
  | ListCons (lop1, rop1), ListCons (lop2, rop2) ->
      equal_expr_desc lop1 lop2 && equal_expr_desc rop1 rop2
  (* | Or _, _
     | And _, _
     | Equal _, _
     | Neq _, _
     | LessThan _, _
     | Leq _, _
     | GreaterThan _, _
     | Geq _, _
     | Appl _, _
     | Plus _, _
     | Minus _, _
     | Times _, _
     | Divide _, _
     | Modulus _, _
     | ListCons _, _ -> false *)
  | Assert e1, Assert e2 | Assume e1, Assume e2 | Not e1, Not e2 ->
      equal_expr_desc e1 e2
  | VariantExpr (l1, e1), VariantExpr (l2, e2) ->
      l1 = l2 && equal_expr_desc e1 e2
  | RecordProj (e1, l1), RecordProj (e2, l2) -> l1 = l2 && equal_expr_desc e1 e2
  (* Type expressions *)
  | TypeVar x1, TypeVar x2 -> x1 = x2
  | TypeInt, TypeInt | TypeBool, TypeBool -> true
  | TypeRecord t1, TypeRecord t2 -> Ident_map.equal equal_expr_desc t1 t2
  | TypeList t1, TypeList t2 -> equal_expr_desc t1 t2
  | TypeArrow (lt1, rt1), TypeArrow (lt2, rt2)
  | TypeUnion (lt1, rt1), TypeUnion (lt2, rt2)
  | TypeIntersect (lt1, rt1), TypeIntersect (lt2, rt2)
  | TypeSet (lt1, rt1), TypeSet (lt2, rt2) ->
      equal_expr_desc lt1 lt2 && equal_expr_desc rt1 rt2
  | TypeArrowD ((id1, lt1), rt1), TypeArrowD ((id2, lt2), rt2) ->
      id1 = id2 && equal_expr_desc lt1 lt2 && equal_expr_desc rt1 rt2
  | TypeRecurse (x1, t1), TypeRecurse (x2, t2) -> x1 = x2 && t1 = t2
  | TypeUntouched s1, TypeUntouched s2 -> s1 = s2
  | TypeVariant l1, TypeVariant l2 ->
      List.equal
        (fun (l1, ve1) (l2, ve2) -> l1 = l2 && equal_expr_desc ve1 ve2)
        l1 l2
  | _ -> false

let rec tagless_equal_funsig : type a. a funsig -> a funsig -> bool =
 fun (Funsig (id1, params1, fe1)) (Funsig (id2, params2, fe2)) ->
  id1 = id2
  && List.eq equal_ident params1 params2
  && tagless_equal_expr_desc fe1 fe2

and tagless_equal_typed_funsig :
    type a. a typed_funsig -> a typed_funsig -> bool =
 fun fsig_1 fsig_2 ->
  match (fsig_1, fsig_2) with
  | ( Typed_funsig (f1, params_with_type_1, (f_body_1, ret_type_1)),
      Typed_funsig (f2, params_with_type_2, (f_body_2, ret_type_2)) ) ->
      equal_ident f1 f2
      && List.equal
           (fun (param1, t1) (param2, t2) ->
             equal_ident param1 param2 && tagless_equal_expr_desc t1 t2)
           params_with_type_1 params_with_type_2
      && tagless_equal_expr_desc f_body_1 f_body_2
      && tagless_equal_expr_desc ret_type_1 ret_type_2
  | ( DTyped_funsig (f1, (param1, t1), (f_body_1, ret_type_1)),
      DTyped_funsig (f2, (param2, t2), (f_body_2, ret_type_2)) ) ->
      equal_ident f1 f2 && equal_ident param1 param2
      && tagless_equal_expr_desc t1 t2
      && tagless_equal_expr_desc f_body_1 f_body_2
      && tagless_equal_expr_desc ret_type_1 ret_type_2
  | _ -> false

and tagless_equal_expr_desc : type a. a expr_desc -> a expr_desc -> bool =
 fun e1 e2 -> tagless_equal_expr e1.body e2.body

and tagless_equal_expr : type a. a expr -> a expr -> bool =
 fun e1 e2 ->
  match (e1, e2) with
  | Int n1, Int n2 -> n1 = n2
  (* | Int _, _ -> false *)
  | Bool b1, Bool b2 -> b1 = b2
  (* | Bool _, _ -> false *)
  | Input, Input -> true
  (* | Input, _ -> false *)
  | Var x1, Var x2 -> x1 = x2
  (* | Var _, _ -> false *)
  | List l1, List l2 -> List.eq tagless_equal_expr_desc l1 l2
  (* | List _, _ -> false *)
  | Record r1, Record r2 -> Ident_map.equal tagless_equal_expr_desc r1 r2
  (* | Record _, _ -> false *)
  | Function (id_lst1, fun_body1), Function (id_lst2, fun_body2) ->
      List.eq equal_ident id_lst1 id_lst2
      && tagless_equal_expr_desc fun_body1 fun_body2
  (* | Function _, _ -> false *)
  | Let (x1, xe1, e1), Let (x2, xe2, e2) ->
      x1 = x2
      && tagless_equal_expr_desc xe1 xe2
      && tagless_equal_expr_desc e1 e2
  (* | Let _, _ -> false *)
  | LetFun (f1, e1), LetFun (f2, e2) ->
      equal_funsig f1 f2 && tagless_equal_expr_desc e1 e2
  (* | LetFun _, _ -> false *)
  | LetRecFun (sig_lst1, e1), LetRecFun (sig_lst2, e2) ->
      List.eq equal_funsig sig_lst1 sig_lst2 && tagless_equal_expr_desc e1 e2
  (* | LetRecFun _, _ -> false *)
  | LetWithType (x1, xe1, e1, t1), LetWithType (x2, xe2, e2, t2) ->
      x1 = x2
      && tagless_equal_expr_desc xe1 xe2
      && tagless_equal_expr_desc e1 e2
      && tagless_equal_expr_desc t1 t2
  (* | LetWithType _, _ -> false *)
  | LetFunWithType (f1, e1), LetFunWithType (f2, e2) ->
      equal_typed_funsig f1 f2 && tagless_equal_expr_desc e1 e2
  (* | LetFunWithType _, _ -> false *)
  | LetRecFunWithType (sig_lst1, e1), LetRecFunWithType (sig_lst2, e2) ->
      List.eq equal_typed_funsig sig_lst1 sig_lst2
      && tagless_equal_expr_desc e1 e2
  (* | LetRecFunWithType _, _ -> false *)
  | Match (me1, pe_lst1), Match (me2, pe_lst2) ->
      let eq_pe (p1, e1) (p2, e2) = p1 = p2 && tagless_equal_expr_desc e1 e2 in
      tagless_equal_expr_desc me1 me2 && List.eq eq_pe pe_lst1 pe_lst2
  (* | Match _, _ -> false *)
  | If (cond1, tb1, fb1), If (cond2, tb2, fb2) ->
      tagless_equal_expr_desc cond1 cond2
      && tagless_equal_expr_desc tb1 tb2
      && tagless_equal_expr_desc fb1 fb2
  (* | If _, _ -> false *)
  | Or (lop1, rop1), Or (lop2, rop2)
  | And (lop1, rop1), And (lop2, rop2)
  | Equal (lop1, rop1), Equal (lop2, rop2)
  | Neq (lop1, rop1), Neq (lop2, rop2)
  | LessThan (lop1, rop1), LessThan (lop2, rop2)
  | Leq (lop1, rop1), Leq (lop2, rop2)
  | GreaterThan (lop1, rop1), GreaterThan (lop2, rop2)
  | Geq (lop1, rop1), Geq (lop2, rop2)
  | Appl (lop1, rop1), Appl (lop2, rop2)
  | Plus (lop1, rop1), Plus (lop2, rop2)
  | Minus (lop1, rop1), Minus (lop2, rop2)
  | Times (lop1, rop1), Times (lop2, rop2)
  | Divide (lop1, rop1), Divide (lop2, rop2)
  | Modulus (lop1, rop1), Modulus (lop2, rop2)
  | ListCons (lop1, rop1), ListCons (lop2, rop2) ->
      tagless_equal_expr_desc lop1 lop2 && tagless_equal_expr_desc rop1 rop2
  (* | Or _, _
     | And _, _
     | Equal _, _
     | Neq _, _
     | LessThan _, _
     | Leq _, _
     | GreaterThan _, _
     | Geq _, _
     | Appl _, _
     | Plus _, _
     | Minus _, _
     | Times _, _
     | Divide _, _
     | Modulus _, _
     | ListCons _, _ -> false *)
  | Assert e1, Assert e2 | Assume e1, Assume e2 | Not e1, Not e2 ->
      tagless_equal_expr_desc e1 e2
  | VariantExpr (l1, e1), VariantExpr (l2, e2) ->
      l1 = l2 && tagless_equal_expr_desc e1 e2
  | RecordProj (e1, l1), RecordProj (e2, l2) ->
      l1 = l2 && tagless_equal_expr_desc e1 e2
  (* Type expressions *)
  | TypeVar x1, TypeVar x2 -> x1 = x2
  | TypeInt, TypeInt | TypeBool, TypeBool -> true
  | TypeRecord t1, TypeRecord t2 ->
      Ident_map.equal tagless_equal_expr_desc t1 t2
  | TypeList t1, TypeList t2 -> tagless_equal_expr_desc t1 t2
  | TypeArrow (lt1, rt1), TypeArrow (lt2, rt2)
  | TypeUnion (lt1, rt1), TypeUnion (lt2, rt2)
  | TypeIntersect (lt1, rt1), TypeIntersect (lt2, rt2)
  | TypeSet (lt1, rt1), TypeSet (lt2, rt2) ->
      tagless_equal_expr_desc lt1 lt2 && tagless_equal_expr_desc rt1 rt2
  | TypeArrowD ((id1, lt1), rt1), TypeArrowD ((id2, lt2), rt2) ->
      id1 = id2
      && tagless_equal_expr_desc lt1 lt2
      && tagless_equal_expr_desc rt1 rt2
  | TypeRecurse (x1, t1), TypeRecurse (x2, t2) -> x1 = x2 && t1 = t2
  | TypeUntouched s1, TypeUntouched s2 -> s1 = s2
  | TypeVariant l1, TypeVariant l2 ->
      List.equal
        (fun (l1, ve1) (l2, ve2) -> l1 = l2 && tagless_equal_expr_desc ve1 ve2)
        l1 l2
  | _ -> false

let compare_helper (x : int) (y : int) : int = if x <> 0 then x else y

let rec compare_funsig : type a. a funsig -> a funsig -> int =
 fun (Funsig (id1, params1, fe1)) (Funsig (id2, params2, fe2)) ->
  compare id1 id2
  |> compare_helper (List.compare compare_ident params1 params2)
  |> compare_helper (compare_expr_desc fe1 fe2)

and compare_typed_funsig : type a. a typed_funsig -> a typed_funsig -> int =
 fun fsig_1 fsig_2 ->
  match (fsig_1, fsig_2) with
  | ( Typed_funsig (f1, params_with_type_1, (f_body_1, ret_type_1)),
      Typed_funsig (f2, params_with_type_2, (f_body_2, ret_type_2)) ) ->
      compare_ident f1 f2
      |> compare_helper
         @@ List.compare
              (fun (param1, t1) (param2, t2) ->
                compare_ident param1 param2
                |> compare_helper @@ compare_expr_desc t1 t2)
              params_with_type_1 params_with_type_2
      |> compare_helper @@ compare_expr_desc f_body_1 f_body_2
      |> compare_helper @@ compare_expr_desc ret_type_1 ret_type_2
  | ( DTyped_funsig (f1, (param1, t1), (f_body_1, ret_type_1)),
      DTyped_funsig (f2, (param2, t2), (f_body_2, ret_type_2)) ) ->
      compare_ident f1 f2
      |> compare_helper @@ compare_ident param1 param2
      |> compare_helper @@ compare_expr_desc t1 t2
      |> compare_helper @@ compare_expr_desc f_body_1 f_body_2
      |> compare_helper @@ compare_expr_desc ret_type_1 ret_type_2
  | DTyped_funsig _, Typed_funsig _ -> 1
  | Typed_funsig _, DTyped_funsig _ -> -1

and compare_expr_desc : type a. a expr_desc -> a expr_desc -> int =
 fun e1 e2 ->
  compare_expr e1.body e2.body |> compare_helper (compare e1.tag e2.tag)

and compare_expr : type a. a expr -> a expr -> int =
 fun e1 e2 ->
  match (e1, e2) with
  | Int n1, Int n2 -> compare n1 n2
  | Bool b1, Bool b2 -> compare b1 b2
  | Input, Input -> 0
  | Var x1, Var x2 -> compare x1 x2
  | List l1, List l2 -> List.compare compare_expr_desc l1 l2
  | Record r1, Record r2 -> Ident_map.compare compare_expr_desc r1 r2
  | Function (id_lst1, fun_body1), Function (id_lst2, fun_body2) ->
      List.compare compare_ident id_lst1 id_lst2
      |> compare_helper (compare_expr_desc fun_body1 fun_body2)
  | Let (x1, xe1, e1), Let (x2, xe2, e2) ->
      compare x1 x2
      |> compare_helper (compare_expr_desc xe1 xe2)
      |> compare_helper (compare_expr_desc e1 e2)
  | LetFun (f1, e1), LetFun (f2, e2) ->
      compare_funsig f1 f2 |> compare_helper (compare_expr_desc e1 e2)
  | LetRecFun (sig_lst1, e1), LetRecFun (sig_lst2, e2) ->
      List.compare compare_funsig sig_lst1 sig_lst2 + compare_expr_desc e1 e2
  | LetWithType (x1, xe1, e1, t1), LetWithType (x2, xe2, e2, t2) ->
      compare x1 x2
      |> compare_helper (compare_expr_desc xe1 xe2)
      |> compare_helper (compare_expr_desc e1 e2)
      |> compare_helper (compare_expr_desc t1 t2)
  | LetFunWithType (f1, e1), LetFunWithType (f2, e2) ->
      compare_typed_funsig f1 f2 |> compare_helper (compare_expr_desc e1 e2)
  | LetRecFunWithType (sig_lst1, e1), LetRecFunWithType (sig_lst2, e2) ->
      List.compare compare_typed_funsig sig_lst1 sig_lst2
      |> compare_helper (compare_expr_desc e1 e2)
  | Match (me1, pe_lst1), Match (me2, pe_lst2) ->
      let compare_pe (p1, e1) (p2, e2) =
        compare_pattern p1 p2 |> compare_helper (compare_expr_desc e1 e2)
      in
      compare_expr_desc me1 me2
      |> compare_helper (List.compare compare_pe pe_lst1 pe_lst2)
  | If (cond1, tb1, fb1), If (cond2, tb2, fb2) ->
      compare_expr_desc cond1 cond2
      |> compare_helper (compare_expr_desc tb1 tb2)
      |> compare_helper (compare_expr_desc fb1 fb2)
  | Or (lop1, rop1), Or (lop2, rop2)
  | And (lop1, rop1), And (lop2, rop2)
  | Equal (lop1, rop1), Equal (lop2, rop2)
  | Neq (lop1, rop1), Neq (lop2, rop2)
  | LessThan (lop1, rop1), LessThan (lop2, rop2)
  | Leq (lop1, rop1), Leq (lop2, rop2)
  | GreaterThan (lop1, rop1), GreaterThan (lop2, rop2)
  | Geq (lop1, rop1), Geq (lop2, rop2)
  | Appl (lop1, rop1), Appl (lop2, rop2)
  | Plus (lop1, rop1), Plus (lop2, rop2)
  | Minus (lop1, rop1), Minus (lop2, rop2)
  | Times (lop1, rop1), Times (lop2, rop2)
  | Divide (lop1, rop1), Divide (lop2, rop2)
  | Modulus (lop1, rop1), Modulus (lop2, rop2)
  | ListCons (lop1, rop1), ListCons (lop2, rop2) ->
      compare_expr_desc lop1 lop2
      |> compare_helper (compare_expr_desc rop1 rop2)
  | Assert e1, Assert e2 | Assume e1, Assume e2 | Not e1, Not e2 ->
      compare_expr_desc e1 e2
  | VariantExpr (l1, e1), VariantExpr (l2, e2) ->
      compare l1 l2 |> compare_helper (compare_expr_desc e1 e2)
  | RecordProj (e1, l1), RecordProj (e2, l2) ->
      compare l1 l2 |> compare_helper (compare_expr_desc e1 e2)
  (* Type expressions *)
  | TypeVar x1, TypeVar x2 -> compare x1 x2
  | TypeInt, TypeInt | TypeBool, TypeBool -> 0
  | TypeRecord t1, TypeRecord t2 -> Ident_map.compare compare_expr_desc t1 t2
  | TypeList t1, TypeList t2 -> compare_expr_desc t1 t2
  | TypeArrow (lt1, rt1), TypeArrow (lt2, rt2)
  | TypeUnion (lt1, rt1), TypeUnion (lt2, rt2)
  | TypeIntersect (lt1, rt1), TypeIntersect (lt2, rt2)
  | TypeSet (lt1, rt1), TypeSet (lt2, rt2) ->
      compare_expr_desc lt1 lt2 + compare_expr_desc rt1 rt2
  | TypeArrowD ((id1, lt1), rt1), TypeArrowD ((id2, lt2), rt2) ->
      compare id1 id2
      |> compare_helper (compare_expr_desc lt1 lt2)
      |> compare_helper (compare_expr_desc rt1 rt2)
  | TypeRecurse (x1, t1), TypeRecurse (x2, t2) ->
      compare x1 x2 |> compare_helper (compare t1 t2)
  | TypeUntouched s1, TypeUntouched s2 -> compare s1 s2
  | TypeVariant l1, TypeVariant l2 ->
      List.compare
        (fun (l1, ve1) (l2, ve2) ->
          compare l1 l2 |> compare_helper (compare_expr_desc ve1 ve2))
        l1 l2
  (* TODO: Another potential source for bug *)
  | Int _, _ -> 1
  | _, Int _ -> -1
  | Bool _, _ -> 1
  | _, Bool _ -> -1
  | Var _, _ -> 1
  | _, Var _ -> -1
  | Function _, _ -> 1
  | _, Function _ -> -1
  | Input, _ -> 1
  | _, Input -> -1
  | Appl _, _ -> 1
  | _, Appl _ -> -1
  | Let _, _ -> 1
  | _, Let _ -> -1
  | LetRecFun _, _ -> 1
  | _, LetRecFun _ -> -1
  | LetFun _, _ -> 1
  | _, LetFun _ -> -1
  | LetWithType _, _ -> 1
  | _, LetWithType _ -> -1
  | LetRecFunWithType _, _ -> 1
  | _, LetRecFunWithType _ -> -1
  | LetFunWithType _, _ -> 1
  | _, LetFunWithType _ -> -1
  | Plus _, _ -> 1
  | _, Plus _ -> -1
  | Minus _, _ -> 1
  | _, Minus _ -> -1
  | Times _, _ -> 1
  | _, Times _ -> -1
  | Divide _, _ -> 1
  | _, Divide _ -> -1
  | Modulus _, _ -> 1
  | _, Modulus _ -> -1
  | Equal _, _ -> 1
  | _, Equal _ -> -1
  | Neq _, _ -> 1
  | _, Neq _ -> -1
  | LessThan _, _ -> 1
  | _, LessThan _ -> -1
  | Leq _, _ -> 1
  | _, Leq _ -> -1
  | GreaterThan _, _ -> 1
  | _, GreaterThan _ -> -1
  | Geq _, _ -> 1
  | _, Geq _ -> -1
  | And _, _ -> 1
  | _, And _ -> -1
  | Or _, _ -> 1
  | _, Or _ -> -1
  | Not _, _ -> 1
  | _, Not _ -> -1
  | If _, _ -> 1
  | _, If _ -> -1
  | Record _, _ -> 1
  | _, Record _ -> -1
  | RecordProj _, _ -> 1
  | _, RecordProj _ -> -1
  | Match _, _ -> 1
  | _, Match _ -> -1
  | VariantExpr _, _ -> 1
  | _, VariantExpr _ -> -1
  | List _, _ -> 1
  | _, List _ -> -1
  | ListCons _, _ -> 1
  | _, ListCons _ -> -1
  | TypeError _, _ -> 1
  | _, TypeError _ -> -1
  | Assert _, _ -> 1
  | _, Assert _ -> -1
  | Assume _, _ -> 1
  | _, Assume _ -> -1
  | TypeVar _, _ -> 1
  | _, TypeVar _ -> -1
  | TypeInt, _ -> 1
  | _, TypeInt -> -1
  | TypeBool, _ -> 1
  | _, TypeBool -> -1
  | TypeRecord _, _ -> 1
  | _, TypeRecord _ -> -1
  | TypeList _, _ -> 1
  | _, TypeList _ -> -1
  | TypeArrow _, _ -> 1
  | _, TypeArrow _ -> -1
  | TypeArrowD _, _ -> 1
  | _, TypeArrowD _ -> -1
  | TypeSet _, _ -> 1
  | _, TypeSet _ -> -1
  | TypeUnion _, _ -> 1
  | _, TypeUnion _ -> -1
  | TypeIntersect _, _ -> 1
  | _, TypeIntersect _ -> -1
  | TypeRecurse _, _ -> 1
  | _, TypeRecurse _ -> -1
  | TypeUntouched _, _ -> 1
  | _, TypeUntouched _ -> -1

module type Expr_desc = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Typed_expr_desc : Expr_desc with type t = syn_bluejay_edesc = struct
  type t = syn_bluejay_edesc

  let equal = equal_expr_desc
  let compare = compare_expr_desc
end

module Semantic_typed_expr_desc : Expr_desc with type t = sem_bluejay_edesc =
struct
  type t = sem_bluejay_edesc

  let equal = equal_expr_desc
  let compare = compare_expr_desc
end

module Core_expr_desc : Expr_desc with type t = core_bluejay_edesc = struct
  type t = core_bluejay_edesc

  let equal = equal_expr_desc
  let compare = compare_expr_desc
end

module Pattern = struct
  type t = pattern

  let equal = equal_pattern
  let compare = compare_pattern
  let to_yojson = pattern_to_yojson
end

(* Takes [expr] as an argument.  Returns the relative precedence of the
    expression.  Higher ints correspond to higher precedences. *)
let expr_precedence_p1 : type a. a expr -> int =
 fun expr ->
  match expr with
  | Function _ | Let _ | LetFun _ | LetRecFun _ | LetWithType _
  | LetFunWithType _ | LetRecFunWithType _ | Match _ ->
      0
  | If _ -> 1
  | Or _ -> 2
  | And _ -> 3
  | Not _ -> 4
  | Equal _ | Neq _ | LessThan _ | Leq _ | GreaterThan _ | Geq _ -> 5
  | ListCons _ -> 6
  | Plus _ | Minus _ -> 7
  | Times _ | Divide _ | Modulus _ -> 8
  | Assert _ | Assume _ | VariantExpr _ -> 9
  | Appl _ -> 10
  | RecordProj _ -> 11
  | Int _ | Bool _ | Input | Var _ | List _ | Record _ -> 12
  (* TODO: For now, all type expressions will have the lowest precedence coz I'm lazy and don't wanna think about it *)
  | TypeVar _ | TypeInt | TypeBool | TypeRecord _ | TypeList _ | TypeArrow _
  | TypeArrowD _ | TypeSet _ | TypeUnion _ | TypeIntersect _ | TypeRecurse _
  | TypeError _ | TypeUntouched _ | TypeVariant _ ->
      13

(** Takes expressions [e1] and [e2] as arguments. Returns 0 if the two
    expressions have equal precedence, a negative int if [e1] has lower
    precedence than [e2], and a positive int if [e1] has higher precedence. *)
let expr_precedence_cmp e1 e2 = expr_precedence_p1 e1 - expr_precedence_p1 e2

let expr_desc_precedence_cmp : type a. a expr_desc -> a expr_desc -> int =
 fun ed1 ed2 -> expr_precedence_cmp ed1.body ed2.body

(* Helper routines to transform internal bluejay to external bluejay *)

let rec from_internal_expr_desc (e : syn_bluejay_edesc) : Bluejay_ast.expr_desc
    =
  let tag' = e.tag in
  let e' = from_internal_expr e.body in
  { tag = tag'; body = e' }

and transform_funsig (fun_sig : 'a funsig) : Bluejay_ast.funsig =
  let (Funsig (f, args, f_body)) = fun_sig in
  let f_body' = from_internal_expr_desc f_body in
  Bluejay_ast.Funsig (f, args, f_body')

and transform_typed_funsig (fun_sig : 'a typed_funsig) :
    Bluejay_ast.typed_funsig =
  match fun_sig with
  | Typed_funsig (f, args_with_type, (f_body, ret_type)) ->
      let args_with_type' =
        List.map
          (fun (arg, t) -> (arg, from_internal_expr_desc t))
          args_with_type
      in
      let f_body' = from_internal_expr_desc f_body in
      let ret_type' = from_internal_expr_desc ret_type in
      Bluejay_ast.Typed_funsig (f, args_with_type', (f_body', ret_type'))
  | DTyped_funsig (f, (arg, t), (f_body, ret_type)) ->
      let f_body' = from_internal_expr_desc f_body in
      let ret_type' = from_internal_expr_desc ret_type in
      Bluejay_ast.DTyped_funsig
        (f, (arg, from_internal_expr_desc t), (f_body', ret_type'))

and from_internal_expr (e : syn_type_bluejay) : Bluejay_ast.expr =
  match e with
  | Int n -> Int n
  | Bool b -> Bool b
  | Var v -> Var v
  | Function (args, f_edesc) -> Function (args, from_internal_expr_desc f_edesc)
  | Input -> Input
  | Appl (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      Appl (ed1', ed2')
  | Let (x, ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      Let (x, ed1', ed2')
  | LetRecFun (fs, ed) ->
      let fs' = List.map transform_funsig fs in
      let ed' = from_internal_expr_desc ed in
      LetRecFun (fs', ed')
  | LetFun (fun_sig, ed) ->
      let fun_sig' = transform_funsig fun_sig in
      let ed' = from_internal_expr_desc ed in
      LetFun (fun_sig', ed')
  | LetWithType (x, ed1, ed2, t) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      let t' = from_internal_expr_desc t in
      LetWithType (x, ed1', ed2', t')
  | LetRecFunWithType (fs, ed) ->
      let fs' = List.map transform_typed_funsig fs in
      let ed' = from_internal_expr_desc ed in
      LetRecFunWithType (fs', ed')
  | LetFunWithType (fun_sig, ed) ->
      let fun_sig' = transform_typed_funsig fun_sig in
      let ed' = from_internal_expr_desc ed in
      LetFunWithType (fun_sig', ed')
  | Plus (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      Plus (ed1', ed2')
  | Minus (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      Minus (ed1', ed2')
  | Times (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      Times (ed1', ed2')
  | Divide (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      Divide (ed1', ed2')
  | Modulus (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      Modulus (ed1', ed2')
  | Equal (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      Equal (ed1', ed2')
  | Neq (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      Neq (ed1', ed2')
  | LessThan (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      LessThan (ed1', ed2')
  | Leq (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      Leq (ed1', ed2')
  | GreaterThan (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      GreaterThan (ed1', ed2')
  | Geq (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      Geq (ed1', ed2')
  | And (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      And (ed1', ed2')
  | Or (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      Or (ed1', ed2')
  | Not ed ->
      let ed' = from_internal_expr_desc ed in
      Not ed'
  | If (ed1, ed2, ed3) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      let ed3' = from_internal_expr_desc ed3 in
      If (ed1', ed2', ed3')
  | Record r ->
      let r' = Ident_map.map from_internal_expr_desc r in
      Record r'
  | RecordProj (ed, l) ->
      let ed' = from_internal_expr_desc ed in
      RecordProj (ed', l)
  | Match (m_ed, pe_lst) ->
      let m_ed' = from_internal_expr_desc m_ed in
      let pe_lst' =
        List.map
          (fun (p, ed) ->
            let ed' = from_internal_expr_desc ed in
            (p, ed'))
          pe_lst
      in
      Match (m_ed', pe_lst')
  | VariantExpr (lbl, ed) ->
      let ed' = from_internal_expr_desc ed in
      VariantExpr (lbl, ed')
  | List eds ->
      let eds' = List.map from_internal_expr_desc eds in
      List eds'
  | ListCons (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      ListCons (ed1', ed2')
  | TypeError x -> TypeError x
  | Assert ed ->
      let ed' = from_internal_expr_desc ed in
      Assert ed'
  | Assume ed ->
      let ed' = from_internal_expr_desc ed in
      Assume ed'
  | TypeVar x -> TypeVar x
  | TypeInt -> TypeInt
  | TypeBool -> TypeBool
  | TypeRecord r ->
      let r' = Ident_map.map from_internal_expr_desc r in
      TypeRecord r'
  | TypeList ed ->
      let ed' = from_internal_expr_desc ed in
      TypeList ed'
  | TypeArrow (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      TypeArrow (ed1', ed2')
  | TypeArrowD ((x, ed1), ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      TypeArrowD ((x, ed1'), ed2')
  | TypeSet (ed, p) ->
      let ed' = from_internal_expr_desc ed in
      let p' = from_internal_expr_desc p in
      TypeSet (ed', p')
  | TypeUnion (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      TypeUnion (ed1', ed2')
  | TypeIntersect (ed1, ed2) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      TypeIntersect (ed1', ed2')
  | TypeRecurse (tv, ed) ->
      let ed' = from_internal_expr_desc ed in
      TypeRecurse (tv, ed')
  | TypeUntouched s -> TypeUntouched s
  | TypeVariant vs ->
      let vs' = List.map (fun (l, ve) -> (l, from_internal_expr_desc ve)) vs in
      TypeVariant vs'

(* Helper routines to transform external bluejay to internal bluejay *)

let rec to_internal_expr_desc (e : Bluejay_ast.expr_desc) : syn_bluejay_edesc =
  let tag' = e.tag in
  let e' = to_internal_expr e.body in
  { tag = tag'; body = e' }

and transform_funsig (fun_sig : Bluejay_ast.funsig) : 'a funsig =
  let (Bluejay_ast.Funsig (f, args, f_body)) = fun_sig in
  let f_body' = to_internal_expr_desc f_body in
  Funsig (f, args, f_body')

and transform_typed_funsig (fun_sig : Bluejay_ast.typed_funsig) :
    'a typed_funsig =
  match fun_sig with
  | Bluejay_ast.Typed_funsig (f, args_with_type, (f_body, ret_type)) ->
      let args_with_type' =
        List.map (fun (arg, t) -> (arg, to_internal_expr_desc t)) args_with_type
      in
      let f_body' = to_internal_expr_desc f_body in
      let ret_type' = to_internal_expr_desc ret_type in
      Typed_funsig (f, args_with_type', (f_body', ret_type'))
  | Bluejay_ast.DTyped_funsig (f, (arg, t), (f_body, ret_type)) ->
      let f_body' = to_internal_expr_desc f_body in
      let ret_type' = to_internal_expr_desc ret_type in
      DTyped_funsig (f, (arg, to_internal_expr_desc t), (f_body', ret_type'))

and to_internal_expr (e : Bluejay_ast.expr) : syn_type_bluejay =
  match e with
  | Int n -> Int n
  | Bool b -> Bool b
  | Var v -> Var v
  | Function (args, f_edesc) -> Function (args, to_internal_expr_desc f_edesc)
  | Input -> Input
  | Appl (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      Appl (ed1', ed2')
  | Let (x, ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      Let (x, ed1', ed2')
  | LetRecFun (fs, ed) ->
      let fs' = List.map transform_funsig fs in
      let ed' = to_internal_expr_desc ed in
      LetRecFun (fs', ed')
  | LetFun (fun_sig, ed) ->
      let fun_sig' = transform_funsig fun_sig in
      let ed' = to_internal_expr_desc ed in
      LetFun (fun_sig', ed')
  | LetWithType (x, ed1, ed2, t) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      let t' = to_internal_expr_desc t in
      LetWithType (x, ed1', ed2', t')
  | LetRecFunWithType (fs, ed) ->
      let fs' = List.map transform_typed_funsig fs in
      let ed' = to_internal_expr_desc ed in
      LetRecFunWithType (fs', ed')
  | LetFunWithType (fun_sig, ed) ->
      let fun_sig' = transform_typed_funsig fun_sig in
      let ed' = to_internal_expr_desc ed in
      LetFunWithType (fun_sig', ed')
  | Plus (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      Plus (ed1', ed2')
  | Minus (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      Minus (ed1', ed2')
  | Times (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      Times (ed1', ed2')
  | Divide (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      Divide (ed1', ed2')
  | Modulus (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      Modulus (ed1', ed2')
  | Equal (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      Equal (ed1', ed2')
  | Neq (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      Neq (ed1', ed2')
  | LessThan (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      LessThan (ed1', ed2')
  | Leq (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      Leq (ed1', ed2')
  | GreaterThan (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      GreaterThan (ed1', ed2')
  | Geq (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      Geq (ed1', ed2')
  | And (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      And (ed1', ed2')
  | Or (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      Or (ed1', ed2')
  | Not ed ->
      let ed' = to_internal_expr_desc ed in
      Not ed'
  | If (ed1, ed2, ed3) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      let ed3' = to_internal_expr_desc ed3 in
      If (ed1', ed2', ed3')
  | Record r ->
      let r' = Ident_map.map to_internal_expr_desc r in
      Record r'
  | RecordProj (ed, l) ->
      let ed' = to_internal_expr_desc ed in
      RecordProj (ed', l)
  | Match (m_ed, pe_lst) ->
      let m_ed' = to_internal_expr_desc m_ed in
      let pe_lst' =
        List.map
          (fun (p, ed) ->
            let ed' = to_internal_expr_desc ed in
            (p, ed'))
          pe_lst
      in
      Match (m_ed', pe_lst')
  | VariantExpr (lbl, ed) ->
      let ed' = to_internal_expr_desc ed in
      VariantExpr (lbl, ed')
  | List eds ->
      let eds' = List.map to_internal_expr_desc eds in
      List eds'
  | ListCons (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      ListCons (ed1', ed2')
  | TypeError x -> TypeError x
  | Assert ed ->
      let ed' = to_internal_expr_desc ed in
      Assert ed'
  | Assume ed ->
      let ed' = to_internal_expr_desc ed in
      Assume ed'
  | TypeVar x -> TypeVar x
  | TypeInt -> TypeInt
  | TypeBool -> TypeBool
  | TypeRecord r ->
      let r' = Ident_map.map to_internal_expr_desc r in
      TypeRecord r'
  | TypeList ed ->
      let ed' = to_internal_expr_desc ed in
      TypeList ed'
  | TypeArrow (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      TypeArrow (ed1', ed2')
  | TypeArrowD ((x, ed1), ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      TypeArrowD ((x, ed1'), ed2')
  | TypeSet (ed, p) ->
      let ed' = to_internal_expr_desc ed in
      let p' = to_internal_expr_desc p in
      TypeSet (ed', p')
  | TypeUnion (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      TypeUnion (ed1', ed2')
  | TypeIntersect (ed1, ed2) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      TypeIntersect (ed1', ed2')
  | TypeRecurse (tv, ed) ->
      let ed' = to_internal_expr_desc ed in
      TypeRecurse (tv, ed')
  | TypeUntouched s -> TypeUntouched s
  | TypeVariant vs ->
      let vs' = List.map (fun (l, ve) -> (l, to_internal_expr_desc ve)) vs in
      TypeVariant vs'

(* Helper routines to transform jay to internal bluejay *)

let rec from_jay_expr_desc (e : Jay.Jay_ast.expr_desc) : core_bluejay_edesc =
  let tag' = e.tag in
  let e' = from_jay_expr e.body in
  { tag = tag'; body = e' }

and transform_funsig (fun_sig : Jay.Jay_ast.funsig) : core_only funsig =
  let (Jay.Jay_ast.Funsig (f, args, f_body)) = fun_sig in
  let f_body' = from_jay_expr_desc f_body in
  Funsig (f, args, f_body')

and from_jay_expr (e : Jay.Jay_ast.expr) : core_bluejay =
  let pat_conv (p : Jay.Jay_ast.pattern) : pattern =
    match p with
    | AnyPat -> AnyPat
    | IntPat -> IntPat
    | BoolPat -> BoolPat
    | FunPat -> FunPat
    | RecPat r -> RecPat r
    | StrictRecPat r -> StrictRecPat r
    | VariantPat (Variant_label l, x) -> VariantPat (Variant_label l, x)
    | VarPat x -> VarPat x
    | EmptyLstPat -> EmptyLstPat
    | LstDestructPat (hd, tl) -> LstDestructPat (hd, tl)
  in
  match e with
  | Int n -> Int n
  | Bool b -> Bool b
  | Var v -> Var v
  | Function (args, f_edesc) -> Function (args, from_jay_expr_desc f_edesc)
  | Input -> Input
  | Appl (ed1, ed2) ->
      let ed1' = from_jay_expr_desc ed1 in
      let ed2' = from_jay_expr_desc ed2 in
      Appl (ed1', ed2')
  | Let (x, ed1, ed2) ->
      let ed1' = from_jay_expr_desc ed1 in
      let ed2' = from_jay_expr_desc ed2 in
      Let (x, ed1', ed2')
  | LetRecFun (fs, ed) ->
      let fs' = List.map transform_funsig fs in
      let ed' = from_jay_expr_desc ed in
      LetRecFun (fs', ed')
  | LetFun (fun_sig, ed) ->
      let fun_sig' = transform_funsig fun_sig in
      let ed' = from_jay_expr_desc ed in
      LetFun (fun_sig', ed')
  | Plus (ed1, ed2) ->
      let ed1' = from_jay_expr_desc ed1 in
      let ed2' = from_jay_expr_desc ed2 in
      Plus (ed1', ed2')
  | Minus (ed1, ed2) ->
      let ed1' = from_jay_expr_desc ed1 in
      let ed2' = from_jay_expr_desc ed2 in
      Minus (ed1', ed2')
  | Times (ed1, ed2) ->
      let ed1' = from_jay_expr_desc ed1 in
      let ed2' = from_jay_expr_desc ed2 in
      Times (ed1', ed2')
  | Divide (ed1, ed2) ->
      let ed1' = from_jay_expr_desc ed1 in
      let ed2' = from_jay_expr_desc ed2 in
      Divide (ed1', ed2')
  | Modulus (ed1, ed2) ->
      let ed1' = from_jay_expr_desc ed1 in
      let ed2' = from_jay_expr_desc ed2 in
      Modulus (ed1', ed2')
  | Equal (ed1, ed2) ->
      let ed1' = from_jay_expr_desc ed1 in
      let ed2' = from_jay_expr_desc ed2 in
      Equal (ed1', ed2')
  | Neq (ed1, ed2) ->
      let ed1' = from_jay_expr_desc ed1 in
      let ed2' = from_jay_expr_desc ed2 in
      Neq (ed1', ed2')
  | LessThan (ed1, ed2) ->
      let ed1' = from_jay_expr_desc ed1 in
      let ed2' = from_jay_expr_desc ed2 in
      LessThan (ed1', ed2')
  | Leq (ed1, ed2) ->
      let ed1' = from_jay_expr_desc ed1 in
      let ed2' = from_jay_expr_desc ed2 in
      Leq (ed1', ed2')
  | GreaterThan (ed1, ed2) ->
      let ed1' = from_jay_expr_desc ed1 in
      let ed2' = from_jay_expr_desc ed2 in
      GreaterThan (ed1', ed2')
  | Geq (ed1, ed2) ->
      let ed1' = from_jay_expr_desc ed1 in
      let ed2' = from_jay_expr_desc ed2 in
      Geq (ed1', ed2')
  | And (ed1, ed2) ->
      let ed1' = from_jay_expr_desc ed1 in
      let ed2' = from_jay_expr_desc ed2 in
      And (ed1', ed2')
  | Or (ed1, ed2) ->
      let ed1' = from_jay_expr_desc ed1 in
      let ed2' = from_jay_expr_desc ed2 in
      Or (ed1', ed2')
  | Not ed ->
      let ed' = from_jay_expr_desc ed in
      Not ed'
  | If (ed1, ed2, ed3) ->
      let ed1' = from_jay_expr_desc ed1 in
      let ed2' = from_jay_expr_desc ed2 in
      let ed3' = from_jay_expr_desc ed3 in
      If (ed1', ed2', ed3')
  | Record r ->
      let r' = Ident_map.map from_jay_expr_desc r in
      Record r'
  | RecordProj (ed, Label l) ->
      let ed' = from_jay_expr_desc ed in
      RecordProj (ed', Label l)
  | Match (m_ed, pe_lst) ->
      let m_ed' = from_jay_expr_desc m_ed in
      let pe_lst' =
        List.map
          (fun (p, ed) ->
            let ed' = from_jay_expr_desc ed in
            (pat_conv p, ed'))
          pe_lst
      in
      Match (m_ed', pe_lst')
  | VariantExpr (Variant_label lbl, ed) ->
      let ed' = from_jay_expr_desc ed in
      VariantExpr (Variant_label lbl, ed')
  | List eds ->
      let eds' = List.map from_jay_expr_desc eds in
      List eds'
  | ListCons (ed1, ed2) ->
      let ed1' = from_jay_expr_desc ed1 in
      let ed2' = from_jay_expr_desc ed2 in
      ListCons (ed1', ed2')
  | Assert ed ->
      let ed' = from_jay_expr_desc ed in
      Assert ed'
  | Assume ed ->
      let ed' = from_jay_expr_desc ed in
      Assume ed'
  | Error x -> TypeError x

(* Helper routines to transform internal bluejay to jay *)

let rec to_jay_expr_desc (e : core_bluejay_edesc) : Jay.Jay_ast.expr_desc =
  let tag' = e.tag in
  let e' = to_jay_expr e.body in
  { tag = tag'; body = e' }

and transform_funsig (fun_sig : core_only funsig) : Jay.Jay_ast.funsig =
  let (Funsig (f, args, f_body)) = fun_sig in
  let f_body' = to_jay_expr_desc f_body in
  Jay.Jay_ast.Funsig (f, args, f_body')

and to_jay_expr (e : core_bluejay) : Jay.Jay_ast.expr =
  let pat_conv (p : pattern) : Jay.Jay_ast.pattern =
    match p with
    | AnyPat -> AnyPat
    | IntPat -> IntPat
    | BoolPat -> BoolPat
    | FunPat -> FunPat
    | RecPat r -> RecPat r
    | StrictRecPat r -> StrictRecPat r
    | VariantPat (Variant_label l, x) -> VariantPat (Variant_label l, x)
    | VarPat x -> VarPat x
    | EmptyLstPat -> EmptyLstPat
    | LstDestructPat (hd, tl) -> LstDestructPat (hd, tl)
  in
  match e with
  | Int n -> Int n
  | Bool b -> Bool b
  | Var v -> Var v
  | Function (args, f_edesc) -> Function (args, to_jay_expr_desc f_edesc)
  | Input -> Input
  | Appl (ed1, ed2) ->
      let ed1' = to_jay_expr_desc ed1 in
      let ed2' = to_jay_expr_desc ed2 in
      Appl (ed1', ed2')
  | Let (x, ed1, ed2) ->
      let ed1' = to_jay_expr_desc ed1 in
      let ed2' = to_jay_expr_desc ed2 in
      Let (x, ed1', ed2')
  | LetRecFun (fs, ed) ->
      let fs' = List.map transform_funsig fs in
      let ed' = to_jay_expr_desc ed in
      LetRecFun (fs', ed')
  | LetFun (fun_sig, ed) ->
      let fun_sig' = transform_funsig fun_sig in
      let ed' = to_jay_expr_desc ed in
      LetFun (fun_sig', ed')
  | Plus (ed1, ed2) ->
      let ed1' = to_jay_expr_desc ed1 in
      let ed2' = to_jay_expr_desc ed2 in
      Plus (ed1', ed2')
  | Minus (ed1, ed2) ->
      let ed1' = to_jay_expr_desc ed1 in
      let ed2' = to_jay_expr_desc ed2 in
      Minus (ed1', ed2')
  | Times (ed1, ed2) ->
      let ed1' = to_jay_expr_desc ed1 in
      let ed2' = to_jay_expr_desc ed2 in
      Times (ed1', ed2')
  | Divide (ed1, ed2) ->
      let ed1' = to_jay_expr_desc ed1 in
      let ed2' = to_jay_expr_desc ed2 in
      Divide (ed1', ed2')
  | Modulus (ed1, ed2) ->
      let ed1' = to_jay_expr_desc ed1 in
      let ed2' = to_jay_expr_desc ed2 in
      Modulus (ed1', ed2')
  | Equal (ed1, ed2) ->
      let ed1' = to_jay_expr_desc ed1 in
      let ed2' = to_jay_expr_desc ed2 in
      Equal (ed1', ed2')
  | Neq (ed1, ed2) ->
      let ed1' = to_jay_expr_desc ed1 in
      let ed2' = to_jay_expr_desc ed2 in
      Neq (ed1', ed2')
  | LessThan (ed1, ed2) ->
      let ed1' = to_jay_expr_desc ed1 in
      let ed2' = to_jay_expr_desc ed2 in
      LessThan (ed1', ed2')
  | Leq (ed1, ed2) ->
      let ed1' = to_jay_expr_desc ed1 in
      let ed2' = to_jay_expr_desc ed2 in
      Leq (ed1', ed2')
  | GreaterThan (ed1, ed2) ->
      let ed1' = to_jay_expr_desc ed1 in
      let ed2' = to_jay_expr_desc ed2 in
      GreaterThan (ed1', ed2')
  | Geq (ed1, ed2) ->
      let ed1' = to_jay_expr_desc ed1 in
      let ed2' = to_jay_expr_desc ed2 in
      Geq (ed1', ed2')
  | And (ed1, ed2) ->
      let ed1' = to_jay_expr_desc ed1 in
      let ed2' = to_jay_expr_desc ed2 in
      And (ed1', ed2')
  | Or (ed1, ed2) ->
      let ed1' = to_jay_expr_desc ed1 in
      let ed2' = to_jay_expr_desc ed2 in
      Or (ed1', ed2')
  | Not ed ->
      let ed' = to_jay_expr_desc ed in
      Not ed'
  | If (ed1, ed2, ed3) ->
      let ed1' = to_jay_expr_desc ed1 in
      let ed2' = to_jay_expr_desc ed2 in
      let ed3' = to_jay_expr_desc ed3 in
      If (ed1', ed2', ed3')
  | Record r ->
      let r' = Ident_map.map to_jay_expr_desc r in
      Record r'
  | RecordProj (ed, Label l) ->
      let ed' = to_jay_expr_desc ed in
      RecordProj (ed', Label l)
  | Match (m_ed, pe_lst) ->
      let m_ed' = to_jay_expr_desc m_ed in
      let pe_lst' =
        List.map
          (fun (p, ed) ->
            let ed' = to_jay_expr_desc ed in
            (pat_conv p, ed'))
          pe_lst
      in
      Match (m_ed', pe_lst')
  | VariantExpr (Variant_label lbl, ed) ->
      let ed' = to_jay_expr_desc ed in
      VariantExpr (Variant_label lbl, ed')
  | List eds ->
      let eds' = List.map to_jay_expr_desc eds in
      List eds'
  | ListCons (ed1, ed2) ->
      let ed1' = to_jay_expr_desc ed1 in
      let ed2' = to_jay_expr_desc ed2 in
      ListCons (ed1', ed2')
  | Assert ed ->
      let ed' = to_jay_expr_desc ed in
      Assert ed'
  | Assume ed ->
      let ed' = to_jay_expr_desc ed in
      Assume ed'
  | TypeError x -> Error x

(* Other helper functions *)
let is_type_expr (ed : syn_bluejay_edesc) : bool =
  match ed.body with
  | TypeVar _ | TypeInt | TypeBool | TypeRecord _ | TypeList _ | TypeArrow _
  | TypeArrowD _ | TypeUnion _ | TypeIntersect _ | TypeSet _ | TypeRecurse _
  | TypeVariant _ | TypeUntouched _ ->
      true
  | _ -> false

let is_fun_type (ed : syn_bluejay_edesc) : bool =
  match ed.body with TypeArrow _ | TypeArrowD _ -> true | _ -> false

let is_dependent_fun_type (ed : syn_bluejay_edesc) : bool =
  match ed.body with TypeArrowD _ -> true | _ -> false

let is_polymorphic_type (ed : syn_bluejay_edesc) : bool =
  match ed.body with TypeUntouched _ -> true | _ -> false

let get_dependent_fun_var (ed : syn_bluejay_edesc) : ident =
  match ed.body with
  | TypeArrowD ((x, _), _) -> x
  | _ ->
      failwith
        "get_dependent_fun_var: Should only be called with a dependent \
         function type!"

let is_record_type : type a. a expr_desc -> bool =
 fun ed -> match ed.body with TypeRecord _ -> true | _ -> false

let is_record_pat (p : pattern) : bool =
  match p with
  | StrictRecPat rec_pat | RecPat rec_pat ->
      not @@ Ident_map.mem (Ident "~untouched") rec_pat
  | _ -> false

let rec is_subtype (ed1 : syn_bluejay_edesc) (ed2 : syn_bluejay_edesc) : bool =
  if tagless_equal_expr_desc ed1 ed2
  then true
  else
    match (ed1.body, ed2.body) with
    | TypeRecord r1, TypeRecord r2 ->
        (* e.g.: { a : int; b : int } <: { a : int v bool }  *)
        let r1_labels = Ident_map.key_list r1 in
        let r2_labels = Ident_map.key_list r2 in
        (* r1 must have all the labels that r2 has to be its subtype *)
        let prelim = List.subset compare_ident r2_labels r1_labels in
        if prelim
        then
          (* r1's fields must all be subtypes to the corresponding fields in r2 *)
          Ident_map.for_all (fun k v -> is_subtype (Ident_map.find k r1) v) r2
        else false
    | TypeList t1, TypeList t2 -> is_subtype t1 t2
    | TypeArrow (dom1, cod1), TypeArrow (dom2, cod2) ->
        is_subtype dom2 dom1 && is_subtype cod1 cod2
    | _, TypeUnion (t1, t2) ->
        if is_subtype ed1 t1 then true else is_subtype ed1 t2
    | _, TypeIntersect (t1, t2) ->
        if is_subtype ed1 t1
        then is_subtype ed1 t2
        else false (* TODO: What other cases are we missing here? *)
    | _ -> failwith "TBI!"

let rec replace_var_of_expr_desc (ed : 'a expr_desc) (og_id : ident)
    (new_id : ident) : 'a expr_desc =
  let e = ed.body in
  { tag = ed.tag; body = replace_var_of_expr e og_id new_id }

and replace_var_of_expr (e : 'a expr) (og_id : ident) (new_id : ident) : 'a expr
    =
  match e with
  | Int _ | Bool _ | Input | TypeInt | TypeBool | TypeUntouched _ | TypeVar _
  | TypeError _ ->
      e
  | Var x -> if Ident.equal x og_id then Var new_id else Var x
  | Function (params, ed) ->
      if List.mem og_id params
      then e
      else Function (params, replace_var_of_expr_desc ed og_id new_id)
  | Appl (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      Appl (ed1', ed2')
  | Plus (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      Plus (ed1', ed2')
  | Minus (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      Minus (ed1', ed2')
  | Times (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      Times (ed1', ed2')
  | Divide (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      Divide (ed1', ed2')
  | Modulus (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      Modulus (ed1', ed2')
  | Equal (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      Equal (ed1', ed2')
  | Neq (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      Neq (ed1', ed2')
  | LessThan (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      LessThan (ed1', ed2')
  | Leq (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      Leq (ed1', ed2')
  | GreaterThan (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      GreaterThan (ed1', ed2')
  | Geq (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      Geq (ed1', ed2')
  | And (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      And (ed1', ed2')
  | Or (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      Or (ed1', ed2')
  | ListCons (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      ListCons (ed1', ed2')
  | TypeArrow (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      TypeArrow (ed1', ed2')
  | TypeSet (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      TypeSet (ed1', ed2')
  | TypeUnion (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      TypeUnion (ed1', ed2')
  | TypeIntersect (ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      TypeIntersect (ed1', ed2')
  | Not ed ->
      let ed' = replace_var_of_expr_desc ed og_id new_id in
      Not ed'
  | RecordProj (ed, lbl) ->
      let ed' = replace_var_of_expr_desc ed og_id new_id in
      RecordProj (ed', lbl)
  | VariantExpr (v_lbl, ed) ->
      let ed' = replace_var_of_expr_desc ed og_id new_id in
      VariantExpr (v_lbl, ed')
  | Assert ed ->
      let ed' = replace_var_of_expr_desc ed og_id new_id in
      Assert ed'
  | Assume ed ->
      let ed' = replace_var_of_expr_desc ed og_id new_id in
      Assume ed'
  | TypeList ed ->
      let ed' = replace_var_of_expr_desc ed og_id new_id in
      TypeList ed'
  | TypeVariant vs ->
      let vs' =
        List.map
          (fun (v_lbl, t_ed) ->
            (v_lbl, replace_var_of_expr_desc t_ed og_id new_id))
          vs
      in
      TypeVariant vs'
  | If (ed1, ed2, ed3) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' = replace_var_of_expr_desc ed2 og_id new_id in
      let ed3' = replace_var_of_expr_desc ed3 og_id new_id in
      If (ed1', ed2', ed3')
  | Record r ->
      let r' =
        Ident_map.map (fun ed -> replace_var_of_expr_desc ed og_id new_id) r
      in
      Record r'
  | TypeRecord r ->
      let r' =
        Ident_map.map (fun ed -> replace_var_of_expr_desc ed og_id new_id) r
      in
      TypeRecord r'
  | List eds ->
      let eds' =
        List.map (fun ed -> replace_var_of_expr_desc ed og_id new_id) eds
      in
      List eds'
  | Match (med, pat_ed_lst) ->
      let med' = replace_var_of_expr_desc med og_id new_id in
      let pat_ed_lst' =
        List.map
          (fun (pat, p_ed) -> (pat, replace_var_of_expr_desc p_ed og_id new_id))
          pat_ed_lst
      in
      Match (med', pat_ed_lst')
  | Let (x, ed1, ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' =
        if Ident.equal x og_id
        then ed2
        else replace_var_of_expr_desc ed2 og_id new_id
      in
      Let (x, ed1', ed2')
  | LetWithType (x, ed1, ed2, ed3) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' =
        if Ident.equal x og_id
        then ed2
        else replace_var_of_expr_desc ed2 og_id new_id
      in
      let ed3' = replace_var_of_expr_desc ed3 og_id new_id in
      LetWithType (x, ed1', ed2', ed3')
  | LetFun (fun_sig, ed) ->
      let fun_sig' = replace_var_of_funsig fun_sig og_id new_id in
      let ed' = replace_var_of_expr_desc ed og_id new_id in
      LetFun (fun_sig', ed')
  | LetFunWithType (fun_sig, ed) ->
      let fun_sig' = replace_var_of_typed_funsig fun_sig og_id new_id in
      let ed' = replace_var_of_expr_desc ed og_id new_id in
      LetFunWithType (fun_sig', ed')
  | LetRecFun (fun_sigs, ed) ->
      let fun_sigs' =
        List.map
          (fun f_sig -> replace_var_of_funsig f_sig og_id new_id)
          fun_sigs
      in
      let ed' = replace_var_of_expr_desc ed og_id new_id in
      LetRecFun (fun_sigs', ed')
  | LetRecFunWithType (fun_sigs, ed) ->
      let fun_sigs' =
        List.map
          (fun f_sig -> replace_var_of_typed_funsig f_sig og_id new_id)
          fun_sigs
      in
      let ed' = replace_var_of_expr_desc ed og_id new_id in
      LetRecFunWithType (fun_sigs', ed')
  | TypeArrowD ((x, ed1), ed2) ->
      let ed1' = replace_var_of_expr_desc ed1 og_id new_id in
      let ed2' =
        if Ident.equal x og_id
        then ed2
        else replace_var_of_expr_desc ed2 og_id new_id
      in
      TypeArrowD ((x, ed1'), ed2')
  | TypeRecurse (x, ed) ->
      let ed' =
        if Ident.equal x og_id
        then ed
        else replace_var_of_expr_desc ed og_id new_id
      in
      TypeRecurse (x, ed')

and replace_var_of_funsig (Funsig (f, params, ed) : 'a funsig) (og_id : ident)
    (new_id : ident) : 'a funsig =
  if Ident.equal f og_id || List.mem og_id params
  then Funsig (f, params, ed)
  else
    let ed' = replace_var_of_expr_desc ed og_id new_id in
    Funsig (f, params, ed')

and replace_var_of_typed_funsig (fun_sig : 'a typed_funsig) (og_id : ident)
    (new_id : ident) : 'a typed_funsig =
  match fun_sig with
  | Typed_funsig (f, typed_params, (body, ret_type)) ->
      let params = List.map fst typed_params in
      (* TODO: Check logic here; might be buggy. *)
      if List.mem og_id params
      then
        let typed_params' =
          List.map
            (fun (x, t) -> (x, replace_var_of_expr_desc t og_id new_id))
            typed_params
        in
        let ret_type' = replace_var_of_expr_desc ret_type og_id new_id in
        Typed_funsig (f, typed_params', (body, ret_type'))
      else if Ident.equal f og_id
      then
        let typed_params' =
          List.map
            (fun (x, t) -> (x, replace_var_of_expr_desc t og_id new_id))
            typed_params
        in
        Typed_funsig (f, typed_params', (body, ret_type))
      else
        let body' = replace_var_of_expr_desc body og_id new_id in
        let typed_params' =
          List.map
            (fun (x, t) -> (x, replace_var_of_expr_desc t og_id new_id))
            typed_params
        in
        let ret_type' = replace_var_of_expr_desc ret_type og_id new_id in
        Typed_funsig (f, typed_params', (body', ret_type'))
  | DTyped_funsig (f, (x, t), (body, ret_type)) ->
      (* TODO: Check logic here *)
      if Ident.equal x og_id
      then
        let t' = replace_var_of_expr_desc t og_id new_id in
        DTyped_funsig (f, (x, t'), (body, ret_type))
      else if Ident.equal f og_id
      then
        let t' = replace_var_of_expr_desc t og_id new_id in
        DTyped_funsig (f, (x, t'), (body, ret_type))
      else
        let body' = replace_var_of_expr_desc body og_id new_id in
        let t' = replace_var_of_expr_desc t og_id new_id in
        let ret_type' = replace_var_of_expr_desc ret_type og_id new_id in
        DTyped_funsig (f, (x, t'), (body', ret_type'))

let rec replace_tagless_expr_desc (ed : 'a expr_desc) (target : 'a expr_desc)
    (replacement : 'a expr_desc) : 'a expr_desc =
  if tagless_equal_expr_desc ed target
  then replacement
  else
    let e = ed.body in
    { tag = ed.tag; body = replace_tagless_expr e target replacement }

and replace_tagless_expr (e : 'a expr) (target : 'a expr_desc)
    (replacement : 'a expr_desc) : 'a expr =
  match e with
  | Int _ | Bool _ | Input | TypeInt | TypeBool | TypeUntouched _ | TypeVar _
  | TypeError _ | Var _ ->
      e
  | Function (params, ed) ->
      let ed' = replace_tagless_expr_desc ed target replacement in
      Function (params, ed')
  | Appl (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      Appl (ed1', ed2')
  | Plus (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      Plus (ed1', ed2')
  | Minus (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      Minus (ed1', ed2')
  | Times (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      Times (ed1', ed2')
  | Divide (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      Divide (ed1', ed2')
  | Modulus (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      Modulus (ed1', ed2')
  | Equal (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      Equal (ed1', ed2')
  | Neq (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      Neq (ed1', ed2')
  | LessThan (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      LessThan (ed1', ed2')
  | Leq (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      Leq (ed1', ed2')
  | GreaterThan (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      GreaterThan (ed1', ed2')
  | Geq (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      Geq (ed1', ed2')
  | And (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      And (ed1', ed2')
  | Or (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      Or (ed1', ed2')
  | ListCons (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      ListCons (ed1', ed2')
  | TypeArrow (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      TypeArrow (ed1', ed2')
  | TypeSet (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      TypeSet (ed1', ed2')
  | TypeUnion (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      TypeUnion (ed1', ed2')
  | TypeIntersect (ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      TypeIntersect (ed1', ed2')
  | Not ed ->
      let ed' = replace_tagless_expr_desc ed target replacement in
      Not ed'
  | RecordProj (ed, lbl) ->
      let ed' = replace_tagless_expr_desc ed target replacement in
      RecordProj (ed', lbl)
  | VariantExpr (v_lbl, ed) ->
      let ed' = replace_tagless_expr_desc ed target replacement in
      VariantExpr (v_lbl, ed')
  | Assert ed ->
      let ed' = replace_tagless_expr_desc ed target replacement in
      Assert ed'
  | Assume ed ->
      let ed' = replace_tagless_expr_desc ed target replacement in
      Assume ed'
  | TypeList ed ->
      let ed' = replace_tagless_expr_desc ed target replacement in
      TypeList ed'
  | TypeVariant vs ->
      let vs' =
        List.map
          (fun (v_lbl, t_ed) ->
            (v_lbl, replace_tagless_expr_desc t_ed target replacement))
          vs
      in
      TypeVariant vs'
  | If (ed1, ed2, ed3) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      let ed3' = replace_tagless_expr_desc ed3 target replacement in
      If (ed1', ed2', ed3')
  | Record r ->
      let r' =
        Ident_map.map
          (fun ed -> replace_tagless_expr_desc ed target replacement)
          r
      in
      Record r'
  | TypeRecord r ->
      let r' =
        Ident_map.map
          (fun ed -> replace_tagless_expr_desc ed target replacement)
          r
      in
      TypeRecord r'
  | List eds ->
      let eds' =
        List.map (fun ed -> replace_tagless_expr_desc ed target replacement) eds
      in
      List eds'
  | Match (med, pat_ed_lst) ->
      let med' = replace_tagless_expr_desc med target replacement in
      let pat_ed_lst' =
        List.map
          (fun (pat, p_ed) ->
            (pat, replace_tagless_expr_desc p_ed target replacement))
          pat_ed_lst
      in
      Match (med', pat_ed_lst')
  | Let (x, ed1, ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      Let (x, ed1', ed2')
  | LetWithType (x, ed1, ed2, ed3) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      let ed3' = replace_tagless_expr_desc ed3 target replacement in
      LetWithType (x, ed1', ed2', ed3')
  | LetFun (fun_sig, ed) ->
      let fun_sig' = replace_tagless_funsig fun_sig target replacement in
      let ed' = replace_tagless_expr_desc ed target replacement in
      LetFun (fun_sig', ed')
  | LetFunWithType (fun_sig, ed) ->
      let fun_sig' = replace_tagless_typed_funsig fun_sig target replacement in
      let ed' = replace_tagless_expr_desc ed target replacement in
      LetFunWithType (fun_sig', ed')
  | LetRecFun (fun_sigs, ed) ->
      let fun_sigs' =
        List.map
          (fun f_sig -> replace_tagless_funsig f_sig target replacement)
          fun_sigs
      in
      let ed' = replace_tagless_expr_desc ed target replacement in
      LetRecFun (fun_sigs', ed')
  | LetRecFunWithType (fun_sigs, ed) ->
      let fun_sigs' =
        List.map
          (fun f_sig -> replace_tagless_typed_funsig f_sig target replacement)
          fun_sigs
      in
      let ed' = replace_tagless_expr_desc ed target replacement in
      LetRecFunWithType (fun_sigs', ed')
  | TypeArrowD ((x, ed1), ed2) ->
      let ed1' = replace_tagless_expr_desc ed1 target replacement in
      let ed2' = replace_tagless_expr_desc ed2 target replacement in
      TypeArrowD ((x, ed1'), ed2')
  | TypeRecurse (x, ed) ->
      let ed' = replace_tagless_expr_desc ed target replacement in
      TypeRecurse (x, ed')

and replace_tagless_funsig (Funsig (f, params, ed) : 'a funsig)
    (target : 'a expr_desc) (replacement : 'a expr_desc) : 'a funsig =
  let ed' = replace_tagless_expr_desc ed target replacement in
  Funsig (f, params, ed')

and replace_tagless_typed_funsig (fun_sig : 'a typed_funsig)
    (target : 'a expr_desc) (replacement : 'a expr_desc) : 'a typed_funsig =
  match fun_sig with
  | Typed_funsig (f, typed_params, (body, ret_type)) ->
      let typed_params' =
        List.map
          (fun (x, t) -> (x, replace_tagless_expr_desc t target replacement))
          typed_params
      in
      let body' = replace_tagless_expr_desc body target replacement in
      let ret_type' = replace_tagless_expr_desc ret_type target replacement in
      Typed_funsig (f, typed_params', (body', ret_type'))
  | DTyped_funsig (f, (x, t), (body, ret_type)) ->
      let body' = replace_tagless_expr_desc body target replacement in
      let t' = replace_tagless_expr_desc t target replacement in
      let ret_type' = replace_tagless_expr_desc ret_type target replacement in
      DTyped_funsig (f, (x, t'), (body', ret_type'))
