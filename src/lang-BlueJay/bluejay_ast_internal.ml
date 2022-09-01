open Batteries

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
  | VariantPat of variant_label * ident
  | VarPat of ident
  | EmptyLstPat
  | LstDestructPat of ident * ident
[@@deriving eq, ord, show, to_yojson]

type predicate = syntactic_only expr_desc
and 'a funsig = Funsig of ident * ident list * 'a expr_desc
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
      ('a funsig list * 'a expr_desc * 'a expr_desc list)
      -> 'a syntactic_and_semantic expr
  | LetFunWithType :
      ('a funsig * 'a expr_desc * 'a expr_desc)
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
  (* | Untouched : string -> 'a expr *)
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
(* | TypeUntouched : string -> syntactic_only expr *)

let counter = ref 0

let fresh_tag () =
  let c = !counter in
  counter := c + 1 ;
  c

let new_expr_desc : type a. a expr -> a expr_desc =
 fun e -> { tag = fresh_tag (); body = e }

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
  (* | Untouched s1, Untouched s2 -> s1 = s2 *)
  (* | Untouched _, _ -> false *)
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
  | LetFunWithType (f1, e1, t1), LetFunWithType (f2, e2, t2) ->
      equal_funsig f1 f2 && equal_expr_desc e1 e2 && equal_expr_desc t1 t2
  (* | LetFunWithType _, _ -> false *)
  | LetRecFunWithType (sig_lst1, e1, t1), LetRecFunWithType (sig_lst2, e2, t2)
    ->
      List.eq equal_funsig sig_lst1 sig_lst2
      && equal_expr_desc e1 e2
      && List.eq equal_expr_desc t1 t2
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
  | TypeIntersect (lt1, rt1), TypeUnion (lt2, rt2)
  | TypeSet (lt1, rt1), TypeSet (lt2, rt2) ->
      equal_expr_desc lt1 lt2 && equal_expr_desc rt1 rt2
  | TypeArrowD ((id1, lt1), rt1), TypeArrowD ((id2, lt2), rt2) ->
      id1 = id2 && equal_expr_desc lt1 lt2 && equal_expr_desc rt1 rt2
  | TypeRecurse (x1, t1), TypeRecurse (x2, t2) -> x1 = x2 && t1 = t2
  (* | TypeUntouched s1, TypeUntouched s2 -> s1 = s2 *)
  | _ -> false

let compare_helper (x : int) (y : int) : int = if x <> 0 then x else y

let rec compare_funsig : type a. a funsig -> a funsig -> int =
 fun (Funsig (id1, params1, fe1)) (Funsig (id2, params2, fe2)) ->
  compare id1 id2
  |> compare_helper (List.compare compare_ident params1 params2)
  |> compare_helper (compare_expr_desc fe1 fe2)

and compare_expr_desc : type a. a expr_desc -> a expr_desc -> int =
 fun e1 e2 ->
  compare_expr e1.body e2.body
  (* |> compare_helper (Option.compare e1.tag e2.tag) *)
  |> compare_helper (compare e1.tag e2.tag)

and compare_expr : type a. a expr -> a expr -> int =
 fun e1 e2 ->
  match (e1, e2) with
  | Int n1, Int n2 -> compare n1 n2
  | Bool b1, Bool b2 -> compare b1 b2
  | Input, Input -> 0
  | Var x1, Var x2 -> compare x1 x2
  | List l1, List l2 -> List.compare compare_expr_desc l1 l2
  | Record r1, Record r2 -> Ident_map.compare compare_expr_desc r1 r2
  (* | Untouched s1, Untouched s2 -> compare s1 s2 *)
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
  | LetFunWithType (f1, e1, t1), LetFunWithType (f2, e2, t2) ->
      compare_funsig f1 f2
      |> compare_helper (compare_expr_desc e1 e2)
      |> compare_helper (compare_expr_desc t1 t2)
  | LetRecFunWithType (sig_lst1, e1, t1), LetRecFunWithType (sig_lst2, e2, t2)
    ->
      List.compare compare_funsig sig_lst1 sig_lst2
      |> compare_helper (compare_expr_desc e1 e2)
      |> compare_helper (List.compare compare_expr_desc t1 t2)
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
  | TypeIntersect (lt1, rt1), TypeUnion (lt2, rt2)
  | TypeSet (lt1, rt1), TypeSet (lt2, rt2) ->
      compare_expr_desc lt1 lt2 + compare_expr_desc rt1 rt2
  | TypeArrowD ((id1, lt1), rt1), TypeArrowD ((id2, lt2), rt2) ->
      compare id1 id2
      |> compare_helper (compare_expr_desc lt1 lt2)
      |> compare_helper (compare_expr_desc rt1 rt2)
  | TypeRecurse (x1, t1), TypeRecurse (x2, t2) ->
      compare x1 x2 |> compare_helper (compare t1 t2)
  (* | TypeUntouched s1, TypeUntouched s2 -> compare s1 s2 *)
  (* TODO: Another potential source for bug *)
  | _ -> 1

module type Expr_desc = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
end

(* module TypedExpr : (Expr with type t = syn_type_bluejay) = struct
     type t = syn_type_bluejay;;
     let equal = equal_expr;;
     let compare = compare_expr;;
   end;;

   module IntermediateExpr : (Expr with type t = sem_type_bluejay) = struct
     type t = sem_type_bluejay;;
     let equal = equal_expr;;
     let compare = compare_expr;;
   end;;

   module CoreExpr : (Expr with type t = core_bluejay) = struct
     type t = core_bluejay;;
     let equal = equal_expr;;
     let compare = compare_expr;;
   end;; *)

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
  (* | Untouched _  *)
  (* TODO: For now, all type expressions will have the lowest precedence coz I'm lazy and don't wanna think about it *)
  | TypeVar _ | TypeInt | TypeBool | TypeRecord _ | TypeList _ | TypeArrow _
  | TypeArrowD _ | TypeSet _ | TypeUnion _ | TypeIntersect _ | TypeRecurse _
  | TypeError _ ->
      13
(* | TypeUntouched _  *)

(** Takes expressions [e1] and [e2] as arguments. Returns 0 if the two
    expressions have equal precedence, a negative int if [e1] has lower
    precedence than [e2], and a positive int if [e1] has higher precedence. *)
let expr_precedence_cmp e1 e2 = expr_precedence_p1 e1 - expr_precedence_p1 e2

let expr_desc_precedence_cmp : type a. a expr_desc -> a expr_desc -> int =
 fun ed1 ed2 -> expr_precedence_cmp ed1.body ed2.body

let rec from_internal_expr (e : syn_type_bluejay) : Bluejay_ast.expr =
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
  | LetFun (f_sig, ed) ->
      let f_sig' = transform_funsig f_sig in
      let ed' = from_internal_expr_desc ed in
      LetFun (f_sig', ed')
  | LetWithType (x, ed1, ed2, t) ->
      let ed1' = from_internal_expr_desc ed1 in
      let ed2' = from_internal_expr_desc ed2 in
      let t' = from_internal_expr_desc t in
      LetWithType (x, ed1', ed2', t')
  | LetRecFunWithType (fs, ed, ts) ->
      let fs' = List.map transform_funsig fs in
      let ed' = from_internal_expr_desc ed in
      let ts' = List.map from_internal_expr_desc ts in
      LetRecFunWithType (fs', ed', ts')
  | LetFunWithType (f_sig, ed, t) ->
      let f_sig' = transform_funsig f_sig in
      let ed' = from_internal_expr_desc ed in
      let t' = from_internal_expr_desc t in
      LetFunWithType (f_sig', ed', t')
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

and from_internal_expr_desc (e : syn_bluejay_edesc) : Bluejay_ast.expr_desc =
  let tag' = e.tag in
  let e' = from_internal_expr e.body in
  { tag = tag'; body = e' }

and transform_funsig (f_sig : 'a funsig) : Bluejay_ast.funsig =
  let (Funsig (f, args, f_body)) = f_sig in
  let f_body' = from_internal_expr_desc f_body in
  Bluejay_ast.Funsig (f, args, f_body')

let rec to_internal_expr (e : Bluejay_ast.expr) : syn_type_bluejay =
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
  | LetFun (f_sig, ed) ->
      let f_sig' = transform_funsig f_sig in
      let ed' = to_internal_expr_desc ed in
      LetFun (f_sig', ed')
  | LetWithType (x, ed1, ed2, t) ->
      let ed1' = to_internal_expr_desc ed1 in
      let ed2' = to_internal_expr_desc ed2 in
      let t' = to_internal_expr_desc t in
      LetWithType (x, ed1', ed2', t')
  | LetRecFunWithType (fs, ed, ts) ->
      let fs' = List.map transform_funsig fs in
      let ed' = to_internal_expr_desc ed in
      let ts' = List.map to_internal_expr_desc ts in
      LetRecFunWithType (fs', ed', ts')
  | LetFunWithType (f_sig, ed, t) ->
      let f_sig' = transform_funsig f_sig in
      let ed' = to_internal_expr_desc ed in
      let t' = to_internal_expr_desc t in
      LetFunWithType (f_sig', ed', t')
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

and to_internal_expr_desc (e : Bluejay_ast.expr_desc) : syn_bluejay_edesc =
  let tag' = e.tag in
  let e' = to_internal_expr e.body in
  { tag = tag'; body = e' }

and transform_funsig (f_sig : Bluejay_ast.funsig) : 'a funsig =
  let (Bluejay_ast.Funsig (f, args, f_body)) = f_sig in
  let f_body' = to_internal_expr_desc f_body in
  Funsig (f, args, f_body')

let rec from_jay_expr (e : Jay.Jay_ast.expr) : core_bluejay =
  let pat_conv (p : Jay.Jay_ast.pattern) : pattern =
    match p with
    | AnyPat -> AnyPat
    | IntPat -> IntPat
    | BoolPat -> BoolPat
    | FunPat -> FunPat
    | RecPat r -> RecPat r
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
      let fs' = List.map transform_funsig' fs in
      let ed' = from_jay_expr_desc ed in
      LetRecFun (fs', ed')
  | LetFun (f_sig, ed) ->
      let f_sig' = transform_funsig' f_sig in
      let ed' = from_jay_expr_desc ed in
      LetFun (f_sig', ed')
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

and from_jay_expr_desc (e : Jay.Jay_ast.expr_desc) : core_bluejay_edesc =
  let tag' = e.tag in
  let e' = from_jay_expr e.body in
  { tag = tag'; body = e' }

and transform_funsig' (f_sig : Jay.Jay_ast.funsig) : core_only funsig =
  let (Jay.Jay_ast.Funsig (f, args, f_body)) = f_sig in
  let f_body' = from_jay_expr_desc f_body in
  Funsig (f, args, f_body')

let rec to_jay_expr (e : core_bluejay) : Jay.Jay_ast.expr =
  let pat_conv (p : pattern) : Jay.Jay_ast.pattern =
    match p with
    | AnyPat -> AnyPat
    | IntPat -> IntPat
    | BoolPat -> BoolPat
    | FunPat -> FunPat
    | RecPat r -> RecPat r
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
      let fs' = List.map transform_funsig' fs in
      let ed' = to_jay_expr_desc ed in
      LetRecFun (fs', ed')
  | LetFun (f_sig, ed) ->
      let f_sig' = transform_funsig' f_sig in
      let ed' = to_jay_expr_desc ed in
      LetFun (f_sig', ed')
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

and to_jay_expr_desc (e : core_bluejay_edesc) : Jay.Jay_ast.expr_desc =
  let tag' = e.tag in
  let e' = to_jay_expr e.body in
  { tag = tag'; body = e' }

and transform_funsig' (f_sig : core_only funsig) : Jay.Jay_ast.funsig =
  let (Funsig (f, args, f_body)) = f_sig in
  let f_body' = to_jay_expr_desc f_body in
  Jay.Jay_ast.Funsig (f, args, f_body')

let is_type_expr (ed : syn_bluejay_edesc) : bool =
  match ed.body with
  | TypeVar _ | TypeInt | TypeBool | TypeRecord _ | TypeList _ | TypeArrow _
  | TypeArrowD _ | TypeUnion _ | TypeIntersect _ | TypeSet _ | TypeRecurse _ ->
      true
  | _ -> false