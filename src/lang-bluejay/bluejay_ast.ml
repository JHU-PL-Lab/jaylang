open Batteries
open Dj_std.Translation_counter

type label = Label of string [@@deriving eq, ord, show, to_yojson]

type ident = Jayil.Ast.ident = Ident of string
[@@deriving eq, ord, show, to_yojson]

module Ident = Jayil.Ast.Ident
module Ident_set = Jayil.Ast.Ident_set
module Ident_map = Jayil.Ast.Ident_map

type variant_label = Variant_label of string
[@@deriving eq, ord, show, to_yojson]

type type_sig =
  | TopType
  | IntType
  | BoolType
  | FunType
  | RecType of Ident_set.t
  | ListType
  | VariantType of variant_label
(* | UntouchedType of string *)
[@@deriving eq, ord, show, to_yojson]

type pattern =
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
(* | UntouchedPat of string *)
[@@deriving eq, ord, show, to_yojson]

type predicate = expr_desc [@@deriving eq, ord, show, to_yojson]

and funsig = Funsig of ident * ident list * expr_desc
[@@deriving eq, ord, show, to_yojson]

(* Typed function signature: (function name, (arg, type) list, (body, return type))  *)
and typed_funsig =
  | Typed_funsig of ident * (ident * expr_desc) list * (expr_desc * expr_desc)
  | DTyped_funsig of ident * (ident * expr_desc) * (expr_desc * expr_desc)
[@@deriving eq, ord, show, to_yojson]

and expr_desc = { body : expr; tag : int } [@@deriving eq, ord, show, to_yojson]

and expr =
  | Int of int
  | Bool of bool
  | Var of ident
  | Function of ident list * expr_desc
  | Input
  | Appl of expr_desc * expr_desc
  | Let of ident * expr_desc * expr_desc
  | LetRecFun of funsig list * expr_desc
  | LetFun of funsig * expr_desc
  (* (variable name, assigned expr, body expr, vairable type) *)
  | LetWithType of ident * expr_desc * expr_desc * expr_desc
  | LetRecFunWithType of typed_funsig list * expr_desc
  | LetFunWithType of typed_funsig * expr_desc
  | Plus of expr_desc * expr_desc
  | Minus of expr_desc * expr_desc
  | Times of expr_desc * expr_desc
  | Divide of expr_desc * expr_desc
  | Modulus of expr_desc * expr_desc
  | Equal of expr_desc * expr_desc
  | Neq of expr_desc * expr_desc
  | LessThan of expr_desc * expr_desc
  | Leq of expr_desc * expr_desc
  | GreaterThan of expr_desc * expr_desc
  | Geq of expr_desc * expr_desc
  | And of expr_desc * expr_desc
  | Or of expr_desc * expr_desc
  | Not of expr_desc
  | If of expr_desc * expr_desc * expr_desc
  | Record of expr_desc Ident_map.t
  | RecordProj of expr_desc * label
  | Match of expr_desc * (pattern * expr_desc) list
  | VariantExpr of variant_label * expr_desc
  | List of expr_desc list
  | ListCons of expr_desc * expr_desc
  (* TODO: Create a separate class of constructors for type errors? *)
  | TypeError of ident
  | Assert of expr_desc
  | Assume of expr_desc
  (* Type expressions *)
  | TypeVar of ident
  | TypeInt
  | TypeBool
  | TypeRecord of expr_desc Ident_map.t
  | TypeList of expr_desc
  | TypeArrow of expr_desc * expr_desc
  | TypeArrowD of (ident * expr_desc) * expr_desc
  | TypeSet of expr_desc * predicate
  | TypeUnion of expr_desc * expr_desc
  | TypeIntersect of expr_desc * expr_desc
  | TypeRecurse of ident * expr_desc
  | TypeUntouched of string
  | TypeVariant of variant_label * expr_desc
[@@deriving eq, ord, show, to_yojson]

let new_expr_desc : expr -> expr_desc = fun e -> { tag = fresh_n (); body = e }

(* Takes [expr] as an argument.  Returns the relative precedence of the
    expression.  Higher ints correspond to higher precedences. *)
let expr_precedence_p1 (expr : expr) : int =
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

let expr_desc_precedence_cmp : expr_desc -> expr_desc -> int =
 fun ed1 ed2 -> expr_precedence_cmp ed1.body ed2.body
