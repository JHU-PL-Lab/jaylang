open Batteries;;
open Jhupllib;;

type label = Label of string [@@deriving eq, ord, show, to_yojson];;

type ident = Ident of string [@@deriving eq, ord, show, to_yojson];;

module Ident =
struct
  type t = ident;;
  let equal = equal_ident;;
  let compare = compare_ident;;
  let pp = pp_ident;;
  let show = show_ident;;
  let to_yojson = ident_to_yojson;;
  let hash = Hashtbl.hash
end;;

module Ident_set = struct
  module M = Set.Make(Ident);;
  include M;;
  include Pp_utils.Set_pp(M)(Ident);;
  include Yojson_utils.Set_to_yojson(M)(Ident);;
end;;

module Ident_map = struct
  module M = Map.Make(Ident);;
  include M;;
  include Pp_utils.Map_pp(M)(Ident);;
  include Yojson_utils.Map_to_yojson(M)(Ident);;
end;;

type variant_label = Variant_label of string 
[@@deriving eq, ord, show, to_yojson]

(* TODO: Should we keep this? This is essentially keeping the same info as
   patterns.   
*)
type type_sig =
  | TopType
  | IntType
  | BoolType
  | FunType
  | RecType of Ident_set.t
  | ListType
  | VariantType of variant_label
  [@@ deriving eq, ord, show, to_yojson]

(* ******************** AST definition for Natodefa ******************** *)

type pattern = AnyPat | IntPat | BoolPat | FunPat
            | RecPat of (ident option) Ident_map.t
            | VariantPat of variant_label * ident
            | VarPat of ident
            | EmptyLstPat | LstDestructPat of ident * ident
            [@@ deriving eq, ord, show, to_yojson]

type funsig = Funsig of ident * ident list * expr_desc
[@@ deriving eq, ord, show, to_yojson]

and expr_desc = 
  { body : expr;
    tag : int;
  }
[@@ deriving eq, ord, show, to_yojson]

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
  | Record of (expr_desc Ident_map.t) 
  | RecordProj of expr_desc * label
  | Match of expr_desc * (pattern * expr_desc) list
  | VariantExpr of variant_label * expr_desc
  | List of expr_desc list 
  | ListCons of expr_desc * expr_desc  
  | Assert of expr_desc 
  | Assume of expr_desc 
  [@@ deriving eq, ord, show, to_yojson]

module Expr_desc = struct
  type t = expr_desc;;
  let equal = equal_expr_desc;;
  let compare = compare_expr_desc;;
  let to_yojson = expr_desc_to_yojson;;
end;;

module Pattern = struct
  type t = pattern;;
  let equal = equal_pattern;;
  let compare = compare_pattern;;
  let to_yojson = pattern_to_yojson;;
end;;

(* *********** Helper functions for creating new AST nodes *********** *)
let counter = ref 0;;

let fresh_tag () = 
  let c = !counter in
  (counter := c + 1); c

let new_expr_desc : expr -> expr_desc = 
  fun e ->
  {tag = fresh_tag (); body = e}

(* Takes expr as an argument.  Returns the relative precedence of the
    expression.  Higher ints correspond to higher precedences. *)
let expr_precedence_p1 (expr : expr) : int =
  match expr with
  | Function _ | Let _ | LetFun _ | LetRecFun _ | Match _ -> 0
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
;;

(** Takes expressions e1 and e2 as arguments.  Returns 0 if the two
    expressions have equal precedence, a negative int if e1 has lower
    precedence than e2, and a positive int if e1 has higher precedence. *)
let expr_precedence_cmp e1 e2 = (expr_precedence_p1 e1) - (expr_precedence_p1 e2);;

let expr_desc_precedence_cmp : expr_desc -> expr_desc -> int = 
  fun ed1 ed2 ->
    expr_precedence_cmp ed1.body ed2.body