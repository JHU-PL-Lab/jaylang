
open Core

module SuduZ3 = Sudu.Simpler_z3_api.Make (struct
  let ctx = Z3.mk_context []
end)

(*
  I think this is actually a good start. I want to note that we either have an int key or a bool key,
  so I should give the key a type parameter.

  When I do it this way, I get one big formula to simplify that doesn't have many equalities. I wonder
  what is easier for the solver. However, I definitely need to reuse some subexpressions. Because this
  is a general program we're talking about. I can't expect it to be beneficial to recompute an expression
  every time I need it.

  I wish that I had a way to recognize a constant key and return that, but then my program is dependently typed.
  This is because I want to accept two keys, and if both of them are const, then I can just give the const
  value they produce. Otherwise, I'll just set as the abstract operation. When I go to convert all formulas
  I need into their expressions, I will do this (noting that a formula is strictly a constructor on two keys,
  and it is not some big recursive type):
    * Maintain a map of the keys to the expressions
    * If the key is in the map, then take the expression
    * If the key is not in the map, then compute its expression (recursively) and add to the map
    * Then fold to the next expression in the list.
  In reality, what is happening is the expression I return is simply a variable name that holds the value I want,
  and I am also returning all the expressions that need to be added to the solver in order for those variable names
  to work well. I do think I should inject constants into the expressions, however, in case the variable comes out
  constant, so there are two cases:
    * The key evaluates to a constant, in which case I have a constant expression that I use directly
    * The key evaluates to a non constant, in which case I get an expression that assigns it, and I use the variable on the left of that assignment for all future uses, so this variable is what is in the map.
  ... so I want to have a GADT for my expressions that tell me that it is a `var expr`, or something like that (where `var` is probably just a phantom type,
  however I should really have `int_var` and `bool_var` phantom types, and I have two maps, where one looks up bools
  and another looks up ints, and that way the maps are well typed and I don't need to try to treat them with some sort
  of fancy subtyping with polymorphic variants).

  e.g. something like
    module M = Map.Make (Concolic_key)
    type int_expr =
      | Const_int expr (* promised to be a constant int expression *)
      | Int_var expr (* promised to be a variable *)
    type map_for_int_keys = int_expr M.t

  But when I write this out, I see it doesn't really matter whether it is a variable or not. That's because if both
  returned expressions (from the operation I'm trying to do to resolve a key) would have been constant, then I already
  would have seen that anyways when making the key (the one I'm trying to resolve) and made it a constant expression
  to begin with, so there are no two expressions after all, and we have a contradiction.
  This works on stuff like alias because I don't have to make a new expression, and I simply say that the key I
  just resolved has the same expression as the one I alias, which is a copy, and transitively these resolve out to
  have very few aliases in the end.

  Maybe I really want to map to a const `int` so that way I can apply simplifications.
  e.g.
    (1 + x) - 1
  is just x, but if I keep these as Z3 expressions only, then I can't simplify this.
  The alternative is I have a big tree for every expression, which leads to repeated subexpressions
  and excessive computation to even reduce it, but the solver is already quite quick, and
  it's possible that all the work to reduce is almost not worth it (I do think that an OCaml simplification
  is more efficient than the abstract Z3 simplifications because of the added effect of fewer clauses in the
  solver, but maybe only marginally, given the performance we already have.)

  I want to have a GADT api for a Z3 expression, where I have like
  module Expr = struct
    type _ t =
      | Int : Z3.Expr.expr -> int t
      | Bool : Z3.Expr.expr -> bool t
  end
  But I do wonder if care to have Int_var and Bool_var constructors too. Probably not. Those can just be functions, I think,
  because they would lead to too much casing when it doens't matter at all. Remember that that is just for the solver api
  and not for this frontend that I'm creating right now.

  I'm beginning to rethink everything about the concolic keys. I need a unique identifier for each clause
  that actually gets put into the solver, and this is done well with step count.
  I also "need" it in order to track hit counts, and generally as a quick comparison for targets.
  However, I don't need it for anything else. So I might drop key altogether, and just create an expression while I give
  it the step count in case it needs a unique identifier.
  Or, better, is to keep key as is, and denv also passes around expressions, and I have a type that is an key
  and expression pair. The problem is that when fetching I would need to know the expression type. That's not sleek.
  The problem is that I need to map to the expressions, but they have two different types (int t and bool t), so I need
  two different maps, and when I go to look up a variable, I don't yet know its type (because it is susceptible to a type error).
  I know the type of expression once I get the dvalue, so I can wait until then, but it feels ridiculous to case on the dvalue so much.
*)

type _ t =
  | Const_bool : bool -> bool t
  | Const_int : int -> int t 
  | Input : int t (* only input. NOTE: Need to make sure that all input clauses are known to model so I can extract a value *)
  | Abstract_int : int e -> int t
  | Abstract_bool : bool e -> bool t

(* abstract expressions only *)
and _ e =
  | Not : bool t -> bool e
  | Plus : int t * int t -> int e
  | Times : int t * int t -> int e
  | Divide : int t * int t -> int e
  | Modulus : int t * int t -> int e
  | Less_than : int t * int t -> bool e
  | Less_than_eq : int t * int t -> bool e
  | Equal_int : int t * int t -> bool e
  | Equal_bool : bool t * bool t -> bool e
  | Not_equal : int t * int t -> bool e
  | And : bool t * bool t -> bool e
  | Or : bool t * bool t -> bool e

let is_const : type a. a t -> bool = function
  | Const_bool _
  | Const_int _ -> true
  | _ -> false

let alias (x : _ t) : _ t =
  x

let simplify (type a b) (construct : a t -> a t -> b t) (op : a -> a -> b t) (x : a t) (y : a t) : b t =
  match x, y with
  | Const_int xi, Const_int yi -> op xi yi
  | Const_bool xb, Const_bool yb -> op xb yb
  | _ -> construct x y

let simplify_int op construct =
  simplify (fun x y -> Abstract_int (construct x y)) (fun x y -> Const_int (op x y))

let simplify_bool op construct =
  simplify (fun x y -> Abstract_bool (construct x y)) (fun x y -> Const_bool (op x y))

let plus = simplify_int ( + ) (fun a b -> Plus (a, b))
let times = simplify_int ( * ) (fun a b -> Times (a, b))
let divide = simplify_int ( / ) (fun a b -> Divide (a, b))
let modulus = simplify_int ( mod ) (fun a b -> Modulus (a, b))
let less_than = simplify_bool ( < ) (fun a b -> Less_than (a, b))
let less_than_eq = simplify_bool ( <= ) (fun a b -> Less_than_eq (a, b))
let equal_int = simplify_bool ( = ) (fun a b -> Equal_int (a, b))
let equal_bool = simplify_bool Bool.( = ) (fun a b -> Equal_bool (a, b))
let not_equal = simplify_bool ( <> ) (fun a b -> Not_equal (a, b))
let and_ = simplify_bool ( && ) (fun a b -> And (a, b))
let or_ = simplify_bool ( || ) (fun a b -> Or (a, b))

(* I may want to return multiple formulas: all those that are needed to be put into the solver *)
let rec int_t_to_formula (x : int t) : Z3.Expr.expr =
  match x with
  | Const_int i -> SuduZ3.box_int i
  | Input -> SuduZ3.int_var 0 (* TODO: payload for input with key *)
  | Abstract_int ex -> int_e_to_formula ex

and bool_t_to_formula (x : bool t) : Z3.Expr.expr =
  match x with
  | Const_bool b -> SuduZ3.box_bool b
  | Abstract_bool ex -> bool_e_to_formula ex

and bool_e_to_formula (x : bool e) : Z3.Expr.expr =
  match x with
  | Not y -> SuduZ3.not_ (bool_t_to_formula y)
  | _ -> failwith "unimplemented"

and int_e_to_formula (x : int e) : Z3.Expr.expr =
  match x with
  | Plus (y, z) -> Z3.Arithmetic.mk_add SuduZ3.ctx [ int_t_to_formula y ; int_t_to_formula z ]
  | _ -> failwith "unimplemented"
  
