
open Core

module Typed_binop = struct
  type iii = int * int * int
  type iib = int * int * bool
  type bbb = bool * bool * bool

  type _ t =
    | Plus : iii t
    | Minus : iii t
    | Times : iii t
    | Divide : iii t
    | Modulus : iii t
    | Less_than : iib t
    | Less_than_eq : iib t
    | Greater_than : iib t
    | Greater_than_eq : iib t
    | Equal_int : iib t
    | Equal_bool : bbb t
    | Not_equal : iib t
    | And : bbb t
    | Or : bbb t
end

module type KEY = sig
  type t
  val uid : t -> int
end

module Symbol = struct
  type ('a, 'k) t = ('a, string) Utils.Separate.t (* should be private *)

  let make_int (k : 'k) (uid : 'k -> int) : (int, 'k) t =
    I (string_of_int @@ uid k)

  let make_bool (k : 'k) (uid : 'k -> int) : (bool, 'k) t =
    B (string_of_int @@ uid k)
end

module Make_symbol (Key : KEY) = struct
  type 'a t = ('a, Key.t) Symbol.t

  let make_int (k : Key.t) : int t =
    Symbol.make_int k Key.uid

  let make_bool (k : Key.t) : bool t =
    Symbol.make_bool k Key.uid
end

module type S = sig
  type ('a, 'k) t

  val equal : ('a, 'k) t -> ('a, 'k) t -> bool

  val const_int : int -> (int, 'k) t
  val const_bool : bool -> (bool, 'k) t

  val symbol : ('a, 'k) Symbol.t -> ('a, 'k) t

  val not_ : (bool, 'k) t -> (bool, 'k) t

  val binop : ('a, 'k) t -> ('a, 'k) t -> ('a * 'a * 'b) Typed_binop.t -> ('b, 'k) t

  val is_const : ('a, 'k) t -> bool

  val and_ : (bool, 'k) t list -> (bool, 'k) t

  (*
    The mli won't expose these 
  *)
  module Private : sig
    val smt_symbol : ('a, 'k) Symbol.t -> Smtml.Symbol.t
    val smt_expr : ('a, 'k) t -> Smtml.Expr.t
  end
end

module T : S = struct
  module S = Smtml
  type ('a, 'k) t = S.Expr.t (* will need to be private *)

  let equal = S.Expr.equal

  let const_int (i : int) : (int, 'k) t = S.Expr.value (S.Value.Int i)
  let const_bool (b : bool) : (bool, 'k) t = S.Expr.value (if b then S.Value.True else S.Value.False)

  (*
    Smtml is really slow at this. Like really slow.
    Maybe has to do with hashing? They hashcons _something_. 
  *)
  let symbol (type a) (s : (a, 'k) Symbol.t) : (a, 'k) t =
    match s with
    | I k -> S.Expr.symbol @@ S.Symbol.make S.Ty.Ty_int k
    | B k -> S.Expr.symbol @@ S.Symbol.make S.Ty.Ty_bool k

  let not_ (e : (bool, 'k) t) : (bool, 'k) t =
    S.Expr.Bool.not e

  (* TODO: make sure division and modulus are like OCaml *)
  (* I really have this ty thing. Which type do they mean? Inputs or output? *)
  let binop (type a b) (x : (a, 'k) t) (y : (a, 'k) t) (op : (a * a * b) Typed_binop.t) : (b, 'k) t =
    let f = 
      match op with
      | Plus            -> S.Expr.binop S.Ty.Ty_int  S.Ty.Binop.Add 
      | Minus           -> S.Expr.binop S.Ty.Ty_int  S.Ty.Binop.Sub 
      | Times           -> S.Expr.binop S.Ty.Ty_int  S.Ty.Binop.Mul 
      | Divide          -> S.Expr.binop S.Ty.Ty_int  S.Ty.Binop.Div  (* TODO *)
      | Modulus         -> S.Expr.binop S.Ty.Ty_int  S.Ty.Binop.Rem  (* TODO *)
      | Less_than       -> S.Expr.relop S.Ty.Ty_bool S.Ty.Relop.Lt
      | Less_than_eq    -> S.Expr.relop S.Ty.Ty_bool S.Ty.Relop.Le
      | Greater_than    -> S.Expr.relop S.Ty.Ty_bool S.Ty.Relop.Gt
      | Greater_than_eq -> S.Expr.relop S.Ty.Ty_bool S.Ty.Relop.Ge
      | Equal_int       -> S.Expr.relop S.Ty.Ty_bool S.Ty.Relop.Eq
      | Equal_bool      -> S.Expr.relop S.Ty.Ty_bool S.Ty.Relop.Eq
      | Not_equal       -> S.Expr.relop S.Ty.Ty_bool S.Ty.Relop.Ne
      | And             -> S.Expr.binop S.Ty.Ty_bool S.Ty.Binop.And 
      | Or              -> S.Expr.binop S.Ty.Ty_bool S.Ty.Binop.Or
    in
    f x y

  let is_const (type a) (x : (a, 'k) t) : bool =
    not (S.Expr.is_symbolic x)

  let and_ (exprs : (bool, 'k) t list) : (bool, 'k) t =
    List.fold ~f:S.Expr.Bool.and_ ~init:(const_bool true) exprs

  module Private = struct
    let smt_symbol : type a. (a, 'k) Symbol.t -> Smtml.Symbol.t = function
      | I k -> Smtml.Symbol.make Smtml.Ty.Ty_int k
      | B k -> Smtml.Symbol.make Smtml.Ty.Ty_bool k

    let[@inline always] smt_expr x = x
  end
end

include T

module Model = struct
  type 'k t = { value : 'a. ('a, 'k) Symbol.t -> 'a option }

  (* FIXME: 'k shouldn't be possible here, I thought *)
  let of_smt_model (model : Smtml.Model.t) : 'k t =
    let value : type a. (a, 'k) Symbol.t -> a option = fun s ->
      match Smtml.Model.evaluate model (Private.smt_symbol s) with
      | Some v -> begin
        match s, v with
        | I _, Int i -> Some i
        | B _, True -> Some true
        | B _, False -> Some false
        | _ -> failwith "Invariant failure: wrong type for symbol in model."
      end
      | None -> None
    in
    { value }
end

type 'k model = 'k Model.t

type 'k solution =
  | Sat of 'k model 
  | Unknown
  | Unsat

module type SOLVER = sig
  val solve : (bool, 'k) t list -> 'k solution
end

module type S1 = sig
  module Key : KEY
  module Symbol : sig
    type 'a t = ('a, Key.t) Symbol.t
    val make_int : Key.t -> int t
    val make_bool : Key.t -> bool t
  end
  
  type 'a t = ('a, Key.t) T.t
end

(*
  Here, we rely on internal correctness, and externally the types will keep everything correct.
*)
module Make (Key : KEY) : S1 with module Key = Key = struct
  module Key = Key
  module Symbol = Make_symbol (Key)

  type 'a t = ('a, Key.t) T.t
end

(*
  BIG PROBLEM: this cannot support parallelism.
    Smtml just isn't set up for it: it only ever works
    with one Z3 context.

  So I need to back off from this representation, but I
  just make use of better representations here to refactor
  my current Z3-specific stuff into a respresentation a
  bit like this one.

  That is, pull expressions out of concolic and make them
  more general, like this.
*)
module Make_solver (Mappings : Smtml.Mappings.S) () : SOLVER = struct
  let solver = Mappings.Solver.make () 

  let solve (exprs : (bool, 'k) t list) : 'k solution =
    let assumptions = [ Private.smt_expr @@ and_ exprs ] in
    match Mappings.Solver.check solver ~assumptions with
    | `Sat -> 
      let model = Option.value_exn @@ Mappings.Solver.model solver in
      let smt_model = Mappings.values_of_model model in
      let value : type a. (a, 'k) Symbol.t -> a option = fun s ->
        match Smtml.Model.evaluate smt_model (Private.smt_symbol s) with
        | Some v -> begin
          match s, v with
          | I _, Int i -> Some i
          | B _, True -> Some true
          | B _, False -> Some false
          | _ -> failwith "Invariant failure: wrong type for symbol in model."
        end
        | None -> None
      in
      Sat { value }
    | `Unknown -> Unknown
    | `Unsat -> Unsat
end

module Z3 = Make_solver (Smtml.Z3_mappings)
module Alt_ergo = Make_solver (Smtml.Altergo_mappings) (* Way slower than Z3 *)
module Cvc5 = Make_solver (Smtml.Cvc5_mappings) (* Not installed yet *)
