
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

(*
  I'll either want this parametrized by the key or allow all keys to map to
  some symbol. I prefer the second as long as I can be sure that I don't use
  a mix of keys to make expresions.
*)

module type S = sig
  module Key : KEY
  module Symbol : Utils.Separate.S with type x = Key.t

  type 'a t (* expressions *)
  (* type model *)

  (* val set_timeout : Core.Time_float.Span.t -> unit *)
  (** [set_timeout t] sets the timeout for a single solve to [t]. *)

  (*
    -------------
    MAKE FORMULAS
    -------------
  *)
  val box_int : int -> int t
  (** [box_int i] is an expression for the constant int [i]. *)

  val box_bool : bool -> bool t
  (** [box_bool b] is an expression for the constant int [b]. *)

  val symbol : 'a Symbol.t -> 'a t

  (*
    ------------------
    VALUES OF FORMULAS 
    ------------------
  *)

  (* val value_of_expr : model -> 'a t -> 'a option *)
  (** [value_of_expr model e] queries the [model] for the OCaml value
      associated with [e]. *)

  (* val constrained_vars : model -> int list *)
  (** [constrained_vars model] is the list of identifiers in the model,
      which all must have been arguments to [int_var] or [bool_var] previously. *)

  (*
    ----------------
    COMBINE FORMULAS
    ----------------
  *)
  val not_ : bool t -> bool t
  val binop : 'a t -> 'a t -> ('a * 'a * 'b) Typed_binop.t -> 'b t

  (*
    ------------------
    ASK ABOUT FORMULAS 
    ------------------
  *)

  val is_const : 'a t -> bool

  (*
    -----
    SOLVE
    -----
  *)
  (* module Solve_status : sig
    type t =
      | Sat of model
      | Unknown
      | Unsat
  end

  val empty_model : model
  (** [empty_model] is the model of an empty solver. *)

  val solve : bool t list -> Solve_status.t *)
  (** [solve exprs] invokes the [Z3] solver for a solution to the [exprs]. *)
end


module Make (Key : KEY) (*: S with module Key = Key*) = struct
  module Key = Key

  module Symbol = Utils.Separate.Make (Key)

  module S = Smtml
  include Utils.Separate.Make (S.Expr)
  
  let box_int i = int_ @@ S.Expr.value (S.Value.Int i)
  let box_bool b = bool_ @@ S.Expr.value (if b then S.Value.True else S.Value.False)

  let symb_of_key k ty =
    Key.uid k
    |> string_of_int
    |> S.Symbol.make ty
    |> S.Expr.symbol

  let symbol (type a) (s : a Symbol.t) : a t =
    match s with
    | I k -> int_ @@ symb_of_key k S.Ty.Ty_int
    | B k -> bool_ @@ symb_of_key k S.Ty.Ty_bool

  let not_ (B e : bool t) : bool t =
    bool_ @@ S.Expr.Bool.not e

  (* TODO: make sure division and modulus are like OCaml *)
  let binop (type a b) (x : a t) (y : a t) (op : (a * a * b) Typed_binop.t) : b t =
    match op, x, y with
    | Plus,            I x, I y -> int_  @@ S.Expr.binop S.Ty.Ty_int  S.Ty.Binop.Add x y
    | Minus,           I x, I y -> int_  @@ S.Expr.binop S.Ty.Ty_int  S.Ty.Binop.Sub x y
    | Times,           I x, I y -> int_  @@ S.Expr.binop S.Ty.Ty_int  S.Ty.Binop.Mul x y
    | Divide,          I x, I y -> int_  @@ S.Expr.binop S.Ty.Ty_int  S.Ty.Binop.Div x y (* TODO *)
    | Modulus,         I x, I y -> int_  @@ S.Expr.binop S.Ty.Ty_int  S.Ty.Binop.Rem x y (* TODO *)
    | Less_than,       I x, I y -> bool_ @@ S.Expr.relop S.Ty.Ty_int  S.Ty.Relop.Lt x y
    | Less_than_eq,    I x, I y -> bool_ @@ S.Expr.relop S.Ty.Ty_int  S.Ty.Relop.Le x y
    | Greater_than,    I x, I y -> bool_ @@ S.Expr.relop S.Ty.Ty_int  S.Ty.Relop.Gt x y
    | Greater_than_eq, I x, I y -> bool_ @@ S.Expr.relop S.Ty.Ty_int  S.Ty.Relop.Ge x y
    | Equal_int,       I x, I y -> bool_ @@ S.Expr.relop S.Ty.Ty_int  S.Ty.Relop.Eq x y
    | Equal_bool,      B x, B y -> bool_ @@ S.Expr.relop S.Ty.Ty_bool S.Ty.Relop.Eq x y
    | Not_equal,       I x, I y -> bool_ @@ S.Expr.relop S.Ty.Ty_int  S.Ty.Relop.Ne x y
    | And,             B x, B y -> bool_ @@ S.Expr.binop S.Ty.Ty_bool S.Ty.Binop.And x y
    | Or,              B x, B y -> bool_ @@ S.Expr.binop S.Ty.Ty_bool S.Ty.Binop.Or x y

  let is_const (type a) (x : a t) : bool =
    not (S.Expr.is_symbolic (extract x))
end

(*
  Here, we rely on internal correctness, and externally, the types keep everything correct.
*)
module Without_boxing (Key : KEY) (*: S with module Key = Key*) = struct
  module Key = Key

  module Symbol = Utils.Separate.Make (Key)

  module S = Smtml
  type 'a t = S.Expr.t (* will need to be private *)
  
  let box_int (i : int) : int t = S.Expr.value (S.Value.Int i)
  let box_bool (b : bool) : bool t = S.Expr.value (if b then S.Value.True else S.Value.False)

  let symb_of_key k ty =
    Key.uid k
    |> string_of_int
    |> S.Symbol.make ty
    |> S.Expr.symbol

  let symbol (type a) (s : a Symbol.t) : a t =
    match s with
    | I k -> symb_of_key k S.Ty.Ty_int
    | B k -> symb_of_key k S.Ty.Ty_bool

  let not_ (e : bool t) : bool t =
    S.Expr.Bool.not e

  (* TODO: make sure division and modulus are like OCaml *)
  let binop (type a b) (x : a t) (y : a t) (op : (a * a * b) Typed_binop.t) : b t =
    let f = 
      match op with
      | Plus            -> S.Expr.binop S.Ty.Ty_int  S.Ty.Binop.Add 
      | Minus           -> S.Expr.binop S.Ty.Ty_int  S.Ty.Binop.Sub 
      | Times           -> S.Expr.binop S.Ty.Ty_int  S.Ty.Binop.Mul 
      | Divide          -> S.Expr.binop S.Ty.Ty_int  S.Ty.Binop.Div  (* TODO *)
      | Modulus         -> S.Expr.binop S.Ty.Ty_int  S.Ty.Binop.Rem  (* TODO *)
      | Less_than       -> S.Expr.relop S.Ty.Ty_int  S.Ty.Relop.Lt
      | Less_than_eq    -> S.Expr.relop S.Ty.Ty_int  S.Ty.Relop.Le
      | Greater_than    -> S.Expr.relop S.Ty.Ty_int  S.Ty.Relop.Gt
      | Greater_than_eq -> S.Expr.relop S.Ty.Ty_int  S.Ty.Relop.Ge
      | Equal_int       -> S.Expr.relop S.Ty.Ty_int  S.Ty.Relop.Eq
      | Equal_bool      -> S.Expr.relop S.Ty.Ty_bool S.Ty.Relop.Eq
      | Not_equal       -> S.Expr.relop S.Ty.Ty_int  S.Ty.Relop.Ne
      | And             -> S.Expr.binop S.Ty.Ty_bool S.Ty.Binop.And 
      | Or              -> S.Expr.binop S.Ty.Ty_bool S.Ty.Binop.Or
    in
    f x y

  let is_const (type a) (x : a t) : bool =
    not (S.Expr.is_symbolic x)

  let and_ (exprs : bool t list) : bool t =
    List.fold_left S.Expr.Bool.and_ (box_bool true) exprs

  let z3_solver = S.Z3_mappings.Solver.make ()

  (* TODO: get the model (and convert to a general model type?) *)
  let solve_z3 (exprs : bool t list) : 'a =
    S.Z3_mappings.Solver.check z3_solver ~assumptions:[ and_ exprs ]
end
