
open Core

module Untyped_binop =
  struct
    type t =
      | Plus
      | Minus
      | Times
      | Divide
      | Modulus
      | Less_than
      | Less_than_eq
      | Equal_int
      | Equal_bool
      | Not_equal
      | And
      | Or
  end

module T =
  struct
    type _ t =
      | Const : 'a -> 'a t
      | Abstract : 'a e -> 'a t

    (* abstract expressions only *)
    and _ e =
      | Bool_key : Concolic_key.t -> bool e
      | Int_key : Concolic_key.t -> int e
      | Not : bool t -> bool e
      | Plus : int t * int t -> int e
      | Minus : int t * int t -> int e
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
  end

include T

let is_const : type a. a t -> bool = function
  | Const _ -> true
  | Abstract _ -> false

let simplify (type a b) (op : a -> a -> b) (construct_e : a t -> a t -> b e) (x : a t) (y : a t) : b t =
  match x, y with
  | Const c1, Const c2 -> Const (op c1 c2)
  | _ -> Abstract (construct_e x y)

let true_ = Const true
let const_bool b = Const b
let const_int i = Const i
let bool_key key = Abstract (Bool_key key)
let int_key key = Abstract (Int_key key)

let not_ (x : bool t) : bool t = match x with Const b -> Const (not b) | _ -> Abstract (Not x)
let plus = simplify ( + ) (fun a b -> Plus (a, b))
let minus = simplify ( - ) (fun a b -> Minus (a, b))
let times = simplify ( * ) (fun a b -> Times (a, b))
let divide = simplify ( / ) (fun a b -> Divide (a, b))
let modulus = simplify ( mod ) (fun a b -> Modulus (a, b))
let less_than = simplify ( < ) (fun a b -> Less_than (a, b))
let less_than_eq = simplify ( <= ) (fun a b -> Less_than_eq (a, b))
let equal_int = simplify ( = ) (fun a b -> Equal_int (a, b))
let equal_bool = simplify Bool.( = ) (fun a b -> Equal_bool (a, b))
let not_equal = simplify ( <> ) (fun a b -> Not_equal (a, b))
let and_ = simplify ( && ) (fun a b -> And (a, b))
let or_ = simplify ( || ) (fun a b -> Or (a, b))

module Cache =
  struct
    module M = Map.Make (Concolic_key)

    module Packed = struct
      type t = Pack : 'a T.t -> t

      (* SCARY: Obj.magic here *)
      let unpack (type a) : t -> a T.t = function Pack e -> Obj.magic e
    end

    type t = Packed.t M.t

    let empty = M.empty

    let add_expr (type a) (m : t) (key : Concolic_key.t) (expr : a T.t) : t =
      Map.set m ~key ~data:(Pack expr)

    let add_alias (key1 : Concolic_key.t) (key2 : Concolic_key.t) (m : t) : t =
      match Map.find m key2 with
      | Some data -> Map.set m ~key:key1 ~data
      | None -> m

    let lookup (m : t) (key : Concolic_key.t) : 'a T.t =
      Packed.unpack
      @@ Map.find_exn m key

    let is_const_bool (m : t) (key : Concolic_key.t) : bool =
      match Map.find m key with
      | Some e -> is_const @@ Packed.unpack e
      | None -> true

    let not_ (m : t) (x : Concolic_key.t) (y : Concolic_key.t) : t =
      add_expr m x
      @@ not_ (lookup m y)

    let binop (key : Concolic_key.t) (untyped_binop : Untyped_binop.t) (left : Concolic_key.t) (right : Concolic_key.t) (m : t) : t =
      let op (type a b) (binop : a T.t -> a T.t -> b T.t) : t =
        add_expr m key @@ binop (lookup m left) (lookup m right)
      in
      match untyped_binop with
      | Plus         -> op plus
      | Minus        -> op minus
      | Times        -> op times
      | Divide       -> op divide
      | Modulus      -> op modulus
      | Less_than    -> op less_than
      | Less_than_eq -> op less_than_eq
      | Equal_int    -> op equal_int
      | Not_equal    -> op not_equal
      | Equal_bool   -> op equal_bool
      | And          -> op and_
      | Or           -> op or_
  end

module Resolve = 
  struct
    let rec int_t_to_formula (x : int t) : int C_sudu.Gexpr.t =
      match x with
      | Const i -> C_sudu.box_int i
      | Abstract e -> int_e_to_formula e

    and bool_t_to_formula (x : bool t) : bool C_sudu.Gexpr.t =
      match x with
      | Const b -> C_sudu.box_bool b
      | Abstract e -> bool_e_to_formula e

    and bool_e_to_formula (x : bool e) : bool C_sudu.Gexpr.t =
      let op_two_ints op e1 e2 = op (int_t_to_formula e1) (int_t_to_formula e2) in
      let op_two_bools op e1 e2 = op (bool_t_to_formula e1) (bool_t_to_formula e2) in
      match x with
      | Bool_key key -> C_sudu.bool_var @@ Concolic_key.uniq_id key
      | Not y -> C_sudu.not_ (bool_t_to_formula y)
      | Less_than (e1, e2) -> op_two_ints C_sudu.less_than e1 e2
      | Less_than_eq (e1, e2) -> op_two_ints C_sudu.less_than_eq e1 e2
      | Equal_int (e1, e2) -> op_two_ints C_sudu.eq_ints e1 e2
      | Equal_bool (e1, e2) -> op_two_bools C_sudu.eq_bools e1 e2
      | Not_equal (e1, e2) -> op_two_ints C_sudu.neq e1 e2
      | And (e1, e2) -> op_two_bools C_sudu.and_ e1 e2
      | Or (e1, e2) -> op_two_bools C_sudu.or_ e1 e2

    and int_e_to_formula (x : int e) : int C_sudu.Gexpr.t =
      let op_two_ints op e1 e2 = op (int_t_to_formula e1) (int_t_to_formula e2) in
      match x with
      | Int_key key -> C_sudu.int_var @@ Concolic_key.uniq_id key
      | Plus (e1, e2) -> op_two_ints C_sudu.plus e1 e2
      | Minus (e1, e2) -> op_two_ints C_sudu.minus e1 e2
      | Times (e1, e2) -> op_two_ints C_sudu.times e1 e2
      | Divide (e1, e2) -> op_two_ints C_sudu.divide e1 e2
      | Modulus (e1, e2) -> op_two_ints C_sudu.modulus e1 e2

  end

let int_t_to_formula = Resolve.int_t_to_formula
let bool_t_to_formula = Resolve.bool_t_to_formula

