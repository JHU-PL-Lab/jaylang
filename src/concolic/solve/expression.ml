
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
      | Const_bool : bool -> bool t
      | Const_int : int -> int t 
      | Abstract_int : int e -> int t
      | Abstract_bool : bool e -> bool t

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
  | Const_bool _
  | Const_int _ -> true
  | _ -> false

let simplify (type a b) (construct : a t -> a t -> b t) (op : a -> a -> b t) (x : a t) (y : a t) : b t =
  match x, y with
  | Const_int xi, Const_int yi -> op xi yi
  | Const_bool xb, Const_bool yb -> op xb yb
  | _ -> construct x y

let simplify_int op construct =
  simplify (fun x y -> Abstract_int (construct x y)) (fun x y -> Const_int (op x y))

let simplify_bool op construct =
  simplify (fun x y -> Abstract_bool (construct x y)) (fun x y -> Const_bool (op x y))

let bool_ key = Abstract_bool (Bool_key key)
let int_ key = Abstract_int (Int_key key)

let not_ (x : bool t) : bool t = match x with Const_bool b -> Const_bool (not b) | _ -> Abstract_bool (Not x)
let alias (x : _ t) : _ t = x
let plus = simplify_int ( + ) (fun a b -> Plus (a, b))
let minus = simplify_int ( - ) (fun a b -> Minus (a, b))
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

module Cache =
  struct
    module M = Map.Make (Concolic_key)
    type t =
      { b_exprs : bool T.t M.t
      ; i_exprs : int T.t M.t }

    let empty =
      { b_exprs = M.empty
      ; i_exprs = M.empty }

    let add_expr (type a) (m : t) (key : Concolic_key.t) (expr : a T.t) : t =
      match expr with
      | (Const_bool _ | Abstract_bool _) as data -> { m with b_exprs = Map.set m.b_exprs ~key ~data }
      | (Const_int _ | Abstract_int _) as data -> { m with i_exprs = Map.set m.i_exprs ~key ~data }

    let add_alias (key1 : Concolic_key.t) (key2 : Concolic_key.t) (m : t) : t =
      match Map.find m.b_exprs key2 with
      | Some expr -> { m with b_exprs = Map.set m.b_exprs ~key:key1 ~data:expr }
      | None -> { m with i_exprs = Map.set m.i_exprs ~key:key1 ~data:(Map.find_exn m.i_exprs key2)}

    let lookup_bool (m : t) (key : Concolic_key.t) : bool T.t =
      Map.find_exn m.b_exprs key

    let lookup_int (m : t) (key : Concolic_key.t) : int T.t =
      Map.find_exn m.i_exprs key

    let is_const_bool (m : t) (key : Concolic_key.t) : bool =
      is_const
      @@ lookup_bool m key

    let not_ (m : t) (x : Concolic_key.t) (y : Concolic_key.t) : t =
      add_expr m x
      @@ not_ (lookup_bool m y)

    let binop (key : Concolic_key.t) (untyped_binop : Untyped_binop.t) (left : Concolic_key.t) (right : Concolic_key.t) (m : t) : t =
      let op_bools (type a) (op : bool T.t -> bool T.t -> a T.t) : t =
        add_expr m key @@ op (lookup_bool m left) (lookup_bool m right)
      in
      let op_ints (type a) (op : int T.t -> int T.t -> a T.t) : t =
        add_expr m key @@ op (lookup_int m left) (lookup_int m right)
      in
      match untyped_binop with
      | Plus -> op_ints plus
      | Minus -> op_ints minus
      | Times -> op_ints times
      | Divide -> op_ints divide
      | Modulus -> op_ints divide
      | Less_than -> op_ints less_than
      | Less_than_eq -> op_ints less_than_eq
      | Equal_int -> op_ints equal_int
      | Not_equal -> op_ints not_equal
      | Equal_bool -> op_bools equal_bool
      | And -> op_bools and_
      | Or -> op_bools or_
  end

module Resolve = 
  struct
    let rec int_t_to_formula (x : int t) : int C_sudu.Gexpr.t =
      match x with
      | Const_int i -> C_sudu.box_int i
      | Abstract_int ex -> int_e_to_formula ex

    and bool_t_to_formula (x : bool t) : bool C_sudu.Gexpr.t =
      match x with
      | Const_bool b -> C_sudu.box_bool b
      | Abstract_bool ex -> bool_e_to_formula ex

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