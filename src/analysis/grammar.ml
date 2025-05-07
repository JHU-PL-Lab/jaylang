
open Core
open Lang.Ast

module Callstack = struct
  module T = struct
    type t = Callsight.t list [@@deriving compare, sexp]
  end

  include T

  let empty : t = []

  let k = 0

  (* very inefficient for now. Also uses fixed k  *)
  let k_cons : t -> Callsight.t -> t = fun stack callsight ->
    List.take (callsight :: stack) k

  module Map = Map.Make (T)
end

module Closure = struct
  type t = { body : Embedded.With_callsights.t ; callstack : Callstack.t }
end

module rec Value : sig 
  type t =
    | VPosInt
    | VNegInt
    | VZero
    | VTrue
    | VFalse
    | VFunClosure of { param : Ident.t ; body : Closure.t }
    | VFrozen of Closure.t
    | VVariant of { label : VariantLabel.t ; payload : t }
    | VRecord of t RecordLabel.Map.t
    | VId

  val compare : t -> t -> int

  val any_int : t M.m
  val any_bool : t M.m

  val op : t -> Lang.Ast.Binop.t -> t -> t M.m
  val not_ : t -> t M.m
end = struct
  type t =
    | VPosInt
    | VNegInt
    | VZero
    | VTrue
    | VFalse
    | VFunClosure of { param : Ident.t ; body : Closure.t }
    | VFrozen of Closure.t
    | VVariant of { label : VariantLabel.t ; payload : t }
    | VRecord of t RecordLabel.Map.t
    | VId
    (* We don't yet handle tables. That will be a failure case in the analysis *)

  (* we're willing to do structural (and therefore intensional) comparison *)
  let compare = Poly.compare

  let any_int = M.choose [ VPosInt ; VNegInt ; VZero ]
  let any_bool = M.choose [ VFalse ; VTrue ]
  let type_mismatch = M.fail Err.type_mismatch

  open M

  let plus (x : t) (y : t) : t m =
    match x, y with
    | VPosInt, VPosInt
    | VPosInt, VZero
    | VZero, VPosInt -> return VPosInt
    | VZero, VZero -> return VZero
    | VNegInt, VNegInt
    | VNegInt, VZero
    | VZero, VNegInt -> return VNegInt
    | VPosInt, VNegInt
    | VNegInt, VPosInt -> any_int
    | _ -> type_mismatch

  let minus (x : t) (y : t) : t m =
    match x, y with
    | VPosInt, VNegInt
    | VPosInt, VZero
    | VZero, VNegInt -> return VPosInt
    | VZero, VZero -> return VZero
    | VNegInt, VZero
    | VNegInt, VPosInt
    | VZero, VPosInt -> return VNegInt
    | VPosInt, VPosInt
    | VNegInt, VNegInt -> any_int
    | _ -> type_mismatch

  let times (x : t) (y : t) : t m =
    match x, y with
    | VPosInt, VPosInt
    | VNegInt, VNegInt -> return VPosInt
    | VPosInt, VNegInt
    | VNegInt, VPosInt -> return VNegInt
    | VZero, VZero
    | VZero, VPosInt
    | VZero, VNegInt
    | VPosInt, VZero
    | VNegInt, VZero -> return VZero
    | _ -> type_mismatch

  (* If abs x < abs y, then x / y = 0 *)
  let divide (x : t) (y : t) : t m =
    match x, y with
    | VPosInt, VNegInt
    | VNegInt, VPosInt -> choose [ VZero ; VNegInt ]
    | VPosInt, VPosInt
    | VNegInt, VNegInt -> choose [ VZero ; VPosInt ]
    | VZero, VPosInt
    | VZero, VNegInt -> return VZero
    | _ -> type_mismatch (* includes divide by zero *)

  (* Modulus follows the sign of x in `x mod y`. *)
  let modulus (x : t) (y : t) : t m =
    match x, y with
    | VPosInt, VNegInt
    | VPosInt, VPosInt -> choose [ VZero ; VPosInt ]
    | VNegInt, VPosInt
    | VNegInt, VNegInt -> choose [ VZero ; VNegInt ]
    | VZero, VPosInt
    | VZero, VNegInt -> return VZero
    | _ -> type_mismatch (* includes modulus by zero *)

  (* Works on bools and ints *)
  let equal (x : t) (y : t) : t m =
    match x, y with
    | VPosInt, VPosInt
    | VNegInt, VNegInt -> any_bool
    | VPosInt, VZero
    | VPosInt, VNegInt
    | VNegInt, VZero
    | VNegInt, VPosInt
    | VZero, VPosInt
    | VZero, VNegInt
    | VTrue, VFalse
    | VFalse, VTrue -> return VFalse
    | VTrue, VTrue
    | VFalse, VFalse
    | VZero, VZero -> return VTrue
    | _ -> type_mismatch

  let not_ (x : t) : t m =
    match x with
    | VTrue -> return VFalse
    | VFalse -> return VTrue
    | _ -> type_mismatch

  let not_equal (x : t) (y : t) : t m =
    let%bind b = equal x y in
    not_ b

  let less_than (x : t) (y : t) : t m =
    match x, y with
    | VPosInt, VPosInt
    | VNegInt, VNegInt -> any_bool
    | VPosInt, VZero
    | VPosInt, VNegInt
    | VZero, VNegInt
    | VZero, VZero -> return VFalse
    | VNegInt, VPosInt
    | VNegInt, VZero
    | VZero, VPosInt -> return VTrue
    | _ -> type_mismatch

  let geq (x : t) (y : t) : t m =
    let%bind b = less_than x y in
    not_ b

  let greater_than (x : t) (y : t) : t m =
    match x, y with
    | VPosInt, VPosInt
    | VNegInt, VNegInt -> any_bool
    | VPosInt, VZero
    | VPosInt, VNegInt
    | VZero, VNegInt -> return VTrue
    | VNegInt, VZero
    | VNegInt, VPosInt
    | VZero, VZero
    | VZero, VPosInt -> return VFalse
    | _ -> type_mismatch

  let leq (x : t) (y : t) : t m =
    let%bind b = greater_than x y in
    not_ b

  let and_ (x : t) (y : t) : t m =
    match x, y with
    | VTrue, VTrue -> return VTrue
    | VTrue, VFalse
    | VFalse, VTrue
    | VFalse, VFalse -> return VFalse
    | _ -> type_mismatch

  let or_ (x : t) (y : t) : t m =
    match x, y with
    | VFalse, VFalse -> return VFalse
    | VTrue, VFalse
    | VFalse, VTrue
    | VTrue, VTrue -> return VTrue
    | _ -> type_mismatch

  let op (left : t) (binop : Lang.Ast.Binop.t) (right : t) : t m =
    let f =
      match binop with
      | BPlus -> plus
      | BMinus -> minus
      | BTimes -> times
      | BDivide -> divide
      | BModulus -> modulus
      | BEqual -> equal
      | BNeq -> not_equal
      | BLessThan -> less_than
      | BLeq -> leq
      | BGreaterThan -> greater_than
      | BGeq -> geq
      | BAnd -> and_
      | BOr -> or_
    in
    f left right
end

and Err : sig
  type t 
  val type_mismatch : t
  val abort : t
  val unbound_variable : Ident.t -> t
end = struct
  type t =
    | Type_mismatch 
    | Unbound_variable of Ident.t
    | Abort

  let type_mismatch : t = Type_mismatch
  let abort : t = Abort
  let unbound_variable : Ident.t -> t = fun id -> Unbound_variable id
end

and Env : sig 
  type t
  val empty : unit -> t (* abstracted just to have one safe recursive module *)
  val compare : t -> t -> int
  val add : Ident.t -> Value.t -> t -> t
  val find : Ident.t -> t -> Value.t option
end = struct
  type t = Value.t Ident.Map.t
  let empty () = Ident.Map.empty
  let compare = Ident.Map.compare Value.compare
  let add id v env = Map.set env ~key:id ~data:v
  let find id env = Map.find env id
end

and Env_set : sig
  type t
  val compare : t -> t -> int
  val union : t -> t -> t
  val singleton : Env.t -> t
  val to_env : t -> Env.t M.m
end = struct
  type t = Env.t list

  let compare = List.compare Env.compare

  let union x y =
    List.fold x ~init:[] ~f:(fun acc env ->
      env :: List.filter y ~f:(fun env' -> not @@ phys_equal env' env) @ acc
    )

  let singleton env = [ env ]

  let to_env = M.choose
end

and Store : sig
  type t 
  val compare : t -> t -> int
  val cons : (Callstack.t * Env_set.t) -> t -> t
  val find : Callstack.t -> t -> Env_set.t option
  val empty : t
end = struct
  type t = Env_set.t Callstack.Map.t

  let compare = Callstack.Map.compare Env_set.compare

  let cons (callstack, env_set) store =
    Map.update store callstack ~f:(function
      | Some env_set' -> Env_set.union env_set' env_set
      | None -> env_set
    )

  let find callstack store = Map.find store callstack

  let empty = Callstack.Map.empty
end

and M : sig 
  type 'a m
  val run_for_error : 'a m -> (unit, Err.t) Result.t
  val return : 'a -> 'a m
  val fail : 'a. Err.t -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val choose : 'a list -> 'a m
  val get : Store.t m
  val modify : (Store.t -> Store.t) -> unit m
  val local : (Env.t -> Env.t) -> 'a m -> 'a m
  val with_call : Callsight.t -> 'a m -> 'a m
  val ask_env : Env.t m
  val ask_callstack : Callstack.t m
  val ask : (Env.t * Callstack.t) m
  val vanish : 'a m
  val log : Embedded.With_callsights.t -> 'a m -> 'a m
end = struct
  module R = struct
    (* the read environment *)
    type t = { env : Env.t ; callstack : Callstack.t ; cache : Cache.t }
    (* need to abstract this or at runtime we get "Undefined recursive module" *)
    let empty () = { env = Env.empty () ; callstack = Callstack.empty ; cache = Cache.empty }
  end
  type 'a m = Store.t -> R.t -> (('a * Store.t) list, Err.t) Result.t

  let run_for_error : 'a m -> (unit, Err.t) Result.t = fun x ->
    match x Store.empty (R.empty ()) with
    | Ok _ -> Ok ()
    | Error e -> Error e

  let return : 'a -> 'a m = fun a ->
    fun s _ -> Ok [ a, s ]

  let fail : Err.t -> 'a m = fun e ->
    fun _ _ -> Error e

  let bind : 'a m -> ('a -> 'b m) -> 'b m = fun x f ->
    fun store r ->
      match x store r with
      | Error e -> Error e
      | Ok lst -> begin
        let rec loop acc = function
        | [] -> Ok acc
        | (a_hd, s_hd) :: tl ->
          match f a_hd s_hd r with
          | Error e -> Error e
          | Ok ls -> loop (ls @ acc) tl
        in
        loop [] lst
      end

  let choose : 'a list -> 'a m = fun a_ls ->
    fun s _ -> Ok (List.map a_ls ~f:(fun a -> a, s))

  let get : Store.t m =
    fun s _ -> Ok [ s, s ]

  let modify : (Store.t -> Store.t) -> unit m = fun f ->
    fun s _ -> Ok [ (), f s ]

  let local : (Env.t -> Env.t) -> 'a m -> 'a m = fun f x ->
    fun s r ->
      x s { r with env = (f r.env) }

  let with_call : Callsight.t -> 'a m -> 'a m = fun callsight x ->
    fun s r ->
      x s { r with callstack= Callstack.k_cons r.callstack callsight }

  let ask_env : Env.t m =
    fun s r -> Ok [ r.env, s ]

  let ask_callstack : Callstack.t m =
    fun s r -> Ok [ r.callstack, s ]

  let ask : (Env.t * Callstack.t) m =
    fun s r -> Ok [ (r.env, r.callstack), s ]

  let vanish : 'a m =
    fun _ _ -> Ok []

  (*
    Log this expression as seen, and vanish if it had already been seen before.
    We don't log values, but it would cause no harm because the cache is like env.

    So the cache should actually be a reader setup, and we log the expressions
    locally.
  *)
  let log : Embedded.With_callsights.t -> 'a m -> 'a m = fun expr x ->
    match expr with
    | EInt _ | EBool _ | EVar _ | EPick_i | EPick_b -> x (* these are fine to re-evalaute *)
    | _ -> (* handle non-values *)
      fun s r ->
        match Cache.put r.cache r.callstack expr r.env s with
        | `Existed_already -> Ok [] (* equivalent to behavior of `vanish` above *)
        | `Added_to new_cache -> x s { r with cache = new_cache }
end

and Cache : sig
  type t 
  val empty : t
  val put : t -> Callstack.t -> Embedded.With_callsights.t -> Env.t -> Store.t -> [ `Existed_already | `Added_to of t ]
end = struct
  module Key = struct
    type expr = Embedded.With_callsights.t

    let compare_expr = Poly.compare (* polymorphic compare is good enough *)

    type t =
      { stack : Callstack.t
      ; expr  : expr
      ; env   : Env.t
      ; store : Store.t
      } [@@deriving compare]
  end

  module S = Stdlib.Set.Make (Key)

  type t = S.t

  let empty : t = S.empty

  let put t stack expr env store =
    let key = Key.{ stack ; expr ; env ; store } in
    if S.mem key t
    then `Existed_already
    else `Added_to (S.add key t)
end
