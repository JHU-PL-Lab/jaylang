
open Core
open Lang.Ast

module Callstack = struct
  module T = struct
    type t = Callsight.t list [@@deriving compare, sexp]
  end

  include T

  let empty : t = []

  let k = 1

  (* inefficient for now and uses fixed k  *)
  let k_cons : t -> Callsight.t -> t = fun stack callsight ->
    List.take (callsight :: stack) k

  module Map = Map.Make (T)
end

module Closure = struct
  type t = { body : Embedded.With_callsights.t ; callstack : Callstack.t }

  let compare a b =
    match Callstack.compare a.callstack b.callstack with
    | 0 -> Poly.compare a.body b.body (* we're willing to do structural comparison on expressions *)
    | x -> x
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
    | VId [@@deriving compare]
  (** [t] are the abstract values. THis uses polymorphic comparison on closures. *)

  module Set : Stdlib.Set.S with type elt = t
  (** [Set] describes sets of values (note the polymorphic comparison). *)

  val to_string : t -> string

  val any_int : t M.m
  (** [any_int] is a nondeterministic abstract int. *)

  val any_bool : t M.m
  (** [any_bool] is a nondeterministic abstract bool. *)

  val op : t -> Lang.Ast.Binop.t -> t -> t M.m
  (** [op left binop right] is the nondeterministic result of [binop] on [left] and [right]. *)

  val not_ : t -> t M.m
  (** [not_ v] is the negative of [v]. *)
end = struct
  module T = struct
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
      | VId [@@deriving compare]
      (* We don't yet handle tables. That will be a failure case in the analysis *)
  end

  include T

  module Set = Stdlib.Set.Make (T)

  let rec to_string = function
    | VPosInt -> "(+)"
    | VNegInt -> "(-)"
    | VZero -> "0"
    | VTrue -> "true"
    | VFalse -> "false"
    | VFunClosure { param ; _ } -> Format.sprintf "(fun %s -> <expr>)" (Ident.to_string param)
    | VFrozen _ -> "Frozen <expr>"
    | VVariant { label ; payload } -> Format.sprintf "(`%s (%s))" (VariantLabel.to_string label) (to_string payload)
    | VRecord record_body -> RecordLabel.record_body_to_string ~sep:"=" record_body to_string
    | VId -> "(fun x -> x)"

  let any_int = M.choose [ VPosInt ; VNegInt ; VZero ]
  let any_bool = M.choose [ VFalse ; VTrue ]
  let type_mismatch a op b =
    let msg = Format.sprintf "Bad operation: %s %s %s" (to_string a) (Binop.to_string op) (to_string b) in
    M.fail @@ Err.type_mismatch msg

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
    | _ -> type_mismatch x BPlus y

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
    | _ -> type_mismatch x BMinus y

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
    | _ -> type_mismatch x BTimes y

  (* If abs x < abs y, then x / y = 0 *)
  let divide (x : t) (y : t) : t m =
    match x, y with
    | VPosInt, VNegInt
    | VNegInt, VPosInt -> choose [ VZero ; VNegInt ]
    | VPosInt, VPosInt
    | VNegInt, VNegInt -> choose [ VZero ; VPosInt ]
    | VZero, VPosInt
    | VZero, VNegInt -> return VZero
    | _ -> type_mismatch x BDivide y (* includes divide by zero *)

  (* Modulus follows the sign of x in `x mod y`. *)
  let modulus (x : t) (y : t) : t m =
    match x, y with
    | VPosInt, VNegInt
    | VPosInt, VPosInt -> choose [ VZero ; VPosInt ]
    | VNegInt, VPosInt
    | VNegInt, VNegInt -> choose [ VZero ; VNegInt ]
    | VZero, VPosInt
    | VZero, VNegInt -> return VZero
    | _ -> type_mismatch x BModulus y (* includes modulus by zero *)

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
    | _ -> type_mismatch x BEqual y

  let not_ (x : t) : t m =
    match x with
    | VTrue -> return VFalse
    | VFalse -> return VTrue
    | _ -> M.fail @@ Err.type_mismatch (Format.sprintf "Bad not: not (%s)" (to_string x))

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
    | _ -> type_mismatch x BLessThan y

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
    | _ -> type_mismatch x BGreaterThan y

  let leq (x : t) (y : t) : t m =
    let%bind b = greater_than x y in
    not_ b

  let and_ (x : t) (y : t) : t m =
    match x, y with
    | VTrue, VTrue -> return VTrue
    | VTrue, VFalse
    | VFalse, VTrue
    | VFalse, VFalse -> return VFalse
    | _ -> type_mismatch x BAnd y

  let or_ (x : t) (y : t) : t m =
    match x, y with
    | VFalse, VFalse -> return VFalse
    | VTrue, VFalse
    | VFalse, VTrue
    | VTrue, VTrue -> return VTrue
    | _ -> type_mismatch x BOr y

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
  (** [t] are the errors the analysis can find. *)

  val to_string : t -> string

  val type_mismatch : string -> t
  (** [type_mismatch msg] is an error for a type mismatch with the given [msg]. *)

  val abort : string -> t
  (** [abort msg] is an error for an abort with the given [msg]. *)

  val unbound_variable : Ident.t -> t
  (** [unbound_variable id] is an error for the unbound variable [id]. *)
end = struct
  type t =
    | Type_mismatch of string
    | Unbound_variable of Ident.t
    | Abort of string

  let to_string = function
    | Type_mismatch msg -> Format.sprintf "Type mismatch:\n  %s\n" msg
    | Abort msg -> Format.sprintf "Abort:\n  %s\n" msg
    | Unbound_variable Ident s -> Format.sprintf "Unbound variable:\n  %s\n" s

  let type_mismatch : string -> t = fun msg -> Type_mismatch msg
  let abort : string -> t = fun msg -> Abort msg
  let unbound_variable : Ident.t -> t = fun id -> Unbound_variable id
end

and Env : sig 
  type t
  (** [t] are the environments: multimaps from identifiers to abstract values. *)

  val empty : unit -> t
  (** [empty ()] is the empty environment. It is suspended so that there are safe values
      in the recursive module cycle. *)

  val merge : t -> t -> t
  (** [merge a b] is the least multimap containing [a] and [b]. *)

  val compare : t -> t -> int
  (** [compare a b] compares keys and values of [a] and [b], using polymorphic comparison
      in to compare closures. *)

  val add : Ident.t -> Value.t -> t -> t
  (** [add id v env] is the least map containing [t] and [{ id -> v }]. *)

  val find : Ident.t -> t -> Value.t M.m
  (** [find id env] is the nondeterministic value mapped to by [id] in [env]. *)
end = struct
  type t = Value.Set.t Ident.Map.t
  let empty () = Ident.Map.empty

  let compare = Ident.Map.compare Value.Set.compare

  let add id v env = 
    Map.update env id ~f:(function
      | Some set -> Value.Set.add v set
      | None -> Value.Set.singleton v
    )

  let merge x y =
    Map.merge x y ~f:(fun ~key:_ -> function
      | `Left set | `Right set -> Some set
      | `Both (a, b) -> Some (Value.Set.union a b)
    )

  let find id env = 
    match Map.find env id with
    | Some set -> M.choose @@ Value.Set.to_list set
    | None -> M.fail (Err.unbound_variable id)
end

and Store : sig
  type t 
  (** [t] are mappings from callstacks to environments. *)

  val compare : t -> t -> int
  (** [compare a b] compares the maps [a] and [b] using environment comparison. *)

  val add : Callstack.t -> Env.t -> t -> t
  (** [add callstack env t] merges the environments [env] and [t(callstack)]. *)

  val find : Callstack.t -> t -> Env.t
  (** [find callstack env] is [env(callstack)], or an unhandled exception if not found. *)

  val empty : unit -> t
  (** [empty ()] is the empty store. It is suspended only to have a safe value
      in the recursive module cycle. *)
end = struct
  type t = Env.t Callstack.Map.t

  let compare = Callstack.Map.compare Env.compare

  let add callstack env store =
    Map.update store callstack ~f:(function
      | Some env' -> Env.merge env' env
      | None -> env
    )

  let find callstack store = 
    match Map.find store callstack with
    | Some env -> env
    | None -> failwith "Unhandled error: callstack does not map to any environment"

  let empty () = Callstack.Map.empty
end

and M : sig 
  (* M is a monad *)
  type 'a m
  (** ['a m] is a state, reader, nondeterministic, result monad. *)

  val return : 'a -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m

  val run_for_error : 'a m -> (unit, Err.t) Result.t
  (** [run_for_error x] is the result of [x] run on empty initial environment and state. *)

  val fail : 'a. Err.t -> 'a m
  (** [fail err] is the error with message [err]. *)

  (*
    NONDETERMINISM
  *)

  val choose : 'a list -> 'a m
  (** [choose ls] chooses an element from [ls]. *)

  val vanish : 'a m
  (** [vanish] is nothing. It simulates divergence. *)

  (*
    STATE 
  *)

  val modify : (Store.t -> Store.t) -> unit m
  (** [modify f] modifies the state with [f]. *)

  val find_env : Callstack.t -> Env.t m
  (** [bind_env callstack] is the environment associated with [callstack] in the state. *)

  (*
    ENVIRONMENT 
  *)

  val local : (Env.t -> Env.t) -> 'a m -> 'a m
  (** [local f x] computes [x] with the local environment transformation [f]. *)

  val with_call : Callsight.t -> 'a m -> 'a m
  (** [with_call callsight x] computes [x] with the [callsight] locally added to the callstack. *)

  val ask_env : Env.t m
  (** [ask_env] is the environment. *)

  val ask : (Env.t * Callstack.t) m
  (** [ask] is the environment and callstack. *)

  (*
    CACHE 
  *)

  val log : Embedded.With_callsights.t -> 'a m -> 'a m
  (** [log expr x] logs the [expr] as seen in the environment and computes [x] if [expr] is new.
      Otherwise, if [expr] had been seen in this strain before, then it vanishes. *)
end = struct
  module R = struct
    (* the read environment *)
    type t = { env : Env.t ; callstack : Callstack.t ; cache : Cache.t }
    (* need to abstract this or at runtime we get "Undefined recursive module" *)
    let empty () = { env = Env.empty () ; callstack = Callstack.empty ; cache = Cache.empty }
  end
  type 'a m = Store.t -> R.t -> (('a * Store.t) list, Err.t) Result.t

  let run_for_error : 'a m -> (unit, Err.t) Result.t = fun x ->
    match x (Store.empty ()) (R.empty ()) with
    | Ok _ -> Ok ()
    | Error e -> Error e

  let return : 'a -> 'a m = fun a ->
    fun s _ -> Ok [ a, s ]

  let fail : Err.t -> 'a m = fun e ->
    fun _ _ -> Error e

  let[@inline always][@specialize] bind : 'a m -> ('a -> 'b m) -> 'b m = fun x f ->
    fun store r ->
      match x store r with
      | Error e -> Error e
      | Ok lst -> begin
        let rec loop acc = function
        | [] -> Ok (List.dedup_and_sort acc ~compare:(Tuple2.compare ~cmp1:Poly.compare ~cmp2:Store.compare))
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

  let find_env : Callstack.t -> Env.t m = fun callstack ->
    let%bind store = get in
    return @@ Store.find callstack store

  let modify : (Store.t -> Store.t) -> unit m = fun f ->
    fun s _ -> Ok [ (), f s ]

  let local : (Env.t -> Env.t) -> 'a m -> 'a m = fun f x ->
    fun s r ->
      x s { r with env = (f r.env) }

  let with_call : Callsight.t -> 'a m -> 'a m = fun callsight x ->
    fun s r ->
      x s { r with callstack = Callstack.k_cons r.callstack callsight }

  let ask_env : Env.t m =
    fun s r -> Ok [ r.env, s ]

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
    | EInt _ | EBool _ | EVar _ | EPick_i | EPick_b | EFunction _ | EFreeze _ -> x (* these are fine to re-evalaute *)
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
