
open Core
open Lang.Ast

module Closure = struct
  type t = { body : Embedded.With_callsights.t ; callstack : Callstack.t }
end

module Callstack = struct
  module T = struct
    type t = Callsight.t list [@@deriving compare, sexp]
  end

  include T

  module Map = Map.Make (T)
end

(*
  State, Reader, Error, Nondeterminsm
  Each path can evolve state differently, so we're
  not letting some state on a different side of the program
  infiltrate places it shouldn't.

  If I have a bug, I should try changing this to one state.

  We're not using this functor version because we
  cannot resolve the signature easily.
*)
(* module SREN (State : T) (Read : T) (Err : T) = struct
  type 'a m = State.t -> Read.t -> (('a * State.t) list, Err.t) Result.t

  let return : 'a -> 'a m = fun a ->
    fun s _ -> Ok [ s, a ]

  let fail : Err.t -> 'a m = fun e ->
    fun _ _ -> Error e

  let bind : 'a m -> ('a -> 'b m) -> 'b m = fun x f ->
    fun state env ->
      match x state env with
      | Error e -> Error e
      | Ok lst -> begin
        let rec loop acc = function
        | [] -> Ok acc
        | (a_hd, s_hd) :: tl ->
          match f a_hd s_hd env with
          | Error e -> Error e
          | Ok ls -> loop (ls @ acc) tl
        in
        loop [] lst
      end

  let choose : 'a list -> 'a m = fun a_ls ->
    fun s _ -> Ok (List.map a_ls ~f:(fun a -> a, s))
end *)

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
end

and Err : sig
  type t 
  val type_mismatch : t
  val abort : t
end = struct
  type t =
    | Type_mismatch 
    | Abort

  let type_mismatch : t = Type_mismatch
  let abort : t = Abort
end

and Env : sig 
  (* type t = Value.t Ident.Map.t *)
  type t
  val empty : t
  val add : Ident.t -> Value.t -> t -> t
  val find : Ident.t -> t -> Value.t option
end = struct
  type t = Value.t Ident.Map.t
  let empty = Ident.Map.empty
  let add id v env = Map.set env ~key:id ~data:v
  let find id env = Map.find env id
end

and Env_set : sig
  type t
  val union : t -> t -> t
end = struct
  type t = Env.t list

  let union x y =
    List.fold x ~init:[] ~f:(fun acc env ->
      List.filter y ~f:(fun env' -> not @@ phys_equal env' env) @ acc
    )
end

and Store : sig
  type t 
  val cons : (Callstack.t * Env_set.t) -> t -> t
  val find : Callstack.t -> t -> Env_set.t option
  val empty : t
end = struct
  type t = Env_set.t Callstack.Map.t

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
  val return : 'a -> 'a m
  val fail : Err.t -> 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val choose : 'a list -> 'a m
  val get : Store.t m
  val modify : (Store.t -> Store.t) -> unit m
  val local : (Env.t -> Env.t) -> 'a m -> 'a m
  val ask : Env.t m
end= struct
  type 'a m = Store.t -> Env.t -> (('a * Store.t) list, Err.t) Result.t

  let return : 'a -> 'a m = fun a ->
    fun s _ -> Ok [ a, s ]

  let fail : Err.t -> 'a m = fun e ->
    fun _ _ -> Error e

  let bind : 'a m -> ('a -> 'b m) -> 'b m = fun x f ->
    fun state env ->
      match x state env with
      | Error e -> Error e
      | Ok lst -> begin
        let rec loop acc = function
        | [] -> Ok acc
        | (a_hd, s_hd) :: tl ->
          match f a_hd s_hd env with
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
    fun s e ->
      x s (f e)

  let ask : Env.t m =
    fun s e -> Ok [ e, s ]
end
