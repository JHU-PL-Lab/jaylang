open Core
open Jayil
open Dj_common
module Ctx = Finite_callstack.CS_1

module AVal = struct
  module T = struct
    type t = AInt | ABool of bool | AClosure of Id.t * Abs_exp.t * Ctx.t | Any
    and aenv = t Map.M(Id).t [@@deriving equal, compare, hash, sexp]

    let pp fmter = function
      | AInt -> Fmt.string fmter "n"
      | ABool b -> Fmt.pf fmter "%a" Std.pp_bo b
      | Any -> Fmt.string fmter "?"
      | AClosure (x, _, _ctx) -> Fmt.pf fmter "{%a}" Id.pp x

    let show = Fmt.to_to_string pp
  end

  include T
  include Comparable.Make (T)
end

module AEnv = struct
  module T = struct
    type t = AVal.aenv [@@deriving equal, compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)

  let show aenv = Sexp.to_string_hum (sexp_of_t aenv)

  let pp fmter env =
    let iter f_ m =
      let f ~key ~data = f_ key data in
      Core.Map.iteri m ~f
    in
    Fmt.Dump.iter_bindings iter Fmt.nop Id.pp AVal.pp fmter env
end

(* type aval_set = Set.M(AVal).t *)
type env_set = Set.M(AEnv).t [@@deriving equal, compare, hash, sexp]

let show_env_set es = Sexp.to_string_hum (sexp_of_env_set es)

let pp_env_set : env_set Fmt.t =
  Fmt.iter ~sep:(Fmt.any ";@ ") (fun f set -> Set.iter set ~f) AEnv.pp

open AVal.T

let pp_aval_set : Set.M(AVal).t Fmt.t =
  Fmt.iter ~sep:(Fmt.any ";@ ") (fun f set -> Set.iter set ~f) AVal.pp

(* type store = AEnv.t Map.M(Ctx).t *)

module AStore = struct
  (* multimap *)

  type t = env_set Map.M(Ctx).t [@@deriving equal, compare, hash, sexp]

  let show s = Sexp.to_string_hum (sexp_of_t s)

  let pp fmter store =
    let iter f_ m =
      let f ~key ~data = f_ key data in
      Core.Map.iteri m ~f
    in
    (* Fmt.Dump.iter_bindings iter Fmt.nop Ctx.pp pp_env_set fmter store *)
    Fmt.iter_bindings ~sep:(Fmt.any ";@ ") iter
      (Fmt.pair ~sep:(Fmt.any " -> ") Ctx.pp (Fmt.box pp_env_set))
      fmter store
end

type astore = AStore.t

let safe_add_store store ctx aenv =
  Map.update store ctx ~f:(function
    | Some envs -> Set.add envs aenv
    | None -> Set.singleton (module AEnv) aenv)

module Abs_result = struct
  module T = struct
    type t = AVal.t * AStore.t [@@deriving equal, compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)

  let only v = Set.singleton v
  let empty = Set.empty
  let pp = Fmt.Dump.pair AVal.pp AStore.pp
end

type result_set = Set.M(Abs_result).t [@@deriving equal, compare, hash, sexp]

let pp_result_set : result_set Fmt.t =
  Fmt.iter (fun f set -> Set.iter set ~f) Abs_result.pp

let show_result_set rset = Sexp.to_string_hum (sexp_of_result_set rset)
