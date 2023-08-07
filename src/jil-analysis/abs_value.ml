open Core
open Jayil
open Dj_common
module Ctx = Finite_callstack.CS_2

module AVal = struct
  module T = struct
    type t = AInt | ABool of bool | AClosure of Id.t * Abs_exp.t * Ctx.t | Any
    and aenv = t Map.M(Id).t [@@deriving equal, compare, hash, sexp]
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
end

type aval_set = Set.M(AVal).t
type env_set = Set.M(AEnv).t [@@deriving equal, compare, hash, sexp]

open AVal.T

let pp fmter = function
  | AInt -> Fmt.string fmter "int"
  | ABool b -> Fmt.pf fmter "B:%B" b
  | Any -> Fmt.string fmter "any"
  | AClosure (x, _, _ctx) -> Fmt.pf fmter "{%a}" Id.pp x

let show = Fmt.to_to_string
let e1 = AInt

let e2 =
  let c0 = Map.empty (module Id) in
  let c1 = Map.add_exn c0 ~key:(Id.Ident "x") ~data:e1 in
  let c2 = Map.add_exn c1 ~key:(Id.Ident "y") ~data:e1 in
  AClosure
    (Id.s_ "x", [ Abs_exp.Clause (Id.s_ "z", Abs_exp.Value Int) ], Ctx.empty)

(* let () =
   pp Fmt.stdout e1 ;
   pp Fmt.stdout e2 ;
   () *)

type store = AEnv.t Map.M(Ctx).t

(* multimap *)
type astore = env_set Map.M(Ctx).t [@@deriving equal, compare, hash, sexp]

let safe_add_store store ctx aenv =
  Map.update store ctx ~f:(function
    | Some envs -> Set.add envs aenv
    | None -> Set.singleton (module AEnv) aenv)

module Abs_result = struct
  module T = struct
    type t = AVal.t * env_set Map.M(Ctx).t
    [@@deriving equal, compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)

  let only v = Set.singleton v
  let empty = Set.empty
end

type result_set = Set.M(Abs_result).t [@@deriving equal, compare, hash, sexp]
(* type result_set2 = Hash_set.M(Abs_result).t *)

(* let aeval e env store : result_set = Set.empty (module Abs_result) *)

let to_string rset = Sexp.to_string_hum (sexp_of_result_set rset)
let env_to_string aenv = Sexp.to_string_hum (sexp_of_aenv aenv)
