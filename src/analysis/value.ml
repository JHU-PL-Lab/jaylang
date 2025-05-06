
open Core
open Lang.Ast

(* There's no great way to reuse previous value definitions from Lang module. Just rewrite *)
type t =
  | VPosInt
  | VNegInt
  | VZero
  | VTrue
  | VFalse
  | VFunClosure of { param : Ident.t ; body : closure }
  | VFrozen of closure
  | VVariant of { label : VariantLabel.t ; payload : t }
  | VRecord of t RecordLabel.Map.t
  | VId
  (* We don't yet handle tables. That will be a failure case in the analysis *)

and closure = { body : Embedded.With_callsights.t ; callstack : Callstack.t }
(* can compare closures by comparing expressions intensionally. This is probably slow though *)
(* notably, we don't care about thawing nondeterminism. Since we assume all results, the inputs are "deterministic", so we can peval `thaw (freeze e)` to just `e`. Can also just remove thaw/freeze altogether if we're sure the fixed point combinator will terminate without it *)

type error = 
  | VTypeMismatch
  | VAbort
  | VDiverge (* not exactly error. Not sure where to put this *)
  | VUnboundVariable of Ident.t

type t_or_error = (t, error) Result.t

let of_int = function
  | 0 -> VZero
  | n -> if n < 0 then VNegInt else VPosInt
  
let of_bool = function
  | true -> VTrue
  | false -> VFalse

let not_ = function
  | VTrue -> Ok VFalse
  | VFalse -> Ok VTrue
  | _ -> Error VTypeMismatch

module Env = struct
  type nonrec t = t Ident.Map.t
end

module Store = struct
  type t = Env.t Callstack.Map.t
end
