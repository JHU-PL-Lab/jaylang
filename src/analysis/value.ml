
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

module Env = struct
  type nonrec t = t Ident.Map.t
end

module Store = struct
  type t = Env.t Callstack.Map.t
end
