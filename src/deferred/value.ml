
open Core
open Lang.Ast

module E = Embedded.With_program_points (* the expressions we work over *)

(* deferred values *)

(* TODO: functor to allow concolic values *)
(* or just make normal values extensible, but I think I don't want to deal with the consequences of that *)

module C = struct
  type 'a symbol = 'a constraint 'a = [> `Symbol ]
  type 'a ok = 'a constraint 'a = [> `Ok ]
end

module Err = struct
  type t =
    | VTypeMismatch of Timestamp.t * string
    | VAbort of Timestamp.t
    | VDiverge of Timestamp.t
    | VUnboundVariable of Ident.t * Timestamp.t
end

open C

(*
  I opt for this representation instead of an Either on normal
  value vs symbol because we can avoid the extra boxing that comes with that.

  The only downside is we have to "cast" sometimes or we get type
  parameters escaping their scopes. The `cast_up` we have later is
  safer than magic and just as fast, but it simply looks word. It also
  skips allocation like an Either would have to do
*)

type _ v =
  | VUnit : 'a ok v
  | VInt : int -> 'a ok v
  | VBool : bool -> 'a ok v
  | VFunClosure : { param : Ident.t ; closure : closure } -> 'a ok v
  | VVariant : { label : VariantLabel.t ; payload : t } -> 'a ok v
  | VRecord : t RecordLabel.Map.t -> 'a ok v
  | VModule : t RecordLabel.Map.t -> 'a ok v
  | VId : 'a ok v
  | VFrozen : closure -> 'a ok v
  | VUntouchable : t -> 'a ok v
  | VSymbol : Timestamp.t -> 'a symbol v

and env = t Ident.Map.t

and closure = { body : E.t ; env : env }

and t = [ `Ok | `Symbol ] v

(* Values in weak head normal form *)
type whnf = [ `Ok ] v

(* Value symbol *)
type symb = [ `Symbol ] v

(* Compiles to identity function and helps cast to the general value type *)
let[@inline always] cast_up (type a) (v : a v) : t =
  match v with
  | (VUnit
    | VInt _
    | VBool _
    | VFunClosure _
    | VVariant _
    | VRecord _
    | VModule _
    | VId
    | VFrozen _ 
    | VUntouchable _
    | VSymbol _) as x -> x

let timestamp_of_symbol (VSymbol t : symb) : Timestamp.t =
  t

(* returns binding if there is a matching pattern *)
let matches (v : whnf) (p : Embedded.pattern) : [ `Matches | `Matches_with of t * Ident.t | `No_match ] =
  match p, v with
  | Pattern.PUntouchable id, VUntouchable v -> `Matches_with (v, id)
  | _, VUntouchable _ -> `No_match
  | PAny, _
  | PInt, VInt _ 
  | PBool, VBool _
  | PUnit, VUnit
  | PRecord, VRecord _ (* Currently, types match this *)
  | PModule, VModule _
  | PFun, VFunClosure _ -> `Matches
  | PType, VRecord m ->
    if List.for_all Lang.Ast_tools.Reserved.[ gen ; check ; wrap ] ~f:(Map.mem m)
    then `Matches
    else `No_match
  | PVariable id, v -> `Matches_with (cast_up v, id)
  | PVariant { variant_label ; payload_id }, VVariant { label ; payload }
      when VariantLabel.equal variant_label label ->
    `Matches_with (payload, payload_id)
  | _ -> `No_match

module Env = struct
  type t = env

  let find id m = Map.find m id
  let add id v m = Map.set m ~key:id ~data:v
end
