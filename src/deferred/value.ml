
open Core
open Lang.Ast

module E = Embedded.With_program_points (* the expressions we work over *)

(* deferred values *)

(* TODO: functor to allow concolic values *)
(* or just make normal values extensible, but I think I don't want to deal with the consequences of that *)

module C = struct
  type 'a error = 'a constraint 'a = [> `Error ]
  type 'a symbol = 'a constraint 'a = [> `Symbol ]
  type 'a ok = 'a constraint 'a = [> `Ok ]

  (* I used to use [< `Ok | `Symbol ], but that kept around *too much* info about subtrees *)
  type 'a safe = 'a constraint 'a = [< `Ok | `Symbol ]
end

open C

(*
  May want one constructor for errors, which has the further reason underneath because
  then it's easier to match on errors.
*)

type _ t =
  | VUnit : 'a ok t
  | VInt : int -> 'a ok t
  | VBool : bool -> 'a ok t
  | VFunClosure : { param : Ident.t ; closure : closure } -> 'a ok t
  | VVariant : { label : VariantLabel.t ; payload : safe_t } -> 'a ok t
  | VRecord : safe_t RecordLabel.Map.t -> 'a ok t
  | VModule : safe_t RecordLabel.Map.t -> 'a ok t
  | VTypeMismatch : 'a error t (* probably needs callstack *)
  | VAbort : Timestamp.t -> 'a error t
  | VDiverge : Timestamp.t -> 'a error t
  | VUnboundVariable : Ident.t * Timestamp.t -> 'a error t
  | VId : 'a ok t
  | VFrozen : closure -> 'a ok t
  | VUntouchable : safe_t -> 'a ok t
  | VSymbol : Timestamp.t -> 'a symbol t

and env = safe_t Ident.Map.t

and closure = { body : E.t ; env : env }

and safe_t = Safe : 'a safe t -> safe_t [@@unboxed]

(* Values in weak head normal form *)
type whnf = [ `Ok ] t

(* Values that are not errors *)
type nonerr = [ `Ok | `Symbol ] t

(* Error values *)
type err = [ `Error ] t

(* All values *)
type any = [ `Error | `Ok | `Symbol ] t

(* let f (a : any) : unit =
  match a with
  | VUnit -> ()
  | VInt _ -> ()
  | VBool _ -> ()
  | VFunClosure _ -> ()
  | VVariant _ -> ()
  | VRecord _ -> ()
  | VModule _ -> ()
  | VTypeMismatch -> ()
  | VAbort _ -> ()
  | VDiverge _ -> ()
  | VUnboundVariable _ -> ()
  | VId -> ()
  | VFrozen _ -> ()
  | VUntouchable _ -> ()
  | VSymbol _ -> () *)

(* returns binding if there is a matching pattern *)
let matches (v : whnf) (p : Embedded.pattern) : [ `Matches | `Matches_with of safe_t * Ident.t | `No_match ] =
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
  | PVariable id, v -> `Matches_with (Safe v, id)
  | PVariant { variant_label ; payload_id }, VVariant { label ; payload }
      when VariantLabel.equal variant_label label ->
    `Matches_with (payload, payload_id)
  | _ -> `No_match
