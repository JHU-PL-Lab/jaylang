
open Core
open Lang
open Ast

exception UnboundVariable of Ident.t

module type V = sig
  type 'a t
  val to_string : ('a -> string) -> 'a t -> string
end

module Tbl = Hashtbl.Make (Ident)

(*
  V is the payload of int and bool. We do this so that we can
  inject Z3 expressions into the values of the concolic evaluator.
*)
module Make (V : V) = struct
  module T = struct
    type t =
      (* all languages *)
      | VInt of int V.t
      | VBool of bool V.t
      | VFunClosure of { param : Ident.t ; body : closure }
      | VVariant of { label : VariantLabel.t ; payload : t }
      | VRecord of t RecordLabel.Map.t
      | VTypeMismatch
      | VAbort
      | VDiverge
      (* embedded only *)
      | VId
      | VFrozen of closure

    and env = t Store.Ref.t Tbl.t

    and closure = { expr : Constraints.embedded Expr.t ; snap : Store.snapshot } (* an expression to be evaluated in an environment *)
  end

  include T

  let rec to_string : t -> string = function
    | VInt i -> V.to_string Int.to_string i
    | VBool b -> V.to_string Bool.to_string b
    | VFunClosure { param = Ident s ; _ } -> Format.sprintf "(fun %s -> <expr>)" s
    | VVariant { label ; payload } -> Format.sprintf "(`%s (%s))" (VariantLabel.to_string label) (to_string payload)
    | VRecord record_body -> RecordLabel.record_body_to_string ~sep:"=" record_body to_string
    | VTypeMismatch -> "Type_mismatch"
    | VAbort -> "Abort"
    | VDiverge -> "Diverge"
    | VId -> "(fun x -> x)"
    | VFrozen _ -> "(Freeze <expr>)"

  module Env = struct
    type t = env
    let env : t = Hashtbl.create (module Ident)
    let store = Store.create ()

    (* let snapshots = Stack.create () *)

    (* let ref_is_changed = ref false *)
    (* let ref_waiting_to_snapback = ref false *)

    let fetch id = 
      (* if !ref_waiting_to_snapback && !ref_is_changed
      then begin
        let snap = Stack.pop_exn snapshots in
        Store.restore store snap;
      end *)
      Store.Ref.get store
      @@ Hashtbl.find_exn env id

    let add id v =
      match Hashtbl.find env id with
      | Some cell -> 
        (* ref_is_changed := true; *)
        Store.Ref.set store cell v
      | None ->
        Hashtbl.set env ~key:id ~data:(
          Store.Ref.make store v
        )
  end
end

include Make (struct type 'a t = 'a * 'a Expression.t let to_string f (v, _) = f v end)
