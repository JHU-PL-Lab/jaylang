
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

    (*
      We want to only restore to the desired snapshot when needed. 
      Therefore, if we go into a snapshot, don't use it, and pop
      back out, then there is nothing to do.

      If we *do* use it, then we need to restore on both sides (entering
      and exiting).

      If we are nested, then how are we supposed to know which snapshot
      needed to be restored? We need a parallel stack (or make it a stack
      of tuples) telling use whether we have restored that.

      In fact a stack is not sufficient. Sometimes we capture not because
      we are about to enter a new state (like about to eval a function
      application), but instead because we are *creating* the closure.

      So the stack is *only* for entering a new state and then popping back
      to the old one. So maybe a stack is sufficient.

      We can have an Env.local function to say locally do all of this
      stuff, and at the end pop back. We also only want to take as many
      snapshots as we need, so we should hold off on taking a snapshot until
      the environment changes. This is actually a very tough question.
      Maybe I need to look into the research for efficient environment management.

      We want to limit our snapshots because each snapshot makes it more expensive
      to restore to other snapshots. So when we choose to evaluate a function
      locally, we should only take the snapshot right before we *change* a cell.
      If we are simply creating a cell, then that doesn't affect the snapshot
      because restoring a snap doesn't destroy new cells. Thus this implementation
      cannot and will never be correct with respect to free variables.

      So when we enter a local computation, set a flag (we're not considering nestedness yet)
      that says to take the snapshot before we change an existing cell. If we are
      about to read from a cell, then we need to restore the state to whatever state
      the local computation wants, which also means we need to take the snapshot right
      before that.
      I think to be careful about *which* cells really need to be restored, we would
      need some alias analysis, which is hard, so I'll ignore that.
      And then when we exit the local computation, we only restore the original state
      if the computation took a snapshot.
    *)

    let fetch id = 
      Store.Ref.get store
      @@ Hashtbl.find_exn env id

    let add id v =
      match Hashtbl.find env id with
      | Some cell -> Store.Ref.set store cell v
      | None ->
        Hashtbl.set env ~key:id ~data:(
          Store.Ref.make store v
        )

    let locally snap f =
      let snap' = Store.capture store in
      Store.restore store snap;
      let res = f () in
      Store.restore store snap';
      res

    let temporarily f =
      let snap = Store.capture store in
      let res = f () in
      Store.restore store snap;
      res
  end
end

include Make (struct type 'a t = 'a * 'a Expression.t let to_string f (v, _) = f v end)
