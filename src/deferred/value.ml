
open Core

include Deferred_concolic.Dvalue.Make (Utils.Identity)

module Without_symbols = Lang.Value.Embedded (Utils.Identity)

(*
  Values cannot be recursive, so this will terminate.
  I am not worrying about performance. This will recompute the same value many
  times in case the symbol shows up in several spots.
*)
let of_concolic (v : Deferred_concolic.Value.whnf) (m : Deferred_concolic.Value.Symbol_map.t) : Without_symbols.t =
  let rec subst (v : Deferred_concolic.Value.t) : Without_symbols.t =
    match v with
    | VSymbol t ->
      Deferred_concolic.Time_map.find t m
      |> Deferred_concolic.Value.cast_up
      |> subst
    (* Nothing to do *)
    | VId -> VId
    | VUnit -> VUnit
    | VInt (i, _) -> VInt i
    | VBool (b, _) -> VBool b
    (* Homomorphic *)
    | VVariant { label ; payload } -> VVariant { label ; payload = subst payload }
    | VRecord record_body -> VRecord (Map.map record_body ~f:subst)
    | VModule module_body -> VModule (Map.map module_body ~f:subst)
    | VUntouchable v -> VUntouchable (subst v)
    (* Expressions FIXME : subst into env *)
    | VFunClosure { param ; closure = { env = _ ; body } } -> VFunClosure { param ; closure = { env = Without_symbols.Env.empty ; body } }
    | VFrozen { env = _ ; body } -> VFrozen { env = Without_symbols.Env.empty ; body }
    (* Unhandled *)
    | VTable { alist } -> VTable { alist = List.map alist ~f:(fun (dom, cod) -> subst dom, subst cod) }
  in
  subst (Deferred_concolic.Value.cast_up v)