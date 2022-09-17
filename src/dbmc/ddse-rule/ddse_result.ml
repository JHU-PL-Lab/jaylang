open Core
open Dj_common

type t = {
  v : Lookup_key.t;
  decisions : Id.t Map.M(Rstack).t;
  cond_decisions : bool Map.M(Lookup_key).t;
  phis : Phi_set.t;
  top_stack : Concrete_stack.t option;
}

let equal t1 t2 =
  let ans =
    Lookup_key.compare t1.v t2.v = 0
    && Map.equal Id.equal t1.decisions t2.decisions
    && Map.equal Bool.equal t1.cond_decisions t2.cond_decisions
    (* && Phi_set.compare t1.phis t2.phis = 0 *)
  in

  (*
     if (not ans) && Phi_set.compare t1.phis t2.phis = 0 then assert false else () ;
     if ans && Phi_set.compare t1.phis t2.phis <> 0 then assert false else () ; *)
  ans

let empty_decisions = Map.empty (module Rstack)
let empty_cond_decisions = Map.empty (module Lookup_key)

let of3 v phis s =
  {
    v;
    phis;
    decisions = empty_decisions;
    cond_decisions = empty_cond_decisions;
    top_stack = Some s;
  }

let with_v_and_phi v phi t = { t with v; phis = Phi_set.(add t.phis phi) }
let add_phi phi t = { t with phis = Phi_set.(add t.phis phi) }
let add_phis phis t = { t with phis = Phi_set.(union t.phis phis) }

let merge_decisions d1 d2 =
  Map.merge_skewed d1 d2 ~combine:(fun ~key v1 v2 ->
      let _ = key in
      if Id.equal v1 v2 then v1 else failwith "inconsistent")

let merge_cond_decisions d1 d2 =
  Map.merge_skewed d1 d2 ~combine:(fun ~key v1 v2 ->
      let _ = key in
      if Bool.equal v1 v2 then v1 else failwith "inconsistent")

let add_decisions decisions (choices : Decision.t list) =
  List.fold choices ~init:decisions ~f:(fun ds d ->
      Map.update ds d.r_stk ~f:(function
        | Some v -> if Id.equal v d.v then d.v else failwith "inconsistent"
        | None -> d.v))

let add_cond_decisions cond_decisions (choices : (Lookup_key.t * bool) list) =
  List.fold choices ~init:cond_decisions ~f:(fun ds (key, beta) ->
      Map.update ds key ~f:(function
        | Some v -> if Bool.equal v beta then v else failwith "inconsistent"
        | None -> beta))

let merge_left t1 t2 =
  let inequal_stk =
    match (t1.top_stack, t2.top_stack) with
    | Some s1, Some s2 -> not (Concrete_stack.equal s1 s2)
    | _ -> true
  in
  if inequal_stk
  then None
  else
    try
      let decisions = merge_decisions t1.decisions t2.decisions in
      let cond_decisions =
        merge_cond_decisions t1.cond_decisions t2.cond_decisions
      in
      let phis = Phi_set.union t1.phis t2.phis in
      Some { t1 with decisions; cond_decisions; phis }
    with _ -> None

let merge_with_v v bop t1 t2 =
  match merge_left t1 t2 with
  | Some t ->
      let phi = Riddler.binop v bop t1.v t2.v in
      Some { t with v; phis = Phi_set.add t.phis phi }
  | None -> None

(*
        v;
   decisions = Set.union t1.decisions t2.decisions;
   phis = Phi_set.union t1.phis t2.phis;
   top_stack = t1.top_stack; *)

let merge_all t1 t2 phis choices cond_choices =
  match merge_left t1 t2 with
  | Some t -> (
      try
        Some
          {
            t with
            phis = Phi_set.union t.phis phis;
            decisions = add_decisions t.decisions choices;
            cond_decisions = add_cond_decisions t.cond_decisions cond_choices;
          }
      with _ -> None)
  | None -> None
