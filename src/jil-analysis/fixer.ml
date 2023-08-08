open Core
open Fix
open Dj_common
open Abs_value

module Quadruple_as_key = struct
  type t = astore * AEnv.t * Ctx.t * Abs_exp.t [@@deriving equal, hash]
end

module Pair_as_prop = struct
  type property = result_set

  let bottom = Abs_result.Set.empty
  let equal = Abs_result.Set.equal
  let is_maximal _v = false
end

module F = Fix.ForHashedType (Quadruple_as_key) (Pair_as_prop)
open Abs_exp.T

let bind (rset : result_set) f : result_set =
  Set.fold rset ~init:Abs_result.Set.empty ~f:(fun acc (v, store) ->
      Set.union acc @@ f v store)

let bind_env (env_set : env_set) f : result_set =
  Set.fold env_set ~init:Abs_result.Set.empty ~f:(fun acc env ->
      Set.union acc @@ f env)

let rec mk_aeval (store0, aenv0, ctx, e0) aeval : result_set =
  match e0 with
  | Just cl -> mk_aeval_clause (store0, aenv0, ctx, cl) aeval
  | More (cl, e) ->
      let res_hd = aeval (store0, aenv0, ctx, Just cl) in
      bind res_hd (fun cl_v cl_store ->
          let aenv' =
            Map.add_exn aenv0 ~key:(Abs_exp.id_of_clause cl) ~data:cl_v
          in
          mk_aeval (cl_store, aenv', ctx, e) aeval)

and mk_aeval_clause (store, aenv, ctx, Clause (x0, clb)) aeval : result_set =
  let env_get x = Map.find_exn aenv (Abs_exp.to_id x) in
  Fmt.pr "-> %a in %s. %s\n" Id.pp x0
    (Abs_exp.clb_to_string clb)
    (env_to_string aenv) ;
  match clb with
  | Value Int -> Abs_result.only (AInt, store)
  | Value (Bool b) -> Abs_result.only (ABool b, store)
  | Value (Function (x, e)) ->
      let v = AVal.AClosure (Abs_exp.to_id x, e, ctx) in
      let store' = safe_add_store store ctx aenv in
      Abs_result.only (v, store')
  | Appl (x1, x2) -> (
      let v1 = Map.find_exn aenv (Abs_exp.to_id x1) in
      let v2 = Map.find_exn aenv (Abs_exp.to_id x2) in
      match v1 with
      | AClosure (xc, e, saved_context) ->
          let saved_envs = Map.find_exn store saved_context in
          let ctx' = Ctx.push (x0, Abs_exp.to_id x1) ctx in
          bind_env saved_envs (fun saved_env ->
              let env_new = Map.add_exn saved_env ~key:xc ~data:v2 in
              aeval (store, env_new, ctx', e))
      | _ -> Abs_result.empty)
  | CVar x -> Abs_result.only (env_get x, store)
  | Not x ->
      let v = env_get x in
      Abs_result.only (v, store)
  | Binop (x1, _binop, x2) ->
      let v1 = env_get x1 in
      let v2 = env_get x2 in
      Abs_result.only (ABool true, store)
  | Cond (x, e1, e2) -> (
      match env_get x with
      | ABool true -> aeval (store, aenv, ctx, e1)
      | ABool false -> aeval (store, aenv, ctx, e2)
      | _ -> Abs_result.empty)
  | _ -> failwith "unknown clause"

let solution = F.lfp mk_aeval

let analyze e =
  let ae = Abs_exp.lift_expr e in
  solution (Map.empty (module Ctx), Map.empty (module Id), Ctx.empty, ae)
