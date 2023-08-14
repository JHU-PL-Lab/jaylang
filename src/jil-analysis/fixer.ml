open Core
open Fix
open Dj_common
open Abs_value

module Quadruple_as_key = struct
  type t = AStore.t * AEnv.t * Ctx.t * Abs_exp.t [@@deriving equal, hash]
end

module Pair_as_prop = struct
  type property = result_set

  let bottom = Abs_result.Set.empty
  let equal = Abs_result.Set.equal
  let is_maximal _v = false
end

module F = Fix.ForHashedType (Quadruple_as_key) (Pair_as_prop)
open Abs_exp.T

let bind (type a) set (f : a -> result_set) : result_set =
  Set.fold set ~init:Abs_result.Set.empty ~f:(fun acc elem ->
      Set.union acc @@ f elem)

let binop bop v1 v2 =
  let open AVal.T in
  match bop with
  | Binary_operator_plus | Binary_operator_minus | Binary_operator_times
  | Binary_operator_divide | Binary_operator_modulus | Binary_operator_less_than
  | Binary_operator_less_than_or_equal_to -> (
      match (v1, v2) with
      | AInt, AInt -> Set.singleton (module AVal) AInt
      | _ -> Set.empty (module AVal))
  | Binary_operator_equal_to | Binary_operator_not_equal_to -> (
      match (v1, v2) with
      | AInt, AInt -> Set.(of_list (module AVal) [ ABool true; ABool false ])
      | _ -> Set.empty (module AVal))
  | Binary_operator_and -> (
      match (v1, v2) with
      | ABool b1, ABool b2 -> Set.singleton (module AVal) (ABool (b1 && b2))
      | _ -> Set.empty (module AVal))
  | Binary_operator_or -> (
      match (v1, v2) with
      | ABool b1, ABool b2 -> Set.singleton (module AVal) (ABool (b1 || b2))
      | _ -> Set.empty (module AVal))

let not_ = function
  | AVal.ABool b -> Set.singleton (module AVal) (ABool (not b))
  | _ -> Set.empty (module AVal)

let rec mk_aeval (store0, aenv0, ctx, e0) aeval : result_set =
  match e0 with
  | Just cl -> mk_aeval_clause (store0, aenv0, ctx, cl) aeval
  | More (cl, e) ->
      let res_hd = aeval (store0, aenv0, ctx, Just cl) in
      bind res_hd (fun (cl_v, cl_store) ->
          let aenv' =
            Map.add_exn aenv0 ~key:(Abs_exp.id_of_clause cl) ~data:cl_v
          in
          mk_aeval (cl_store, aenv', ctx, e) aeval)

and mk_aeval_clause (store, aenv, ctx, Clause (x0, clb)) aeval : result_set =
  (* Mismatch step 2: fetch x from the wrong env *)
  (* let env_get_exn x = Map.find_exn aenv (Abs_exp.to_id x) in *)
  let env_get x = Map.find aenv (Abs_exp.to_id x) in
  let env_get_bind x f =
    Option.value_map (env_get x) ~default:Abs_result.empty ~f
  in
  let env_add_bind env x v f =
    let ar = Map.add env ~key:x ~data:v in
    match ar with `Ok env' -> f env' | `Duplicate -> Abs_result.empty
  in
  Fmt.pr "@\n%a with env @[<h>%a@]@\n with store %a@\n" Abs_exp.pp_clause
    (Clause (x0, clb))
    AEnv.pp aenv AStore.pp store ;
  match clb with
  | Value Int -> Abs_result.only (AInt, store)
  | Value (Bool b) -> Abs_result.only (ABool b, store)
  | Value (Function (x, e)) ->
      let v = AVal.AClosure (Abs_exp.to_id x, e, ctx) in
      let store' = safe_add_store store ctx aenv in
      Abs_result.only (v, store')
  | Appl (x1, x2) -> (
      match (env_get x1, env_get x2) with
      | Some (AClosure (xc, e, saved_context)), Some v2 ->
          (* Mismatch step 1: pick the wrong env *)
          let saved_envs = Map.find_exn store saved_context in
          let ctx' = Ctx.push (x0, Abs_exp.to_id x1) ctx in
          bind saved_envs (fun saved_env ->
              env_add_bind saved_env xc v2 (fun env_new ->
                  aeval (store, env_new, ctx', e))
              (* let env_new = Map.add_exn saved_env ~key:xc ~data:v2 in
                 aeval (store, env_new, ctx', e) *))
      | _ -> Abs_result.empty)
  | CVar x -> env_get_bind x (fun v -> Abs_result.only (v, store))
  | Not x -> (
      match env_get x with
      | Some v -> bind (not_ v) (fun v -> Abs_result.only (v, store))
      | None -> Abs_result.empty)
  | Binop (x1, bop, x2) -> (
      match (env_get x1, env_get x2) with
      | Some v1, Some v2 ->
          let v = binop bop v1 v2 in
          bind v (fun v -> Abs_result.only (v, store))
      | _ -> Abs_result.empty)
  | Cond (x, e1, e2) -> (
      match env_get x with
      | Some (ABool true) -> aeval (store, aenv, ctx, e1)
      | Some (ABool false) -> aeval (store, aenv, ctx, e2)
      | _ -> Abs_result.empty)
  | _ -> failwith "unknown clause"

let solution = F.lfp mk_aeval

let analyze e =
  let ae = Abs_exp.lift_expr e in
  solution (Map.empty (module Ctx), Map.empty (module Id), Ctx.empty, ae)
