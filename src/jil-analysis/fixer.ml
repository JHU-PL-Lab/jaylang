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

(* let rec aeval (store0, aenv0, ctx, cls) rel =
   let rec loop store aenv cl_hd cl_tl : result_set =
     let (Clause (x, _cl)) = cl_hd in
     let cl_result = aeval_clause (store, aenv, ctx, cl_hd) rel in
     if List.is_empty cl_tl
     then cl_result
     else
       bind cl_result (fun cl_v cl_store ->
           let aenv' = Map.add_exn aenv ~key:x ~data:cl_v in
           loop cl_store aenv' (List.hd_exn cl_tl) (List.tl_exn cl_tl))
   in
   loop store0 aenv0 (List.hd_exn cls) (List.tl_exn cls) *)

(* let rec aeval (store0, aenv0, ctx, cls) rel =
   let cl_prev, cl_last = (List.drop_last_exn cls, List.last_exn cls) in
   (* let res_hd = aeval_clause (store0, aenv0, ctx, cl_hd) rel in *)
   if List.is_empty cl_prev
   then aeval_clause (store0, aenv0, ctx, cl_last) rel
   else
     let res_prev = rel (store0, aenv0, ctx, cl_prev) in
     let (Clause (x, _)) = List.last_exn cl_prev in
     bind res_prev (fun cl_v cl_store ->
         let aenv' = Map.add_exn aenv0 ~key:x ~data:cl_v in
         aeval_clause (cl_store, aenv', ctx, cl_last) rel) *)

let rec mk_aeval (store0, aenv0, ctx, cls) aeval : result_set =
  let cl_hd, cl_tl = (List.hd_exn cls, List.tl_exn cls) in
  (* let res_hd = aeval_clause (store0, aenv0, ctx, cl_hd) rel in *)
  if List.is_empty cl_tl
  then mk_aeval_clause (store0, aenv0, ctx, cl_hd) aeval
  else
    let res_hd = aeval (store0, aenv0, ctx, [ cl_hd ]) in
    let (Clause (x, _)) = cl_hd in
    bind res_hd (fun cl_v cl_store ->
        let aenv' = Map.add_exn aenv0 ~key:x ~data:cl_v in
        mk_aeval (cl_store, aenv', ctx, cl_tl) aeval)

and mk_aeval_clause (store, aenv, ctx, Clause (x, clb)) aeval =
  Fmt.pr "-> %a in %s. %s\n" Id.pp x
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
      | AClosure (id, e, saved_context) ->
          let envs = Map.find_exn store saved_context in
          let ctx' = Ctx.push (x, Abs_exp.to_id x1) ctx in
          Abs_result.empty
      | _ -> Abs_result.empty)
  | Binop (x1, _binop, x2) ->
      let v1 = Map.find_exn aenv (Abs_exp.to_id x1) in
      let v2 = Map.find_exn aenv (Abs_exp.to_id x2) in
      Abs_result.only (v1, store)
  | _ -> failwith "why"

let solution = F.lfp mk_aeval

let analyze e =
  let ae = Abs_exp.lift_expr e in
  solution (Map.empty (module Ctx), Map.empty (module Id), Ctx.empty, ae)
