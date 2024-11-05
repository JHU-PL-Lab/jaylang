open Core
open Fix
open Dj_common
(* open Abs_exp *)

module Make (Ctx : Finite_callstack.C) = struct
  module All_vals = Abs_val.Make (Ctx)
  open All_vals

  let _bind (type a) set (f : a -> result_set) : result_set =
    Set.fold set ~init:Abs_result.empty ~f:(fun acc elem ->
        Set.union acc @@ f elem)

  let bind (type a) set (f : a -> result_set) : result_set =
    set |> Set.to_list |> List.map ~f |> Set.union_list (module Abs_result)

  let lift_store (vs : Set.M(AVal).t) store : result_set =
    Set.map (module Abs_result) vs ~f:(fun v -> (v, store))

  let both_bools : Set.M(AVal).t =
    Set.of_list (module AVal) [ ABool true; ABool false ]

  let just_int : Set.M(AVal).t = Set.of_list (module AVal) [ AInt ]
  let just_bool b : Set.M(AVal).t = Set.of_list (module AVal) [ ABool b ]
  let empty_v : Set.M(AVal).t = Set.empty (module AVal)

  let binop bop v1 v2 =
    let open AVal in
    let open Abs_exp in
    match bop with
    | Binary_operator_plus | Binary_operator_minus | Binary_operator_times
    | Binary_operator_divide | Binary_operator_modulus -> (
        match (v1, v2) with AInt, AInt -> just_int | _ -> empty_v)
    | Binary_operator_less_than | Binary_operator_less_than_or_equal_to -> (
        match (v1, v2) with AInt, AInt -> both_bools | _ -> empty_v)
    | Binary_operator_equal_to | Binary_operator_not_equal_to -> (
        match (v1, v2) with AInt, AInt -> both_bools | _ -> empty_v)
    | Binary_operator_and -> (
        match (v1, v2) with
        | ABool b1, ABool b2 -> just_bool (b1 && b2)
        | _ -> empty_v)
    | Binary_operator_or -> (
        match (v1, v2) with
        | ABool b1, ABool b2 -> just_bool (b1 || b2)
        | _ -> empty_v)

  let is_v_pat v pat =
    let open Abs_exp in
    match (pat, v) with
    | Fun_pat, AVal.AClosure _ -> true
    | Int_pat, AVal.AInt -> true
    | Bool_pat, AVal.ABool _ -> true
    | Record_pat set, ARecord (rmap, _ctx) ->
        Set.for_all set ~f:(Set.mem (Map.key_set rmap))
    | Strict_record_pat set1, ARecord (rmap, _ctx) ->
        (* let set2 = map |> Map.keys |> Set.of_list (module Id) in *)
        Set.equal set1 (Map.key_set rmap)
    | Any_pat, _ -> true
    | _, _ -> false

  let not_ = function AVal.ABool b -> just_bool (not b) | _ -> empty_v

  module Quadruple_as_key = struct
    module T = struct
      type t = AStore.t * AEnv.t * Ctx.t * Abs_exp.t
      [@@deriving equal, hash, compare, sexp]
    end

    include T
    include Comparator.Make (T)
  end

  module Pair_as_prop = struct
    type property = result_set

    let bottom = Abs_result.empty
    let equal = Set.equal

    let is_maximal v =
      (* true *)
      false
    (* Set.length v >= 3 *)
  end

  module F = Fix.ForHashedType (Quadruple_as_key) (Pair_as_prop)

  let make_solution () =
    let visited = Hashtbl.create (module Quadruple_as_key) in
    let store_saved = ref None in
    let depth = ref 0 in
    let max_depth = ref 0 in

    let probe (store, aenv, ctx, e) =
      Hashtbl.update visited (store, aenv, ctx, e) ~f:(function
        | Some n ->
            Fmt.pr "store=%a @." AStore.pp store ;
            (* ignore @@ failwith "store" ; *)
            n + 1
        | None -> 1) ;

      (match !store_saved with
      | None -> store_saved := Some store
      | Some s2 ->
          if AStore.weight store > AStore.weight s2
          then store_saved := Some store) ;

      let counter = Hashtbl.length visited in

      (* Fmt.pr "ENV=%a" (pp_aenv_deep store ctx) aenv ; *)
      if counter mod 5000 = -1 (* && 5000 < counter && counter < 1006 *)
      then (
        let key_count = Hashtbl.length visited in
        let val_count =
          Hashtbl.fold visited ~init:0 ~f:(fun ~key ~data acc -> acc + data)
        in

        let store_count, aenv_count, ctx_count, e_count =
          let astore_set = Hash_set.create (module AStore) in
          let aenv_set = Hash_set.create (module AEnv) in
          let ctx_set = Hash_set.create (module Ctx) in
          let e_set = Hash_set.create (module Abs_exp) in
          Hashtbl.fold visited ~init:(0, 0, 0, 0)
            ~f:(fun ~key ~data (store_n, aenv_n, ctx_n, e_n) ->
              let key = (store, aenv, ctx, e) in
              let store_n =
                match Hash_set.strict_add astore_set store with
                | Ok _ -> store_n + 1
                | Error _ -> store_n
              in
              let aenv_n =
                match Hash_set.strict_add aenv_set aenv with
                | Ok _ -> aenv_n + 1
                | Error _ -> aenv_n
              in
              let ctx_n =
                match Hash_set.strict_add ctx_set ctx with
                | Ok _ -> ctx_n + 1
                | Error _ -> ctx_n
              in
              let e_n =
                match Hash_set.strict_add e_set e with
                | Ok _ -> e_n + 1
                | Error _ -> e_n
              in
              (store_n, aenv_n, ctx_n, e_n))
        in
        Fmt.pr "#k=%d #v=%d #store=%d #env=%d #ctx=%d #e=%d #d=%d e=%a@."
          key_count val_count store_count aenv_count ctx_count e_count !depth
          Abs_exp.pp e ;
        Fmt.pr "#k=%d $store=_d $env=%d ctx=%a e=%a@." key_count
          (AStore.weight store) (* (AStore.weight_env store ctx aenv) *)
          Ctx.pp ctx Abs_exp.pp e ;

        (* Fmt.pr "store=%a" AStore.pp (Option.value_exn !store_saved) ; *)
        ())
    in

    let rec mk_aeval (store, aenv, ctx, e0) aeval : result_set =
      match e0 with
      | Abs_exp.Just cl ->
          (* the critical part to run and cache the result;
             we can think it as the base case of the recursive call,
             while the recursive call doesn't do the computation work but just
             decompose into the basic case.
          *)
          mk_aeval_clause (store, aenv, ctx, cl) aeval
      | Abs_exp.More (cl, e) ->
          let res_hd = aeval (store, aenv, ctx, Abs_exp.Just cl) in
          bind res_hd (fun (cl_v, cl_store) ->
              match cl_v with
              | AVal.AAbort -> Abs_result.empty
              | _ ->
                  let aenv' =
                    AEnv.add_binding_exn ~key:(Abs_exp.id_of_clause cl)
                      ~data:cl_v aenv
                  in
                  aeval (cl_store, aenv', ctx, e))
    and mk_aeval_clause (store, (aenv : AEnv.t), ctx, Clause (x0, clb)) aeval :
        result_set =
      probe (store, aenv, ctx, Just (Clause (x0, clb))) ;

      (* Mismatch step 2: fetch x from the wrong env *)
      let env_get_by_id x = Map.find (HashCons.data aenv) x in
      let env_get x = env_get_by_id (Abs_exp.to_id x) in
      let env_get_map x f =
        Option.value_map (env_get x) ~default:Abs_result.empty ~f
      in
      (* let env_add_map env x v f =
           let ar = Map.add env ~key:x ~data:v in
           match ar with `Ok env' -> f env' | `Duplicate -> Abs_result.empty
         in *)
      let env_add_map env x v f =
        match AEnv.add_binding ~key:x ~data:v env with
        | `Ok env' -> f env'
        | `Duplicate -> Abs_result.empty
      in

      (* Fmt.pr "@\n%a with env @[<h>%a@]@\n with store %a@\n" Abs_exp.pp_clause
         (Clause (x0, clb))
         AEnv.pp aenv AStore.pp store ; *)
      (* Fmt.pr "%a@." Abs_exp.pp_clause (Clause (x0, clb)) ; *)
      (* Fmt.pr "@\n%a with env @[<h>%a@]@\n \n" Abs_exp.pp_clause
         (Clause (x0, clb))
         AEnv.pp aenv ; *)
      match clb with
      | Value Int -> Abs_result.only (AInt, store)
      | Value (Bool b) -> Abs_result.only (ABool b, store)
      | Value (Function (x, e)) ->
          let v = AVal.AClosure (Abs_exp.to_id x, e, ctx) in
          let store' = AStore.safe_add store ctx aenv in
          Abs_result.only (v, store')
      | Value (Record rmap) ->
          let v = AVal.ARecord (rmap, ctx) in
          let store' = AStore.safe_add store ctx aenv in
          Abs_result.only (v, store')
          (* List.fold_until (Map.to_alist rmap) ~init:[]
             ~f:(fun acc (lb, x) ->
               match env_get_by_id x with
               | Some v -> Continue ((lb, v) :: acc)
               | None -> Stop Abs_result.empty)
             ~finish:(fun acc ->
               let map' = Map.of_alist_exn (module Id) acc in
               let store' = safe_add_store store ctx map' in
               (* let keys = Map.keys rmap |> Set.of_list (module Id) in *)
               Abs_result.only (ARecord (Set.empty (module Id), ctx), store')) *)
      | CVar x -> env_get_map x (fun v -> Abs_result.only (v, store))
      | Appl (x1, x2) -> (
          match (env_get x1, env_get x2) with
          | Some (AClosure (xc, e, saved_context)), Some v2 ->
              (* Mismatch step 1: pick the wrong env *)
              let saved_envs = AStore.find_exn store saved_context in
              let ctx' = Ctx.push (x0, Abs_exp.to_id x1) ctx in
              bind saved_envs (fun saved_env ->
                  env_add_map saved_env xc v2 (fun env_new ->
                      aeval (store, env_new, ctx', e)))
          | _ -> Abs_result.empty)
      | Not x -> (
          match env_get x with
          | Some v -> lift_store (not_ v) store
          | None -> Abs_result.empty)
      | Binop (x1, bop, x2) -> (
          match (env_get x1, env_get x2) with
          | Some v1, Some v2 ->
              let vs = binop bop v1 v2 in
              lift_store vs store
          | _ -> Abs_result.empty)
      | Cond (x, e1, e2) -> (
          match env_get x with
          | Some (ABool true) ->
              (* aeval (store, aenv, ctx, e1) *)
              aeval (store, aenv, Ctx.push (x0, Id.cond_id true) ctx, e1)
          | Some (ABool false) ->
              (* aeval (store, aenv, ctx, e2) *)
              aeval (store, aenv, Ctx.push (x0, Id.cond_id false) ctx, e2)
          | _ -> Abs_result.empty)
      | Match (x, pat) -> (
          match env_get x with
          | Some v -> Abs_result.only (AVal.ABool (is_v_pat v pat), store)
          | _ -> Abs_result.empty)
      | Project (x, lb) -> (
          match env_get x with
          (* | Some (ARecord (_keys, r_ctx)) -> *)
          | Some (AVal.ARecord (rmap, r_ctx)) -> (
              match Map.find rmap lb with
              | Some v_lb ->
                  let saved_envs = AStore.find_exn store r_ctx in
                  bind saved_envs (fun saved_env ->
                      match Map.find (HashCons.data saved_env) v_lb with
                      | Some v -> Abs_result.only (v, store)
                      | None -> Abs_result.empty)
              | None -> Abs_result.empty)
          | _ -> Abs_result.empty)
      | Abort -> Abs_result.only (AAbort, store)
      | Diverge -> Abs_result.only (ADiverge, store)
    in

    (F.lfp mk_aeval, visited)

  let analysis_result ?(dump = false) e =
    let analyze e =
      let solution, visited = make_solution () in

      let ae = Abs_exp.lift_expr e in
      let result = solution (AStore.empty, AEnv.empty, Ctx.empty, ae) in
      (solution, visited, result)
    in
    let build_result_alist solution visited =
      let same_e_in_quadruple (_, _, _, e1) (_, _, _, e2) =
        Abs_exp.compare e1 e2
      in
      let pp_e_in_q fmt (_, _, _, e) = Abs_exp.pp fmt e in
      visited |> Hashtbl.keys
      |> List.sort_and_group ~compare:same_e_in_quadruple
      |> List.map ~f:(fun es ->
             let _, _, _, e = List.hd_exn es in
             let vs =
               List.fold es
                 ~init:(Set.empty (module AVal))
                 ~f:(fun acc e ->
                   Set.fold (solution e) ~init:acc ~f:(fun acc (v, _s) ->
                       Set.add acc v))
             in
             (Abs_exp.id_of_e_exn e, vs))
    in
    let binding_from_result visited e =
      let para_to_fun_def_map =
        Jayil.Ast_tools.make_para_to_fun_def_mapping e
      in
      let get_env (_, env, _, _) = env in
      let filter_entries env =
        Map.filteri
          ~f:(fun ~key ~data -> Jayil.Ast.Ident_map.mem key para_to_fun_def_map)
          (HashCons.data env)
      in
      visited |> Hashtbl.keys |> List.map ~f:get_env
      |> List.map ~f:filter_entries
    in

    let solution, visited, result_set = analyze e in
    let result_alist = build_result_alist solution visited in
    let result_map = Hashtbl.of_alist_exn (module Id) result_alist in
    let para_map_list = binding_from_result visited e in
    List.iter
      ~f:(fun env ->
        Map.iteri env ~f:(fun ~key ~data ->
            Hashtbl.update result_map key ~f:(function
              | Some vset -> Set.add vset data
              | None -> Set.singleton (module AVal) data)))
      para_map_list ;

    if dump
    then
      Fmt.pr "Analysis: %a"
        (Fmt.Dump.iter_bindings Std.iteri_core_hashtbl Fmt.nop Id.pp pp_aval_set)
        result_map ;
    (result_set, result_map)

  let update_block_clauses result_map para_to_fun_def_map block =
    let open Dj_common.Cfg in
    let open AVal in
    let clauses_mapper cls =
      List.map cls ~f:(fun cl ->
          let cat' =
            match (cl.cat, cl.clause) with
            | Direct, _ -> Direct
            | Fun, _ -> Fun
            | Cond, _ -> Cond
            | App [], Clause (_, Appl_body (Var f, _)) ->
                let dsts =
                  (* `f` can be unreachable *)
                  Hashtbl.find_and_call result_map f
                    ~if_found:(fun vs ->
                      vs |> Set.to_list
                      |> List.filter_map ~f:(function
                           | AInt -> None
                           | ABool _ -> None
                           | ARecord _ -> None
                           | AClosure (x, _, _) ->
                               Some
                                 (Jayil.Ast.Ident_map.find x para_to_fun_def_map)
                           (* TODO: Check abort logic *)
                           | AAbort -> None
                           | ADiverge -> None))
                    ~if_not_found:(fun _ -> [])
                in
                App dsts
            | c, cl ->
                Fmt.pr "c=%a\ncl=%a\n" Dj_common.Cfg.pp_clause_cat c
                  Jayil.Pp.clause cl ;
                failwith "impossible pair"
          in
          { cl with cat = cat' })
    in
    match block.kind with
    | Main -> { block with clauses = clauses_mapper block.clauses }
    | Fun fb -> { block with clauses = clauses_mapper block.clauses }
    | Cond cb -> { block with clauses = clauses_mapper block.clauses }

  (* update fun and cond block

     iterate all key, result_value pairs.
     for each key, find its clauses and case the clause
      1. application clause `x = f _`. Iterate the possible closures results for f. If we are in the f's closure (function) body, we may come from `x`. The fid of _closure (function) body_ is retrieved from `para_to_fun_def_map`, the block associated with fid is retrieved from `block_map`, then we update the block by adding a new possible callsite `x`.
      2. condition clause `x = c ? e1 : e2`. The design is `x` points to a `cond_both` block, and `e1` and `e2` points to a `cond_case` block. A `cond_both` blocks contains two optional `cond_case` blocks. A `cond_case` block has a `possible` field. `cfg_of_course` creats these three blocks, with optional values not-none and `possible` to be `true`. Then the analysis updates the `possible` to be false and the optional block to be `none` if unreachable.
  *)
  let update_fun_and_cond_block block_map result_map id_to_clause_map
      para_to_fun_def_map ~key:x ~data:vs =
    let open Dj_common.Cfg in
    let open AVal in
    let open Jayil.Ast in
    let cl = Ident_map.find_opt x id_to_clause_map in
    match cl with
    | Some (Clause (Var xc, Appl_body (Var f, Var _))) ->
        Hashtbl.find_exn result_map f
        |> Set.to_list
        |> List.iter ~f:(function
             | AInt -> ()
             | ABool _ -> ()
             | ARecord _ -> ()
             | AClosure (x, _, _) ->
                 let fid = Jayil.Ast.Ident_map.find x para_to_fun_def_map in
                 let block_map' =
                   Jayil.Ast.Ident_map.update_stdlib fid
                     (function
                       | Some block ->
                           let fblock = cast_to_fun_block_info block in
                           let fblock' =
                             { fblock with callsites = xc :: fblock.callsites }
                           in
                           Some { block with kind = Fun fblock' }
                       | None -> failwith "must have this fblock")
                     !block_map
                 in
                 block_map := block_map'
             | AAbort -> () (* TODO: Again, check abort logic. *)
             | ADiverge -> ())
    | Some (Clause (Var xc, Conditional_body (Var c, _, _))) ->
        let vs = Hashtbl.find_exn result_map c in
        let cond_both = find_cond_blocks xc !block_map in
        let may_be_true =
          Set.exists ~f:(function ABool true -> true | _ -> false) vs
        in
        let may_be_false =
          Set.exists ~f:(function ABool false -> true | _ -> false) vs
        in
        if not may_be_true
        then set_block_unreachable block_map (Option.value_exn cond_both.then_) ;
        if not may_be_false
        then set_block_unreachable block_map (Option.value_exn cond_both.else_) ;
        ()
    | _ -> ()

  let main e =
    let block_map = ref (Dj_common.Cfg_of_source.block_map_of_expr e) in
    let _, result_map = analysis_result ~dump:false e in
    let para_to_fun_def_map = Jayil.Ast_tools.make_para_to_fun_def_mapping e in
    let id_to_clause_map = Jayil.Ast_tools.clause_mapping e in

    (* We need to mark all the result_map information in block_map.
       Given current implementation (or even for general goodness), we cannot mutate one map during its iteration. (I may have done this) More specifically, I will iterate along the keys but mutate its values. It may work, however, not in the current _functional_ data structure.
    *)
    (* `block_map` is a bad-designed block-index functional map. The workflow to update is:
       1. find the matching block
       2. functionally update the block to get the block'
       3. save the block' to get a new map'
       4. store the map' to the reference of the old map

       For this scenario, one block can have multiple matching clauses e.g.
       when we have {f -> {v}}, we can heve clauses `r1 = f a1; r2 = f a2`
    *)

    (* update all clauses in each blocks *)
    block_map :=
      Jayil.Ast.Ident_map.map
        (update_block_clauses result_map para_to_fun_def_map)
        !block_map ;

    Hashtbl.iteri result_map
      ~f:
        (update_fun_and_cond_block block_map result_map id_to_clause_map
           para_to_fun_def_map) ;

    !block_map
end

let block_map_of_expr k e =
  let module Ctx : Finite_callstack.C = Finite_callstack.Make (struct
    let k = k
  end) in
  let module Analyzer = Make (Ctx) in
  Analyzer.main e
