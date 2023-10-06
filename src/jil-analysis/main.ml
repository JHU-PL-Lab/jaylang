open Core
open Dj_common
(* open Abs_exp *)

module Make (Ctx : Finite_callstack.C) = struct
  module All_vals = Abs_val.Make (Ctx)
  open All_vals

  let bind (type a) set (f : a -> result_set) : result_set =
    Set.fold set ~init:Abs_result.empty ~f:(fun acc elem ->
        Set.union acc @@ f elem)

  let binop bop v1 v2 =
    let open AVal in
    let open Abs_exp in
    match bop with
    | Binary_operator_plus | Binary_operator_minus | Binary_operator_times
    | Binary_operator_divide | Binary_operator_modulus
    | Binary_operator_less_than | Binary_operator_less_than_or_equal_to -> (
        match (v1, v2) with
        | AInt, AInt -> Set.of_list (module AVal) [ ABool true; ABool false ]
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

  let is_v_pat v pat =
    let open Abs_exp in
    match (pat, v) with
    | Fun_pat, AVal.AClosure _ -> true
    | Int_pat, AVal.AInt -> true
    | Bool_pat, AVal.ABool _ -> true
    | Record_pat set, ARecord (keys, _ctx) -> Set.for_all set ~f:(Set.mem keys)
    | Strict_record_pat set1, ARecord (keys, _ctx) ->
        (* let set2 = map |> Map.keys |> Set.of_list (module Id) in *)
        Set.equal set1 keys
    | Any_pat, _ -> true
    | _, _ -> false

  let not_ = function
    | AVal.ABool b -> Set.singleton (module AVal) (ABool (not b))
    | _ -> Set.empty (module AVal)

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

  module F = Fix.Fix.ForHashedType (Quadruple_as_key) (Pair_as_prop)

  let make_solution () =
    let visited = Hashtbl.create (module Quadruple_as_key) in
    let counter = ref 0 in

    let rec mk_aeval (store0, aenv0, ctx, e0) aeval : result_set =
      match e0 with
      | Abs_exp.Just cl ->
          (* the critical part to run and cache the result;
             we can think it as the base case of the recursive call,
             while the recursive call doesn't do the computation work but just
             decompose into the basic case.
          *)
          Hashtbl.update visited (store0, aenv0, ctx, e0) ~f:(function
            | Some n -> n + 1
            | None -> 0) ;
          (* Int.incr counter ;

             if !counter mod 500 = 0
             then
               Fmt.pr "%a"
                 (Fmt.iter_bindings Std.iteri_core_hashtbl
                    (Fmt.Dump.pair
                       (Fmt.using
                          (fun (_, _, ctx, e) ->
                            let x = e |> Abs_exp.id_of_e_exn in
                            (x, ctx))
                          (Fmt.pair Id.pp Ctx.pp))
                       Fmt.int))
                 visited ; *)
          mk_aeval_clause (store0, aenv0, ctx, cl) aeval
          (* let vs = mk_aeval_clause (store0, aenv0, ctx, cl) aeval in
             let x0 = Abs_exp.clause_of_e_exn e0 in
             vs *)
      | Abs_exp.More (cl, e) ->
          let res_hd = aeval (store0, aenv0, ctx, Abs_exp.Just cl) in
          bind res_hd (fun (cl_v, cl_store) ->
              let aenv' =
                Map.add_exn aenv0 ~key:(Abs_exp.id_of_clause cl) ~data:cl_v
                (* match Map.add aenv0 ~key:(Abs_exp.id_of_clause cl) ~data:cl_v with
                   | `Ok env -> env
                   | `Duplicate -> aenv0 *)
              in
              mk_aeval (cl_store, aenv', ctx, e) aeval)
    and mk_aeval_clause (store, aenv, ctx, Clause (x0, clb)) aeval : result_set
        =
      (* Mismatch step 2: fetch x from the wrong env *)
      let env_get_exn x = Map.find_exn aenv x in
      let env_get_by_id x = Map.find aenv x in
      let env_get x = env_get_by_id (Abs_exp.to_id x) in
      let env_get_bind x f =
        Option.value_map (env_get x) ~default:Abs_result.empty ~f
      in
      let env_add_bind env x v f =
        let ar = Map.add env ~key:x ~data:v in
        match ar with `Ok env' -> f env' | `Duplicate -> Abs_result.empty
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
          let store' = safe_add_store store ctx aenv in
          Abs_result.only (v, store')
      | Value (Record rmap) ->
          List.fold_until (Map.to_alist rmap) ~init:[]
            ~f:(fun acc (k, lb) ->
              match env_get_by_id lb with
              | Some v -> Continue ((k, v) :: acc)
              | None -> Stop Abs_result.empty)
            ~finish:(fun acc ->
              let map' = Map.of_alist_exn (module Id) acc in
              let store' = safe_add_store store ctx map' in
              let keys = Map.keys rmap |> Set.of_list (module Id) in
              Abs_result.only (ARecord (keys, ctx), store'))
          (* Abs_result.only (ARecord ctx, store))  *)
      | CVar x -> env_get_bind x (fun v -> Abs_result.only (v, store))
      | Appl (x1, x2) -> (
          match (env_get x1, env_get x2) with
          | Some (AClosure (xc, e, saved_context)), Some v2 ->
              (* Mismatch step 1: pick the wrong env *)
              let saved_envs = Map.find_exn store saved_context in
              let ctx' = Ctx.push (x0, Abs_exp.to_id x1) ctx in
              bind saved_envs (fun saved_env ->
                  env_add_bind saved_env xc v2 (fun env_new ->
                      aeval (store, env_new, ctx', e)))
          | _ -> Abs_result.empty)
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
      | Match (x, pat) -> (
          match env_get x with
          | Some v -> Abs_result.only (AVal.ABool (is_v_pat v pat), store)
          | _ -> Abs_result.empty)
      | Project (x, lb) -> (
          match env_get x with
          | Some (ARecord (keys, r_ctx)) ->
              let saved_maps = Map.find_exn store r_ctx in
              bind saved_maps (fun map ->
                  match Map.find map lb with
                  | Some v -> Abs_result.only (v, store)
                  | None -> Abs_result.empty)
          | _ -> Abs_result.empty)
      | Abort -> Abs_result.empty
      | Assume _x -> Abs_result.only (AVal.ABool true, store)
      | Assert _x -> Abs_result.only (AVal.ABool true, store)
    in
    (F.lfp mk_aeval, visited)

  let analysis_result ?(dump = false) e =
    let analyze e =
      let solution, visited = make_solution () in

      let ae = Abs_exp.lift_expr e in
      let result =
        solution (Map.empty (module Ctx), Map.empty (module Id), Ctx.empty, ae)
      in
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
          env
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
      Fmt.pr "%a"
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
            | Direct, Clause (_, Value_body (Value_function _)) -> Direct
            | Fun, _ -> Fun
            | Cond, _ -> Cond
            | App [], Clause (_, Appl_body (Var (f, _), _)) ->
                let dsts =
                  (* `f` can be unreachable *)
                  Hashtbl.find_and_call result_map f
                    ~if_found:(fun vs ->
                      vs |> Set.to_list
                      |> List.filter_map ~f:(function
                           | AInt -> None
                           | ABool _ -> None
                           | Any -> None
                           | ARecord _ -> None
                           | AClosure (x, _, _) ->
                               Some
                                 (Jayil.Ast.Ident_map.find x para_to_fun_def_map)))
                    ~if_not_found:(fun _ -> [])
                in
                App dsts
            | Direct, _ -> Direct
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
    | Some (Clause (Var (xc, _), Appl_body (Var (f, _), Var _))) ->
        Hashtbl.find_exn result_map f
        |> Set.to_list
        |> List.iter ~f:(function
             | AInt -> ()
             | ABool _ -> ()
             | Any -> ()
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
                 block_map := block_map')
    | Some (Clause (Var (xc, _), Conditional_body (Var (c, _), _, _))) ->
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
    let _, result_map = analysis_result ~dump:true e in
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
