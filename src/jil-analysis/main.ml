open Core
open Fix
open Dj_common
open Abs_value

module Quadruple_as_key = struct
  module T = struct
    type t = AStore.t * AEnv.t * Ctx.t * Abs_exp.t
    [@@deriving equal, hash, compare, sexp]
  end

  include T
  include Comparable.Make (T)
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
  match (pat, v) with
  | Fun_pat, AVal.AClosure _ -> true
  | Int_pat, AVal.AInt -> true
  | Bool_pat, AVal.ABool _ -> true
  | Record_pat set, ARecord map -> Set.for_all set ~f:(Map.mem map)
  | Strict_record_pat set1, ARecord map ->
      let set2 = map |> Map.keys |> Set.of_list (module Id) in
      Set.equal set1 set2
  | Any_pat, _ -> true
  | _, _ -> false

let not_ = function
  | AVal.ABool b -> Set.singleton (module AVal) (ABool (not b))
  | _ -> Set.empty (module AVal)

let make_solution () =
  let visited = Hash_set.create (module Quadruple_as_key) in

  let rec mk_aeval (store0, aenv0, ctx, e0) aeval : result_set =
    match e0 with
    | Just cl ->
        (* the critical part to run and cache the result;
           we can think it as the base case of the recursive call,
           while the recursive call doesn't do the computation work but just
           decompose into the basic case.
        *)
        Hash_set.add visited (store0, aenv0, ctx, e0) ;
        mk_aeval_clause (store0, aenv0, ctx, cl) aeval
        (* let vs = mk_aeval_clause (store0, aenv0, ctx, cl) aeval in
           let x0 = Abs_exp.clause_of_e_exn e0 in
           vs *)
    | More (cl, e) ->
        let res_hd = aeval (store0, aenv0, ctx, Just cl) in
        bind res_hd (fun (cl_v, cl_store) ->
            let aenv' =
              Map.add_exn aenv0 ~key:(Abs_exp.id_of_clause cl) ~data:cl_v
              (* match Map.add aenv0 ~key:(Abs_exp.id_of_clause cl) ~data:cl_v with
                 | `Ok env -> env
                 | `Duplicate -> aenv0 *)
            in
            mk_aeval (cl_store, aenv', ctx, e) aeval)
  and mk_aeval_clause (store, aenv, ctx, Clause (x0, clb)) aeval : result_set =
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
    match clb with
    (* | Nobody -> Abs_result.only (env_get_exn x0, store) *)
    | Value Int -> Abs_result.only (AInt, store)
    | Value (Bool b) -> Abs_result.only (ABool b, store)
    | Value (Function (x, e)) ->
        let v = AVal.AClosure (Abs_exp.to_id x, e, ctx) in
        let store' = safe_add_store store ctx aenv in
        Abs_result.only (v, store')
    | Value (Record map) ->
        List.fold_until (Map.keys map) ~init:[]
          ~f:(fun acc x ->
            match env_get_by_id x with
            | Some v -> Continue ((x, v) :: acc)
            | None -> Stop Abs_result.empty)
          ~finish:(fun acc ->
            let map' = Map.of_alist_exn (module Id) acc in
            Abs_result.only (ARecord map', store))
    | CVar x -> env_get_bind x (fun v -> Abs_result.only (v, store))
    | Appl (x1, x2) -> (
        match (env_get x1, env_get x2) with
        | Some (AClosure (xc, e, saved_context)), Some v2 ->
            (* Mismatch step 1: pick the wrong env *)
            let saved_envs = Map.find_exn store saved_context in
            let ctx' = Ctx.push (x0, Abs_exp.to_id x1) ctx in
            bind saved_envs (fun saved_env ->
                env_add_bind saved_env xc v2 (fun env_new ->
                    (* let e' = Abs_exp.(More (Clause (xc, Nobody), e)) in *)
                    let e' = e in
                    aeval (store, env_new, ctx', e'))
                (* let env_new = Map.add_exn saved_env ~key:xc ~data:v2 in
                   aeval (store, env_new, ctx', e) *))
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
        | Some (ARecord map) -> (
            match Map.find map lb with
            | Some v -> Abs_result.only (v, store)
            | None -> Abs_result.empty)
        | _ -> Abs_result.empty)
    | Abort -> Abs_result.empty
    | Assume _x -> Abs_result.only (AVal.ABool true, store)
    | Assert _x -> Abs_result.only (AVal.ABool true, store)
  in
  (F.lfp mk_aeval, visited)

type result_table = (Id.t, AVal.t list) Hashtbl.t

let analyze e =
  let solution, visited = make_solution () in

  let ae = Abs_exp.lift_expr e in
  let result =
    solution (Map.empty (module Ctx), Map.empty (module Id), Ctx.empty, ae)
  in
  (solution, visited, result)

let build_result_alist solution visited =
  let same_e_in_quadruple (_, _, _, e1) (_, _, _, e2) = Abs_exp.compare e1 e2 in
  let pp_e_in_q fmt (_, _, _, e) = Abs_exp.pp fmt e in
  visited |> Hash_set.to_list
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

let binding_from_result visited e =
  let para_to_fun_def_map = Jayil.Ast_tools.make_para_to_fun_def_mapping e in
  let get_env (_, env, _, _) = env in
  let filter_entries env =
    Map.filteri
      ~f:(fun ~key ~data -> Jayil.Ast.Ident_map.mem key para_to_fun_def_map)
      env
  in
  visited |> Hash_set.to_list |> List.map ~f:get_env
  |> List.map ~f:filter_entries

let analysis_result ?(dump = false) e =
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
      (Fmt.Dump.iter_bindings Std.iteri_core_hashtbl Fmt.nop Id.pp
         Abs_value.pp_aval_set)
      result_map ;
  (result_set, result_map)
