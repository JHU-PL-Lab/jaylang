open Batteries
(* open Jhupllib *)
open Odefa_ast
open Ast
(* open Ast_pp *)
open Odefa_ddpa
open Ddpa_abstract_ast
open Ddpa_graph
(* open Ddpa_utils *)
(* open Interpreter_types;; *)
(* open Logger_utils *)

open Ast_helper
open Ddpa_helper

let log_acl acl prevs = 
  print_endline @@ Printf.sprintf "%s \t\t[prev: %s]"
    (Jhupllib.Pp_utils.pp_to_string
       pp_brief_annotated_clause acl)
    (Jhupllib.Pp_utils.pp_to_string
       (Jhupllib.Pp_utils.pp_list pp_brief_annotated_clause) prevs)

let log_id id = 
  print_endline @@ Printf.sprintf "%s"
    (Jhupllib.Pp_utils.pp_to_string pp_ident id)

module Tracelet = struct

  (* TODO: we might get rid of map at all, by using tree (zipper) *)
  let name_main = "0_main"

  let id_main = Ident name_main

  (* duplicate for simplicity *)
  type clause_cat = 
    | Direct
    | Fun 
    | App of ident list
    | Cond of ident list

  type tl_clause = {
    id : ident;
    cat : clause_cat;
    clause : clause;
  }

  type id_with_dst = ident * ident list

  type block = {
    clauses : tl_clause list;
    app_ids : id_with_dst list;
    cond_ids : id_with_dst list;
    (* first_v : id of the first clause *)
    (* ret: id of the last clause *)
  }

  let empty_block = {
    clauses = []; app_ids = []; cond_ids = []
  }

  type fun_block = {
    para: ident;
    block: block;
    callsites: ident list;
  }

  type cond_source_block = { 
    cond : ident;
    then_block : block;
    else_block : block;
  }

  type cond_running_block = {
    cond : ident;
    choice : bool;
    block : block;
    other_block : block;
  }

  (* when running, we cannot be in a CondBoth.
     Using a function call analogy, it's a function always with two fbodies.
     When called, pick one.

     The question gets down to a caller decision, or a callee decision.
      We can only get the block after the decision.

      Let's pick the caller decision
  *)
  type block_node = 
    | Main of block
    | Fun of fun_block
    | CondBoth of cond_source_block
    | CondChosen of cond_running_block

  type source_tracelet = {
    point : ident;
    outer_point : ident;
    source_block : block_node;
  }

  type t = source_tracelet

  (* 
  let return_id_of_tl tl = 
    return_id_of_block (get_block tl)

  let return_id_of_block =
    List.last *)

  let running_cond choice (cb : cond_source_block) : cond_running_block =
    { cond = cb.cond;
      choice;
      block = if choice then cb.then_block else cb.else_block;
      other_block = if choice then cb.else_block else cb.then_block;
    }

  let para_of tl =
    match tl.source_block with
    | Fun b -> b.para
    | _ -> failwith "not fun block in tracelet"

  let get_block ?choice:(choice=true) tl =
    match tl.source_block with
    | Main b -> b
    | Fun b -> b.block
    | CondBoth c -> failwith "CondBoth"
    | CondChosen c -> c.block

  let debug_get_block tl =
    match tl.source_block with
    | Main b -> b
    | Fun b -> b.block
    | CondBoth c -> 
      {
        clauses = c.then_block.clauses @ c.else_block.clauses;
        app_ids = c.then_block.app_ids @ c.else_block.app_ids;
        cond_ids = c.then_block.cond_ids @ c.else_block.cond_ids
      }
    | CondChosen c -> c.block

  let update_block f tl =
    let source_block = 
      match tl.source_block with
      | Main b -> Main (f b)
      | Fun b -> Fun 
                   {b with
                    block = f b.block
                   }
      | CondBoth c -> CondBoth 
                        {c with 
                         then_block = f c.then_block;
                         else_block = f c.else_block
                        }
      | CondChosen c -> CondChosen 
                          {c with
                           block = f c.block;
                           other_block = f c.other_block
                          }
    in
    {tl with source_block}

  let cids_of tl =
    tl
    |> debug_get_block
    |> fun block -> List.map (fun tc -> let Ident n = tc.id in n) block.clauses    

  let direct_cids_of tl =
    tl
    |> debug_get_block
    |> fun block -> List.filter_map (fun tc ->
        match tc.cat with
        | Direct -> let Ident n = tc.id in Some n
        | _ -> None) block.clauses

  let app_ids_of tl =
    tl
    |> debug_get_block
    |> fun block -> List.map (fun (Ident id, _) -> id) block.app_ids   

  let cond_ids_of tl =
    tl
    |> debug_get_block
    |> fun block -> List.map (fun (Ident id, _) -> id) block.cond_ids   

  let debug_def_ids_of tl =
    tl
    |> debug_get_block
    |> fun block -> List.map (fun (Ident id, dsts) -> 
        let dst_names = List.map (fun (Ident name) -> name) dsts in
        id, dst_names)
      (block.app_ids @ block.cond_ids)

  let split_clauses (Expr clauses) : tl_clause list * id_with_dst list * id_with_dst list =
    List.fold_left (fun (cs, app_ids, cond_ids) (Clause(Var (cid, _), b) as c) ->
        match b with
        | Appl_body (_, _)
          -> (cs @ [{ id = cid; cat = App []; clause = c }], app_ids @ [cid, []], cond_ids)
        | Conditional_body (Var (x, _), _, _)
          -> (cs @ [{ id = cid; cat = Cond []; clause = c }], app_ids, cond_ids @ [cid, []])
        | Value_body (Value_function _)
          -> (cs @ [{ id = cid; cat = Fun; clause = c }], app_ids, cond_ids)
        | _
          -> (cs @ [{ id = cid; cat = Direct; clause = c }], app_ids, cond_ids)

      ) ([], [], []) clauses

  let block_of_expr e =
    let clauses, app_ids, cond_ids = split_clauses e in
    { clauses; app_ids; cond_ids }

  let tracelet_map_of_expr e : t Ident_map.t =
    let map = ref Ident_map.empty in

    let main_tracelet = 
      let block = block_of_expr e in
      let source_block = Main block in
      { point = id_main; outer_point = id_main; source_block } in
    map := Ident_map.add id_main main_tracelet !map
    ;

    let rec loop outer_point e =
      let Expr clauses = e in
      let handle_clause = function
        | Clause (Var (cid, _), Value_body (Value_function (Function_value (Var (para, _), fbody)))) ->
          let block = block_of_expr fbody in
          let source_block = Fun { para; block; callsites = [] } in
          let tracelet = 
            { point = cid ; outer_point; source_block } in
          map := Ident_map.add cid tracelet !map;
          loop cid fbody
        | Clause (Var (cid, _), Conditional_body (Var(cond, _), e1, e2)) ->
          let then_block = block_of_expr e1
          and else_block = block_of_expr e2 in
          let source_block = 
            CondBoth {cond; then_block; else_block} in
          let tracelet = 
            { point = cid ; outer_point; source_block } in
          map := Ident_map.add cid tracelet !map;
          loop cid e1;
          loop cid e2     
        | _ -> ()
      in
      List.iter handle_clause clauses
    in

    loop id_main e;
    !map

  let cut_before with_end x tl =
    let cut_block block : block = 
      let clauses, app_ids, cond_ids, _ =
        List.fold_while 
          (fun (_, _, _, find_x) tc -> 
             if with_end
             then find_x
             else tc.id <> x)
          (fun (cs, app_ids, cond_ids, find_x) tc ->
             (* TODO: bad design *)
             let cid = tc.id in
             let find_x = tc.id <> x in
             match tc.cat with
             | App _ ->
               let dst = List.assoc cid block.app_ids in
               (cs @ [tc], app_ids @ [cid, dst], cond_ids, find_x)
             | Cond _ ->
               let dst = List.assoc cid block.cond_ids in
               (cs @ [tc], app_ids, cond_ids @ [cid, dst], find_x)
             | Fun ->
               (cs @ [tc], app_ids, cond_ids, find_x)
             | Direct ->
               (cs @ [tc], app_ids, cond_ids, find_x)
          )
          ([], [], [], true)
          block.clauses
        |> fst
      in
      { clauses; app_ids; cond_ids }
    in
    let source_block = 
      match tl.source_block with
      | Main b -> Main (cut_block b)
      | Fun b -> Fun { b with block = cut_block b.block }
      | CondBoth cond ->
        (* TODO : 
           Noting: cutting can only occur in one block of a cond, therefore
           it's a hidden change from CondBoth to CondChosen
        *)
        let choice = List.exists (fun tc -> tc.id = x) cond.then_block.clauses in
        let cond' = running_cond choice cond in
        CondChosen { cond' with block = cut_block cond'.block }
      | CondChosen cond -> 
        CondChosen { cond with block = cut_block cond.block }
    in
    { tl with source_block }

  let find_by_id x tl_map =
    tl_map
    |> Ident_map.values
    |> Enum.find (fun tl -> List.exists (fun tc -> tc.id = x) @@ (debug_get_block tl).clauses )
    |> (fun tl -> 
        match tl.source_block with
        | CondBoth cb ->
          let choice = List.exists (fun tc -> tc.id = x) cb.then_block.clauses in
          let cond = running_cond choice cb in
          { tl with source_block = CondChosen cond }
        | _ -> tl
      )

  let cut_before_id x tl_map =
    find_by_id x tl_map
    |> cut_before false x

  let take_until x tl_map =
    find_by_id x tl_map
    |> cut_before true x  

  let update_id_dst id dst0 tl =
    let add_dst = function
      | Some dst -> 
        Some (if List.mem dst0 dst then dst else dst0::dst)
      | None -> None
    in
    let add_dsts dst0 dsts =
      if List.mem dst0 dsts 
      then dsts
      else dst0 :: dsts
    in
    let add_dst_in_clause tc = 
      if tc.id = id 
      then { tc with cat = match tc.cat with
          | App dsts -> App (add_dsts dst0 dsts)
          | Cond dsts -> Cond (add_dsts dst0 dsts)
          | other -> other
        }
      else tc
    in
    update_block 
      (fun block -> 
         let clauses = List.map add_dst_in_clause block.clauses
         and app_ids = List.modify_opt id add_dst block.app_ids
         and cond_ids = List.modify_opt id add_dst block.cond_ids
         in
         { clauses; app_ids; cond_ids }
      )
      tl

  let add_id_dst site_x def_x tl_map =
    (* log_id site_x; *)
    (* log_id def_x; *)
    let tl = find_by_id site_x tl_map in
    (* log_id tl.point; *)
    let tl' = update_id_dst site_x def_x tl in
    Ident_map.add tl.point tl' tl_map

  let cond_set b tl =
    let source_block = 
      match tl.source_block with
      | CondBoth c -> CondChosen (running_cond b c)
      | _ -> failwith "it should called just once on CondBoth"
    in
    { tl with source_block }

  let choose_cond_block acls cfg tl_map =
    let cond_site, choice =
      match acls with
      | Unannotated_clause(Abs_clause(Abs_var cond_site, Abs_conditional_body _)) 
        :: wire_cl
        :: [] -> 
        cond_site, find_cond_top wire_cl cfg
      | _ -> failwith "wrong precondition to call"
    in
    let tracelet = Ident_map.find cond_site tl_map in
    let tracelet' = cond_set choice tracelet in
    Ident_map.add cond_site tracelet' tl_map

  let _add_callsite site tl =
    let source_block = 
      match tl.source_block with
      | Fun b -> Fun { b with callsites = (site :: b.callsites) }
      | _ -> failwith "wrong precondition to call add_callsite"
    in
    { tl with source_block }

  let add_callsite f_def site tl_map =
    let tl = Ident_map.find f_def tl_map in
    let tl' = _add_callsite site tl in
    Ident_map.add f_def tl' tl_map
end

module Oracle = struct
  type 'a oracle =
    | Oracle of ('a list -> 'a * 'a oracle)

  let make_list_oracle choices = 
    let rec pick choices ids =
      match choices with
      | [] -> 
        List.hd ids, Oracle (pick [])
      | n :: ns -> 
        if List.length ids > 1
        then
          List.nth ids n, Oracle (pick ns)
        else
          List.hd ids, Oracle (pick choices)
    in
    Oracle (pick choices)

  let pick_bool = function
    | Oracle orl ->
      let ic, oracle' = orl [Ident "true"; Ident "false"] in
      (ic = Ident "true"), oracle'
end

module Tracelet_constraint = struct
  open Tracelet
  open Interpreter_types
  open Mega_constraint
  open Oracle

  let ast_value_of_fun_tl tl stk =
    Constraint.Function (Function_value(Var(tl.point, None), Expr []))

  let constraints_of_block_type tl stk = 
    match tl.source_block with
    | Main b -> [Constraint.Constraint_stack(Relative_stack.stackize Relative_stack.empty)]
    (* | Fun b -> [Constraint.Constraint_value(Symbol(tl.point, stk), ast_value_of_fun_tl tl stk)] *)
    | Fun _ -> []
    | CondBoth c -> [constraint_of_bool (Symbol(c.cond, stk)) true]
    | CondChosen c -> [constraint_of_bool (Symbol(c.cond, stk)) c.choice]

  let constraints_of_direct_clause stk tc = 
    let x_sym = Symbol(tc.id, stk) in  
    match tc.cat, tc.clause with
    | Direct, Clause(_, Value_body(Value_int n)) -> 
      Some (Constraint.Constraint_value(x_sym, Constraint.Int n))
    | Direct, Clause(_, Value_body(Value_bool b)) -> 
      Some (Constraint.Constraint_value(x_sym, Constraint.Bool b))
    | Direct, Clause(_, Var_body(Var (y, _))) -> 
      let y_sym = Symbol(y, stk) in
      Some (constraint_of_alias x_sym y_sym)
    | _ -> None

  let constraint_of_funenter arg stk para stk' =
    let sym_arg =  Symbol(arg, stk) in
    let sym_para = Symbol(para, stk') in
    constraint_of_alias sym_arg sym_para

  let constraint_of_funexit ret_site stk ret_bodu stk' =
    let sym_site =  Symbol(ret_site, stk) in
    let sym_body = Symbol(ret_bodu, stk') in
    constraint_of_alias sym_site sym_body

  let rec constraints_of_site oracle tl_map no_fun_return stk tc =
    match tc.cat, oracle with
    | App dsts, Oracle orl -> (
        let dst, oracle' = orl dsts in
        (* let dst = List.hd dsts in  *)
        (* log_id dst; *)
        let tl_dst = Ident_map.find dst tl_map
        and stk' = Relstack.push stk tc.id
        in
        let c_call = constraint_of_funenter (app_id2_of_clause tc.clause) stk (para_of tl_dst) stk'
        and oracle'', c_body = constraints_of_tracelet oracle' tl_map false tl_dst stk'
        and c_return = 
          let ret_id = (List.last (debug_get_block tl_dst).clauses).id in
          constraint_of_funexit tc.id stk ret_id stk' 
        in
        oracle'', if no_fun_return then c_call :: c_body else c_call :: c_return :: c_body )
    | Cond _, Oracle orl -> (
        let tl = Ident_map.find tc.id tl_map in
        match tl.source_block with
        | CondBoth cond ->
          let choice, oracle' = Oracle.pick_bool oracle in
          let source_block = CondChosen (running_cond choice cond) in
          let tl' = { tl with source_block } in
          constraints_of_tracelet oracle' tl_map false tl' stk
        | CondChosen cond ->
          constraints_of_tracelet oracle tl_map false tl stk
        | _ ->
          failwith "non cond block in condsite"
      )
    | _ -> oracle, []

  and constraints_of_tracelet oracle tl_map dangling tl stk = 
    let clauses = match tl.source_block with
      | Main b -> b.clauses
      | Fun b -> b.block.clauses
      | CondBoth c -> c.then_block.clauses
      | CondChosen c -> c.block.clauses
    in
    let cds = List.filter_map (constraints_of_direct_clause stk) clauses in
    let c0 = constraints_of_block_type tl stk in
    let oracle', ccs = List.fold_lefti 
        (fun (oracle, acc) i clause ->
           (* a tracelet has no returning constraint if
              the tracelet is in the main trace and the clause is the last one
           *)
           let no_fun_return = dangling && i == List.length clauses - 1 in
           let oracle', cs = constraints_of_site oracle tl_map no_fun_return stk clause in
           oracle', acc @ cs) 
        (oracle, []) clauses in
    oracle', c0 @ cds @ ccs
end

module Tunnel = struct
  open Tracelet
  open Oracle
  open Tracelet_constraint
  open Mega_constraint
  open Interpreter_types


  type frame = 
    | Frame of ident * ident

  type t = frame list

  exception Invalid_query of string

  let cfg_of e =
    let open Odefa_ddpa in
    let conf : (module Ddpa_context_stack.Context_stack) = 
      (module Ddpa_single_element_stack.Stack) in
    let module Stack = (val conf) in
    let module Analysis = Ddpa_analysis.Make(Stack) in
    e
    |> Analysis.create_initial_analysis
    |> Analysis.perform_full_closure
    |> Analysis.cfg_of_analysis

  (* dynamic stack or block map *)
  (* type tunnel =
     | Full of ident
     | Partial of ident * ident

     type tunnel_map = (ident, ident) BatMultiPMap.t

     type callsite_tunnel = ident Ident_map.t *)

  (* we cannot use tracelet map to represent the dynamic call graph/stack.
     the point is for one block, we can have a full version and a partial version
     at the same time.
     For this, we may set the original or annotated source code as a (static) map
     and use another data structure for dynamic
  *)

  (* annotate tracelet from the ddpa cfg.
     for call-site `s = e1 e2`, annotate e1 with the real function def_var
     for cond-site `s = c ? e1 : e2`, replace s with 
  *)
  let annotate e pt : Tracelet.t Ident_map.t =
    let map = ref (Tracelet.tracelet_map_of_expr e)
    (* and visited_pred_map = ref BatMultiPMap.empty *)
    and cfg = cfg_of e
    (* and id_first = first_var e *)
    and ret_to_fun_def_map =
      make_ret_to_fun_def_mapping e 
    and para_to_fun_def_map = 
      make_para_to_fun_def_mapping e
    and acl =
      try
        Unannotated_clause(
          lift_clause @@ Ident_map.find pt (clause_mapping e))
      with
      | Not_found ->
        raise @@ Invalid_query(
          Printf.sprintf "Variable %s is not defined" (show_ident pt))
    in

    (* let debug_bomb = ref 20 in *)

    (* let circleless_map = ref BatMultiPMap.empty in *)
    let visited = ref Annotated_clause_set.empty in
    (* map := BatMultiPMap.add id_start id_start !map;
       !map *)

    (* let tunnel_map = ref BatMultiPMap.empty in *)

    let rec loop acl dangling : unit = 
      if Annotated_clause_set.mem acl !visited
      then ()
      else
        begin
          visited := Annotated_clause_set.add acl !visited;

          let prev_acls = List.of_enum @@ preds acl cfg in
          (* log_acl acl prev_acls; *)

          (* debug to prevent infinite loop *)
          (* debug_bomb := !debug_bomb - 1;
             if !debug_bomb = 0
             then failwith "bomb"
             else ()
             ; *)

          (* process logic *)
          (* if cfg shows only one of then-block and else-block is possible,
             we can change the tracelet accordingly.
             e.g. [prev: [r = c ? ...; r = r1 @- r]]
          *)
          if List.length prev_acls = 2 && has_condition_clause prev_acls
          then map := Tracelet.choose_cond_block prev_acls cfg !map
          else ()
          ;

          (* step logic *)
          let continue = ref true
          and block_dangling = ref dangling in
          begin
            match acl with
            | Unannotated_clause _
            | Start_clause _ | End_clause _ ->
              ()
            (* into fbody *)
            | Binding_exit_clause (Abs_var _para, Abs_var ret_var, Abs_clause(Abs_var site_r, Abs_appl_body _)) -> 
              (* para can also be ignored in Fun since para is a property of a Fun block, defined in the source code
              *)
              let f_def = Ident_map.find ret_var ret_to_fun_def_map in
              map := Tracelet.add_id_dst site_r f_def !map;
              block_dangling := false
            (* out of fbody *)
            | Binding_enter_clause (Abs_var para, _, Abs_clause(Abs_var site_r, Abs_appl_body _)) ->
              let f_def = Ident_map.find para para_to_fun_def_map in
              map := Tracelet.add_id_dst site_r f_def !map;

              map := Tracelet.add_callsite f_def site_r !map;

              continue := dangling
            (* into cond-body *)
            | Binding_exit_clause (_, Abs_var ret_var, Abs_clause(Abs_var site_r, Abs_conditional_body _)) -> 
              block_dangling := false
            (* out of cond-body *)
            | Nonbinding_enter_clause (Abs_value_bool cond, 
                                       Abs_clause(Abs_var site_r, Abs_conditional_body(Abs_var x1, _e_then, _e_else))) ->
              continue := dangling
            | Binding_exit_clause (_, _, _) ->
              failwith "impossible binding exit for non-sites"
            | Binding_enter_clause (_, _, _) ->
              failwith "impossible binding enter for non callsites"
            | Nonbinding_enter_clause (_, _) ->
              failwith "impossible non-binding enter for non condsites"
          end;
          if !continue
          then
            Enum.iter (fun acl -> loop acl !block_dangling) (preds acl cfg)
          else
            ()

        end
    in
    loop acl true;
    !map

  let run_deterministic oracle e pt =
    let map = annotate e pt in

    let rec loop oracle pt acc =
      let tl = Tracelet.find_by_id pt map in
      match tl.source_block with
      | Main _ -> oracle, ((Frame (tl.point, pt)) :: acc)
      | Fun b -> (
          match oracle with
          | Oracle orl -> (
              let pt', oracle' = orl b.callsites in
              loop oracle' pt' ((Frame (tl.point, pt)):: acc))
        )
      | _ -> loop oracle tl.point ((Frame (tl.point, pt)) :: acc)
    in

    let oracle', trace = loop oracle pt [] in

    oracle', map, trace

  let run_shortest e pt = 
    let shortest_oracle = make_list_oracle [] in
    let _, map, trace = run_deterministic shortest_oracle e pt in
    map, trace

  let empty_relstk = Mega_constraint.Relstack.empty_relstk

  let gen_clauses_frames oracle map frames =
    List.fold_lefti (fun (oracle0, acc) i (Frame (tid, pt)) -> 
        let tl_static = Ident_map.find tid map in
        let with_end = i <> List.length frames - 1 in
        let tl = Tracelet.cut_before with_end pt tl_static in
        let oracle1, cs = constraints_of_tracelet oracle0 map true tl empty_relstk in
        oracle1, acc @ cs
      ) (oracle, []) frames 

  let gen_clauses oracle e pt =
    let oracle', map, frames = run_deterministic oracle e pt in
    let _, clauses = gen_clauses_frames oracle' map frames in
    clauses

  let gen_shortest_clauses_frames map frames =
    let shortest_oracle = make_list_oracle [] in
    let _, clauses = gen_clauses_frames shortest_oracle map frames in
    clauses

  let gen_shortest_clauses e pt =
    let shortest_oracle = make_list_oracle [] in
    gen_clauses shortest_oracle e pt

  let update_env env clauses stk =
    List.fold_left (fun env tc ->
        let (Clause (Var (cid, _), _)) = tc.clause in
        Ident_map.add cid (Symbol (cid, stk)) env
      ) env clauses

  let constraint_of_direct_clause stk env (tc : Tracelet.tl_clause) = 
    let x_sym = Symbol(tc.id, stk) in  
    match tc.cat, tc.clause with
    | Direct, Clause(_, Value_body(Value_int n)) -> 
      Some (Constraint.Constraint_value(x_sym, Constraint.Int n))
    | Direct, Clause(_, Value_body(Value_bool b)) -> 
      Some (Constraint.Constraint_value(x_sym, Constraint.Bool b))
    | Direct, Clause(_, Var_body (Var (id, _))) -> 
      Some (constraint_of_alias x_sym (Ident_map.find id env))
    | Direct, Clause(_, Binary_operation_body (Var (id1, _), op, Var (id2, _))  ) -> 
      Some (Constraint.Constraint_binop (x_sym, (Ident_map.find id1 env), op, (Ident_map.find id2 env)))
    | _ -> None

  let rec eval_tracelet tl_map main_cf (tl : Tracelet.t) oracle env stk =
    let clauses = match tl.source_block with
      | Main b -> b.clauses
      | Fun b -> b.block.clauses
      | CondBoth c -> failwith "cut_before can only return CondChosen determinstically"
      | CondChosen c -> c.block.clauses 
    in
    let env1 = update_env env clauses stk in
    let c0 = constraints_of_block_type tl stk in
    let cds = List.filter_map (constraint_of_direct_clause stk env1) clauses in
    let oracle', ccs = List.fold_lefti
        (fun (oracle, acc) i clause ->
           let no_fun_return = main_cf && i == List.length clauses - 1 in
           let oracle', cs = eval_site tl_map no_fun_return clause oracle env stk in
           oracle', acc @ cs
        ) 
        (oracle, []) clauses in
    oracle', env, stk, c0 @ cds @ ccs

  and eval_site tl_map no_fun_return clause oracle env stk =
    let Oracle orl = oracle in
    match clause.cat with
    | App dsts ->
      oracle, []
    | Cond _ ->
      oracle, []
    | _ ->
      oracle, []

  let naive_eval oracle map frames =
    let _, _, _, cs = 
      List.fold_lefti (fun (oracle, env, stk, acc) i (Frame (tid, pt)) -> 
          let tl_static = Ident_map.find tid map in
          let with_end = i <> List.length frames - 1 in
          let tl = Tracelet.cut_before with_end pt tl_static in
          let oracle', env', stk', constaints = eval_tracelet map true tl oracle env stk in
          oracle', env', stk', acc @ constaints
        ) (oracle, Ident_map.empty, empty_relstk, []) frames 
    in
    cs

  (* two passes ? *)
end

module Naive = struct
  (* the inner and outer should have similar structure,
     when we use one_oracle for inner, it hints we should also use one_oracle for outer *)
  type oracle = {
    block_id : Ident.t;
    x_to : Ident.t option;
    path : path;
    inner : oracle list;
    outer : oracle option }
  and path =
    | Choice of bool
    | Call of Ident.t * Ident.t
    | Main

  (* if the oracle is homomorphic to the frame actication (function call) 
     an empty oracle is NOT the same as None

     Todo:
      where to put funenter funexit
     Relusion:
      caller_block: funenter
      callee_block: funexit a.k.a. funexit bounds to the block
     Reason:

  *)
  (* oracle api : split, join, reverse *)

  (*  *)

  let id_of_clause (Clause (Var (id, _), _)) = id

  open Tracelet
  open Tracelet_constraint
  open Mega_constraint

  let oracle_of_naive_call fname arg =
    let x = Ident fname 
    and a = Ident arg in
    { block_id = x; 
      path = Call (x, a);
      x_to = None; 
      inner = []; 
      outer = None} 


  let stack_of_x oracle stack x = stack

  let walk oracle map program stack0 =
    let all_cs = ref [] in

    let rec loop (oracle : oracle) stack =
      let tl = 
        match oracle.x_to with
        | Some pt -> 
          let tl0 = Tracelet.find_by_id pt map in
          Tracelet.cut_before false pt tl0
        | None ->
          Ident_map.find oracle.block_id map
      in
      let block = Tracelet.get_block tl in 
      let cds = List.fold_left (fun acc tc ->
          let c' = 
            match tc.cat with
            | Direct -> (
                match constraints_of_direct_clause stack tc with
                | Some c -> [c]
                | _ -> []
              )
            | _ -> []
          in
          acc @ c'
        ) [] block.clauses in
      let _ = List.fold_left (fun choices tc ->
          let choices' = 
            match tc.cat with
            | App _ -> (
                let choice, choices'' = List.hd choices, List.tl choices in
                (match choice.path with
                 | Call(f, _arg) -> (
                     let stack' = Relstack.push stack tc.id in
                     loop choice stack'
                   )
                 | _ -> failwith "call oracle"
                ) ;
                choices'')
            | _ -> choices
          in choices'
        ) oracle.inner block.clauses in

      let cb = constraints_of_block_type tl stack in

      let cs = cds @ cb in
      all_cs := !all_cs @ cs;
      match oracle.outer with
      | Some oracle -> 
        loop oracle stack
      | None -> ()
    in

    loop oracle stack0;
    !all_cs


  (* let oracle_split oracle_block clauses =

     let (cs, c, _), _ =
      List.fold_while 
        (fun (cs, c, found) clause ->
           if oracle_block.last_block
           then found
           else id_of_clause clause <> oracle_block.x
        )
        (fun (cs, _, found) clause ->
           cs @ [clause], clause, id_of_clause clause <> oracle_block.x
        )
        ([], (List.hd clauses), false)
        clauses
     in
     cs, c *)
end