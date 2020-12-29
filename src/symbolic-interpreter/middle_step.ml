open Batteries
(* open Jhupllib *)
open Odefa_ast
open Ast
open Odefa_ddpa
open Ddpa_abstract_ast
open Ddpa_graph

open Ast_helper
open Ddpa_helper

module Tracelet = struct
  (* TODO: we might get rid of map at all, by using tree (zipper) *)
  let name_main = "0_main"
  let id_main = Ident name_main

  type clause_cat = 
    | Direct
    | Fun 
    | App of ident list
    | Cond of ident list
  [@@deriving show {with_path = false}]

  type tl_clause = {
    id : ident;
    cat : clause_cat;
    clause : clause [@opaque];
  }
  [@@deriving show {with_path = false}]

  type clause_list = tl_clause list
  [@@deriving show {with_path = false}]

  type main_block = {
    point : ident;

    clauses: clause_list;
  }

  type fun_block = {
    point : ident;
    outer_point : ident;

    para: ident;
    clauses: clause_list;
    callsites: ident list;
  }

  type cond_block = { 
    point : ident;
    outer_point : ident;

    cond : ident;
    possible : bool option;
    choice : bool option;
    then_ : clause_list;
    else_ : clause_list;
  }

  type block = 
    | Main of main_block
    | Fun of fun_block
    | Cond of cond_block

  type t = block

  let get_clauses block = 
    match block with
    | Main mb -> mb.clauses
    | Fun fb -> fb.clauses
    | Cond cb ->
      (* must have a choice *)
      let choice = BatOption.get cb.choice in
      if choice then
        cb.then_
      else
        cb.else_

  let id_of_block = function
    | Main b -> b.point
    | Fun fb -> fb.point
    | Cond cb -> cb.point

  let outer_id_of_block = function
    | Main b -> failwith "no outer_point for main block"
    | Fun fb -> fb.outer_point
    | Cond cb -> cb.outer_point

  let ret_of block =
    let clauses = get_clauses block in
    (List.last clauses).id

  let update_clauses f block =
    match block with
    | Main b -> 
      let clauses = f b.clauses in 
      Main { b with clauses }
    | Fun b -> 
      let clauses = f b.clauses in 
      Fun { b with clauses }
    | Cond c -> 
      let then_ = f c.then_ in
      let else_ = f c.else_ in 
      Cond {c with then_ ; else_ }

  (* let cut_before with_target target block =
     let take clauses =
      let (clauses', _) , _ = 
        List.fold_while 
          (fun (acc, pre_found) tc ->
             (* use pre_found to postpone the loop for one iteration to contain the target *)
             if with_target then
               pre_found
             else
               tc.id <> target)
          (fun (acc, _) tc ->
             acc @ [tc], tc.id <> target
          )
          ([], false)
          clauses 
      in
      clauses'
     in
     update_clauses take block *)

  let find_by_id x block_map =
    block_map
    |> Ident_map.values
    |> Enum.find_map (fun tl -> match tl with
        | Main b -> 
          if List.exists (fun tc -> tc.id = x) b.clauses then
            Some tl
          else
            None
        | Fun b -> 
          if List.exists (fun tc -> tc.id = x) b.clauses then
            Some tl
          else
            None
        | Cond c -> 
          if List.exists (fun tc -> tc.id = x) c.then_ then
            Some (Cond {c with choice = Some true})
          else if List.exists (fun tc -> tc.id = x) c.else_ then
            Some (Cond {c with choice = Some false})
          else
            None
      )

  let clause_of_x block x =
    List.find_opt (fun tc -> tc.id = x) (get_clauses block)

  let clause_of_x_exn block x =
    List.find (fun tc -> tc.id = x) (get_clauses block)

  let update_id_dst id dst0 block =
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
    update_clauses (List.map add_dst_in_clause) block

  let add_id_dst site_x def_x tl_map =
    let tl = find_by_id site_x tl_map in
    let tl' = update_id_dst site_x def_x tl in
    Ident_map.add (id_of_block tl) tl' tl_map

  let choose_cond_block acls cfg tl_map =
    let cond_site, choice =
      match acls with
      | Unannotated_clause(Abs_clause(Abs_var cond_site, Abs_conditional_body _)) 
        :: wire_cl
        :: [] -> 
        cond_site, find_cond_top wire_cl cfg
      | _ -> failwith "wrong precondition to call"
    in
    let tl = Ident_map.find cond_site tl_map in
    let tracelet' = match tl with 
      | Cond c -> (
          let p' = match c.possible with
            | Some p when p = choice  -> Some p
            | Some p (* p <> choice *)-> None
            | None -> Some choice
          in          
          Cond { c with possible = p' }
        )
      | _ -> failwith "it should called just once on CondBoth"
    in
    Ident_map.add cond_site tracelet' tl_map

  let _add_callsite site tl =
    match tl with
    | Fun b -> Fun { b with callsites = (site :: b.callsites) }
    | _ -> failwith "wrong precondition to call add_callsite"

  let add_callsite f_def site tl_map =
    let tl = Ident_map.find f_def tl_map in
    let tl' = _add_callsite site tl in
    Ident_map.add f_def tl' tl_map

  let clauses_of_expr e =
    let (Expr clauses) = e in 
    List.fold_left (fun cs (Clause(Var (cid, _), b) as c) ->
        let c' = match b with
          | Appl_body (_, _)
            -> { id = cid; cat = App []; clause = c }
          | Conditional_body (Var (x, _), _, _)
            -> { id = cid; cat = Cond []; clause = c }
          | Value_body (Value_function _)
            -> { id = cid; cat = Fun; clause = c }
          | _
            -> { id = cid; cat = Direct; clause = c }
        in
        cs @ [c']
      ) [] clauses

  let tracelet_map_of_expr e : t Ident_map.t =
    let map = ref Ident_map.empty in

    let main_tracelet = 
      let clauses = clauses_of_expr e in
      Main { point = id_main; clauses } in
    map := Ident_map.add id_main main_tracelet !map
    ;

    let rec loop outer_point e =
      let Expr clauses = e in
      let handle_clause = function
        | Clause (Var (cid, _), Value_body (Value_function (Function_value (Var (para, _), fbody)))) ->
          let clauses = clauses_of_expr fbody in
          let tracelet = 
            Fun { point = cid ; outer_point; para; callsites = []; clauses } in
          map := Ident_map.add cid tracelet !map;
          loop cid fbody
        | Clause (Var (cid, _), Conditional_body (Var(cond, _), e1, e2)) ->
          let then_ = clauses_of_expr e1
          and else_ = clauses_of_expr e2 
          and possible = None in
          let tracelet = 
            Cond { point = cid ; outer_point; cond; possible; then_; else_; choice = None } in
          map := Ident_map.add cid tracelet !map;
          loop cid e1;
          loop cid e2     
        | _ -> ()
      in
      List.iter handle_clause clauses
    in

    loop id_main e;
    !map

  (*
    let fun_var_at_callsite s tl = 
    let block = get_block tl in
    let tc = List.find (fun cl -> cl.id = s) block.clauses in
    app_id1_of_clause tc.clause

  let legacy_get_block tl =
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

    let ids_of tl = 
    let block = get_block tl in
    List.map (fun tc -> tc.id) block.clauses

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
      (block.app_ids @ block.cond_ids) *)
end

(* module Oracle = struct
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
   end *)

(* module Tracelet_constraint = struct
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


   (* lagecy version *)
   let constraints_of_direct_clause' stk tc = 
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

   let constraints_of_direct_clause oracle_stk stk tc = 
    let x_sym = Symbol(tc.id, stk) in  
    match tc.cat, tc.clause with
    | Direct, Clause(_, Value_body(Value_int n)) -> 
      Some (Constraint.Constraint_value(x_sym, Constraint.Int n))
    | Direct, Clause(_, Value_body(Value_bool b)) -> 
      Some (Constraint.Constraint_value(x_sym, Constraint.Bool b))
    | Direct, Clause(_, Var_body(Var (y, _))) -> 
      let y_stk = oracle_stk stk y in
      let y_sym = Symbol(y, y_stk) in
      Some (constraint_of_alias x_sym y_sym)
    | _ -> None

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
          let ret_id = (List.last (legacy_get_block tl_dst).clauses).id in
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
    let cds = List.filter_map (constraints_of_direct_clause' stk) clauses in
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
   end *)

module Tunnel = struct
  (* open Tracelet *)
  (* open Oracle *)
  (* open Tracelet_constraint *)
  (* open Mega_constraint *)
  (* open Interpreter_types *)
  type frame = 
    | Frame of ident * ident

  type t = frame list

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
        raise @@ Interpreter.Invalid_query(
          Printf.sprintf "Variable %s is not defined" (show_ident pt))
    in

    (* let debug_bomb = ref 20 in *)
    let visited = ref Annotated_clause_set.empty in
    (* map := BatMultiPMap.add id_start id_start !map;
       !map *)
    let rec loop acl dangling : unit = 
      if Annotated_clause_set.mem acl !visited
      then ()
      else
        begin
          visited := Annotated_clause_set.add acl !visited;

          let prev_acls = List.of_enum @@ preds acl cfg in
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

  (* let run_deterministic oracle e pt =
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
     cs *)
end

module Naive = struct
  type oracle = {
    block_id : Ident.t;
    x_to : Ident.t option;
    path : path;
    inner : oracle list;
    outer : oracle option }
  and path =
    | Choice of bool
    | CallIn
    | CallOut of Ident.t
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

  (* 
  let id_of_clause (Clause (Var (id, _), _)) = id

     open Tracelet
     open Tracelet_constraint
     open Mega_constraint

     let oracle_of_naive_call fname  =
     let f = Ident fname in
     { block_id = f; 
      path = CallIn;
      x_to = None; 
      inner = []; 
      outer = None} 

     let tl_of_oracle oracle tl_map = 
     match oracle.x_to with
     | Some pt -> 
      let tl0 = Tracelet.find_by_id pt tl_map in
      Tracelet.cut_before false pt tl0
     | None ->
      Ident_map.find oracle.block_id tl_map

     let rec def_stack_of_x oracle tl_map stack x = 
     let tl = Ident_map.find oracle.block_id tl_map in
     log_id x;
     let ids = Tracelet.ids_of tl in 
     log_id (List.hd ids);
     if List.mem x ids
     then stack
     else (
      match oracle.outer with
      | Some oracle -> (
          match oracle.x_to with
          | Some callsite -> (
              let tlc = Ident_map.find oracle.block_id tl_map in
              let fun_var = fun_var_at_callsite callsite tlc in
              log_id fun_var;
              let stack' = Relstack.co_pop stack tl.point in
              def_stack_of_x oracle tl_map stack' fun_var)
          | None -> failwith "stack_of_x none")
      | _ -> failwith "stack_of_x"
     )

     let walk oracle map program stack0 =
     let all_cs = ref [] in

     let cds_of_block block stack =
      List.fold_left (fun acc tc ->
          let c' = 
            match tc.cat with
            | Direct -> (
                let oracle_stk = def_stack_of_x oracle map in
                match constraints_of_direct_clause oracle_stk stack tc with
                | Some c -> [c]
                | _ -> []
              )
            | _ -> []
          in
          acc @ c'
        ) [] block.clauses 
     in

     let rec walk_inner (oracle : oracle list) clauses stack =
      List.fold_left (fun choices tc ->
          let choices' = 
            match tc.cat with
            | App _ -> (
                let choice, choices'' = List.hd choices, List.tl choices in
                (match choice.path with
                 | CallIn -> (
                     let f = choice.block_id in
                     let stack' = Relstack.push stack tc.id in
                     let ftl = Ident_map.find f map in
                     let para = para_of ftl in
                     let c_callin = constraint_of_funenter (app_id2_of_clause tc.clause) stack para stack' in
                     let c_return = 
                       let ret_id = ret_of ftl in
                       constraint_of_funexit tc.id stack ret_id stack'
                     in
                     all_cs := !all_cs @ [c_callin; c_return];
                     loop false choice stack'
                   )
                 | _ -> failwith "call oracle"
                ) ;
                choices'')
            | _ -> choices
          in choices'
        ) oracle clauses 

     and loop is_walk_out (oracle : oracle) stack =
      let tl = tl_of_oracle oracle map in
      let block = Tracelet.get_block tl in 
      let cds = cds_of_block block stack in
      let _ = walk_inner oracle.inner block.clauses stack in
      let cb = constraints_of_block_type tl stack in

      let cs = cds @ cb in
      all_cs := !all_cs @ cs;

      if is_walk_out
      then 
        match oracle.outer, oracle.path with
        | Some oracle, CallOut arg -> 
          let stack' = Relstack.co_pop stack tl.point in
          let c_callin =
            let para = para_of tl in 
            constraint_of_funenter arg stack' para stack in
          all_cs := !all_cs @ [c_callin];
          loop true oracle stack'
        | _, _ -> ()
      else
        ()
     in

     loop true oracle stack0;
     !all_cs *)
end