open Core
open Graph
open Dj_common
open Log.Export
open Jayil
open Ast
module SuduZ3 = Solver.SuduZ3
open SuduZ3

type dvalue =
  | Direct of value
  | FunClosure of Ident.t * function_value * denv
  | RecordClosure of record_value * denv
  | AbortClosure of denv

and dvalue_with_stack = dvalue * Concrete_stack.t
and denv = dvalue_with_stack Ident_map.t

let value_of_dvalue = function
  | Direct v -> v
  | FunClosure (_fid, fv, _env) -> Value_function fv
  | RecordClosure (r, _env) -> Value_record r
  | AbortClosure _ -> Value_bool false

let rec pp_dvalue oc = function
  | Direct v -> Jayil.Pp.value oc v
  | FunClosure _ -> Format.fprintf oc "(fc)"
  | RecordClosure (r, env) -> pp_record_c (r, env) oc
  | AbortClosure _ -> Format.fprintf oc "(abort)"

and pp_record_c (Record_value r, env) oc =
  let pp_entry oc (x, v) = Fmt.pf oc "%a = %a" Id.pp x Jayil.Pp.var_ v in
  (Fmt.braces (Fmt.iter_bindings ~sep:(Fmt.any ", ") Ident_map.iter pp_entry))
    oc r

exception Found_target of { x : Id.t; stk : Concrete_stack.t; v : dvalue }
exception Found_abort of dvalue
exception Terminate of dvalue
exception Reach_max_step of Id.t * Concrete_stack.t
exception Run_the_same_stack_twice of Id.t * Concrete_stack.t
exception Run_into_wrong_stack of Id.t * Concrete_stack.t

type mode =
  | Plain
  | With_target_x of Id.t
  | With_full_target of Id.t * Concrete_stack.t

type clause_cb = Id.t -> Concrete_stack.t -> value -> unit
type debug_mode = No_debug | Debug_clause of clause_cb

module G = Imperative.Digraph.ConcreteBidirectional (Id_with_stack)

type session = {
  (* mode *)
  input_feeder : Input_feeder.t;
  mode : mode;
  (* tuning *)
  step : int ref;
  max_step : int option;
  (* book-keeping *)
  alias_graph : G.t;
  (* debug *)
  is_debug : bool; (* TODO: get rid of this *)
  debug_mode : debug_mode;
  val_def_map : (Id_with_stack.t, clause_body * dvalue) Hashtbl.t;
  term_detail_map : (Lookup_key.t, Lookup_detail.t) Hashtbl.t;
  block_map : Cfg.block Jayil.Ast.Ident_map.t;
  rstk_picked : (Rstack.t, bool) Hashtbl.t;
  lookup_alert : Lookup_key.t Hash_set.t;
}

let make_default_session () =
  {
    input_feeder = Fn.const 42;
    mode = Plain;
    max_step = None;
    is_debug = false;
    debug_mode = No_debug;
    step = ref 0;
    alias_graph = G.create ();
    val_def_map = Hashtbl.create (module Id_with_stack);
    block_map = Jayil.Ast.Ident_map.empty;
    term_detail_map = Hashtbl.create (module Lookup_key);
    rstk_picked = Hashtbl.create (module Rstack);
    lookup_alert = Hash_set.create (module Lookup_key);
  }

let create_session ?max_step ?(debug_mode = No_debug) (state : Global_state.t)
    (config : Global_config.t) mode input_feeder : session =
  (* = With_full_target (config.target, target_stk) *)
  {
    input_feeder;
    mode;
    max_step;
    is_debug = config.debug_interpreter;
    debug_mode;
    step = ref 0;
    alias_graph = G.create ();
    block_map = state.info.block_map;
    val_def_map = Hashtbl.create (module Id_with_stack);
    term_detail_map = state.search.lookup_detail_map;
    rstk_picked = state.stat.rstk_picked;
    lookup_alert = state.stat.lookup_alert;
  }

(**************** Branch Tracking: ****************)

(* Branch status types: *)
type status = Hit | Unhit

type branch_status = {
  mutable true_branch : status;
  mutable false_branch : status;
}

type branch = True_branch of ident | False_branch of ident

let branch_to_str b =
  match b with
  | True_branch (Ident s) -> s ^ ":true"
  | False_branch (Ident s) -> s ^ ":false"

(* Exceptions: *)
exception All_Branches_Hit
exception Unreachable_Branch of branch
exception Unsatisfiable_Branch of branch
exception Missed_Target_Branch

(* Tracks if a given branch has been hit in our testing yet. *)
let branches = Ident_hashtbl.create 2

(* Tracks the target branch: the next branch to solve constraints for.
   Includes branch identifier as lookup_key, branch condition as lookup_key,
   and branch direction as boolean. *)
let target_branch : (Lookup_key.t * Lookup_key.t * bool) option ref = ref None

let status_to_string status =
  match status with Hit -> "HIT" | Unhit -> "UNHIT"

let print_branch_status x branch_status =
  let (Ident s) = x in
  Format.printf "%s: True=%s; False=%s\n" s
    (status_to_string branch_status.true_branch)
    (status_to_string branch_status.false_branch)

let print_branches () =
  Format.printf "\nBranch Information:\n" ;
  Ident_hashtbl.iter (fun x s -> print_branch_status x s) branches

(* Adds a branch to tracking. *)
let add_branch x =
  Ident_hashtbl.add branches x { true_branch = Unhit; false_branch = Unhit }

(* Updates target branch to new branch, with specific condition and direction: *)
let update_target_branch branch_key condition_key direction =
  Format.printf "Updating target branch to: %s; condition: %s = %b\n"
    (Lookup_key.to_string branch_key)
    (Lookup_key.to_string condition_key)
    direction ;
  target_branch := Some (branch_key, condition_key, direction)

(* Marks a branch as hit. Updates target branch if needed: *)
let hit_branch branch_key condition_key direction =
  Format.printf "Hitting: %s: %b\n" (Lookup_key.to_string branch_key) direction ;
  let branch_status = Ident_hashtbl.find branches branch_key.x in
  match (direction, branch_status.true_branch, branch_status.false_branch) with
  | true, Unhit, Unhit ->
      branch_status.true_branch <- Hit ;
      update_target_branch branch_key condition_key (not direction)
  | false, Unhit, Unhit ->
      branch_status.false_branch <- Hit ;
      update_target_branch branch_key condition_key (not direction)
  | true, Unhit, Hit -> branch_status.true_branch <- Hit
  | false, Hit, Unhit -> branch_status.false_branch <- Hit
  | true, Hit, Unhit ->
      update_target_branch branch_key condition_key (not direction)
  | false, Unhit, Hit ->
      update_target_branch branch_key condition_key (not direction)
  | _, _, _ -> ()

(* Find all branches in an expression. *)
let rec find_branches (e : expr) : unit =
  let (Expr clauses) = e in
  List.fold clauses ~init:() ~f:(fun () clause ->
      find_branches_in_clause clause)

(* Find all branches in a clause. *)
and find_branches_in_clause (clause : clause) : unit =
  let (Clause (Var (x, _), cbody)) = clause in
  match cbody with
  | Conditional_body (x2, e1, e2) ->
      add_branch x ;
      find_branches e1 ;
      find_branches e2
  | Value_body (Value_function (Function_value (x2, e1))) -> find_branches e1
  | _ -> ()

let assert_target_hit
    (current_target : (Lookup_key.t * Lookup_key.t * bool) option) =
  match current_target with
  | None -> ()
  | Some (branch, condition, direction) -> (
      let branch_status = Ident_hashtbl.find branches branch.x in
      let result =
        match direction with
        | true -> branch_status.true_branch
        | false -> branch_status.false_branch
      in
      match result with Hit -> () | Unhit -> raise Missed_Target_Branch)

(* Checks if all branches have been hit. Returns an unhit branch or None. *)
let all_branches_hit () =
  let folder x status accum =
    match (status.true_branch, status.false_branch) with
    | Unhit, Unhit -> Some (True_branch x)
    | Unhit, Hit -> Some (True_branch x)
    | Hit, Unhit -> Some (False_branch x)
    | Hit, Hit -> accum
  in
  Ident_hashtbl.fold folder branches None

(*************** SMT Solver *************)
let solver = Z3.Solver.mk_solver SuduZ3.ctx None

(* Stores implications made by taking a certain branch path: *)
let implication_store = Hashtbl.create (module Lookup_key)

(* Stores necessary preconditions needed to use a variable: *)
let parent_implications = Hashtbl.create (module Lookup_key)

(* Add formula to implication store: "parent => formula" *)
let add_implication formula (condition, direction) =
  let store = Hashtbl.find implication_store condition in
  match store with
  | None -> Hashtbl.add_exn implication_store ~key:condition ~data:[ formula ]
  | Some lst ->
      Hashtbl.set implication_store ~key:condition ~data:(formula :: lst)

(* Parse a list of parents: generate conditions for each parent and list of grandparents: *)
let parse_parents parents =
  List.fold parents ~init:([], [])
    ~f:(fun (exps, parents) (parent, direction) ->
      (Riddler.eqv parent (Value_bool direction) :: exps, parent :: parents))

(* Given a list of child variables, generate antecedent clause for all of their parents. Also return list
   of grandparents. *)
let gen_antecedents children =
  List.fold children ~init:([], []) ~f:(fun (exps, parents) key ->
      match Hashtbl.find parent_implications key with
      | None -> (exps, parents)
      | Some parent_lst ->
          let new_exps, new_parents = parse_parents parent_lst in
          (new_exps @ exps, new_parents @ parents))

(* Just get dependency antecdent for a list of children variables: *)
let rec get_dependencies children =
  List.fold children ~init:[] ~f:(fun deps key ->
      match Hashtbl.find parent_implications key with
      | None -> deps
      | Some lst ->
          let exps, parents = parse_parents lst in
          deps @ exps @ get_dependencies parents)

(* Recursively create right associative chain of implications. *)
let rec gen_formula children formula =
  match gen_antecedents children with
  | [], _ -> formula
  | exps, parents ->
      let antecedent =
        if List.length exps > 1 then Riddler.and_ exps else List.hd_exn exps
      in
      let implication = Riddler.( @=> ) antecedent formula in
      gen_formula parents implication

(* Add formula to constraint solver: *)
let add_formula children parent formula =
  let new_formula = gen_formula children formula in
  match parent with
  | None ->
      Printf.printf "ADD FORMULA: %s\n" (Z3.Expr.to_string new_formula) ;
      Z3.Solver.add solver [ new_formula ]
  | Some parent -> add_implication new_formula parent

(* Adds parents based off of completion of a conditional clause: *)
let add_parents child_key condition_key direction ret_key =
  match
    ( Hashtbl.find parent_implications child_key,
      Hashtbl.find parent_implications ret_key )
  with
  | None, None ->
      Hashtbl.set parent_implications ~key:child_key
        ~data:[ (condition_key, direction) ]
  | Some lst, None ->
      Hashtbl.set parent_implications ~key:child_key
        ~data:((condition_key, direction) :: lst)
  | None, Some lst ->
      Hashtbl.set parent_implications ~key:child_key
        ~data:((condition_key, direction) :: lst)
  | Some lst1, Some lst2 ->
      Hashtbl.set parent_implications ~key:child_key
        ~data:(((condition_key, direction) :: lst1) @ lst2)

(* Adds parent dependencies for a child variable: *)
let add_parents2_ child_key dependencies =
  let cur_parents =
    match Hashtbl.find parent_implications child_key with
    | None -> []
    | Some lst -> lst
  in
  List.fold dependencies ~init:cur_parents ~f:(fun cur dep ->
      match Hashtbl.find parent_implications dep with
      | None -> cur
      | Some lst ->
          let new_lst = lst @ cur in
          Hashtbl.set parent_implications ~key:child_key ~data:new_lst ;
          new_lst)

(* Complete conditional branch and flush all implications from direction taken: *)
let flush_implications parent condition_key direction =
  let antecedent = Riddler.eqv condition_key (Value_bool direction) in
  let store = Hashtbl.find implication_store condition_key in
  match store with
  | None -> ()
  | Some lst ->
      let consequent = Riddler.and_ lst in
      let implication = Riddler.( @=> ) antecedent consequent in
      add_formula [ condition_key ] parent implication ;
      Hashtbl.remove implication_store condition_key

(* Is concolic run the first run? *)
let first_run = ref true

(* Solves constraints for a target branch: *)
let solve_SMT_branch (branch_key, condition_key, direction) =
  (* Force us to pick target branch and use its condition: *)
  let pick_formula = Riddler.picked branch_key in
  let condition_formula = Riddler.eqv condition_key (Value_bool direction) in
  Format.printf "Solving for target branch:\n" ;
  Format.printf "Branch to pick: %s\n" (Z3.Expr.to_string pick_formula) ;
  Format.printf "Branch condition: %s\n" (Z3.Expr.to_string condition_formula) ;
  match
    get_model solver
    @@ Z3.Solver.check solver [ condition_formula; pick_formula ]
  with
  | Some model -> model
  | None ->
      let ex =
        if direction
        then Unsatisfiable_Branch (True_branch branch_key.x)
        else Unsatisfiable_Branch (False_branch branch_key.x)
      in
      raise ex

(* Generates new input feeder for next concolic run: *)
let generate_input_feeder target =
  Concolic_feeder.from_model @@ solve_SMT_branch target

let cond_fid b = if b then Ident "$tt" else Ident "$ff"

(* This function will add a directed edge x1 -> x2 in the alias graph. Thus
   x1 here needs to be the *later* defined variable. *)
let add_alias x1 x2 session : unit =
  let alias_graph = session.alias_graph in
  G.add_edge alias_graph x1 x2

let add_val_def_mapping x vdef session : unit =
  let val_def_mapping = session.val_def_map in
  Hashtbl.add_exn ~key:x ~data:vdef val_def_mapping

let debug_update_read_node session x stk =
  match (session.is_debug, session.mode) with
  | true, With_full_target (_, target_stk) ->
      let r_stk = Rstack.relativize target_stk stk in
      let block = Cfg.(find_reachable_block x session.block_map) in
      let key = Lookup_key.of3 x r_stk block in
      (* Fmt.pr "@[Update Get to %a@]\n" Lookup_key.pp key; *)
      Hashtbl.change session.term_detail_map key ~f:(function
        | Some td -> Some { td with get_count = td.get_count + 1 }
        | None -> failwith "not term_detail")
  | _, _ -> ()

let debug_update_write_node session x stk =
  match (session.is_debug, session.mode) with
  | true, With_full_target (_, target_stk) ->
      let r_stk = Rstack.relativize target_stk stk in
      let block = Cfg.(find_reachable_block x session.block_map) in
      let key = Lookup_key.of3 x r_stk block in
      (* Fmt.pr "@[Update Set to %a@]\n" Lookup_key.pp key; *)
      Hashtbl.change session.term_detail_map key ~f:(function
        | Some td -> Some { td with is_set = true }
        | None -> failwith "not term_detail")
  | _, _ -> ()

let debug_stack session x stk (v, _) =
  match (session.is_debug, session.mode) with
  | true, With_full_target (_, target_stk) ->
      let rstk = Rstack.relativize target_stk stk in
      Fmt.pr "@[%a = %a\t\t R = %a@]\n" Id.pp x pp_dvalue v Rstack.pp rstk
  | _, _ -> ()

let raise_if_with_stack session x stk v =
  match session.mode with
  | With_full_target (target_x, target_stk) when Ident.equal target_x x ->
      if Concrete_stack.equal_flip target_stk stk
      then raise (Found_target { x; stk; v })
      else
        Fmt.(
          pr "found %a at stack %a, expect %a\n" pp_ident x Concrete_stack.pp
            target_stk Concrete_stack.pp stk)
  | With_target_x target_x when Ident.equal target_x x ->
      raise (Found_target { x; stk; v })
  | _ -> ()

let alert_lookup session x stk =
  match session.mode with
  | With_full_target (_, target_stk) ->
      let r_stk = Rstack.relativize target_stk stk in
      let block = Cfg.(find_reachable_block x session.block_map) in
      let key = Lookup_key.of3 x r_stk block in
      Fmt.epr "@[Update Alert to %a\t%a@]\n" Lookup_key.pp key Concrete_stack.pp
        stk ;
      Hash_set.add session.lookup_alert key
  | _ -> ()

let rec same_stack s1 s2 =
  match (s1, s2) with
  | (cs1, fid1) :: ss1, (cs2, fid2) :: ss2 ->
      Ident.equal cs1 cs2 && Ident.equal fid1 fid2 && same_stack ss1 ss2
  | [], [] -> true
  | _, _ -> false

let debug_clause ~session x v stk =
  ILog.app (fun m -> m "@[%a = %a@]" Id.pp x pp_dvalue v) ;

  (match session.debug_mode with
  | Debug_clause clause_cb -> clause_cb x stk (value_of_dvalue v)
  | No_debug -> ()) ;

  raise_if_with_stack session x stk v ;
  debug_stack session x stk (v, stk) ;
  ()

(* Creates lookup key based on variable name and current stack. To be used to generate constraint formulas. *)
let generate_lookup_key x stk =
  let rstk : Rstack.t = Rstack.from_concrete stk in
  let block : Cfg.block = { id = x; clauses = []; kind = Main } in
  (* UNUSED *)
  let key : Lookup_key.t = { x; r_stk = rstk; block } in
  key

(* OB: we cannot enter the same stack twice. *)
let rec eval_exp ~session stk env e parent : denv * dvalue =
  ILog.app (fun m -> m "@[-> %a@]\n" Concrete_stack.pp stk) ;
  (match session.mode with
  | With_full_target (_, target_stk) ->
      let r_stk = Rstack.relativize target_stk stk in
      Hashtbl.change session.rstk_picked r_stk ~f:(function
        | Some true -> Some false
        | Some false -> raise (Run_into_wrong_stack (Ast_tools.first_id e, stk))
        | None -> None)
  | _ -> ()) ;

  let (Expr clauses) = e in
  let denv, vs' =
    List.fold_map ~f:(eval_clause ~session stk parent) ~init:env clauses
  in
  (denv, List.last_exn vs')

(* OB: once stack is to change, there must be an `eval_exp` *)
and eval_clause ~session stk parent env clause : denv * dvalue =
  let (Clause (Var (x, _), cbody)) = clause in
  (match session.max_step with
  | None -> ()
  | Some t ->
      Int.incr session.step ;
      if !(session.step) > t then raise (Reach_max_step (x, stk)) else ()) ;

  debug_update_write_node session x stk ;
  let x_key = generate_lookup_key x stk in

  let (v : dvalue) =
    match cbody with
    | Value_body (Value_function vf as v) ->
        (* x = fun ... ; *)
        let retv = FunClosure (x, vf, env) in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in
        (* Add symbolic formula: *)
        add_formula [] parent @@ Riddler.eq_term_v x_key (Some v) ;
        retv
    | Value_body (Value_record r as v) ->
        (* x = {...} ; *)
        let retv = RecordClosure (r, env) in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in
        (* Add symbolic formula: *)
        add_formula [] parent @@ Riddler.eq_term_v x_key (Some v) ;
        retv
    | Value_body v ->
        (* x = <bool or int> ; *)
        let retv = Direct v in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in
        (* Add symbolic formula: *)
        add_formula [] parent @@ Riddler.eq_term_v x_key (Some v) ;
        retv
    | Var_body vx ->
        (* x = y ; *)
        let (Var (y, _)) = vx in
        let ret_val, ret_stk = fetch_val_with_stk ~session ~stk env vx in
        add_alias (x, stk) (y, ret_stk) session ;
        (* Add symbolic formula: *)
        let y_key = generate_lookup_key y ret_stk in
        add_formula [ y_key ] parent @@ Riddler.eq x_key y_key ;
        let _ = add_parents2_ x_key [ y_key ] in
        ret_val
    | Conditional_body (x2, e1, e2) ->
        (* x = if y then e1 else e2 ; *)
        let branch_result = fetch_val_to_bool ~session ~stk env x2 in
        let _, condition_stk = fetch_val_with_stk ~session ~stk env x2 in
        let (Var (y, _)) = x2 in
        let condition_key = generate_lookup_key y condition_stk in
        (* Hit branch: *)
        hit_branch x_key condition_key branch_result ;

        let e, stk' =
          if branch_result
          then (e1, Concrete_stack.push (x, cond_fid true) stk)
          else (e2, Concrete_stack.push (x, cond_fid false) stk)
        in

        (* Evaluate branch with new parent condition: *)
        let ret_env, ret_val =
          eval_exp ~session stk' env e (Some (condition_key, branch_result))
        in
        let (Var (ret_id, _) as last_v) = Ast_tools.retv e in
        let _, ret_stk = fetch_val_with_stk ~session ~stk:stk' ret_env last_v in

        (* Map x to result of 'if' statement: *)
        let ret_key = generate_lookup_key ret_id ret_stk in
        add_formula [ ret_key ] (Some (condition_key, branch_result))
        @@ Riddler.eq x_key ret_key ;
        flush_implications parent condition_key branch_result ;
        (* Add parent relationship to return variable: *)
        add_parents x_key condition_key branch_result ret_key ;

        (* TODO: add Pick x_key => parents? *)
        let dependencies =
          match parent with
          | Some (key, direction) ->
              Riddler.eqv key (Value_bool direction) :: get_dependencies [ key ]
          | _ -> []
        in
        let pick_formula =
          Riddler.( @=> ) (Riddler.picked x_key) (Riddler.and_ dependencies)
        in
        Format.printf "ADD PICK FORMULA: %s\n" (Z3.Expr.to_string pick_formula) ;
        Z3.Solver.add solver [ pick_formula ] ;

        add_alias (x, stk) (ret_id, ret_stk) session ;
        ret_val
    | Input_body ->
        (* TODO: the interpreter may propagate the dummy value (through the value should never be used in any control flow)  *)
        let n = session.input_feeder (x, stk) in
        let retv = Direct (Value_int n) in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in

        let (Ident s) = x in
        Format.printf "Feed %d to %s\n" n s ;
        (* Add symbolic formula: *)
        add_formula [] parent @@ Riddler.eq_term_v x_key None ;
        retv
    | Appl_body (vx1, (Var (x2, _) as vx2)) -> (
        (* x = f y ; *)
        match fetch_val ~session ~stk env vx1 with
        | FunClosure (fid, Function_value (Var (arg, _), body), fenv) ->
            let v2, v2_stk = fetch_val_with_stk ~session ~stk env vx2 in
            let stk2 = Concrete_stack.push (x, fid) stk in
            let env2 = Ident_map.add arg (v2, stk2) fenv in
            add_alias (arg, stk) (x2, v2_stk) session ;

            (* Enter function: *)
            let (Var (xid, _)) = vx1 in
            let _, f_stk = fetch_val_with_stk ~session ~stk env vx1 in
            let key_f = generate_lookup_key xid f_stk in
            let key_para = generate_lookup_key arg stk2 in
            let key_arg = generate_lookup_key x2 v2_stk in
            (* add_formula [key_f; key_arg] parent @@ Riddler.same_funenter key_f fid key_para key_arg; *)
            add_formula [ key_f; key_arg ] parent
            @@ Riddler.enter_fun key_para key_arg ;

            let ret_env, ret_val = eval_exp ~session stk2 env2 body parent in
            let (Var (ret_id, _) as last_v) = Ast_tools.retv body in
            let _, ret_stk =
              fetch_val_with_stk ~session ~stk:stk2 ret_env last_v
            in
            add_alias (x, stk) (ret_id, ret_stk) session ;

            (* Exit function: *)
            let key_out = generate_lookup_key ret_id ret_stk in
            (* add_formula [key_out; key_f; key_arg] parent @@ Riddler.same_funexit key_f fid x_key key_out; *)
            add_formula [ key_out; key_f; key_arg ] parent
            @@ Riddler.exit_fun x_key key_out ;

            (* Add dependencies for x: *)
            let _ = add_parents2_ x_key [ key_out; key_f; key_arg ] in

            ret_val
        | _ -> failwith "app to a non fun")
    | Match_body (vx, p) ->
        (* x = y ~ <pattern>; *)
        let match_res = Value_bool (check_pattern ~session ~stk env vx p) in
        let retv = Direct match_res in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in
        (* Add symbolic formula: *)
        let (Var (y, _)) = vx in
        let match_key = generate_lookup_key y stk in
        let x_exp = Riddler.key_to_var x_key in
        let pat_exp = Riddler.is_pattern match_key p in
        add_formula [ match_key ] parent
        @@ Riddler.and_
             [
               SuduZ3.ifBool x_exp;
               SuduZ3.eq (SuduZ3.project_bool x_exp) pat_exp;
             ] ;
        let _ = add_parents2_ x_key [ match_key ] in
        retv
    | Projection_body (v, key) -> (
        match fetch_val ~session ~stk env v with
        | RecordClosure (Record_value r, denv) ->
            let (Var (proj_x, _) as vv) = Ident_map.find key r in
            let dvv, vv_stk = fetch_val_with_stk ~session ~stk denv vv in
            add_alias (x, stk) (proj_x, vv_stk) session ;
            (* TODO: limited functionality with records as of now... *)
            let (Var (v_ident, _)) = v in
            let _, v_stk = fetch_val_with_stk ~session ~stk env v in
            let record_key = generate_lookup_key v_ident v_stk in
            let proj_key = generate_lookup_key proj_x vv_stk in
            add_formula [ proj_key; record_key ] parent
            @@ Riddler.eq x_key proj_key ;
            let _ = add_parents2_ x_key [ proj_key; record_key ] in
            dvv
        | Direct (Value_record (Record_value _record)) ->
            (* let vv = Ident_map.find key record in
               fetch_val env vv *)
            failwith "project should also have a closure"
        | _ -> failwith "project on a non record")
    | Not_body vx ->
        (* x = not y ; *)
        let v = fetch_val_to_direct ~session ~stk env vx in
        let bv =
          match v with
          | Value_bool b -> Value_bool (not b)
          | _ -> failwith "incorrect not"
        in
        let retv = Direct bv in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in

        (* Add symbolic formula: *)
        let (Var (y, _)) = vx in
        let y_key = generate_lookup_key y stk in
        add_formula [ y_key ] parent @@ Riddler.not_ x_key y_key ;
        let _ = add_parents2_ x_key [ y_key ] in

        retv
    | Binary_operation_body (vx1, op, vx2) ->
        (* x = y OP z ; *)
        let v1 = fetch_val_to_direct ~session ~stk env vx1
        and v2 = fetch_val_to_direct ~session ~stk env vx2 in
        let v =
          match (op, v1, v2) with
          | Binary_operator_plus, Value_int n1, Value_int n2 ->
              Value_int (n1 + n2)
          | Binary_operator_minus, Value_int n1, Value_int n2 ->
              Value_int (n1 - n2)
          | Binary_operator_times, Value_int n1, Value_int n2 ->
              Value_int (n1 * n2)
          | Binary_operator_divide, Value_int n1, Value_int n2 ->
              Value_int (n1 / n2)
          | Binary_operator_modulus, Value_int n1, Value_int n2 ->
              Value_int (n1 mod n2)
          | Binary_operator_less_than, Value_int n1, Value_int n2 ->
              Value_bool (n1 < n2)
          | Binary_operator_less_than_or_equal_to, Value_int n1, Value_int n2 ->
              Value_bool (n1 <= n2)
          | Binary_operator_equal_to, Value_int n1, Value_int n2 ->
              Value_bool (n1 = n2)
          | Binary_operator_equal_to, Value_bool b1, Value_bool b2 ->
              Value_bool (Bool.( = ) b1 b2)
          | Binary_operator_and, Value_bool b1, Value_bool b2 ->
              Value_bool (b1 && b2)
          | Binary_operator_or, Value_bool b1, Value_bool b2 ->
              Value_bool (b1 || b2)
          | Binary_operator_not_equal_to, Value_int n1, Value_int n2 ->
              Value_bool (n1 <> n2)
          | _, _, _ -> failwith "incorrect binop"
        in
        let retv = Direct v in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in

        (* Add symbolic formula: *)
        let (Var (y, _)) = vx1 in
        let (Var (z, _)) = vx2 in
        let _, stk1 = fetch_val_with_stk ~session ~stk env vx1 in
        let _, stk2 = fetch_val_with_stk ~session ~stk env vx2 in
        let y_key = generate_lookup_key y stk1 in
        let z_key = generate_lookup_key z stk2 in
        add_formula [ y_key; z_key ] parent
        @@ Riddler.binop x_key op y_key z_key ;
        let _ = add_parents2_ x_key [ y_key; z_key ] in

        retv
    | Abort_body -> (
        (* TODO: *)
        let ab_v = AbortClosure env in
        let () = add_val_def_mapping (x, stk) (cbody, ab_v) session in
        match session.mode with
        | Plain -> raise @@ Found_abort ab_v
        | With_target_x target ->
            if Id.equal target x
            then raise @@ Found_target { x; stk; v = ab_v }
            else raise @@ Found_abort ab_v
        | With_full_target (target, tar_stk) ->
            if Id.equal target x && Concrete_stack.equal_flip tar_stk stk
            then raise @@ Found_target { x; stk; v = ab_v }
            else raise @@ Found_abort ab_v)
    | Assert_body _ | Assume_body _ ->
        (* Limited functionality as of now: *)
        let retv = Direct (Value_bool true) in
        let () = add_val_def_mapping (x, stk) (cbody, retv) session in
        retv
    (* failwith "not supported yet" *)
  in
  debug_clause ~session x v stk ;
  (Ident_map.add x (v, stk) env, v)

and fetch_val_with_stk ~session ~stk env (Var (x, _)) :
    dvalue * Concrete_stack.t =
  let res = Ident_map.find x env in
  debug_update_read_node session x stk ;
  res

and fetch_val ~session ~stk env x : dvalue =
  fst (fetch_val_with_stk ~session ~stk env x)

and fetch_val_to_direct ~session ~stk env vx : value =
  match fetch_val ~session ~stk env vx with
  | Direct v -> v
  | _ -> failwith "eval to non direct value"

and fetch_val_to_bool ~session ~stk env vx : bool =
  match fetch_val ~session ~stk env vx with
  | Direct (Value_bool b) -> b
  | _ -> failwith "eval to non bool"

and check_pattern ~session ~stk env vx pattern : bool =
  let is_pass =
    match (fetch_val ~session ~stk env vx, pattern) with
    | Direct (Value_int _), Int_pattern -> true
    | Direct (Value_bool _), Bool_pattern -> true
    | Direct (Value_function _), _ -> failwith "fun must be a closure"
    | Direct (Value_record _), _ -> failwith "record must be a closure"
    | RecordClosure (Record_value record, _), Rec_pattern key_set ->
        Ident_set.for_all (fun id -> Ident_map.mem id record) key_set
    | RecordClosure (Record_value record, _), Strict_rec_pattern key_set ->
        Ident_set.equal key_set (Ident_set.of_enum @@ Ident_map.keys record)
    | FunClosure (_, _, _), Fun_pattern -> true
    | _, Any_pattern -> true
    | _, _ -> false
  in
  is_pass

(* Random integer generator. *)
let random_var_gen = Int.gen_incl (-10) 10
let global_max_step = 1000

let reset_tracking_vars () =
  target_branch := None ;
  Z3.Solver.reset solver ;
  Hashtbl.clear implication_store ;
  Hashtbl.clear parent_implications

(* Create session object for concolic execution: *)
let create_concolic_session input_feeder =
  {
    (make_default_session ()) with
    input_feeder;
    step = ref 0;
    max_step = Some global_max_step;
  }

(* First concolic run uses random integer generator as input feeder: *)
let default_concolic_session =
  create_concolic_session (fun _ ->
      Quickcheck.random_value ~seed:`Nondeterministic random_var_gen)

(* Creates a new session for a concolic execution run. *)
let generate_new_session () =
  match (all_branches_hit (), !target_branch) with
  | None, _ -> raise All_Branches_Hit
  | Some unhit, None ->
      if !first_run
      then default_concolic_session
      else raise @@ Unreachable_Branch unhit
  | Some _, Some target ->
      create_concolic_session @@ generate_input_feeder target

let rec eval e =
  Format.printf "------------------------------\nRunning program...\n" ;
  print_branches () ;

  let target_branch_str =
    match !target_branch with
    | None -> "None"
    | Some (branch_key, condition_key, direction) ->
        Lookup_key.to_string branch_key
        ^ "; condition: "
        ^ Lookup_key.to_string condition_key
        ^ " = " ^ Bool.to_string direction
  in
  Format.printf "\nTarget branch: %s\n" target_branch_str ;

  (* Check if execution is complete by generating a new session: *)
  let session = generate_new_session () in
  (* Weird bug where we need to manually reset these fields: *)
  session.step := 0 ;
  Hashtbl.clear session.val_def_map ;

  (* Store target branch and check that is has been hit after this run: *)
  let current_target = !target_branch in

  (* Now, we have a new session; reset tracking variables: *)
  reset_tracking_vars () ;

  (* Create new environment and evaluate program: *)
  let empty_env = Ident_map.empty in
  try
    let v = snd (eval_exp ~session Concrete_stack.empty empty_env e None) in
    (* Assert that we hit our target branch: *)
    assert_target_hit current_target ;
    (* Print evaluated result and run again. *)
    Format.printf "Evaluated to: %a\n" pp_dvalue v ;
    first_run := false ;
    eval e
  with
  (* TODO: Error cases: TODO, if we hit abort, re-run if setting is applied. *)
  | Reach_max_step (x, stk) ->
      (* Fmt.epr "Reach max steps\n" ; *)
      (* alert_lookup target_stk x stk session.lookup_alert; *)
      raise (Reach_max_step (x, stk))
  | Run_the_same_stack_twice (x, stk) ->
      Fmt.epr "Run into the same stack twice\n" ;
      alert_lookup session x stk ;
      raise (Run_the_same_stack_twice (x, stk))
  | Run_into_wrong_stack (x, stk) ->
      Fmt.epr "Run into wrong stack\n" ;
      alert_lookup session x stk ;
      raise (Run_into_wrong_stack (x, stk))

(* Concolically execute/test program. *)
let concolic_eval e =
  (* Collect branch information from AST: *)
  Ident_hashtbl.clear branches ;
  find_branches e ;
  (* Reset variables: *)
  reset_tracking_vars () ;
  first_run := true ;

  Format.printf "\nStarting concolic execution...\n" ;

  (* Repeatedly evaluate program: *)
  eval e
