open Core

(* Hashtbl.t is mutable by default.
   Using explicit *mutable* is for replacing a new one easier.
*)
type info = {
  first : Id.t;
  target : Id.t;
  program : Odefa_ast.Ast.expr;
  block_map : Tracelet.block Odefa_ast.Ast.Ident_map.t;
}

type state = {
  (* graph attr *)
  root_node : Gate.Node.t ref;
  mutable tree_size : int;
  (* node attr *)
  node_map : (Lookup_key.t, Gate.Node.t ref) Hashtbl.t;
  (* constraints *)
  mutable phis_z3 : Z3.Expr.expr list;
  phi_map : (Lookup_key.t, Constraint.t list) Hashtbl.t;
  (* cvar *)
  cvar_counter : int ref;
  cvar_complete : (Cvar.t, bool) Hashtbl.t;
  cvar_complete_false : Cvar.t Hash_set.t;
  mutable cvar_complete_true_z3 : Z3.Expr.expr list;
  mutable cvar_picked_map : (Cvar.t, bool) Hashtbl.t;
  (* pvar *)
  pvar_reach_top : bool ref;
  lookup_created : Lookup_key.t Hash_set.t;
  (* debug *)
  noted_phi_map : (Lookup_key.t, (string * Z3.Expr.expr) list) Hashtbl.t;
}

let create_state block x_target =
  let state =
    {
      root_node = ref (Gate.root_node (block |> Tracelet.id_of_block) x_target);
      tree_size = 1;
      node_map = Hashtbl.create (module Lookup_key);
      phis_z3 = [];
      phi_map = Hashtbl.create (module Lookup_key);
      cvar_counter = ref 0;
      cvar_complete = Hashtbl.create (module Cvar);
      cvar_complete_false = Hash_set.create (module Cvar);
      cvar_complete_true_z3 = [];
      cvar_picked_map = Hashtbl.create (module Cvar);
      pvar_reach_top = ref false;
      lookup_created = Hash_set.create (module Lookup_key);
      noted_phi_map = Hashtbl.create (module Lookup_key);
    }
  in
  Hash_set.strict_add_exn state.lookup_created (Lookup_key.start x_target);
  state

let create_cvar state cvar_partial =
  let counter : int =
    Int.incr state.cvar_counter;
    !(state.cvar_counter)
    (* Hashtbl.find_or_add state.cvar_partial_map cvar_partial ~default:(fun () -> *)
  in

  let cvar =
    let lookups, cat, r_stk = cvar_partial in
    Cvar.
      {
        lookups;
        cat;
        r_stk;
        complete_name = Cvar.str_of_complete counter;
        picked_name = Cvar.str_of_picked counter;
      }
  in
  Hashtbl.add_exn state.cvar_complete ~key:cvar ~data:false;
  Hash_set.strict_add_exn state.cvar_complete_false cvar;
  cvar

let clear_phis state = state.phis_z3 <- []

let get_cvars_z3 ?(debug = false) state =
  let cvars_false = Hash_set.to_list state.cvar_complete_false in
  let cvars_false_z3 = Solver.cvar_complete_false_to_z3 cvars_false in
  Debug_log.log_choices_complete debug cvars_false_z3;
  let cvars_true_z3 = state.cvar_complete_true_z3 in
  state.cvar_complete_true_z3 <- [];
  (cvars_true_z3, cvars_false_z3)

(* let phi_z3_list =
     let phi_z3_map =
       Hashtbl.mapi state.phi_map ~f:(fun ~key ~data ->
           let debug_tool =
             Option.map state.noted_phi_map ~f:(fun map -> (key, map))
           in
           List.map data
             ~f:(Solver.phi_z3_of_constraint ?debug_tool))
     in
     Hashtbl.data phi_z3_map
   in *)

let guarantee_singleton_c_stk_exn state =
  let stop (node : Gate.Node_ref.t) =
    let picked =
      if List.is_empty !node.preds then
        true
      else
        List.fold !node.preds ~init:false ~f:(fun acc edge ->
            let this =
              Option.value_map edge.label_cvar ~default:true ~f:(fun cvar ->
                  Option.value
                    (Hashtbl.find state.cvar_picked_map cvar)
                    ~default:false)
            in
            acc || this)
    in
    not picked
  in
  let done_c_stk_set = Hash_set.create (module Concrete_stack) in
  let at_node (node : Gate.Node_ref.t) =
    match !node.rule with
    | Done c_stk -> Hash_set.add done_c_stk_set c_stk
    | _ -> ()
  in
  Gate.traverse_node ~stop ~at_node ~init:()
    ~acc_f:(fun _ _ -> ())
    state.root_node;
  Logs.info (fun m ->
      m "C_stk: %a"
        (Fmt.Dump.list Concrete_stack.pp)
        (Hash_set.to_list done_c_stk_set));
  if Hash_set.length done_c_stk_set = 1 then
    List.hd_exn (Hash_set.to_list done_c_stk_set)
  else if Hash_set.length done_c_stk_set = 0 then
    Concrete_stack.empty
  else
    failwith "Incorrect c_stk set."

let[@landmark] find_c_stk state =
  let found = ref false in
  let result_c_stk = ref Concrete_stack.empty in
  let stop (node : Gate.Node_ref.t) =
    if !found then
      true
    else
      let picked =
        if List.is_empty !node.preds then
          true
        else
          List.fold !node.preds ~init:false ~f:(fun acc edge ->
              let this =
                Option.value_map edge.label_cvar ~default:true ~f:(fun cvar ->
                    Option.value
                      (Hashtbl.find state.cvar_picked_map cvar)
                      ~default:false)
              in
              acc || this)
      in
      not picked
  in
  let at_node (node : Gate.Node_ref.t) =
    match !node.rule with
    | Done c_stk ->
        result_c_stk := c_stk;
        found := true
    | _ -> ()
  in
  Gate.traverse_node ~stop ~at_node ~init:()
    ~acc_f:(fun _ _ -> ())
    state.root_node;
  !result_c_stk
