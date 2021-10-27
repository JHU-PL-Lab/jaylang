open Core

let ctx = Z3.mk_context []

module SuduZ3 = Sudu.Z3_api.Make (struct
  let ctx = ctx
end)

let solver = Z3.Solver.mk_solver ctx None

let reset () = Z3.Solver.reset solver

let check phis_z3 cvars_z3 =
  Z3.Solver.add solver phis_z3;
  SuduZ3.check_with_assumption solver cvars_z3

let string_of_solver () = Z3.Solver.to_string solver

let var_of_symbol sym =
  let (Symbol.Id (x, r_stk)) = sym in
  SuduZ3.var_s (Symbol.name_of_lookup [ x ] r_stk)

let phi_z3_of_constraint ?debug_tool cs =
  let open SuduZ3 in
  let log_noted_phi note phi =
    match debug_tool with
    | Some (key, note_map) -> Hashtbl.add_multi note_map ~key ~data:(note, phi)
    | None -> ()
  in
  let rec z3_phis_of_smt_phi = function
    | Constraint.Eq_v (sx, cv) ->
        let x = var_of_symbol sx in
        let v =
          match cv with
          | Int i -> int_ i
          | Bool b -> bool_ b
          | Fun fid -> fun_ fid
          | Record -> failwith "no record yet"
        in
        eq x v
    (* [eq x v; not_ (eq x bottom_)] *)
    | Constraint.Eq_x (sx, sy) ->
        let x = var_of_symbol sx in
        let y = var_of_symbol sy in
        eq x y
    (* [eq x y; not_ (eq x bottom_); not_ (eq y bottom_)] *)
    | Constraint.Eq_binop (sy, sx1, op, sx2) ->
        let y = var_of_symbol sy in
        let x1 = var_of_symbol sx1 in
        let x2 = var_of_symbol sx2 in
        let fop =
          match op with
          | Add -> fn_plus
          | Sub -> fn_minus
          | Mul -> fn_times
          | Div -> fn_divide
          | Mod -> fn_modulus
          | Le -> fn_lt
          | Leq -> fn_le
          | Eq -> fn_eq
          | And -> fn_and
          | Or -> fn_or
          | Xor -> fn_xor
        in
        fop y x1 x2
    (* [eq y (fop x1 x2); not_ (eq y bottom_); not_ (eq x1 bottom_); not_ (eq x2 bottom_)] *)
    | Constraint.Eq_lookup (xs1, s1, xs2, s2) ->
        let x = var_s @@ Symbol.name_of_lookup xs1 s1 in
        let y = var_s @@ Symbol.name_of_lookup xs2 s2 in
        eq x y
    (* [eq x y; not_ (eq x bottom_); not_ (eq y bottom_)] *)
    | Constraint.C_and (c1, c2) ->
        let e1 = z3_phis_of_smt_phi c1 in
        let e2 = z3_phis_of_smt_phi c2 in
        join [ e1; e2 ]
    | Constraint.C_cond_bottom (cs, cvars) ->
        let cs_complete =
          List.map cvars ~f:(fun cvar -> cvar.complete_name |> mk_bool_s)
        in
        let cs_picked =
          List.map cvars ~f:(fun cvar -> cvar.picked_name |> mk_bool_s)
        in

        let payloads = List.map cs ~f:z3_phis_of_smt_phi in
        let only_pick_the_complete =
          List.map2_exn cs_picked cs_complete ~f:( @=> ) |> join
        in
        let exclusion = make_exclusive cs_picked payloads in
        let no_paths_complete = join (List.map cs_complete ~f:not_) in
        (* exclusion *)
        or_ [ join [ only_pick_the_complete; exclusion ]; no_paths_complete ]
    | Constraint.Fbody_to_callsite (_lookups, fc) ->
        let cvars = List.map fc.outs ~f:(fun out -> out.cvar) in
        let cs_complete =
          List.map cvars ~f:(fun cvar -> cvar.complete_name |> mk_bool_s)
        in
        let cs_picked =
          List.map cvars ~f:(fun cvar -> cvar.picked_name |> mk_bool_s)
        in
        let eq_lookups =
          List.map fc.outs ~f:(fun out ->
              Constraint.eq_lookup fc.xs_in fc.stk_in out.xs_out out.stk_out
              |> z3_phis_of_smt_phi)
        in
        let eq_fids =
          List.map fc.outs ~f:(fun out ->
              Constraint.bind_fun out.f_out out.stk_out fc.fun_in
              |> z3_phis_of_smt_phi)
        in
        (* not(c1c)  AND  not(c2c) *)
        let no_paths_complete = join (List.map cs_complete ~f:not_) in
        (* (c1c -> [𝑥1]!=ThisFun(𝐶))  AND  (c2c -> [𝑥2]!=ThisFun(𝐶)) *)
        let all_complete_paths_invalid =
          List.mapi cs_complete ~f:(fun i cc_i ->
              let eq_fid = List.nth_exn eq_fids i in
              cc_i @=> not_ eq_fid)
          |> join
        in
        let picked_a_complete_path =
          (* c1p  XORs  c2p *)
          let exclusion = make_exclusion cs_picked in
          (* c1p => c1c  AND  c2p => c2c *)
          let only_pick_the_complete =
            List.map2_exn cs_picked cs_complete ~f:( @=> ) |> join
          in
          (* c1p  =>  [𝑥]||𝑋=[𝑥1,𝑥]||𝑋 ∧  [𝑥1]=ThisFun(𝐶) *)
          let only_pick_the_valid =
            List.mapi cs_picked ~f:(fun i cp ->
                let eq_lookup = List.nth_exn eq_lookups i in
                let eq_fid = List.nth_exn eq_fids i in
                cp @=> and2 eq_lookup eq_fid)
            |> join
          in
          join [ exclusion; only_pick_the_complete; only_pick_the_valid ]
        in
        log_noted_phi "no_cc" no_paths_complete;
        log_noted_phi "all_invld" all_complete_paths_invalid;
        log_noted_phi "pick_one" picked_a_complete_path;

        or_
          [
            no_paths_complete;
            all_complete_paths_invalid;
            picked_a_complete_path;
          ]
    | Constraint.Callsite_to_fbody (_lookups, cf) ->
        let cvars = List.map cf.ins ~f:(fun in_ -> in_.cvar) in
        let cs_complete =
          List.map cvars ~f:(fun cvar -> cvar.complete_name |> mk_bool_s)
        in
        let cs_picked =
          List.map cvars ~f:(fun cvar -> cvar.picked_name |> mk_bool_s)
        in
        let eq_lookups =
          List.map cf.ins ~f:(fun in_ ->
              Constraint.eq_lookup cf.xs_out cf.stk_out in_.xs_in in_.stk_in
              |> z3_phis_of_smt_phi)
        in
        let eq_fids =
          List.map cf.ins ~f:(fun in_ ->
              Constraint.bind_fun cf.f_out cf.stk_out in_.fun_in
              |> z3_phis_of_smt_phi)
        in
        (* cs_complete_i is determined by the L(x_f,C) and L(x'_i, C'_i),
              in which L(x_f,C) is shared by each cs_complete_i
        *)
        let no_paths_complete = join (List.map cs_complete ~f:not_) in
        let all_complete_paths_invalid =
          List.mapi cs_complete ~f:(fun i cc_i ->
              let eq_fid = List.nth_exn eq_fids i in
              cc_i @=> not_ eq_fid)
          |> join
        in
        let picked_a_complete_path =
          let exclusion = make_exclusion cs_picked in
          let only_pick_the_complete =
            List.map2_exn cs_picked cs_complete ~f:( @=> ) |> join
          in
          let only_pick_the_valid =
            List.mapi cs_picked ~f:(fun i cp ->
                let eq_lookup = List.nth_exn eq_lookups i in
                let eq_fid = List.nth_exn eq_fids i in
                cp @=> and2 eq_lookup eq_fid)
            |> join
          in
          join [ exclusion; only_pick_the_complete; only_pick_the_valid ]
        in
        or_
          [
            no_paths_complete;
            all_complete_paths_invalid;
            picked_a_complete_path;
          ]
    | Constraint.Target_stack _stk
    (* -> (let open StringSort in
         eq (var_s top_stack_name) (string_ @@ (stk |> Concrete_stack.to_string))
       ) *) ->
        ground_truth
    | Constraint.Eq_projection (_, _, _) -> failwith "no project yet"
  (*
        the length of choices can never be 0
    
        when the length of choices is 1:
          at_least_one is true,
          get_other_choices is [],
          at_most_one is (c0 -> not (or []))
            which is (c0 -> not false) => (c0 -> true), thus c0 must be true
    
        when the length of choices is 2:
          at_least_one is (c0 or c1)
          get_other_choices is [c1] for c0,
          at_most_one is [(c0 -> not (or [c1])) ; (c1 -> not (or [c0]))]
            which is [c0 -> not c1; c1 -> not c0]
    
        when the length of choices is >2:
          it works similar to case=2
       *)
  and make_exclusion choices =
    let at_least_one = or_ choices in
    let get_other_choices ci =
      List.filteri choices ~f:(fun i _ -> Int.(ci <> i))
    in
    let at_most_one =
      List.mapi choices ~f:(fun i c -> c @=> not_ (or_ (get_other_choices i)))
      |> join
    in
    join [ at_least_one; at_most_one ]
  and make_exclusive choice_vars payloads =
    let chosen_payloads =
      List.mapi payloads ~f:(fun ci payload ->
          let ci_var = List.nth_exn choice_vars ci in
          ci_var @=> payload)
    in
    if List.length choice_vars = 1 then
      let only_one = List.hd_exn choice_vars in
      join (only_one :: chosen_payloads)
    else
      let at_least_one = Z3.Boolean.mk_or ctx choice_vars in
      let at_most_one =
        List.mapi payloads ~f:(fun ci _ ->
            let ci_var = List.nth_exn choice_vars ci in
            let other_vars =
              List.filteri choice_vars ~f:(fun i _ -> Int.(ci <> i))
            in
            let exclusion =
              other_vars |> Z3.Boolean.mk_or ctx |> Z3.Boolean.mk_not ctx
            in
            ci_var @=> exclusion)
      in
      join ((at_least_one :: at_most_one) @ chosen_payloads)
  in
  z3_phis_of_smt_phi cs

let solution_input_feeder model target_stack (x, call_stack) : int option =
  let stk = Rstack.relativize target_stack call_stack in
  let name = Symbol.name_of_lookup [ x ] stk in
  (* Fmt.pr "Qeury Input: %s\n" name; *)
  SuduZ3.get_int_s model name

let memorized_solution_input_feeder mem model target_stack =
  let input_feeder = solution_input_feeder model target_stack in
  fun query ->
    let answer = input_feeder query in
    mem := answer :: !mem;
    answer

let get_inputs target_x model (target_stack : Concrete_stack.t) program =
  let input_history = ref [] in
  let input_feeder =
    memorized_solution_input_feeder input_history model target_stack
  in
  let target = (target_x, target_stack) in
  let _ = Naive_interpreter.eval ~input_feeder ~target program in
  List.rev !input_history

let get_cvar_picked model cvar_complete =
  Hashtbl.mapi
    ~f:(fun ~key:cvar ~data:_cc ->
      cvar.Cvar.picked_name |> SuduZ3.mk_bool_s |> SuduZ3.get_bool model
      |> Option.value ~default:false)
    cvar_complete

let cvar_complete_to_z3 cvar b =
  SuduZ3.eq
    (cvar.Cvar.complete_name |> SuduZ3.mk_bool_s)
    (Z3.Boolean.mk_val ctx b)

let cvar_complete_false_to_z3 cvar_complete_false =
  List.map cvar_complete_false ~f:(fun cvar -> cvar_complete_to_z3 cvar false)
