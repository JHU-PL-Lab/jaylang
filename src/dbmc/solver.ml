open Core
module Sym = Symbol
open Z3

module type Context = sig
  val ctx : Z3.context
end

module Make (C : Context) () = struct
  let ctx = C.ctx

  let _counter = ref 0

  let get_new_sym () =
    let last = !_counter in
    let name = Fmt.str "$s%03d" last in
    Int.incr _counter;
    Symbol.mk_string ctx name

  let make_indexed_symbol pre i =
    let name = Fmt.str "$%s%03d" pre i in
    Symbol.mk_string ctx name

  let make_choice_var ~from_id ~to_id post rel_stk =
    let id_name = function Some id -> Id.show id | None -> "" in
    let name =
      Fmt.str "$%a%s_to_%s_%s" Relative_stack.pp rel_stk (id_name from_id)
        (id_name to_id) post
    in
    Symbol.mk_string ctx name

  let intS = Arithmetic.Integer.mk_sort ctx

  let boolS = Boolean.mk_sort ctx

  let strS = Seq.mk_string_sort ctx

  let intC =
    Datatype.mk_constructor_s ctx "Int"
      (Symbol.mk_string ctx "is-Int")
      [ Symbol.mk_string ctx "i" ]
      [ Some intS ] [ 1 ]

  let boolC =
    Datatype.mk_constructor_s ctx "Bool"
      (Symbol.mk_string ctx "is-Bool")
      [ Symbol.mk_string ctx "b" ]
      [ Some boolS ] [ 1 ]

  let funC =
    Datatype.mk_constructor_s ctx "Fun"
      (Symbol.mk_string ctx "is-Fun")
      [ Symbol.mk_string ctx "fid" ]
      [ Some strS ] [ 1 ]

  (* let funC = Datatype.mk_constructor_s ctx "Fun"
      (Symbol.mk_string ctx "is-Fun") [Symbol.mk_string ctx "fid"; Symbol.mk_string ctx "fi"] [Some strS; Some intS] [1;2] *)

  (* let bottomC = Datatype.mk_constructor_s ctx "Bottom"
      (Symbol.mk_string ctx "is-Bottom") [] [] [] *)

  let valS = Datatype.mk_sort_s ctx "IntOrBoolOrFun" [ intC; boolC; funC ]

  let intR, boolR, funR =
    match Datatype.get_recognizers valS with
    | [ r1; r2; r3 ] -> (r1, r2, r3)
    | _ -> failwith "recogniziers mismatch"

  let ifInt e = FuncDecl.apply intR [ e ]

  let ifBool e = FuncDecl.apply boolR [ e ]

  let ifFun e = FuncDecl.apply funR [ e ]

  let getInt, getBool, getFun =
    match Datatype.get_accessors valS with
    | [ [ a1 ]; [ a2 ]; [ a3 ] ] -> (a1, a2, a3)
    (* | [a1]::[a2]::[a3;a4]::[] -> a1, a2, (a3, a4) *)
    | _ -> failwith "accessors mismatch"

  let intD = Datatype.Constructor.get_constructor_decl intC

  let boolD = Datatype.Constructor.get_constructor_decl boolC

  let funD = Datatype.Constructor.get_constructor_decl funC

  (* let bottomD = Datatype.Constructor.get_constructor_decl bottomC *)

  let int_ i = FuncDecl.apply intD [ Arithmetic.Integer.mk_numeral_i ctx i ]

  let bool_ b = FuncDecl.apply boolD [ Boolean.mk_val ctx b ]

  let fun_ s = FuncDecl.apply funD [ Seq.mk_string ctx s ]

  (* let fun_ s = FuncDecl.apply funD [Seq.mk_string ctx s; Z3.Arithmetic.Integer.mk_const ctx (get_new_sym ())] *)
  (* let bottom_ = FuncDecl.apply bottomD [] *)

  let true_ = bool_ true

  let false_ = bool_ false

  module StringSort = struct
    let strS = Z3.Seq.mk_string_sort ctx

    let string_ s = Seq.mk_string ctx s

    let var_s n = Expr.mk_const_s ctx n strS
  end

  let var_s n = Expr.mk_const_s ctx n valS

  let var_ n = Expr.mk_const ctx n valS

  let top_stack_name = "!stack"

  let top_stack_var = StringSort.var_s top_stack_name

  let eq e1 e2 = Boolean.mk_eq ctx e1 e2

  let and2 e1 e2 = Boolean.mk_and ctx [ e1; e2 ]

  let and_ = Boolean.mk_and ctx

  let join = and_

  let all = and_

  let or_ = Boolean.mk_or ctx

  let not_ = Boolean.mk_not ctx

  let implies = Boolean.mk_implies ctx

  let ( @=> ) = implies

  let bop case inj fn e1 e2 =
    let p1 = FuncDecl.apply case [ e1 ] in
    let p2 = FuncDecl.apply case [ e2 ] in
    let p3 = fn p1 p2 in
    FuncDecl.apply inj [ p3 ]

  let fn_two_ints fop y e1 e2 =
    let ey = bop getInt intD fop e1 e2 in
    join [ eq y ey; ifInt e1; ifInt e2 ]

  let fn_two_ints_to_bool fop y e1 e2 =
    let ey = bop getInt boolD fop e1 e2 in
    join [ eq y ey; ifInt e1; ifInt e2 ]

  let fn_two_bools fop y e1 e2 =
    let ey = bop getBool boolD fop e1 e2 in
    join [ eq y ey; ifBool e1; ifBool e2 ]

  let fn_plus = fn_two_ints (fun e1 e2 -> Arithmetic.mk_add ctx [ e1; e2 ])

  let fn_minus = fn_two_ints (fun e1 e2 -> Arithmetic.mk_sub ctx [ e1; e2 ])

  let fn_times = fn_two_ints (fun e1 e2 -> Arithmetic.mk_mul ctx [ e1; e2 ])

  let fn_divide = fn_two_ints (Arithmetic.mk_div ctx)

  let fn_modulus = fn_two_ints (Arithmetic.Integer.mk_mod ctx)

  let fn_lt = fn_two_ints_to_bool (Arithmetic.mk_lt ctx)

  let fn_le = fn_two_ints_to_bool (Arithmetic.mk_le ctx)

  let fn_eq = fn_two_ints_to_bool (Boolean.mk_eq ctx)

  let fn_and = fn_two_bools and2

  let fn_or = fn_two_bools (fun e1 e2 -> Boolean.mk_or ctx [ e1; e2 ])

  let fn_xor = fn_two_bools (Boolean.mk_xor ctx)

  (* let fn_eq y x1 x2 =
     eq y (eq x1 x2) *)
  let ground_truth = eq true_ true_

  let var_of_symbol sym = sym |> Sym.show |> var_s

  let boole_of_str s = Z3.Boolean.mk_const_s ctx s

  let z3_gate_out_phis ccs =
    List.map ccs ~f:(fun (cname, cval) ->
        eq
          (boole_of_str (Constraint.mk_cvar_complete cname))
          (Z3.Boolean.mk_val ctx cval))

  let phi_z3_of_constraint ?(debug = false) ?debug_tool cs =
    let log_noted_phi note phi =
      let key, note_map = Option.value_exn debug_tool in
      if debug then
        Hashtbl.add_multi !note_map ~key ~data:(note, phi)
      else
        ()
    in
    let rec z3_phis_of_smt_phi = function
      | Constraint.Eq_v (sx, cv) ->
          let x = var_of_symbol sx in
          let v =
            match cv with
            | Int i -> int_ i
            | Bool b -> bool_ b
            | Fun fid -> fun_ (fid |> Id.show)
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
          let x = var_s @@ Constraint.name_of_lookup xs1 s1 in
          let y = var_s @@ Constraint.name_of_lookup xs2 s2 in
          eq x y
      (* [eq x y; not_ (eq x bottom_); not_ (eq y bottom_)] *)
      | Constraint.C_and (c1, c2) ->
          let e1 = z3_phis_of_smt_phi c1 in
          let e2 = z3_phis_of_smt_phi c2 in
          join [ e1; e2 ]
      | Constraint.C_cond_bottom (site, r_stk, cs) ->
          let cvars = Constraint.cvars_of_condsite site r_stk in
          let cvar_complete, cvar_picked =
            Constraint.derive_complete_and_picked cvars
          in
          let cs_complete, cs_picked =
            ( List.map ~f:boole_of_str cvar_complete,
              List.map ~f:boole_of_str cvar_picked )
          in
          let payloads = List.map cs ~f:z3_phis_of_smt_phi in
          let only_pick_the_complete =
            List.map2_exn cs_picked cs_complete ~f:( @=> ) |> join
          in
          let exclusion = make_exclusive cs_picked payloads in
          let no_paths_complete = join (List.map cs_complete ~f:not_) in
          (* exclusion *)
          or_ [ join [ only_pick_the_complete; exclusion ]; no_paths_complete ]
      | Constraint.Fbody_to_callsite fc ->
          let cvars = Constraint.cvars_of_fc fc in
          let cvar_complete, cvar_picked =
            Constraint.derive_complete_and_picked cvars
          in
          let cs_complete, cs_picked =
            ( List.map ~f:boole_of_str cvar_complete,
              List.map ~f:boole_of_str cvar_picked )
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
      | Constraint.Callsite_to_fbody cf ->
          let cvars = Constraint.cvars_of_cf cf in
          let cvar_complete, cvar_picked =
            Constraint.derive_complete_and_picked cvars
          in
          let cs_complete, cs_picked =
            ( List.map ~f:boole_of_str cvar_complete,
              List.map ~f:boole_of_str cvar_picked )
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
        join (at_least_one :: at_most_one @ chosen_payloads)
    in
    z3_phis_of_smt_phi cs

  (* model and solution *)
  let bool_of_expr_exn v =
    match Z3.Boolean.get_bool_value v with
    | L_TRUE -> true
    | L_FALSE -> false
    | L_UNDEF -> failwith "pass_if_true"

  let bool_of_expr v =
    match Z3.Boolean.get_bool_value v with
    | L_TRUE -> true
    | L_FALSE -> false
    | L_UNDEF -> false

  let int_of_expr e =
    e |> Z3.Arithmetic.Integer.get_big_int |> Big_int_Z.int_of_big_int

  let string_of_expr e = e |> Seq.get_string ctx

  let is_bool_from_model model e =
    bool_of_expr (Option.value_exn (Model.eval model (ifBool e) false))

  let is_int_from_model model e =
    bool_of_expr (Option.value_exn (Model.eval model (ifInt e) false))

  let is_fun_from_model model e =
    bool_of_expr (Option.value_exn (Model.eval model (ifFun e) false))

  let get_bool_expr model e =
    Option.value_exn (Model.eval model (FuncDecl.apply getBool [ e ]) false)

  let get_int_expr model e =
    Option.value_exn (Model.eval model (FuncDecl.apply getInt [ e ]) false)

  let get_fun_expr model e =
    Option.value_exn (Model.eval model (FuncDecl.apply getFun [ e ]) false)

  let get_value model e =
    if is_int_from_model model e then
      Some (Constraint.Int (get_int_expr model e |> int_of_expr))
    else if is_bool_from_model model e then
      Some (Constraint.Bool (get_bool_expr model e |> bool_of_expr))
    else if is_fun_from_model model e then
      let fid = get_fun_expr model e |> string_of_expr in
      Some (Constraint.Fun (Id.Ident fid))
    else
      None
  (* failwith "get_value" *)

  (* TODO: using functions above *)
  let get_int_s model s =
    let x = FuncDecl.apply getInt [ var_s s ] in
    let r = Option.value_exn (Model.eval model x true) in
    Z3.Arithmetic.Integer.get_big_int r |> Big_int_Z.int_of_big_int

  let get_bool model e =
    let r = Option.value_exn (Model.eval model e false) in
    match Z3.Boolean.get_bool_value r with
    | L_TRUE -> Some true
    | L_FALSE -> Some false
    | L_UNDEF ->
        Logs.warn (fun m -> m "[warning] %s L_UNDEF" (Z3.Expr.to_string e));
        None

  let eval_value model e = Option.value_exn (Model.eval model e false)

  let get_top_stack model =
    let stack_v = Option.value_exn (Model.eval model top_stack_var true) in
    let stack_str = Seq.get_string ctx stack_v in
    Concrete_stack.of_string stack_str

  let check_with_assumption solver assumptions =
    match Z3.Solver.check solver assumptions with
    | Z3.Solver.SATISFIABLE -> (
        match Z3.Solver.get_model solver with
        | None ->
            failwith
              ("check is not invoked before; " ^ "the result is not SAT; "
             ^ " the model production is not enabled")
        | Some model -> Result.Ok model)
    | Z3.Solver.UNSATISFIABLE -> Result.Error (Z3.Solver.get_unsat_core solver)
    | Z3.Solver.UNKNOWN ->
        failwith
        @@ Printf.sprintf "[check_and_get_model] Unknown result in solve: %s"
             (Z3.Solver.get_reason_unknown solver)
end
