open Core
open Tracelet
open Odefa_ast
open Odefa_ast.Ast

let ctx = Solver.ctx

module SuduZ3 = Solver.SuduZ3

let top_stack = SuduZ3.var_s "X_topstack"

let pick_at_key key = "P_" ^ Lookup_key.to_str key |> SuduZ3.mk_bool_s

let pick_at xs r_stk = pick_at_key (Lookup_key.of_parts2 xs r_stk)

let mk_encode_constraint block_map (_state : Search_tree.state) =
  (* let pick_at_key key = pick_at_key state key in *)

  (* let lookup xs r_stk = lookup state xs r_stk in *)
  let lookup xs r_stk = Lookup_key.parts2_to_str xs r_stk |> SuduZ3.var_s in

  let bind_x_v xs r_stk v =
    let x = lookup xs r_stk in
    let v =
      match Constraint.to_smt_v v with
      | Constraint.Int i -> SuduZ3.int_ i
      | Constraint.Bool b -> SuduZ3.bool_ b
      | Constraint.Fun fid -> SuduZ3.fun_ fid
      | Constraint.Record -> failwith "no record yet"
    in
    SuduZ3.eq x v
  in

  let bind_fun xs r_stk (Id.Ident fid) =
    SuduZ3.eq (lookup xs r_stk) (SuduZ3.fun_ fid)
  in
  let bind_x_y x y r_stk = SuduZ3.eq (lookup x r_stk) (lookup y r_stk) in
  let bind_x_y' x r_stk y r_stk' =
    SuduZ3.eq (lookup x r_stk) (lookup y r_stk')
  in
  let bind_binop op y x1 x2 r_stk =
    let ey = lookup y r_stk in
    let ex1 = lookup x1 r_stk in
    let ex2 = lookup x2 r_stk in
    let open SuduZ3 in
    let fop =
      match op with
      | Binary_operator_plus -> fn_plus
      | Binary_operator_minus -> fn_minus
      | Binary_operator_times -> fn_times
      | Binary_operator_divide -> fn_divide
      | Binary_operator_modulus -> fn_modulus
      | Binary_operator_less_than -> fn_lt
      | Binary_operator_less_than_or_equal_to -> fn_le
      | Binary_operator_equal_to -> fn_eq
      | Binary_operator_and -> fn_and
      | Binary_operator_or -> fn_or
      | Binary_operator_xor -> fn_xor
    in
    fop ey ex1 ex2
  in

  let encode_constraint ?(x_first = None) ?(callsites = []) key defined_site =
    let open SuduZ3 in
    let open Lookup_key in
    (* helpers *)
    let xs0, r_stk = (lookups key, key.r_stk) in
    let p = pick_at_key key in

    let deal_with_value v =
      let x, xs = (List.hd_exn xs0, List.tl_exn xs0) in
      let eq_x_v =
        match v with
        (* Ast.Value_body for function *)
        | Some (Value_function _) -> bind_fun [ x ] r_stk x
        (* Ast.Value_body *)
        | Some v -> bind_x_v [ x ] r_stk v
        (* Ast.Input_body *)
        | None -> bind_x_y [ x ] [ x ] r_stk
      in
      match (List.is_empty xs, x_first) with
      (* Discover Main *)
      | true, None ->
          let this_c_stk =
            eq top_stack
              (r_stk |> Rstack.concretize |> Concrete_stack.sexp_of_t
             |> Sexp.to_string_mach |> SuduZ3.fun_)
          in
          p @=> and_ [ eq_x_v; this_c_stk ]
      (* Discover Non-Main *)
      | true, Some x_first ->
          p @=> and2 eq_x_v (pick_at_key (Lookup_key.to_first key x_first))
      (* Discard *)
      | false, None ->
          p
          @=> and_
                [
                  (* this v muse be a Fun *)
                  eq_x_v;
                  bind_x_y xs0 xs r_stk;
                  pick_at xs r_stk;
                ]
      | false, Some _ -> failwith "error"
    in
    let x, xs = (List.hd_exn xs0, List.tl_exn xs0) in
    let encode = function
      (* Value *)
      | At_clause { clause = Clause (_, Value_body v); _ } ->
          deal_with_value (Some v)
      (* Input *)
      | At_clause { clause = Clause (_, Input_body); _ } -> deal_with_value None
      (* Alias *)
      | At_clause { clause = Clause (_, Var_body (Var (x', _))); _ } ->
          p
          @=> and_
                [
                  bind_x_y (x :: xs) (x' :: xs) r_stk; pick_at (x' :: xs) r_stk;
                ]
      (* Binop *)
      | At_clause
          {
            clause =
              Clause (_, Binary_operation_body (Var (x1, _), bop, Var (x2, _)));
            _;
          } ->
          p
          @=> and_
                [
                  bind_binop bop [ x ] [ x1 ] [ x2 ] r_stk;
                  pick_at (x1 :: xs) r_stk;
                  pick_at (x2 :: xs) r_stk;
                ]
      (* Fun Enter *)
      | At_fun_para (is_local, fb) ->
          let fid = fb.point in
          let cs, rs =
            List.fold callsites ~init:([], []) ~f:(fun (cs, rs) callsite ->
                let _callsite_block, x', x'', x''' =
                  Tracelet.fun_info_of_callsite callsite block_map
                in
                match Rstack.pop r_stk (x', fid) with
                | Some callsite_stk ->
                    let p_X' =
                      if is_local then
                        pick_at (x''' :: xs) callsite_stk
                      else
                        pick_at (x'' :: x :: xs) callsite_stk
                    in
                    let p_x'' = pick_at [ x'' ] callsite_stk in
                    let choice_i = and2 p_X' p_x'' in
                    let eq_on_para =
                      if is_local then (* para == arg *)
                        bind_x_y' (x :: xs) r_stk (x''' :: xs) callsite_stk
                      else (* nonlocal == def *)
                        bind_x_y' (x :: xs) r_stk (x'' :: x :: xs) callsite_stk
                    in
                    let eq_fid = bind_fun [ x'' ] callsite_stk fid in
                    ( cs @ [ choice_i ],
                      rs @ [ choice_i @=> and2 eq_on_para eq_fid ] )
                | None -> (cs, rs))
          in
          p @=> and_ (or_ cs :: rs)
      (* Fun Exit *)
      | At_clause
          {
            clause = Clause (_, Appl_body (Var (xf, _), Var (_xv, _)));
            cat = App fids;
            _;
          } ->
          let cs, rs =
            List.fold fids ~init:([], []) ~f:(fun (cs, rs) fid ->
                let fblock = Ident_map.find fid block_map in
                let x' = Tracelet.ret_of fblock in
                let r_stk' = Rstack.push r_stk (x, fid) in
                let p_x' = pick_at (x' :: xs) r_stk' in
                let eq_arg_para = bind_x_y' (x :: xs) r_stk (x' :: xs) r_stk' in
                let eq_fid = bind_fun [ xf ] r_stk fid in
                (cs @ [ p_x' ], rs @ [ p_x' @=> and2 eq_arg_para eq_fid ]))
          in
          p @=> and_ (or_ cs :: rs)
      (* Cond Top / Cond Choice *)
      | At_chosen cb ->
          let choice = Option.value_exn cb.choice in
          let x2 = cb.cond in
          let condsite_stack =
            match Rstack.pop r_stk (cb.point, Id.cond_fid choice) with
            | Some stk -> stk
            | None -> failwith "impossible in CondTop"
          in
          let eq_lookup = bind_x_y' (x :: xs) r_stk (x :: xs) condsite_stack in
          p
          @=> and_
                [
                  bind_x_v [ x2 ] condsite_stack (Value_bool choice);
                  pick_at [ x2 ] condsite_stack;
                  pick_at (x :: xs) condsite_stack;
                  eq_lookup;
                ]
      (* Cond Bottom / Condsite *)
      | At_clause
          {
            clause = Clause (_, Conditional_body (Var (x', _), _, _));
            id = tid;
            _;
          } ->
          let cond_block =
            Ident_map.find tid block_map |> Tracelet.cast_to_cond_block
          in
          if Option.is_some cond_block.choice then
            failwith "conditional_body: not both"
          else
            ();
          let cs, rs =
            List.fold [ true; false ] ~init:([], []) ~f:(fun (cs, rs) beta ->
                let ctracelet = Cond { cond_block with choice = Some beta } in
                let x_ret = Tracelet.ret_of ctracelet in
                let cbody_stack = Rstack.push r_stk (x, Id.cond_fid beta) in
                let p_x_ret_beta = pick_at (x_ret :: xs) cbody_stack in
                let eq_beta = bind_x_v [ x' ] r_stk (Value_bool beta) in
                let eq_lookup =
                  (* bind_x_y' (x :: xs) r_stk (x_ret :: xs) cbody_stack *)
                  bind_x_y' [ x ] r_stk [ x_ret ] cbody_stack
                in
                ( cs @ [ p_x_ret_beta ],
                  rs @ [ p_x_ret_beta @=> and2 eq_beta eq_lookup ] ))
          in
          p @=> and_ (or_ cs :: rs)
      | Lookup_mismatch -> p @=> box_bool false
      | _ -> failwith "error lookup cases"
    in

    encode defined_site
  in
  encode_constraint
