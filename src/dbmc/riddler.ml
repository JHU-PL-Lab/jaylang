open Core
open Tracelet
open Odefa_ast
open Odefa_ast.Ast
module SuduZ3 = Solver.SuduZ3
open SuduZ3

let ctx = Solver.ctx

let top_stack = SuduZ3.var_s "X_topstack"

let pick_at_key key = "P_" ^ Lookup_key.to_str key |> SuduZ3.mk_bool_s

let pick_at xs r_stk = pick_at_key (Lookup_key.of_parts2 xs r_stk)

let lookup xs r_stk = Lookup_key.parts2_to_str xs r_stk |> SuduZ3.var_s

let counter = ref 0

let reset () = counter := 0

let bind_x_v xs r_stk v =
  let x = lookup xs r_stk in
  let v =
    match v with
    | Value_int i -> SuduZ3.int_ i
    | Value_bool b -> SuduZ3.bool_ b
    | Value_function _ -> failwith "should not be a function"
    | Value_record _ ->
        Int.incr counter;
        SuduZ3.int_ !counter
  in
  SuduZ3.eq x v

let bind_fun xs r_stk (Id.Ident fid) =
  SuduZ3.eq (lookup xs r_stk) (SuduZ3.fun_ fid)

let bind_x_y x y r_stk = SuduZ3.eq (lookup x r_stk) (lookup y r_stk)

let bind_x_y' x r_stk y r_stk' = SuduZ3.eq (lookup x r_stk) (lookup y r_stk')

let bind_binop op y x1 x2 r_stk =
  let ey = lookup y r_stk in
  let ex1 = lookup x1 r_stk in
  let ex2 = lookup x2 r_stk in
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

let alias key x' =
  let x, xs, r_stk = Lookup_key.to_parts key in
  pick_at_key key
  @=> and_ [ bind_x_y (x :: xs) (x' :: xs) r_stk; pick_at (x' :: xs) r_stk ]

let alias_key key key' =
  let xs, r_stk = Lookup_key.to_parts2 key in
  let xs', r_stk' = Lookup_key.to_parts2 key' in
  pick_at_key key @=> and_ [ bind_x_y xs xs' r_stk; pick_at xs' r_stk' ]

let binop key bop x1 x2 =
  let x, xs, r_stk = Lookup_key.to_parts key in
  pick_at_key key
  @=> and_
        [
          bind_binop bop [ x ] [ x1 ] [ x2 ] r_stk;
          pick_at (x1 :: xs) r_stk;
          pick_at (x2 :: xs) r_stk;
        ]

let cond_top key cb condsite_stack =
  let x, xs, r_stk = Lookup_key.to_parts key in
  let choice = Option.value_exn cb.choice in
  let x2 = cb.cond in
  let eq_lookup = bind_x_y' (x :: xs) r_stk (x :: xs) condsite_stack in
  pick_at_key key
  @=> and_
        [
          bind_x_v [ x2 ] condsite_stack (Value_bool choice);
          pick_at [ x2 ] condsite_stack;
          pick_at (x :: xs) condsite_stack;
          eq_lookup;
        ]

let cond_bottom key cond_block x' =
  let x, xs, r_stk = Lookup_key.to_parts key in
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
        (cs @ [ p_x_ret_beta ], rs @ [ p_x_ret_beta @=> and2 eq_beta eq_lookup ]))
  in
  pick_at_key key @=> and_ (or_ cs :: rs)

let fun_enter key is_local (fb : fun_block) callsites block_map =
  let x, xs, r_stk = Lookup_key.to_parts key in

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
            (cs @ [ choice_i ], rs @ [ choice_i @=> and2 eq_on_para eq_fid ])
        | None -> (cs, rs))
  in
  pick_at_key key @=> and_ (or_ cs :: rs)

let fun_exit key xf fids block_map =
  let x, xs, r_stk = Lookup_key.to_parts key in
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
  pick_at_key key @=> and_ (or_ cs :: rs)

let mismatch key = pick_at_key key @=> box_bool false

let eq_x_v x v r_stk =
  match v with
  (* Ast.Value_body for function *)
  | Some (Value_function _) -> bind_fun [ x ] r_stk x
  (* Ast.Value_body *)
  | Some v -> bind_x_v [ x ] r_stk v
  (* Ast.Input_body *)
  | None -> bind_x_y [ x ] [ x ] r_stk

let discover_main key v =
  let x, _, r_stk = Lookup_key.to_parts key in
  let this_c_stk =
    eq top_stack
      (r_stk |> Rstack.concretize_top |> Concrete_stack.sexp_of_t
     |> Sexp.to_string_mach |> SuduZ3.fun_)
  in
  pick_at_key key @=> and_ [ eq_x_v x v r_stk; this_c_stk ]

let discover_non_main key x_first v =
  let x, _, r_stk = Lookup_key.to_parts key in

  pick_at_key key
  @=> and2 (eq_x_v x v r_stk) (pick_at_key (Lookup_key.to_first key x_first))

let discard key v =
  let x, xs, r_stk = Lookup_key.to_parts key in
  pick_at_key key
  @=> and_
        [
          (* this v muse be a Fun *)
          eq_x_v x v r_stk;
          bind_x_y (x :: xs) xs r_stk;
          pick_at xs r_stk;
        ]
