open Core
open Tracelet
open Odefa_ast
open Odefa_ast.Ast

let encode_constraint defined_site =
  match defined_site with
  | At_clause { clause = Clause (_, Value_body _v); _ } -> ()
  | _ -> ()

let ctx = Solver.ctx

module SuduZ3 = Solver.SuduZ3

let var_of_symbol sym = sym |> Symbol.show |> SuduZ3.var_s

let encode_constraint ?(x_first = None) ?(callsites = []) xs0 r_stk defined_site
    =
  let open SuduZ3 in
  (* helpers *)
  let p = Constraint.name_of_lookup xs0 r_stk |> var_s in
  let pick_at xs r_stk = Constraint.name_of_lookup xs r_stk |> var_s in
  let pick_first_at x_first r_stk =
    Constraint.name_of_lookup [ x_first ] r_stk |> var_s
  in
  let bind_x_v xs r_stk v =
    let x = var_s (Constraint.name_of_lookup xs r_stk) in
    let v =
      match Constraint.to_smt_v v with
      | Constraint.Int i -> int_ i
      | Constraint.Bool b -> bool_ b
      | Constraint.Fun fid -> fun_ fid
      | Constraint.Record -> failwith "no record yet"
    in
    v
  in
  let bind_x_y x y r_stk =
    let ex = var_s (Constraint.name_of_lookup x r_stk) in
    let ey = var_s (Constraint.name_of_lookup y r_stk) in
    eq ex ey
  in
  let bind_binop op y x1 x2 r_stk =
    let ey = var_s (Constraint.name_of_lookup y r_stk) in
    let ex1 = var_s (Constraint.name_of_lookup x1 r_stk) in
    let ex2 = var_s (Constraint.name_of_lookup x2 r_stk) in
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
  let deal_with_value xs0 r_stk v =
    let _x, xs = (List.hd_exn xs0, List.tl_exn xs0) in
    let eq_x_v =
      match v with
      (* Ast.Value_body *)
      | Some v -> bind_x_v xs0 r_stk v
      (* Ast.Input_body *)
      | None -> bind_x_y xs0 xs0 r_stk
    in
    match (List.is_empty xs, x_first) with
    | true, None ->
        (* Discover Main *)
        let top_stack = ground_truth in
        p @=> and2 eq_x_v top_stack
    | true, Some x_first ->
        (* Discover Non-Main *)
        p @=> and2 eq_x_v (pick_first_at x_first r_stk)
    | false, None ->
        (* Discard *)
        p @=> and_ [ eq_x_v; bind_x_y xs0 xs r_stk; pick_at xs r_stk ]
    | false, Some _ -> failwith "error"
  in
  let x, xs = (List.hd_exn xs0, List.tl_exn xs0) in
  let encode = function
    | At_clause { clause = Clause (_, Value_body v); _ } ->
        deal_with_value xs0 r_stk (Some v)
    | At_clause { clause = Clause (_, Input_body); _ } ->
        deal_with_value xs0 r_stk None
    | At_clause { clause = Clause (_, Var_body (Var (x', _))); _ } ->
        p
        @=> and_
              [ bind_x_y (x :: xs) (x' :: xs) r_stk; pick_at (x' :: xs) r_stk ]
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
    | At_fun_para (_is_local, fb) ->
        let _fid = fb.point in
        p @=> and_ (List.map callsites ~f:(fun _callsite -> ground_truth))
    | _ -> failwith "error lookup cases"
  in

  encode defined_site
