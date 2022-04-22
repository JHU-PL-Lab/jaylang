open Core
open Tracelet
open Odefa_ast.Ast
open Riddler
module SuduZ3 = Solver.SuduZ3
open SuduZ3

let discover_main key v =
  let x, r_stk = Lookup_key.to2 key in
  let this_c_stk =
    eq top_stack
      (r_stk |> Rstack.concretize_top |> Concrete_stack.sexp_of_t
     |> Sexp.to_string_mach |> SuduZ3.fun_)
  in
  and_ [ eq_x_v x v r_stk; this_c_stk ]

let discover_non_main key _x_first v =
  let x, r_stk = Lookup_key.to2 key in
  eq_x_v x v r_stk

let binop key bop x1 x2 =
  let x, r_stk = Lookup_key.to2 key in
  bind_binop bop x x1 x2 r_stk

let mismatch _key = box_bool false
let cond_top xc beta r_stk = bind_x_v xc r_stk (Value_bool beta)

(* let cond_bottom key beta cond_block x' =
     let x, r_stk = Lookup_key.to2 key in
     let ctracelet = Cond { cond_block with choice = Some beta } in
     let x_ret = Tracelet.ret_of ctracelet in
     let cbody_stack = Rstack.push r_stk (x, Id.cond_fid beta) in
     let eq_beta = bind_x_v x' r_stk (Value_bool beta) in
   (* let eq_lookup = bind_x_y' (x :: xs) r_stk (x_ret :: xs) cbody_stack in
      and2 eq_beta eq_lookup *) *)

let fun_enter_local key (fb : fun_block) x'' x''' callsite_stk =
  let x, r_stk = Lookup_key.to2 key in
  let fid = fb.point in
  let eq_on_para = bind_x_y' x r_stk x''' callsite_stk in
  let eq_fid = bind_fun x'' callsite_stk fid in
  and2 eq_on_para eq_fid

let fun_exit key xf fid block_map =
  let x, r_stk = Lookup_key.to2 key in
  let fblock = Ident_map.find fid block_map in
  let x' = Tracelet.ret_of fblock in
  let r_stk' = Rstack.push r_stk (x, fid) in
  let eq_arg_para = bind_x_y' x r_stk x' r_stk' in
  let eq_fid = bind_fun xf r_stk fid in
  and2 eq_arg_para eq_fid
