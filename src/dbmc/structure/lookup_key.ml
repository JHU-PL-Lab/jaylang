open Core
open Dj_common

module T = struct
  type t = { x : Id.t; r_stk : Rstack.t; block : Cfg.block [@ignore] }
  [@@deriving compare, equal, hash]

  let sexp_of_t k = [%sexp_of: Id.t * Rstack.t] (k.x, k.r_stk)
end

include T
include Comparator.Make (T)

let start (x : Id.t) block : t = { x; r_stk = Rstack.empty; block }
let of3 x r_stk block = { x; r_stk; block }
let with_x key x = { key with x }
let to_first = with_x

let to_string key =
  Printf.sprintf "%s_%s" (Id.show key.x) (Rstack.to_string key.r_stk)

let pp oc key = Fmt.pf oc "%a[%a]" Id.pp key.x Rstack.pp key.r_stk

let to_str2 x r_stk =
  Printf.sprintf "%s_%s" (Id.show x) (Rstack.to_string r_stk)

let chrono_compare map k1 k2 =
  let { x = x1; r_stk = r_stk1; _ } = k1 in
  let { x = x2; r_stk = r_stk2; _ } = k2 in
  (* assert (List.is_empty xs1);
     assert (List.is_empty xs2); *)
  let rec compare_stack s1 s2 =
    match (s1, s2) with
    | [], [] ->
        if Id.equal x1 x2 then 0 else if Cfg.is_before map x1 x2 then 1 else -1
    | (cs1, f1) :: ss1, (cs2, f2) :: ss2 ->
        if Rstack.equal_frame (cs1, f1) (cs2, f2)
        then compare_stack ss1 ss2
        else if Id.equal cs1 cs2
        then 0 (* failwith "the same promgram points" *)
        else if Cfg.is_before map cs1 cs2
        then 1
        else -1
    | _, _ -> 1
  in
  let stk1, co_stk1 = Rstack.construct_stks r_stk1 in
  let stk2, co_stk2 = Rstack.construct_stks r_stk2 in
  let result_co_stk = compare_stack co_stk1 co_stk2 in
  if result_co_stk = 0 then compare_stack stk1 stk2 else result_co_stk

let length key = 1 + Rstack.length key.r_stk

let get_f_return map fid term =
  let fblock = Jayil.Ast.Ident_map.find fid map in
  let x' = Cfg.ret_of fblock in
  let r_stk' = Rstack.push term.r_stk (term.x, fid) in
  let key_ret = of3 x' r_stk' fblock in
  key_ret

let return_key_of_cond key block_map beta =
  let open Cfg in
  let beta_block =
    let cond_block =
      Jayil.Ast.Ident_map.find key.x block_map |> Cfg.cast_to_cond_block
    in
    if Option.is_some cond_block.choice
    then failwith "conditional_body: not both"
    else () ;
    Cond { cond_block with choice = Some beta }
  in
  let x_ret = Cfg.ret_of beta_block in
  let beta_stack = Rstack.push key.r_stk (key.x, Id.cond_fid beta) in
  of3 x_ret beta_stack beta_block

let get_callsites r_stk (fb : Cfg.fun_block) =
  let fid = fb.point in
  let callsites =
    match Rstack.paired_callsite r_stk fid with
    | Some callsite -> [ callsite ]
    | None -> fb.callsites
  in
  callsites